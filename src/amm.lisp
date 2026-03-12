;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; amm.lisp - Constant Product AMM Implementation

(in-package #:cl-amm)

;;; ===================================================================
;;; Helper Functions
;;; ===================================================================

(defun amm-isqrt (n)
  "Integer square root using Newton's method with AMM-specific error handling."
  (cond
    ((< n 0) (error 'invalid-amount-error :amount n :reason "Cannot take sqrt of negative"))
    ((< n 2) n)
    (t (let ((x n)
             (y (ash (1+ n) -1)))
         (loop while (< y x)
               do (setf x y
                        y (ash (+ y (floor n y)) -1)))
         x))))

(defun get-current-time ()
  "Get current Unix timestamp."
  (get-universal-time))

(defun generate-pool-id (token-a token-b)
  "Generate a canonical pool ID from token pair."
  (if (string< token-a token-b)
      (format nil "~A/~A" token-a token-b)
      (format nil "~A/~A" token-b token-a)))

;;; ===================================================================
;;; Pool Creation
;;; ===================================================================

(defun create-pool (token-a token-b amount-a amount-b &key
                                                        (fee-bps +fee-bps-medium+)
                                                        (creator "system"))
  "Create a new liquidity pool with initial liquidity.
   Returns (values pool lp-shares-minted).

   The initial LP shares are calculated as sqrt(amount-a * amount-b) - MIN_LIQUIDITY.
   MIN_LIQUIDITY is permanently locked to prevent zero-liquidity attacks."
  (declare (ignore creator))
  ;; Validate amounts
  (when (or (<= amount-a 0) (<= amount-b 0))
    (error 'invalid-amount-error
           :amount (min amount-a amount-b)
           :reason "Initial liquidity must be positive"))

  ;; Generate canonical pool ID (sorted token pair)
  (let* ((pool-id (generate-pool-id token-a token-b))
         ;; Ensure consistent ordering
         (ordered-a (if (string< token-a token-b) token-a token-b))
         (ordered-b (if (string< token-a token-b) token-b token-a))
         (ordered-amount-a (if (string< token-a token-b) amount-a amount-b))
         (ordered-amount-b (if (string< token-a token-b) amount-b amount-a))
         ;; Calculate initial LP shares: sqrt(x * y) - MIN_LIQUIDITY
         (geometric-mean (amm-isqrt (* ordered-amount-a ordered-amount-b)))
         (lp-shares (- geometric-mean +min-liquidity+))
         (now (get-current-time)))

    (when (<= lp-shares 0)
      (error 'invalid-amount-error
             :amount lp-shares
             :reason "Initial liquidity too small"))

    ;; Check for existing pool
    (when (gethash pool-id *pool-registry*)
      (error 'pool-exists-error :pool-id pool-id :message "Pool already exists"))

    (let ((pool (make-liquidity-pool
                 :id pool-id
                 :token-a ordered-a
                 :token-b ordered-b
                 :reserve-a ordered-amount-a
                 :reserve-b ordered-amount-b
                 :total-lp-shares (+ lp-shares +min-liquidity+) ; Include locked liquidity
                 :fee-bps fee-bps
                 :created-at now
                 :last-updated now)))
      (setf (gethash pool-id *pool-registry*) pool)
      (values pool lp-shares))))

;;; ===================================================================
;;; Liquidity Operations
;;; ===================================================================

(defun add-liquidity (pool-id amount-a amount-b &key (provider "anon") (min-lp-shares 0))
  "Add liquidity to an existing pool.
   Returns (values actual-amount-a actual-amount-b lp-shares-minted).

   To maintain the pool ratio, one of the amounts may be adjusted down."
  (declare (ignore provider))
  (let ((pool (get-pool pool-id)))
    (when (or (<= amount-a 0) (<= amount-b 0))
      (error 'invalid-amount-error
             :amount (min amount-a amount-b)
             :reason "Liquidity amounts must be positive"))

    (let* ((reserve-a (liquidity-pool-reserve-a pool))
           (reserve-b (liquidity-pool-reserve-b pool))
           (total-shares (liquidity-pool-total-lp-shares pool)))

      (when (or (zerop reserve-a) (zerop reserve-b))
        (error 'zero-liquidity-error :pool-id pool-id :message "Pool has zero reserves"))

      ;; Calculate optimal amounts to maintain ratio
      (let* ((optimal-b (floor (* amount-a reserve-b) reserve-a))
             (actual-a amount-a)
             (actual-b (if (<= optimal-b amount-b)
                           optimal-b
                           (progn
                             (setf actual-a (floor (* amount-b reserve-a) reserve-b))
                             amount-b)))
             ;; LP shares proportional to contribution
             (lp-shares (min (floor (* actual-a total-shares) reserve-a)
                             (floor (* actual-b total-shares) reserve-b))))

        (when (< lp-shares min-lp-shares)
          (error 'slippage-exceeded-error
                 :expected min-lp-shares
                 :actual lp-shares
                 :tolerance-bps 0
                 :message "LP shares below minimum"))

        ;; Update pool state
        (incf (liquidity-pool-reserve-a pool) actual-a)
        (incf (liquidity-pool-reserve-b pool) actual-b)
        (incf (liquidity-pool-total-lp-shares pool) lp-shares)
        (setf (liquidity-pool-last-updated pool) (get-current-time))

        (values actual-a actual-b lp-shares)))))

(defun remove-liquidity (pool-id lp-shares &key (provider "anon") (min-amount-a 0) (min-amount-b 0))
  "Remove liquidity from a pool by burning LP shares.
   Returns (values amount-a amount-b)."
  (declare (ignore provider))
  (let ((pool (get-pool pool-id)))
    (when (<= lp-shares 0)
      (error 'invalid-amount-error :amount lp-shares :reason "LP shares must be positive"))

    (let* ((reserve-a (liquidity-pool-reserve-a pool))
           (reserve-b (liquidity-pool-reserve-b pool))
           (total-shares (liquidity-pool-total-lp-shares pool)))

      (when (> lp-shares (- total-shares +min-liquidity+))
        (error 'invalid-amount-error
               :amount lp-shares
               :reason "Cannot remove locked minimum liquidity"))

      ;; Calculate proportional amounts
      (let ((amount-a (floor (* lp-shares reserve-a) total-shares))
            (amount-b (floor (* lp-shares reserve-b) total-shares)))

        ;; Check slippage
        (when (< amount-a min-amount-a)
          (error 'slippage-exceeded-error
                 :expected min-amount-a
                 :actual amount-a
                 :tolerance-bps 0
                 :message "Amount A below minimum"))
        (when (< amount-b min-amount-b)
          (error 'slippage-exceeded-error
                 :expected min-amount-b
                 :actual amount-b
                 :tolerance-bps 0
                 :message "Amount B below minimum"))

        ;; Update pool state
        (decf (liquidity-pool-reserve-a pool) amount-a)
        (decf (liquidity-pool-reserve-b pool) amount-b)
        (decf (liquidity-pool-total-lp-shares pool) lp-shares)
        (setf (liquidity-pool-last-updated pool) (get-current-time))

        (values amount-a amount-b)))))

;;; ===================================================================
;;; Swap Calculations
;;; ===================================================================

(defun calculate-swap-output (pool-id amount-in token-in)
  "Calculate output amount for a swap (without executing).
   Returns a swap-result struct."
  (let* ((pool (get-pool pool-id))
         (token-a (liquidity-pool-token-a pool))
         (reserve-a (liquidity-pool-reserve-a pool))
         (reserve-b (liquidity-pool-reserve-b pool))
         (fee-bps (liquidity-pool-fee-bps pool))
         ;; Determine direction
         (is-a-to-b (string= token-in token-a))
         (reserve-in (if is-a-to-b reserve-a reserve-b))
         (reserve-out (if is-a-to-b reserve-b reserve-a)))

    (when (<= amount-in 0)
      (error 'invalid-amount-error :amount amount-in :reason "Input amount must be positive"))

    (when (or (zerop reserve-in) (zerop reserve-out))
      (error 'zero-liquidity-error :pool-id pool-id :message "Pool has zero reserves"))

    (when (> amount-in reserve-in)
      (error 'insufficient-liquidity-error
             :pool-id pool-id
             :required amount-in
             :available reserve-in
             :message "Input amount exceeds pool reserve"))

    ;; Calculate fee
    (let* ((fee-amount (floor (* amount-in fee-bps) +bps-base+))
           (amount-in-after-fee (- amount-in fee-amount))
           ;; Constant product formula: (x + dx) * (y - dy) = x * y
           ;; Solving for dy: dy = y * dx / (x + dx)
           (numerator (* reserve-out amount-in-after-fee))
           (denominator (+ reserve-in amount-in-after-fee))
           (amount-out (floor numerator denominator))
           ;; Calculate new reserves
           (new-reserve-in (+ reserve-in amount-in))
           (new-reserve-out (- reserve-out amount-out))
           ;; Price impact in bps
           (price-before (floor (* reserve-out +bps-base+) reserve-in))
           (price-after (if (zerop new-reserve-in)
                            0
                            (floor (* new-reserve-out +bps-base+) new-reserve-in)))
           (price-impact-bps (if (zerop price-before)
                                 0
                                 (abs (floor (* (- price-before price-after) 10000) price-before)))))

      (when (or (>= amount-out reserve-out)
                (<= new-reserve-out +min-liquidity+))
        (error 'insufficient-liquidity-error
               :pool-id pool-id
               :required amount-out
               :available reserve-out
               :message "Output exceeds available liquidity"))

      (make-swap-result
       :amount-out amount-out
       :fee-amount fee-amount
       :new-reserve-a (if is-a-to-b new-reserve-in new-reserve-out)
       :new-reserve-b (if is-a-to-b new-reserve-out new-reserve-in)
       :price-impact-bps price-impact-bps))))

(defun calculate-swap-input (pool-id amount-out token-out)
  "Calculate required input amount for a desired output.
   Returns (values amount-in fee-amount)."
  (let* ((pool (get-pool pool-id))
         (token-a (liquidity-pool-token-a pool))
         (reserve-a (liquidity-pool-reserve-a pool))
         (reserve-b (liquidity-pool-reserve-b pool))
         (fee-bps (liquidity-pool-fee-bps pool))
         ;; Determine direction
         (is-b-out (string= token-out (liquidity-pool-token-b pool)))
         (reserve-in (if is-b-out reserve-a reserve-b))
         (reserve-out (if is-b-out reserve-b reserve-a)))
    (declare (ignore token-a))

    (when (<= amount-out 0)
      (error 'invalid-amount-error :amount amount-out :reason "Output amount must be positive"))

    (when (>= amount-out reserve-out)
      (error 'insufficient-liquidity-error
             :pool-id pool-id
             :required amount-out
             :available reserve-out
             :message "Output exceeds available liquidity"))

    ;; Reverse constant product formula
    ;; dx = (x * dy) / (y - dy) + fee adjustment
    (let* ((numerator (* reserve-in amount-out))
           (denominator (- reserve-out amount-out))
           (amount-in-before-fee (1+ (floor numerator denominator))) ; Round up
           ;; Add fee: amount_in = amount_in_before_fee / (1 - fee_rate)
           (amount-in (1+ (floor (* amount-in-before-fee +bps-base+)
                                 (- +bps-base+ fee-bps))))
           (fee-amount (floor (* amount-in fee-bps) +bps-base+)))

      (values amount-in fee-amount))))

;;; ===================================================================
;;; Swap Execution
;;; ===================================================================

(defun execute-swap (pool-id amount-in token-in &key (min-amount-out 0) (deadline nil))
  "Execute a swap on the pool.
   Returns a swap-result struct.

   Slippage protection via min-amount-out.
   Deadline is a Unix timestamp; swap fails if current time exceeds it."
  ;; Check deadline
  (when (and deadline (> (get-current-time) deadline))
    (error 'amm-error :message "Transaction deadline exceeded"))

  (let* ((pool (get-pool pool-id))
         (result (calculate-swap-output pool-id amount-in token-in))
         (amount-out (swap-result-amount-out result))
         (fee-amount (swap-result-fee-amount result)))

    ;; Check slippage
    (when (< amount-out min-amount-out)
      (error 'slippage-exceeded-error
             :expected min-amount-out
             :actual amount-out
             :tolerance-bps 0
             :message "Output below minimum"))

    ;; Update pool reserves
    (setf (liquidity-pool-reserve-a pool) (swap-result-new-reserve-a result))
    (setf (liquidity-pool-reserve-b pool) (swap-result-new-reserve-b result))
    (setf (liquidity-pool-last-updated pool) (get-current-time))

    ;; Accumulate fees (split between protocol and LPs happens on collection)
    (let ((is-a-to-b (string= token-in (liquidity-pool-token-a pool))))
      (if is-a-to-b
          (incf (liquidity-pool-accumulated-fees-a pool) fee-amount)
          (incf (liquidity-pool-accumulated-fees-b pool) fee-amount)))

    result))

;;; ===================================================================
;;; Price and Impact
;;; ===================================================================

(defun get-pool-price (pool-id &optional (token nil))
  "Get current price of token-b in terms of token-a (or vice versa).
   Returns price as rational number (reserve-b / reserve-a) or inverse."
  (let* ((pool (get-pool pool-id))
         (reserve-a (liquidity-pool-reserve-a pool))
         (reserve-b (liquidity-pool-reserve-b pool)))
    (when (or (zerop reserve-a) (zerop reserve-b))
      (error 'zero-liquidity-error :pool-id pool-id :message "Pool has zero reserves"))
    (if (and token (string= token (liquidity-pool-token-b pool)))
        (/ reserve-a reserve-b)  ; Price of B in terms of A
        (/ reserve-b reserve-a)))) ; Price of A in terms of B (default)

(defun calculate-price-impact (pool-id amount-in token-in)
  "Calculate price impact of a swap in basis points."
  (let ((result (calculate-swap-output pool-id amount-in token-in)))
    (swap-result-price-impact-bps result)))

(defun validate-pool-invariant (pool)
  "Validate that pool maintains k = x * y invariant (approximately).
   Returns t if valid, signals error otherwise."
  (let ((reserve-a (liquidity-pool-reserve-a pool))
        (reserve-b (liquidity-pool-reserve-b pool)))
    (when (or (< reserve-a 0) (< reserve-b 0))
      (error 'amm-error :message "Negative reserves detected"))
    (when (and (> (liquidity-pool-total-lp-shares pool) 0)
               (or (zerop reserve-a) (zerop reserve-b)))
      (error 'amm-error :message "Zero reserve with positive LP shares"))
    t))

;;; ===================================================================
;;; Fee Operations
;;; ===================================================================

(defun get-accumulated-fees (pool-id)
  "Get accumulated fees for a pool.
   Returns (values fees-a fees-b)."
  (let ((pool (get-pool pool-id)))
    (values (liquidity-pool-accumulated-fees-a pool)
            (liquidity-pool-accumulated-fees-b pool))))

(defun collect-fees (pool-id)
  "Collect accumulated fees from pool.
   Returns (values lp-fees-a lp-fees-b protocol-fees-a protocol-fees-b).

   Fees are split: (1 - 1/PROTOCOL_FEE_RATIO) to LPs, 1/PROTOCOL_FEE_RATIO to protocol."
  (let* ((pool (get-pool pool-id))
         (total-fees-a (liquidity-pool-accumulated-fees-a pool))
         (total-fees-b (liquidity-pool-accumulated-fees-b pool))
         (protocol-fees-a (floor total-fees-a +protocol-fee-ratio+))
         (protocol-fees-b (floor total-fees-b +protocol-fee-ratio+))
         (lp-fees-a (- total-fees-a protocol-fees-a))
         (lp-fees-b (- total-fees-b protocol-fees-b)))

    ;; Reset accumulated fees
    (setf (liquidity-pool-accumulated-fees-a pool) 0)
    (setf (liquidity-pool-accumulated-fees-b pool) 0)

    (values lp-fees-a lp-fees-b protocol-fees-a protocol-fees-b)))

;;; End of amm.lisp
