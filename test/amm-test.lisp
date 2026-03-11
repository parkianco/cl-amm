;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; amm-test.lisp - Tests for cl-amm

(defpackage #:cl-amm/test
  (:use #:cl #:cl-amm)
  (:export #:run-tests))

(in-package #:cl-amm/test)

;;; ===================================================================
;;; Test Infrastructure
;;; ===================================================================

(defvar *tests* nil "List of test functions")
(defvar *passed* 0 "Count of passed tests")
(defvar *failed* 0 "Count of failed tests")

(defmacro deftest (name &body body)
  "Define a test function."
  `(progn
     (defun ,name ()
       (handler-case
           (progn ,@body t)
         (error (e)
           (format t "~&FAIL: ~A - ~A~%" ',name e)
           nil)))
     (pushnew ',name *tests*)))

(defmacro assert-equal (expected actual &optional message)
  "Assert that expected equals actual."
  `(unless (equal ,expected ,actual)
     (error "~A~%  Expected: ~S~%  Actual: ~S"
            (or ,message "Assertion failed")
            ,expected ,actual)))

(defmacro assert-true (expr &optional message)
  "Assert that expression is true."
  `(unless ,expr
     (error "~A: ~S is not true" (or ,message "Assertion failed") ',expr)))

(defmacro assert-error (error-type &body body)
  "Assert that body signals an error of the given type."
  `(handler-case
       (progn ,@body
              (error "Expected ~A but no error was signaled" ',error-type))
     (,error-type () t)
     (error (e)
       (error "Expected ~A but got ~A" ',error-type (type-of e)))))

;;; ===================================================================
;;; Pool Creation Tests
;;; ===================================================================

(deftest test-create-pool-basic
  (cl-amm:clear-registry)
  (multiple-value-bind (pool shares)
      (cl-amm:create-pool "ETH" "USDC" 1000000 2000000000)
    (assert-true (cl-amm:liquidity-pool-p pool))
    (assert-equal "ETH/USDC" (cl-amm:liquidity-pool-id pool))
    (assert-equal 1000000 (cl-amm:liquidity-pool-reserve-a pool))
    (assert-equal 2000000000 (cl-amm:liquidity-pool-reserve-b pool))
    (assert-true (> shares 0) "Should mint LP shares")))

(deftest test-create-pool-canonical-ordering
  (cl-amm:clear-registry)
  ;; Creating with reversed order should still give canonical ID
  (multiple-value-bind (pool shares)
      (cl-amm:create-pool "USDC" "ETH" 2000000000 1000000)
    (declare (ignore shares))
    (assert-equal "ETH/USDC" (cl-amm:liquidity-pool-id pool))
    ;; Token A should be ETH (comes first alphabetically)
    (assert-equal "ETH" (cl-amm:liquidity-pool-token-a pool))
    (assert-equal "USDC" (cl-amm:liquidity-pool-token-b pool))))

(deftest test-create-pool-duplicate-error
  (cl-amm:clear-registry)
  (cl-amm:create-pool "ETH" "USDC" 1000000 2000000000)
  (assert-error cl-amm:pool-exists-error
    (cl-amm:create-pool "ETH" "USDC" 500000 1000000000)))

(deftest test-create-pool-zero-amount-error
  (cl-amm:clear-registry)
  (assert-error cl-amm:invalid-amount-error
    (cl-amm:create-pool "ETH" "USDC" 0 2000000000)))

;;; ===================================================================
;;; Liquidity Tests
;;; ===================================================================

(deftest test-add-liquidity
  (cl-amm:clear-registry)
  (cl-amm:create-pool "ETH" "USDC" 1000000 2000000000)
  (multiple-value-bind (actual-a actual-b shares)
      (cl-amm:add-liquidity "ETH/USDC" 500000 1000000000)
    (assert-true (> shares 0) "Should mint LP shares")
    (assert-equal 500000 actual-a)
    (assert-equal 1000000000 actual-b))
  ;; Verify pool state
  (let ((pool (cl-amm:get-pool "ETH/USDC")))
    (assert-equal 1500000 (cl-amm:liquidity-pool-reserve-a pool))
    (assert-equal 3000000000 (cl-amm:liquidity-pool-reserve-b pool))))

(deftest test-add-liquidity-ratio-adjustment
  (cl-amm:clear-registry)
  (cl-amm:create-pool "ETH" "USDC" 1000000 2000000000)
  ;; Try to add with wrong ratio - should adjust
  (multiple-value-bind (actual-a actual-b shares)
      (cl-amm:add-liquidity "ETH/USDC" 500000 500000000) ; Too little USDC
    (declare (ignore shares))
    ;; Should use all USDC and proportional ETH
    (assert-equal 250000 actual-a)
    (assert-equal 500000000 actual-b)))

(deftest test-remove-liquidity
  (cl-amm:clear-registry)
  (multiple-value-bind (pool shares)
      (cl-amm:create-pool "ETH" "USDC" 1000000 2000000000)
    (declare (ignore pool))
    ;; Remove half the shares
    (let ((remove-shares (floor shares 2)))
      (multiple-value-bind (amount-a amount-b)
          (cl-amm:remove-liquidity "ETH/USDC" remove-shares)
        (assert-true (> amount-a 0))
        (assert-true (> amount-b 0))))))

;;; ===================================================================
;;; Swap Tests
;;; ===================================================================

(deftest test-calculate-swap-output
  (cl-amm:clear-registry)
  (cl-amm:create-pool "ETH" "USDC" 1000000 2000000000 :fee-bps 3000) ; 0.3%
  (let ((result (cl-amm:calculate-swap-output "ETH/USDC" 10000 "ETH")))
    (assert-true (cl-amm:swap-result-p result))
    (assert-true (> (cl-amm:swap-result-amount-out result) 0))
    (assert-true (> (cl-amm:swap-result-fee-amount result) 0))
    ;; Output should be less than proportional due to curve and fees
    (assert-true (< (cl-amm:swap-result-amount-out result) 20000000))))

(deftest test-execute-swap
  (cl-amm:clear-registry)
  (cl-amm:create-pool "ETH" "USDC" 1000000 2000000000 :fee-bps 3000)
  (let* ((result (cl-amm:execute-swap "ETH/USDC" 10000 "ETH"))
         (pool (cl-amm:get-pool "ETH/USDC")))
    (assert-true (> (cl-amm:swap-result-amount-out result) 0))
    ;; Reserves should be updated
    (assert-equal 1010000 (cl-amm:liquidity-pool-reserve-a pool))
    (assert-true (< (cl-amm:liquidity-pool-reserve-b pool) 2000000000))))

(deftest test-swap-slippage-protection
  (cl-amm:clear-registry)
  (cl-amm:create-pool "ETH" "USDC" 1000000 2000000000)
  ;; Set unreasonably high min output
  (assert-error cl-amm:slippage-exceeded-error
    (cl-amm:execute-swap "ETH/USDC" 10000 "ETH" :min-amount-out 999999999)))

(deftest test-swap-insufficient-liquidity
  (cl-amm:clear-registry)
  (cl-amm:create-pool "ETH" "USDC" 1000000 2000000000)
  ;; Try to swap more than reserve
  (assert-error cl-amm:insufficient-liquidity-error
    (cl-amm:calculate-swap-output "ETH/USDC" 100000000000 "ETH")))

;;; ===================================================================
;;; Price Tests
;;; ===================================================================

(deftest test-get-pool-price
  (cl-amm:clear-registry)
  (cl-amm:create-pool "ETH" "USDC" 1000000 2000000000)
  (let ((price (cl-amm:get-pool-price "ETH/USDC")))
    ;; 1 ETH = 2000 USDC
    (assert-equal 2000 price)))

(deftest test-price-impact
  (cl-amm:clear-registry)
  (cl-amm:create-pool "ETH" "USDC" 1000000 2000000000)
  ;; Small trade should have small impact
  (let ((small-impact (cl-amm:calculate-price-impact "ETH/USDC" 100 "ETH")))
    (assert-true (< small-impact 10) "Small trade should have <10 bps impact"))
  ;; Large trade should have larger impact
  (let ((large-impact (cl-amm:calculate-price-impact "ETH/USDC" 100000 "ETH")))
    (assert-true (> large-impact small-impact))))

;;; ===================================================================
;;; Fee Tests
;;; ===================================================================

(deftest test-fee-accumulation
  (cl-amm:clear-registry)
  (cl-amm:create-pool "ETH" "USDC" 1000000 2000000000 :fee-bps 3000)
  ;; Execute some swaps
  (cl-amm:execute-swap "ETH/USDC" 10000 "ETH")
  (cl-amm:execute-swap "ETH/USDC" 20000 "ETH")
  ;; Check accumulated fees
  (multiple-value-bind (fees-a fees-b)
      (cl-amm:get-accumulated-fees "ETH/USDC")
    (assert-true (> fees-a 0) "Should have accumulated fees in token A")
    (assert-equal 0 fees-b "No swaps in token B direction")))

(deftest test-fee-collection
  (cl-amm:clear-registry)
  (cl-amm:create-pool "ETH" "USDC" 1000000 2000000000 :fee-bps 3000)
  (cl-amm:execute-swap "ETH/USDC" 100000 "ETH")
  (multiple-value-bind (lp-a lp-b proto-a proto-b)
      (cl-amm:collect-fees "ETH/USDC")
    (assert-true (> lp-a 0) "LP should receive fees")
    (assert-true (> proto-a 0) "Protocol should receive fees")
    (assert-equal 0 lp-b)
    (assert-equal 0 proto-b)
    ;; LP fees should be greater than protocol fees
    (assert-true (> lp-a proto-a)))
  ;; Fees should be reset
  (multiple-value-bind (fees-a fees-b)
      (cl-amm:get-accumulated-fees "ETH/USDC")
    (assert-equal 0 fees-a)
    (assert-equal 0 fees-b)))

;;; ===================================================================
;;; Invariant Tests
;;; ===================================================================

(deftest test-constant-product-invariant
  (cl-amm:clear-registry)
  (cl-amm:create-pool "ETH" "USDC" 1000000 2000000000)
  (let* ((pool (cl-amm:get-pool "ETH/USDC"))
         (k-before (* (cl-amm:liquidity-pool-reserve-a pool)
                      (cl-amm:liquidity-pool-reserve-b pool))))
    ;; Execute swap
    (cl-amm:execute-swap "ETH/USDC" 10000 "ETH")
    (let ((k-after (* (cl-amm:liquidity-pool-reserve-a pool)
                      (cl-amm:liquidity-pool-reserve-b pool))))
      ;; k should increase (due to fees) or stay same
      (assert-true (>= k-after k-before)
                   "Invariant should not decrease"))))

;;; ===================================================================
;;; Test Runner
;;; ===================================================================

(defun run-tests ()
  "Run all tests and report results."
  (setf *passed* 0 *failed* 0)
  (format t "~&Running cl-amm tests...~%")
  (format t "~&========================================~%")
  (dolist (test (reverse *tests*))
    (format t "~&  ~A... " test)
    (if (funcall test)
        (progn
          (format t "OK~%")
          (incf *passed*))
        (incf *failed*)))
  (format t "~&========================================~%")
  (format t "~&Results: ~A passed, ~A failed~%~%" *passed* *failed*)
  (zerop *failed*))

;;; End of amm-test.lisp
