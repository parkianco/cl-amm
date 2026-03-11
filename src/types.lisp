;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; types.lisp - Core data structures for cl-amm

(in-package #:cl-amm)

;;; ===================================================================
;;; Constants
;;; ===================================================================

;; Fee tiers in basis points (1 bps = 0.01%)
(defconstant +fee-bps-low+ 100)        ; 0.01% (1 bps = stablecoin pairs)
(defconstant +fee-bps-medium+ 500)     ; 0.05% (standard pairs)
(defconstant +fee-bps-high+ 3000)      ; 0.30% (exotic pairs)
(defconstant +fee-bps-very-high+ 10000) ; 1.00% (highly volatile)

;; Protocol fee as ratio of trading fees (1/6 = ~16.67%)
(defconstant +protocol-fee-ratio+ 6)

;; Minimum liquidity locked forever to prevent zero-liquidity attacks
(defconstant +min-liquidity+ 1000)

;; Basis points denominator
(defconstant +bps-base+ 1000000)

;;; ===================================================================
;;; Error Conditions
;;; ===================================================================

(define-condition amm-error (error)
  ((message :initarg :message :reader amm-error-message))
  (:report (lambda (c s) (format s "AMM Error: ~A" (amm-error-message c)))))

(define-condition insufficient-liquidity-error (amm-error)
  ((pool-id :initarg :pool-id :reader insufficient-liquidity-pool-id)
   (required :initarg :required :reader insufficient-liquidity-required)
   (available :initarg :available :reader insufficient-liquidity-available))
  (:report (lambda (c s)
             (format s "Insufficient liquidity in pool ~A: required ~A, available ~A"
                     (insufficient-liquidity-pool-id c)
                     (insufficient-liquidity-required c)
                     (insufficient-liquidity-available c)))))

(define-condition slippage-exceeded-error (amm-error)
  ((expected :initarg :expected :reader slippage-expected)
   (actual :initarg :actual :reader slippage-actual)
   (tolerance-bps :initarg :tolerance-bps :reader slippage-tolerance-bps))
  (:report (lambda (c s)
             (format s "Slippage exceeded: expected ~A, got ~A (tolerance: ~A bps)"
                     (slippage-expected c)
                     (slippage-actual c)
                     (slippage-tolerance-bps c)))))

(define-condition invalid-amount-error (amm-error)
  ((amount :initarg :amount :reader invalid-amount-value)
   (reason :initarg :reason :reader invalid-amount-reason))
  (:report (lambda (c s)
             (format s "Invalid amount ~A: ~A"
                     (invalid-amount-value c)
                     (invalid-amount-reason c)))))

(define-condition pool-not-found-error (amm-error)
  ((pool-id :initarg :pool-id :reader pool-not-found-id))
  (:report (lambda (c s)
             (format s "Pool not found: ~A" (pool-not-found-id c)))))

(define-condition pool-exists-error (amm-error)
  ((pool-id :initarg :pool-id :reader pool-exists-id))
  (:report (lambda (c s)
             (format s "Pool already exists: ~A" (pool-exists-id c)))))

(define-condition zero-liquidity-error (amm-error)
  ((pool-id :initarg :pool-id :reader zero-liquidity-pool-id))
  (:report (lambda (c s)
             (format s "Pool has zero liquidity: ~A" (zero-liquidity-pool-id c)))))

;;; ===================================================================
;;; Core Structures
;;; ===================================================================

(defstruct liquidity-pool
  "A constant product AMM liquidity pool (x * y = k)."
  (id nil :type (or string null))
  (token-a nil :type (or string null))
  (token-b nil :type (or string null))
  (reserve-a 0 :type integer)
  (reserve-b 0 :type integer)
  (total-lp-shares 0 :type integer)
  (fee-bps +fee-bps-medium+ :type integer)
  (protocol-fee-bps 0 :type integer)
  (accumulated-fees-a 0 :type integer)
  (accumulated-fees-b 0 :type integer)
  (created-at 0 :type integer)
  (last-updated 0 :type integer))

(defstruct lp-share
  "Represents a liquidity provider's share in a pool."
  (holder nil :type (or string null))
  (pool-id nil :type (or string null))
  (amount 0 :type integer))

(defstruct swap-result
  "Result of a swap calculation or execution."
  (amount-out 0 :type integer)
  (fee-amount 0 :type integer)
  (new-reserve-a 0 :type integer)
  (new-reserve-b 0 :type integer)
  (price-impact-bps 0 :type integer))

;;; ===================================================================
;;; Global State
;;; ===================================================================

(defvar *pool-registry* (make-hash-table :test 'equal)
  "Registry of all liquidity pools indexed by pool-id.")

(defvar *lp-shares* (make-hash-table :test 'equal)
  "LP shares indexed by (pool-id . holder) cons.")

;;; ===================================================================
;;; Registry Operations
;;; ===================================================================

(defun register-pool (pool)
  "Register a pool in the global registry."
  (let ((id (liquidity-pool-id pool)))
    (when (gethash id *pool-registry*)
      (error 'pool-exists-error :pool-id id :message "Pool already exists"))
    (setf (gethash id *pool-registry*) pool)
    pool))

(defun get-pool (pool-id)
  "Get a pool by ID. Signals pool-not-found-error if not found."
  (or (gethash pool-id *pool-registry*)
      (error 'pool-not-found-error :pool-id pool-id :message "Pool not found")))

(defun get-all-pools ()
  "Return a list of all registered pools."
  (let ((pools nil))
    (maphash (lambda (k v) (declare (ignore k)) (push v pools)) *pool-registry*)
    (nreverse pools)))

(defun clear-registry ()
  "Clear all registries. Useful for testing."
  (clrhash *pool-registry*)
  (clrhash *lp-shares*)
  t)

;;; End of types.lisp
