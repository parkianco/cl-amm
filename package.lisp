;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; package.lisp - Package definition for cl-amm

(defpackage #:cl-amm
  (:use #:cl)
  (:export
   ;;; ===================================================================
   ;;; Core Types
   ;;; ===================================================================

   ;; Liquidity Pool
   #:liquidity-pool
   #:make-liquidity-pool
   #:liquidity-pool-id
   #:liquidity-pool-token-a
   #:liquidity-pool-token-b
   #:liquidity-pool-reserve-a
   #:liquidity-pool-reserve-b
   #:liquidity-pool-total-lp-shares
   #:liquidity-pool-fee-bps
   #:liquidity-pool-protocol-fee-bps
   #:liquidity-pool-accumulated-fees-a
   #:liquidity-pool-accumulated-fees-b
   #:liquidity-pool-created-at
   #:liquidity-pool-last-updated

   ;; LP Share
   #:lp-share
   #:make-lp-share
   #:lp-share-holder
   #:lp-share-pool-id
   #:lp-share-amount

   ;; Swap Result
   #:swap-result
   #:make-swap-result
   #:swap-result-amount-out
   #:swap-result-fee-amount
   #:swap-result-new-reserve-a
   #:swap-result-new-reserve-b
   #:swap-result-price-impact-bps

   ;;; ===================================================================
   ;;; Constants
   ;;; ===================================================================
   #:+fee-bps-low+
   #:+fee-bps-medium+
   #:+fee-bps-high+
   #:+fee-bps-very-high+
   #:+protocol-fee-ratio+
   #:+min-liquidity+
   #:+bps-base+

   ;;; ===================================================================
   ;;; Pool Operations
   ;;; ===================================================================
   #:create-pool
   #:add-liquidity
   #:remove-liquidity
   #:calculate-swap-output
   #:calculate-swap-input
   #:execute-swap
   #:get-pool-price
   #:calculate-price-impact
   #:validate-pool-invariant

   ;;; ===================================================================
   ;;; Fee Operations
   ;;; ===================================================================
   #:collect-fees
   #:get-accumulated-fees

   ;;; ===================================================================
   ;;; Registry Operations
   ;;; ===================================================================
   #:*pool-registry*
   #:*lp-shares*
   #:register-pool
   #:get-pool
   #:get-all-pools
   #:clear-registry

   ;;; ===================================================================
   ;;; Error Conditions
   ;;; ===================================================================
   #:amm-error
   #:insufficient-liquidity-error
   #:slippage-exceeded-error
   #:invalid-amount-error
   #:pool-not-found-error
   #:pool-exists-error
   #:zero-liquidity-error))
