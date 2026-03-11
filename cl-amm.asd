;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; cl-amm.asd - Automated Market Maker System Definition

(defsystem #:cl-amm
  :name "cl-amm"
  :version "1.0.0"
  :author "Parkian Company LLC"
  :license "BSD-3-Clause"
  :description "Constant Product Automated Market Maker (x*y=k)"
  :long-description "A standalone Common Lisp implementation of an Automated Market Maker
using the constant product formula. Supports liquidity pools, LP token minting/burning,
fee collection, and swap execution with slippage protection."
  :depends-on ()
  :serial t
  :components ((:file "package")
               (:module "src"
                :serial t
                :components ((:file "types")
                             (:file "amm"))))
  :in-order-to ((test-op (test-op #:cl-amm/test))))

(defsystem #:cl-amm/test
  :name "cl-amm/test"
  :version "1.0.0"
  :author "Parkian Company LLC"
  :license "BSD-3-Clause"
  :description "Tests for cl-amm"
  :depends-on (#:cl-amm)
  :serial t
  :components ((:module "test"
                :serial t
                :components ((:file "amm-test"))))
  :perform (test-op (op c)
             (uiop:symbol-call :cl-amm/test :run-tests)))
