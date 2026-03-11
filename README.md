# cl-amm

A standalone Common Lisp implementation of a Constant Product Automated Market Maker (AMM).

## Overview

cl-amm implements the classic x*y=k AMM formula, providing:

- Liquidity pool creation with configurable fee tiers
- LP token minting/burning using geometric mean
- Swap execution with slippage protection
- Fee accumulation and distribution
- Price impact calculation

## Installation

Clone the repository and load with ASDF:

```lisp
(asdf:load-system :cl-amm)
```

## Quick Start

```lisp
(use-package :cl-amm)

;; Create a pool with 1 ETH and 2000 USDC (0.3% fee)
(create-pool "ETH" "USDC" 1000000 2000000000 :fee-bps 3000)

;; Add more liquidity
(add-liquidity "ETH/USDC" 500000 1000000000)

;; Check price
(get-pool-price "ETH/USDC")  ; => 2000

;; Execute a swap (10000 units of ETH)
(execute-swap "ETH/USDC" 10000 "ETH" :min-amount-out 19000000)

;; Calculate price impact before swapping
(calculate-price-impact "ETH/USDC" 100000 "ETH")

;; Remove liquidity
(remove-liquidity "ETH/USDC" 50000 :min-amount-a 25000 :min-amount-b 50000000)
```

## Fee Tiers

| Constant | Value | Rate |
|----------|-------|------|
| `+fee-bps-low+` | 100 | 0.01% |
| `+fee-bps-medium+` | 500 | 0.05% |
| `+fee-bps-high+` | 3000 | 0.30% |
| `+fee-bps-very-high+` | 10000 | 1.00% |

## API Reference

### Pool Operations

- `create-pool (token-a token-b amount-a amount-b &key fee-bps creator)` - Create a new pool
- `add-liquidity (pool-id amount-a amount-b &key provider min-lp-shares)` - Add liquidity
- `remove-liquidity (pool-id lp-shares &key provider min-amount-a min-amount-b)` - Remove liquidity

### Swap Operations

- `calculate-swap-output (pool-id amount-in token-in)` - Calculate output without executing
- `calculate-swap-input (pool-id amount-out token-out)` - Calculate required input
- `execute-swap (pool-id amount-in token-in &key min-amount-out deadline)` - Execute swap

### Price Functions

- `get-pool-price (pool-id &optional token)` - Get current spot price
- `calculate-price-impact (pool-id amount-in token-in)` - Calculate price impact in bps

### Fee Functions

- `get-accumulated-fees (pool-id)` - Get accumulated fees
- `collect-fees (pool-id)` - Collect and distribute fees

## Testing

```lisp
(asdf:test-system :cl-amm)
```

## License

BSD-3-Clause. See LICENSE file.
