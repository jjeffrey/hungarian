# hungarian
Solves cost matrices via the hungarian method

##Usage

The functions `hungarianMin` and `hungarianMax` take in a cost matrix and return a matrix with zeros in optimal assignment locations. In `hungarianMin` it optimizes for the minimum sum, while in `hungarianMax` it optimizes for the maximum sum.

##Installation
To install globally, run:
```
runhaskell Setup.hs configure --ghc
runhaskell Setup.hs build
runhaskell Setup.hs install

```
