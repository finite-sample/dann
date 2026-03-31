# dann

[![R-CMD-check](https://github.com/finite-sample/dann/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/finite-sample/dann/actions/workflows/R-CMD-check.yaml)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Trevor Hastie’s Original Fortran Implementation of Discriminant Adaptive
Nearest Neighbors

## What is DANN?

Discriminant Adaptive Nearest Neighbor (DANN) classification improves on
standard k-nearest neighbors by adapting the distance metric locally.
Instead of using Euclidean distance (which treats all directions
equally), DANN:

1.  Computes a local discriminant analysis around each query point
2.  Stretches the neighborhood along class boundaries
3.  Shrinks it in the direction orthogonal to boundaries

This makes DANN particularly effective when classes have different
covariance structures or when irrelevant features are present.

## Why This Package?

This package preserves **Trevor Hastie’s original Fortran
implementation** of DANN, converted from Ratfor. The Fortran backend
provides computational efficiency for the core algorithm.

**Note**: A different `dann` package exists on CRAN (by gmcmacran),
which is an R/C++ reimplementation based on a Python port. This package
uses Hastie’s original code.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("soodoku/dann")
```

## Quick Example

``` r
library(dann)

# Load included glass dataset
data(dannt)

# Standardize training and test data
std <- stand(glass.train$x, glass.test$x)

# Standard k-NN
knn_pred <- knn(std$x, std$xx, glass.train$y, k = 5)
knn_acc <- mean(knn_pred == glass.test$y)

# DANN
dann_pred <- dann(std$x, std$xx, glass.train$y, k = 5, kmetric = 50)
dann_acc <- mean(dann_pred == glass.test$y)

cat("5-NN accuracy:", round(knn_acc, 3), "\n")
cat("DANN accuracy:", round(dann_acc, 3), "\n")
```

## Key Parameters

| Parameter | Description                                        | Default |
|-----------|----------------------------------------------------|---------|
| `k`       | Number of neighbors for final classification       | 5       |
| `kmetric` | Number of neighbors for computing the local metric | 50      |
| `epsilon` | Softening parameter (larger = more regularization) | 1       |
| `fullw`   | Use full (non-diagonal) covariance matrix          | FALSE   |
| `scalar`  | Use scalar (isotropic) covariance                  | FALSE   |

## Authors

- **Trevor Hastie** - Original algorithm and Fortran implementation
- **Gaurav Sood** - R interface, packaging, and maintenance

## License

MIT

## References

Hastie, T., & Tibshirani, R. (1996). Discriminant Adaptive Nearest
Neighbor Classification. *IEEE Transactions on Pattern Analysis and
Machine Intelligence*, 18(6), 607-616.
[PDF](https://web.stanford.edu/~hastie/Papers/dann_IEEE.pdf)
