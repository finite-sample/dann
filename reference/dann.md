# dann

Run Discriminant Adaptive Nearest Neighbors

## Usage

``` r
dann(
  x,
  testx = NULL,
  y,
  k = 5,
  kmetric = NULL,
  epsilon = 1,
  fullw = FALSE,
  scalar = FALSE,
  iter = 1,
  covmin = 1e-04,
  cv = FALSE
)
```

## Arguments

- x:

  covariates matrix

- testx:

  test covariate matrix

- y:

  labels

- k:

  number of clusters

- kmetric:

  metric

- epsilon:

  epsilon

- fullw:

  Boolean

- scalar:

  Boolean

- iter:

  maximum number of iterations

- covmin:

  cov

- cv:

  boolean reflecting whether to cross-validate or not

## Value

Predicted class labels

## Examples

``` r
if (FALSE) { # \dontrun{
dann(x <- matrix(rnorm(120,1,.2)), testx <- glass.test$x, y <- matrix(rnorm(120,1,.5)),
epsilon = 1, fullw = FALSE, iter = 100,  covmin = 1e-04, cv = FALSE)
} # }
```
