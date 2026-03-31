# dannsub

dannsub

## Usage

``` r
dannsub(
  x,
  y,
  km = max(50, 0.2 * n),
  k = 5,
  epsilon = 1,
  fullw = FALSE,
  scalex = TRUE,
  scalar = TRUE,
  dims = seq(p),
  iter.sub = 1,
  iter = 1,
  method = "knn",
  xnew,
  ynew
)
```

## Arguments

- x:

  covariate matrix

- y:

  labels

- km:

  max subspace

- k:

  number of clusters

- epsilon:

  error

- fullw:

  Boolean

- scalex:

  Boolean, default true

- scalar:

  Boolean,

- dims:

  dims

- iter.sub:

  Max. number of subspace iterations

- iter:

  Max number of iterations

- method:

  knn

- xnew:

  new x

- ynew:

  new ys

## Value

an object of class dannsub

## Examples

``` r
if (FALSE) { # \dontrun{
dannsub(km = max(50, 0.2 * n), k = 5, epsilon = 1, fullw = FALSE, scalex = TRUE, 
   scalar = TRUE, dims = seq(p), iter.sub = 1, iter = 1, method = "knn", xnew, ynew)
} # }
```
