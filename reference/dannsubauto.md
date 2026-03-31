# dannsubauto

dannsubauto

## Usage

``` r
dannsubauto(
  x,
  testx = matrix(double(p), nrow = 1),
  y,
  k = 5,
  kmetric = max(0.2 * n, 50),
  epsilon = 1,
  plus = 1,
  trace = FALSE,
  ...
)
```

## Arguments

- x:

  mat

- testx:

  test mat

- y:

  labels

- k:

  k

- kmetric:

  kmet

- epsilon:

  epsilon

- plus:

  1

- trace:

  Boolean

- ...:

  Additional arguments passed to underlying functions

## Value

A list with items including Name of the Application, No. of pages
remaining (given the money), No. of fields remaining (given the money),
and when the application credits expire.
