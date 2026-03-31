# mydann

mydann

## Usage

``` r
mydann(
  train.data,
  test.data,
  p = dim(train.data$x)[2],
  kmetric = max(50, 0.2 * length(train.data$y)),
  k = 5,
  epsilon.list = c(1, 0.5, 2, 5),
  iter.list = 1,
  ...
)
```

## Arguments

- train.data:

  training data. All the features in matrix x. and class labels in y.

- test.data:

  test data. Same format as the test data

- p:

  dimensions of x

- kmetric:

  k met

- k:

  Number of nearest neighbors

- epsilon.list:

  epsilon

- iter.list:

  iteration

## Value

results

## Examples

``` r
if (FALSE) { # \dontrun{ 
mydann()
} # }
```
