# stand

Standardize each column of both of the matrices with mean and standard
deviation derived from the first matrix.

## Usage

``` r
stand(x, xx)
```

## Arguments

- x:

  matrix

- xx:

  another matrix, with same number of columns as x

## Value

list of standardized matrices x and xx along with scaling information —
mean and standard deviation of original columns

## Examples

``` r
stand(matrix(1:6, ncol=3), matrix(1:6, ncol=3))
#> $x
#>            [,1]       [,2]       [,3]
#> [1,] -0.7071068 -0.7071068 -0.7071068
#> [2,]  0.7071068  0.7071068  0.7071068
#> attr(,"scaled:center")
#> [1] 1.5 3.5 5.5
#> attr(,"scaled:scale")
#> [1] 0.7071068 0.7071068 0.7071068
#> 
#> $xx
#>            [,1]       [,2]       [,3]
#> [1,] -0.7071068 -0.7071068 -0.7071068
#> [2,]  0.7071068  0.7071068  0.7071068
#> attr(,"scaled:center")
#> [1] 1.5 3.5 5.5
#> attr(,"scaled:scale")
#> [1] 0.7071068 0.7071068 0.7071068
#> 
#> $mean
#> [1] 1.5 3.5 5.5
#> 
#> $sd
#> [1] 0.7071068 0.7071068 0.7071068
#> 
```
