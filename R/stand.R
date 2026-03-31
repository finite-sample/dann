#' stand
#'
#' Standardize each column of both of the matrices with mean and standard deviation derived from the first matrix.
#'
#' @param x  matrix
#' @param xx another matrix, with same number of columns as x
#'
#' @return list of standardized matrices x and xx along with scaling information --- mean and standard deviation of original columns
#'
#' @export
#' @examples stand(matrix(1:6, ncol=3), matrix(1:6, ncol=3))

stand <- function(x, xx) {
  x <- as.matrix(x)
  xx <- as.matrix(xx)
  assertMatrix(x, mode = "numeric", any.missing = FALSE, min.rows = 1)
  assertMatrix(xx, mode = "numeric", any.missing = FALSE, ncols = ncol(x))

  mm <- apply(x, 2, mean)
  dd <- sqrt(apply(x, 2, var))
  x <- scale(x, center = mm, scale = dd)
  xx <- scale(xx, center = mm, scale = dd)

  list(x = x, xx = xx, mean = mm, sd = dd)
}
