#' mydann
#'
#' @param train.data training data. All the features in matrix x. and class labels in y.
#' @param test.data  test data. Same format as the test data
#' @param p dimensions of x
#' @param kmetric k met
#' @param k Number of nearest neighbors
#' @param epsilon.list epsilon
#' @param iter.list iteration
#' @param ... Additional arguments passed to dann2
#'
#' @return  results
#' @examples \dontrun{ 
#' mydann()
#' }

mydann <- function(train.data, test.data, p = dim(train.data$x)[2], kmetric = max(50, 0.2 * length(train.data$y)), k = 5, epsilon.list = c(1, 0.5, 2, 5), iter.list = 1, ...) {
  this.call <- match.call()
  x <- train.data$x[, seq(p)]
  y <- as.integer(factor(train.data$y))
  n <- length(y)
  mm <- apply(x, 2, var)
  x <- scale(x, center = FALSE, scale = sqrt(mm))
  xx <- test.data$x[, seq(p)]
  xx <- scale(xx, center = FALSE, scale = sqrt(mm))
  results <- matrix(0, nrow = length(iter.list), ncol = length(epsilon.list))
  dimnames(results) <- list(format(round(iter.list, 0)), format(round(epsilon.list, 2)))
  for (i in seq_along(iter.list)) {
    iter <- iter.list[i]
    a <- dann2(x, testx = xx, y = y, kmetric = kmetric, k = k, epsilon = epsilon.list, iter = iter, ...)
    aa <- apply(a != test.data$y, 2, sum)
    cat("iter", iter, "error", aa, "\n")
    results[i, ] <- aa
  }
  results
  kk <- knn(x, xx, y, k = 5)
  knnerror <- sum(kk != test.data$y)
  attr(results, "5nn") <- knnerror
  attr(results, "call") <- this.call
  results
}
