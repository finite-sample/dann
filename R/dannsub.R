#' dannsub
#'
#' @param x covariate matrix
#' @param km max subspace
#' @param y  labels
#' @param k  number of clusters
#' @param epsilon error
#' @param fullw  Boolean
#' @param scalex Boolean, default true
#' @param scalar Boolean, 
#' @param iter   Max number of iterations
#' @param iter.sub Max. number of subspace iterations
#' @param method  knn
#' @param dims dims
#' @param xnew new x
#' @param ynew new ys
#'
#' @return an object of class dannsub
#'
#' @examples \dontrun{
#' dannsub(km = max(50, 0.2 * n), k = 5, epsilon = 1, fullw = FALSE, scalex = TRUE, 
#'    scalar = TRUE, dims = seq(p), iter.sub = 1, iter = 1, method = "knn", xnew, ynew)
#' }
#' @export
dannsub <- function(x, y, km = max(50, 0.2 * n), k = 5, epsilon = 1, fullw = FALSE, scalex = TRUE, scalar = TRUE, dims = seq(p), iter.sub = 1, iter = 1, method = "knn", xnew, ynew) {
  if (scalex) {
    mm <- apply(x, 2, mean)
    dd <- sqrt(apply(x, 2, var))
    x <- scale(x, center = mm, scale = dd)
  }
  
  storage.mode(x) <- "double"
  storage.mode(y) <- "integer"
  nclass <- length(table(y))
  y <- as.integer(factor(y))
  np <- dim(x)
  p <- np[2]
  n <- np[1]
  
  errors <- matrix(0, length(dims), length(epsilon))
  dimnames(errors) <- list(format(round(dims, 0)), format(round(epsilon, 1)))
  
  between <- .Fortran("dannsub", as.integer(np[1]), as.integer(np[2]), x, y, as.integer(nclass), double(np[2]), as.integer(km), as.integer(iter.sub), fullw, scalar, epsilon = as.double(epsilon[1]), which = integer(n), dist = double(n), covw = matrix(double(p^2), p, p), covmin = as.double(1e-04), means = matrix(double(nclass * p), nclass, p), weight = double(n), between = matrix(double(p^2), p, p), double(n + 2 * p^2 + 3 * p), PACKAGE = "dann")$between
  
  eb <- eigen(between)
  x <- x %*% eb$vectors
  
  if (missing(xnew)) {
    if (method == "knn") {
      for (i in seq_along(dims)) {
        errors[i, ] <- attr(knncv(x[, seq(dims[i]), drop = FALSE], y, k), "error")
      }
    } else {
      for (i in seq_along(dims)) {
        errors[i, ] <- apply(y != dann(x[, seq(dims[i]), drop = FALSE], y = y, k = k, kmetric = km, epsilon = epsilon, cv = TRUE, iter = iter), 2, sum)
      }
    }
  } else {
    cat("# New x and y data for testing\n")
    
    if (scalex) {
      xnew <- scale(xnew, center = mm, scale = dd)
    }
    
    xnew <- xnew %*% eb$vectors
    
    if (method == "knn") {
      for (i in seq_along(dims)) {
        errors[i, ] <- sum(knn(x[, seq(dims[i]), drop = FALSE], xnew[, seq(dims[i]), drop = FALSE], y, k) != ynew)
      }
    } else {
      for (i in seq_along(dims)) {
        errors[i, ] <- apply(ynew != dann(x[, seq(dims[i]), drop = FALSE], xnew[, seq(dims[i]), drop = FALSE], y = y, k = k, kmetric = km, epsilon = epsilon, cv = FALSE, iter = iter), 2, sum)
      }
    }
  }
  
  eb$errors <- errors
  
  if (scalex) {
    eb$scale <- dd
    eb$centers <- mm
  }
  
  class(eb) <- "dannsub"
  eb
}
