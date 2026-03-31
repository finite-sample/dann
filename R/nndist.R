#' nndist
#'
#' Main function 
#' 
#' @param x  matrix
#' @param y  y
#' @param x0  colMeans of x
#' @param k  number of clusters
#' @param epsilon epsilon
#' @param fullw  fullw
#' @param scalar boolean
#' @param iter   number of iterations
#' @param cv cross-validate
#'
#'
#' @return A list with items including Name of the Application, No. of pages remaining (given the money), 
#' No. of fields remaining (given the money), and when the application credits expire. 
#' 
#' @export
#' 
#' @examples \dontrun{
#' nndist()
#'}

nndist <- function(x, y, x0 = NULL, k = NULL, epsilon = 1, fullw = FALSE, scalar = FALSE, iter = 1, cv = 0) {
  x <- as.matrix(x)
  assertMatrix(x, mode = "numeric", any.missing = FALSE, min.rows = 2)
  assertIntegerish(y, len = nrow(x), any.missing = FALSE)
  assertNumeric(epsilon, lower = 0, any.missing = FALSE)
  assertFlag(fullw)
  assertFlag(scalar)
  assertCount(iter, positive = TRUE)

  np <- dim(x)
  p <- np[2]
  n <- np[1]

  if (is.null(x0)) x0 <- matrix(apply(x, 2, mean), nrow = 1)
  x0 <- as.matrix(x0)
  assertMatrix(x0, mode = "numeric", any.missing = FALSE, ncols = p)

  if (is.null(k)) k <- as.integer(n / 2)
  assertCount(k, positive = TRUE)
  if (k > n)
    stop("k cannot exceed number of observations")

  nclass <- as.integer(length(unique(y)))
  if (nclass < 2L)
    stop("y must have at least 2 classes")

  storage.mode(x) <- "double"
  storage.mode(x0) <- "double"
  storage.mode(y) <- "integer"
  
  junk <- .Fortran("nndist", as.integer(np[1]), as.integer(np[2]), as.integer(nclass), as.integer(k), x, x0, as.integer(cv), y, as.integer(iter), fullw, scalar, epsilon = as.double(epsilon), which = integer(n), dist = double(n), covw = matrix(double(p^2), p, p), covmin = as.double(1e-04), means = matrix(double(nclass * p), nclass, p), weight = double(n), values = double(p), vectors = double(p * p), double(p * p), double(n + 2 * p), PACKAGE = "dann")
  
  return(junk)
}
