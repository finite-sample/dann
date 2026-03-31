#' dann2
#'
#' @param x xmat
#' @param testx test mat 
#' @param y labels
#' @param k nearest neighbors
#' @param kmetric kmet
#' @param epsilon epsilon
#' @param rate rate
#' @param fullw fullw
#' @param scalar scalar
#' @param iter iter
#' @param covmin covmin
#' @param cv cv
#'
#' @return A list with items including Name of the Application, No. of pages remaining (given the money), 
#' No. of fields remaining (given the money), and when the application credits expire. 
#' @export

dann2 <- function(x, testx = NULL, y, k = 5, kmetric = NULL,
                  epsilon = 1, rate = 0.5, fullw = FALSE, scalar = FALSE, iter = 1,
                  covmin = 1e-04, cv = FALSE) {

  x <- as.matrix(x)
  assertMatrix(x, mode = "numeric", any.missing = FALSE, min.rows = 2)
  assertIntegerish(y, len = nrow(x), any.missing = FALSE)
  assertCount(k, positive = TRUE)
  assertNumeric(epsilon, lower = 0, any.missing = FALSE)
  assertNumber(rate, lower = 0, upper = 1)
  assertFlag(fullw)
  assertFlag(scalar)
  assertCount(iter, positive = TRUE)
  assertNumber(covmin, lower = 0, finite = TRUE)
  assertFlag(cv)

  np <- dim(x)
  p <- np[2]
  n <- np[1]

  if (is.null(kmetric)) kmetric <- as.integer(n / 2)
  assertCount(kmetric, positive = TRUE)

  if (is.null(testx)) testx <- matrix(double(p), nrow = 1)
  testx <- as.matrix(testx)
  assertMatrix(testx, mode = "numeric", ncols = p)

  nclass <- as.integer(length(unique(y)))
  if (nclass < 2L)
    stop("y must have at least 2 classes")
  if (k > n)
    stop("k cannot exceed number of observations")
  if (kmetric > n)
    stop("kmetric cannot exceed number of observations")

  storage.mode(x) <- "double"
  storage.mode(testx) <- "double"
  storage.mode(y) <- "integer"
  storage.mode(epsilon) <- "double"
  neps <- length(epsilon)

  if (cv)
    ntest <- n
  else ntest <- nrow(testx)
  
  predict <- matrix(integer(ntest * neps), ntest, neps, dimnames = list(NULL, format(round(epsilon, 5))))
  
  .Fortran("dann2", as.integer(np[1]), as.integer(np[2]), x, 
           y, as.integer(nclass), t(testx), as.logical(cv), as.integer(ntest), 
           predict, as.integer(c(kmetric, k, iter)), c(fullw, scalar), epsilon = epsilon, 
           as.integer(neps), integer(n), double(n), matrix(double(p^2), 
                                                           p, p), matrix(double(p^2), p, p), as.double(c(rate, 
                                                                                                        epsilon[1], covmin)), matrix(double(nclass * p), 
                                                                                                                                     nclass, p), double(n), as.single(runif(ntest)), double(n + 
                                                                                                                                                                                          2 * p^2 + 3 * p), x, y, PACKAGE = "dann")
  
  return(predict)
}
