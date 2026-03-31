#' dann
#'
#' Run Discriminant Adaptive Nearest Neighbors
#'
#' @param x     covariates matrix
#' @param testx test covariate matrix
#' @param y  labels
#' @param k  number of clusters
#' @param kmetric metric
#' @param epsilon epsilon
#' @param fullw  Boolean
#' @param scalar Boolean
#' @param iter   maximum number of iterations
#' @param covmin cov
#' @param cv boolean reflecting whether to cross-validate or not
#'
#'
#' @return Predicted class labels
#'
#' @export
#'
#' @examples \dontrun{
#' dann(x <- matrix(rnorm(120,1,.2)), testx <- glass.test$x, y <- matrix(rnorm(120,1,.5)),
#' epsilon = 1, fullw = FALSE, iter = 100,  covmin = 1e-04, cv = FALSE)
#' }

dann <- function(x, testx = NULL, y, k = 5,
                 kmetric = NULL, epsilon = 1, fullw = FALSE, scalar = FALSE, iter = 1,
                 covmin = 1e-04, cv = FALSE) {

  x <- as.matrix(x)
  assertMatrix(x, mode = "numeric", any.missing = FALSE, min.rows = 2)
  assertIntegerish(y, len = nrow(x), any.missing = FALSE)
  assertCount(k, positive = TRUE)
  assertNumeric(epsilon, lower = 0, any.missing = FALSE)
  assertFlag(fullw)
  assertFlag(scalar)
  assertCount(iter, positive = TRUE)
  assertNumber(covmin, lower = 0, finite = TRUE)
  assertFlag(cv)

  np <- dim(x)
  p <- np[2]
  n <- np[1]

  if (is.null(kmetric)) kmetric <- max(50, 0.2 * n)
  assertCount(kmetric, positive = TRUE)

  if (is.null(testx)) testx <- matrix(0, nrow = 1, ncol = p)
  testx <- as.matrix(testx)
  if (!cv) {
    assertMatrix(testx, mode = "numeric", any.missing = FALSE, ncols = p)
  } else {
    assertMatrix(testx, mode = "numeric", ncols = p)
  }

  nclass <- as.integer(length(unique(y)))
  if (nclass < 2L)
    stop("y must have at least 2 classes")
  if (k > n)
    stop("k cannot exceed number of observations")
  if (kmetric > n)
    stop("kmetric cannot exceed number of observations")

  storage.mode(x)  <- "double"
  storage.mode(testx) <- "double"
  storage.mode(y)  <- "integer"
  storage.mode(epsilon) <- "double"
  neps <- as.integer(length(epsilon))

  if (cv) {
    ntest <- as.integer(n)
  } else {
    ntest <- as.integer(nrow(testx))
  }

  pred <- matrix(integer(ntest * neps), nrow = ntest, ncol = neps,
                 dimnames = list(NULL, format(round(epsilon, 5))))

  out <- .Fortran("dann",
           as.integer(np[1]), as.integer(np[2]), x, y, nclass, t(testx),
           as.logical(cv), ntest,
           pred = pred, as.integer(kmetric), as.integer(k), as.integer(iter),
           as.logical(fullw), as.logical(scalar), epsilon,
           neps, integer(n), double(n), matrix(double(p^2), nrow = p, ncol = p),
           as.double(covmin), matrix(double(nclass * p), nrow = nclass, ncol = p),
           double(n), as.single(runif(ntest)),
           double(n + 2 * p^2 + 3 * p), PACKAGE = "dann")
  out$pred
}
