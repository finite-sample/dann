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

dann <- function(x, testx = matrix(nrow = 1, ncol = p), y, k = 5,
                 kmetric = max(50, 0.2 * n), epsilon = 1, fullw = FALSE, scalar = FALSE, iter = 1,
                 covmin = 1e-04, cv = FALSE) {

  storage.mode(x)  <- "double"
  storage.mode(testx) <- "double"
  storage.mode(y)  <- "integer"

  np <- dim(x)
  p <- np[2]
  n <- np[1]

  storage.mode(epsilon) <- "double"
  neps <- as.integer(length(epsilon))
  nclass <- as.integer(length(table(y)))

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
