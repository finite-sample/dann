#' knn
#'
#' K-nearest Neighbors
#'
#' @param train  matrix
#' @param test test
#' @param cl  y
#' @param k  number of clusters
#'
#'
#' @return result
#'
#' @export
#'
#' @examples \dontrun{
#' knn(train, test, cl, k=2)
#'}

knn <- function(train, test, cl, k = 1) {
  train <- as.matrix(train)
  test <- as.matrix(test)

  assertMatrix(train, mode = "numeric", any.missing = FALSE, min.rows = 1)
  assertMatrix(test, mode = "numeric", any.missing = FALSE, min.rows = 1, ncols = ncol(train))
  assertIntegerish(cl, len = nrow(train), any.missing = FALSE)
  assertCount(k, positive = TRUE)

  ntr <- as.integer(dim(train)[1])
  nte <- as.integer(dim(test)[1])
  p <- as.integer(dim(train)[2])

  if (k > ntr)
    stop("k cannot exceed number of training observations")

  k <- as.integer(k)

  if (is.factor(cl)) {
    cl1 <- as.integer(as.numeric(cl))
  } else {
    cl1 <- as.integer(cl)
  }

  storage.mode(train) <- "double"
  storage.mode(test) <- "double"

  res <- integer(nte)
  u <- as.single(runif(nte))
  d <- double(nte)

  out <- .Fortran("knn",
           k, ntr, nte, p, train, cl1, test, res = res, u, d, PACKAGE = "dann")
  res <- out$res

  if (is.factor(cl)) {
    levels(res) <- levels(cl)
    res <- as.factor(res)
  }

  return(res)
}
