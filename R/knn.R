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

  ntr <- as.integer(dim(train)[1])
  nte <- as.integer(dim(test)[1])
  p <- as.integer(dim(train)[2])
  k <- as.integer(k)

  if (length(cl) != ntr)
    stop("train and class have different lengths")

  if (dim(test)[2] != p)
    stop("Dims of test and train differ")

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
