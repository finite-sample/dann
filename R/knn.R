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
  if (is.factor(cl) || is.character(cl)) {
    assertVector(cl, len = nrow(train), any.missing = FALSE)
  } else {
    assertIntegerish(cl, len = nrow(train), any.missing = FALSE)
  }
  assertCount(k, positive = TRUE)

  ntr <- as.integer(dim(train)[1])
  nte <- as.integer(dim(test)[1])
  p <- as.integer(dim(train)[2])

  if (k > ntr)
    stop("k cannot exceed number of training observations")

  k <- as.integer(k)

  # Class labels index the fixed-size votes[] array in knn.c directly, so they
  # must be 1..K with no gaps. Nothing enforced that: labels {1, 60} indexed
  # past the end of votes[51] and returned values that were not classes at all.
  # Map to codes for the call and back to the caller's labels on the way out.
  cl_lev <- sort(unique(cl))
  cl1 <- as.integer(factor(cl, levels = cl_lev))

  storage.mode(train) <- "double"
  storage.mode(test) <- "double"

  res <- integer(nte)
  u <- as.single(runif(nte))
  d <- double(nte)

  out <- .Fortran("knn",
           k, ntr, nte, p, train, cl1, test, res = res, u, d, PACKAGE = "dann")
  code <- out$res
  code[code < 1L | code > length(cl_lev)] <- NA_integer_
  res <- cl_lev[code]

  return(res)
}
