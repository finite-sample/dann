#' knncv
#'
#' Cross-validated k-means
#' 
#' @param x matrix  
#' @param y labels
#' @param k number of clusters
#'
#' @return list of standardized matrices along with scaling information (centers)
#' @export
#' @examples \dontrun{
#' knncv(x, y, k=5)
#' }

knncv <- function(x, y, k = 5) {
  x <- as.matrix(x)
  assertMatrix(x, mode = "numeric", any.missing = FALSE, min.rows = 2)
  assertIntegerish(y, len = nrow(x), any.missing = FALSE)
  assertCount(k, positive = TRUE)

  np <- dim(x)
  p <- np[2]
  n <- np[1]

  if (k > n)
    stop("k cannot exceed number of observations")

  storage.mode(x) <- "double"
  storage.mode(y) <- "integer"
  
  junk <- .Fortran("knncv", as.integer(np[1]), as.integer(np[2]), x, y, predict = integer(n), error = integer(1), as.integer(k), as.single(runif(n)), double(n), PACKAGE = "dann")
  
  res <- junk$predict
  attr(res, "error") <- junk$error
  
  res
}
