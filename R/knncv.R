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
  # Labels index Fortran arrays directly (means(class,p), sumw(class)), so
  # they must be 1..nclass with no gaps; gapped labels ran out of bounds.
  ylev <- sort(unique(y))
  y <- as.integer(factor(y, levels = ylev))
  storage.mode(y) <- "integer"
  
  junk <- .Fortran("knncv", as.integer(np[1]), as.integer(np[2]), x, y, predict = integer(n), error = integer(1), as.integer(k), as.single(runif(n)), double(n), PACKAGE = "dann")
  
  # Back to the caller's own labels: knncv.f predicts in the coded space.
  code <- junk$predict
  code[code < 1L | code > length(ylev)] <- NA_integer_
  res <- ylev[code]
  attr(res, "error") <- junk$error
  
  res
}
