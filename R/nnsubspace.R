#' nnsubspace
#'
#' @param x mat
#' @param y labels
#' @param k clusters
#' @param epsilon epsilon
#' @param fullw fullw
#' @param iter iter
#' @param cv cv
#'
#' @return subspace
#' @export
#' @examples \dontrun{
#' dannsub()
#' }

nnsubspace <- function(x, y, k = length(y)/2, epsilon = 1, fullw = FALSE, iter = 1, cv = 0) {
  storage.mode(x) <- "double"
  # Labels index Fortran arrays directly (means(class,p), sumw(class)), so
  # they must be 1..nclass with no gaps; gapped labels ran out of bounds.
  y <- as.integer(factor(y, levels = sort(unique(y))))
  storage.mode(y) <- "integer"
  nclass <- length(table(y))
  np <- dim(x)
  p <- np[2]
  n <- np[1]
  between <- matrix(0, p, p)
  
  for (i in seq(n)) {
    x0 <- x[i, ]
    junk <- .Fortran("nndist", as.integer(np[1]), as.integer(np[2]), 
                    as.integer(nclass), as.integer(k), x, x0, 
                    as.integer(cv), y, as.integer(iter), fullw, TRUE, epsilon = as.double(epsilon), 
                    which = integer(n), dist = double(n), covw = matrix(double(p^2), 
                                                                       p, p), covmin = as.double(1e-04), means = matrix(double(nclass * 
                                                                                                                               p), nclass, p), weight = double(n), values = double(p), 
                    vectors = double(p * p), double(p * p), double(n + 
                                                                   2 * p), PACKAGE = "dann")$means
    between <- between + t(junk) %*% junk
    cat(i, " ")
  }
  
  between / n
}
