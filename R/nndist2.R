#' nndist2
#'
#' Main function 
#' 
#' @param x  matrix
#' @param y  y
#' @param x0  colMeans of x
#' @param ktarget  number of clusters
#' @param rate  rate of learning
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
#' @usage # dann()
#'

nndist2 <- function(x, y, x0 = colMeans(x), kmetric = length(y)/2, ktarget = 5, rate = 0.5, epsilon = 1, fullw = FALSE, scalar = FALSE, iter = 1, cv = 0) {
  storage.mode(x) <- "double"
  storage.mode(x0) <- "double"
  # Labels index Fortran arrays directly (means(class,p), sumw(class)), so
  # they must be 1..nclass with no gaps; gapped labels ran out of bounds.
  y <- as.integer(factor(y, levels = sort(unique(y))))
  storage.mode(y) <- "integer"
  nclass <- length(table(y))
  np <- dim(x)
  p <- np[2]
  n <- np[1]
  junk <- .Fortran("nndist2", as.integer(n), as.integer(p), as.integer(nclass),
                   k = as.integer(c(kmetric, ktarget, iter)),
                   rem = as.double(c(rate, epsilon, 1e-04)),
                   x, x0, as.integer(cv), y, c(fullw, scalar),
                   which = integer(n), dist = double(n),
                   metric = double(p^2), covw = double(p^2),
                   means = double(nclass * p), weight = double(n),
                   values = double(p), vectors = double(p * p),
                   double(p * p), double(n + 2 * p), x,
                   ynew = y, kback = integer(2), PACKAGE = "dann")
  return(junk)
}
