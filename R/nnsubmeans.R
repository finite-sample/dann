#' nnsubmeans
#'
#' Main function 
#' 
#' @param x  matrix
#' @param y  y
#' @param k  number of clusters
#' @param epsilon epsilon
#' @param fullw  fullw
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

nnsubmeans <- function(x, y, k = 20, epsilon = 1, fullw = FALSE, iter = 1, cv = 0) {
  storage.mode(x) <- "double"
  # Labels index Fortran arrays directly (means(class,p), sumw(class)), so
  # they must be 1..nclass with no gaps; gapped labels ran out of bounds.
  y <- as.integer(factor(y, levels = sort(unique(y))))
  storage.mode(y) <- "integer"
  
  nclass <- length(table(y))
  np <- dim(x)
  p <- np[2]
  n <- np[1]
  
  bet <- matrix(0, n, 4)
  
  for (i in seq(n)) {
    junk <- .Fortran("nndist", as.integer(np[1]), as.integer(np[2]), 
                     as.integer(nclass), as.integer(k), x, x[i, ], 
                     as.integer(cv), y, as.integer(iter), fullw, TRUE, 
                     epsilon = as.double(epsilon), which = integer(n), 
                     dist = double(n), covw = matrix(double(p^2), p, p), 
                     covmin = as.double(1e-04), means = matrix(double(nclass * p), nclass, p), 
                     weight = double(n), values = double(p), vectors = double(p * p), 
                     double(p * p), double(n + 2 * p), PACKAGE = "dann")$means
    
    bb <- eigen(t(junk) %*% junk)
    bb <- bb$vec[, 1] * sqrt(bb$val[1])
    bet[i, ] <- c(x[i, ] + bb/2, x[i, ] - bb/2)
    cat(i, " ")
  }
  
  return(bet)
}
