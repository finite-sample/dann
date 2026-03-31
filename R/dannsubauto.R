#' dannsubauto
#'
#' @param x mat
#' @param testx test mat
#' @param y labels
#' @param k k
#' @param plus 1
#' @param kmetric kmet
#' @param epsilon epsilon
#' @param trace Boolean
#' @param ... Additional arguments passed to underlying functions
#'
#' @return A list with items including Name of the Application, No. of pages remaining (given the money),
#' No. of fields remaining (given the money), and when the application credits expire.
#' @export

dannsubauto <-
function (x, testx = matrix(double(p), 
    nrow = 1), y, k = 5, kmetric = max(0.2 * 
    n, 50), epsilon = 1, plus = 1, trace = FALSE, ...) 
{
    mm <- apply(x, 2, var)
    x <- scale(x, F, sqrt(mm))
    testx <- scale(testx, F, sqrt(mm))
    p <- ncol(x)
    n <- length(y)
    trans <- diag(p)
    oldd <- p + 1
    newd <- p
    while (newd < oldd) {
        if (trace) 
            cat("new dimension", newd, "\n")
        junk <- dannsub(x, y, km = kmetric, k = k, epsilon = epsilon, 
            dims = seq(ncol(x)), iter.sub = 1, iter = 1, method = "dann")
        dd <- min2d(junk$error)
        oldd <- newd
        newd <- min(dd[1] + plus, ncol(x))
        uptrans <- junk$vectors[, 
            seq(newd), drop = FALSE]/junk$scale
        x <- x %*% uptrans
        trans <- trans %*% uptrans
        if (trace) {
            print(junk$error)
            cat("new dimension", newd, "\n")
        }
    }
    nn <- min2d(junk$error)[1]
    if (trace) 
        cat("chosen dimension", nn, "\n")
    xx <- testx %*% trans[, seq(nn), drop = FALSE]
    x <- x[, seq(nn), drop = FALSE]
    dann(x, xx, y = y, kmetric = kmetric, k = k, epsilon = epsilon)
}
