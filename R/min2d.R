#' min2d
#'
#' @param mat matrix 
#' @return A vector
#' @examples \dontrun{
#' min2d(matrix(rnorm(100,1,.2), ncol=5))
#' }

min2d <-
function (mat=NULL) 
{
    i <- order(mat)[1]  # linear index of the smallest value, 1-based
    dd <- dim(mat)      # dims of the matrix
    # Linear indices are 1-based, so shift to 0-based before dividing and back
    # after. Dividing i directly returned row 0 whenever the minimum fell in the
    # last row, and put the column one too far right.
    i1 <- ((i - 1) %/% dd[1]) + 1  # column
    i2 <- ((i - 1) %% dd[1]) + 1   # row
    c(i2, i1)
}