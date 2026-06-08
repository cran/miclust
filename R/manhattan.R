#' Manhattan distance.
#'
#' Computes the Manhattan distance from data to centers.
#' @param x internally provided by \code{miclust} function.
#' @param centers internally provided by \code{miclust} function.
#' @return internal value to be used by \code{miclust} function.
#' @noRd
#' @importFrom matrixStats colSums2
manhattan <- function(x, centers) {
  z <- matrix(0, nrow = nrow(x), ncol = nrow(centers))
  for(k in 1:nrow(centers))
   z[, k] <- matrixStats::colSums2(abs(t(x) - centers[k, ]))
  return(z)
}
