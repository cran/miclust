#' Computes euclidean distance.
#'
#' \code{euclidean} computes the euclidean distance from data to centers.
#' @param x internally provided by \code{miclust} function.
#' @param centers internally provided by \code{miclust} function.
#' @return internal value to be used by \code{miclust} function.
#' @keywords internal
#' @importFrom matrixStats colSums2
euclidean <- function(x, centers) {
  z <- matrix(0, nrow = nrow(x), ncol = nrow(centers))
  for (k in 1:nrow(centers))
    z[, k] <- sqrt(matrixStats::colSums2((t(x) - centers[k, ])^2))
  return(z)
}
