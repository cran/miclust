#' Computes initial centroids.
#'
#' \code{getinitialcentroids} computes initial centroids for the clustering
#'   process.
#' @param data internally provided by \code{doclusterkmeans} function.
#' @param ncentr internally provided by \code{doclusterkmeans} function.
#' @return internal value to be used by \code{doclusterkmeans} function.
#' @keywords internal
#' @importFrom stats runif
#' @importFrom matrixStats colMins colMaxs
getinitialcentroids <- function(data, ncentr) {
  mins <- matrixStats::colMins(as.matrix(data), na.rm = TRUE)
  maxs <- matrixStats::colMaxs(as.matrix(data), na.rm = TRUE)
  initcentmatrix <- as.data.frame(matrix(nrow = ncentr, ncol = dim(data)[2]))
  names(initcentmatrix) <- names(data)
  for (i in 1:dim(data)[2])
    initcentmatrix[, i] <- runif(n = nrow(initcentmatrix), min = mins[i], max = maxs[i])
  return(initcentmatrix)
 }
