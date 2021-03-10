#' Computes centroid.
#'
#' \code{centroid} computes the centroid for each cluster (mean o median).
#' @param data internally provided by \code{doclusterkmeans} function.
#' @param cluster internally provided by \code{doclusterkmeans} function.
#' @param centpos internally provided by \code{doclusterkmeans} function.
#' @return internal value to be used by \code{doclusterkmeans} function.
#' @keywords internal
centroid <- function(data, cluster, centpos) {
  ncluster <- length(levels(as.factor(cluster)))

  if (class(data) == "numeric")
    data <- as.matrix(data)
  centroid <- matrix(NA, nrow = ncluster, ncol = dim(data)[2])

  if (centpos == "means") {
    for (i in 1:ncluster)
      centroid[i, ] <- matrixStats::colMeans2(as.matrix(data[cluster == i, ]))
  }

  if (centpos == "medians") {
    for (i in 1:ncluster)
      centroid[i, ] <- matrixStats::colMedians(as.matrix(data[cluster == i, ]))
  }
  return(centroid)
}
