#' Center data.
#'
#' \code{centerdata} centers all variables at the mean.
#' @param data internally provided by \code{standardizedata} function.
#' @return internal value to be used by \code{standardizedata} function.
#' @keywords internal
centerdata <- function(data) {
  n <- nrow(data)
  m <- matrixStats::colMeans2(as.matrix(data))
  cendata <- data - rep(1, n) %*% t(m)
  return(cendata)
}
