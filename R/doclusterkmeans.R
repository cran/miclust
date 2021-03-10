#' Performs K-means clustering with optional variable selection.
#'
#' \code{doclusterkmeans} performs K-means clustering with optional variable
#'   selection.
#' @param search internally provided by \code{miclust} function.
#' @param data internally provided by \code{miclust} function.
#' @param k internally provided by \code{miclust} function.
#' @param metriccent internally provided by \code{miclust} function.
#' @param inertiapower internally provided by \code{miclust} function.
#' @param maxvars internally provided by \code{miclust} function.
#' @param centpos internally provided by \code{miclust} function.
#' @param initcl internally provided by \code{miclust} function.
#' @return internal value to be used by \code{miclust} function.
#' @keywords internal
doclusterkmeans <- function(search, data, k, metriccent, inertiapower = 1,
                            maxvars = NULL, centpos, initcl) {
  if (search == "forward") {
    res <- doclusterkmeansforward(data = data, k = k, metriccent = metriccent,
                                  inertiapower = inertiapower, maxvars = maxvars,
                                  centpos = centpos, initcl = initcl)
    return(res)
    }
  if (search == "backward") {
    res <- doclusterkmeansbackward(data = data, k = k, metriccent = metriccent,
                                   inertiapower = inertiapower,
                                   centpos = centpos, initcl = initcl)
    return(res)
  }
  if (search == "none") {
    res <- doclusterkmeansnone(data = data, k = k, metriccent = metriccent,
                               inertiapower = inertiapower,
                               centpos = centpos, initcl = initcl)
    return(res)
  }
}
