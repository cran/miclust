#' Performs K-means with forward selection.
#'
#' \code{doclusterkmeansforward} performs K-means clustering with forward
#'   variable selection.
#' @param data internally provided by \code{doclusterkmeans} function.
#' @param k internally provided by \code{doclusterkmeans} function.
#' @param metriccent internally provided by \code{doclusterkmeans} function.
#' @param inertiapower internally provided by \code{doclusterkmeans} function.
#' @param maxvars internally provided by \code{doclusterkmeans} function.
#' @param centpos internally provided by \code{doclusterkmeans} function.
#' @param initcl internally provided by \code{doclusterkmeans} function.
#' @return internal value to be used by \code{doclusterkmeans} function.
#' @keywords internal
doclusterkmeansforward <- function(data, k, metriccent, inertiapower = 1,
                                   maxvars, centpos, initcl) {
  ###############################
  if (initcl == "hc") {
    res <- doclusterkmeansforwardhc(data = data, k = k, metriccent = metriccent,
                                    inertiapower = inertiapower, maxvars = maxvars,
                                    centpos = centpos)
    return(res)
  }
  ###############################
  if (initcl == "rand") {
    res <- doclusterkmeansforwardrand(data = data, k = k, metriccent = metriccent,
                                      inertiapower = inertiapower, maxvars = maxvars,
                                      centpos = centpos)
    return(res)
  }
}
