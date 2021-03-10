#' Computes CritCF.
#'
#' \code{getcritcfkcca} computes CritCF (see references).
#' @param kmeansfitted internally provided by \code{doclusterkmeans} function.
#' @param inertiapower internally provided by \code{doclusterkmeans} function.
#' @return internal value to be used by \code{doclusterkmeans} function.
#' @importFrom flexclust info
#' @keywords internal
getcritcfkcca <- function(kmeansfitted, inertiapower) {
  k <- dim(kmeansfitted@centers)[1]
  m <- dim(kmeansfitted@centers)[2]
  W <- sum(abs(kmeansfitted@cldist)^inertiapower)
  B <- sum(info(kmeansfitted, "size") * apply(kmeansfitted@centers,
                                              1,
                                              function(x) sum(abs(x)^inertiapower)))
  res <- exp(
    ((1 + log(k + 1, base = 2)) / (1 + log(m + 1, base = 2))) *
      (log(2 * m / (2 * m + 1)) - log(1 + W / B))
    )
  return(res)
}
