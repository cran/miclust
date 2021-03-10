#' Performs K-means without variable selection.
#'
#' \code{doclusterkmeansnone} performs K-means clustering without variable
#'   selection.
#' @param data internally provided by \code{doclusterkmeans} function.
#' @param k internally provided by \code{doclusterkmeans} function.
#' @param metriccent internally provided by \code{doclusterkmeans} function.
#' @param inertiapower internally provided by \code{doclusterkmeans} function.
#' @param centpos internally provided by \code{doclusterkmeans} function.
#' @param initcl internally provided by \code{doclusterkmeans} function.
#' @return internal value to be used by \code{doclusterkmeans} function.
#' @keywords internal
doclusterkmeansnone <- function(data, k, metriccent, inertiapower = 1, centpos,
                                initcl) {
  numberofpatterns <- sum(!duplicated(data))
  if (numberofpatterns < k)
    stop("Number of patterns is less than number of clusters.")

  if (initcl == "hc") {
    hc <- stats::hclust(dist(data), method = "complete")
    cluster <- stats::cutree(hc, k = k)
    }

  if (initcl == "rand")
    cluster <- sample(x = 1:k, size = nrow(data), replace = TRUE)

  initialcentroids <- centroid(data = data, cluster = cluster, centpos = centpos)
  while (sum(duplicated(initialcentroids)) > 0)
    initialcentroids <- getinitialcentroids(data, k)
  colnames(initialcentroids) <- names(data)
  mod <- flexclust::kcca(x = data, k = as.matrix(initialcentroids),
                         family = metriccent, simple = TRUE)
  clustervector <- mod@cluster
  critcf <- getcritcfkcca(mod, inertiapower = inertiapower)
  res <- list(clustervector = clustervector, critcfmax = critcf)
  return(res)
}
