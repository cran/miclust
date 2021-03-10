#' Relabel clusters.
#'
#' \code{relabelclusters} relabels the clusters so that they all have the same
#'   meaning in the all the data sets.
#' @param refcluster internally provided by \code{assignprobandkappas} function.
#' @param cluster internally provided by \code{assignprobandkappas} function.
#' @return internal value to be used by \code{assignprobandkappas} function.
#' @keywords internal
relabelclusters <- function(refcluster, cluster) {
  res <- NULL
  k <- length(table(refcluster))
  n <- length(refcluster)
  permut <- combinat::permn(1:k)
  npermut <- length(permut)
  kappas <- rep(NA, npermut)
  auxcluster <- list()
  kappas <- lapply(permut,
                   FUN = function(x) {
                     auxcluster <- rep(NA, n)
                     for (j in 1:k)
                       auxcluster[cluster == j] <- x[j]
                     irr::kappa2(ratings = cbind(refcluster, auxcluster),
                                 weight = "equal")$value
                     }
                   )
  kappas <- unlist(kappas)
  id <- which(kappas == max(kappas))
  if (length(id) > 1)
    id <- sample(id, 1)
  kappa <- kappas[id]
  id <- permut[[id]]
  newcluster <- rep(NA, n)
  for (j in 1:k)
    newcluster[cluster == j] <- id[j]
  res$newcluster <- newcluster
  res$kappa <- kappa
  return(res)
}
