#' Relabel clusters.
#'
#' \code{relabelclusters} relabels the clusters so that they all have the same
#'   meaning in the all the datasets.
#' @param refcluster internally provided by \code{assignprobandkappas} function.
#' @param cluster internally provided by \code{assignprobandkappas} function.
#' @return internal value to be used by \code{assignprobandkappas} function.
#' @noRd
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
                     ### OLD until 1.2.8 (before irr is archived):
                     # irr::kappa2(ratings = cbind(refcluster, auxcluster),
                     #             weight = "equal")$value
                     ### NEW from 1.3.0 (after removing irr dependency)
                     ### and using my own internal function "kappaunweighted":
                     kappaunweighted(x = cbind(refcluster, auxcluster))
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
