#' Performs K-means with forward selection.
#'
#' \code{doclusterkmeansforwardrand} performs K-means clustering with forward
#'   variable selection (option rand).
#' @param data internally provided by \code{doclusterkmeans} function.
#' @param k internally provided by \code{doclusterkmeans} function.
#' @param metriccent internally provided by \code{doclusterkmeans} function.
#' @param inertiapower internally provided by \code{doclusterkmeans} function.
#' @param maxvars internally provided by \code{doclusterkmeans} function.
#' @param centpos internally provided by \code{doclusterkmeans} function.
#' @return internal value to be used by \code{doclusterkmeans} function.
#' @keywords internal
doclusterkmeansforwardrand <- function(data, k, metriccent, inertiapower = 1,
                                       maxvars, centpos) {
  selectedset <- NULL
  candidateset <- names(data)
  p <- dim(data)[2]
  scannedvariables <- 0
  notscannedvariables <- p
  maxcritcf <- 0
  clusters <- list()
  maxcritcfvector <- vector()
  while (notscannedvariables > 0 & length(selectedset) <= maxvars) {
    critcfvector <- vector()
    auxclusters <- list()
    for (i in 1:notscannedvariables) {
      dat <- data.frame(data[, c(selectedset, candidateset[i])])
      numberofpatterns <- sum(!duplicated(dat))
      if (numberofpatterns < k) {
        critcfvector[i] <- -1
        auxclusters[[i]] <- NULL } else {
          names(dat) <- c(selectedset, candidateset[i])
          cluster <- sample(x = 1:k, size = nrow(dat), replace = TRUE)
          initialcentroids <- centroid(data = dat, cluster = cluster, centpos = centpos)
          mod <- flexclust::kcca(x = dat, k = as.matrix(initialcentroids),
                                 family = metriccent, simple = TRUE)
          auxclusters[[i]] <- mod@cluster
          critcfvector[i] <- getcritcfkcca(mod, inertiapower = inertiapower)
        }
    }
    maxcritcfvector[p - notscannedvariables + 1] <- max(critcfvector)
    idselectedvariable <- which(critcfvector == max(critcfvector))
    if (length(idselectedvariable) > 1)
      idselectedvariable <- sample(idselectedvariable, 1)
    clusters[[p - notscannedvariables + 1]] <- auxclusters[[idselectedvariable]]
    selectedset <- c(selectedset, candidateset[idselectedvariable])
    candidateset <- candidateset[-idselectedvariable]
    notscannedvariables <- notscannedvariables - 1
  }
  idbestset <- which(maxcritcfvector == max(maxcritcfvector))
  res <- list(selectedvariables = selectedset[1:idbestset],
              clustervector = clusters[[idbestset]],
              maxcritcfvector = maxcritcfvector,
              critcfmax = max(maxcritcfvector),
              maxcritcfvector = maxcritcfvector)
  names(res$clustervector) <- NULL
  return(res)
}
