#' Performs K-means with backward selection.
#'
#' \code{doclusterkmeansbackward} performs K-means clustering with backward
#'   variable selection.
#' @param data internally provided by \code{doclusterkmeans} function.
#' @param k internally provided by \code{doclusterkmeans} function.
#' @param metriccent internally provided by \code{doclusterkmeans} function.
#' @param inertiapower internally provided by \code{doclusterkmeans} function.
#' @param centpos internally provided by \code{doclusterkmeans} function.
#' @param initcl internally provided by \code{doclusterkmeans} function.
#' @return internal value to be used by \code{doclusterkmeans} function.
#' @keywords internal
#' @importFrom stats hclust dist
doclusterkmeansbackward <- function(data, k, metriccent, inertiapower = 1,
                                    centpos, initcl) {
  numberofpatterns <- sum(!duplicated(data))

  if (numberofpatterns < k)
    stop("Number of patterns is less than number of clusters.")

  selectedset <- names(data)
  removedset <- NULL
  p <- dim(data)[2]
  scannedvariables <- 0
  notscannedvariables <- p
  maxcritcf <- 0
  clusters <- list()
  maxcritcfvector <- vector()
  minimumpatterns <- TRUE
  if (initcl == "hc") {
    hc <- stats::hclust(dist(data), method = "complete")
    cluster <- stats::cutree(hc, k = k) } else {
      cluster <- sample(x = 1:k, size = nrow(data), replace = TRUE)
    }
  initialcentroids <- centroid(data = data, cluster = cluster, centpos = centpos)
  while (sum(duplicated(initialcentroids)) > 0)
    initialcentroids <- getinitialcentroids(data, k)
  colnames(initialcentroids) <- names(data)
  mod <- flexclust::kcca(x = data, k = as.matrix(initialcentroids),
                         family = metriccent, simple = TRUE)
  clusters[[p - notscannedvariables + 1]] <- mod@cluster
  maxcritcfvector[p - notscannedvariables + 1] <- getcritcfkcca(mod, inertiapower = inertiapower)
  initialcentroids <- mod@centers
  while (sum(duplicated(initialcentroids)) > 0)
    initialcentroids <- getinitialcentroids(data, k)
  colnames(initialcentroids) <- names(data)
  while (notscannedvariables > 0 & minimumpatterns) {
    critcfvector <- vector()
    auxclusters <- list()
    for (i in 1:notscannedvariables) {
      dat <- data.frame(data[, selectedset[-i]])
      numberofpatterns <- sum(!duplicated(dat))
      if (numberofpatterns < k) {
        critcfvector[i] <- -1
        auxclusters[[i]] <- NULL } else {
          names(dat) <- selectedset[-i]
          inicentroids <- initialcentroids[, selectedset[-i]]
          while (sum(!duplicated(inicentroids)) < k)
            inicentroids <- getinitialcentroids(dat, k)
          mod <- flexclust::kcca(x = dat, k = as.matrix(inicentroids),
                                 family = metriccent, simple = TRUE)
          auxclusters[[i]] <- mod@cluster
          critcfvector[i] <- getcritcfkcca(mod, inertiapower = inertiapower)
        }
    }
    maxcritcfvector[p - notscannedvariables + 2] <- max(critcfvector)
    if (max(critcfvector) == -1) {
      clusters[[p - notscannedvariables + 2]] <- NULL
      minimumpatterns <- FALSE } else {
        idremovedvariable <- which(critcfvector == max(critcfvector))
        if(length(idremovedvariable) > 1)
          idremovedvariable <- sample(idremovedvariable, 1)
        clusters[[p - notscannedvariables + 2]] <- auxclusters[[idremovedvariable]]
        removedset<- c(removedset, selectedset[idremovedvariable])
        selectedset <- selectedset[-idremovedvariable]
        notscannedvariables <- notscannedvariables - 1
      }
  }
  idbestset <- which(maxcritcfvector == max(maxcritcfvector))
  cut <- p + 1 - idbestset
  selectorder <- c(selectedset, removedset[length(removedset):1])

  res <- list(selectedvariables = selectorder[1:cut],
              clustervector = clusters[[idbestset]],
              maxcritcfvector = maxcritcfvector[-length(maxcritcfvector)],
              critcfmax = max(maxcritcfvector))
  names(res$clustervector) <- NULL
  return(res)
}
