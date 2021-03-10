#' Cluster analysis in multiple imputed data sets with optional variable
#'   selection.
#'
#' Performs cluster analysis in multiple imputed data sets with optional variable
#'   selection. Results can be summarized and visualized with the \code{summary}
#'   and \code{plot} methods.
#'
#' @param data object of class \code{midata} obtained with the function \code{\link{getdata}}.
#' @param method clustering method. Currently, only \code{"kmeans"} is accepted.
#' @param search search algorithm for the selection variable procedure: \code{"backward"},
#'  \code{"forward"} or \code{"none"}. If \code{"none"} (default), no variable selection
#'  is performed.
#' @param ks the values of the explored number of clusters. Default is exploring 2 and 3
#'  clusters.
#' @param maxvars if \code{method = "forward"}, the maximum number of variables to be
#'  selected.
#' @param usedimp numeric. Which imputed data sets must be included in the cluster
#'  analysis. If \code{NULL} (default), all available imputed data sets are included.
#'  If \code{usedimp} is numeric (or a numeric vector), its values indicate which imputed
#'  data sets are included.
#' @param distance two metrics are allowed to compute distances: \code{"manhattan"}
#'  (default) and \code{"euclidean"}.
#' @param centpos position computation of the cluster centroid. If \code{"means"}
#'  (default) the position of the centroid is computed by the mean. If \code{"medians"},
#'  by the median.
#' @param initcl starting values for the clustering algorithm. If \code{"rand"}, they are
#'  randomly selected; if \code{"hc"}, they are computed via hierarchical clustering. See
#'  Details below.
#' @param verbose a logical value indicating output status messages. Default is \code{TRUE}.
#' @param seed a number. Seed for reproducibility of results. Default is \code{NULL} (no
#'   seed).
#' @details The optimal number of clusters and the final set of variables are selected
#'  according to CritCF. CritCF is defined as
#'  \deqn{CritCF = \left(\frac{2m}{2m + 1} \cdot \frac{1}{1 + W / B}\right)^{\frac{1 + \log_2(k + 1)}{1 + \log_2(m + 1)}},}
#'  where \eqn{m} is the number of variables, \eqn{k} is the number of clusters,
#'  and \eqn{W} and \eqn{B} are the within- and between-cluster inertias. Higher
#'  values of CritCF are preferred (Breaban, 2011). See References below for further
#'  details about the clustering algorithm.
#'
#'  For computational reasons, option \code{"rand"} is suggested instead of \code{"hc"}
#'  for high dimensional \code{data}.
#' @return A list with class "miclust" including the following items:
#'  \describe{
#'    \item{clustering}{a list of lists containing the results of the clustering algorithm
#'     for each analyzed data set and for each analyzed number of clusters. Includes
#'     information about selected variables and the cluster vector.}
#'    \item{completecasesperc}{if \code{data} contains a single data frame, percentage
#'     of complete cases in \code{data}.}
#'    \item{data}{input \code{data}.}
#'    \item{ks}{the values of the explored number of clusters.}
#'    \item{usedimp}{indicator of the imputed data sets used.}
#'    \item{kfin}{optimal number of clusters.}
#'    \item{critcf}{if \code{data} contains a single data frame, \code{critcf} contains
#'     the optimal (maximum) value of CritCF (see Details) and the number of selected
#'     variables in the reduction procedure for each explored number of clusters. If
#'     \code{data} is a list, \code{critcf} contains the optimal value of CritCF for
#'     each imputed data set and for each explored value of the number of clusters.}
#'    \item{numberofselectedvars}{number of selected variables.}
#'    \item{selectedkdistribution}{if \code{data} is a list, frequency of selection of
#'     each analyzed number of clusters.}
#'    \item{method}{input \code{method}.}
#'    \item{search}{input \code{search}.}
#'    \item{maxvars}{input \code{maxvars}.}
#'    \item{distance}{input \code{distance}.}
#'    \item{centpos}{input \code{centpos}.}
#'    \item{selmetriccent}{an object of class \code{kccaFamily} needed by the specific
#'     \code{summary} method.}
#'    \item{initcl}{input \code{initcl}.}
#'   }
#' @references
#' \itemize{
#'   \item Basagana X, Barrera-Gomez J, Benet M, Anto JM, Garcia-Aymerich J. A
#'    framework for multiple imputation in cluster analysis. American Journal of
#'    Epidemiology. 2013;177(7):718-25.
#'   \item Breaban M, Luchian H. A unifying criterion for unsupervised clustering
#'    and feature selection. Pattern Recognition 2001;44(4):854-65.
#'   }
#' @seealso \code{\link{getdata}} for data preparation before using \code{miclust}.
#' @examples
#' ### data preparation:
#' minhanes1 <- getdata(data = minhanes)
#'
#' ##################
#' ###
#' ### Example 1:
#' ###
#' ### Multiple imputation clustering process with backward variable selection
#' ###
#' ##################
#'
#' ### using only the imputations 1 to 10 for the clustering process and exploring
#' ### 2 vs. 3 clusters:
#' minhanes1clust <- miclust(data = minhanes1, search = "backward", ks = 2:3,
#'                           usedimp = 1:10, seed = 4321)
#' minhanes1clust
#' minhanes1clust$kfin  ### optimal number of clusters
#'
#' ### graphical summary:
#' plot(minhanes1clust)
#'
#' ### selection frequency of the variables for the optimal number of clusters:
#' y <- getvariablesfrequency(minhanes1clust)
#' y
#' plot(y$percfreq, type = "h", main = "", xlab = "Variable",
#'      ylab = "Percentage of times selected", xlim = 0.5 + c(0, length(y$varnames)),
#'      lwd = 15, col = "blue", xaxt = "n")
#' axis(1, at = 1:length(y$varnames), labels = y$varnames)
#'
#' ### default summary for the optimal number of clusters:
#' summary(minhanes1clust)
#'
#' ## summary forcing 3 clusters:
#' summary(minhanes1clust, k = 3)
#'
#' ##################
#' ###
#' ### Example 2:
#' ###
#' ### Same analysis but without variable selection
#' ###
#' ##################
#' \donttest{
#' minhanes2clust <- miclust(data = minhanes1, ks = 2:3, usedimp = 1:10, seed = 4321)
#' minhanes2clust
#' plot(minhanes2clust)
#' summary(minhanes2clust)
#' }
#'
#' ##################
#' ###
#' ### Example 3:
#' ###
#' ### Complete case clustering process with backward variable selection
#' ###
#' ##################
#'
#' nhanes0 <- getdata(data = minhanes[[1]])
#' nhanes2clust <- miclust(data = nhanes0, search = "backward", ks = 2:3, seed = 4321)
#' nhanes2clust
#'
#' summary(nhanes2clust)
#'
#' ### nothing to plot for a single data set analysis
#' # plot(nhanes2clust)
#'
#' ##################
#' ###
#' ### Example 4:
#' ###
#' ### Complete case clustering process without variable selection
#' ###
#' ##################
#' \donttest{
#' nhanes3clust <- miclust(data = nhanes0, ks = 2:3, seed = 4321)
#' nhanes3clust
#' summary(nhanes3clust)
#' }
#' @export
#' @importFrom flexclust kcca kccaFamily
#' @importFrom matrixStats colMeans2 colMedians
#' @importFrom stats complete.cases median quantile sd
miclust <- function(data,
                    method = "kmeans",
                    search = c("none", "backward", "forward"),
                    ks = 2:3,
                    maxvars = NULL,
                    usedimp = NULL,
                    distance = c("manhattan", "euclidean"),
                    centpos = c("means", "medians"),
                    initcl = c("hc", "rand"),
                    verbose = TRUE,
                    seed = NULL) {
  ### control "data"
  # if the user provides a single data.frame (not result of getdata), add imputed data as NA:
  if (!inherits(data, "midata")) {
    if (!inherits(data, "data.frame"))
      stop("Argument 'data' must be of class 'midata' (i.e. result of getdata()) or 'data.frame'.")
    data <- list(rawdata = data, impdata = NA)
  }
  # now data is always a list with:
  # rawdata (data.frame)
  # impdata (either NA if no imputed data sets are provided,
  #         or a list with the imputed data.frames)
  data0 <- data$rawdata
  n <- nrow(data0)
  p <- ncol(data0)
  # number of available imputed data sets:
  if (!inherits(data$impdata, "list")) {
    m <- 0 } else {
      m <- length(data$impdata)
    }
  ### control "method"
  if (method != "kmeans")
    stop("Argument 'method' must be 'kmeans'.")
  ### control "search"
  search <- match.arg(search)
  ### control "ks"
  if ((!inherits(ks, "numeric") && !inherits(ks, "integer")) || is.na(ks[1]))
    stop("Argument 'ks' must be numeric.")
  ks <- as.vector(ks)
  if (sum(floor(ks) != ceiling(ks)) > 0)
    stop("Values in 'ks' must be integer.")
  ks <- ks[!duplicated(ks)]
  ks <- ks[order(ks)]
  if (max(ks) < 2)
    stop("At least 2 clusters are required in argument 'ks'.")
  ks <- ks[ks >= 2]
  nk <- length(ks)
  ### control "distance"
  distance <- match.arg(distance)
  ### control "centpos"
  centpos <- match.arg(centpos)
  ### control "initcl"
  initcl <- match.arg(initcl)
  ### control "maxvars"
  if ((search == "forward") && ((length(maxvars) != 1L) || is.na(maxvars) || (maxvars < 1) || (floor(maxvars) != ceiling(maxvars)) || (maxvars > p)))
     stop(paste("Argument 'maxvars' must be a positive integer and not greater than", p, "if search is 'forward'."))
  ### control "verbose"
  if ((!inherits(verbose, "logical")) || (length(verbose) != 1L)) {
    stop("'verbose' must be TRUE or FALSE.")
  }
  ### control "seed"
  if (!is.null(seed)) {
    if ((length(seed) != 1L) || (!inherits(seed, "numeric"))) {
      stop("If 'seed' is not NULL, then it must be a single numeric value.")
    }
    set.seed(seed)
  }
  ### control "usedimp"
  if (is.null(usedimp)) {
    if (m == 0) {
      usedimp <- 0 } else {
        usedimp <- 1:m
      } } else {
        if ((!inherits(usedimp, "numeric")) && (!inherits(usedimp, "integer")))
          stop("Argument 'usedimp' must be numeric.")
        usedimp <- usedimp[usedimp %in% 1:m]
      }


  if (method == "kmeans") {
    if (centpos == "means")
      centaux <- "colMeans2"
    if (centpos == "medians")
      centaux <- "colMedians"

  selmetriccent <- do.call("kccaFamily", list(dist = as.name(distance), cent = as.name(centaux)))

  if (m == 0) {
    idcomplete <- complete.cases(data0)
    completecasesperc <- 100 * mean(idcomplete)
    aux <- standardizedata(data0[idcomplete, ])
    clustering <- vector("list", nk)
    critcf <- rep(NA, nk)
    if (search != "none")
      numberofselectedvars <- rep(NA, nk)
    for (i in 1:nk) {
      clustering[[i]] <- doclusterkmeans(search = search,
                                         data = aux,
                                         k = ks[i],
                                         metriccent = selmetriccent,
                                         inertiapower = 1,
                                         maxvars = maxvars,
                                         centpos = centpos,
                                         initcl = initcl)
      auxclust <- rep(NA, nrow(aux))
      auxclust[idcomplete] <- clustering[[i]]$clustervector
      clustering[[i]]$clustervector <- auxclust
      critcf[i] <- clustering[[i]]$critcfmax
      if (search != "none")
        numberofselectedvars[i] <- length(clustering[[i]]$selectedvariables)
      if (verbose)
        cat("Clustering with k =", ks[i], "done\n")
    }
    names(clustering) <- paste0("k=", ks)
    critcftable <- data.frame(critcf = critcf)
    rownames(critcftable) <- paste0("k=", ks)
    idmaxcritcf <- which(critcf == max(critcf))[1]
    if (search != "none") {
      critcftable$numberofselectedvars <- numberofselectedvars
      names(critcftable)[2] <- "Number of selected variables"
      numberofselectedvars <- numberofselectedvars[idmaxcritcf]
    }
    kfin <- ks[idmaxcritcf]
    if (search == "none") {
      res <- list(clustering = clustering,
                  data = data,
                  completecasesperc = completecasesperc,
                  critcf = critcftable,
                  ks = ks,
                  usedimp = NULL,
                  kfin = kfin,
                  method = method,
                  search = search,
                  distance = distance,
                  centpos = centpos,
                  selmetriccent = selmetriccent,
                  initcl = initcl) } else {
                    res <- list(clustering = clustering,
                                data = data,
                                completecasesperc = completecasesperc,
                                critcf = critcftable,
                                ks = ks,
                                usedimp = NULL,
                                kfin = kfin,
                                numberofselectedvars = numberofselectedvars,
                                method = method,
                                search = search,
                                maxvars = maxvars,
                                distance = distance,
                                centpos = centpos,
                                selmetriccent = selmetriccent,
                                initcl = initcl) } } else {
                                  nimp <- length(usedimp)
                                  clustering <- vector("list", nimp)
                                  ### verbose message
                                  simvec <- 1:nimp
                                  if (verbose) {
                                    verbmes <- rep(".", nimp)
                                    verbmes[(simvec %% 5) == 0] <- paste("imp", simvec[(simvec %% 5) == 0])
                                    verbbreak <- rep("", nimp)
                                    verbbreak[(simvec %% 50) == 0] <- "\n"
                                  }
                                  for (imp in 1:nimp) {
                                    clustering[[imp]] <- vector("list", nk)
                                    if (verbose) {
                                      cat(verbmes[imp])
                                      cat(verbbreak[imp])
                                    }
                                    aux <- data$impdata[[usedimp[imp]]]
                                    for (i in 1:nk) {
                                      clustering[[imp]][[i]] <- doclusterkmeans(search = search,
                                                                                data = aux,
                                                                                k = ks[i],
                                                                                metriccent = selmetriccent,
                                                                                inertiapower = 1,
                                                                                maxvars = maxvars,
                                                                                centpos = centpos,
                                                                                initcl = initcl)
                                    }
                                    names(clustering[[imp]]) <- paste0("k=", ks)
                                  }
                                  names(clustering) <- paste0("imp", usedimp)
                                  critcf <- matrix(nrow = nimp, ncol = nk)
                                  rownames(critcf) <- paste0("imp", usedimp)
                                  colnames(critcf) <- ks
                                  if (search != "none")
                                    numberofselectedvars <- critcf
                                  for (i in 1:nimp) {
                                    for (j in 1:nk) {
                                      critcf[i, j] <- clustering[[i]][[j]]$critcfmax
                                      if (search != "none")
                                        numberofselectedvars[i, j] <- length(clustering[[i]][[j]]$selectedvariables)
                                    }
                                  }
                                  idoptimk <- apply(critcf, 1, FUN = function(x) which(x == max(x))[1])
                                  optimk <- ks[idoptimk]
                                  selectedkdistribution <- rep(0, nk)
                                  names(selectedkdistribution) <- ks
                                  aux <- 100 * summary(as.factor(optimk)) / nimp
                                  selectedkdistribution[names(aux)] <- aux
                                  selcol <- as.numeric(which(selectedkdistribution == max(selectedkdistribution))[1])
                                  kfin <- ks[selcol]
                                  rownames(critcf) <- paste0("imp", usedimp)
                                  colnames(critcf) <- paste0("k=", ks)
                                  clusters <- vector("list", nk)
                                  kappas <- matrix(nrow = nimp - 1, ncol = nk)
                                  for (j in 1:nk) {
                                    clusters[[j]] <- matrix(nrow = n, ncol = nimp)
                                    clusters[[j]][, 1] <- clustering[[1]][[paste0("k=", ks[j])]]$clustervector
                                    clustering[[1]][[paste0("k=", ks[j])]]$clustervector <- NULL
                                    for (i in 2:nimp) {
                                      auxcluster <- clustering[[i]][[paste0("k=", ks[j])]]$clustervector
                                      aux <- relabelclusters(refcluster = clusters[[j]][, 1], cluster = auxcluster)
                                      clusters[[j]][, i] <- aux$newcluster
                                      kappas[i - 1, j] <- aux$kappa
                                      clustering[[i]][[paste0("k=", ks[j])]]$clustervector <- NULL
                                    }
                                    clusters[[j]] <- as.data.frame(clusters[[j]])
                                    names(clusters[[j]]) <- paste0("imp", usedimp)
                                    clusters[[j]]$assigned <- apply(clusters[[j]],
                                                                    1,
                                                                    FUN = function(x) {
                                                                      aux <- table(as.numeric(x))
                                                                      aux1 <- as.numeric(aux)
                                                                      idw <- which(aux1 == max(aux1))
                                                                      if (length(idw) > 1)
                                                                        idw <- idw[sample(x = 1:length(idw), size = 1)]
                                                                      res <- as.numeric(names(aux)[idw])
                                                                      return(res)
                                                                    }
                                                                    )
                                  }
                                  kappas <- as.data.frame(kappas)
                                  rownames(kappas) <- paste0("imp", usedimp[-1])
                                  colnames(kappas) <- paste0("k=", ks)
                                  names(clusters) <- paste0("k=", ks)
                                  clustering$clusters <- clusters
                                  clustering$kappas <- kappas
                                  if (search == "none") {
                                    res <- list(clustering = clustering,
                                                data = data,
                                                ks = ks,
                                                usedimp = usedimp,
                                                kfin = kfin,
                                                critcf = critcf,
                                                selectedkdistribution = selectedkdistribution,
                                                method = method,
                                                search = search,
                                                distance = distance,
                                                centpos = centpos,
                                                selmetriccent = selmetriccent,
                                                initcl = initcl) } else {
                                                  res <- list(clustering = clustering,
                                                              data = data,
                                                              ks = ks,
                                                              usedimp = usedimp,
                                                              kfin = kfin,
                                                              critcf  = critcf,
                                                              numberofselectedvars = numberofselectedvars,
                                                              selectedkdistribution = selectedkdistribution,
                                                              method = method,
                                                              search = search,
                                                              maxvars = maxvars,
                                                              distance = distance,
                                                              centpos = centpos,
                                                              selmetriccent = selmetriccent,
                                                              initcl = initcl)
                                                }
                                }
  }
  class(res) <- "miclust"
  if (verbose) {
    cat("\n Analysis done.\n\n")
  }
  return(res)
}
