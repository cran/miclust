#' Cluster analysis in multiple imputed datasets with optional variable
#'   selection.
#'
#' Performs cluster analysis in multiple imputed datasets with optional variable
#'   selection. Results can be summarized and visualized with the \code{summary}
#'   and \code{plot} methods.
#'
#' @name miclust
#' @param data object of class \code{midata} obtained with the function \code{\link{getdata}}.
#' @param method clustering method. Currently, only \code{"kmeans"} is accepted.
#' @param search search algorithm for the selection variable procedure:
#'  \code{"backward"}, \code{"forward"} or \code{"none"}. If \code{"none"}
#'  (default), no variable selection is performed.
#' @param ks the values of the explored number of clusters. Default is exploring
#'  2 and 3 clusters.
#' @param maxvars if \code{method = "forward"}, the maximum number of variables
#'  to be selected.
#' @param usedimp numeric. Which imputed datasets must be included in the
#'  cluster analysis. If \code{NULL} (default), all available imputed datasets
#'  are included. If \code{usedimp} is numeric (or a numeric vector), its values
#'  indicate which imputed datasets are included.
#' @param distance two metrics are allowed to compute distances:
#'  \code{"manhattan"} (default) and \code{"euclidean"}.
#' @param centpos position computation of the cluster centroid. If \code{"means"}
#'  (default) the position of the centroid is computed by the mean. If
#'  \code{"medians"}, by the median.
#' @param initcl starting values for the clustering algorithm. If \code{"rand"},
#'  they are randomly selected; if \code{"hc"}, they are computed via
#'  hierarchical clustering. See Details below.
#' @param verbose a logical value indicating output status messages. Default is
#'  \code{TRUE}.
#' @param seed a number. Seed for reproducibility of results. Default is
#'  \code{NULL} (no seed).
#' @param x for \code{print.miclust} and \code{plot.miclust}, an object of
#'   class \code{miclust} obtained with the function \code{\link{miclust}}.
#' @param k for \code{plot.miclust}, number of clusters. The default value is
#'   the optimal number of clusters obtained by \code{\link{miclust}}.
#' @param metric for \code{plot.miclust}, metrics to be represented. \code{"all"}
#'   (default, corresponding to the output provided by version 1.2.8 and earlier
#'   represents all metrics. Other options are: \code{"nclfreq"} (percentage of
#'   times each number of clusters has been selected); \code{"critcf"} (critCF
#'   distribution for each number of clusters); \code{"nvarfreq"} (distribution
#'   of the number of selected variables), and \code{"varsel"} (percentage of
#'   appearance of the variables that remained in the final set of selected
#'   variables).
#' @param col.nclfreq,col.critcf,col.nvarfreq,col.varsel for \code{plot.miclust},
#'   a character string or integer specifying the color for each specific
#'   \code{metric}. They control the color of the bars in the cluster frequency
#'   plot (\code{"nclfreq"}), the boxes in the CritCF plot (\code{"critcf"}),
#'   the bars in the variable frequency plot (\code{"nvarfreq"}), and the
#'   segments in the variable selection plot (\code{"varsel"}). Defaults are
#'   \code{"gray"} for all cases but \code{"black"} for \code{"varsel"}.
#' @param col.all An optional character string or integer specifying a global
#'   color. If provided, it overrides all specific color arguments listed above,
#'   applying the same color across all subplots. Defaults to \code{NULL}.
#' @param \dots further arguments for \code{print.miclust} and \code{plot.miclust}.
#' @details The optimal number of clusters and the final set of variables are
#'  selected according to CritCF. CritCF is defined as
#'  \deqn{CritCF = \left(\frac{2m}{2m + 1} \cdot \frac{1}{1 + W / B}\right)^{\frac{1 + \log_2(k + 1)}{1 + \log_2(m + 1)}},}
#'  where \eqn{m} is the number of variables, \eqn{k} is the number of clusters,
#'  and \eqn{W} and \eqn{B} are the within- and between-cluster inertias. Higher
#'  values of CritCF are preferred (Breaban, 2011). See References below for further
#'  details about the clustering algorithm.
#'
#'  For computational reasons, option \code{"rand"} is suggested instead of \code{"hc"}
#'  for high dimensional \code{data}.
#' @returns A list with class "miclust" including the following items:
#'  \describe{
#'    \item{clustering}{a list of lists containing the results of the clustering algorithm
#'     for each analyzed dataset and for each analyzed number of clusters. Includes
#'     information about selected variables and the cluster vector.}
#'    \item{completecasesperc}{if \code{data} contains a single data frame, percentage
#'     of complete cases in \code{data}.}
#'    \item{data}{input \code{data}.}
#'    \item{ks}{the values of the explored number of clusters.}
#'    \item{usedimp}{indicator of the imputed datasets used.}
#'    \item{kfin}{optimal number of clusters.}
#'    \item{critcf}{if \code{data} contains a single data frame, \code{critcf} contains
#'     the optimal (maximum) value of CritCF (see Details) and the number of selected
#'     variables in the reduction procedure for each explored number of clusters. If
#'     \code{data} is a list, \code{critcf} contains the optimal value of CritCF for
#'     each imputed dataset and for each explored value of the number of clusters.}
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
#' ### Complete cases clustering process with backward variable selection
#' ###
#' ##################
#'
#' nhanes0 <- getdata(data = minhanes[[1]])
#' nhanes2clust <- miclust(data = nhanes0, search = "backward", ks = 2:3, seed = 4321)
#' nhanes2clust
#'
#' summary(nhanes2clust)
#'
#' ### nothing to plot for a single dataset analysis
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
#' @import graphics
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
  # impdata (either NA if no imputed datasets are provided,
  #         or a list with the imputed data.frames)
  data0 <- data$rawdata
  n <- nrow(data0)
  p <- ncol(data0)
  # number of available imputed datasets:
  if (!inherits(data$impdata, "list")) {
    m <- 0
  } else {
    m <- length(data$impdata)
  }
  ### control "method"
  method <- match.arg(method)
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
                                  clustering <- vector(mode = "list", length = nimp)
                                  ### verbose message
                                  # simvec <- 1:nimp
                                  # if (verbose) {
                                  #   verbmes <- rep(".", nimp)
                                  #   verbmes[(simvec %% 5) == 0] <- paste("imp", simvec[(simvec %% 5) == 0])
                                  #   verbbreak <- rep("", nimp)
                                  #   verbbreak[(simvec %% 20) == 0] <- "\n"
                                  # }
                                  if (verbose)
                                    verb_message <- get_miclust_verbose(nimp = nimp, breakeach = 20)

                                  for (imp in 1:nimp) {
                                    clustering[[imp]] <- vector(mode = "list", length = nk)
                                    # if (verbose) {
                                    #   cat(verbmes[imp])
                                    #   cat(verbbreak[imp])
                                    # }
                                    if (verbose)
                                      cat(verb_message[imp])

                                    data_imp <- data$impdata[[usedimp[imp]]]
                                    for (i in 1:nk) {
                                      clustering[[imp]][[i]] <- doclusterkmeans(search = search,
                                                                                data = data_imp,
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
                                  colnames(critcf) <- paste0("k=", ks)
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
                                  clusters <- vector(mode = "list", length = nk)
                                  kappas <- matrix(nrow = nimp - 1, ncol = nk)
                                  for (j in 1:nk) {
                                    clusters[[j]] <- matrix(nrow = n, ncol = nimp)
                                    clusters[[j]][, 1] <- clustering[[1]][[paste0("k=", ks[j])]]$clustervector
                                    clustering[[1]][[paste0("k=", ks[j])]]$clustervector <- NULL
                                    for (i in 2:nimp) {
                                      auxcluster <- clustering[[i]][[paste0("k=", ks[j])]]$clustervector
                                      aux <- relabelclusters(refcluster = clusters[[j]][, 1], cluster = auxcluster)
                                      #######################################
                                      if (is.null(aux$newcluster))
                                       stop(paste("object 'newcluster' in relabelclusters() is NULL for imputation",
                                                   i, ", and k =", ks[j], "."))
                                      ######################################
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






###-------------------------------------------
###-------------------------------------------
###-------------------------------------------
###-------------------------------------------
###
###     S3  Method print.miclust
###
###-------------------------------------------
###-------------------------------------------
###-------------------------------------------
###-------------------------------------------
#' @rdname miclust
#' @method print miclust
#' @export
print.miclust <- function(x, ...) {
  if (!inherits(x, "miclust"))
    stop("Argument 'x' must be of class 'miclust'.")

  ############################

  nusedimp <- length(x$usedimp)

  if (nusedimp == 0) {
    cat("\n")
    cat("   Results for complete cases (",
        sprintf("%.2f", x$completecasesperc),
        "% of cases):\n",
        sep = "")
    cat("------------------------------------------------\n")
    cat("Explored number of clusters                :",
        paste(x$ks, collapse = ", "),
        "\n")
    cat("Optimal number of clusters                 :", x$kfin, "\n")
    if (x$search != "none")
      cat("Number of selected variables for",
          x$kfin,
          "clusters:",
          x$numberofselectedvars, "\n")
  }

  ############################

  if (nusedimp > 0) {
    cat("\n")
    cat("   Results using", length(x$usedimp), "imputations:\n")
    cat("------------------------------------------------\n")
    cat("\n")
    res <- sprintf("%.2f", 100 * x$selectedkdistribution / sum(x$selectedkdistribution))
    if (x$search == "none") {
      res <- data.frame(res)
      names(res) <- "Frequency of selection (%)"
      rownames(res) <- paste0("k=", x$ks) } else {
        res <- rbind(res, sprintf("%.1f", apply(x$numberofselectedvars, 2, median)))
        res <- as.data.frame(res)
        rownames(res) <- c("Frequency of selection (%)",
                           "Median number of selected variables")
        names(res) <- paste0("k=", x$ks)
      }
    print(res)
  }
}



###-------------------------------------------
###-------------------------------------------
###-------------------------------------------
###-------------------------------------------
###
###     S3  Method plot.miclust
###
###-------------------------------------------
###-------------------------------------------
###-------------------------------------------
###-------------------------------------------
#' @rdname miclust
#' @method plot miclust
#' @export
plot.miclust <- function(x,
                         k = NULL,
                         metric = c("all", "nclfreq", "critcf", "nvarfreq", "varsel"),
                         col.nclfreq = "gray",
                         col.critcf = "gray",
                         col.nvarfreq = "gray",
                         col.varsel = "black",
                         col.all = NULL,
                         ...) {
  if (!inherits(x, "miclust"))
    stop("Argument 'x' must be of class 'miclust'.")

  nusedimp <- length(x$usedimp)

  if (nusedimp == 0)
    stop("Nothing to plot for a single dataset analysis.")

  if ((!inherits(k, "NULL")) && !is.na(k) && (((length(k) != 1L) || (k < 2) || (floor(k) != ceiling(k)))))
    stop("The number of clusters, 'k', must be a integer greater than 1.")

  kmax <- max(x$ks)

  if ((!inherits(k, "NULL")) && !is.na(k) && (k > kmax))
    stop(paste0("The number of clusters, 'k', cannot be greater than the maximum explored number of clusters, ", max(x$ks), "."))

  if (inherits(k, "NULL") || is.na(k))
    k <- x$kfin


  metric <- match.arg(metric)

  if (x$search == "none" && metric %in% c("nvarfreq", "varsel"))
    stop("In this analysis, 'metric' must be 'all', 'nclfreq' or 'critcf'.")


  y <- 100 * x$selectedkdistribution / sum(x$selectedkdistribution)
  ymax <- max(y)


  # If user sets col.all, apply it to all plots:
  if (!is.null(col.all)) {
    col.nclfreq <- col.all
    col.critcf <- col.all
    col.nvarfreq <- col.all
    col.varsel <- col.all
  }


  ### Save and restore global parameters ONLY if plotting
  ### all metrics together (metric = "all") because in that
  ### case we modify mfrow:
  if (metric == "all") {
    oldpar <- par(no.readonly = TRUE)  # make a copy of current par settings
    on.exit(par(oldpar))               # reset par setting on exit
  }


  #################################

  if (x$search == "none") {
    ### set mfrow depending on the number of plots:
    if (metric == "all") {
      ### case of all metrics ("nclfreq" and "critcf").
      ### increase top margin for outer title:
      par(oma = c(0, 0, 1, 0), mfrow = c(1, 2), las = 1)
    } else {
      ### case of one metric ("nclfreq" or "critcf"):
      par(las = 1)
    }

    ### plot "nclfreq"?:
    if (metric %in% c("all", "nclfreq"))
      barplot(y, main = "", names.arg = paste("k", x$ks, sep = "="),
              xlab = "Number of clusters", ylab = "Selection frequency (%)",
              ylim = c(0, ymax), col = col.nclfreq, ...)

    ### plot "critcf"?:
    if (metric %in% c("all", "critcf"))
      boxplot(x$critcf, main = "", xlab = "Number of clusters",
              ylab = "CritCF", col = col.critcf, ...)

  }


  #################################

  if (x$search != "none") {
    ### set mfrow depending on the number of plots:
    if (metric == "all") {
      ### case of all metrics ("nclfreq" and "critcf").
      ### increase top margin for outer title:
      par(oma = c(0, 0, 1, 0), mfrow = c(2, 2), las = 1)
    } else {
      ### case of one metric ("nclfreq" or "critcf"):
      par(las = 1)
    }

    ### plot "nclfreq"?:
    if (metric %in% c("all", "nclfreq"))
      barplot(y, main = "", names.arg = paste("k", x$ks, sep = "="),
              xlab = "Number of clusters", ylab = "Selection frequency (%)",
              ylim = c(0, ymax), col = col.nclfreq, ...)


    ### plot "critcf"?:
    if (metric %in% c("all", "critcf"))
      boxplot(x$critcf, main = "", xlab = "Number of clusters",
              ylab = "CritCF", col = col.critcf, ...)


    ### plot "nvarfreq" and/or "varsel"?:
    # the following objects are commom to metrics "nvarfreq" and "varsel":
    # xmin, xmax, nv, selectedvarssummary,
    # q1number, mediannumber, meannumber, q3number
    if (metric %in% c("all", "nvarfreq", "varsel")) {
      selectedvars <- list()
      numberofselectedvariables <- vector()
      for (i in 1:nusedimp) {
        selectedvars[[i]] <- x$clustering[[i]][[paste0("k=", k)]]$selectedvariables
        numberofselectedvariables[i] <- length(selectedvars[[i]])
      }
      selectedvars <- as.factor(unlist(selectedvars))
      selectedvarssummary <- as.data.frame(sort(summary(selectedvars),
                                                decreasing = TRUE))
      selectedvarssummary <- selectedvarssummary * 100 / nusedimp
      xmin <- min(selectedvarssummary)
      xmax <- max(selectedvarssummary)
      nv <- dim(selectedvarssummary)[1]
      mediannumber <- median(numberofselectedvariables)
      meannumber <- mean(numberofselectedvariables)
      aux <- quantile(numberofselectedvariables, probs = c(1 / 4, 3 / 4))
      q1number <- aux[1]
      q3number <- aux[2]
      rm(aux)
    }
    ### plot "nvarfreq"?:
    if (metric %in% c("all", "nvarfreq")) {
      aux <- table(numberofselectedvariables)
      aux <- 100 * aux / sum(aux)
      medianfancy <- sprintf("%.1f", mediannumber)
      meanfancy <- sprintf("%.1f", meannumber)
      sdfancy <- sprintf("%.1f", sd(numberofselectedvariables))
      q1fancy <- sprintf("%.1f", q1number)
      q3fancy <- sprintf("%.1f", q3number)
      summ <- paste0("(Mean = ", meanfancy,
                     ", Median = ", medianfancy,
                     ", Sd = ", sdfancy,
                     ", Q1 = ", q1fancy,
                     ", Q3 = ", q3fancy, ")")

      barplot(aux, main = "", ylab = "Frequency of selection (%)",
              xlab = paste("Number of selected variables for", k, "clusters"),
              col = col.nvarfreq, ...)

      mtext(summ, side = 3, line = 1, cex = 0.8)
    }

    ### plot "varsel"?:
    if (metric %in% c("all", "varsel")) {
      ### Adapt left margin to the length of the labels in Y-axis:
      # Extract variable names (labels on the Y-axis):
      vars_labels <- rownames(selectedvarssummary)
      # Count the number of characters of the longest variable name:
      max_char_length <- max(nchar(vars_labels))

      # Dynamically calculate the left margin based on the longest name.
      # We allow approximately 0.5 lines of margin per character.
      # A minimum of 4.1 (R's default) ensures no excess blank space for
      # short names:
      dynamic_left_mar <- max(4.1, max_char_length * 0.5)

      # # Save the current graphical parameters for this specific panel:
      # current_par <- par(no.readonly = TRUE)
      # Guardamos SOLO el vector de margenes actual
      oldmar <- par("mar")

      # Update ONLY the left margin, i.e., the 2nd element in the
      # c(bottom, left, top, right) vector.
      # This affects only this plot, even within an mfrow layout.
      # par(mar = c(current_par$mar[1],
      #             dynamic_left_mar,
      #             current_par$mar[3],
      #             current_par$mar[4]))
      par(mar = c(oldmar[1],
                  dynamic_left_mar,
                  oldmar[3],
                  oldmar[4]))

      # plot:
      plot(c(xmin, xmax), c(1, nv), type = "n", yaxt = "n", main = "",
           xlab = paste("Variable selection frequency for", k, "clusters (%)"),
           ylab = "", xlim = c(0, 100), ylim = c(1, nv * 1.3), ...)

      axis(2, at = 1:nv, rownames(selectedvarssummary)[nv:1], cex.axis = 0.6)

      segments(x0 = rep(0, nv),
               y0 = 1:nv,
               x1 = selectedvarssummary[nv + 1 - (1:nv), 1],
               y1 = 1:nv,
               lwd = 1.5, col = col.varsel)

      abline(h = nv + 1 - q1number, lty = 3)
      abline(h = nv + 1 - mediannumber, lty = 2)
      abline(h = nv + 1 - meannumber, lty = 4)
      abline(h = nv + 1 - q3number, lty = 3)
      legend("top", title = "Number of selected variables",
             legend = c("Q1 and Q3", "Median", "Mean"),
             horiz = TRUE, bty = "n",
             lty = c(3, 2, 4), cex = 0.6)

      # Restore the previous margin settings right after completing this plot
      # par(mar = current_par$mar)
      par(mar = oldmar)
    }
  }

  ### add an outer title only if metric = "all":
  if (metric == "all")
    title(paste("Between-imputations results using", nusedimp, "imputed datasets"),
          outer = TRUE)
}
