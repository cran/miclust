#' Summarizes the results.
#'
#' Performs a within-cluster descriptive analysis of the variables after the
#'   clustering process performed by the function \code{\link{miclust}}.
#'
#' @param object object of class \code{miclust} obtained with the function \code{\link{miclust}}.
#' @param k number of clusters. The default value is the optimal number of
#'   clusters obtained by \code{\link{miclust}}.
#' @param quantilevars numeric. If a variable selection procedure was used,
#'   the cut-off percentile in order to decide the number of selected variables
#'   in the variable reduction procedure by decreasing order of presence along
#'   the imputations results. The default value is \code{quantilevars} = 0.5,
#'   i.e., the number of selected variables is the median number of selected
#'   variables along the imputations.
#' @param \dots further arguments for \code{summary} and \code{print.summary}
#'   methods.
#' @return A \code{list} including the following items:
#'  \describe{
#'    \item{allocationprobabilities}{if imputations were analysed, descriptive
#'      summary of the probability of cluster assignment.}
#'    \item{classmatrix}{if imputations were analysed, the individual
#'      probabilities of cluster assignment.}
#'    \item{cluster}{if imputations were analysed, the final individual cluster
#'      assignment.}
#'    \item{clusterssize}{if imputations were analysed, size of the imputed
#'      cluster and between-imputations summary of the cluster size.}
#'    \item{clustervector}{if a single dataset (raw dataset) has been
#'      clustered, a vector containing the individuals cluster assignments.}
#'    \item{clustervectors}{if imputed datasets have been clustered, the
#'      individual cluster assignment in each imputation.}
#'    \item{completecasesperc}{if a single dataset (raw dataset) has been
#'      clustered, the percentage of complete cases in the dataset.}
#'    \item{k}{number of clusters.}
#'    \item{kappas}{if imputations were analysed, the Cohen's kappa values after
#'      comparing the cluster vector in the first imputation with the cluster
#'      vector in each of the remaining imputations.}
#'    \item{kappadistribution}{a summary of \code{kappas}.}
#'    \item{m}{number of imputations used in the descriptive analysis which is
#'      the total number of imputations provided.}
#'    \item{quantilevars}{if variable selection was performed, the input value
#'      of \code{quantilevars}.}
#'    \item{search}{search algorithm for the selection variable procedure.}
#'    \item{selectedvariables}{if variable selection was performed, the selected
#'      variables obtained considering \code{quantilevars}.}
#'    \item{selectedvarspresence}{if imputations were analysed and variable
#'      selection was performed, the presence of the selected variables along
#'      imputations.}
#'    \item{summarybycluster}{within-cluster descriptive analysis of the
#'      selected variables.}
#'    \item{usedimp}{indicator of imputations used in the clustering procedure.}
#'   }
#' @seealso \code{\link{miclust}}.
#' @examples
#' ### see examples in miclust.
#' @import stats
#' @importFrom doBy summaryBy
#' @importFrom matrixStats colMeans2
#' @export
summary.miclust <- function(object, k = NULL, quantilevars = NULL, ...) {
  if(!inherits(object, "miclust"))
    stop("Argument 'object' must be of class 'miclust'.")

  if (!is.null(k) && !is.na(k) && (((length(k) != 1L) || (k < 2) || (floor(k) != ceiling(k)))))
    stop("The number of clusters, 'k', must be a integer greater than 1.")

  kmax <- max(object$ks)

  if (!is.null(k) && !is.na(k) && (k > kmax))
     stop(paste0("The number of clusters, 'k', cannot be greater than the maximum explored number of clusters, ",
                kmax,
                ".")
          )

  if (is.null(k) || is.na(k))
    k <- object$kfin

  if (!is.null(quantilevars) && !is.na(quantilevars) && (((length(quantilevars) != 1L) || (quantilevars <= 0) || (quantilevars > 1))))
   	  stop("The argument 'quantilevars' must be in (0, 1].")

  if ((object$search != "none") && (is.null(quantilevars) || is.na(quantilevars))) {
    quantilevars <- 0.5
    warn <- paste0("'quantilevars' not provided. Setting it to ", quantilevars, ".\n")
    warning(warn)
  }

  nusedimp <- length(object$usedimp)

  ##########################
  ##########################
  ###
  ###  single analysis
  ###
  ##########################
  ##########################

  if (nusedimp == 0) {
    ##########################
    if (object$search == "none") {
      selectedvariables <- colnames(object$data$rawdata)
      cluster <- object$clustering[[paste0("k=", k)]]$clustervector
    }
    ##########################
    if (object$search != "none") {
        selectedvars <- object$clustering[[paste0("k=", k)]]$selectedvariables
        cutpoint <- round(quantile(object$numberofselectedvars, probs = quantilevars))
        if (cutpoint == 0)
          stop(paste0("No variables selected if 'quantilevars' is ", quantilevars, "."))
        selectedvariables <- selectedvars[1:cutpoint]
        dat <- object$data$rawdata[, selectedvariables]

        if (inherits(dat, "numeric")) {
          dat <- as.data.frame(dat)
          names(dat) <- selectedvariables
        }
        idcc <- complete.cases(dat)
        dat <- dat[idcc, ]
        n <- nrow(object$data$rawdata)
        cluster <- rep(NA, n)
        mod <- kcca(x = dat, k = k, family = object$selmetriccent, simple = TRUE)
        cluster[idcc] <- mod@cluster
      }
    object$data$rawdata$cluster <- cluster
  }

  ##########################
  ##########################
  ##########################

  if (nusedimp > 0) {
    M <- length(object$data$impdata)
    n <- nrow(object$data$rawdata)
    ##########################
    if (object$search != "none") {
      selectedvars <- list()
      numberofselectedvariables <- vector()
      for (i in 1:nusedimp) {
        selectedvars[[i]] <- object$clustering[[i]][[paste0("k=", k)]]$selectedvariables
        numberofselectedvariables[i] <- length(selectedvars[[i]])
      }
      selectedvars <- as.factor(unlist(selectedvars))
      selectedvarssummary <- as.data.frame(sort(summary(selectedvars), decreasing = TRUE))
      selectedvarssummary <- selectedvarssummary * 100 / nusedimp
      tab <- data.frame(Variable = rownames(selectedvarssummary),
                        frequencypercent = selectedvarssummary[, 1])
      names(tab)[2] <- "Presence(%)"
      cutpoint <- round(quantile(numberofselectedvariables, probs = quantilevars))
      if (cutpoint == 0)
        stop(paste0("No variables have been selected if 'quantilevars' is ", quantilevars, "."))
      selectedvariables <- as.character(tab$Variable[1:cutpoint])
    }
    ##########################
    if (object$search == "none") {
      selectedvariables <- names(object$data$rawdata)
    }
    ##########################

    ##################################################################
    ##################################################################
    ###
    ### BEGIN control empty clusters
    ###
    ### In cases where the cluster vector in any imputation or the
    ### "assigned" cluster has an empty cluster, assignprobandkappas()
    ### throws an error because it calls kcca() requesting to create
    ### more clusters than "initialcluster" indicates.
    ### Even without this error, relabelclusters() re-labeling and the
    ### result of assignprobandkappas() would not produce valid results
    ### or would throw an error.
    ###
    ### As a workaround for these cases, I send an error message in
    ### those cases and then summary.miclust() will not return any
    ### results.
    ###
    ##################################################################
    ##################################################################
    ### check for empty clusters:
    # matrix of clusters imp1, ..., impM, assigned:
    cluster_matrix <- object$clustering$clusters[[paste0("k=", k)]]
    # cluster sizes in imp1, ..., impM, assigned:
    cluster_table <- t(apply(cluster_matrix,
                             MARGIN = 2,
                             function(x) {
                               res <- table(factor(x, levels = 1:k))
                               return(res)
                             }))
    # number of empty clusters in imp1, ..., impM, assigned:
    n_empty_clusters <- k - rowSums(cluster_table > 0)
    # keep imputations and/or assigned with empty cluster:
    n_empty_clusters <- n_empty_clusters[n_empty_clusters > 0]
    n_empty_clusters <- data.frame(scenario = names(n_empty_clusters),
                                   number_of_empty_clusters = n_empty_clusters)
    rownames(n_empty_clusters) <- NULL
    if (nrow(n_empty_clusters) > 0) {
      # Print the explanatory message to the console
      cat("summary() cannot be applied in this analysis with k = ", k,
          " because the following cases have empty clusters:\n\n",
          sep = "")

      # Print the data frame showing the specific imputations/assigned cases
      print(n_empty_clusters)

      # Add an aesthetic line break before throwing the error
      cat("\n")

      # Halt execution cleanly without showing the internal function call
      stop("The execution has been stopped due to the empty clusters listed above.",
           call. = FALSE)
    }
    ##################################################################
    ##################################################################
    ###
    ### END control empty clusters
    ###
    ##################################################################
    ##################################################################

    aux <- assignprobandkappas(variables = selectedvariables,
                               k = k,
                               metriccent = object$selmetriccent,
                               data = object$data$impdata,
                               initialcluster = object$clustering$clusters[[paste0("k=", k)]]$assigned)
    classmatrix <- aux$classmatrix
    clustervectors <- aux$clustervectors
    kappas <- aux$kappas
    kappadistribution <- quantile(kappas, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
    kappadistribution <- c(kappadistribution[1:3], mean(kappas), kappadistribution[4:5])
    names(kappadistribution)[4] <- "mean"
    aux <- classmatrix
    assigned <- rep(NA, n)
    for (i in 1:n) {
      x <- aux[i, ]
      assig <- which(x == max(x))
      if (length(assig) > 1) {
        l <- length(assig)
        sel <- sample(1:l, 1)
        assig <- assig[sel]
      }
      assigned[i] <- assig
    }
    aux$prob <- apply(aux, 1, max)
    aux$assigned <- assigned
    aux <- aux[, c("assigned", "prob")]
    aux <- summaryBy(prob ~ assigned, data = aux, FUN = function(x) quantile(x, probs = 0:4/4))
    aux <- aux[, -1]
    rownames(aux) <- paste("Assigned to cluster", 1:nrow(aux))
    colnames(aux) <- c("min", paste0("Q", 1:3), "max")
    allocationprobabilities <- aux
    object$data$rawdata$cluster <- assigned
    aux <- data.frame(matrix(0, nrow = M, ncol = k))
    for (i in 1:M) {
      v <- clustervectors[, i]
      object$data$impdata[[i]]$cluster <- v
      for (j in 1:k)
        aux[i, j] <- sum(v == j)
    }
    clusterssize <- t(apply(aux, 2, FUN = function(x) quantile(x, probs = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1))))
    clusterssize <- round(cbind(clusterssize[, 1:4], matrixStats::colMeans2(as.matrix(aux)), clusterssize[, 5:7]), 1)
    colnames(clusterssize)[c(1, 5, 8)] <- c("min.", "mean", "max.")
    imputedclustersize <- rep(0, k)
    names(imputedclustersize) <- 1:k
    ### fuerzo a que tenga todos los niveles 1:k:
    # auximpcl <- summary(as.factor(assigned))
    auximpcl <- summary(factor(assigned, levels = 1:k))
    imputedclustersize[names(auximpcl)] <- auximpcl
    clusterssize <- cbind(imputedclustersize, clusterssize)
    colnames(clusterssize)[1] <- "size"
    rownames(clusterssize) <- paste("cluster", 1:k)
  }
  aux <- object$data$rawdata[, c(selectedvariables, "cluster")]
  ### BEGIN missingness by variable and cluster:
  isna_selectedvars_by_cluster <- as.data.frame(is.na(aux))
  isna_selectedvars_by_cluster$cluster <- aux$cluster
  # missingnes by variable:
  miss <- 100 * matrixStats::colMeans2(as.matrix(isna_selectedvars_by_cluster))
  miss <- miss[-length(miss)]
  # missingnes by variable and cluster:
  isna_selectedvars_by_cluster$cluster <- factor(isna_selectedvars_by_cluster$cluster,
                                                 levels = 1:k)
  miss_by_cluster <- aggregate(. ~ cluster,
                               data = isna_selectedvars_by_cluster,
                               FUN = function(x) 100 * mean(x, na.rm = TRUE),
                               drop = FALSE) # force all levels to appear
  miss_by_cluster$cluster <- NULL
  miss_by_cluster <- as.data.frame(t(miss_by_cluster))
  names(miss_by_cluster) <- paste0("%miss.(cl.", 1:k, ")")
  missingness <- cbind(miss, miss_by_cluster)
  names(missingness)[1] <- "%miss."
  ### END missingness by variable and cluster:

  ### means by variable and cluster:
  aux <- aux[complete.cases(aux), ]
  aux$cluster <- factor(aux$cluster, levels = 1:k)
  means_by_cluster <- aggregate(. ~ cluster,
                               data = aux,
                               FUN = function(x) mean(x, na.rm = TRUE),
                               drop = FALSE) # force all levels to appear

  means_by_cluster$cluster <- NULL
  means_by_cluster <- as.data.frame(t(means_by_cluster))
  names(means_by_cluster) <- paste0("mean (cl.", 1:k, ")")

  ### sds by variable and cluster:
  sds_by_cluster <- aggregate(. ~ cluster,
                                data = aux,
                                FUN = function(x) sd(x, na.rm = TRUE),
                                drop = FALSE) # force all levels to appear

  sds_by_cluster$cluster <- NULL
  sds_by_cluster <- as.data.frame(t(sds_by_cluster))
  names(sds_by_cluster) <- paste0("sd (cl.", 1:k, ")")

  summarybycluster <- cbind(missingness, means_by_cluster, sds_by_cluster)

  if (nusedimp == 0) {
    res <- list(m = 0,
                usedimp = object$usedimp,
                completecasesperc = object$completecasesperc,
                k = k,
                selectedvariables = selectedvariables,
                clustervector = cluster,
                summarybycluster = summarybycluster,
                quantilevars = quantilevars,
                search = object$search)
  }

  if (nusedimp > 0) {
    if (object$search == "none") {
      res <- list(m = M,
                  k = k,
                  classmatrix = classmatrix,
                  clustervectors = clustervectors,
                  kappas = kappas,
                  kappadistribution = kappadistribution,
                  cluster = assigned,
                  clusterssize = clusterssize,
                  allocationprobabilities = allocationprobabilities,
                  summarybycluster = summarybycluster,
                  usedimp = object$usedimp,
                  search = object$search)
    }

    if (object$search != "none") {
      res <- list(m = M,
                  k = k,
                  quantilevars = quantilevars,
                  selectedvarspresence = tab,
                  selectedvariables = selectedvariables,
                  classmatrix = classmatrix,
                  clustervectors = clustervectors,
                  kappas = kappas,
                  kappadistribution = kappadistribution,
                  cluster = assigned,
                  clusterssize = clusterssize,
                  allocationprobabilities = allocationprobabilities,
                  summarybycluster = summarybycluster,
                  usedimp = object$usedimp,
                  search = object$search)
    }
  }
  class(res) <- c("summary.miclust", class(res))
  return(res)
}





###-------------------------------------------
###-------------------------------------------
###-------------------------------------------
###-------------------------------------------
###
###     S3  Method print.summary.miclust
###
###-------------------------------------------
###-------------------------------------------
###-------------------------------------------
###-------------------------------------------

#' @rdname summary.miclust
#' @param x for the \code{print.summary} method, an object obtained with the
#'   method \code{summary}.
#' @param digits digits for the \code{print.summary} method. Default is 2.
#' @importFrom stats median
#' @export
print.summary.miclust <- function(x, digits = 2, ...) {
  ### control "x"
  if (!inherits(x, "summary.miclust"))
    stop("Argument 'x' must be of class 'summary.miclust'.")

  nusedimp <- length(x$usedimp)


  ####################
  ###
  ### BEGIN narrower format for x$summarybycluster
  ###
  ####################
  ## aux copy:
  auxsummary <- x$summarybycluster

  ## add fancy columns:
  for (i in 1:x$k) {
    cols2sel <- paste0(c("%miss.(cl.", "mean (cl.", "sd (cl."), i, ")")
    aux <- auxsummary[, cols2sel]
    missi <- sprintf(paste0("%.", digits, "f"), aux[, 1])
    meansi <- sprintf(paste0("%.", digits, "f"), aux[, 2])
    sdsi <- sprintf(paste0("%.", digits, "f"), aux[, 3])
    auxsummary$newcol <- paste0(missi, ";", meansi, ";", sdsi)
    names(auxsummary)[which(names(auxsummary) == "newcol")] <- paste0("cl.", i)
    rm(cols2sel, aux, missi, meansi, sdsi)
  }
  ## keep only columns of interest:
  auxsummary <- auxsummary[, c("%miss.", paste0("cl.", 1:x$k))]
  x$summarybycluster <- auxsummary

  rm(auxsummary)
  ####################
  ###
  ### END narrower format for x$summarybycluster
  ###
  ####################


  ####################
  if (nusedimp == 0) {
    cat("\n")
    cat("   Results for complete cases (",
        x$completecasesperc,
        "% of cases) and for ",
        x$k,
        " clusters:\n", sep = "")
    cat("---------------------------------------------------------------------\n")
    cat("Selected variables:\n")
    print(x$selectedvariables)
    cat("\n")
    cat("Cluster vector:\n")
    print(x$cluster)
    cat("\n")
    cat("Within-cluster summary [%miss.;mean;sd]:\n", sep = "")
    print(x$summarybycluster, digits = digits)
  }

  ####################

  if (nusedimp > 0) {
    cat("\n")
    cat("Results using:\n")
    cat("   ", nusedimp, "imputed datasets for the cluster analysis\n")
    cat("   ", x$m, "imputed datasets for the descriptive summary\n")
    cat("   ", x$k, "as the final number of clusters\n")
    cat("-----------------------------------------------------------\n")
    cat("\n")
    if (x$search != "none") {
      cat("Presence of the variables in the subset of selected variables:\n")
      print(x$selectedvarspresence)
      cat("\n")
      cat("Selected variables:\n")
      print(x$selectedvariables)
      cat("\n")
    }
    cat("Cohen's kappa between-imputations distribution (",
        length(x$kappas),
        " comparisons):\n",
        sep = "")
    print(x$kappadistribution, digits = digits)
    cat("\n")
    cat("Between-imputation clusters size distribution (",
        1 + length(x$kappas),
        " imputations):\n",
        sep = "")
    print(x$clusterssize, digits = digits)
    cat("\n")
    cat("Probability of assignment to the cluster distribution (",
        1 + length(x$kappas),
        " imputations):\n",
        sep = "")
    print(x$allocationprobabilities, digits = digits)
    cat("\n")
    cat("Within-cluster summary (",
        1 + length(x$kappas),
        " imputations) [%miss.;mean;sd]:\n", sep = "")
    print(x$summarybycluster, digits = digits)
  }
}
