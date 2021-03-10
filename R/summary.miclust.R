#' Summarizes the results.
#'
#' Performs a within-cluster descriptive analysis of the variables after the
#'   clustering process performed by the function \code{\link{miclust}}.
#'
#' @param object object of class \code{miclust} obtained with the function \code{\link{miclust}}.
#' @param k number of clusters. The default value is the optimal number of clusters
#'   obtained by \code{\link{miclust}}.
#' @param quantilevars numeric. If a variable selection procedure was used, the
#'   cut-off percentile in order to decide the number of selected variables in the
#'   variable reduction procedure by decreasing order of presence along the imputations
#'   results. The default value is \code{quantilevars} = 0.5, i.e., the number of
#'   selected variables is the median number of selected variables along the imputations.
#' @param \dots further arguments for the plot function.
#' @return An object with classes c("list", "summary.miclust") including the following items:
#'  \describe{
#'    \item{allocationprobabilities}{if imputations were analyzed, descriptive
#'      summary of the probability of cluster assignment.}
#'    \item{classmatrix}{if imputations were analyzed, the individual probabilities
#'      of cluster assignment.}
#'    \item{cluster}{if imputations were analyzed, the final individual cluster
#'      assignment.}
#'    \item{clusterssize}{if imputations were analyzed, size of the imputed
#'      cluster and between-imputations summary of the cluster size.}
#'    \item{clustervector}{if a single data set (raw data set) has been clustered,
#'      a vector containing the individuals cluster assignments.}
#'    \item{clustervectors}{if imputed data sets have been clustered, the individual
#'      cluster assignment in each imputation.}
#'    \item{completecasesperc}{if a single data set (raw data set) has been
#'      clustered, the percentage of complete cases in the data set.}
#'    \item{k}{number of clusters.}
#'    \item{kappas}{if imputations were analyzed, the Cohen's kappa values after
#'      comparing the cluster vector in the first imputation with the cluster
#'      vector in each of the remaining imputations.}
#'    \item{kappadistribution}{a summary of \code{kappas}.}
#'    \item{m}{number of imputations used in the descriptive analysis which is the
#'      total number of imputations provided.}
#'    \item{quantilevars}{if variable selection was performed, the input value
#'      of \code{quantilevars}.}
#'    \item{search}{search algorithm for the selection variable procedure.}
#'    \item{selectedvariables}{if variable selection was performed, the selected
#'      variables obtained considering \code{quantilevars}.}
#'    \item{selectedvarspresence}{if imputations were analyzed and variable
#'      selection was performed, the presence of the selected variables along
#'      imputations.}
#'    \item{summarybycluster}{within-cluster descriptive analysis of the selected
#'      variables.}
#'    \item{usedimp}{indicator of imputations used in the clustering procedure.}
#'   }
#' @seealso \code{\link{miclust}}, \code{\link{plot.miclust}}.
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
        if (class(dat) == "numeric") {
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
    auximpcl <- summary(as.factor(assigned))
    imputedclustersize[names(auximpcl)] <- auximpcl
    clusterssize <- cbind(imputedclustersize, clusterssize)
    colnames(clusterssize)[1] <- "size"
    rownames(clusterssize) <- paste("cluster", 1:k)
  }
  aux <- object$data$rawdata[, c(selectedvariables, "cluster")]
  aux <- aux[complete.cases(aux), ]
  aux1 <- aux
  aux1$cluster <- NULL
  aux1 <- as.data.frame(is.na(aux1))
  miss <- 100 * matrixStats::colMeans2(as.matrix(aux1))
  aux1$cluster <- aux$cluster
  aux1 <- aux1 * 1
  aux1 <- 100 * t(summaryBy(. ~ cluster, data = aux1, FUN = mean))[-1, ]
  if (inherits(aux1, "numeric")) {
    aux1 <- t(as.matrix(aux1))
    rownames(aux1) <- selectedvariables
  }
  colnames(aux1) <- paste0("%miss.(cl.", colnames(aux1), ")")
  aux1 <- as.data.frame(aux1)
  aux1 <- cbind(miss, aux1)
  names(aux1)[1] <- "%miss."
  aux2 <- t(summaryBy(. ~ cluster,
                      data = aux,
                      FUN = function(x) mean(x, na.rm = TRUE)))[-1, ]
  if (inherits(aux2, "numeric")) {
    aux2 <- t(as.matrix(aux2))
    rownames(aux2) <- selectedvariables
  }
  colnames(aux2) <- paste0("mean (cl.", colnames(aux2), ")")
  aux2 <- as.data.frame(aux2)
  aux3 <- t(summaryBy(. ~ cluster,
                      data = aux,
                      FUN = function(x) sd(x, na.rm = TRUE)))[-1, ]
  if (inherits(aux3, "numeric")) {
    aux3 <- t(as.matrix(aux3))
    rownames(aux3) <- selectedvariables
  }
  colnames(aux3) <- paste0("sd (cl.", colnames(aux3), ")")
  aux3 <- as.data.frame(aux3)
  aux <- cbind(aux2, aux3)
  rownames(aux) <- selectedvariables
  id1 <- rep(1:k, each = 2)
  id2 <- rep(c(0, k), k)
  id <- id1 + id2
  summarybycluster <- cbind(aux1, aux[, id])

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
