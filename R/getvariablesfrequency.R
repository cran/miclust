#' Calculates the ranked selection frequency of the variables.
#'
#' Creates a ranked selection frequency for all the variables that have been
#'   selected at least once along the analyzed imputed data sets. \code{getvariablesfrequency}
#'   can be useful for customizing the plot of these frequencies as it is shown
#'   in Examples below.
#' @param x an object of class \code{miclust} obtained with the function \code{\link{miclust}}.
#' @param k the number of clusters. The default value is the optimal number of clusters
#'   obtained by the function \code{\link{miclust}}.
#' @return A list including the following items:
#'  \describe{
#'    \item{percfreq}{vector of the selection frequencies (percentage of times) of the
#'      variables in decreasing order.}
#'    \item{varnames}{names of the variables.}
#'   }
#' @export
#' @seealso \code{\link{miclust}}.
#' @examples
#' ### see examples in miclust.
getvariablesfrequency <- function(x, k = NULL) {
  if (!inherits(x, "miclust"))
    stop("Argument 'x' must be of class 'miclust'.")

  if (x$search == "none")
    stop("Argument object 'search' in 'x' cannot be 'none'.")

  if (class(k) != "NULL" && !is.na(k) && (((length(k) != 1L) || (k < 2) || (floor(k) != ceiling(k)))))
    stop("The number of clusters, 'k', must be an integer greater than 1.")

  if (class(k) != "NULL" && !is.na(k) && (k > x$kmax))
    stop(paste0("The number of clusters, 'k', cannot be greater than the maximum explored number of clusters, ", x$kmax, "."))

  if (class(k) == "NULL" || is.na(k))
    k <- x$kfin

  selectedvars <- list()
  numberofselectedvariables <- vector()
  m <- length(x$usedimp)
  for (i in 1:m) {
    selectedvars[[i]] <- x$clustering[[i]][[paste0("k=", k)]]$selectedvariables
    numberofselectedvariables[i] <- length(selectedvars[[i]])
   }
  selectedvars <- as.factor(unlist(selectedvars))
  selectedvarssummary <- as.data.frame(sort(summary(selectedvars), decreasing = TRUE))
  selectedvarssummary <- selectedvarssummary * 100 / m
  res <- list(percfreq = selectedvarssummary[, 1],
              varnames = rownames(selectedvarssummary))
  return(res)
}
