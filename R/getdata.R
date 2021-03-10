#' Creates a \code{midata} object.
#'
#' Creates an object of class \code{miData} to be clustered by the function \code{\link{miclust}}.
#'
#' @param data a \code{list} or \code{data.frame} object. If it is a data frame, it is
#'   assumed to contain just the raw data, with or without missing data. If it
#'   is a list of data frames, it is assumed that the first element contains the
#'   raw data and the remaining ones correspond to multiple imputed data sets.
#'   Since all variables are considered in the clustering procedure, no identifier
#'   variables must be present in the data. In addition, all variables need to be
#'   treated as numeric (i.e. categorical variables must be coded with numeric
#'   values). See Details below.
#' @details All variables in data frames in \code{impdata} are standardized by \code{getdata},
#'   so categorical variables need to be coded with numeric values. Standardization
#'   is performed by centering all variables at the mean and then dividing by the
#'   standard deviation (or the difference between the maximum and the minimum values
#'   for binary variables). Such a standardization is applied only to the imputed
#'   data sets. The standardization of the raw data is internally applied by the
#'   \code{\link{miclust}} if needed (which is the case of analyzing just the raw data, i.e.
#'   complete cases analysis).
#' @return An object of classes c("list", "midata") including the following items:
#'  \describe{
#'    \item{rawdata}{a data frame containing the raw data.}
#'    \item{impdata}{if \code{data} is an object of class \code{list}, \code{impdata} is
#'      a list containing the standardized imputed data sets.}
#'   }
#' @export
#' @seealso \code{\link{miclust}}.
#' @examples
#' ### data minhanes:
#' data(minhanes)
#' class(minhanes)
#'
#' ### number of imputed datasets:
#' length(minhanes) - 1
#'
#' ### raw data with missing values:
#' summary(minhanes[[1]])
#'
#' ### first imputed data set:
#' minhanes[[2]]
#' summary(minhanes[[2]])
#'
#' ### data preparation for a complete case cluster analysis:
#' data1 <- getdata(minhanes[[1]])
#' class(data1)
#' names(data1)
#'
#' ### there are no imputed data sets:
#' data1$impdata
#'
#' ### data preparation for a multiple imputation cluster analysis:
#' data2 <- getdata(minhanes)
#' class(data2)
#' names(data2)
#'
#' ### number of imputed data sets:
#' length(data2$impdata)
#'
#' ### imputed data sets are standardized:
#' summary(data2$rawdata)
#' summary(data2$impdata[[1]])
getdata <- function(data) {
  if (!inherits(data, "list") && (!inherits(data, "data.frame")))
     stop("Argument 'data' must be either a list of data.frames containing the raw data (in the first data.frame) and optionally the imputed data.frames, or a data.frame containing the raw data.")

  res <- NULL
  if ((inherits(data, "list") & (length(data) == 1)) | (inherits(data, "data.frame"))) {
    res$rawdata <- as.data.frame(data)
   	res$impdata <- NA } else {
   	  ndf <- length(data)
   	  classes <- rep(NA, ndf)
   	  for (i in 1:ndf)
   	    classes[i] <- !inherits(data[[i]], "data.frame")
   	  if (any(classes))
   	    stop("Argument 'data' must contain objects of class 'data.frame'.")
   	  rawdata <- data[[1]]
   	  aux <- data
      aux[[1]] <- NULL
      if (any(sapply(aux, anyNA)))
        stop("The imputed data sets cannot contain missing data.")
      ### check all data.frames have the same dimension:
      if (dim(unique(t(sapply(data, FUN = function(x) dim(x)))))[1] != 1)
        stop("All data.frames in 'data' must have the same dimension.")
      ### standardize data:
      m <- length(data) - 1
      res$rawdata <- data[[1]]
      res$impdata <- vector("list", m)
      for (i in 1:m) {
        aux <- data[[i + 1]]
        aux <- standardizedata(data = aux)
        res$impdata[[i]] <- aux
      }
   	}
  class(res) <- c("list", "midata")
  return(res)
}
