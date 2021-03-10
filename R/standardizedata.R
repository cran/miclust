#' Standardize data.
#'
#' \code{standardizedata} standardizes variables in data.
#' @param data internally provided by \code{getdata} function.
#' @return internal value to be used by \code{getdata} function.
#' @keywords internal
#' @importFrom stats sd
standardizedata <- function(data) {
  xs <- as.matrix(centerdata(data))
  p <- ncol(data)
  for (i in 1:p) {
    x <- xs[, i]
    if (length(levels(factor(x))) > 2)
      xs[, i] <- x / sd(x, na.rm = TRUE)
    if (length(levels(factor(x))) == 2)
      xs[, i] <- x / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  }
  xs <- as.data.frame(xs)
  names(xs) <- names(data)
  return(xs)
 }
