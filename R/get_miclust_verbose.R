#' Get verbose text for miclust.
#'
#' Computes character vector verbose for miclust. Internal of \code{miclust}.
#' @param nimp number on imputed datasets analysed.
#' @param breakeach break verbose line each \code{breakeach} imputed datasets.
#' @return internal value to be used by \code{miclust} function.
#' @noRd
get_miclust_verbose <- function(nimp = 100, breakeach = 20) {
  # sequence of imputations:
  imps <- 1:nimp

  # BEGIN create verbose labels of equal width:
  # maximum verbose width:
  w <- nchar(nimp)
  # format (e.g., "%3d" if w = 3)
  mask <- paste0("%", w, "d")
  # apply mask to get pretty labels:
  imp_labels <- sprintf(mask, imps)

  ### verbose vector:
  verbmes <- rep(".", nimp)
  verbmes[(imps %% 5) == 0] <- paste("imp", imp_labels[(imps %% 5) == 0])

  ### break_line every breakline_each imputations:
  verbbreak <- rep("", nimp)
  verbbreak[(imps %% breakeach) == 0] <- "\n"

  ### vebose vector with breaks:
  res <- paste(verbmes, verbbreak)
  return(res)
}

