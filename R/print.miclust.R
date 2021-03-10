#' Prints the results.
#'
#' Creates a summary print of the results of \code{\link{miclust}}.
#'
#' @param x object of class \code{miclust} obtained with the function \code{\link{miclust}}.
#' @param \dots further arguments for the print method.
#' @return prints a description of the clustering main results.
#' @importFrom stats median
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
        names(res) <- paste0("k=", colnames(res))
      }
    print(res)
  }
}
