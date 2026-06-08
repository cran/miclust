#' Cohen's kappa.
#'
#' \code{kappaunweighted} computes unweighted Cohen's kappa.
#' @param x 2-column matrix internally provided by \code{relabelclusters}
#'   function.
#' @return internal value to be used by \code{relabelclusters} function.
#' @noRd
kappaunweighted <- function(x) {
  x <- x[complete.cases(x), ]

  if (nrow(x) == 0)
    stop("Error: No complete cases available to calculate Kappa.\n")

  ## Extract columns:
  rater1 <- x[, 1]
  rater2 <- x[, 2]

  ### Ensure both vectors are treated with the exact same levels
  levels <- sort(unique(c(rater1, rater2)))

  # 3. Build the observed confusion matrix
  tab <- table(factor(rater1, levels = levels),
               factor(rater2, levels = levels))

  # 4. Calculate observed agreement (Po) as the sum of the diagonal proportions
  p_ij <- tab / sum(tab)
  Po <- sum(diag(p_ij))

  # 5. Calculate expected agreement by chance (Pe) using marginal totals
  marg_f <- rowSums(p_ij)
  marg_c <- colSums(p_ij)
  Pe <- sum(marg_f * marg_c)

  # 6. Apply the classic Cohen's Kappa formula
  if (Pe == 1) {
    return(1) # Handle perfect expected agreement to avoid division by zero
  } else {
    kappa_val <- (Po - Pe) / (1 - Pe)
    return(kappa_val)
  }
}

