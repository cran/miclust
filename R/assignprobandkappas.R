#' Computes probabilities of (relabeled) cluster and kappas.
#'
#' \code{assignprobandkappas} returns a list with information on probabilities of
#'   cluster belonging and Cohen's kappas.
#' @param variables internally provided by \code{summary.miclust}.
#' @param k internally provided by \code{summary.miclust}.
#' @param metriccent internally provided by \code{summary.miclust}.
#' @param data internally provided by \code{summary.miclust}.
#' @param initialcluster internally provided by \code{summary.miclust}.
#' @return internal value to be used by \code{summary.miclust}.
#' @keywords internal
#' @importFrom flexclust kcca predict
assignprobandkappas <- function(variables, k, metriccent, data, initialcluster) {
  res <- NULL
  dat <- data[[1]][, variables]
  if (class(dat) == "numeric") {
    dat <- as.data.frame(dat)
    names(dat) <- variables
  }
  mod <- kcca(x = dat, k = initialcluster, family = metriccent, simple = TRUE)
  n <- dim(data[[1]])[1]
  m <- length(data)
  preds <- matrix(nrow = n, ncol = m)
  preds[, 1] <- predict(mod)
  kappas <- rep(NA, m - 1)
  for (i in 2:m) {
    dat <- data[[i]][, variables]
    if (class(dat) == "numeric") {
      dat <- as.data.frame(dat)
      names(dat) <- variables
    }
    mod1 <- kcca(x = dat, k = initialcluster, family = metriccent, simple = TRUE)
    aux <- relabelclusters(refcluster = preds[, 1], cluster = mod1@cluster)
    preds[, i] <- aux$newcluster
    kappas[i - 1] <- aux$kappa
  }
  classmatrix <- matrix(nrow = n, ncol = k)
  for (i in 1:n) {
    aux <- preds[i, ]
    for (j in 1:k)
      classmatrix[i, j] <- sum(aux == j)
  }
  classmatrix <- prop.table(classmatrix, 1)
  classmatrix <- as.data.frame(classmatrix)
  names(classmatrix) <- paste0("C", 1:k)
  preds <- as.data.frame(preds)
  names(preds) <- paste0("imp", 1:m)
  res$classmatrix <- classmatrix
  res$clustervectors <- preds
  res$kappas <- kappas
  return(res)
 }
