#' Shows a graphical representation of the results.
#'
#' Creates a graphical representation of the results of \code{\link{miclust}}.
#'
#' @param x object of class \code{miclust} obtained with the function \code{\link{miclust}}.
#' @param k number of clusters. The default value is the optimal number of clusters
#'   obtained by \code{\link{miclust}}.
#' @param \dots further arguments for the plot function.
#' @return a plot to visualize the clustering results.
#' @seealso \code{\link{miclust}}, \code{\link{summary.miclust}}.
#' @import graphics
#' @export
plot.miclust <- function(x, k = NULL, ...) {
  if (!inherits(x, "miclust"))
    stop("Argument 'x' must be of class 'miclust'.")

  nusedimp <- length(x$usedimp)

  if (nusedimp == 0)
    stop("Nothing to plot for a single data set analysis.")

  if (class(k) != "NULL" && !is.na(k) && (((length(k) != 1L) || (k < 2) || (floor(k) != ceiling(k)))))
    stop("The number of clusters, 'k', must be a integer greater than 1.")

  kmax <- max(x$ks)

  if (class(k) != "NULL" && !is.na(k) && (k > kmax))
    stop(paste0("The number of clusters, 'k', cannot be greater than the maximum explored number of clusters, ", max(x$ks), "."))

  if (class(k) == "NULL" || is.na(k))
    k <- x$kfin

  y <- 100 * x$selectedkdistribution / sum(x$selectedkdistribution)
  ymax <- max(y)


  oldpar <- par(no.readonly = TRUE)      # make a copy of current par settings
  on.exit(par(oldpar))                   # reset par setting on exit


  #################################

  if (x$search == "none") {
    par(oma = c(0, 0, 1, 0), mfrow = c(1, 2), las = 1)
    barplot(y, main = "", names.arg = paste("k", x$ks, sep = "="),
            xlab = "Number of clusters", ylab = "Selection frequency (%)",
            ylim = c(0, ymax))
    boxplot(x$critcf, col = "gray", main = "", xlab = "Number of clusters",
            ylab = "CritCF")
  }

  #################################

  if (x$search != "none") {
    par(oma = c(0, 0, 1, 0), mfrow = c(2, 2), las = 1)
    ########
    barplot(y, main = "", names.arg = paste("k", x$ks, sep = "="),
            xlab = "Number of clusters", ylab = "Selection frequency (%)",
            ylim = c(0, ymax))
    ########
    boxplot(x$critcf, col = "gray", main = "", xlab = "Number of clusters",
            ylab = "CritCF")
    ########
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
            xlab = paste("Number of selected variables for", k, "clusters"))
    mtext(summ, side = 3, line = 1, cex = 0.8)
    ########
    plot(c(xmin, xmax), c(1, nv), type = "n", yaxt = "n", main = "",
         xlab = paste("Variable selection frequency for", k, "clusters (%)"),
         ylab = "", xlim = c(0, 100), ylim = c(1, nv * 1.3)) #xlim = c(xmin, xmax))
    axis(2, at = 1:nv, rownames(selectedvarssummary)[nv:1], cex.axis = 0.6)
    for (i in 1:nv)
      segments(0, i, selectedvarssummary[nv + 1 - i, 1], i, lwd = 1.5)
    abline(h = nv + 1 - q1number, lty = 3)
    abline(h = nv + 1 - mediannumber, lty = 2)
    abline(h = nv + 1 - meannumber, lty = 4)
    abline(h = nv + 1 - q3number, lty = 3)
    legend("top", title = "Number of selected variables",
           legend = c("Q1 and Q3", "Median", "Mean"),
           horiz = TRUE, bty = "n",
           lty = c(3, 2, 4), cex = 0.6)
    title(paste("Between-imputations results using", nusedimp, "imputed datasets"),
          outer = TRUE)
  }
}
