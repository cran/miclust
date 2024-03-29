% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/miclust-package.R
\docType{package}
\name{miclust-package}
\alias{miclust-package}
\title{miclust-package: integrating multiple imputation with cluster analysis}
\description{
Cluster analysis with selection of the final number of clusters
  and an optional variable selection procedure. The package is designed to
  integrate the results of multiply imputed data sets while accounting for the
  uncertainty that the imputations introduce in the final results. See `Procedure'
  below for further details on how the tool works.
}
\section{Procedure}{
 The tool consists of a two-step procedure. In the first step,
  the user provides the data to be analyzed. They can be a single data.frame or a
  list of data.frames including the raw data and the imputed data sets. In the
  latter case, \code{getdata} needs to by used first to get data prepared. In the
  second step, the \code{miclust} performs k-means clustering with selection of
  the final number of clusters and an optional (backward or forward) variable
  selection procedure. Specific \code{summary} and \code{plot} methods are provided
  to summarize and visualize the impact of the imputations on the results.
}

\section{Authors}{

  Jose Barrera-Gomez (maintainer, <jose.barrera@isglobal.org>) and Xavier Basagana.
}

\references{
The methodology used in the package is described in

Basagana X, Barrera-Gomez J, Benet M, Anto JM, Garcia-Aymerich J. A Framework
for Multiple Imputation in Cluster Analysis. \emph{American Journal of
Epidemiology}. 2013;177(7):718-725.
}
