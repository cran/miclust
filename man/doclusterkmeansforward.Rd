% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doclusterkmeansforward.R
\name{doclusterkmeansforward}
\alias{doclusterkmeansforward}
\title{Performs K-means with forward selection.}
\usage{
doclusterkmeansforward(
  data,
  k,
  metriccent,
  inertiapower = 1,
  maxvars,
  centpos,
  initcl
)
}
\arguments{
\item{data}{internally provided by \code{doclusterkmeans} function.}

\item{k}{internally provided by \code{doclusterkmeans} function.}

\item{metriccent}{internally provided by \code{doclusterkmeans} function.}

\item{inertiapower}{internally provided by \code{doclusterkmeans} function.}

\item{maxvars}{internally provided by \code{doclusterkmeans} function.}

\item{centpos}{internally provided by \code{doclusterkmeans} function.}

\item{initcl}{internally provided by \code{doclusterkmeans} function.}
}
\value{
internal value to be used by \code{doclusterkmeans} function.
}
\description{
\code{doclusterkmeansforward} performs K-means clustering with forward
  variable selection.
}
\keyword{internal}
