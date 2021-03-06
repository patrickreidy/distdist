% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distances.R
\name{pMetric}
\alias{pMetric}
\alias{pMetric,Distribution,Distribution-method}
\title{p-Metric}
\usage{
\S4method{pMetric}{Distribution,Distribution}(p1, p2, p)
}
\arguments{
\item{p1, p2}{\code{\link[=Distribution-class]{Distributions}}.}

\item{p}{An atomic numeric greater than or equal to 1 that determines the
power of the norm.}
}
\value{
The \emph{p}-metric between distributions \code{p1} and \code{p2}.
}
\description{
Compute the \emph{p}-metric between two probability distributions.
}
\details{
Let \code{p1} and \code{p2} denote the vectors of probability mass assigned
by two distributions defined on the same state space. Then, the \emph{p}-metric
between \code{p1} and \code{p2} is given by \code{(sum(abs(p1 - p2)^p))^(1/p)}.
}
