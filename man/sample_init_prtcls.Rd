% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{sample_init_prtcls}
\alias{sample_init_prtcls}
\title{Samples initial particles from prior}
\usage{
sample_init_prtcls(mmu, sdd, N)
}
\arguments{
\item{mmu}{mean value as double}

\item{sdd}{standard deviation as double}

\item{N}{number of particles (int)}
}
\value{
a draw from a multivariate normal with equal means (\code{mmu}) and
standard deviations \code{sdd} as a \code{N}x1 arma::colvec
}
\description{
Samples initial particles from prior
}
