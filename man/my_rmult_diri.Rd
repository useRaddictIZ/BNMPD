% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00_helper_generate_data.R
\name{my_rmult_diri}
\alias{my_rmult_diri}
\title{Generates random samples from a Dirichlet-multinomial distribution}
\usage{
my_rmult_diri(alpha, num_counts)
}
\arguments{
\item{alpha}{alpha parameters of dirichlet distribution given as a matrix}

\item{num_counts}{number of counts for the dirichlet shares/fractions}
}
\value{
a nxD dimensional matrix of Dirichlet-multinomial draws
}
\description{
Generates random samples from Dirichlet-dirichlet distribution; the dimension
of the Dirichlet-multinomial distribution (i.e. the number of shares of
fractions) is taken as the number of columns in the \code{alpha} matrix; the
number of samples are taken as the rows of the \code{alpha} matrix; hence,
each row of \code{alpha} corresponds to \code{D} \code{alpha} parameters for
which a \code{D}-dimensional random draw is generated, and all these \code{n}
draws are returned in matrix form.
}
