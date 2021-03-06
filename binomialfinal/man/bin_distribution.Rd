% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workout3.R
\name{bin_distribution}
\alias{bin_distribution}
\title{bin_distribution}
\usage{
bin_distribution(trials, prob)
}
\arguments{
\item{trials}{number of fixed trials}

\item{prob}{probability of success on each trial}
}
\value{
data frame with success and probability sections
}
\description{
calculates the distribution of probability over a number of successes
}
\examples{
bin_dstribution(trials = 5, prob = 0.5)
bin_distribution(5, 0.3)
}
