\name{opt_list}
\alias{opt_list}
\docType{data}
\title{
  Example List of Options
}
\description{
  A list containing example options for a hypothetical analysis.
  This includes a path to summary statistics, a population identifier,
  and a sample size.
}
\usage{
data(opt_list)
}
\format{
  A list with 3 components:
  \describe{
    \item{\code{sumstats}}{
      A character string. Path to the summary statistics file.
    }
    \item{\code{population}}{
      A character string. Identifier for the population, e.g., 'EUR'.
    }
    \item{\code{n}}{
      An integer. The sample size.
    }
  }
}
\source{
  Your data source or method of creation here (if applicable).
}
\examples{
data(opt_list)
print(opt_list)
}
