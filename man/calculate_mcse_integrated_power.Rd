% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MCSE.R
\name{calculate_mcse_integrated_power}
\alias{calculate_mcse_integrated_power}
\title{Monte Carlo Standard Error for Integrated Power Metrics}
\usage{
calculate_mcse_integrated_power(
  values,
  weights,
  n_sims,
  is_power_metric = TRUE
)
}
\arguments{
\item{values}{Vector of power or probability values}

\item{weights}{Vector of weights for integration}

\item{n_sims}{Total number of simulations}

\item{is_power_metric}{Logical indicating if this is a power metric (TRUE) or probability metric (FALSE)}
}
\value{
Monte Carlo Standard Error for integrated metric
}
\description{
Calculate Monte Carlo Standard Error for integrated power metrics that combine
results across multiple effect sizes or sample sizes using weighted averages.
}
\keyword{internal}
