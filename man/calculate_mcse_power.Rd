% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MCSE.R
\name{calculate_mcse_power}
\alias{calculate_mcse_power}
\title{Monte Carlo Standard Error (MCSE) Calculation}
\usage{
calculate_mcse_power(successes, n_sims)
}
\arguments{
\item{successes}{Vector of success indicators (TRUE/FALSE or 1/0)}

\item{n_sims}{Total number of simulations}
}
\value{
Monte Carlo Standard Error
}
\description{
Calculate Monte Carlo Standard Error for power metrics based on simulation results.
MCSE provides an estimate of the uncertainty in power estimates due to Monte Carlo sampling.
}
\keyword{internal}
