% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_power_analysis.R
\name{run}
\alias{run}
\title{Run Analysis Objects}
\usage{
run(object, ...)
}
\arguments{
\item{object}{An S7 object to run (e.g., rctbp_power_analysis)}

\item{...}{Additional arguments passed to specific methods}
}
\value{
The result depends on the specific method called. For power analysis
objects, returns the modified object with results stored in the
\code{summarized_results} and \code{raw_results} properties.
}
\description{
Generic function for executing analysis objects. This function provides a
unified interface for running different types of analysis configurations.
}
\details{
This generic function dispatches to appropriate methods based on the class
of the input object. Currently supported:
\itemize{
\item \code{rctbp_power_analysis}: Executes Bayesian power analysis
}
}
\examples{
\dontrun{
# Create and run power analysis
power_config <- rctbp_power_analysis(conditions = conditions, n_sims = 100)
power_config <- run(power_config)
}
}
