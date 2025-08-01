% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/required_fn_args.R
\name{required_fn_args}
\alias{required_fn_args}
\title{Identify Required Parameters for Design or Model Objects}
\usage{
required_fn_args(object, print = TRUE)
}
\arguments{
\item{object}{Either an rctbp_design or rctbayespower_model object}

\item{print}{Logical. If TRUE (default), prints the required parameters to console}
}
\value{
For design objects: a list with simulation, interim, and all parameters.
For model objects: a character vector of required parameters.
Both returned invisibly.
}
\description{
Generic wrapper function that identifies required parameters for either
rctbp_design or rctbayespower_model objects by dispatching to the
appropriate specific function.
}
\examples{
\dontrun{
required_fn_args(my_object)
}

}
