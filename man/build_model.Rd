% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rctbayespower_model.R
\name{build_model}
\alias{build_model}
\title{Create an build_model Object}
\usage{
build_model(
  data_simulation_fn,
  brms_model,
  n_endpoints = NULL,
  endpoint_types = NULL,
  n_treatment_arms = NULL,
  n_repeated_measures = NULL,
  model_name = NULL,
  pre_defined_model = NULL
)
}
\arguments{
\item{data_simulation_fn}{A function that simulates data for the RCT. Must take
parameters n_total, p_alloc, and further parameters needed for
data simulation.}

\item{brms_model}{A fitted brmsfit object that serves as the template model.
This should be compiled without posterior draws (chains = 0) for efficiency.}

\item{n_endpoints}{Number of endpoints in the study (must be positive integer)}

\item{endpoint_types}{Character vector specifying the type of each endpoint.
Valid types are "continuous", "binary", "count". Length must match n_endpoints.}

\item{n_treatment_arms}{Number of treatment arms in the study (must be positive integer)}

\item{n_repeated_measures}{Number of repeated measures per participant.
Use NULL or 0 for single time point studies.}

\item{model_name}{Optional character string providing a descriptive name for the model}

\item{pre_defined_model}{Optional character string specifying a predefined model
to use instead of creating a custom model. Currently supported values:
\itemize{
\item "ancova_cont" - ANCOVA model for continuous outcomes with baseline covariate
}
When specified, other parameters are ignored and the predefined model is returned.}
}
\value{
An object of class "build_model" containing:
\describe{
\item{data_simulation_fn}{The provided data simulation function}
\item{brms_model}{The compiled brms model template}
}
The object has the following attributes:
\describe{
\item{model_name}{Descriptive name for the model}
\item{n_endpoints}{Number of study endpoints}
\item{endpoint_types}{Types of endpoints}
\item{n_treatment_arms}{Number of treatment arms}
\item{n_repeated_measures}{Number of repeated measures}
\item{parameter_names_sim_fn}{Parameter names extracted from simulation function}
\item{parameter_names_brms}{Parameter names from brms model}
}
}
\description{
Constructs an build_model object that encapsulates a data simulation
function, a compiled brms model, and associated metadata for power analysis.
This object serves as the foundation for Bayesian power analysis in RCTs.
}
\details{
The build_model class encapsulates all components needed for power
analysis simulation:

\strong{Predefined Models:} For convenience, users can specify pre_defined_model
to use ready-made model configurations. This is the recommended approach for
standard analyses. When using predefined models, other parameters are ignored.

\strong{Custom Models:} For advanced users, custom models can be created by
providing all required parameters:

\strong{Data Simulation Function:} Must accept n_total (total sample size),
p_alloc (vector of allocation probabilities), and true_parameter_values
(named list of parameter values). The function should return a data.frame with
simulated baseline data ready for outcome generation.

\strong{BRMS Model:} A compiled brms model that will be used as a template.
The model should be fitted with minimal chains (e.g., chains = 0) to serve
as a compilation template only.

\strong{Validation:} The function validates that the data simulation function
has the required parameter structure and that the brms model is properly fitted.
}
\examples{
\donttest{
# Method 1: Use predefined model (recommended)
ancova_model <- build_model(pre_defined_model = "ancova_cont")

# Method 2: Create custom model
# Define a simple data simulation function
simulate_ancova_data <- function(n_total, p_alloc,
                                 true_parameter_values = list(intercept, b_grouptreat, b_baseline, sigma)) {
  data.frame(
    baseline = rnorm(n_total),
    group = factor(
      sample(c(0, 1), size = n_total, prob = p_alloc, replace = TRUE),
      levels = c(0, 1), labels = c("ctrl", "treat")
    )
  )
}


# Create mock data and fit template model
mock_data <- simulate_ancova_data(
  100, c(0.5, 0.5),
  list(
    intercept = 0, b_grouptreat = 0.5,
    b_baseline = 0.2, sigma = 1
  )
)
mock_data$outcome <- rnorm(100) # Add outcome for model fitting

template_model <- brms::brm(
  outcome ~ baseline + group,
  data = mock_data,
  family = gaussian(),
  chains = 0, # Compile only
  silent = 2
)

# Create build_model object
my_model <- build_model(
  data_simulation_fn = simulate_ancova_data,
  brms_model = template_model,
  n_endpoints = 1,
  endpoint_types = "continuous",
  n_treatment_arms = 2,
  n_repeated_measures = 0,
  model_name = "ANCOVA Model"
)
}
}
\seealso{
\code{\link[=build_design]{build_design()}}, \code{\link[=build_model_ancova_cont]{build_model_ancova_cont()}}
}
