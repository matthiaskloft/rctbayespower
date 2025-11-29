#' Create General ANCOVA Model with Flexible Specifications
#'
#' Creates a build_model object for ANCOVA (Analysis of Covariance) with flexible
#' specifications for number of arms, contrasts, allocation ratios, and parameters.
#' This is the general function that underlies all ANCOVA model variants in the package.
#'
#' @param prior_intercept Prior for the intercept parameter. If NULL (default),
#'   uses normal(0, 10). Must be a brmsprior object created with [brms::set_prior()].
#' @param prior_sigma Prior for the residual standard deviation. If NULL (default),
#'   uses normal(0, 10). Must be a brmsprior object created with [brms::set_prior()].
#' @param prior_covariate Prior for the covariate effect. If NULL (default),
#'   uses student_t(3, 0, 1). Must be a brmsprior object created with [brms::set_prior()].
#' @param prior_treatment Prior for the treatment effect. If NULL (default),
#'   uses student_t(3, 0, 1). Must be a brmsprior object created with [brms::set_prior()].
#' @param link_sigma Link function for the residual standard deviation. Default is "identity".
#' @param n_arms Number of arms in the trial (must be >= 2). Required parameter.
#' @param contrasts Contrast method for treatment arms. Either a character string
#'   (e.g., "contr.treatment", "contr.sum") or a contrast matrix. Required parameter.
#' @param p_alloc Numeric vector of allocation probabilities summing to 1.
#'   Length must equal n_arms. Required parameter.
#' @param intercept Intercept value for data generation. Required parameter.
#' @param b_arm_treat Treatment effect coefficients for data generation.
#'   Vector length must equal n_arms - 1. Required parameter.
#' @param b_covariate Covariate effect coefficient for data generation. Required parameter.
#' @param sigma Residual standard deviation for data generation (must be > 0). Required parameter.
#'
#' @details
#' This function creates a complete ANCOVA model with the following structure:
#'
#' \strong{Model Formula:} outcome ~ 1 + covariate + arm
#'
#' \strong{Data Structure:} The generated data includes:
#' \itemize{
#'   \item covariate: Standardized normal covariate
#'   \item arm: Factor with levels "ctrl" and treatment arms ("treat_1", "treat_2", etc.)
#'   \item outcome: Continuous outcome generated from the linear model
#' }
#'
#' \strong{Parameters:} The model includes parameters for intercept, covariate effect,
#' treatment effects, and residual standard deviation (sigma).
#'
#' \strong{Model Compilation:} The function compiles the brms model during creation,
#' which may take some time but enables efficient power analysis later.
#'
#' \strong{Convenience Functions:} For common use cases, consider the wrapper functions that can be called via the 'model_name' argument in [build_design()]:
#' \itemize{
#'   \item [build_model_ancova_cont_2arms()] - 2-arm continuous ANCOVA
#'   \item [build_model_ancova_cont_3arms()] - 3-arm continuous ANCOVA
#' }
#'
#' @return An S7 object of class "rctbp_model" (legacy) ready for use with
#'   [build_design()] and power analysis functions.
#'
#' @export
#' @importFrom brms set_prior brm
#' @importFrom stats gaussian contrasts<- model.matrix
#' @seealso [build_design()], [build_model_ancova_cont_2arms()],
#'   [build_model_ancova_cont_3arms()]
#'
#' @examples
#' \dontrun{
#' # Create 2-arm ANCOVA model
#' model_2arm <- build_model_ancova(
#'   n_arms = 2,
#'   contrasts = "contr.treatment",
#'   p_alloc = c(0.5, 0.5),
#'   intercept = 0,
#'   b_arm_treat = 0.5,
#'   b_covariate = 0.3,
#'   sigma = 1
#' )
#' }
build_model_ancova <- function(prior_intercept = NULL,
                               prior_sigma = NULL,
                               prior_covariate = NULL,
                               prior_treatment = NULL,
                               link_sigma = "identity",
                               n_arms = NULL,
                               contrasts = NULL,
                               p_alloc = NULL,
                               intercept = NULL,
                               b_arm_treat = NULL,
                               b_covariate,
                               sigma = NULL) {
  # Enhanced validation using model properties ---------------------------------

  # Validate that parameter names from simulation function include required parameters
  required_sim_params <- c("n_total",
                           "n_arms",
                           "p_alloc",
                           "intercept",
                           "b_arm_treat",
                           "b_covariate",
                           "sigma")

  # Validation of ANCOVA-specific parameters ----------------------------------

  # Validate p_alloc
  if (!is.null(p_alloc) && !is.null(n_arms)) {
    if (length(p_alloc) != n_arms) {
      cli::cli_abort(c(
        "{.arg p_alloc} must have length equal to {.arg n_arms}",
        "x" = "You supplied {.arg p_alloc} with length {.val {length(p_alloc)}} but {.arg n_arms} = {.val {n_arms}}",
        "i" = "Provide a probability vector with {.val {n_arms}} elements"
      ))
    }
    if (abs(sum(p_alloc) - 1) > 1e-6) {
      cli::cli_abort(c(
        "{.arg p_alloc} must sum to 1",
        "x" = "You supplied {.arg p_alloc} that sums to {.val {sum(p_alloc)}}",
        "i" = "Ensure all probabilities sum to 1.0"
      ))
    }
  }

  # Validate b_arm_treat
  if (!is.null(b_arm_treat) && !is.null(n_arms)) {
    if (length(b_arm_treat) != (n_arms - 1)) {
      cli::cli_abort(c(
        "{.arg b_arm_treat} must have length equal to {.code n_arms - 1}",
        "x" = "You supplied {.arg b_arm_treat} with length {.val {length(b_arm_treat)}} but need {.val {n_arms - 1}} coefficients",
        "i" = "Provide {.val {n_arms - 1}} treatment effect coefficients for {.val {n_arms}} arms"
      ))
    }
  }

  # Validate sigma
  if (!is.null(sigma) && sigma <= 0) {
    cli::cli_abort(c(
      "{.arg sigma} must be positive",
      "x" = "You supplied {.arg sigma} = {.val {sigma}}",
      "i" = "Use a value > 0"
    ))
  }

  # create the data simulation function
  simulate_data_ancova <- local({
    default_n_arms <- n_arms
    default_contrasts <- contrasts
    default_p_alloc <- p_alloc
    default_intercept <- intercept
    default_b_arm_treat <- b_arm_treat
    default_b_covariate <- b_covariate
    default_sigma <- sigma

    function(n_total,
             n_arms = default_n_arms,
             contrasts = default_contrasts,
             p_alloc = default_p_alloc,
             intercept = default_intercept,
             b_arm_treat = default_b_arm_treat,
             b_covariate = default_b_covariate,
             sigma = default_sigma) {
      # validation of inputs -----------------------------------------------------

      # validate n_total, must be numeric and whole number
      if (is.null(n_total) ||
          !is.numeric(n_total) || length(n_total) != 1 ||
          n_total <= 0 || n_total != round(n_total)) {
        cli::cli_abort(c(
          "{.arg n_total} must be a positive integer value",
          "x" = "You supplied {.val {n_total}}",
          "i" = "Use a positive whole number"
        ))
      }
      # validate n_arms, must be integer > 2
      if (is.null(n_arms) ||
          !is.numeric(n_arms) || length(n_arms) != 1 ||
          n_arms < 2 || n_arms != round(n_arms)) {
        cli::cli_abort(c(
          "{.arg n_arms} must be a positive integer >= 2",
          "x" = "You supplied {.val {n_arms}}",
          "i" = "Use an integer value of 2 or more"
        ))
      }
      # validate contrasts, must be a string or matrix for creating contrasts
      if (!is.null(contrasts) &
          !is.character(contrasts) & !is.matrix(contrasts)) {
        cli::cli_abort(c(
          "{.arg contrasts} must be a character string or matrix",
          "x" = "You supplied {.type {contrasts}}",
          "i" = "Use a contrast method name (e.g., {.val contr.treatment}) or a contrast matrix"
        ))
      } else{
        # create contrast matrix
        if (!is.null(contrasts) & is.character(contrasts)) {
          # validate string is one of the valid 'contr.' methods from 'stats'
          valid_contrasts <- c(
            "contr.treatment",
            "contr.sum",
            "contr.poly",
            "contr.helmert",
            "contr.SAS"
          )
          if (!(contrasts %in% valid_contrasts)) {
            cli::cli_abort(c(
              "{.arg contrasts} must be a valid contrast method",
              "x" = "You supplied {.val {contrasts}}",
              "i" = "Use one of: {.val {valid_contrasts}}"
            ))
          }
          # create contrast matrix
          tryCatch({
            contrasts_fn <- get(contrasts)
            contrast_matrix <- contrasts_fn(n_arms)
          }, error = function(e) {
            cli::cli_abort(c(
              "Failed to create contrast matrix from {.arg contrasts}",
              "x" = "Error: {e$message}",
              "i" = "Use a valid contrast method (e.g., {.val contr.treatment})"
            ))
          })
        }
      }
      # validate matrix
      if (!is.null(contrasts) & is.matrix(contrasts)) {
        # validate dimensions of contrast matrix,
        # nrow == n_arms, ncol == n_arms - 1
        if (nrow(contrasts) != n_arms ||
            ncol(contrasts) != n_arms - 1) {
          cli::cli_abort(c(
            "{.arg contrasts} matrix must have dimensions {.code n_arms x (n_arms - 1)}",
            "x" = "You supplied a {.val {nrow(contrasts)}} x {.val {ncol(contrasts)}} matrix but need {.val {n_arms}} x {.val {n_arms - 1}}",
            "i" = "Ensure the contrast matrix has {.val {n_arms}} rows and {.val {n_arms - 1}} columns"
          ))
        }
        tryCatch({
          contrasts_fn <- get(contrasts)
          contrast_matrix <- contrasts
        }, error = function(e) {
          cli::cli_abort(c(
            "Invalid {.arg contrasts} specification",
            "x" = "Error: {e$message}",
            "i" = "Provide a valid contrast method name or matrix"
          ))
        })
      }
      # validate p_alloc, must be of length n_arms
      # must be a numeric vector of probabilities summing to 1
      if (is.null(p_alloc) ||
          !is.numeric(p_alloc) || length(p_alloc) != n_arms ||
          sum(p_alloc) != 1) {
        cli::cli_abort(c(
          "{.arg p_alloc} must be a numeric vector of probabilities summing to 1",
          "x" = "You supplied {.val {p_alloc}} with length {.val {length(p_alloc)}} and sum {.val {sum(p_alloc)}}",
          "i" = "Provide {.val {n_arms}} probabilities that sum to 1.0"
        ))
      }
      # validate intercept, must be numeric
      if (is.null(intercept) || !is.numeric(intercept)) {
        cli::cli_abort(c(
          "{.arg intercept} must be a numeric value",
          "x" = "You supplied {.type {intercept}}",
          "i" = "Provide a numeric intercept value"
        ))
      }
      # validate b_arm_treat, must be numeric
      if (is.null(b_arm_treat) || !is.numeric(b_arm_treat)) {
        cli::cli_abort(c(
          "{.arg b_arm_treat} must be a numeric value",
          "x" = "You supplied {.type {b_arm_treat}}",
          "i" = "Provide numeric treatment effect coefficient(s)"
        ))
      }
      # validate b_covariate, must be numeric
      if (is.null(b_covariate) || !is.numeric(b_covariate)) {
        cli::cli_abort(c(
          "{.arg b_covariate} must be a numeric value",
          "x" = "You supplied {.type {b_covariate}}",
          "i" = "Provide a numeric covariate coefficient"
        ))
      }
      # validate sigma, must be numeric and positive
      if (is.null(sigma) || !is.numeric(sigma) || sigma <= 0) {
        cli::cli_abort(c(
          "{.arg sigma} must be a positive numeric value",
          "x" = "You supplied {.val {sigma}}",
          "i" = "Use a value > 0"
        ))
      }

      # end of validation --------------------------------------------------------


      # simulate data for ANCOVA -------------------------------------------------

      # predictors
      df <- data.frame(
        covariate = stats::rnorm(n_total),
        arm = factor(
          sample(
            x = seq_len(n_arms) - 1,
            size = n_total,
            prob = p_alloc,
            replace = TRUE
          ),
          levels = seq_len(n_arms) - 1,
          labels = c("ctrl", paste0("treat_", seq_len(n_arms - 1)))
        )
      )
      # set contrasts
      contrasts(df$arm) <- contrast_matrix

      # simulate outcomes
      df <- df |>
        dplyr::mutate(
          # outcome
          outcome = stats::rnorm(
            n_total,
            mean = intercept +
              model.matrix( ~ arm, data = df)[, -1, drop = FALSE] %*% b_arm_treat +
              covariate * b_covariate,
            sd = sigma
          )
        )
      return(df)
    }
  })

  # simulate some data to compile the model
  mock_data_ancova <- simulate_data_ancova(
    n_total = 20,
    n_arms = n_arms,
    contrasts = contrasts,
    p_alloc = rep(1, n_arms) / n_arms,
    intercept = 0,
    b_arm_treat = rep(0, n_arms - 1),
    b_covariate = 0,
    sigma = 1
  )

  # priors ---------------------------------------------------------------------
  # use user-specified priors if !is.null(prior_intercept) else use default priors
  # check that the priors are specified with brms::set_prior()

  if (is.null(prior_intercept)) {
    prior_intercept <- brms::set_prior("normal(0, 10)", class = "Intercept")
  } else if (!inherits(prior_intercept, "brmsprior")) {
    cli::cli_abort(c(
      "{.arg prior_intercept} must be a valid brmsprior object",
      "x" = "You supplied {.cls {class(prior_intercept)}}",
      "i" = "Create priors using {.fn brms::set_prior}"
    ))
  }
  if (is.null(prior_sigma)) {
    prior_sigma <- brms::set_prior("normal(0, 10)", class = "sigma", lb = 0)
  } else if (!inherits(prior_sigma, "brmsprior")) {
    cli::cli_abort(c(
      "{.arg prior_sigma} must be a valid brmsprior object",
      "x" = "You supplied {.cls {class(prior_sigma)}}",
      "i" = "Create priors using {.fn brms::set_prior}"
    ))
  }
  if (is.null(prior_covariate)) {
    prior_covariate <- brms::set_prior("student_t(3, 0, 1)", class = "b", coef = "covariate")
  } else if (!inherits(prior_covariate, "brmsprior")) {
    cli::cli_abort(c(
      "{.arg prior_covariate} must be a valid brmsprior object",
      "x" = "You supplied {.cls {class(prior_covariate)}}",
      "i" = "Create priors using {.fn brms::set_prior}"
    ))
  }
  if (is.null(prior_treatment)) {
    for (i in seq_len(n_arms - 1)) {
      prior_treatment <- brms::set_prior("student_t(3, 0, 1)",
                                         class = "b",
                                         coef = paste0("armtreat_", i))
    }
    prior_treatment <- brms::set_prior("student_t(3, 0, 1)", class = "b")
  } else if (!inherits(prior_treatment, "brmsprior")) {
    cli::cli_abort(c(
      "{.arg prior_treatment} must be a valid brmsprior object",
      "x" = "You supplied {.cls {class(prior_treatment)}}",
      "i" = "Create priors using {.fn brms::set_prior}"
    ))
  }

  # combine the priors into a single vector
  priors <- c(prior_covariate,
              prior_treatment,
              prior_intercept,
              prior_sigma)

  # end of priors --------------------------------------------------------------


  # compile the brms model -----------------------------------------------------

  # model for retrieving parameter names
  brms_model_ancova <-
    suppressMessages(suppressWarnings(
      brms::brm(
        formula = outcome ~ 1 + covariate + arm,
        data = mock_data_ancova,
        family = brms::brmsfamily("gaussian",link_sigma = link_sigma),
        prior = priors,
        chains = 1,
        iter = 500,
        refresh = 0,
        silent = 2
      )
    ))

  # build model object ---------------------------------------------------------

  # Create S7 ANCOVA model object
  ancova_model <- rctbp_model(
    sim_fn = simulate_data_ancova,
    inference_model = brms_model_ancova,
    model_name = "ANCOVA",
    n_endpoints = 1L,
    endpoint_types = "continuous",
    n_arms = as.integer(n_arms),
    n_repeated_measures = 0L
  )


  return(ancova_model)
}


# Specific default models

#' Create 2-Arm ANCOVA Model for Continuous Outcomes
#'
#' Creates a 2-arm ANCOVA model with sensible defaults for continuous outcomes.
#' This is a convenience wrapper around [build_model_ancova()].
#'
#' @param ... Additional arguments passed to [build_model_ancova()]. Can override
#'   any of the default parameters.
#'
#' @details
#' Default parameters:
#' \itemize{
#'   \item n_arms = 2
#'   \item contrasts = "contr.treatment"
#'   \item p_alloc = c(0.5, 0.5) (equal allocation)
#'   \item intercept = 0
#'   \item b_arm_treat = NULL (must be specified)
#'   \item b_covariate = NULL (must be specified)
#'   \item sigma = 1
#' }
#'
#' @return An S7 object of class "rctbp_model_ancova" ready for use with
#'   [build_design()] and power analysis functions.
#'
#' @export
#' @seealso [build_model_ancova()], [build_model_ancova_cont_3arms()]
#'
#' @examples
#' \dontrun{
#' # Create 2-arm ANCOVA model (must specify effect sizes)
#' model_2arm <- build_model_ancova_cont_2arms(
#'   b_arm_treat = 0.5,
#'   b_covariate = 0.3
#' )
#' }
build_model_ancova_cont_2arms <- function(...) {
  # collect additional arguments
  dots <- list(...)
  # set default arguments
  default_args <- list(
    prior_intercept = NULL,
    prior_sigma = NULL,
    prior_covariate = NULL,
    prior_treatment = NULL,
    n_arms = 2,
    contrasts = "contr.treatment",
    p_alloc = c(0.5, 0.5),
    intercept = 0,
    b_arm_treat = NULL,
    b_covariate = NULL,
    sigma = 1
  )
  # modify default arguments with user-specified arguments
  final_args <- modifyList(default_args, dots)
  # call the build_model_ancova function with the final arguments
  model <- do.call(build_model_ancova, final_args)

  # add predefined model name
  model@predefined_model <- "ancova_cont_2arms"
  # return the model object
  invisible(model)
}

#' Create 3-Arm ANCOVA Model for Continuous Outcomes
#'
#' Creates a 3-arm ANCOVA model with sensible defaults for continuous outcomes.
#' This is a convenience wrapper around [build_model_ancova()].
#'
#' @param ... Additional arguments passed to [build_model_ancova()]. Can override
#'   any of the default parameters.
#'
#' @details
#' Default parameters:
#' \itemize{
#'   \item n_arms = 3
#'   \item contrasts = "contr.treatment"
#'   \item p_alloc = c(1/3, 1/3, 1/3) (equal allocation)
#'   \item intercept = 0
#'   \item b_arm_treat = NULL (must be specified, length 2)
#'   \item b_covariate = NULL (must be specified)
#'   \item sigma = 1
#' }
#'
#' @return An S7 object of class "rctbp_model_ancova" ready for use with
#'   [build_design()] and power analysis functions.
#'
#' @export
#' @seealso [build_model_ancova()], [build_model_ancova_cont_2arms()]
#'
#' @examples
#' \dontrun{
#' # Create 3-arm ANCOVA model (must specify effect sizes)
#' model_3arm <- build_model_ancova_cont_3arms(
#'   b_arm_treat = c(0.5, 0.7),
#'   b_covariate = 0.3
#' )
#' }
build_model_ancova_cont_3arms <- function(...) {
  # collect additional arguments
  dots <- list(...)
  # set default arguments
  default_args <- list(
    prior_intercept = NULL,
    prior_sigma = NULL,
    prior_covariate = NULL,
    prior_treatment = NULL,
    n_arms = 3,
    contrasts = "contr.treatment",
    p_alloc = rep(1, 3) / 3,
    intercept = 0,
    b_arm_treat = NULL,
    b_covariate = NULL,
    sigma = 1
  )
  # modify default arguments with user-specified arguments
  final_args <- modifyList(default_args, dots)
  # call the build_model_ancova function with the final arguments
  model <- do.call(build_model_ancova, final_args)
  # add predefined model name

  model@predefined_model <- "ancova_cont_3arms"

  # return the model object
  invisible(model)
}


# =============================================================================
# BATCH SIMULATION FUNCTIONS (for BayesFlow Backend)
# =============================================================================

#' Simulate ANCOVA Data - Batched Format (2-arm)
#'
#' Generates multiple simulations at once for NPE/BayesFlow efficiency.
#' Returns matrices instead of data.frames for direct use with neural networks.
#'
#' @param n_sims Number of simulations to generate (batch size)
#' @param n_total Sample size per simulation
#' @param p_alloc Allocation probability for treatment (default 0.5)
#' @param intercept Intercept value (default 0)
#' @param b_arm_treat Treatment effect coefficient
#' @param b_covariate Covariate effect coefficient (default 0)
#' @param sigma Residual standard deviation (default 1)
#'
#' @return List with batch-formatted arrays:
#'   \itemize{
#'     \item outcome: matrix (n_sims x n_total)
#'     \item covariate: matrix (n_sims x n_total)
#'     \item group: matrix (n_sims x n_total), binary treatment indicator
#'     \item N: integer, sample size per simulation
#'     \item p_alloc: numeric, allocation probability
#'   }
#'
#' @details
#' This function is optimized for batch generation using vectorized operations.
#' It generates all simulations simultaneously, making it suitable for
#' BayesFlow/NPE workflows where batching improves efficiency.
#'
#' The group matrix contains binary (0/1) values for 2-arm trials,
#' where 0 = control and 1 = treatment.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Generate 64 simulations with 100 subjects each
#' batch_data <- simulate_data_ancova_cont_2arms_batch(
#'   n_sims = 64,
#'   n_total = 100,
#'   b_arm_treat = 0.5,
#'   b_covariate = 0.3
#' )
#' dim(batch_data$outcome)  # [64, 100]
#' }
simulate_data_ancova_cont_2arms_batch <- function(n_sims, n_total, p_alloc = 0.5,
                                                   intercept = 0, b_arm_treat = 0,
                                                   b_covariate = 0, sigma = 1) {
  # Validate inputs
  n_sims <- as.integer(n_sims)
  n_total <- as.integer(n_total)

  if (n_sims <= 0) {
    cli::cli_abort(c(
      "{.arg n_sims} must be a positive integer",
      "x" = "You supplied {.val {n_sims}}"
    ))
  }
  if (n_total <= 0) {
    cli::cli_abort(c(
      "{.arg n_total} must be a positive integer",
      "x" = "You supplied {.val {n_total}}"
    ))
  }
  if (sigma <= 0) {
    cli::cli_abort(c(
      "{.arg sigma} must be positive",
      "x" = "You supplied {.val {sigma}}"
    ))
  }

  total_elements <- n_sims * n_total

  # Generate all random values at once (vectorized)
  covariate_mat <- matrix(
    stats::rnorm(total_elements),
    nrow = n_sims,
    ncol = n_total
  )

  group_mat <- matrix(
    stats::rbinom(total_elements, 1, p_alloc),
    nrow = n_sims,
    ncol = n_total
  )

  # Calculate outcomes using vectorized operations
  # outcome = intercept + covariate * b_covariate + group * b_arm_treat + error
  outcome_mat <- intercept +
    covariate_mat * b_covariate +
    group_mat * b_arm_treat +
    matrix(stats::rnorm(total_elements, 0, sigma), nrow = n_sims, ncol = n_total)

  list(
    outcome = outcome_mat,
    covariate = covariate_mat,
    group = group_mat,
    N = n_total,
    p_alloc = p_alloc
  )
}


#' Simulate ANCOVA Data - Batched Format (3-arm)
#'
#' Generates multiple 3-arm simulations at once for NPE/BayesFlow efficiency.
#' Returns matrices instead of data.frames for direct use with neural networks.
#'
#' @param n_sims Number of simulations to generate (batch size)
#' @param n_total Sample size per simulation
#' @param p_alloc Allocation probabilities (default c(1/3, 1/3, 1/3))
#' @param intercept Intercept value (default 0)
#' @param b_arm_treat Treatment effect coefficients (length 2 vector)
#' @param b_covariate Covariate effect coefficient (default 0)
#' @param sigma Residual standard deviation (default 1)
#'
#' @return List with batch-formatted arrays:
#'   \itemize{
#'     \item outcome: matrix (n_sims x n_total)
#'     \item covariate: matrix (n_sims x n_total)
#'     \item group: matrix (n_sims x n_total), values 0/1/2 for arms
#'     \item N: integer, sample size per simulation
#'     \item p_alloc: numeric vector, allocation probabilities
#'   }
#'
#' @details
#' Similar to [simulate_data_ancova_cont_2arms_batch()] but for 3-arm trials.
#' The group matrix contains values 0, 1, 2 corresponding to control,
#' treatment 1, and treatment 2 respectively.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Generate 64 simulations with 150 subjects each
#' batch_data <- simulate_data_ancova_cont_3arms_batch(
#'   n_sims = 64,
#'   n_total = 150,
#'   b_arm_treat = c(0.3, 0.5),
#'   b_covariate = 0.3
#' )
#' dim(batch_data$outcome)  # [64, 150]
#' }
simulate_data_ancova_cont_3arms_batch <- function(n_sims, n_total,
                                                   p_alloc = c(1/3, 1/3, 1/3),
                                                   intercept = 0,
                                                   b_arm_treat = c(0, 0),
                                                   b_covariate = 0, sigma = 1) {
  # Validate inputs
  n_sims <- as.integer(n_sims)
  n_total <- as.integer(n_total)

  if (n_sims <= 0) {
    cli::cli_abort(c(
      "{.arg n_sims} must be a positive integer",
      "x" = "You supplied {.val {n_sims}}"
    ))
  }
  if (n_total <= 0) {
    cli::cli_abort(c(
      "{.arg n_total} must be a positive integer",
      "x" = "You supplied {.val {n_total}}"
    ))
  }
  if (length(p_alloc) != 3 || abs(sum(p_alloc) - 1) > 1e-6) {
    cli::cli_abort(c(
      "{.arg p_alloc} must be length 3 and sum to 1",
      "x" = "You supplied {.val {p_alloc}}"
    ))
  }
  if (length(b_arm_treat) != 2) {
    cli::cli_abort(c(
      "{.arg b_arm_treat} must be length 2 for 3-arm trials",
      "x" = "You supplied {.val {b_arm_treat}} with length {.val {length(b_arm_treat)}}"
    ))
  }
  if (sigma <= 0) {
    cli::cli_abort(c(
      "{.arg sigma} must be positive",
      "x" = "You supplied {.val {sigma}}"
    ))
  }

  total_elements <- n_sims * n_total

  # Generate covariates
  covariate_mat <- matrix(
    stats::rnorm(total_elements),
    nrow = n_sims,
    ncol = n_total
  )

  # Generate group assignments (0, 1, 2) using multinomial sampling
  group_vec <- sample(
    x = c(0L, 1L, 2L),
    size = total_elements,
    prob = p_alloc,
    replace = TRUE
  )
  group_mat <- matrix(group_vec, nrow = n_sims, ncol = n_total)

  # Calculate treatment effects
  # For treatment coding: group 0 = control (ref), group 1 = treat_1, group 2 = treat_2
  treat_effect <- matrix(0, nrow = n_sims, ncol = n_total)
  treat_effect[group_mat == 1] <- b_arm_treat[1]
  treat_effect[group_mat == 2] <- b_arm_treat[2]

  # Calculate outcomes
  outcome_mat <- intercept +
    covariate_mat * b_covariate +
    treat_effect +
    matrix(stats::rnorm(total_elements, 0, sigma), nrow = n_sims, ncol = n_total)

  list(
    outcome = outcome_mat,
    covariate = covariate_mat,
    group = group_mat,
    N = n_total,
    p_alloc = p_alloc
  )
}
