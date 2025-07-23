# S7 Subclass for ANCOVA Models
#' @importFrom S7 new_class
rctbp_model_ancova <- S7::new_class("rctbp_model_ancova",
                                            parent = rctbp_model,
                                            properties = list(
                                              # ANCOVA-specific properties
                                              contrasts = S7::class_character | S7::class_any| NULL,  # Can be string or matrix
                                              p_alloc = S7::class_numeric| NULL,
                                              intercept = S7::class_numeric | NULL,
                                              b_arm_treat = S7::class_numeric | NULL,
                                              b_covariate = S7::class_numeric | NULL,
                                              sigma = S7::class_numeric | NULL
                                            ),
                                            validator = function(self) {
                                              # Validate ANCOVA-specific properties
                                              if (!is.null(self@p_alloc) && length(self@p_alloc) != self@n_arms) {
                                                return("'p_alloc' must have length equal to 'n_arms'.")
                                              }
                                              if (!is.null(self@p_alloc) && abs(sum(self@p_alloc) - 1) > 1e-6) {
                                                return("'p_alloc' must sum to 1.")
                                              }
                                              if (!is.null(self@b_arm_treat) && length(self@b_arm_treat) != (self@n_arms - 1)) {
                                                return("'b_arm_treat' must have length equal to 'n_arms - 1'.")
                                              }
                                              if (!is.null(self@sigma) && self@sigma <= 0) {
                                                return("'sigma' must be positive.")
                                              }
                                              # Return NULL if all validations pass
                                              NULL
                                            }
)


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
#' \strong{Convenience Functions:} For common use cases, consider the wrapper functions that can be called via the 'predefined_model' argument in [build_model()]:
#' \itemize{
#'   \item [build_model_ancova_cont_2arms()] - 2-arm continuous ANCOVA
#'   \item [build_model_ancova_cont_3arms()] - 3-arm continuous ANCOVA
#' }
#'
#' @return An S7 object of class "rctbp_model_ancova" ready for use with
#'   [build_design()] and power analysis functions.
#'
#' @export
#' @importFrom brms set_prior brm
#' @importFrom stats gaussian contrasts<- model.matrix
#' @seealso [build_model()], [build_design()], [build_model_ancova_cont_2arms()],
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
                               n_arms = NULL,
                               contrasts = NULL,
                               p_alloc = NULL,
                               intercept = NULL,
                               b_arm_treat = NULL,
                               b_covariate,
                               sigma = NULL) {
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
        stop("'n_total' must be a positive integer value.")
      }
      # validate n_arms, must be integer > 2
      if (is.null(n_arms) ||
          !is.numeric(n_arms) || length(n_arms) != 1 ||
          n_arms < 2 || n_arms != round(n_arms)) {
        stop("'n_arms' must be a positive integer greater than or equal to 2.")
      }
      # validate contrasts, must be a string or matrix for creating contrasts
      if (!is.null(contrasts) &
          !is.character(contrasts) & !is.matrix(contrasts)) {
        stop(
          "'contrasts' must be a character string indicating a contrast method (e.g., \"contr.treatment\") or a matrix."
        )
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
            stop(
              paste0(
                "'contrasts' must be one of the following valid contrast methods: ",
                paste(valid_contrasts, collapse = ", ")
              )
            )
          }
          # create contrast matrix
          tryCatch({
            contrasts_fn <- get(contrasts)
            contrast_matrix <- contrasts_fn(n_arms)
          }, error = function(e) {
            stop("'contrasts' must be a valid contrast method (e.g., \"contr.treatment\").")
          })
        }
      }
      # validate matrix
      if (!is.null(contrasts) & is.matrix(contrasts)) {
        # validate dimensions of contrast matrix,
        # nrow == n_arms, ncol == n_arms - 1
        if (nrow(contrasts) != n_arms ||
            ncol(contrasts) != n_arms - 1) {
          stop("'contrasts' matrix must have dimensions n_arms x (n_arms - 1).")
        }
        tryCatch({
          contrasts_fn <- get(contrasts)
          contrast_matrix <- contrasts
        }, error = function(e) {
          stop(
            "'contrasts' must be a valid contrast method (e.g., \"contr.treatment\") or matrix."
          )
        })
      }
      # validate p_alloc, must be of length n_arms
      # must be a numeric vector of probabilities summing to 1
      if (is.null(p_alloc) ||
          !is.numeric(p_alloc) || length(p_alloc) != n_arms ||
          sum(p_alloc) != 1) {
        stop("'p_alloc' must be a numeric vector of probabilities summing to 1.")
      }
      # validate intercept, must be numeric
      if (is.null(intercept) || !is.numeric(intercept)) {
        stop("'intercept' must be a numeric value.")
      }
      # validate b_arm_treat, must be numeric
      if (is.null(b_arm_treat) || !is.numeric(b_arm_treat)) {
        stop("'b_arm_treat' must be a numeric value.")
      }
      # validate b_covariate, must be numeric
      if (is.null(b_covariate) || !is.numeric(b_covariate)) {
        stop("'b_covariate' must be a numeric value.")
      }
      # validate sigma, must be numeric and positive
      if (is.null(sigma) || !is.numeric(sigma) || sigma <= 0) {
        stop("'sigma' must be a positive numeric value.")
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
    stop("'prior_intercept' must be a valid brmsprior object.")
  }
  if (is.null(prior_sigma)) {
    prior_sigma <- brms::set_prior("normal(0, 10)", class = "sigma")
  } else if (!inherits(prior_sigma, "brmsprior")) {
    stop("'prior_sigma' must be a valid brmsprior object.")
  }
  if (is.null(prior_covariate)) {
    prior_covariate <- brms::set_prior("student_t(3, 0, 1)", class = "b", coef = "covariate")
  } else if (!inherits(prior_covariate, "brmsprior")) {
    stop("'prior_covariate' must be a valid brmsprior object.")
  }
  if (is.null(prior_treatment)) {
    for (i in seq_len(n_arms - 1)) {
      prior_treatment <- brms::set_prior("student_t(3, 0, 1)",
                                         class = "b",
                                         coef = paste0("armtreat_", i))
    }
    prior_treatment <- brms::set_prior("student_t(3, 0, 1)", class = "b")
  } else if (!inherits(prior_treatment, "brmsprior")) {
    stop("'prior_treatment' must be a valid brmsprior object.")
  }

  # combine the priors into a single vector
  priors <- c(prior_covariate,
              prior_treatment,
              prior_intercept,
              prior_sigma)

  # end of priors --------------------------------------------------------------


  # compile the brms model -----------------------------------------------------

  # fit the brms model
  cat("Compiling the brms model ...\n")

  # model for retrieving parameter names
  brms_model_ancova <-
    suppressMessages(suppressWarnings(
      brms::brm(
        formula = outcome ~ 1 + covariate + arm,
        data = mock_data_ancova,
        family = gaussian(),
        prior = priors,
        chains = 1,
        iter = 500,
        refresh = 0,
        silent = 2
      )
    ))
  cat("Model compilation done!\n")



  # build model object ---------------------------------------------------------

  # Create S7 ANCOVA model object
  ancova_model <- rctbp_model_ancova(
    data_simulation_fn = simulate_data_ancova,
    brms_model = brms_model_ancova,
    model_name = "ANCOVA",
    n_endpoints = 1L,
    endpoint_types = "continuous",
    n_arms = as.integer(n_arms),
    n_repeated_measures = 0L,
    parameter_names_sim_fn = names(formals(simulate_data_ancova)),
    parameter_names_brms = stringr::str_subset(brms::variables(brms_model_ancova), pattern = "^b_"),
    # ANCOVA-specific properties
    contrasts = contrasts,
    p_alloc = p_alloc,
    intercept = intercept,
    b_arm_treat = b_arm_treat,
    b_covariate = b_covariate,
    sigma = sigma
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
  # return the model object
  invisible(model)
}
