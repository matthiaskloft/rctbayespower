# =============================================================================
# SURVIVAL MODEL: 2-ARM EXPONENTIAL
# =============================================================================
# Data simulation function for 2-arm survival (time-to-event) analysis.
# Generates enrollment times, event times (exponential), administrative
# censoring from staggered entry, and dropout censoring.
#
# Compatible with brms: brm(time | cens(censored) ~ arm, family = weibull())

#' Create 2-Arm Survival Model
#'
#' Creates a model object for 2-arm survival (time-to-event) power analysis.
#' Generates data with exponential event times, staggered enrollment, and
#' administrative censoring.
#'
#' @param prior_intercept Prior for the intercept (log baseline hazard).
#'   If NULL, uses `normal(0, 5)`.
#' @param prior_treatment Prior for the treatment effect (log hazard ratio).
#'   If NULL, uses `normal(0, 2)`.
#' @param p_alloc Allocation probability vector (length 2, sums to 1).
#' @param baseline_hazard Baseline hazard rate for the control arm.
#' @param hazard_ratio Hazard ratio (treatment / control). Values < 1 indicate
#'   treatment benefit.
#' @param accrual_rate Patients enrolled per time unit.
#' @param followup_time Minimum follow-up time after last patient enrolled.
#'
#' @details
#' \strong{Data Generation:}
#' \enumerate{
#'   \item Enrollment times from [generate_enrollment_times()]
#'   \item Event times: Exponential with rate = `baseline_hazard` (control)
#'     or `baseline_hazard * hazard_ratio` (treatment)
#'   \item Administrative censoring: `max_calendar_time - enrollment_time`,
#'     where `max_calendar_time = max(enrollment_time) + followup_time`
#'   \item Observed time: `min(event_time, admin_censor_time)`
#'   \item Censoring indicator: 0 if event observed, 1 if right-censored (brms convention)
#' }
#'
#' \strong{Output columns:}
#' \itemize{
#'   \item `time`: Observed follow-up time
#'   \item `censored`: 0 = event observed, 1 = right-censored (brms convention)
#'   \item `arm`: Factor with levels "ctrl", "treat_1"
#'   \item `enrollment_time`: Calendar time of enrollment
#' }
#'
#' \strong{brms formula:} `time | cens(censored) ~ arm`
#'
#' @return An S7 object of class "rctbp_design" for use with [build_conditions()].
#'
#' @export
#' @seealso [build_model_ancova_cont()], [build_design()]
#'
#' @examples
#' \dontrun{
#' model <- build_model_survival_2arms(
#'   p_alloc = c(0.5, 0.5),
#'   baseline_hazard = 0.1,
#'   hazard_ratio = 0.7,
#'   accrual_rate = 10,
#'   followup_time = 12
#' )
#' }
build_model_survival_2arms <- function(prior_intercept = NULL,
                                        prior_treatment = NULL,
                                        p_alloc = NULL,
                                        baseline_hazard = NULL,
                                        hazard_ratio = NULL,
                                        accrual_rate = NULL,
                                        followup_time = NULL) {

  # Default priors
  if (is.null(prior_intercept)) {
    prior_intercept <- brms::set_prior("normal(0, 5)", class = "Intercept")
  }
  if (is.null(prior_treatment)) {
    prior_treatment <- brms::set_prior("normal(0, 2)", class = "b")
  }

  priors <- prior_intercept + prior_treatment

  # Create the data simulation function
  simulate_data_survival <- local({
    default_p_alloc <- p_alloc
    default_baseline_hazard <- baseline_hazard
    default_hazard_ratio <- hazard_ratio
    default_accrual_rate <- accrual_rate
    default_followup_time <- followup_time

    function(n_total,
             p_alloc = default_p_alloc,
             baseline_hazard = default_baseline_hazard,
             hazard_ratio = default_hazard_ratio,
             accrual_rate = default_accrual_rate,
             followup_time = default_followup_time) {

      if (is.null(p_alloc)) p_alloc <- c(0.5, 0.5)
      if (is.null(baseline_hazard) || baseline_hazard <= 0) {
        cli::cli_abort("{.arg baseline_hazard} must be a positive number")
      }
      if (is.null(hazard_ratio) || hazard_ratio <= 0) {
        cli::cli_abort("{.arg hazard_ratio} must be a positive number")
      }
      if (is.null(accrual_rate) || accrual_rate <= 0) {
        cli::cli_abort("{.arg accrual_rate} must be a positive number")
      }
      if (is.null(followup_time) || followup_time < 0) {
        cli::cli_abort("{.arg followup_time} must be non-negative")
      }

      # Arm assignment
      arm_idx <- sample(
        0:1, n_total, replace = TRUE, prob = p_alloc
      )
      arm <- factor(arm_idx, levels = 0:1, labels = c("ctrl", "treat_1"))

      # Enrollment times (staggered entry)
      enrollment_time <- generate_enrollment_times(
        n_total, accrual_rate, accrual_pattern = "uniform"
      )

      # Event times: exponential with arm-specific hazard
      hazard <- ifelse(arm_idx == 0, baseline_hazard,
                        baseline_hazard * hazard_ratio)
      event_time <- stats::rexp(n_total, rate = hazard)

      # Administrative censoring from staggered enrollment
      max_calendar_time <- max(enrollment_time) + followup_time
      admin_censor_time <- max_calendar_time - enrollment_time

      # Observed time and censoring indicator
      time <- pmin(event_time, admin_censor_time)
      # brms convention: censored = 0 means event, censored = 1 means right-censored
      censored <- as.integer(event_time > admin_censor_time)

      data.frame(
        time = time,
        censored = censored,
        arm = arm,
        enrollment_time = enrollment_time
      )
    }
  })

  # Compile brms model with minimal data
  sample_data <- data.frame(
    time = c(1, 2, 3, 4),
    censored = c(0, 0, 1, 0),
    arm = factor(c("ctrl", "ctrl", "treat_1", "treat_1"),
                 levels = c("ctrl", "treat_1"))
  )

  formula <- brms::bf(time | cens(censored) ~ arm)

  compiled_model <- brms::brm(
    formula = formula,
    data = sample_data,
    family = brms::weibull(),
    prior = priors,
    chains = 0
  )

  build_design(
    inference_model = compiled_model,
    sim_fn = simulate_data_survival,
    target_params = "b_armtreat_1"
  )
}
