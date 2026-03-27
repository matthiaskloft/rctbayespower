#' @keywords internal
#'
#' @description
#' Conduct Bayesian power analyses for randomized controlled trials (RCTs)
#' using simulation-based workflows. Supports continuous, binary, proportional,
#' and survival outcomes with fixed or group-sequential designs. Uses
#' ROPE-based decision criteria and dual backends: brms/Stan (MCMC) and
#' BayesFlow (neural posterior estimation).
#'
#' @section Typical workflow:
#' ```
#' # 1. Pick a predefined model
#' show_predefined_models()
#' design <- build_design(predefined_model = "ancova_cont_2arms",
#'                        target_params = "b_arm2")
#'
#' # 2. Define conditions to evaluate
#' conditions <- build_conditions(
#'   design,
#'   crossed = list(n_total = c(50, 100, 200), b_arm_treat = c(0.3, 0.5)),
#'   constant = list(thr_dec_eff = 0.975)
#' )
#'
#' # 3. Run the power analysis
#' result <- power_analysis(conditions, n_sims = 1000) |> run()
#'
#' # 4. Explore results
#' summary(result)
#' plot(result)
#' as.data.frame(result)
#' ```
#'
#' @section Key functions:
#' **Discovery:**
#' - [show_predefined_models()] -- list available models
#' - [show_target_params()] -- parameters available for inference
#' - [show_condition_args()] -- arguments for [build_conditions()]
#' - [show_boundaries()] -- boundary functions for group-sequential designs
#'
#' **Design & analysis:**
#' - [build_design()] -- create a trial design
#' - [build_conditions()] -- define parameter grid
#' - [power_analysis()] -- configure power analysis
#' - [run()] -- execute the analysis
#'
#' **Results:**
#' - [report()] -- formatted results report
#' - [plot.rctbp_power_analysis()] -- visualize results
#' - [pareto_optimize()] -- multi-objective optimization
#'
#' **Group-sequential designs:**
#' - [interim_success_futility()], [interim_futility_only()] -- stopping rules
#' - [boundary_obf()], [boundary_pocock()], [boundary_hsd()],
#'   [boundary_wang_tsiatis()] -- spending boundaries
#'
#' @seealso
#' - Package website: <https://matthiaskloft.github.io/rctbayespower/>
#' - GitHub: <https://github.com/matthiaskloft/rctbayespower>
"_PACKAGE"

# Enable data.table awareness for `:=` and other operators
.datatable.aware = TRUE

## Suppress R CMD check warnings for undefined global variables
if (getRversion() >= "2.15.1") {
  # Global variables for R CMD check
  utils::globalVariables(
    c(
      # New column names
      "par_name",
      "thr_fx_eff",
      "thr_fx_fut",
      "thr_dec_eff",
      "thr_dec_fut",
      "id_cond",
      "id_iter",
      "id_look",
      "pr_eff",
      "pr_fut",
      "dec_eff",
      "dec_fut",
      "post_med",
      "post_mad",
      "post_mn",
      "post_sd",
      "pwr_eff",
      "pwr_fut",
      "error_msg",
      # Variables from summarize_sims function
      "pr_eff_mean",
      "pr_eff_mcse",
      "pr_fut_mean",
      "pr_fut_mcse",
      "pwr_eff_mean",
      "pwr_eff_mcse",
      "pwr_fut_mean",
      "pwr_fut_mcse",
      "post_med_mean",
      "post_med_mcse",
      "post_mad_mean",
      "post_mad_mcse",
      "post_mn_mean",
      "post_mn_mcse",
      "post_sd_mean",
      "post_sd_mcse",
      "conv_rate_mean",
      "conv_rate_mcse",
      "rhat_mean",
      "rhat_mcse",
      "ess_bulk_mean",
      "ess_bulk_mcse",
      "ess_tail_mean",
      "ess_tail_mcse",
      # Standard diagnostics (unchanged)
      "rhat",
      "ess_bulk",
      "ess_tail",
      "converged",
      # Other variables
      "arm",
      "baseline",
      "convergence_rate",
      "measures",
      "res",
      "i",
      "required_parameters",
      "covariate",
      "x",
      "n",
      # Variables from summarize_sims_with_interim function
      "n_analyzed",
      "stop_reason",
      "stop_n",
      "n_planned",
      "effective_n",
      "stopped_early",
      "n_mn",
      # Per-look stopping stats
      "n_stp_look",
      "n_eff_look",
      "n_fut_look",
      "prop_stp_look",
      "prop_eff_look",
      "prop_fut_look",
      "cumul_stp",
      # Overall stopping proportions
      "prop_stp_eff",
      "prop_stp_fut",
      # data.table special symbols and operators
      ":=",
      ".N",
      ".I",
      ".SD",
      # Variables from data.table operations in summarization
      "n_total_sims",
      "stopped",
      "triggers_stop",
      "already_stopped",
      "is_stop_point",
      "thr_dec_eff_new",
      "thr_dec_fut_new",
      # Accrual plot variables
      "calendar_time",
      "count",
      "status",
      "condition",
      "xintercept"
    )
  )
}
