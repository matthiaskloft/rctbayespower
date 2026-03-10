# Extracted from test-integration.R:157

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "rctbayespower", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran()
skip_if_not_installed("brms")
skip_if_not_installed("posterior")
minimal_brms_args <- list(chains = 1, iter = 150, warmup = 50)
design_fixed <- tryCatch(
  suppressMessages(suppressWarnings(
    build_design(
      predefined_model = "ancova_cont_2arms",
      target_params = "b_arm2"
    )
  )),
  error = function(e) NULL
)
if (is.null(design_fixed)) {
  skip("Stan model compilation failed (missing BH or compiler)")
}
expected_summary_cols <- c(
  "id_cond", "par_name",
  "pwr_eff", "se_pwr_eff", "pwr_fut", "se_pwr_fut",
  "pr_eff", "se_pr_eff", "pr_fut", "se_pr_fut",
  "post_med", "post_mn", "post_sd",
  "rhat", "ess_bulk", "ess_tail", "conv_rate"
)

# test -------------------------------------------------------------------------
skip_if(parallel::detectCores() < 2, "Need at least 2 cores")
conditions <- suppressMessages(suppressWarnings(
    build_conditions(
      design_fixed,
      crossed = list(n_total = 100),
      constant = list(
        b_arm_treat = 0.5,
        b_covariate = 0.3,
        thr_dec_eff = 0.975,
        thr_dec_fut = 0.9,
        thr_fx_eff = 0,
        thr_fx_fut = 0,
        p_alloc = c(0.5, 0.5)
      )
    )
  ))
result <- suppressMessages(suppressWarnings(
    power_analysis(
      conditions,
      n_sims = 5,
      n_cores = 2,
      verbosity = 0,
      brms_args = minimal_brms_args
    )
  ))
expect_s3_class(result, "rctbayespower::rctbp_power_analysis")
rc <- result@results_conditions
expect_equal(nrow(rc), 1)
expect_true(rc$pwr_eff >= 0 && rc$pwr_eff <= 1)
