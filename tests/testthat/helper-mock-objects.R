# Mock Object Helpers for Testing
#
# Creates lightweight mock objects that satisfy S7 validators without
# requiring brms model compilation or Python/BayesFlow.

#' Create a minimal mock brmsfit object
#'
#' Satisfies inherits(x, "brmsfit") checks without compilation.
#' Stripped-down structure with just enough for validators.
#'
mock_brmsfit <- function() {
  # Minimal structure that passes inherits() and brms::variables()
  # This is the structure of a brmsfit after stats::update(chains = 0)
  fit <- list(
    formula = NULL,
    data = data.frame(x = 1),
    fit = structure(list(), class = "stanfit")
  )
  class(fit) <- "brmsfit"
  fit
}

#' Create a mock simulation function (plain function, not rctbp_sim_fn)
#'
#' Minimal function that satisfies design validator requirements:
#' must have n_total and p_alloc parameters.
#'
mock_sim_fn <- function() {
  # Use NULL defaults to match real ANCOVA sim functions.
  # get_args_without_defaults() treats NULL defaults as "required",
  # so build_conditions() will include these in params_sim.
  function(n_total, p_alloc, b_arm_treat = NULL, intercept = NULL,
           b_covariate = NULL, sigma = NULL) {
    if (is.null(b_arm_treat)) b_arm_treat <- 0
    if (is.null(intercept)) intercept <- 0
    if (is.null(b_covariate)) b_covariate <- 0
    if (is.null(sigma)) sigma <- 1
    data.frame(
      outcome = rnorm(n_total),
      covariate = rnorm(n_total),
      arm = factor(
        sample(0:1, n_total, replace = TRUE, prob = p_alloc),
        levels = 0:1
      )
    )
  }
}

#' Create a mock rctbp_sim_fn object
#'
#' Uses build_sim_fn() to create a proper rctbp_sim_fn with test output cached.
#'
mock_rctbp_sim_fn <- function() {
  fn <- function(n_total, p_alloc = c(0.5, 0.5), b_arm_treat = 0,
                 intercept = 0, b_covariate = 0, sigma = 1) {
    data.frame(
      outcome = rnorm(n_total),
      covariate = rnorm(n_total),
      arm = factor(
        sample(0:1, n_total, replace = TRUE, prob = p_alloc),
        levels = 0:1
      )
    )
  }

  build_sim_fn(
    fn = fn,
    test_args = list(
      n_total = 10L,
      p_alloc = c(0.5, 0.5),
      b_arm_treat = 0,
      intercept = 0,
      b_covariate = 0,
      sigma = 1
    )
  )
}

#' Create a mock rctbp_design directly via S7 constructor
#'
#' Bypasses build_design() to avoid brms compilation.
#' Creates a valid design with mock components.
#'
mock_design <- function(backend = "brms",
                        target_params = "b_arm2",
                        trial_type = "fixed",
                        sim_fn = NULL) {
  if (is.null(sim_fn)) {
    sim_fn <- mock_sim_fn()
  }

  rctbp_design(
    sim_fn = sim_fn,
    inference_model = mock_brmsfit(),
    backend = backend,
    display_name = "Mock ANCOVA",
    n_endpoints = 1L,
    endpoint_types = "continuous",
    n_arms = 2L,
    n_repeated_measures = 0L,
    par_names_inference = c("b_Intercept", "b_arm2", "b_covariate"),
    target_params = target_params,
    trial_type = trial_type,
    design_name = "mock_design"
  )
}

#' Create a mock rctbp_conditions object
#'
#' Bypasses build_conditions() for fast test setup.
#' Creates a minimal valid conditions object.
#'
mock_conditions <- function(design = NULL, n_conditions = 2L) {
  if (is.null(design)) {
    design <- mock_design()
  }

  # Build a simple grid
  grid <- data.frame(
    id_cond = seq_len(n_conditions),
    n_total = seq(100, by = 100, length.out = n_conditions)
  )

  # Build params_by_cond
  params_by_cond <- lapply(seq_len(n_conditions), function(i) {
    list(
      id_cond = i,
      sim_args = list(
        n_total = grid$n_total[i],
        p_alloc = c(0.5, 0.5),
        b_arm_treat = 0.3,
        intercept = 0,
        b_covariate = 0.3,
        sigma = 1
      ),
      analysis_args = list(
        thr_fx_eff = 0.2,
        thr_fx_fut = 0,
        thr_dec_eff = 0.975,
        thr_dec_fut = 0.5,
        analysis_at = NULL,
        interim_function = NULL,
        trial_type = "fixed"
      )
    )
  })

  rctbp_conditions(
    grid = grid,
    params_by_cond = params_by_cond,
    design = design,
    crossed = list(n_total = grid$n_total),
    constant = list()
  )
}

#' Create mock raw simulation results
#'
#' Creates a deterministic data.frame mimicking the output of simulation runs.
#' Uses evenly-spaced values instead of random draws for reproducible tests.
#'
mock_raw_results <- function(n_sims = 10, n_conditions = 2) {
  rows <- list()
  for (cond in seq_len(n_conditions)) {
    # Deterministic sequences that vary by condition
    pr_effs <- seq(0.6, 1.0, length.out = n_sims)
    pr_futs <- seq(0.0, 0.4, length.out = n_sims)
    post_meds <- seq(0.2, 0.4, length.out = n_sims) + (cond - 1) * 0.05
    for (sim in seq_len(n_sims)) {
      pr_eff <- pr_effs[sim]
      pr_fut <- pr_futs[sim]
      rows[[length(rows) + 1]] <- data.frame(
        id_cond = cond,
        id_iter = sim,
        par_name = "b_arm2",
        thr_fx_eff = 0.2,
        thr_fx_fut = 0,
        thr_dec_eff = 0.975,
        thr_dec_fut = 0.5,
        pr_eff = pr_eff,
        pr_fut = pr_fut,
        dec_eff = as.integer(pr_eff >= 0.975),
        dec_fut = as.integer(pr_fut >= 0.5),
        post_med = post_meds[sim],
        post_mad = 0.05,
        post_mn = post_meds[sim] + 0.01,
        post_sd = 0.1,
        rhat = 1.0,
        ess_bulk = 1000,
        ess_tail = 800,
        converged = TRUE,
        error_msg = NA_character_,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}
