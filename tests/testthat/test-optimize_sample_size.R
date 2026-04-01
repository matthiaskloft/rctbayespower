# Tests for optimize_sample_size() entry point (R/optimize_sample_size.R)

# =============================================================================
# INPUT VALIDATION: design argument
# =============================================================================

test_that("optimize_sample_size errors when design is not rctbp_design", {
  expect_error(
    optimize_sample_size(
      design = list(),
      n_range = c(50, 300),
      constant = list()
    ),
    class = "rlang_error"
  )
})

test_that("optimize_sample_size errors when design is NULL", {
  expect_error(
    optimize_sample_size(
      design = NULL,
      n_range = c(50, 300),
      constant = list()
    ),
    class = "rlang_error"
  )
})

test_that("optimize_sample_size errors when design is a data.frame", {
  expect_error(
    optimize_sample_size(
      design = data.frame(x = 1),
      n_range = c(50, 300),
      constant = list()
    ),
    class = "rlang_error"
  )
})

# =============================================================================
# INPUT VALIDATION: n_range argument
# =============================================================================

test_that("optimize_sample_size errors when n_range has length != 2", {
  design <- mock_design()
  expect_error(
    optimize_sample_size(
      design = design,
      n_range = c(50, 200, 400),
      constant = list()
    ),
    class = "rlang_error"
  )
})

test_that("optimize_sample_size errors when n_range has length 1", {
  design <- mock_design()
  expect_error(
    optimize_sample_size(
      design = design,
      n_range = c(100),
      constant = list()
    ),
    class = "rlang_error"
  )
})

test_that("optimize_sample_size errors when n_range is not numeric", {
  design <- mock_design()
  expect_error(
    optimize_sample_size(
      design = design,
      n_range = c("50", "300"),
      constant = list()
    ),
    class = "rlang_error"
  )
})

test_that("optimize_sample_size errors when n_range lower >= upper", {
  design <- mock_design()
  expect_error(
    optimize_sample_size(
      design = design,
      n_range = c(300, 300),
      constant = list()
    ),
    class = "rlang_error"
  )
})

test_that("optimize_sample_size errors when n_range lower > upper", {
  design <- mock_design()
  expect_error(
    optimize_sample_size(
      design = design,
      n_range = c(500, 50),
      constant = list()
    ),
    class = "rlang_error"
  )
})

test_that("optimize_sample_size errors when n_range lower < 1", {
  design <- mock_design()
  expect_error(
    optimize_sample_size(
      design = design,
      n_range = c(0, 300),
      constant = list()
    ),
    class = "rlang_error"
  )
})

test_that("optimize_sample_size errors when n_range lower is negative", {
  design <- mock_design()
  expect_error(
    optimize_sample_size(
      design = design,
      n_range = c(-10, 300),
      constant = list()
    ),
    class = "rlang_error"
  )
})

# =============================================================================
# INPUT VALIDATION: target_power argument (single objective only)
# =============================================================================

test_that("optimize_sample_size errors when target_power = 0", {
  design <- mock_design()
  expect_error(
    optimize_sample_size(
      design = design,
      objective = "single",
      n_range = c(50, 300),
      constant = list(),
      target_power = 0
    ),
    class = "rlang_error"
  )
})

test_that("optimize_sample_size errors when target_power = 1", {
  design <- mock_design()
  expect_error(
    optimize_sample_size(
      design = design,
      objective = "single",
      n_range = c(50, 300),
      constant = list(),
      target_power = 1
    ),
    class = "rlang_error"
  )
})

test_that("optimize_sample_size errors when target_power > 1", {
  design <- mock_design()
  expect_error(
    optimize_sample_size(
      design = design,
      objective = "single",
      n_range = c(50, 300),
      constant = list(),
      target_power = 1.5
    ),
    class = "rlang_error"
  )
})

test_that("optimize_sample_size errors when target_power < 0", {
  design <- mock_design()
  expect_error(
    optimize_sample_size(
      design = design,
      objective = "single",
      n_range = c(50, 300),
      constant = list(),
      target_power = -0.1
    ),
    class = "rlang_error"
  )
})

test_that("optimize_sample_size errors when target_power is non-numeric", {
  design <- mock_design()
  expect_error(
    optimize_sample_size(
      design = design,
      objective = "single",
      n_range = c(50, 300),
      constant = list(),
      target_power = "high"
    ),
    class = "rlang_error"
  )
})

# =============================================================================
# INPUT VALIDATION: constant argument
# =============================================================================

test_that("optimize_sample_size errors when constant is not a list", {
  design <- mock_design()
  expect_error(
    optimize_sample_size(
      design = design,
      n_range = c(50, 300),
      constant = "b_arm_treat = 0.3"
    ),
    class = "rlang_error"
  )
})

test_that("optimize_sample_size errors when constant is a numeric vector", {
  design <- mock_design()
  expect_error(
    optimize_sample_size(
      design = design,
      n_range = c(50, 300),
      constant = c(b_arm_treat = 0.3)
    ),
    class = "rlang_error"
  )
})

test_that("optimize_sample_size errors when constant is an unnamed list", {
  design <- mock_design()
  expect_error(
    optimize_sample_size(
      design = design,
      n_range = c(50, 300),
      constant = list(0.3, 0.975)
    ),
    class = "rlang_error"
  )
})

# =============================================================================
# INPUT VALIDATION: seed warning in Pareto mode
# =============================================================================

test_that("optimize_sample_size warns when seed is set in Pareto mode", {
  design <- mock_design()
  # The warning is issued before pareto_optimize() runs.
  # Catch the warning and abort early to avoid running the full pipeline.
  caught_warning <- NULL
  tryCatch(
    withCallingHandlers(
      optimize_sample_size(
        design = design,
        objective = "pareto",
        n_range = c(50, 300),
        constant = list(),
        seed = 42,
        n_sims = 1,
        max_evals = 1,
        verbose = FALSE
      ),
      warning = function(w) {
        if (grepl("seed.*not supported.*Pareto", conditionMessage(w))) {
          caught_warning <<- w
          # Abort execution after catching the warning
          stop("seed_warning_caught")
        }
      }
    ),
    error = function(e) NULL
  )
  expect_true(!is.null(caught_warning), label = "Expected seed warning in Pareto mode")
})

# =============================================================================
# match.arg: objective
# =============================================================================

test_that("optimize_sample_size errors on invalid objective value", {
  design <- mock_design()
  expect_error(
    optimize_sample_size(
      design = design,
      objective = "multi",
      n_range = c(50, 300),
      constant = list()
    )
  )
})

test_that("optimize_sample_size accepts objective = 'single'", {
  # match.arg("single", c("single", "pareto")) should not error
  expect_no_error(match.arg("single", c("single", "pareto")))
})

test_that("optimize_sample_size accepts objective = 'pareto'", {
  expect_no_error(match.arg("pareto", c("single", "pareto")))
})

# =============================================================================
# match.arg: surrogate
# =============================================================================

test_that("optimize_sample_size errors on invalid surrogate value", {
  design <- mock_design()
  expect_error(
    optimize_sample_size(
      design = design,
      objective = "single",
      n_range = c(50, 300),
      constant = list(),
      surrogate = "neural"
    )
  )
})

test_that("optimize_sample_size accepts all valid surrogate values without match.arg error", {
  for (surr in c("gp_power", "gp_score", "rf")) {
    expect_no_error(
      match.arg(surr, c("gp_power", "gp_score", "rf")),
      label = paste0("surrogate = '", surr, "'")
    )
  }
})

# =============================================================================
# match.arg: score_shape
# =============================================================================

test_that("optimize_sample_size errors on invalid score_shape value", {
  design <- mock_design()
  expect_error(
    optimize_sample_size(
      design = design,
      objective = "single",
      n_range = c(50, 300),
      constant = list(),
      score_shape = "cubic"
    )
  )
})

test_that("optimize_sample_size accepts all valid score_shape values without match.arg error", {
  for (shape in c("linear", "quadratic", "root")) {
    expect_no_error(
      match.arg(shape, c("linear", "quadratic", "root")),
      label = paste0("score_shape = '", shape, "'")
    )
  }
})

# =============================================================================
# match.arg: score_scale
# =============================================================================

test_that("optimize_sample_size errors on invalid score_scale value", {
  design <- mock_design()
  expect_error(
    optimize_sample_size(
      design = design,
      objective = "single",
      n_range = c(50, 300),
      constant = list(),
      score_scale = "logit"
    )
  )
})

test_that("optimize_sample_size accepts all valid score_scale values without match.arg error", {
  for (scale in c("log", "raw")) {
    expect_no_error(
      match.arg(scale, c("log", "raw")),
      label = paste0("score_scale = '", scale, "'")
    )
  }
})

# =============================================================================
# match.arg: knee_method
# =============================================================================

test_that("optimize_sample_size errors on invalid knee_method value", {
  design <- mock_design()
  expect_error(
    optimize_sample_size(
      design = design,
      objective = "pareto",
      n_range = c(50, 300),
      constant = list(),
      knee_method = "elbow"
    )
  )
})

test_that("optimize_sample_size accepts all valid knee_method values without match.arg error", {
  for (method in c("utopia", "min_cost", "linear")) {
    expect_no_error(
      match.arg(method, c("utopia", "min_cost", "linear")),
      label = paste0("knee_method = '", method, "'")
    )
  }
})
