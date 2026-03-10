# =============================================================================
# parse_simplex_specs()
# =============================================================================

test_that("parse_simplex_specs uses design@n_arms for allocation search", {
  # 3-arm design
  d <- rctbp_design(
    sim_fn = mock_sim_fn(),
    inference_model = mock_brmsfit(),
    backend = "brms",
    display_name = "Mock 3-arm",
    n_endpoints = 1L,
    endpoint_types = "continuous",
    n_arms = 3L,
    n_repeated_measures = 0L,
    par_names_inference = c("b_Intercept", "b_arm2", "b_arm3", "b_covariate"),
    target_params = "b_arm2",
    trial_type = "fixed",
    design_name = "mock_3arm"
  )

  specs <- parse_simplex_specs(
    search = list(p_alloc = search_p_alloc(min = 0.1)),
    design = d
  )

  expect_equal(specs$p_alloc$n_arms, 3)
  expect_equal(specs$p_alloc$n_dims, 2)  # k-1 ILR dimensions
  expect_equal(specs$p_alloc$type, "p_alloc")

  # 2-arm design
  d2 <- mock_design()
  specs2 <- parse_simplex_specs(
    search = list(p_alloc = search_p_alloc(min = 0.2)),
    design = d2
  )

  expect_equal(specs2$p_alloc$n_arms, 2)
  expect_equal(specs2$p_alloc$n_dims, 1)
})

# =============================================================================
# apply_simplex_transforms() vs apply_simplex_transforms_flat()
# =============================================================================

test_that("apply_simplex_transforms returns structured list with $crossed, $ilr_values, $simplex_values", {
  xs <- list(p_alloc = 0.6, n_total = 100)
  specs <- list(
    p_alloc = list(type = "p_alloc", n_arms = 2, n_dims = 1, min_prop = 0.1)
  )

  result <- apply_simplex_transforms(xs, specs)

  expect_type(result, "list")
  expect_named(result, c("crossed", "ilr_values", "simplex_values"))
  # crossed should have p_alloc as a list(c(0.4, 0.6)) and n_total preserved
  expect_equal(result$crossed$n_total, 100)
  expect_equal(result$crossed$p_alloc, list(c(0.4, 0.6)))
  # simplex_values should contain the allocation vector
  expect_equal(result$simplex_values$p_alloc, c(0.4, 0.6))
})

test_that("apply_simplex_transforms_flat returns flat named list", {
  xs <- list(p_alloc = 0.6, n_total = 100)
  specs <- list(
    p_alloc = list(type = "p_alloc", n_arms = 2, n_dims = 1, min_prop = 0.1)
  )

  result <- apply_simplex_transforms_flat(xs, specs)

  expect_type(result, "list")
  # Flat list: no $crossed, $ilr_values, $simplex_values
  expect_null(result$crossed)
  expect_null(result$ilr_values)
  expect_null(result$simplex_values)
  # p_alloc transformed in-place
  expect_equal(result$p_alloc, list(c(0.4, 0.6)))
  expect_equal(result$n_total, 100)
})

test_that("both simplex transform functions handle looks specs", {
  xs <- list(analysis_at = 0.5)
  specs <- list(
    analysis_at = list(type = "looks", n_looks = 2, n_dims = 1, min_spacing = 0.1)
  )

  # Structured version
  structured <- apply_simplex_transforms(xs, specs)
  expect_named(structured, c("crossed", "ilr_values", "simplex_values"))
  expect_equal(structured$simplex_values$analysis_at, 0.5)

  # Flat version
  flat <- apply_simplex_transforms_flat(xs, specs)
  expect_null(flat$crossed)
  expect_equal(flat$analysis_at, c(0.5, 1.0))
})

test_that("both simplex transform functions handle empty specs", {
  xs <- list(n_total = 200, b_arm_treat = 0.3)
  specs <- list()

  structured <- apply_simplex_transforms(xs, specs)
  expect_equal(structured$crossed$n_total, 200)
  expect_equal(structured$crossed$b_arm_treat, 0.3)

  flat <- apply_simplex_transforms_flat(xs, specs)
  expect_equal(flat$n_total, 200)
  expect_equal(flat$b_arm_treat, 0.3)
})
