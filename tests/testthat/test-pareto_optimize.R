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
