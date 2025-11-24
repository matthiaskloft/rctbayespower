library(testthat)
library(rctbayespower)

# Check if parallel testing is enabled via .Renviron
parallel_tests <- Sys.getenv("PARALLEL_TESTS", "false")

if (tolower(parallel_tests) == "true") {
  # Calculate number of cores to use (available cores - 2, minimum 1)
  n_cores <- max(1, parallel::detectCores() - 2)

  message(sprintf("Running tests in parallel mode using %d cores", n_cores))

  # Set testthat parallel options
  withr::local_options(list(
    Ncpus = n_cores
  ))

  # Run tests with parallel execution
  test_check("rctbayespower", reporter = testthat::ParallelProgressReporter$new())
} else {
  # Run tests sequentially (default)
  message("Running tests in sequential mode")
  test_check("rctbayespower")
}
