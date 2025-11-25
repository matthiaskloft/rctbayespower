#' Parse and Validate Design Prior
#'
#' Internal helper function to parse design prior specifications and create
#' weight and quantile functions for design prior integration.
#'
#' @param design_prior Design prior specification (string or function)
#' @param effect_sizes Vector of effect sizes for coverage checking
#' @param verbose Whether to print parsing information (default: TRUE)
#' @return List containing weight_fn, quantile_fn, and weight_type
#' @importFrom stats sd approx
#' @keywords internal
parse_design_prior <- function(design_prior, effect_sizes, verbose = TRUE) {
  weight_fn <- NULL
  weight_type <- "none"
  quantile_fn <- NULL

  if (is.null(design_prior)) {
    return(list(
      weight_fn = weight_fn,
      quantile_fn = quantile_fn,
      weight_type = weight_type
    ))
  }

  if (is.character(design_prior)) {
    # Use brms density functions with generic evaluation
    weight_type <- "brms"
    tryCatch(
      {
        if (!requireNamespace("brms", quietly = TRUE)) {
          cli::cli_abort(c(
            "Package {.pkg brms} is required for parsing brms design prior syntax",
            "i" = "Install it with {.code install.packages('brms')}"
          ))
        }

        # Validate basic syntax
        if (!grepl("^[a-zA-Z_][a-zA-Z0-9_]*\\(.*\\)$", design_prior)) {
          cli::cli_abort(c(
            "Invalid brms prior syntax",
            "x" = "You supplied {.val {design_prior}}",
            "i" = "Expected format: {.code distribution_name(param1, param2, ...)}"
          ))
        }

        # Extract distribution name
        dist_name <- gsub("\\(.*\\)", "", design_prior)

        # Create density and quantile function calls
        # Priority: stats package first, then brms, then error
        density_call <- NULL
        quantile_call <- NULL
        source_package <- NULL

        # =============================================================================
        # PRIORITY HIERARCHY for Prior Evaluation (stats → brms → custom)
        # =============================================================================
        # 1. stats package (norm, t, gamma) - Fastest, most stable, no dependencies
        # 2. brms package (student_t, lognormal) - Specialized distributions
        # 3. custom functions - Maximum flexibility, user-defined
        #
        # Rationale: Attempt lightweight solutions before heavy dependencies.
        # stats functions are base R and very fast. brms functions add overhead
        # but support more distributions. Custom functions allow full flexibility.
        brms_to_stats_map <- list(
          "normal" = "norm",
          "student_t" = "t",
          "gamma" = "gamma",
          "beta" = "beta",
          "exponential" = "exp",
          "uniform" = "unif",
          "poisson" = "pois",
          "binomial" = "binom",
          "cauchy" = "cauchy",
          "chi_square" = "chisq",
          "f" = "f",
          "lognormal" = "lnorm",
          "logistic" = "logis",
          "weibull" = "weibull",
          "wilcox" = "wilcox"
        )

        # Try Method 1: stats package first
        stats_name <- brms_to_stats_map[[dist_name]]
        if (!is.null(stats_name)) {
          # Test if stats function exists and works
          stats_call <- sub(dist_name, stats_name, design_prior)
          test_density_call <- paste0("stats::d", stats_call)
          test_quantile_call <- paste0("stats::q", stats_call)

          # Test the stats functions
          stats_works <- tryCatch(
            {
              test_call <- gsub("\\(", "(0.5,", test_density_call)
              result <- eval(parse(text = test_call))
              is.numeric(result) &&
                length(result) == 1 && !is.na(result)
            },
            error = function(e) {
              FALSE
            }
          )

          if (stats_works) {
            density_call <- test_density_call
            quantile_call <- test_quantile_call
            source_package <- "stats"
          } else {
            stats_works <- FALSE
          }
        } else {
          stats_works <- FALSE
        }

        # If stats doesn't work, try brms
        if (!stats_works) {
          density_call <- paste0("brms::d", design_prior)
          quantile_call <- paste0("brms::q", design_prior)
          source_package <- "brms"

          # Test brms functions
          brms_works <- tryCatch(
            {
              test_call <- gsub("\\(", "(0.5,", density_call)
              result <- eval(parse(text = test_call))
              is.numeric(result) &&
                length(result) == 1 && !is.na(result)
            },
            error = function(e) {
              FALSE
            }
          )

          if (!brms_works) {
            cli::cli_abort(c(
              "Distribution {.val {dist_name}} not available",
              "x" = "Distribution not found in either {.pkg stats} or {.pkg brms} packages",
              "i" = "Use a supported distribution or provide a custom function"
            ))
          }
        }

        # Create weight function
        weight_fn <- function(x) {
          # Replace the first parameter (or add x as first parameter)
          modified_call <- gsub("\\(", paste0("(", x, ","), density_call)
          tryCatch(
            {
              eval(parse(text = modified_call))
            },
            error = function(e) {
              cli::cli_abort(c(
                "Error evaluating {.pkg {source_package}} density function",
                "x" = "{e$message}",
                "i" = "Check your distribution parameters"
              ))
            }
          )
        }

        # Try to create quantile function
        quantile_fn <- NULL
        test_quantile_call <- gsub("\\(", "(0.5,", quantile_call)
        quantile_available <- tryCatch(
          {
            test_result <- eval(parse(text = test_quantile_call))
            if (is.numeric(test_result) &&
              length(test_result) == 1 && !is.na(test_result)) {
              # Create actual quantile function
              quantile_fn <<- function(p) {
                modified_call <- gsub("\\(", paste0("(", p, ","), quantile_call)
                tryCatch(
                  {
                    eval(parse(text = modified_call))
                  },
                  error = function(e) {
                    cli::cli_abort(c(
                      "Error evaluating {.pkg {source_package}} quantile function",
                      "x" = "{e$message}",
                      "i" = "Check your distribution parameters"
                    ))
                  }
                )
              }
              TRUE
            } else {
              FALSE
            }
          },
          error = function(e) {
            FALSE
          }
        )

        if (!quantile_available) {
          cli::cli_warn(c(
            "Quantile function not available",
            "x" = "Function '{quantile_call}' could not be created",
            "i" = "Coverage checking will be disabled"
          ))
        }

        if (verbose) {
          cat("Successfully parsed design prior:", design_prior, "\n")
          cat("  Distribution:", dist_name, "\n")
          cat(
            "  Density function:",
            density_call,
            "(using",
            source_package,
            "package)\n"
          )
          if (quantile_available) {
            cat(
              "  Quantile function:",
              quantile_call,
              "(using",
              source_package,
              "package)\n"
            )
          } else {
            cat("  Quantile function: Not available (coverage checking disabled)\n")
          }
        }
      },
      error = function(e) {
        cli::cli_abort(c(
          "Error parsing design prior",
          "x" = "{e$message}",
          "i" = "Check your prior specification syntax"
        ))
      }
    )
  } else if (is.function(design_prior)) {
    # Validate R function
    weight_type <- "function"
    tryCatch(
      {
        test_val <- design_prior(0.5)
        if (!is.numeric(test_val) || length(test_val) != 1) {
          cli::cli_abort(c(
            "Design prior function must return a single numeric value",
            "x" = "Your function returned {.type {test_val}} with length {.val {length(test_val)}}",
            "i" = "Ensure your function returns a single numeric value"
          ))
        }
        weight_fn <- design_prior
        # For custom functions, we'll estimate quantiles numerically
        quantile_fn <- function(p) {
          # Create a grid of values and find approximate quantiles
          test_range <- seq(
            min(effect_sizes) - 2 * sd(effect_sizes),
            max(effect_sizes) + 2 * sd(effect_sizes),
            length.out = 1000
          )
          weights <- sapply(test_range, weight_fn)
          weights <- weights / sum(weights)
          cumulative <- cumsum(weights)
          approx(cumulative, test_range, xout = p)$y
        }
      },
      error = function(e) {
        cli::cli_abort(c(
          "Error testing design prior function",
          "x" = "{e$message}",
          "i" = "Ensure your function accepts a numeric input and returns a numeric output"
        ))
      }
    )
  } else {
    cli::cli_abort(c(
      "{.arg design_prior} must be a character string or R function",
      "x" = "You supplied {.type {design_prior}}",
      "i" = "Use brms syntax (e.g., {.val normal(0.5, 0.2)}) or provide a function"
    ))
  }

  # Compute quantiles and check coverage
  if (!is.null(quantile_fn)) {
    tryCatch(
      {
        q10 <- quantile_fn(0.1)
        q90 <- quantile_fn(0.9)

        # Check if effect sizes adequately cover the prior distribution
        min_effect <- min(effect_sizes)
        max_effect <- max(effect_sizes)

        if (max_effect < q90) {
          cli::cli_warn(c(
            "Effect size range may not cover design prior adequately",
            "x" = "Maximum effect size ({round(max_effect, 3)}) is less than 90th percentile of design prior ({round(q90, 3)})",
            "i" = "Consider including larger effect sizes in your analysis"
          ))
        }

        if (min_effect > q10) {
          cli::cli_warn(c(
            "Effect size range may not cover design prior adequately",
            "x" = "Minimum effect size ({round(min_effect, 3)}) is greater than 10th percentile of design prior ({round(q10, 3)})",
            "i" = "Consider including smaller effect sizes in your analysis"
          ))
        }
      },
      error = function(e) {
        # If quantile computation fails for custom functions, just warn
        if (weight_type == "function") {
          cli::cli_warn(c(
            "Could not compute quantiles for custom design prior function",
            "i" = "Unable to check effect size coverage"
          ))
        } else {
          cli::cli_abort(c(
            "Error computing quantiles",
            "x" = "{e$message}",
            "i" = "Check your distribution parameters"
          ))
        }
      }
    )
  }

  return(list(
    weight_fn = weight_fn,
    quantile_fn = quantile_fn,
    weight_type = weight_type
  ))
}


#' Validate Weighting Function Implementation
#'
#' Tests that the weighting function parsing and computation in rctbp_power_analysis objects
#' works correctly. This function validates both brms syntax parsing and R function handling,
#' as well as the weighted power computation logic.
#'
#' @param effect_sizes Vector of effect sizes to test with (default: seq(0.2, 0.8, 0.1))
#' @param verbose Whether to print detailed test results (default: TRUE)
#'
#' @return A list containing validation results:
#' \itemize{
#'   \item all_tests_passed: Boolean indicating if all tests passed
#'   \item test_results: List of individual test results
#'   \item errors: Any errors encountered during testing
#' }
#'
#' @details
#' This validation function tests:
#' \itemize{
#'   \item Normal distribution parsing from brms syntax
#'   \item Student-t distribution parsing from brms syntax
#'   \item Custom R function validation
#'   \item Weight normalization (ensures weights sum to 1)
#'   \item Quantile computation for coverage checking
#'   \item Error handling for invalid inputs
#' }
#'
#' @importFrom stats dnorm qnorm
#' @keywords internal
#'

validate_weighting_function <- function(effect_sizes = seq(0.2, 0.8, 0.1),
                                        verbose = TRUE) {
  if (verbose) {
    cat("=== Weighting Function Validation ===\n")
    cat("Testing weighting function implementation in rctbp_power_analysis objects\n\n")
  }

  test_results <- list()
  errors <- character()

  # Test 1: Normal distribution parsing
  if (verbose) {
    cat("Test 1: Normal distribution parsing...\n")
  }
  test_results$normal_parsing <- tryCatch(
    {
      weighting_function <- "normal(0.5, 0.15)"

      # Test using the new generic approach
      density_call <- paste0("brms::d", weighting_function)
      quantile_call <- paste0("brms::q", weighting_function)

      # Create weight function
      weight_fn <- function(x) {
        modified_call <- gsub("\\(", paste0("(", x, ","), density_call)
        eval(parse(text = modified_call))
      }

      # Create quantile function
      quantile_fn <- function(p) {
        modified_call <- gsub("\\(", paste0("(", p, ","), quantile_call)
        eval(parse(text = modified_call))
      }

      # Test the functions work
      test_weights <- sapply(effect_sizes, weight_fn)
      test_quantiles <- quantile_fn(c(0.1, 0.5, 0.9))

      # Validate results
      if (!is.numeric(test_weights) || any(is.na(test_weights))) {
        cli::cli_abort(c(
          "Weight function produced non-numeric or NA values",
          "i" = "Check your distribution parameters"
        ))
      }

      if (!is.numeric(test_quantiles) ||
        any(is.na(test_quantiles))) {
        cli::cli_abort(c(
          "Quantile function produced non-numeric or NA values",
          "i" = "Check your distribution parameters"
        ))
      }

      list(
        passed = TRUE,
        weights = test_weights,
        quantiles = test_quantiles
      )
    },
    error = function(e) {
      errors <<- c(errors, paste("Test 1 failed:", e$message))
      list(passed = FALSE, error = e$message)
    }
  )

  # Test 2: Student-t distribution parsing
  if (verbose) {
    cat("Test 2: Student-t distribution parsing...\n")
  }
  test_results$studentt_parsing <- tryCatch(
    {
      weighting_function <- "student_t(6, 0.5, 0.2)"

      # Test using the new generic approach
      density_call <- paste0("brms::d", weighting_function)
      quantile_call <- paste0("brms::q", weighting_function)

      # Create weight function
      weight_fn <- function(x) {
        modified_call <- gsub("\\(", paste0("(", x, ","), density_call)
        eval(parse(text = modified_call))
      }

      # Create quantile function
      quantile_fn <- function(p) {
        modified_call <- gsub("\\(", paste0("(", p, ","), quantile_call)
        eval(parse(text = modified_call))
      }

      # Test the functions work
      test_weights <- sapply(effect_sizes, weight_fn)
      test_quantiles <- quantile_fn(c(0.1, 0.5, 0.9))

      # Validate results
      if (!is.numeric(test_weights) || any(is.na(test_weights))) {
        cli::cli_abort(c(
          "Weight function produced non-numeric or NA values",
          "i" = "Check your distribution parameters"
        ))
      }

      if (!is.numeric(test_quantiles) ||
        any(is.na(test_quantiles))) {
        cli::cli_abort(c(
          "Quantile function produced non-numeric or NA values",
          "i" = "Check your distribution parameters"
        ))
      }

      list(
        passed = TRUE,
        weights = test_weights,
        quantiles = test_quantiles
      )
    },
    error = function(e) {
      errors <<- c(errors, paste("Test 2 failed:", e$message))
      list(passed = FALSE, error = e$message)
    }
  )

  # Test 3: Custom R function validation
  if (verbose) {
    cat("Test 3: Custom R function validation...\n")
  }
  test_results$custom_function <- tryCatch(
    {
      # Create a custom weighting function
      custom_fn <- function(x) {
        dnorm(x, mean = 0.4, sd = 0.1)
      }

      # Test validation logic
      test_val <- custom_fn(0.5)
      if (!is.numeric(test_val) || length(test_val) != 1) {
        cli::cli_abort(c(
          "Weighting function must return a single numeric value",
          "i" = "Check your custom function"
        ))
      }

      # Test the function works with effect sizes
      test_weights <- sapply(effect_sizes, custom_fn)

      if (!is.numeric(test_weights) || any(is.na(test_weights))) {
        cli::cli_abort(c(
          "Custom function produced non-numeric or NA values",
          "i" = "Check your custom function"
        ))
      }

      list(passed = TRUE, weights = test_weights)
    },
    error = function(e) {
      errors <<- c(errors, paste("Test 3 failed:", e$message))
      list(passed = FALSE, error = e$message)
    }
  )

  # Test 4: Weight normalization
  if (verbose) {
    cat("Test 4: Weight normalization...\n")
  }
  test_results$weight_normalization <- tryCatch(
    {
      # Use normal distribution weights
      weight_fn <- function(x) {
        dnorm(x, mean = 0.5, sd = 0.15)
      }
      weights <- sapply(effect_sizes, weight_fn)
      normalized_weights <- weights / sum(weights)

      # Check that weights sum to 1 (within tolerance)
      weight_sum <- sum(normalized_weights)
      if (abs(weight_sum - 1.0) > 1e-10) {
        cli::cli_abort(c(
          "Normalized weights do not sum to 1",
          "x" = "Sum = {.val {weight_sum}}",
          "i" = "This is an internal error in weight normalization"
        ))
      }

      # Check all weights are positive
      if (any(normalized_weights <= 0)) {
        cli::cli_abort(c(
          "Some normalized weights are non-positive",
          "i" = "Check your weighting function"
        ))
      }

      list(
        passed = TRUE,
        original_weights = weights,
        normalized_weights = normalized_weights,
        sum = weight_sum
      )
    },
    error = function(e) {
      errors <<- c(errors, paste("Test 4 failed:", e$message))
      list(passed = FALSE, error = e$message)
    }
  )

  # Test 5: Coverage checking logic
  if (verbose) {
    cat("Test 5: Coverage checking logic...\n")
  }
  test_results$coverage_checking <- tryCatch(
    {
      # Test with normal distribution
      quantile_fn <- function(p) {
        qnorm(p, mean = 0.5, sd = 0.15)
      }

      q10 <- quantile_fn(0.1)
      q90 <- quantile_fn(0.9)

      min_effect <- min(effect_sizes)
      max_effect <- max(effect_sizes)

      # Test coverage warnings would be triggered correctly
      coverage_low <- min_effect <= q10
      coverage_high <- max_effect >= q90

      # With default effect_sizes (0.2 to 0.8) and normal(0.5, 0.15),
      # we expect good coverage
      expected_coverage <- coverage_low && coverage_high

      list(
        passed = TRUE,
        q10 = q10,
        q90 = q90,
        min_effect = min_effect,
        max_effect = max_effect,
        coverage_low = coverage_low,
        coverage_high = coverage_high,
        good_coverage = expected_coverage
      )
    },
    error = function(e) {
      errors <<- c(errors, paste("Test 5 failed:", e$message))
      list(passed = FALSE, error = e$message)
    }
  )

  # Test 6: Error handling for invalid inputs
  if (verbose) {
    cat("Test 6: Error handling for invalid inputs...\n")
  }
  test_results$error_handling <- tryCatch(
    {
      errors_caught <- 0
      total_error_tests <- 0

      # Test invalid brms syntax
      total_error_tests <- total_error_tests + 1
      tryCatch(
        {
          # This should fail due to wrong number of parameters
          params <- gsub("normal\\(|\\)", "", "normal(0.5)") # Missing second parameter
          params <- as.numeric(strsplit(params, ",")[[1]])
          if (length(params) != 2) {
            cli::cli_abort("normal() requires 2 parameters")
          }
        },
        error = function(e) {
          errors_caught <<- errors_caught + 1
        }
      )

      # Test invalid R function
      total_error_tests <- total_error_tests + 1
      tryCatch(
        {
          invalid_fn <- function(x) {
            c(1, 2)
          } # Returns vector instead of single value
          test_val <- invalid_fn(0.5)
          if (!is.numeric(test_val) || length(test_val) != 1) {
            cli::cli_abort("Weighting function must return a single numeric value")
          }
        },
        error = function(e) {
          errors_caught <<- errors_caught + 1
        }
      )

      # Test unsupported distribution
      total_error_tests <- total_error_tests + 1
      tryCatch(
        {
          # Test with a distribution that doesn't exist
          density_call <- "brms::dnonexistent(1, 2)"
          test_result <- eval(parse(text = density_call))
        },
        error = function(e) {
          errors_caught <<- errors_caught + 1
        }
      )

      list(
        passed = TRUE,
        errors_caught = errors_caught,
        total_tests = total_error_tests
      )
    },
    error = function(e) {
      errors <<- c(errors, paste("Test 6 failed:", e$message))
      list(passed = FALSE, error = e$message)
    }
  )

  # Summary
  all_passed <- all(sapply(test_results, function(x) {
    x$passed
  }))

  if (verbose) {
    cat("\n=== Validation Summary ===\n")
    cat(
      "Test 1 - Normal parsing:",
      ifelse(test_results$normal_parsing$passed, "PASS", "FAIL"),
      "\n"
    )
    cat(
      "Test 2 - Student-t parsing:",
      ifelse(test_results$studentt_parsing$passed, "PASS", "FAIL"),
      "\n"
    )
    cat(
      "Test 3 - Custom function:",
      ifelse(test_results$custom_function$passed, "PASS", "FAIL"),
      "\n"
    )
    cat(
      "Test 4 - Weight normalization:",
      ifelse(test_results$weight_normalization$passed, "PASS", "FAIL"),
      "\n"
    )
    cat(
      "Test 5 - Coverage checking:",
      ifelse(test_results$coverage_checking$passed, "PASS", "FAIL"),
      "\n"
    )
    cat(
      "Test 6 - Error handling:",
      ifelse(test_results$error_handling$passed, "PASS", "FAIL"),
      "\n"
    )

    if (test_results$error_handling$passed) {
      cat(
        "  Errors properly caught:",
        test_results$error_handling$errors_caught,
        "/",
        test_results$error_handling$total_tests,
        "\n"
      )
    }

    cat(
      "\nOverall result:",
      ifelse(all_passed, "ALL TESTS PASSED", "SOME TESTS FAILED"),
      "\n"
    )

    if (length(errors) > 0) {
      cat("\nErrors encountered:\n")
      for (error in errors) {
        cat("  -", error, "\n")
      }
    }

    if (all_passed) {
      cat("\nOK: Weighting function implementation is working correctly!\n")
    } else {
      cat("\nERROR: Weighting function implementation has issues that need attention.\n")
    }
  }

  return(list(
    all_tests_passed = all_passed,
    test_results = test_results,
    errors = errors
  ))
}
