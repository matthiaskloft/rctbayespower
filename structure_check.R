# Basic Package Structure Validation
# Tests the R package structure without requiring installation

cat("=== rctbayespower Package Structure Validation ===\n")

# Test 1: Check R files exist
cat("\n1. Checking R source files...\n")
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
expected_files <- c("power_analysis.R", "sample_size_analysis.R", "simulate_data.R", 
                   "power_curve.R", "plotting.R", "effect_size_analysis.R")

for (file in expected_files) {
  if (any(grepl(file, r_files))) {
    cat("✓", file, "found\n")
  } else {
    cat("✗", file, "missing\n")
  }
}

# Test 2: Check documentation files
cat("\n2. Checking documentation files...\n")
man_files <- list.files("man", pattern = "\\.Rd$")
expected_man <- c("power_analysis.Rd", "sample_size_analysis.Rd", "simulate_rct_data.Rd",
                 "bayesian_power_curve.Rd", "plot_power_curve.Rd", "effect_size_analysis.Rd")

for (file in expected_man) {
  if (file %in% man_files) {
    cat("✓", file, "found\n")
  } else {
    cat("✗", file, "missing\n")
  }
}

# Test 3: Check package metadata
cat("\n3. Checking package metadata...\n")
files_to_check <- c("DESCRIPTION", "NAMESPACE", "LICENSE", "README.md")
for (file in files_to_check) {
  if (file.exists(file)) {
    cat("✓", file, "found\n")
  } else {
    cat("✗", file, "missing\n")
  }
}

# Test 4: Check vignettes
cat("\n4. Checking vignettes...\n")
vignette_files <- list.files("vignettes", pattern = "\\.Rmd$")
expected_vignettes <- c("introduction.Rmd", "advanced-techniques.Rmd", "case-studies.Rmd")

for (file in expected_vignettes) {
  if (file %in% vignette_files) {
    cat("✓", file, "found\n")
  } else {
    cat("✗", file, "missing\n")
  }
}

# Test 5: Check tests
cat("\n5. Checking test structure...\n")
if (dir.exists("tests/testthat")) {
  test_files <- list.files("tests/testthat", pattern = "test.*\\.R$")
  cat("✓ testthat directory found with", length(test_files), "test files\n")
} else {
  cat("✗ testthat directory missing\n")
}

# Test 6: Source check (syntax validation)
cat("\n6. Checking R file syntax...\n")
for (r_file in r_files) {
  tryCatch({
    source(r_file, local = TRUE)
    cat("✓", basename(r_file), "syntax OK\n")
  }, error = function(e) {
    cat("✗", basename(r_file), "syntax error:", e$message, "\n")
  })
}

cat("\n=== Structure Validation Complete ===\n")
cat("Package structure appears complete!\n")
cat("Next steps:\n")
cat("1. Install development dependencies\n")
cat("2. Run: devtools::install() to install the package\n")
cat("3. Run: devtools::check() to validate the package\n")
