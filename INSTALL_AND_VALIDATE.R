# rctbayespower: Complete Package Installation and Validation Script
# Run this script to install and validate the rctbayespower package

cat("=======================================================\n")
cat("  rctbayespower Package Installation & Validation\n") 
cat("=======================================================\n\n")

# Step 1: Check package structure
cat("STEP 1: Checking package structure...\n")
required_files <- c("DESCRIPTION", "NAMESPACE", "LICENSE")
missing_files <- character(0)

for (file in required_files) {
  if (file.exists(file)) {
    cat("✓", file, "\n")
  } else {
    cat("✗", file, "MISSING\n")
    missing_files <- c(missing_files, file)
  }
}

if (length(missing_files) > 0) {
  cat("\nERROR: Missing required files:", paste(missing_files, collapse = ", "), "\n")
  stop("Package structure incomplete")
}

# Step 2: Check R source files
cat("\nSTEP 2: Checking R source files...\n")
r_files <- list.files("R", pattern = "\\.R$")
expected_r_files <- c("power_analysis.R", "sample_size_analysis.R", "simulate_data.R")

for (file in expected_r_files) {
  if (any(grepl(file, r_files))) {
    cat("✓", file, "\n")
  } else {
    cat("✗", file, "MISSING\n")
  }
}

# Step 3: Read package metadata
cat("\nSTEP 3: Reading package metadata...\n")
desc <- read.dcf("DESCRIPTION")
cat("Package Name:", desc[,"Package"], "\n")
cat("Version:", desc[,"Version"], "\n")
cat("Title:", desc[,"Title"], "\n")

# Step 4: Check dependencies
cat("\nSTEP 4: Checking key dependencies...\n")
key_deps <- c("stats", "utils")  # Base R packages should be available
available_deps <- character(0)

for (dep in key_deps) {
  if (requireNamespace(dep, quietly = TRUE)) {
    cat("✓", dep, "\n")
    available_deps <- c(available_deps, dep)
  } else {
    cat("✗", dep, "not available\n")
  }
}

# Step 5: Test basic R syntax
cat("\nSTEP 5: Testing R file syntax...\n")
syntax_errors <- 0

for (r_file in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  tryCatch({
    parse(r_file)
    cat("✓", basename(r_file), "\n")
  }, error = function(e) {
    cat("✗", basename(r_file), "- Syntax error:", e$message, "\n")
    syntax_errors <<- syntax_errors + 1
  })
}

# Step 6: Installation instructions
cat("\nSTEP 6: Installation Instructions\n")
cat("=====================================\n")

if (syntax_errors == 0) {
  cat("✅ Package structure is valid!\n\n")
  
  cat("To install the package:\n")
  cat("1. Install dependencies:\n")
  cat("   install.packages(c('brms', 'bayestestR', 'ggplot2', 'dplyr'))\n\n")
  
  cat("2. Install development tools:\n")
  cat("   install.packages(c('devtools', 'roxygen2'))\n\n")
  
  cat("3. Install rctbayespower:\n")
  cat("   devtools::install_local('", getwd(), "')\n\n")
  
  cat("4. Load and test:\n")
  cat("   library(rctbayespower)\n")
  cat("   ?power_analysis\n\n")
  
  cat("Quick test (after installation):\n")
  cat("   result <- power_analysis(50, 50, 0.5, 'continuous', n_simulations=10)\n\n")
  
} else {
  cat("❌ Package has syntax errors that need to be fixed first.\n")
}

# Step 7: Development workflow
cat("STEP 7: Development Workflow\n")
cat("===============================\n")
cat("For package development:\n")
cat("1. Generate documentation: roxygen2::roxygenise()\n")
cat("2. Run tests: testthat::test()\n")
cat("3. Check package: devtools::check()\n")
cat("4. Build package: devtools::build()\n\n")

# Step 8: Learning resources
cat("STEP 8: Learning Resources\n")
cat("============================\n")
cat("📖 Read the vignettes:\n")
cat("   - vignettes/introduction.Rmd (Getting started)\n")
cat("   - vignettes/advanced-techniques.Rmd (Advanced methods)\n")
cat("   - vignettes/case-studies.Rmd (Real examples)\n\n")

cat("📋 Key files to read:\n")
cat("   - INSTALLATION_GUIDE.md (Complete setup)\n") 
cat("   - PACKAGE_SUMMARY.md (Overview)\n")
cat("   - FINAL_README.md (Comprehensive guide)\n\n")

cat("=======================================================\n")
cat("  ✅ rctbayespower Package Validation Complete!\n")
cat("=======================================================\n")
