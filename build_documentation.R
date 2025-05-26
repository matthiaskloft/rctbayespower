#!/usr/bin/env Rscript
# Build Documentation and Vignettes for rctbayespower Package
# This script generates all documentation and builds vignettes

cat("🔧 Building rctbayespower Documentation and Vignettes\n")
cat("===================================================\n\n")

# Set working directory to package root
setwd("c:/Users/Matze/Documents/GitHub/rctbayespower")

# Install required packages for documentation building
required_packages <- c(
  "devtools", "roxygen2", "knitr", "rmarkdown", 
  "pkgdown", "testthat", "brms", "bayestestR", 
  "ggplot2", "dplyr", "tidyr"
)

cat("📦 Installing required packages...\n")
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if(length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, dependencies = TRUE)
} else {
  cat("All required packages are already installed.\n")
}

# Load required libraries
library(devtools)
library(roxygen2)
library(knitr)
library(rmarkdown)

cat("\n🔍 Checking package structure...\n")

# Verify package structure
if (!file.exists("DESCRIPTION")) {
  stop("❌ DESCRIPTION file not found. Make sure you're in the package root directory.")
}

if (!dir.exists("R")) {
  stop("❌ R/ directory not found. Package structure is incomplete.")
}

cat("✅ Package structure looks good.\n")

cat("\n📚 Step 1: Generating roxygen2 documentation...\n")

# Generate documentation from roxygen2 comments
tryCatch({
  roxygen2::roxygenise()
  cat("✅ Roxygen2 documentation generated successfully.\n")
}, error = function(e) {
  cat("⚠️ Warning during roxygen2 generation:", e$message, "\n")
  cat("Attempting to continue...\n")
})

cat("\n📖 Step 2: Building vignettes...\n")

# Build vignettes if they exist
if (dir.exists("vignettes")) {
  vignette_files <- list.files("vignettes", pattern = "\\.Rmd$", full.names = TRUE)
  
  if (length(vignette_files) > 0) {
    cat("Found", length(vignette_files), "vignette files:\n")
    for (vfile in vignette_files) {
      cat("  -", basename(vfile), "\n")
    }
    
    # Try to build vignettes
    tryCatch({
      devtools::build_vignettes()
      cat("✅ Vignettes built successfully.\n")
    }, error = function(e) {
      cat("⚠️ Warning during vignette building:", e$message, "\n")
      
      # Try building individual vignettes
      cat("Attempting to build vignettes individually...\n")
      for (vfile in vignette_files) {
        tryCatch({
          rmarkdown::render(vfile, 
                          output_format = "html_document",
                          output_dir = "doc")
          cat("✅ Built:", basename(vfile), "\n")
        }, error = function(e2) {
          cat("❌ Failed to build:", basename(vfile), "-", e2$message, "\n")
        })
      }
    })
  } else {
    cat("No vignette files found in vignettes/ directory.\n")
  }
} else {
  cat("No vignettes/ directory found.\n")
}

cat("\n🧪 Step 3: Running package checks...\n")

# Run basic package checks
tryCatch({
  devtools::check_man()
  cat("✅ Manual pages check passed.\n")
}, error = function(e) {
  cat("⚠️ Manual pages check warning:", e$message, "\n")
})

# Check for common issues
cat("\n🔍 Step 4: Checking for common issues...\n")

# Check NAMESPACE
if (file.exists("NAMESPACE")) {
  namespace_content <- readLines("NAMESPACE")
  if (length(namespace_content) > 0) {
    cat("✅ NAMESPACE file exists and has content.\n")
  } else {
    cat("⚠️ NAMESPACE file is empty.\n")
  }
} else {
  cat("❌ NAMESPACE file missing.\n")
}

# Check man/ directory
if (dir.exists("man")) {
  man_files <- list.files("man", pattern = "\\.Rd$")
  cat("✅ Found", length(man_files), "documentation files in man/\n")
  for (mfile in man_files) {
    cat("  -", mfile, "\n")
  }
} else {
  cat("❌ man/ directory missing.\n")
}

cat("\n📊 Step 5: Package documentation summary...\n")

# Summary of documentation
cat("Documentation Status:\n")
cat("────────────────────\n")
cat("📁 Package root: ", getwd(), "\n")
cat("📄 DESCRIPTION: ", ifelse(file.exists("DESCRIPTION"), "✅ Found", "❌ Missing"), "\n")
cat("📄 NAMESPACE: ", ifelse(file.exists("NAMESPACE"), "✅ Found", "❌ Missing"), "\n")
cat("📁 R/ directory: ", ifelse(dir.exists("R"), "✅ Found", "❌ Missing"), "\n")
cat("📁 man/ directory: ", ifelse(dir.exists("man"), "✅ Found", "❌ Missing"), "\n")
cat("📁 vignettes/ directory: ", ifelse(dir.exists("vignettes"), "✅ Found", "❌ Missing"), "\n")
cat("📁 tests/ directory: ", ifelse(dir.exists("tests"), "✅ Found", "❌ Missing"), "\n")

# Count files
if (dir.exists("R")) {
  r_files <- length(list.files("R", pattern = "\\.R$"))
  cat("🔧 R source files: ", r_files, "\n")
}

if (dir.exists("man")) {
  rd_files <- length(list.files("man", pattern = "\\.Rd$"))
  cat("📚 Documentation files: ", rd_files, "\n")
}

if (dir.exists("vignettes")) {
  vign_files <- length(list.files("vignettes", pattern = "\\.Rmd$"))
  cat("📖 Vignette files: ", vign_files, "\n")
}

cat("\n🎯 Step 6: Quick functionality test...\n")

# Try loading the package
tryCatch({
  devtools::load_all()
  cat("✅ Package loads successfully.\n")
  
  # Test if main functions are available
  if (exists("power_analysis")) {
    cat("✅ power_analysis() function is available.\n")
  }
  if (exists("simulate_rct_data")) {
    cat("✅ simulate_rct_data() function is available.\n")
  }
  if (exists("plot_power_curve")) {
    cat("✅ plot_power_curve() function is available.\n")
  }
  
}, error = function(e) {
  cat("⚠️ Package loading issue:", e$message, "\n")
})

cat("\n🏁 Documentation build process completed!\n")
cat("========================================\n")

cat("\n📋 Next Steps:\n")
cat("1. Review any warnings or errors above\n")
cat("2. Check the man/ directory for generated .Rd files\n")
cat("3. Review built vignettes in doc/ directory\n")
cat("4. Run devtools::install() to install the package\n")
cat("5. Test the package with library(rctbayespower)\n")

cat("\n✨ Happy documenting! ✨\n")
