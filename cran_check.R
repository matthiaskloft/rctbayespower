# CRAN Package Check for rctbayespower
# This script runs comprehensive CRAN checks including R CMD check

cat("🔍 CRAN Package Check for rctbayespower\n")
cat("=====================================\n\n")

# Set working directory
setwd("c:/Users/Matze/Documents/GitHub/rctbayespower")

# Install required packages for checking
required_pkgs <- c("devtools", "rcmdcheck", "rhub", "urlchecker")
missing_pkgs <- required_pkgs[!required_pkgs %in% installed.packages()[,"Package"]]

if(length(missing_pkgs) > 0) {
  cat("📦 Installing required packages:", paste(missing_pkgs, collapse = ", "), "\n")
  install.packages(missing_pkgs, dependencies = TRUE)
}

# Load required libraries
suppressMessages({
  library(devtools)
  library(rcmdcheck)
})

cat("📋 Step 1: Basic package structure check...\n")
cat("──────────────────────────────────────────\n")

# Check if this is a valid package directory
if(!file.exists("DESCRIPTION")) {
  stop("❌ Not a valid R package directory - DESCRIPTION file missing")
}

# Read package info
desc <- read.dcf("DESCRIPTION")
pkg_name <- desc[1, "Package"]
pkg_version <- desc[1, "Version"]

cat("Package:", pkg_name, "\n")
cat("Version:", pkg_version, "\n")
cat("✅ Package structure validated\n\n")

cat("🧪 Step 2: Load all and basic checks...\n")
cat("─────────────────────────────────────────\n")

# Load all functions
tryCatch({
  devtools::load_all(".", quiet = TRUE)
  cat("✅ Package loads successfully\n")
}, error = function(e) {
  cat("❌ Package loading failed:", e$message, "\n")
})

# Check documentation
tryCatch({
  devtools::check_man()
  cat("✅ Manual pages check passed\n")
}, error = function(e) {
  cat("⚠️ Manual pages warning:", e$message, "\n")
})

cat("\n📝 Step 3: Comprehensive R CMD check...\n")
cat("─────────────────────────────────────────\n")

# Run comprehensive R CMD check
cat("Running R CMD check (this may take several minutes)...\n")

check_results <- tryCatch({
  rcmdcheck::rcmdcheck(
    path = ".",
    args = c("--as-cran", "--no-manual"),
    build_args = "--no-build-vignettes",
    check_dir = tempdir(),
    quiet = FALSE
  )
}, error = function(e) {
  cat("❌ R CMD check failed to run:", e$message, "\n")
  return(NULL)
})

if(!is.null(check_results)) {
  cat("\n📊 R CMD CHECK RESULTS:\n")
  cat("═══════════════════════\n")
  
  # Summary
  cat("Status:", check_results$status, "\n")
  cat("Errors:", length(check_results$errors), "\n")
  cat("Warnings:", length(check_results$warnings), "\n")
  cat("Notes:", length(check_results$notes), "\n\n")
  
  # Show errors
  if(length(check_results$errors) > 0) {
    cat("❌ ERRORS:\n")
    for(i in seq_along(check_results$errors)) {
      cat(i, ".", check_results$errors[i], "\n")
    }
    cat("\n")
  }
  
  # Show warnings
  if(length(check_results$warnings) > 0) {
    cat("⚠️ WARNINGS:\n")
    for(i in seq_along(check_results$warnings)) {
      cat(i, ".", check_results$warnings[i], "\n")
    }
    cat("\n")
  }
  
  # Show notes
  if(length(check_results$notes) > 0) {
    cat("📝 NOTES:\n")
    for(i in seq_along(check_results$notes)) {
      cat(i, ".", check_results$notes[i], "\n")
    }
    cat("\n")
  }
  
  # Overall assessment
  if(length(check_results$errors) == 0 && length(check_results$warnings) == 0) {
    cat("🎉 EXCELLENT! Package passes R CMD check with no errors or warnings!\n")
    if(length(check_results$notes) == 0) {
      cat("🌟 PERFECT! No notes either - ready for CRAN submission!\n")
    } else {
      cat("📝 Notes present but these are usually acceptable for CRAN\n")
    }
  } else if(length(check_results$errors) == 0) {
    cat("✅ Good! No errors, but warnings need to be addressed\n")
  } else {
    cat("❌ Errors must be fixed before CRAN submission\n")
  }
}

cat("\n🔗 Step 4: URL checks...\n")
cat("─────────────────────────\n")

# Check URLs if urlchecker is available
if(require(urlchecker, quietly = TRUE)) {
  tryCatch({
    url_results <- urlchecker::url_check(".")
    if(nrow(url_results) == 0) {
      cat("✅ All URLs are valid\n")
    } else {
      cat("⚠️ URL issues found:\n")
      print(url_results)
    }
  }, error = function(e) {
    cat("⚠️ URL check failed:", e$message, "\n")
  })
} else {
  cat("📦 urlchecker package not available, skipping URL checks\n")
}

cat("\n🎯 Step 5: CRAN submission readiness...\n")
cat("─────────────────────────────────────────\n")

# Check key files exist
cran_files <- c(
  "DESCRIPTION", "NAMESPACE", "LICENSE", 
  "README.md", "NEWS.md"
)

cat("📁 Required files check:\n")
for(file in cran_files) {
  exists <- file.exists(file)
  status <- if(exists) "✅" else "❌"
  cat(status, file, "\n")
}

# Check DESCRIPTION requirements
cat("\n📄 DESCRIPTION file check:\n")
desc_fields <- c("Title", "Description", "Authors@R", "License", "URL", "BugReports")
for(field in desc_fields) {
  has_field <- field %in% colnames(desc)
  status <- if(has_field) "✅" else "❌"
  cat(status, field, "\n")
}

# Check for common issues
cat("\n🔍 Common CRAN issues check:\n")

# Check for Examples in documentation
rd_files <- list.files("man", pattern = "\\.Rd$", full.names = TRUE)
has_examples <- FALSE
for(rd_file in rd_files) {
  content <- readLines(rd_file)
  if(any(grepl("\\\\examples", content))) {
    has_examples <- TRUE
    break
  }
}
cat(if(has_examples) "✅" else "❌", "Documentation includes examples\n")

# Check for tests
has_tests <- dir.exists("tests") && length(list.files("tests", recursive = TRUE)) > 0
cat(if(has_tests) "✅" else "❌", "Package includes tests\n")

# Check for vignettes
has_vignettes <- dir.exists("vignettes") && length(list.files("vignettes", pattern = "\\.Rmd$")) > 0
cat(if(has_vignettes) "✅" else "❌", "Package includes vignettes\n")

cat("\n🏆 FINAL ASSESSMENT\n")
cat("═══════════════════\n")

if(!is.null(check_results)) {
  if(length(check_results$errors) == 0 && length(check_results$warnings) == 0) {
    cat("🎉 CRAN READY! Your package passes all essential checks.\n")
    cat("\n📋 Next steps for CRAN submission:\n")
    cat("1. Review any notes and address if necessary\n")
    cat("2. Test on multiple platforms (rhub, win-builder)\n")
    cat("3. Submit to CRAN via submission form\n")
    cat("4. Respond to CRAN feedback promptly\n")
  } else {
    cat("🔧 NEEDS WORK: Address errors/warnings before CRAN submission.\n")
    cat("\n📋 Required actions:\n")
    if(length(check_results$errors) > 0) {
      cat("• Fix all errors (", length(check_results$errors), ")\n")
    }
    if(length(check_results$warnings) > 0) {
      cat("• Address warnings (", length(check_results$warnings), ")\n")
    }
  }
} else {
  cat("⚠️ INCOMPLETE: R CMD check could not be completed.\n")
  cat("Please check for missing dependencies or other issues.\n")
}

cat("\n✨ CRAN check completed! ✨\n")
