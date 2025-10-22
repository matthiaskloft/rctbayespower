#!/usr/bin/env Rscript

# Vignette Rebuild Script
# ========================
# This script rebuilds package vignettes from source files in the ./src/ directory.
# It converts source .Rmd files (prefixed with underscore) to final .Rmd vignettes
# in GitHub Flavored Markdown format.

library(rmarkdown)

# Helper Functions
# ----------------

#' Remove File if it Exists
#'
#' Safe file removal wrapper that checks for existence before deletion.
#'
#' @param file_path Character string specifying the file path to remove
rmf <- function(file_path)
{
  if (file.exists(file_path))
    file.remove(file_path)
}

#' Clean All Vignette Output Files
#'
#' Removes all .Rmd files in the current directory (vignettes folder).
#' This ensures a clean rebuild by removing previously generated vignette files.
#' Note: This does NOT remove source files in ./src/ subdirectory.
clean <- function()
{
  # Find all .Rmd files in current directory (not recursive)
  vignette_files = dir(pattern = "*.Rmd", recursive = FALSE)

  # Remove each found file
  for (file in vignette_files)
    rmf(file)
}

#' Build a Single Vignette from Source
#'
#' Converts a source vignette file (typically prefixed with underscore) to a
#' final vignette file by:
#' 1. Removing the underscore prefix from the filename
#' 2. Rendering to GitHub Flavored Markdown (.Rmd) format
#' 3. Preserving YAML frontmatter for package vignette metadata
#'
#' @param source_file Character string specifying the source file path (e.g., "./src/_intro.Rmd")
#' @return Invisible TRUE on success
build_vignette <- function(source_file)
{
  # Extract the base filename (e.g., "_intro.Rmd")
  source_basename <- basename(source_file)

  # Remove leading underscore to create output filename (e.g., "intro.Rmd")
  output_file <- sub(source_basename, pattern = "^_", replacement = "")

  # Remove existing output file if present
  rmf(output_file)

  # Configure output format: GitHub Flavored Markdown with preserved YAML
  # The .Rmd extension is used because package vignettes expect this format
  output_format <- rmarkdown::md_document(variant = "gfm",
                                          preserve_yaml = TRUE,
                                          ext = ".Rmd")

  # Render the source file to the output file
  rmarkdown::render(
    source_file,
    output_file = output_file,
    output_dir = getwd(),  # Output to current directory (vignettes/)
    output_format = output_format
  )

  invisible(TRUE)
}


# Main Execution
# --------------

# Step 1: Clean up any existing vignette output files
clean()

# Step 2: Build individual vignettes from source files
# Add additional build_vignette() calls here as new vignettes are added
build_vignette("./src/01-introduction.Rmd")
