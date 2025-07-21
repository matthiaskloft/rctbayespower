#!/usr/bin/env Rscript
library(rmarkdown)
library(here)

rmf <- function(f)
{
  if (file.exists(f))
    file.remove(f)
}

clean <- function()
{
  files = dir(pattern = "*.Rmd", recursive = FALSE)
  for (f in files)
    rmf(f)
}

set_path_vignettes <- function()
{
  while (!file.exists("DESCRIPTION"))
  {
    setwd("..")
    if (getwd() == "/home")
      stop("couldn't find package!")
  }

  setwd("vignettes")
  getwd()
}


set_path_project <- function()
{
  while (!file.exists("DESCRIPTION"))
  {
    setwd("..")
    if (getwd() == "/home")
      stop("couldn't find package!")
  }
  getwd()
}


build_vignette <- function(f)
{
  f_Rmd <- basename(f)
  of <- sub(f_Rmd, pattern = "^_", replacement = "")
  rmf(of)

  fmt <- rmarkdown::md_document(variant = "gfm",
                                preserve_yaml = TRUE,
                                ext = ".Rmd")

  rmarkdown::render(
    f,
    output_file = of,
    output_dir = getwd(),
    output_format = fmt
  )

  invisible(TRUE)
}



# ------------------------------------------------------------------------------

set_path_vignettes()


#clean()


build_vignette(file.path("src", "_01-introduction.Rmd"))
#build_vignette(file.path("src", "_02-prior-specification.Rmd"))
build_vignette(file.path("src", "_03-algorithm-performance.Rmd"))

set_path_project()
