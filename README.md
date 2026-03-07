
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rctbayespower

<!-- badges: start -->

[![R-CMD-check](https://github.com/matthiaskloft/rctbayespower/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/matthiaskloft/rctbayespower/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/matthiaskloft/rctbayespower/graph/badge.svg)](https://codecov.io/gh/matthiaskloft/rctbayespower)
<!-- badges: end -->

## Disclaimer

This is an experimental package that has not yet been fully tested.
Therefore, I do not recommend using it for production code or critical
analyses.

## Installation

You can install the development version of `rctbayespower` from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("matthiaskloft/rctbayespower")
```

## Overview

`rctbayespower` provides tools for conducting Bayesian power analysis
for randomized controlled trials (RCTs) with dual backend support.

### Key Features

- **Dual backend support**: brms/Stan (MCMC) and BayesFlow (neural posterior estimation)
- **Group sequential designs** with interim analyses
- **Look-dependent stopping boundaries** (O'Brien-Fleming, Pocock, Linear, Power family)
- **Post-hoc boundary comparison** without re-running simulations
- **ANCOVA models** for continuous outcomes (2-arm and 3-arm)
- **Parallelization** with automatic model caching

## Resources

The best way to get started is to visit the package website and the
introductory vignette:

- Website: <https://matthiaskloft.github.io/rctbayespower/>

- Vignettes:

  - [Introduction](https://matthiaskloft.github.io/rctbayespower/articles/01-introduction.html):
    Basic usage and concepts

  - [Prior Specifications](https://matthiaskloft.github.io/rctbayespower/articles/02-prior-specification.html):
    Working with priors

  - [Algorithm Performance](https://matthiaskloft.github.io/rctbayespower/articles/03-algorithm-performance.html):
    Performance optimization

  - [BayesFlow Backend](https://matthiaskloft.github.io/rctbayespower/articles/04-bayesflow.html):
    Neural posterior estimation for fast inference

  - [BayesFlow vs brms (CPU)](https://matthiaskloft.github.io/rctbayespower/articles/05a-bf_vs_brms_cpu_1_core.html):
    Backend comparison on CPU

  - [BayesFlow vs brms (GPU)](https://matthiaskloft.github.io/rctbayespower/articles/05b-bf_vs_brms_gpu.html):
    Backend comparison with GPU acceleration

  - [Model Validation](https://matthiaskloft.github.io/rctbayespower/articles/99-model-validation.html):
    Validation of implemented predefined models

- [Open an Issue](https://github.com/matthiaskloft/rctbayespower/issues)
