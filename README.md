
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rctbayespower

<!-- badges: start -->

<!-- Note: Badges are placeholders for when the package is published -->

<!-- badges: end -->

## Disclaimer

This is an experimental package developed with the assistance of LLMs
and it has not yet been fully tested. Therefore, I do not recommend
using it for production code or critical analyses.

## Installation

You can install the development version of `rctbayespower` from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("matthiaskloft/rctbayespower")
```

## Overview

`rctbayespower` provides tools for conducting Bayesian power analysis
for randomized controlled trials (RCTs) using `brms` and Stan. The
package allows researchers to:

### Multiple Outcome Types

- **Continuous outcomes**: Using Cohen’s d effect sizes
- **Binary outcomes**: Using log odds ratios
- **Count outcomes**: Using log rate ratios

### Flexible Power Metrics

- **Probability of Success/Futility**: Probability effect exceeds
  success/futility thresholds
- **Significance Power**: Traditional frequentist-like power

### Advanced Features

- Custom prior specifications
- Power curves and sample size curves
- Comprehensive plotting functions

## Ressources

The best way to get started is to visit the package website and the
introductory vignette:

- Website: <https://matthiaskloft.github.io/rctbayespower/>

- Vignettes:

  - [Introduction](https://matthiaskloft.github.io/rctbayespower/articles/01-introduction.html):
    Basic usage and concepts
  - [Case
    Studies](https://matthiaskloft.github.io/rctbayespower/articles/02-case-studies.html):
    Real-world applications
  - [Prior
    Specifications](https://matthiaskloft.github.io/rctbayespower/articles/03-prior-specifications.html):
    Working with priors
  - [Advanced
    Modeling](https://matthiaskloft.github.io/rctbayespower/articles/04-advanced-modeling.html):
    Complex designs
  - [Algorithm
    Performance](https://matthiaskloft.github.io/rctbayespower/articles/05-algorithm-performance.html):
    Performance optimization

- [Open an Issue](https://github.com/matthiaskloft/rctbayespower/issues)
