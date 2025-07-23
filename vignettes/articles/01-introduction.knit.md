---
title: "Getting Started with 'rctbayespower'"
author: 
 - name: Matthias Kloft
   orcid: 0000-0003-1845-6957
date: "2025-07-23"
format:
  html:
    toc: true
    number-sections: true
    theme: cosmo
    code-fold: true
    code-tools: true
    code-summary: "Show the code"
    fig-width: 7
    fig-height: 4.5
    embed-resources: true
execute:
  message: false
  warning: false
---



# Introduction

The `rctbayespower` package provides tools for conducting Bayesian power analysis for randomized controlled trials (RCTs) using the `brms` package and Stan. 



# Basic Power Analysis Using ANCOVA Design



### Model


::: {.cell}

```{.r .cell-code}
model_file <- here::here("test_model_ancova.rds")
if (file.exists(model_file)) {
  # load the model from a file
  model_ancova <- readRDS(model_file)
} else{
  # create the model
  model_ancova <- build_model(pre_defined_model = "ancova_cont_2arms")
  # save the model to a file
  saveRDS(model_ancova, file = model_file)
}
```

::: {.cell-output .cell-output-stdout}

```
Compiling the brms model ...
Model compilation done!
```


:::

```{.r .cell-code}
print(model_ancova)
```

::: {.cell-output .cell-output-stdout}

```

Object of class: 'rctbayespower_model'
--------------------------------------------------

Model name: 
Number of endpoints: 
Endpoint types:  
Number of arms: 
Number of repeated measures: 0 
Parameter names - simulation function: n_total n_arms contrasts p_alloc intercept b_arm_treat b_covariate sigma 
Parameter names - brms model: b_Intercept b_covariate b_arm2 

Brms model:
 Family: gaussian 
  Links: mu = identity; sigma = identity 
Formula: outcome ~ 1 + covariate + arm 
   Data: mock_data_ancova (Number of observations: 20) 

The model does not contain posterior draws.
```


:::
:::


### Design

Find possible target parameters in the model.

::: {.cell}

```{.r .cell-code}
attr(model_ancova, "parameter_names_brms")
```

::: {.cell-output .cell-output-stdout}

```
NULL
```


:::
:::


Specify the study design with target parameter, thresholds for success and futility, and significance levels for frequentist-like power.

::: {.cell}

```{.r .cell-code}
design <- build_design(
  model = model_ancova,
  target_params = "b_arm2",
  thresholds_success = 0.1,
  thresholds_futility = 0,
  p_sig_success = 0.975,
  p_sig_futility = 0.5
)
print(design)
```

::: {.cell-output .cell-output-stdout}

```

Object of class: 'rctbayespower_design'
--------------------------------------------------

=== Model Specifications ===

Number of endpoints: 1 
Endpoint types: continuous 
Number of arms: 2 
Number of repeated measures: 0 
Parameter names - simulation function: n_total, n_arms, contrasts, p_alloc, intercept, b_arm_treat, b_covariate, sigma 
Parameter names - brms model: b_Intercept, b_covariate, b_arm2 


=== Design Specifications ===

Design name: 
Target parameters: b_arm2 
Number of interim analyses: 
Thresholds for success: 0.1 
Thresholds for futility: 0 
Probability of success significance: 0.975 
Probability of futility significance: 0.5 
Parameter names - interim function:  


=== Brms Model ===

 Family: gaussian 
  Links: mu = identity; sigma = identity 
Formula: outcome ~ 1 + covariate + arm 
   Data: mock_data_ancova (Number of observations: 20) 

The model does not contain posterior draws.


 === Allocation Function ===

NULL
```


:::
:::



### Conditions

Find parameters that need user specification for the design.

::: {.cell}

```{.r .cell-code}
required_fn_args(design)
```

::: {.cell-output .cell-output-stdout}

```

Arguments that need user specification.

Simulation function:
n_total, b_arm_treat, b_covariate 

Interim function:
 
```


:::
:::


Specify conditions.

::: {.cell}

```{.r .cell-code}
conditions <- build_conditions(
  design = design,
  condition_values = list(
    # two sample sizes
    n_total = 300,
        # two effect sizes
    b_arm_treat = c(0,0.3)
  ),
  static_values = list(
    # equal allocation
    p_alloc =
      list(c(0.5, 0.5)),
          # baseline effect
    b_covariate = 0
  )
)
print(conditions, n = 100)
```

::: {.cell-output .cell-output-stdout}

```

Object of class: 'rctbayespower_conditions'
--------------------------------------------------

Number of conditions: 2 
Number of varying parameters: 3 

Condition Grid:
# A tibble: 2 × 3
  id_cond n_total b_arm_treat
    <int>   <dbl>       <dbl>
1       1     300         0  
2       2     300         0.3
```


:::
:::



## Run Power Analysis
Set number of cores and simulations. The number of simulations is set to a lower value for faster vignette building, but you can increase it for more robust results.

::: {.cell}

```{.r .cell-code}
n_cores <- parallel::detectCores() - 2
n_sims <- 1e3
```
:::



::: {.cell}

```{.r .cell-code}
power_results <- power_analysis(
  conditions = conditions,
  n_cores = n_cores,
  n_simulations = n_sims,
  verbose = TRUE,
  brms_args = list(
    chains = 4,
    iter = 700,
    warmup = 200
  )
)
```

::: {.cell-output .cell-output-stdout}

```

=== Power Analysis ===
Total simulations: 2000 
Total conditions to test: 2 
Conditions:
# A tibble: 2 × 3
  id_cond n_total b_arm_treat
    <int>   <dbl>       <dbl>
1       1     300         0  
2       2     300         0.3

Number of simulations per condition: 1000 
Number of total simulations : 2000 
Parallel cores: 14 

Total analysis time: 2.01 minutes
```


:::

```{.r .cell-code}
power_results$results_df
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 2 × 30
  id_cond n_total b_arm_treat parameter    threshold_success threshold_futility
    <dbl>   <dbl>       <dbl> <named list>             <dbl>              <dbl>
1       1     300         0   <chr [1]>                  0.1                  0
2       2     300         0.3 <chr [1]>                  0.1                  0
# ℹ 24 more variables: success_prob <dbl>, success_prob_mcse <dbl>,
#   futility_prob <dbl>, futility_prob_mcse <dbl>, success_power <dbl>,
#   success_power_mcse <dbl>, futility_power <dbl>, futility_power_mcse <dbl>,
#   est_median <dbl>, est_median_mcse <dbl>, est_mad <dbl>, est_mad_mcse <dbl>,
#   est_mean <dbl>, est_mean_mcse <dbl>, est_sd <dbl>, est_sd_mcse <dbl>,
#   rhat <dbl>, rhat_mcse <dbl>, ess_bulk <dbl>, ess_bulk_mcse <dbl>,
#   ess_tail <dbl>, ess_tail_mcse <dbl>, convergence_rate <dbl>, …
```


:::
:::




***


# Session Information


::: {.cell}

```{.r .cell-code}
sessionInfo()
```

::: {.cell-output .cell-output-stdout}

```
R version 4.5.0 (2025-04-11 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26100)

Matrix products: default
  LAPACK version 3.12.1

locale:
[1] LC_COLLATE=German_Germany.utf8  LC_CTYPE=German_Germany.utf8   
[3] LC_MONETARY=German_Germany.utf8 LC_NUMERIC=C                   
[5] LC_TIME=German_Germany.utf8    

time zone: Europe/Berlin
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] rctbayespower_0.1.0 devtools_2.4.5      usethis_3.1.0      
 [4] lubridate_1.9.4     forcats_1.0.0       stringr_1.5.1      
 [7] dplyr_1.1.4         purrr_1.0.4         readr_2.1.5        
[10] tidyr_1.3.1         tibble_3.3.0        ggplot2_3.5.2      
[13] tidyverse_2.0.0    

loaded via a namespace (and not attached):
 [1] tidyselect_1.2.1     farver_2.1.2         loo_2.8.0           
 [4] fastmap_1.2.0        tensorA_0.36.2.1     promises_1.3.3      
 [7] digest_0.6.37        timechange_0.3.0     mime_0.13           
[10] lifecycle_1.0.4      StanHeaders_2.32.10  ellipsis_0.3.2      
[13] processx_3.8.6       magrittr_2.0.3       posterior_1.6.1     
[16] compiler_4.5.0       rlang_1.1.6          tools_4.5.0         
[19] yaml_2.3.10          knitr_1.50           bridgesampling_1.1-2
[22] htmlwidgets_1.6.4    pkgbuild_1.4.8       here_1.0.1          
[25] RColorBrewer_1.1-3   pkgload_1.4.0        abind_1.4-8         
[28] miniUI_0.1.2         withr_3.0.2          grid_4.5.0          
[31] stats4_4.5.0         urlchecker_1.0.1     profvis_0.4.0       
[34] xtable_1.8-4         inline_0.3.21        scales_1.4.0        
[37] cli_3.6.5            mvtnorm_1.3-3        rmarkdown_2.29      
[40] generics_0.1.4       remotes_2.5.0        RcppParallel_5.1.10 
[43] rstudioapi_0.17.1    tzdb_0.5.0           sessioninfo_1.2.3   
[46] cachem_1.1.0         rstan_2.32.7         bayesplot_1.13.0    
[49] parallel_4.5.0       matrixStats_1.5.0    brms_2.22.0         
[52] vctrs_0.6.5          Matrix_1.7-3         jsonlite_2.0.0      
[55] callr_3.7.6          hms_1.1.3            glue_1.8.0          
[58] ps_1.9.1             codetools_0.2-20     distributional_0.5.0
[61] stringi_1.8.7        gtable_0.3.6         QuickJSR_1.8.0      
[64] later_1.4.2          pillar_1.10.2        htmltools_0.5.8.1   
[67] Brobdingnag_1.2-9    R6_2.6.1             rprojroot_2.0.4     
[70] evaluate_1.0.4       shiny_1.11.0         lattice_0.22-7      
[73] backports_1.5.0      memoise_2.0.1        httpuv_1.6.16       
[76] rstantools_2.4.0     Rcpp_1.0.14          gridExtra_2.3       
[79] coda_0.19-4.1        nlme_3.1-168         checkmate_2.3.2     
[82] xfun_0.52            fs_1.6.6             pkgconfig_2.0.3     
```


:::
:::


