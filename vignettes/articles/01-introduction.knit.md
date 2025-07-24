---
title: "Getting Started with 'rctbayespower'"
author: 
 - name: Matthias Kloft
   orcid: 0000-0003-1845-6957
date: "2025-07-24"
format:
  html:
    toc: true
    number-sections: true
    theme: cosmo
    code-fold: false
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



## Model

Show predefined models available in the package:

::: {.cell}

```{.r .cell-code}
list_predefined_models()
```

::: {.cell-output .cell-output-stdout}

```
[1] "ancova_cont_2arms" "ancova_cont_3arms"
```


:::
:::



Get the predefined ANCOVA model for continuous outcomes with two arms:

::: {.cell}

```{.r .cell-code}
model_ancova <- build_model(predefined_model = "ancova_cont_2arms")
model_ancova@parameter_names_brms
```

::: {.cell-output .cell-output-stdout}

```
[1] "b_Intercept" "b_covariate" "b_arm2"     
```


:::
:::


## Design

Find possible target parameters in the model.

::: {.cell}

```{.r .cell-code}
model_ancova@parameter_names_brms
```

::: {.cell-output .cell-output-stdout}

```
[1] "b_Intercept" "b_covariate" "b_arm2"     
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

S7 Object of class: 'rctbp_design'
--------------------------------------------------

=== Model Specifications ===

Number of endpoints: 1 
Endpoint types: continuous 
Number of arms: 2 
Number of repeated measures: 0 
Parameter names - simulation function: n_total, n_arms, contrasts, p_alloc, intercept, b_arm_treat, b_covariate, sigma 
Parameter names - brms model: b_Intercept, b_covariate, b_arm2 

=== Design Specifications ===

Design name: NULL 
Target parameters: b_arm2 
Number of interim analyses: 0 
Thresholds for success: 0.1 
Thresholds for futility: 0 
Probability of success significance: 0.975 
Probability of futility significance: 0.5 
Parameter names - interim function: NULL 

=== 'brms' Model ===

 Family: gaussian 
  Links: mu = identity; sigma = identity 
Formula: outcome ~ 1 + covariate + arm 
   Data: mock_data_ancova (Number of observations: 20) 
  Draws: 1 chains, each with iter = 500; warmup = 250; thin = 1;
         total post-warmup draws = 250

Regression Coefficients:
          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
Intercept     0.48      0.25    -0.00     0.95 1.00      294      259
covariate    -0.06      0.18    -0.45     0.29 1.00      229      110
arm2         -0.32      0.37    -0.96     0.42 1.00      274      206

Further Distributional Parameters:
      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
sigma     0.98      0.18     0.69     1.38 1.00      161      129

Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
and Tail_ESS are effective sample size measures, and Rhat is the potential
scale reduction factor on split chains (at convergence, Rhat = 1).
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
    n_total = seq(80, 240, by = 40),
    # two effect sizes
    b_arm_treat = c(0, 0.3)
  ),
  static_values = list(
    # baseline effect
    b_covariate = 0
  )
)
print(conditions, n = 100)
```

::: {.cell-output .cell-output-stdout}

```

S7 Object of class: 'rctbp_conditions'
--------------------------------------------------

Number of conditions: 10 
Number of varying parameters: 2 
Number of static parameters: 1 

Condition Grid:
# A tibble: 10 × 3
   id_cond n_total b_arm_treat
     <int>   <dbl>       <dbl>
 1       1      80         0  
 2       2      80         0.3
 3       3     120         0  
 4       4     120         0.3
 5       5     160         0  
 6       6     160         0.3
 7       7     200         0  
 8       8     200         0.3
 9       9     240         0  
10      10     240         0.3
```


:::
:::



## Run Power Analysis

Set number of cores and simulations:

::: {.cell}

```{.r .cell-code}
n_cores <- parallel::detectCores() - 2
n_sims <- 1e3
```
:::


Run the analysis using the `power_analysis` function. This will run the model for each condition and return the results.

::: {.cell}

```{.r .cell-code}
power <- power_analysis(
  conditions = conditions,
  n_cores = n_cores,
  n_sims = n_sims,
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
Conditions:
# A tibble: 10 × 3
   id_cond n_total b_arm_treat
     <int>   <dbl>       <dbl>
 1       1      80         0  
 2       2      80         0.3
 3       3     120         0  
 4       4     120         0.3
 5       5     160         0  
 6       6     160         0.3
 7       7     200         0  
 8       8     200         0.3
 9       9     240         0  
10      10     240         0.3

Conditions to test: 10 
Simulations per condition: 1000 
Total simulations: 10000 

Parallel execution using 14 cores: 
Functions exported from package namespace
Combined results dimensions: 10000 18 
Column names: parameter, threshold_success, threshold_futility, success_prob, futility_prob, power_success, power_futility, median, mad, mean, sd, rhat, ess_bulk, ess_tail, id_sim, id_cond, converged, error 

Total analysis time: 12.09 minutes
```


:::
:::



## Power for Effect of 0.3

::: {.cell}

```{.r .cell-code}
power@summarized_results |> 
  dplyr::filter(b_arm_treat != 0) |> 
  select(
    n_total,
    b_arm_treat,
    prob_success,
    prob_success_se,
    prob_futility,
    prob_futility_se,
    power_success,
    power_success_se,
    power_futility,
    power_futility_se
  ) |>
  dplyr::arrange(desc(b_arm_treat)) |> 
    kableExtra::kable(digits = 3, 
                    format = "html")
```

::: {.cell-output-display}
`````{=html}
<table>
 <thead>
  <tr>
   <th style="text-align:right;"> n_total </th>
   <th style="text-align:right;"> b_arm_treat </th>
   <th style="text-align:right;"> prob_success </th>
   <th style="text-align:right;"> prob_success_se </th>
   <th style="text-align:right;"> prob_futility </th>
   <th style="text-align:right;"> prob_futility_se </th>
   <th style="text-align:right;"> power_success </th>
   <th style="text-align:right;"> power_success_se </th>
   <th style="text-align:right;"> power_futility </th>
   <th style="text-align:right;"> power_futility_se </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 0.733 </td>
   <td style="text-align:right;"> 0.008 </td>
   <td style="text-align:right;"> 0.172 </td>
   <td style="text-align:right;"> 0.006 </td>
   <td style="text-align:right;"> 0.115 </td>
   <td style="text-align:right;"> 0.010 </td>
   <td style="text-align:right;"> 0.080 </td>
   <td style="text-align:right;"> 0.009 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 120 </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 0.767 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 0.132 </td>
   <td style="text-align:right;"> 0.006 </td>
   <td style="text-align:right;"> 0.160 </td>
   <td style="text-align:right;"> 0.012 </td>
   <td style="text-align:right;"> 0.056 </td>
   <td style="text-align:right;"> 0.007 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 160 </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 0.810 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 0.091 </td>
   <td style="text-align:right;"> 0.004 </td>
   <td style="text-align:right;"> 0.210 </td>
   <td style="text-align:right;"> 0.013 </td>
   <td style="text-align:right;"> 0.033 </td>
   <td style="text-align:right;"> 0.006 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 200 </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 0.846 </td>
   <td style="text-align:right;"> 0.006 </td>
   <td style="text-align:right;"> 0.063 </td>
   <td style="text-align:right;"> 0.004 </td>
   <td style="text-align:right;"> 0.276 </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 0.013 </td>
   <td style="text-align:right;"> 0.004 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 240 </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 0.850 </td>
   <td style="text-align:right;"> 0.006 </td>
   <td style="text-align:right;"> 0.057 </td>
   <td style="text-align:right;"> 0.003 </td>
   <td style="text-align:right;"> 0.311 </td>
   <td style="text-align:right;"> 0.015 </td>
   <td style="text-align:right;"> 0.018 </td>
   <td style="text-align:right;"> 0.004 </td>
  </tr>
</tbody>
</table>

`````
:::
:::



## Alpha Error Rate for Null Effect 

The power for success indicates the alpha error rate for the null effect (b_arm_treat = 0). This is the probability of rejecting the null hypothesis when it is true, which should be close to the significance level (0.05) if the design is well specified.


::: {.cell}

```{.r .cell-code}
power@summarized_results |> 
  dplyr::filter(b_arm_treat == 0) |>
  select(
    n_total,
    b_arm_treat,
    prob_success,
    prob_success_se,
    prob_futility,
    prob_futility_se,
    power_success,
    power_success_se,
    power_futility,
    power_futility_se
  ) |>
  dplyr::arrange(desc(b_arm_treat)) |> 
  kableExtra::kable(digits = 3, 
                    format = "html")
```

::: {.cell-output-display}
`````{=html}
<table>
 <thead>
  <tr>
   <th style="text-align:right;"> n_total </th>
   <th style="text-align:right;"> b_arm_treat </th>
   <th style="text-align:right;"> prob_success </th>
   <th style="text-align:right;"> prob_success_se </th>
   <th style="text-align:right;"> prob_futility </th>
   <th style="text-align:right;"> prob_futility_se </th>
   <th style="text-align:right;"> power_success </th>
   <th style="text-align:right;"> power_success_se </th>
   <th style="text-align:right;"> power_futility </th>
   <th style="text-align:right;"> power_futility_se </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.366 </td>
   <td style="text-align:right;"> 0.008 </td>
   <td style="text-align:right;"> 0.505 </td>
   <td style="text-align:right;"> 0.009 </td>
   <td style="text-align:right;"> 0.003 </td>
   <td style="text-align:right;"> 0.002 </td>
   <td style="text-align:right;"> 0.509 </td>
   <td style="text-align:right;"> 0.016 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 120 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.334 </td>
   <td style="text-align:right;"> 0.008 </td>
   <td style="text-align:right;"> 0.515 </td>
   <td style="text-align:right;"> 0.009 </td>
   <td style="text-align:right;"> 0.006 </td>
   <td style="text-align:right;"> 0.002 </td>
   <td style="text-align:right;"> 0.543 </td>
   <td style="text-align:right;"> 0.016 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 160 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.313 </td>
   <td style="text-align:right;"> 0.008 </td>
   <td style="text-align:right;"> 0.517 </td>
   <td style="text-align:right;"> 0.009 </td>
   <td style="text-align:right;"> 0.005 </td>
   <td style="text-align:right;"> 0.002 </td>
   <td style="text-align:right;"> 0.546 </td>
   <td style="text-align:right;"> 0.016 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 200 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.310 </td>
   <td style="text-align:right;"> 0.008 </td>
   <td style="text-align:right;"> 0.498 </td>
   <td style="text-align:right;"> 0.009 </td>
   <td style="text-align:right;"> 0.003 </td>
   <td style="text-align:right;"> 0.002 </td>
   <td style="text-align:right;"> 0.497 </td>
   <td style="text-align:right;"> 0.016 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 240 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.284 </td>
   <td style="text-align:right;"> 0.008 </td>
   <td style="text-align:right;"> 0.506 </td>
   <td style="text-align:right;"> 0.009 </td>
   <td style="text-align:right;"> 0.001 </td>
   <td style="text-align:right;"> 0.001 </td>
   <td style="text-align:right;"> 0.508 </td>
   <td style="text-align:right;"> 0.016 </td>
  </tr>
</tbody>
</table>

`````
:::
:::


## Run Time


::: {.cell}

```{.r .cell-code  code-fold="true"}
n_runs <- nrow(power@conditions@conditions_grid) * power@n_sims
cat("Total run time:", round(power@elapsed_time,1), "minutes for", n_runs, "total simulation repetitions using", power@n_cores, "cores.\n")
```

::: {.cell-output .cell-output-stdout}

```
Total run time: 12.1 minutes for 10000 total simulation repetitions using 14 cores.
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
 [1] rctbayespower_0.0.0.9000 lubridate_1.9.4          forcats_1.0.0           
 [4] stringr_1.5.1            dplyr_1.1.4              purrr_1.0.4             
 [7] readr_2.1.5              tidyr_1.3.1              tibble_3.3.0            
[10] ggplot2_3.5.2            tidyverse_2.0.0         

loaded via a namespace (and not attached):
 [1] gtable_0.3.6         tensorA_0.36.2.1     xfun_0.52           
 [4] QuickJSR_1.8.0       inline_0.3.21        lattice_0.22-7      
 [7] tzdb_0.5.0           vctrs_0.6.5          tools_4.5.0         
[10] generics_0.1.4       stats4_4.5.0         parallel_4.5.0      
[13] pkgconfig_2.0.3      brms_2.22.0          Matrix_1.7-3        
[16] checkmate_2.3.2      RColorBrewer_1.1-3   S7_0.2.0            
[19] distributional_0.5.0 RcppParallel_5.1.10  lifecycle_1.0.4     
[22] compiler_4.5.0       farver_2.1.2         textshaping_1.0.1   
[25] Brobdingnag_1.2-9    codetools_0.2-20     htmltools_0.5.8.1   
[28] bayesplot_1.13.0     yaml_2.3.10          pillar_1.10.2       
[31] StanHeaders_2.32.10  bridgesampling_1.1-2 abind_1.4-8         
[34] nlme_3.1-168         posterior_1.6.1      rstan_2.32.7        
[37] tidyselect_1.2.1     digest_0.6.37        mvtnorm_1.3-3       
[40] stringi_1.8.7        reshape2_1.4.4       fastmap_1.2.0       
[43] grid_4.5.0           cli_3.6.5            magrittr_2.0.3      
[46] loo_2.8.0            pkgbuild_1.4.8       withr_3.0.2         
[49] scales_1.4.0         backports_1.5.0      timechange_0.3.0    
[52] rmarkdown_2.29       matrixStats_1.5.0    gridExtra_2.3       
[55] hms_1.1.3            kableExtra_1.4.0     pbapply_1.7-4       
[58] coda_0.19-4.1        evaluate_1.0.4       knitr_1.50          
[61] viridisLite_0.4.2    rstantools_2.4.0     rlang_1.1.6         
[64] Rcpp_1.0.14          glue_1.8.0           xml2_1.3.8          
[67] svglite_2.2.1        rstudioapi_0.17.1    jsonlite_2.0.0      
[70] plyr_1.8.9           R6_2.6.1             systemfonts_1.2.3   
```


:::
:::


