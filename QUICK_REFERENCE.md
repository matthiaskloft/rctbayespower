# rctbayespower Quick Reference Card

## 🚀 Installation
```r
install.packages(c("brms", "bayestestR", "ggplot2", "dplyr"))
devtools::install_local("c:/Users/Matze/Documents/GitHub/rctbayespower")
library(rctbayespower)
```

## 📊 Main Functions

### Power Analysis
```r
power_analysis(n_control, n_treatment, effect_size, outcome_type, n_simulations)
```

### Sample Size Determination  
```r
sample_size_analysis(effect_size, target_power, outcome_type, sample_sizes)
```

### Power Curves
```r
bayesian_power_curve(n_control, n_treatment, effect_sizes, outcome_type)
```

### Data Simulation
```r
simulate_rct_data(n_control, n_treatment, effect_size, outcome_type)
```

### Visualization
```r
plot_power_curve(result)
```

## 🎯 Outcome Types
- `"continuous"` - Blood pressure, depression scores
- `"binary"` - Success/failure, cure/no cure  
- `"count"` - Hospital visits, adverse events

## 📈 Power Metrics
- **ROPE Power** - Outside practical equivalence region
- **Directional Power** - Effect in expected direction
- **Significance Power** - Traditional statistical significance

## 💡 Quick Examples

### Continuous Outcome
```r
result <- power_analysis(50, 50, 0.5, "continuous", 1000)
print(result)
```

### Binary Outcome
```r
result <- power_analysis(100, 100, 0.3, "binary", 500, baseline_prob = 0.2)
```

### With Covariates
```r
covs <- list(age = list(type = "continuous", mean = 50, sd = 10))
result <- power_analysis(50, 50, 0.4, "continuous", 500, covariates = covs)
```

### Sample Size for 80% Power
```r
ss <- sample_size_analysis(0.5, 0.8, "continuous", seq(20, 100, 10))
```

## 📚 Learning Resources
- `?power_analysis` - Function help
- `browseVignettes("rctbayespower")` - Detailed guides
- `INSTALLATION_GUIDE.md` - Setup instructions
- `vignettes/case-studies.Rmd` - Real examples

## ⚡ Performance Tips
- Start with 100-200 simulations for testing
- Use 1000+ simulations for final analysis
- Enable `parallel = TRUE` for large studies
- Monitor memory usage with big sample sizes

## 🔧 Troubleshooting
- **brms not found**: `install.packages("brms")`
- **Slow performance**: Reduce `n_simulations`
- **Memory issues**: Use `parallel = FALSE`
- **Convergence warnings**: Increase brms iterations

---
📄 **Full Documentation**: `FINAL_README.md` | 🏥 **Case Studies**: `vignettes/` | 🛠️ **Support**: GitHub Issues
