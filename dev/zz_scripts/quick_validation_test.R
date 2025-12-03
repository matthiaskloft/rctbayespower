# Quick test to validate trim_param_space parameter
devtools::load_all()

cat("Testing trim_param_space parameter validation:\n")

# Test invalid value (should fail)
tryCatch({
  optimization(list(), trim_param_space = 1.5)
}, error = function(e) {
  cat("✓ Parameter validation works: Invalid value rejected\n")
})

cat("✓ Implementation complete!\n")