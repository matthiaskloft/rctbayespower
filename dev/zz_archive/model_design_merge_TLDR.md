# TLDR: Should We Merge Model + Design Classes?

## Answer: **NO** - Keep Them Separate ❌

## Current State
- **rctbp_design**: Tiny (5 properties) - just config for analysis
- **rctbp_model**: Large (11+ properties) - data generation + statistical model
- Design always contains a model via `design@model`

## Why It Looks Tempting to Merge
- Design is so minimal
- They're always used together
- Would simplify from `design@model@property` to `design@property`

## Why We SHOULDN'T Merge (Stronger Arguments)

### 1. Model Compilation is Expensive 💰
```r
# With separation: Compile once, use many times
model <- build_model("ancova_cont_2arms")  # 2 min compile
design1 <- build_design(model, target = "b_arm2")
design2 <- build_design(model, target = "b_covariate")  # Reuses model!

# If merged: Recompile every time
design1 <- build_merged("ancova_cont_2arms", ...)  # 2 min
design2 <- build_merged("ancova_cont_2arms", ...)  # 2 min AGAIN
```

### 2. Clean Separation of Concerns 🎯
- **Model** = WHAT can be simulated (data generation, Stan model)
- **Design** = HOW to analyze it (which params, what thresholds)

### 3. Future Flexibility 🚀
- Can analyze different parameters from same model
- Can use different thresholds with same model
- Can share models across projects
- Clean place to add interim analysis features

### 4. Follows Good Architecture 📐
Current structure respects Single Responsibility Principle:
- Model → Statistical modeling
- Design → Analysis configuration
- Conditions → Parameter values
- Power Analysis → Orchestration

## Better Alternative: Add Convenience

```r
# Allow this shortcut:
design <- build_design(
  model = "ancova_cont_2arms",  # Auto-builds model
  target_params = "b_arm2",
  ...
)
```

## Bottom Line

The minimal design class is a **feature, not a bug**. It's a clean abstraction that:
- Saves compilation time through model reuse
- Keeps concerns properly separated
- Provides flexibility for future features

**Don't merge them.**