# Competitor Analysis: rctbayespower

*Last updated: 2026-02-16*

## 1. Executive Summary

The landscape of free, open-source tools for Bayesian power analysis of RCTs is fragmented. No single package offers a complete workflow from Bayesian power calculation through group sequential monitoring to automated design optimization. Existing tools tend to be either (a) Bayesian but without power analysis focus (RBesT, bayesDP), (b) power-focused but frequentist (gsDesign, rpact, pwr), or (c) simulation-based but without Bayesian inference (Superpower, simr).

**rctbayespower** targets the unoccupied intersection: a dedicated Bayesian power analysis tool for RCTs with ROPE-based decision making, group sequential boundaries, dual estimation backends (MCMC + neural posterior estimation), and automated Bayesian optimization for design search. For psychological intervention trials specifically, the built-in ANCOVA models match the dominant analysis approach (pre-post designs with baseline adjustment), and the ROPE framework aligns with the field's growing emphasis on clinically meaningful effect sizes.

No competitor offers neural posterior estimation for power analysis, automated Pareto-optimal design search, or post-hoc boundary re-analysis without re-simulation.

---

## 2. Competitor Profiles

### 2.1 Direct: Bayesian Power / Sample Size (R)

#### bpp — Bayesian Predictive Power

- **Overview**: Computes and updates Bayesian predictive power following interim analyses in clinical trials. Focused on normally distributed endpoints with known variance, with a prominent use case being hazard ratios in Phase III time-to-event trials.
- **Key features**:
  - Sequential updates of predictive power after interim analyses
  - Blinded and unblinded interim analysis support
  - Prior-to-posterior updating of trial success likelihood
- **Estimation backend**: Analytical (closed-form normal calculations)
- **Optimization method**: None (user specifies sample size manually)
- **Strengths**: Simple, focused tool; maintained by Roche statisticians (Rufibach, Jordan, Abt); lightweight (only depends on mvtnorm)
- **Limitations**: Only normal endpoints with known variance; no ROPE framework; no design optimization; no group sequential boundary functions; primarily designed for updating ongoing trials, not prospective design
- **Psych relevance**: Low — designed for Phase III pivotal trials with hazard ratios; the known-variance assumption is rarely appropriate for psychological outcome scales
- **Status**: CRAN v1.0.6, published 2025-02-22, actively maintained

#### BayesPPD — Bayesian Power Prior Design

- **Overview**: Calculates Bayesian power and Type I error for generalized linear models using power priors and normalized power priors. Designed for trial design with historical data incorporation.
- **Key features**:
  - Power and Type I error calculation via simulation
  - Power prior and normalized power prior for historical data
  - Supports GLMs (normal, binary, Poisson, exponential outcomes)
  - Companion package BayesPPDSurv for survival endpoints
- **Estimation backend**: MCMC via custom C++ (Rcpp/RcppArmadillo)
- **Optimization method**: None (user runs at specified sample sizes)
- **Strengths**: Rigorous power prior methodology (Ibrahim et al.); multiple outcome types; handles historical data formally; published in R Journal
- **Limitations**: No group sequential designs; no ROPE framework; no automated optimization; slower MCMC computation; no ANCOVA models specifically
- **Psych relevance**: Medium — supports normal and binary outcomes which are relevant; power prior could incorporate results from previous therapy trials; but no ANCOVA covariate adjustment
- **Status**: CRAN v1.1.3, published 2025-01-13, actively maintained

#### BayesianPower — Sample Size for Bayes Factor Testing

- **Overview**: Determines required sample sizes for evaluating inequality-constrained hypotheses using Bayes factors. Also computes unconditional and conditional error probabilities.
- **Key features**:
  - Sample size for Bayes factor hypothesis tests
  - Inequality-constrained hypotheses (e.g., H1: mu_1 > mu_2 > mu_3)
  - Error probability calculations
- **Estimation backend**: Analytical / numerical integration
- **Optimization method**: Analytical inversion (solves for sample size given target Bayes factor)
- **Strengths**: Principled Bayes factor framework; handles ordered constraints (common in dose-response studies); solves directly for sample size
- **Limitations**: Bayes factor paradigm (not posterior probability / ROPE); limited to specific constraint types; last updated 2020 — possibly unmaintained; no sequential designs; no simulation of trial data
- **Psych relevance**: Low-Medium — Bayes factors are used in psychology (Wagenmakers school), but the inequality-constraint focus is niche; does not support typical two-arm RCT designs common in therapy research
- **Status**: CRAN v0.2.3, published 2020-06-22, likely unmaintained (5+ years without update)

#### gsbDesign — Group Sequential Bayesian Design

- **Overview**: Computes operating characteristics for Bayesian two-arm group sequential trials with known sigma and normal endpoints. The closest direct competitor to rctbayespower's group sequential capabilities.
- **Key features**:
  - Group sequential Bayesian design with multiple interim analyses
  - Success/futility stopping based on posterior probabilities
  - Operating characteristics via numerical integration
  - Customizable prior distributions
  - Depends on gsDesign for boundary specifications
- **Estimation backend**: Numerical integration (not MCMC — uses closed-form posterior updates for normal-normal conjugate model)
- **Optimization method**: None (user specifies design parameters; evaluates operating characteristics at given design)
- **Strengths**: Fast computation via numerical integration (no MCMC needed); rigorous methodology (published in JSS); combines Bayesian posterior decisions with group sequential structure
- **Limitations**: Only normal endpoints with known sigma; only two-arm designs; no ROPE framework (uses posterior probability of H0); no covariate adjustment (no ANCOVA); no automated design search; no multiple boundary functions with post-hoc comparison
- **Psych relevance**: Medium — normal endpoints are relevant for symptom scales; group sequential design is useful for large psych RCTs; but the known-sigma assumption and lack of ANCOVA limit practical applicability
- **Status**: CRAN v1.0-3, published 2024-01-28, sporadically maintained

---

### 2.2 Adjacent: Bayesian Adaptive Trial Design (R)

#### bayesCT — Simulation of Adaptive Bayesian Trials

- **Overview**: Simulated adaptive Bayesian clinical trials with early stopping for binomial, Gaussian, and time-to-event endpoints. Was the most feature-complete Bayesian trial simulation package in R.
- **Key features**:
  - Adaptive Bayesian trial simulation
  - Gaussian, binomial, and survival endpoints
  - Historical data incorporation
  - Early stopping for efficacy and futility
  - Interim analyses
- **Estimation backend**: Simulation-based with conjugate Bayesian updating
- **Optimization method**: None (user specifies designs)
- **Strengths**: Multiple endpoint types; adaptive design support; clean API
- **Limitations**: **Archived from CRAN** (2025-04-01, maintainer unreachable); no ROPE framework; no automated optimization; no ANCOVA models; no neural inference
- **Psych relevance**: Medium — Gaussian and binary endpoints match psych trial needs; but archived status makes it unsuitable for new projects
- **Status**: **ARCHIVED** from CRAN 2025-04-01

#### BayesCTDesign — Two-Arm Bayesian Trial Design with Historical Controls

- **Overview**: Power and sample size calculations for two-arm Bayesian randomized trials, with and without historical control data, via simulation.
- **Key features**:
  - Simulation-based power estimation at specified sample sizes
  - Historical control incorporation with assessment of historical-vs-randomized control drift
  - Six outcome types: Gaussian, Poisson, Bernoulli, Lognormal, Weibull, Piecewise Exponential
  - Visualization of power across sample sizes
- **Estimation backend**: Simulation-based (Monte Carlo)
- **Optimization method**: Grid evaluation (`simple_sim()` evaluates at user-specified sample sizes)
- **Strengths**: Broad outcome type support; explicit handling of historical control discrepancy; published in JSS (2021); straightforward API
- **Limitations**: Only two-arm designs; no group sequential / interim analyses; no ROPE framework; no automated optimization; no ANCOVA; not actively maintained (last update 2021)
- **Psych relevance**: Medium — Gaussian and Bernoulli outcomes apply; historical control data from prior therapy trials could be incorporated; but lack of ANCOVA and group sequential limits utility for typical psych RCTs
- **Status**: CRAN v0.6.1, published 2021-11-30, likely inactive

#### bayesDP — Bayesian Discount Prior

- **Overview**: Data augmentation using Bayesian discount priors for single-arm and two-arm clinical trials. Developed with the Medical Device Innovation Consortium (MDIC).
- **Key features**:
  - Discount function for down-weighting historical data
  - Binomial, normal, linear regression, and survival outcomes
  - Single-arm and two-arm trial support
  - Posterior inference with discounted historical prior
- **Estimation backend**: MCMC (via MCMCpack and custom Rcpp)
- **Optimization method**: None (analysis tool, not design tool)
- **Strengths**: Principled historical data borrowing; regulatory engagement (MDIC); multiple outcome types; actively maintained
- **Limitations**: Analysis-focused, not design/power-focused; no power calculations built in; no group sequential; no ROPE; no optimization; requires users to write their own simulation wrapper for power analysis
- **Psych relevance**: Low — useful for incorporating historical data but does not directly address power analysis or trial design questions
- **Status**: CRAN v1.3.7, published 2025-01-12, actively maintained

#### RBesT — R Bayesian Evidence Synthesis Tools (Novartis)

- **Overview**: Bayesian evidence synthesis toolkit for meta-analysis, robust prior derivation from historical data, and operating characteristics evaluation. Industry-backed by Novartis.
- **Key features**:
  - Meta-Analytic-Predictive (MAP) priors from historical data
  - Mixture priors for robustification
  - Operating characteristics via simulation
  - One-sample and two-sample designs
  - Normal, binary, and Poisson endpoints
- **Estimation backend**: MCMC via Stan (rstan)
- **Optimization method**: Grid-based operating characteristics (evaluates at user-specified scenarios)
- **Strengths**: Industry standard (Novartis); rigorous MAP prior methodology (Neuenschwander, Schmidli); well-documented; actively maintained; regulatory credibility
- **Limitations**: Focused on prior derivation and evidence synthesis, not full power analysis workflow; no group sequential designs; no ROPE framework; no automated optimization; no ANCOVA models
- **Psych relevance**: Medium — MAP priors could synthesize evidence from prior therapy trials to inform new designs; normal and binary endpoints are relevant; but requires users to build their own power analysis pipeline around it
- **Status**: CRAN v1.8-2, published 2025-04-25, actively maintained by Novartis

---

### 2.3 Frequentist Power / Sequential Design (R)

#### gsDesign — Group Sequential Design (Merck)

- **Overview**: The industry-standard R package for deriving and evaluating frequentist group sequential clinical trial designs. Supports time-to-event, binary, and continuous outcomes with various spending functions.
- **Key features**:
  - Alpha and beta spending functions (Lan-DeMets, O'Brien-Fleming, Pocock, etc.)
  - Sample size calculation with group sequential adjustment
  - Flexible boundary specifications
  - Continuous, binary, and time-to-event endpoints
  - Extensive boundary plotting and tabulation
- **Estimation backend**: Analytical (numerical integration of multivariate normal distribution)
- **Optimization method**: Analytical inversion (solves for sample size given power, alpha, boundary shape)
- **Strengths**: Industry standard; regulatory acceptance (FDA); extensive documentation; actively maintained by Merck; fast analytical computation; mature (20+ years of development)
- **Limitations**: Purely frequentist paradigm; no Bayesian posterior inference; no ROPE; no prior specification; no ANCOVA adjustment; no automated multi-objective optimization
- **Psych relevance**: Medium-High — group sequential designs are increasingly used in large psych RCTs (e.g., NIMH-funded trials); continuous and binary endpoints match psych outcomes; but frequentist framework limits Bayesian analysis
- **Status**: CRAN v3.9.0, published 2026-02-15, actively maintained

#### rpact — Confirmatory Adaptive Clinical Trial Design and Analysis

- **Overview**: Comprehensive package for designing and analyzing confirmatory adaptive clinical trials. Implements group sequential, multi-stage adaptive, and combination testing methods.
- **Key features**:
  - Group sequential and adaptive multi-stage designs
  - Continuous, binary, and survival endpoints
  - Sample size recalculation
  - Combination testing (inverse normal, Fisher combination)
  - Conditional power and predictive power
  - Rich plotting and reporting tools
- **Estimation backend**: Analytical (numerical integration)
- **Optimization method**: Analytical inversion (solves for sample size); conditional/predictive power for adaptive re-sizing
- **Strengths**: Most comprehensive frequentist adaptive design package; well-documented (textbook by Wassmer & Brannath); active commercial support (rpact.com); LGPL license; handles complex adaptive designs
- **Limitations**: Frequentist paradigm; no Bayesian inference; no ROPE; no prior specification; no ANCOVA; limited to confirmatory (not exploratory) designs
- **Psych relevance**: Medium — adaptive designs with sample size re-estimation are useful for psych trials where effect sizes are uncertain; continuous and binary endpoints apply; but frequentist framework
- **Status**: CRAN v4.3.0, published 2025-12-16, actively maintained

#### pwr — Basic Functions for Power Analysis

- **Overview**: Classical power analysis implementing Cohen's (1988) methods. The most widely-used R power package, providing simple functions for common statistical tests.
- **Key features**:
  - t-tests (one-sample, two-sample, paired)
  - ANOVA (one-way)
  - Chi-squared tests
  - Correlation tests
  - Proportions tests
  - Cohen's effect size conventions (small, medium, large)
- **Estimation backend**: Analytical (closed-form formulas from Cohen 1988)
- **Optimization method**: Analytical inversion (solves for any one unknown parameter: n, power, effect size, or alpha)
- **Strengths**: Simple, widely known; Cohen's effect size conventions are standard in psychology; fast; lightweight; no dependencies
- **Limitations**: Only simple designs (no ANCOVA, no mixed models, no sequential); frequentist only; no Bayesian methods; no simulation; does not handle realistic trial complexity
- **Psych relevance**: High (for simple designs) — the default power tool in psychology; Cohen's d is the standard effect size; but limited to simple tests that don't match modern psych RCT analysis (ANCOVA is standard)
- **Status**: CRAN v1.3-0, published 2020-03-17, sporadically maintained

#### Superpower — Simulation-Based Power for Factorial Designs

- **Overview**: Simulation-based power analysis for ANOVA designs with up to three factors. Calculates observed power for all main effects, interactions, and pairwise comparisons. Popular in psychology research.
- **Key features**:
  - Factorial ANOVA designs (up to 3 factors, between/within/mixed)
  - Simulation-based and analytical power calculations
  - Power curves across effect sizes or sample sizes
  - Emmeans-based comparisons
  - ANCOVA support (via vignettes)
  - Effect size calculations
- **Estimation backend**: Simulation-based (Monte Carlo ANOVA) + analytical (via afex)
- **Optimization method**: Power curves across parameter ranges (grid evaluation)
- **Strengths**: Designed for psychologists; handles complex factorial designs; well-documented with multiple vignettes; published methodology (Lakens & Caldwell 2021); ANCOVA support
- **Limitations**: Frequentist only; no Bayesian methods; limited to ANOVA/ANCOVA framework (no GLM, no mixed models for repeated measures); no sequential designs; no group sequential boundaries; no automated optimization beyond grid
- **Psych relevance**: Very High — designed specifically for psychology researchers; factorial designs are common in intervention research (e.g., treatment × gender); ANCOVA support matches psych RCT analysis; Cohen's effect sizes throughout
- **Status**: CRAN v0.2.4.1, published 2025-08-22, actively maintained

#### simr — Power Analysis for GLMMs by Simulation

- **Overview**: Simulation-based power analysis for generalized linear mixed models fitted with lme4. Designed for complex designs with random effects (e.g., clustered, longitudinal, multi-site trials).
- **Key features**:
  - Power for any GLMM (linear, logistic, Poisson, etc.)
  - Simulation from fitted lme4 models
  - Extend/modify existing models (change effect sizes, add observations)
  - Power curves over parameters
  - Confidence intervals on power estimates
- **Estimation backend**: Simulation-based (Monte Carlo from lme4 models)
- **Optimization method**: Power curves (grid evaluation over sample sizes or effect sizes)
- **Strengths**: Handles complex random-effects structures (sites, therapists, repeated measures); works with any lme4-compatible model; published methodology (Green & MacLeod 2016); flexible model specification
- **Limitations**: Frequentist only; no Bayesian methods; slow for complex models (each simulation requires model fitting); no sequential designs; no automated optimization; no ROPE
- **Psych relevance**: Very High — multilevel/mixed models are standard for psych RCTs (patients nested in therapists/sites, repeated measures); handles clustering that simpler tools miss; but frequentist inference
- **Status**: CRAN v1.0.8, published 2025-08-18, actively maintained

---

### 2.4 Python Packages

#### statsmodels.stats.power — Frequentist Power Analysis

- **Overview**: Power analysis module within the statsmodels Python library. Provides analytical power calculations for common statistical tests.
- **Key features**:
  - t-tests (independent, paired)
  - F-test / ANOVA
  - Chi-squared goodness-of-fit
  - Normal distribution tests (z-tests)
  - Solve for any parameter (sample size, power, effect size, alpha)
- **Estimation backend**: Analytical (closed-form formulas)
- **Optimization method**: Analytical inversion (root-finding for sample size)
- **Strengths**: Part of the large statsmodels ecosystem; Python-native; well-tested; fast analytical calculations
- **Limitations**: Frequentist only; limited test types (no ANCOVA, no mixed models, no GLM); no Bayesian methods; no sequential designs; no simulation; Python-only (not integrated with R clinical trial ecosystem)
- **Psych relevance**: Low-Medium — Python equivalent of `pwr`; same limitation of only simple tests; psychology research predominantly uses R
- **Status**: PyPI v0.14.6, published 2025-12-05, actively maintained

#### bambi — BAyesian Model-Building Interface

- **Overview**: High-level Bayesian model-building interface for Python, wrapping PyMC. Designed for fitting Bayesian mixed-effects models common in social sciences and biology using formula syntax.
- **Key features**:
  - R-style formula interface (`y ~ x1 + (1|group)`)
  - Automatic prior specification
  - Gaussian, Bernoulli, Poisson, and other families
  - Mixed-effects models (random intercepts/slopes)
  - Integration with ArviZ for diagnostics
- **Estimation backend**: MCMC via PyMC (NUTS sampler)
- **Optimization method**: None (model fitting tool, not power analysis tool)
- **Strengths**: User-friendly formula syntax familiar to R users; automatic priors; well-documented; published in JSS (2022); designed for social science models
- **Limitations**: **No power analysis functionality** — purely a model-fitting tool; users would need to write their own simulation wrapper; Python-only; slower than analytical methods; no sequential designs; no optimization
- **Psych relevance**: Medium — excellent for Bayesian analysis of psych data, but provides no power analysis tools; could theoretically be used for simulation-based power by wrapping model fits, but this is not built in
- **Status**: PyPI, actively maintained, Python 3.11+

---

## 3. Feature Comparison Matrix

| Feature | rctbayespower | bpp | BayesPPD | BayesianPower | gsbDesign | bayesCT | BayesCTDesign | bayesDP | RBesT | gsDesign | rpact | pwr | Superpower | simr | statsmodels | bambi |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| **Framework** | | | | | | | | | | | | | | | | |
| Bayesian | Yes | Yes | Yes | Yes | Yes | Yes | Yes | Yes | Yes | No | No | No | No | No | No | Yes |
| Frequentist | No | No | No | No | No | No | No | No | No | Yes | Yes | Yes | Yes | Yes | Yes | No |
| ROPE decisions | Yes | No | No | No | No | No | No | No | No | No | No | No | No | No | No | No |
| Bayes factors | No | No | No | Yes | No | No | No | No | No | No | No | No | No | No | No | No |
| **Sequential** | | | | | | | | | | | | | | | | |
| Group sequential | Yes | No | No | No | Yes | Yes | No | No | No | Yes | Yes | No | No | No | No | No |
| Interim analyses | Yes | Yes | No | No | Yes | Yes | No | No | No | Yes | Yes | No | No | No | No | No |
| Look-dependent boundaries | Yes | No | No | No | No | No | No | No | No | Yes | Yes | No | No | No | No | No |
| Post-hoc boundary re-analysis | Yes | No | No | No | No | No | No | No | No | No | No | No | No | No | No | No |
| **Computation** | | | | | | | | | | | | | | | | |
| MCMC | Yes (brms/Stan) | No | Yes (Rcpp) | No | No | No | No | Yes (MCMCpack) | Yes (Stan) | No | No | No | No | No | No | Yes (PyMC) |
| Simulation-based | Yes | No | Yes | No | No | Yes | Yes | No | Yes | No | No | No | Yes | Yes | No | No |
| Analytical | No | Yes | No | Yes | Yes | No | No | No | No | Yes | Yes | Yes | Yes | No | Yes | No |
| Neural posterior estimation | Yes (BayesFlow) | No | No | No | No | No | No | No | No | No | No | No | No | No | No | No |
| GPU acceleration | Yes (planned) | No | No | No | No | No | No | No | No | No | No | No | No | No | No | No |
| Parallelization | Yes | No | No | No | No | No | No | No | No | No | No | No | No | No | No | No |
| **Optimization** | | | | | | | | | | | | | | | | |
| Automated sample size search | Yes | No | No | Yes | No | No | No | No | No | Yes | Yes | Yes | No | No | Yes | No |
| Bayesian optimization | Yes (mlr3mbo) | No | No | No | No | No | No | No | No | No | No | No | No | No | No | No |
| Pareto optimization | Yes | No | No | No | No | No | No | No | No | No | No | No | No | No | No | No |
| Progressive fidelity | Yes | No | No | No | No | No | No | No | No | No | No | No | No | No | No | No |
| **Outcomes** | | | | | | | | | | | | | | | | |
| Continuous | Yes | Yes | Yes | Yes | Yes | Yes | Yes | Yes | Yes | Yes | Yes | Yes | Yes | Yes | Yes | Yes |
| Binary | No | No | Yes | No | No | Yes | Yes | Yes | Yes | Yes | Yes | Yes | No | Yes | No | Yes |
| Survival | No | No | No | No | No | Yes | Yes | Yes | No | Yes | Yes | No | No | No | No | No |
| Count | No | No | Yes | No | No | No | Yes | No | Yes | No | No | No | No | Yes | No | Yes |
| **Models** | | | | | | | | | | | | | | | | |
| ANCOVA | Yes | No | No | No | No | No | No | No | No | No | No | No | Yes | No | No | No |
| t-test | No | No | No | No | Yes | No | No | No | No | No | Yes | Yes | Yes | No | Yes | No |
| Mixed models | No | No | No | No | No | No | No | No | No | No | No | No | No | Yes | No | Yes |
| GLM | No | No | Yes | No | No | No | Yes | Yes | No | No | No | No | No | Yes | No | Yes |
| **Historical data** | | | | | | | | | | | | | | | | |
| Power priors | No | No | Yes | No | No | No | No | No | No | No | No | No | No | No | No | No |
| MAP priors | No | No | No | No | No | No | No | No | Yes | No | No | No | No | No | No | No |
| Discount prior | No | No | No | No | No | No | Yes | Yes | No | No | No | No | No | No | No | No |
| Informative priors | Yes | Yes | No | Yes | Yes | Yes | No | No | Yes | No | No | No | No | No | No | Yes |
| **Psych relevance** | | | | | | | | | | | | | | | | |
| Factorial designs | No | No | No | No | No | No | No | No | No | No | No | No | Yes | No | No | No |
| Repeated measures | No | No | No | No | No | No | No | No | No | No | No | No | Yes | Yes | No | Yes |
| Cohen's d effect sizes | No | No | No | No | No | No | No | No | No | No | No | Yes | Yes | No | Yes | No |
| **Usability** | | | | | | | | | | | | | | | | |
| Language | R | R | R | R | R | R | R | R | R | R | R | R | R | R | Python | Python |
| CRAN/PyPI | No | Yes | Yes | Yes | Yes | Archived | Yes | Yes | Yes | Yes | Yes | Yes | Yes | Yes | Yes | Yes |
| Active maintenance | Yes | Yes | Yes | No | Sporadic | No | No | Yes | Yes | Yes | Yes | Sporadic | Yes | Yes | Yes | Yes |

---

## 4. Unique Value Propositions

Derived from the feature comparison matrix — these are capabilities where rctbayespower has features that **no or very few** competitors match:

### 4.1 Neural Posterior Estimation for Power Analysis

**rctbayespower is the only tool in this landscape that uses neural posterior estimation (BayesFlow) for power analysis.** All Bayesian competitors use either MCMC (BayesPPD, bayesDP, RBesT, bambi) or analytical conjugate models (bpp, gsbDesign). The BayesFlow backend enables 100-1000x speedup over MCMC once models are trained, making large-scale design exploration feasible.

*No competitor offers this.*

### 4.2 Bayesian Optimization for Trial Design

**rctbayespower is the only tool offering automated Bayesian optimization (via mlr3mbo) for trial design search, including Pareto-optimal design discovery and progressive fidelity.** All competitors require manual specification of design parameters or simple grid evaluation. The closest alternative is analytical inversion in gsDesign/rpact/pwr, but these only solve for a single parameter (sample size) given fixed values of all others — they cannot jointly optimize multiple design parameters or trade off competing objectives.

*No competitor offers this.*

### 4.3 ROPE-Based Decision Framework

**rctbayespower is the only power analysis tool implementing Region of Practical Equivalence (ROPE) decisions.** All other Bayesian tools use either posterior probability of a point null (gsbDesign, bayesCT) or Bayes factors (BayesianPower). Frequentist tools use p-value thresholds. The ROPE framework is more clinically meaningful for psychological interventions, where researchers care about whether an effect exceeds a minimally important difference on validated scales (e.g., 3 points on the PHQ-9).

*No competitor offers this.*

### 4.4 Post-Hoc Boundary Re-Analysis

**rctbayespower is the only tool that stores full posterior probability trajectories, enabling comparison of different stopping boundaries without re-running simulations.** Changing the stopping rule in gsDesign or rpact requires re-computing the design from scratch. In gsbDesign, changing boundaries requires re-running the numerical integration. In rctbayespower, users can apply different boundary functions (O'Brien-Fleming, Pocock, linear, power family) to existing simulation results instantly.

*No competitor offers this.*

### 4.5 Dual Backend Architecture

**rctbayespower is the only tool offering interchangeable estimation backends** — users can switch between brms/Stan (MCMC) and BayesFlow (neural posterior estimation) without changing their analysis code. This allows validation (run both, compare results) and flexibility (use MCMC for small explorations, BayesFlow for large-scale optimization).

*No competitor offers this.*

### 4.6 Bayesian ANCOVA Power with Group Sequential Monitoring

**rctbayespower is the only Bayesian tool that combines ANCOVA models with group sequential interim analyses.** gsbDesign offers Bayesian sequential but only for simple normal means (no covariates). Superpower offers ANCOVA power but is frequentist and non-sequential. No other tool combines all three: Bayesian inference + ANCOVA + group sequential.

*No competitor offers this combination.*

---

## 5. Gap Analysis

Gaps identified by comparing rctbayespower's current capabilities against what its target use case (Bayesian power analysis for psychological intervention RCTs) requires, benchmarked against what competitors already provide.

### High Priority

| Gap | Why it matters for psych RCTs | Competitors that have it |
|-----|-------------------------------|--------------------------|
| **Binary outcomes** | Remission (yes/no), response (yes/no), and clinical cutoffs are primary endpoints in many therapy trials | BayesPPD, bayesCT, BayesCTDesign, bayesDP, RBesT, gsDesign, rpact, pwr, simr, bambi |
| **Test coverage (0%)** | Cannot be trusted for real trial design without validated tests | Most mature packages have test suites |
| **CRAN publication** | Discoverability and credibility; researchers search CRAN Task Views | All non-archived competitors |

### Medium Priority

| Gap | Why it matters for psych RCTs | Competitors that have it |
|-----|-------------------------------|--------------------------|
| **Mixed-model / repeated measures support** | Longitudinal designs (pre-mid-post, weekly assessments) are standard in therapy trials | simr, bambi |
| **Cohen's d and standardized effect sizes** | Psychology uses Cohen's d universally for effect size communication and meta-analysis | pwr, Superpower, statsmodels |
| **Factorial designs** | Treatment x moderator designs (e.g., CBT vs. medication x severity level) are common | Superpower |
| **Historical data incorporation** | Psych interventions have substantial prior literature; formal borrowing could reduce sample sizes | BayesPPD, BayesCTDesign, bayesDP, RBesT |

### Lower Priority

| Gap | Why it matters for psych RCTs | Competitors that have it |
|-----|-------------------------------|--------------------------|
| **Cluster randomization** | Therapy delivered by therapists (clustering) or in group settings | simr (via random effects) |
| **Count outcomes** | Symptom counts, behavioral frequency measures | BayesPPD, BayesCTDesign, RBesT, simr, bambi |
| **Survival / time-to-event** | Relapse time, time to dropout — less common but used in substance use research | bayesCT, BayesCTDesign, bayesDP, gsDesign, rpact |
| **GUI / Shiny app** | Lower barrier for clinical researchers without R experience | None of the competitors have this either |

### Gaps That Are Strategic Non-Issues

| Non-gap | Rationale |
|---------|-----------|
| Frequentist methods | rctbayespower is deliberately Bayesian-only; users needing frequentist analysis use gsDesign/rpact/pwr |
| Bayes factors | Different decision paradigm; ROPE is the chosen framework |
| Power priors / MAP priors / discount priors | These are prior specification methods that could be used *upstream* of rctbayespower (e.g., derive a prior with RBesT, then pass it to rctbayespower) |
