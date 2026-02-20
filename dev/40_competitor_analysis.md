# Competitor Analysis: rctbayespower

*Last updated: 2026-02-20*

## 1. Executive Summary

The landscape of free, open-source tools for Bayesian power analysis of RCTs is fragmented. No single package offers a complete workflow from Bayesian power calculation through group sequential monitoring to automated design optimization. Existing tools tend to be either (a) Bayesian but limited in scope (gsbDesign for normal endpoints only, BayesPPD for power priors), (b) power-focused but frequentist (gsDesign, rpact, pwr), or (c) simulation-based but without Bayesian inference (Superpower, simr).

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

#### BATSS — Bayesian Adaptive Trials Simulator Software

- **Overview**: Simulates Bayesian adaptive clinical trial designs and evaluates their operating characteristics. Uses INLA (Integrated Nested Laplace Approximation) for fitting Bayesian GLMs, supporting covariate adjustment and adaptive randomization. The most feature-rich actively maintained Bayesian adaptive trial simulation package on CRAN (bayesCT was archived in 2025).
- **Key features**:
  - Bayesian GLM via INLA for fast approximate posterior inference
  - Normal, binary, Poisson, negative binomial, and survival endpoints
  - Covariate adjustment (ANCOVA supported)
  - Treatment stopping rules (efficacy and futility)
  - Trial-wide stopping rules
  - Bayesian response-adaptive randomization
  - Parallel processing and cluster computing support
- **Estimation backend**: INLA (Integrated Nested Laplace Approximation) — a unique backend among competitors; faster than MCMC but approximate
- **Optimization method**: None (user specifies designs; evaluates operating characteristics via simulation)
- **Strengths**: Broad outcome support including ANCOVA; INLA is faster than full MCMC; adaptive randomization; actively maintained; parallelization; the only active Bayesian adaptive trial package on CRAN
- **Limitations**: No ROPE framework; no automated design optimization; no group sequential boundaries (uses fixed stopping rules); INLA requires separate installation (not on CRAN); no post-hoc boundary comparison; no neural inference
- **Psych relevance**: High — supports ANCOVA (standard for psych RCTs), binary outcomes (remission), and Bayesian inference; adaptive randomization could be useful for multi-arm psych trials comparing therapies; covariate adjustment for baseline severity
- **Status**: CRAN v1.1.1, published 2025-10-02, actively maintained

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

#### longpower — Power for Longitudinal Data

- **Overview**: Power and sample size calculations for linear models of longitudinal data, supporting mixed-effects models, generalized least squares, and generalized estimating equations.
- **Key features**:
  - Power for linear mixed-effects models with longitudinal data
  - GLS and GEE model support
  - Analytical formulas from Liu & Liang (1997) and Diggle et al. (2002)
  - Sample size for detecting slopes and rate-of-change differences
- **Estimation backend**: Analytical (closed-form formulas)
- **Optimization method**: Analytical inversion (solves for sample size given power targets)
- **Strengths**: Dedicated to longitudinal designs; well-established methodology; applicable to Alzheimer's and other repeated-measures clinical trials; lightweight
- **Limitations**: Only continuous outcomes; frequentist only; no Bayesian methods; limited to linear models (no GLMMs); no sequential designs; no ANCOVA; no automated multi-objective optimization
- **Psych relevance**: High — longitudinal mixed models are standard for therapy trials with weekly/monthly assessments; power for detecting treatment × time interactions is central to psych RCT design
- **Status**: CRAN v1.0.27, published 2024-09-05, actively maintained

#### PowerUpR — Power for Multilevel Randomized Experiments

- **Overview**: Calculates statistical power, minimum detectable effect size (MDES), and minimum required sample size for multilevel randomized experiments with continuous outcomes. Supports 14+ design types including cluster randomized and partially nested designs.
- **Key features**:
  - 14 designs for primary treatment effects in multilevel experiments
  - 7 designs for moderated treatment effects
  - 5 designs for mediated treatment effects
  - 4 partially nested designs
  - Power, MDES, and MDESD calculations
- **Estimation backend**: Analytical (closed-form formulas for multilevel designs)
- **Optimization method**: Analytical inversion (solves for sample size at each level, MDES, or MDESD)
- **Strengths**: Comprehensive coverage of multilevel/cluster designs; handles nested structures (patients in therapists in clinics); supports moderation and mediation power; calculates power at each clustering level
- **Limitations**: Only continuous outcomes; frequentist only; no Bayesian methods; no sequential designs; no ANCOVA; no simulation component; limited to balanced designs
- **Psych relevance**: High — cluster randomization is common in psych interventions (therapists, clinics, schools as clusters); partially nested designs (treatment group has therapist clustering, control does not) are a frequent psych RCT pattern
- **Status**: CRAN v1.1.0, published 2021-10-25, sporadically maintained

#### Mediana — Clinical Scenario Evaluation Framework

- **Overview**: A general framework for clinical trial simulations based on the Clinical Scenario Evaluation (CSE) approach. Decomposes trial design into data models, analysis models, and evaluation models, enabling power evaluation and design optimization across multiple endpoints and decision criteria.
- **Key features**:
  - Simulation-based power evaluation and sample size calculations
  - Continuous, binary, survival, count, and multivariate endpoints
  - Multiplicity adjustments for multiple endpoints/comparisons
  - Composite success criteria (e.g., disjunctive power, weighted criteria)
  - Adaptive designs and Go/No-Go decision frameworks
  - Confidence intervals on power estimates
  - Parallelization via doParallel/foreach
- **Estimation backend**: Simulation-based (Monte Carlo)
- **Optimization method**: Grid evaluation across scenarios; tradeoff-based optimization for multiplicity strategies
- **Strengths**: Very flexible framework handling multivariate endpoints and complex success criteria; multiplicity adjustments built in; published methodology (Dmitrienko & D'Agostino 2013); supports compound decision rules beyond simple power
- **Limitations**: Frequentist only; no Bayesian methods; no ROPE; no ANCOVA models; no group sequential boundaries; not recently updated (2019); steeper learning curve due to modular CSE framework
- **Psych relevance**: Medium — continuous and binary endpoints match psych trials; multiplicity adjustment useful for trials with multiple outcome scales (e.g., depression + anxiety + functioning); but the CSE framework is more complex than needed for typical psych RCT power analysis
- **Status**: CRAN v1.0.8, published 2019-05-08, not recently updated

---

### 2.4 Honorable Mentions

The following packages are relevant to the Bayesian clinical trial ecosystem but were excluded from the main analysis because they lack built-in power analysis / sample size planning features or are not directly applicable to RCTs.

- **bayesDP** (R, CRAN) — Bayesian discount prior for historical data incorporation. Analysis-focused tool for down-weighting historical controls; no power calculations or sample size planning built in. Could serve as an upstream prior derivation tool.
- **RBesT** (R, CRAN, Novartis) — Bayesian evidence synthesis and Meta-Analytic-Predictive (MAP) prior derivation. Industry-standard for synthesizing historical data into informative priors, but focused on prior derivation rather than power analysis workflows. Could complement rctbayespower by providing priors.
- **statsmodels.stats.power** (Python, PyPI) — Analytical power for t-tests, ANOVA, and chi-squared. Essentially the Python equivalent of `pwr`; not RCT-specific and lacks Bayesian methods, sequential designs, or ANCOVA support.
- **bambi** (Python, PyPI) — Bayesian model-building interface wrapping PyMC with R-style formula syntax. Excellent for fitting Bayesian mixed models but has no power analysis functionality; users would need to write custom simulation wrappers.
- **TrialSize** (R, CRAN) — 80+ frequentist sample size functions implementing Chow et al.'s textbook. Comprehensive but purely frequentist, primarily pharma-oriented, and covers many designs not relevant to psych RCTs.
- **simglm** (R, CRAN) — Simulation-based power for generalized linear mixed models, similar niche to simr but less focused on clinical trials. Last updated 2022.
- **adpss** (R, CRAN) — Adaptive sample size determination with frequentist efficiency guarantees. Specialized methodology for adaptive trials, not a general-purpose power analysis tool.
- **MAMS** (R, CRAN) — Multi-arm multi-stage frequentist design with power via `mams.sim()`. Normal endpoints with known variance only; multi-arm focus is niche for typical 2-arm psych RCTs. Actively maintained (v3.0.3, Aug 2025).

---

## 3. Feature Comparison Matrix

Only packages with explicit trial simulation (Monte Carlo simulation of individual trials) are included. Analytical-only tools (bpp, BayesianPower, gsbDesign, gsDesign, rpact, pwr, longpower, PowerUpR) are excluded.

| Feature | rctbayespower | BayesPPD | bayesCT | BayesCTDesign | BATSS | Superpower | simr | Mediana |
|---|---|---|---|---|---|---|---|---|
| **Framework** | | | | | | | | |
| Bayesian | Yes | Yes | Yes | Yes | Yes | No | No | No |
| Frequentist | No | No | No | No | No | Yes | Yes | Yes |
| ROPE decisions | Yes | No | No | No | No | No | No | No |
| Bayes factors | No | No | No | No | No | No | No | No |
| **Sequential** | | | | | | | | |
| Group sequential | Yes | No | Yes | No | No | No | No | No |
| Interim analyses | Yes | No | Yes | No | Yes | No | No | No |
| Look-dependent boundaries | Yes | No | No | No | No | No | No | No |
| Post-hoc boundary re-analysis | Yes | No | No | No | No | No | No | No |
| **Computation** | | | | | | | | |
| MCMC | Yes (brms/Stan) | Yes (Rcpp) | No | No | No | No | No | No |
| INLA | No | No | No | No | Yes | No | No | No |
| Simulation-based | Yes | Yes | Yes | Yes | Yes | Yes | Yes | Yes |
| Analytical | No | No | No | No | No | Yes | No | No |
| Neural posterior estimation | Yes (BayesFlow) | No | No | No | No | No | No | No |
| GPU acceleration | Yes (planned) | No | No | No | No | No | No | No |
| Parallelization | Yes | No | No | No | Yes | No | No | Yes |
| **Optimization** | | | | | | | | |
| Automated sample size search | Yes | No | No | No | No | No | No | No |
| Bayesian optimization | Yes (mlr3mbo) | No | No | No | No | No | No | No |
| Pareto optimization | Yes | No | No | No | No | No | No | No |
| Progressive fidelity | Yes | No | No | No | No | No | No | No |
| **Outcomes** | | | | | | | | |
| Continuous | Yes | Yes | Yes | Yes | Yes | Yes | Yes | Yes |
| Binary | No | Yes | Yes | Yes | Yes | No | Yes | Yes |
| Survival | No | No | Yes | Yes | Yes | No | No | Yes |
| Count | No | Yes | No | Yes | Yes | No | Yes | Yes |
| **Models** | | | | | | | | |
| ANCOVA | Yes | No | No | No | Yes | Yes | No | No |
| t-test | No | No | No | No | No | Yes | No | No |
| Mixed models | No | No | No | No | No | No | Yes | No |
| GLM | No | Yes | No | Yes | Yes | No | Yes | No |
| Multilevel / cluster | No | No | No | No | No | No | Yes | No |
| **Historical data** | | | | | | | | |
| Power priors | No | Yes | No | No | No | No | No | No |
| Discount prior | No | No | No | Yes | No | No | No | No |
| Informative priors | Yes | No | Yes | No | Yes | No | No | No |
| **Psych relevance** | | | | | | | | |
| Factorial designs | No | No | No | No | No | Yes | No | No |
| Repeated measures | No | No | No | No | No | Yes | Yes | No |
| Cohen's d effect sizes | No | No | No | No | No | Yes | No | No |
| **Usability** | | | | | | | | |
| Language | R | R | R | R | R | R | R | R |
| CRAN/PyPI | No | Yes | Archived | Yes | Yes | Yes | Yes | Yes |
| Active maintenance | Yes | Yes | No | No | Yes | Yes | Yes | No |

---

## 4. Unique Value Propositions

Derived from the feature comparison matrix — these are capabilities where rctbayespower has features that **no or very few** competitors match:

### 4.1 Neural Posterior Estimation for Power Analysis

**rctbayespower is the only tool in this landscape that uses neural posterior estimation (BayesFlow) for power analysis.** All Bayesian competitors use either MCMC (BayesPPD), INLA (BATSS), or analytical conjugate models (bpp, gsbDesign). The BayesFlow backend enables 100-1000x speedup over MCMC once models are trained, making large-scale design exploration feasible.

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

**rctbayespower is the only Bayesian tool that combines ANCOVA models with group sequential interim analyses and post-hoc boundary re-analysis.** BATSS also offers Bayesian ANCOVA with interim analyses, but uses a different framework (adaptive stopping rules via INLA rather than group sequential boundaries) and does not support post-hoc boundary re-analysis. gsbDesign offers Bayesian sequential but only for simple normal means (no covariates). Superpower offers ANCOVA power but is frequentist and non-sequential.

*BATSS is the closest competitor but lacks group sequential boundary functions (OBF, Pocock, etc.) and post-hoc boundary re-analysis.*

---

## 5. Gap Analysis

Gaps identified by comparing rctbayespower's current capabilities against what its target use case (Bayesian power analysis for psychological intervention RCTs) requires, benchmarked against what competitors already provide.

### High Priority

| Gap | Why it matters for psych RCTs | Competitors that have it |
|-----|-------------------------------|--------------------------|
| **Binary outcomes** | Remission (yes/no), response (yes/no), and clinical cutoffs are primary endpoints in many therapy trials | BayesPPD, bayesCT, BayesCTDesign, BATSS, gsDesign, rpact, pwr, simr, Mediana |
| **Test coverage (0%)** | Cannot be trusted for real trial design without validated tests | Most mature packages have test suites |
| **CRAN publication** | Discoverability and credibility; researchers search CRAN Task Views | All non-archived competitors |

### Medium Priority

| Gap | Why it matters for psych RCTs | Competitors that have it |
|-----|-------------------------------|--------------------------|
| **Mixed-model / repeated measures support** | Longitudinal designs (pre-mid-post, weekly assessments) are standard in therapy trials | simr, longpower |
| **Cohen's d and standardized effect sizes** | Psychology uses Cohen's d universally for effect size communication and meta-analysis | pwr, Superpower |
| **Factorial designs** | Treatment x moderator designs (e.g., CBT vs. medication x severity level) are common | Superpower |
| **Historical data incorporation** | Psych interventions have substantial prior literature; formal borrowing could reduce sample sizes | BayesPPD, BayesCTDesign |

### Lower Priority

| Gap | Why it matters for psych RCTs | Competitors that have it |
|-----|-------------------------------|--------------------------|
| **Cluster randomization** | Therapy delivered by therapists (clustering) or in group settings | simr (via random effects), PowerUpR |
| **Count outcomes** | Symptom counts, behavioral frequency measures | BayesPPD, BayesCTDesign, BATSS, simr, Mediana |
| **Survival / time-to-event** | Relapse time, time to dropout — less common but used in substance use research | bayesCT, BayesCTDesign, BATSS, gsDesign, rpact, Mediana |
| **GUI / Shiny app** | Lower barrier for clinical researchers without R experience | None of the competitors have this either |

### Gaps That Are Strategic Non-Issues

| Non-gap | Rationale |
|---------|-----------|
| Frequentist methods | rctbayespower is deliberately Bayesian-only; users needing frequentist analysis use gsDesign/rpact/pwr |
| Bayes factors | Different decision paradigm; ROPE is the chosen framework |
