# Bayesian Adaptive Trial Designs Reference

This document provides a comprehensive reference of historical Bayesian adaptive trial designs that could be implemented in `rctbayespower`.

## Overview

Bayesian adaptive designs leverage posterior and predictive probability distributions to make interim decisions about trial conduct. Key advantages include:

- Natural incorporation of prior information
- Continuous monitoring without alpha-spending penalties
- Intuitive probability statements for decision-making
- Flexibility in timing of interim analyses

---

## 1. Bayesian Interim Monitoring (High Priority)

These designs align with the current `06_interim_analysis_plan.md` and should be prioritized for implementation.

### 1.1 Posterior Probability Stopping Rules

| Aspect | Details |
|--------|---------|
| **Year** | 1985+ |
| **Key Authors** | Berry; Spiegelhalter & Freedman |
| **Core Idea** | Stop when P(θ > θ₀ \| data) exceeds threshold |

**Decision Rules:**
- **Efficacy**: Stop if P(treatment effect > ROPE_upper | data) > threshold_efficacy
- **Futility**: Stop if P(treatment effect < ROPE_lower | data) > threshold_futility

**Implementation Notes:**
- Directly computable from `brms` posterior samples
- Threshold calibration via simulation to control frequentist operating characteristics
- Typical efficacy threshold: 0.95-0.99
- Typical futility threshold: 0.80-0.95

### 1.2 Bayesian Predictive Probability

| Aspect | Details |
|--------|---------|
| **Year** | 1989+ |
| **Key Authors** | Spiegelhalter, Freedman, Parmar |
| **Core Idea** | P(trial success \| current data, future enrollment) |

**Key Distinction from Posterior Probability:**
- **Posterior probability**: "Is the treatment effective based on current data?"
- **Predictive probability**: "Will the trial succeed if we continue to planned sample size?"

**Why Predictive Probability is Preferred for Futility:**
- Directly addresses whether continuing the trial is worthwhile
- Accounts for remaining sample size
- More conservative early in trial, more decisive later

**Formula:**
```
PP = ∫ P(success | θ) × p(θ | current data) dθ
```

Where success is defined as posterior probability exceeding efficacy threshold at final analysis.

### 1.3 Bayesian Decision-Theoretic Design

| Aspect | Details |
|--------|---------|
| **Year** | 2007 |
| **Key Authors** | Lewis, Lipsky & Berry |
| **Core Idea** | Minimize expected total cost (sample size + terminal loss) |

**Components:**
- Terminal loss function reflecting hypothesis-testing goals
- Backward induction to determine optimal stopping rules
- Can outperform O'Brien-Fleming in mean sample size

---

## 2. Bayesian Phase II Designs

### 2.1 Thall-Simon Design (1994)

| Aspect | Details |
|--------|---------|
| **Year** | 1994 |
| **Key Authors** | Thall & Simon |
| **Publication** | Biometrics 1994; 50: 337-349 |

**Design Features:**
- Single-arm trial comparing experimental treatment to historical control
- Informative prior for standard treatment θS
- Non-informative prior for experimental treatment θE
- Continuous monitoring with posterior probability

**Stopping Rules:**
- **Efficacy**: P(θE > θS + δ | data) > upper threshold
- **Futility**: P(θE > θS | data) < lower threshold
- **Maximum sample size**: Continue if neither threshold crossed

**Extensions:**
- Thall-Simon-Estey (1995): Multiple outcomes (efficacy + toxicity)
- Hierarchical version for diseases with subtypes (Thall 2003)

### 2.2 BOP2 Design (Bayesian Optimal Phase II)

| Aspect | Details |
|--------|---------|
| **Year** | 2017 |
| **Key Authors** | Zhou et al. |

**Features:**
- Adaptive probability cutoffs at each interim
- Maximizes power while controlling type I error
- Applicable to single-arm and two-arm trials
- Flexible number of interim looks

### 2.3 TOP Design (Transformed Odds Prior)

| Aspect | Details |
|--------|---------|
| **Year** | 2019 |
| **Key Authors** | Lin & Lee |

**Features:**
- Handles complex outcomes
- Uses transformed odds for better prior specification

---

## 3. Bayesian Response-Adaptive Randomization (RAR)

### 3.1 Thompson Sampling

| Aspect | Details |
|--------|---------|
| **Year** | 1933 (original) |
| **Key Author** | Thompson |

**Algorithm:**
1. Sample θₖ from posterior for each arm k
2. Allocate next patient to arm with highest sampled θₖ
3. Update posteriors with observed outcome
4. Repeat

**Advantages:**
- Allocates more patients to better-performing arms
- Natural exploration-exploitation balance
- Ethical benefits in patient allocation

**Limitations in Clinical Trials:**
- Can have low statistical power
- Covariate imbalances may arise
- Works best with short-term outcome ascertainment

### 3.2 Top-Two Thompson Sampling (TTTS)

| Aspect | Details |
|--------|---------|
| **Year** | 2017+ |
| **Purpose** | Best-arm identification |

**Improvement over Standard Thompson Sampling:**
- Better performance for small sample sizes typical in clinical trials
- Accelerated versions available for drug development scenarios

### 3.3 Notable RAR Implementations

**I-SPY 2 Trial (2010+):**
- Breast cancer neoadjuvant platform trial
- Bayesian adaptive randomization across 10 biomarker subtypes
- Graduation threshold: P(success in phase III) > 85%
- Futility threshold: P(success) < 10% for all signatures

**BATTLE Trial (2006-2009):**
- First biomarker-based adaptive randomization in lung cancer
- Four treatments, five biomarker subgroups
- Suspension of randomization for underperforming treatment/marker pairs

---

## 4. Bayesian Incorporation of Historical Data

These methods are highly relevant for `rctbayespower` since the package already uses design priors.

### 4.1 Power Prior

| Aspect | Details |
|--------|---------|
| **Year** | 2000 |
| **Key Authors** | Ibrahim & Chen |

**Formula:**
```
π(θ | D₀, α₀) ∝ L(θ | D₀)^α₀ × π₀(θ)
```

Where:
- D₀ = historical data
- α₀ ∈ [0,1] = weight parameter (0 = ignore, 1 = full borrowing)
- π₀(θ) = initial prior

**Considerations:**
- Fixed α₀ requires careful selection
- Can lead to type I error inflation if historical and current data conflict

### 4.2 Normalized Power Prior

| Aspect | Details |
|--------|---------|
| **Year** | 2006 |
| **Key Authors** | Duan et al. |

**Improvement:**
- Addresses normalization issues in original power prior
- Proper posterior distribution

### 4.3 Commensurate Prior

| Aspect | Details |
|--------|---------|
| **Year** | 2011 |
| **Key Authors** | Hobbs et al. |

**Key Innovation:**
- Allows historical and current data model parameters to differ
- Commensurability parameter τ controls borrowing adaptively
- Automatically downweights historical data when conflict detected

**Model Structure:**
```
θ_current ~ N(θ_historical, 1/τ)
```

Where τ characterizes the degree of commensurability.

### 4.4 Robust Meta-Analytic-Predictive (MAP) Prior

| Aspect | Details |
|--------|---------|
| **Year** | 2014 |
| **Key Authors** | Schmidli et al. |

**Features:**
- Meta-analytic framework for multiple historical studies
- Robustification component to handle prior-data conflict
- Mixture prior with non-informative component

---

## 5. Bayesian Dose-Finding Designs (Phase I)

### 5.1 Continual Reassessment Method (CRM)

| Aspect | Details |
|--------|---------|
| **Year** | 1990 |
| **Key Authors** | O'Quigley, Pepe & Fisher |
| **Type** | Model-based |

**Features:**
- Assumes parametric dose-toxicity model
- Bayesian updating of model parameters after each cohort
- Recommends dose closest to target toxicity rate

**Common Models:**
- Power model: P(DLT | dose d) = d^exp(β)
- Logistic model: logit(P(DLT)) = α + β × log(dose)

### 5.2 Escalation with Overdose Control (EWOC)

| Aspect | Details |
|--------|---------|
| **Year** | 1998 |
| **Key Authors** | Babb et al. |
| **Type** | Model-based |

**Key Constraint:**
- Controls P(dose > MTD) ≤ α (typically 0.25-0.50)
- More conservative than CRM

### 5.3 Bayesian Optimal Interval (BOIN)

| Aspect | Details |
|--------|---------|
| **Year** | 2015 |
| **Key Authors** | Liu & Yuan |
| **Type** | Model-assisted |

**Key Innovation:**
- As simple to implement as 3+3
- Performance comparable to CRM
- Pre-tabulated decision rules based on optimal Bayesian intervals

**Decision Boundaries:**
- Escalate if observed DLT rate < λe
- De-escalate if observed DLT rate > λd
- Stay if λe ≤ observed rate ≤ λd

### 5.4 Keyboard Design (mTPI-2)

| Aspect | Details |
|--------|---------|
| **Year** | 2017 |
| **Key Authors** | Yan et al. |
| **Type** | Model-assisted |

**Innovation:**
- Defines equal-width dosing intervals ("keys")
- Fixes overdosing issues in original mTPI
- Uses posterior probability of each interval containing true toxicity

**Software:** Available at www.trialdesign.org

---

## 6. Bayesian Seamless Phase II/III Designs

### 6.1 Bayesian Adaptive Treatment Selection

**Phase II:**
- Multiple treatments compared
- Selection based on posterior probability of being best
- Possible early stopping for futility

**Transition to Phase III:**
- Selected treatment(s) continue
- Combined analysis using all data

### 6.2 Bayesian Predictive Power Sample Size Re-estimation

| Aspect | Details |
|--------|---------|
| **Application** | Sample size adjustment at interim |
| **Method** | Re-estimate based on Bayesian predictive probability |

**Process:**
1. Calculate predictive probability of success at planned sample size
2. If in "promising zone," increase sample size
3. If predictive probability too low, stop for futility

### 6.3 Hybrid Bayesian Subgroup Selection

| Aspect | Details |
|--------|---------|
| **Year** | 2025 |
| **Key Authors** | Duputel et al. |

**Features:**
- Select promising subpopulations at interim
- Shrinkage priors for borrowing across subgroups
- Binary Phase II outcome, time-to-event Phase III outcome

---

## 7. Platform Trial Bayesian Methods

### 7.1 I-SPY 2 Paradigm

**Bayesian Components:**
1. **Adaptive randomization** within biomarker subtypes
2. **Graduation**: P(pCR improvement > δ in phase III | data) > 0.85
3. **Futility**: P(any signature successful) < 0.10
4. **Perpetual enrollment**: Arms enter/exit based on posterior

### 7.2 BATTLE Paradigm

**Bayesian Components:**
1. **Biomarker-stratified randomization**
2. **Adaptive allocation** based on observed treatment/marker performance
3. **Suspension rules** for underperforming combinations

---

## Implementation Priority for rctbayespower

### Phase 1: Core Bayesian Interim (Immediate)

| Feature | Complexity | Notes |
|---------|------------|-------|
| Posterior probability stopping | Low | Direct from brms posteriors |
| Bayesian predictive probability | Medium | Requires simulation |
| Conditional power (Bayesian) | Medium | Average of conditional power over posterior |

### Phase 2: Enhanced Prior Borrowing (Near-term)

| Feature | Complexity | Notes |
|---------|------------|-------|
| Power prior | Medium | Weight parameter selection |
| Commensurate prior | Medium-High | Adaptive borrowing |
| Robust MAP prior | High | Meta-analytic framework |

### Phase 3: Advanced Designs (Future)

| Feature | Complexity | Notes |
|---------|------------|-------|
| Thall-Simon continuous monitoring | Medium | Single-arm with historical |
| Thompson sampling RAR | High | Requires real-time updates |
| Bayesian seamless II/III | High | Multi-phase framework |

---

## Key References

### Textbooks

- Berry SM, Carlin BP, Lee JJ, Muller P (2010). *Bayesian Adaptive Methods for Clinical Trials*. Chapman & Hall/CRC.
- Spiegelhalter DJ, Abrams KR, Myles JP (2004). *Bayesian Approaches to Clinical Trials and Health-Care Evaluation*. Wiley.

### Foundational Papers

- Thall PF, Simon R (1994). Practical Bayesian guidelines for phase IIB clinical trials. *Biometrics* 50:337-349.
- Thall PF, Simon R, Estey E (1995). Bayesian sequential monitoring designs for single-arm clinical trials with multiple outcomes. *Statistics in Medicine* 14:357-379.
- Ibrahim JG, Chen MH (2000). Power prior distributions for regression models. *Statistical Science* 15:46-60.
- Hobbs BP, Carlin BP, Mandrekar SJ, Sargent DJ (2011). Hierarchical commensurate and power prior models for adaptive incorporation of historical information. *Biometrics* 67:1047-1056.
- Liu S, Yuan Y (2015). Bayesian optimal interval designs for phase I clinical trials. *Journal of the Royal Statistical Society Series C* 64:507-523.

### Review Articles

- [PMC - The Bayesian Design of Adaptive Clinical Trials](https://pmc.ncbi.nlm.nih.gov/articles/PMC7826635/)
- [PMC - Tutorial on Modern Bayesian Methods in Clinical Trials](https://pmc.ncbi.nlm.nih.gov/articles/PMC10117244/)
- [PMC - Utility of Bayesian Predictive Probabilities for Interim Monitoring](https://pmc.ncbi.nlm.nih.gov/articles/PMC4247348/)

### Software

- **www.trialdesign.org** - BOIN Suite for dose-finding designs
- **brms** / **Stan** - General Bayesian modeling (used by rctbayespower)
- **gsDesign** - Group sequential designs with Bayesian extensions

---

## Glossary

| Term | Definition |
|------|------------|
| **Posterior Probability** | P(θ \| data) - probability of parameter given observed data |
| **Predictive Probability** | P(future success \| current data) - probability of trial success |
| **Power Prior** | Prior that incorporates historical data with weight α₀ |
| **Commensurate Prior** | Prior allowing adaptive borrowing based on data consistency |
| **ROPE** | Region of Practical Equivalence - decision boundary for treatment effects |
| **MTD** | Maximum Tolerated Dose |
| **DLT** | Dose-Limiting Toxicity |
| **pCR** | Pathological Complete Response |

---

*Document created: 2025-11-26*
*Last updated: 2025-11-26*
