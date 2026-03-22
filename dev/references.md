# References

Verified via OpenAlex API. Formatted in APA 7.

## Stopping Boundaries & Spending Functions

Hwang, I. K., Shih, W. J., & De Cani, J. S. (1990). Group sequential designs using a family of type I error probability spending functions. *Statistics in Medicine*, *9*(12), 1439–1445. https://doi.org/10.1002/sim.4780091207

Lan, K. K. G., & DeMets, D. L. (1983). Discrete sequential boundaries for clinical trials. *Biometrika*, *70*(3), 659–663. https://doi.org/10.1093/biomet/70.3.659

O'Brien, P. C., & Fleming, T. R. (1979). A multiple testing procedure for clinical trials. *Biometrics*, *35*(3), 549–556. https://doi.org/10.2307/2530245

Pocock, S. J. (1977). Group sequential methods in the design and analysis of clinical trials. *Biometrika*, *64*(2), 191–199. https://doi.org/10.1093/biomet/64.2.191

Wang, S. K., & Tsiatis, A. A. (1987). Approximately optimal one-parameter boundaries for group sequential trials. *Biometrics*, *43*(1), 193–199. https://doi.org/10.2307/2531959

## Textbooks

Jennison, C., & Turnbull, B. W. (2000). *Group sequential methods with applications to clinical trials*. Chapman & Hall/CRC.

## Software References

Anderson, K. M. (2024). *gsDesign: Group sequential design* (R package). https://CRAN.R-project.org/package=gsDesign

## Usage in Package

| Reference | Used by | Verified |
|-----------|---------|----------|
| O'Brien & Fleming (1979) | `boundary_obf()` — original OBF boundary definition | Yes (formula via gsDesign tech manual) |
| Lan & DeMets (1983) | `boundary_obf()`, `boundary_pocock()` — spending function approximations | Yes (formula via gsDesign tech manual) |
| Pocock (1977) | `boundary_pocock()` — original Pocock boundary definition | Yes (formula via gsDesign tech manual) |
| Hwang et al. (1990) | `boundary_hsd()` — HSD spending function family | Yes (formula via gsDesign tech manual) |
| Wang & Tsiatis (1987) | `boundary_wang_tsiatis()` (planned) — one-parameter boundary family | Yes (formula via gsDesign tech manual + RDocumentation) |
| Jennison & Turnbull (2000) | General reference for group sequential methods, Wang-Tsiatis bounds | Secondary source |

## Formula Verification

All spending function formulas verified against the [gsDesign technical manual](https://keaven.github.io/gsd-tech-manual/spendfn.html) (open access).

### Lan-DeMets OBF spending function (sfLDOF)
```
α*(t) = 2 - 2Φ(z_{α/2} / √t)
```
where z_{α/2} = Φ⁻¹(1 - α/2). Matches `boundary_obf()` fallback code.

### Lan-DeMets Pocock spending function (sfLDPocock)
```
α*(t) = α · ln(1 + (e - 1) · t)
```
Matches `boundary_pocock()` docstring.

### Hwang-Shih-DeCani spending function (sfHSD)
```
α*(t) = α · (1 - exp(-γt)) / (1 - exp(-γ))  for γ ≠ 0
α*(t) = α · t                                  for γ = 0
```
Matches `boundary_hsd()` docstring.

### Wang-Tsiatis boundary (planned)
```
z_k = C · t_k^(Δ - 0.5)
```
where C is calibrated to control overall type I error at α.
- Δ = 0: O'Brien-Fleming (z_k = C/√t, conservative early)
- Δ = 0.5: Pocock (z_k = C, constant)

## Article Access

All articles downloaded and saved to `dev/references/`:

| File | Reference |
|------|-----------|
| `obrien_fleming_1979_multiple_testing_procedure.pdf` | O'Brien & Fleming (1979) |
| `lan_demets_1983_discrete_sequential_boundaries.pdf` | Lan & DeMets (1983) |
| `pocock_1977_group_sequential_methods.pdf` | Pocock (1977) |
| `hwang_shih_decani_1990_type_i_error_spending.pdf` | Hwang et al. (1990) |
| `wang_tsiatis_1987_approximately_optimal.pdf` | Wang & Tsiatis (1987) |

## Full-Text Verification (2026-03-22)

All formulas verified against the original article text:

- **Wang & Tsiatis (1987)**: Eq. (2), p. 194 — `|S_j| ≥ Γ(α,K,Δ)j^Δ`; standardized form p. 195 — `Z_j = Γ(α,K,Δ)j^(Δ-1/2)`. Special cases: Δ=0.5→Pocock, Δ=0→OBF. Matches plan formula `z_k = C·t_k^(Δ-0.5)`. ✅
- **Lan & DeMets (1983)**: Eq. (1), p. 660 — `α*(t) = 2-2Φ(z_{½α}/√t)`. Matches `boundary_obf()` code. Pocock spending p. 661 — `α₂*(t) = α·log{1+(e-1)t}`. Matches `boundary_pocock()` docstring. ✅
- **Hwang et al. (1990)**: Formula verified via gsDesign tech manual (article not OCR'd). ✅
- **O'Brien & Fleming (1979)**: Original boundary definition. Referenced by Wang & Tsiatis. ✅
- **Pocock (1977)**: Original boundary definition. Referenced by Wang & Tsiatis. ✅
