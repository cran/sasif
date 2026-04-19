# sasif

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/sasif)](https://CRAN.R-project.org/package=sasif)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/sasif)](https://CRAN.R-project.org/package=sasif)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**SAS IF-Style Data Step Logic for R** — Improving readability and consistency
in SDTM & ADaM derivations.

`sasif` lets clinical programmers write one condition that governs **multiple
variable assignments** in a single block — just like SAS `IF ... THEN DO`.
No repeated conditions. No logic drift. No maintenance burden.

---

## The Core Problem

In traditional R, every variable assignment needs its **own repeated condition**:

```r
# ❌ Traditional R (case_when) — condition repeated for EVERY variable
adsl <- adsl %>% mutate(
  SAFFL   = case_when(ACTARMCD == "TRTA" ~ "Y"),
  SAFFLN  = case_when(ACTARMCD == "TRTA" ~ 1),
  TRT01A  = case_when(ACTARMCD == "TRTA" ~ ACTARMCD),
  TRT01AN = case_when(ACTARMCD == "TRTA" ~ 1),
  TRTSDT  = case_when(ACTARMCD == "TRTA" ~ as.Date(RFSTDTC, "%Y-%m-%d")),
  TRTEDT  = case_when(ACTARMCD == "TRTA" ~ as.Date(RFENDTC, "%Y-%m-%d")),
  ITTFL   = case_when(ACTARMCD == "TRTA" ~ "Y"),
  FASFL   = case_when(ACTARMCD == "TRTA" ~ "Y"),
  RANDFL  = case_when(ACTARMCD == "TRTA" ~ "Y"),
  PPFL    = case_when(ACTARMCD == "TRTA" ~ "Y")
  # condition repeated 10 times — high QC risk if any diverge
)
```

If one condition is ever updated, you must find and change it in every single line.
Miss one and your derivation silently diverges. This is a real QC risk in
regulated clinical trial data.

---

## The sasif Solution

One condition. All assignments grouped together. Just like SAS:

```r
# ✅ sasif — condition written ONCE, governs all assignments
library(sasif)

ADSL <- data_step(adsl,
  if_do(ACTARMCD == "TRTA",
    SAFFL   = "Y",
    SAFFLN  = 1,
    TRT01A  = ACTARMCD,
    TRT01AN = 1,
    TRTSDT  = as.Date(RFSTDTC, "%Y-%m-%d"),
    TRTEDT  = as.Date(RFENDTC, "%Y-%m-%d"),
    ITTFL   = "Y",
    FASFL   = "Y",
    RANDFL  = "Y",
    PPFL    = "Y"
  )
)
```

Clean. Readable. Audit-friendly. Identical in intent to SAS `IF ... THEN DO`.

---

## Installation

```r
# Install from CRAN
install.packages("sasif")

# Install development version from GitHub
# install.packages("devtools")
devtools::install_github("chandrt23-lang/sasif")
```

> **Note:** `sasif` requires a `data.table` as input. Convert with `setDT(df)`
> before calling `data_step()`.

---

## Functions

| Function | SAS Equivalent | Description |
|----------|---------------|-------------|
| `data_step()` | `DATA` step | Opens a SAS-style processing block on a `data.table` |
| `if_do()` | `IF ... THEN DO` | One condition governs multiple variable assignments |
| `else_if_do()` | `ELSE IF ... THEN DO` | Secondary condition — skipped if prior block matched |
| `else_do()` | `ELSE DO` | Default assignments when all prior conditions are FALSE |
| `delete_if()` | `DELETE` | Removes rows where condition is TRUE |
| `if_independent()` | Multiple standalone `IF` | Each condition evaluated independently — not a chain |

---

## Clinical Examples

### Example 1 — ADSL: Multiple Population Flags in One Block

The most common use case — one treatment condition drives 10 variable
assignments simultaneously:

```r
ADSL <- data_step(adsl,
  if_do(ACTARMCD == "TRTA",
    SAFFL   = "Y",
    SAFFLN  = 1,
    TRT01A  = ACTARMCD,
    TRT01AN = 1,
    TRTSDT  = as.Date(RFSTDTC, "%Y-%m-%d"),
    TRTEDT  = as.Date(RFENDTC, "%Y-%m-%d"),
    ITTFL   = "Y",
    FASFL   = "Y",
    RANDFL  = "Y",
    PPFL    = "Y"
  )
)
```

---

### Example 2 — ADSL: Age Category with IF / ELSE IF / ELSE

Mutually exclusive chain — first matching condition wins, others are skipped.
Both `AGECAT` (character) and `AGECATN` (numeric) are derived together:

```r
ADSL <- data_step(adsl,
  if_do(AGE <= 45,
    AGECAT  = "YOUNG",
    AGECATN = 1
  ),
  else_if_do(AGE <= 70,
    AGECAT  = "MIDDLE",
    AGECATN = 2
  ),
  else_do(
    AGECAT  = "OLD",
    AGECATN = 3
  )
)
```

Compare this to `data.table` nested `fifelse()` — which forces you to
repeat the condition separately for each variable:

```r
# ❌ data.table nested fifelse — condition repeated per variable
adlb[, `:=`(
  AGECAT  = fifelse(AGE <= 45, "YOUNG", fifelse(AGE <= 70, "MIDDLE", "OLD")),
  AGECATN = fifelse(AGE <= 45, 1L,      fifelse(AGE <= 70, 2L,       3L))
)]
```

---

### Example 3 — ADLB: Lab Categorisation (Character + Numeric Together)

Derive both the category label and its numeric code from one condition block:

```r
out <- data_step(adlb,
  if_do(LBTESTCD == "ALB" & AVAL < ANRLO,
    ALBCAT  = "LOW",
    ALBCATN = 1
  ),
  else_if_do(LBTESTCD == "ALB" & AVAL > ANRHI,
    ALBCAT  = "HIGH",
    ALBCATN = 2
  ),
  else_do(
    ALBCAT  = "NORMAL",
    ALBCATN = 3
  )
)
```

---

### Example 4 — ADAE: Treatment-Emergent Flag

Flag adverse events that started on or after treatment start date:

```r
ADAE <- data_step(adae,
  if_do(ASTDT >= TRTSDT & ASTDT <= TRTEDT,
    TRTEMFL = "Y",
    TRTEMA  = AEDECOD
  )
)
```

---

### Example 5 — ADSL: Multi-Arm Treatment Assignment

Assign treatment label, numeric code, and start date together per arm:

```r
ADSL <- data_step(adsl,
  if_do(ACTARMCD == "TRTA",
    TRT01A  = "Treatment A",
    TRT01AN = 1,
    TRTSDT  = as.Date(RFSTDTC, "%Y-%m-%d")
  ),
  else_if_do(ACTARMCD == "TRTB",
    TRT01A  = "Treatment B",
    TRT01AN = 2,
    TRTSDT  = as.Date(RFSTDTC, "%Y-%m-%d")
  ),
  else_do(
    TRT01A  = "Placebo",
    TRT01AN = 99,
    TRTSDT  = as.Date(RFSTDTC, "%Y-%m-%d")
  )
)
```

---

### Example 6 — ADLB: Independent Flags (if_independent)

Use `if_independent()` when conditions are **not** mutually exclusive —
each is evaluated on its own, so multiple flags can apply to the same row:

```r
out <- data_step(adlb,
  if_independent(AVAL < ANRLO,  LOWNFL = "Y"),
  if_independent(AVAL > ANRHI,  HINFL  = "Y"),
  if_independent(LBTESTCD == "ALB", ALBFL = "Y")
)
```

> **Important:** Do not mix `if_do()` chains with `if_independent()` on the
> same variable — `if_independent()` runs **after** the chain and will
> overwrite it. Use one approach consistently per variable.

---

### Example 7 — DELETE: Remove Unwanted Rows

Remove screen failures and unscheduled visits explicitly:

```r
# Remove screen failure subjects
ADSL <- data_step(adsl,
  delete_if(ACTARMCD == "SCRNFAIL")
)

# Remove records with missing test codes and unscheduled visits
ADLB <- data_step(adlb,
  delete_if(is.na(LBTESTCD)),
  delete_if(VISIT == "UNSCHEDULED")
)
```

---

## Why Not case_when() or fifelse()?

| Feature | sasif | case_when() | data.table fifelse() |
|---------|-------|-------------|----------------------|
| One condition → multiple variables | ✅ Natural | ❌ Repeated per variable | ❌ Repeated per variable |
| IF / ELSE IF / ELSE chain | ✅ Native | ⚠️ Simulated | ⚠️ Nested |
| SAS programmer readability | ⭐⭐⭐⭐⭐ | ⭐⭐ | ⭐⭐⭐ |
| Risk of condition drift across variables | Low ✅ | High ⚠️ | High ⚠️ |
| Vectorized performance | ✅ | ✅ | ✅ |
| Audit-friendly derivation flow | ✅ | ⚠️ | ⚠️ |

---

## When to Use sasif

- Deriving multiple population flags (`SAFFL`, `ITTFL`, `FASFL`, `RANDFL`) from one condition
- Multi-variable ADaM derivations where the same condition governs several assignments
- Migrating SAS `IF ... THEN DO` blocks directly to R
- QC-sensitive code where auditability and readability matter

## When NOT to Use sasif

`sasif` is focused on conditional derivation logic. Use standard R packages for:

- `RETAIN` / stateful accumulation → use `data.table` or base R
- `LAG` / previous-row values → use `shift()` in `data.table`
- `ARRAY` processing → use `lapply` or `data.table` column operations
- By-group stateful logic → use `data.table` `by=` syntax

---

## Validation

A formal **IQ/OQ/PQ Validation Document** is available for GxP-regulated
environments, covering all 6 functions in accordance with:

- FDA 21 CFR Part 11
- ICH E6(R2) Good Clinical Practice
- GAMP 5

Contact the maintainer to request the validation document.

---

## Getting Help

- CRAN documentation: <https://CRAN.R-project.org/package=sasif>
- Bug reports & feature requests: <https://github.com/chandrt23-lang/sasif/issues>

---

## Citation

```r
citation("sasif")
```

---

## License

MIT © Thiyagarajan Chandrasekaran
