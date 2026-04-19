## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(sasif)
library(data.table)

## ----adsl_problem, eval=FALSE-------------------------------------------------
# # ❌ Traditional R — condition repeated for every variable
# adsl <- adsl %>% mutate(
#   SAFFL   = case_when(ACTARMCD == "TRTA" ~ "Y"),
#   SAFFLN  = case_when(ACTARMCD == "TRTA" ~ 1),
#   TRT01A  = case_when(ACTARMCD == "TRTA" ~ ACTARMCD),
#   TRT01AN = case_when(ACTARMCD == "TRTA" ~ 1),
#   ITTFL   = case_when(ACTARMCD == "TRTA" ~ "Y"),
#   FASFL   = case_when(ACTARMCD == "TRTA" ~ "Y"),
#   RANDFL  = case_when(ACTARMCD == "TRTA" ~ "Y"),
#   PPFL    = case_when(ACTARMCD == "TRTA" ~ "Y")
#   # Same condition written 8 times — high QC risk
# )

## ----adsl_solution------------------------------------------------------------
# Create sample ADSL data
adsl <- data.table(
  USUBJID  = c("S01", "S02", "S03", "S04"),
  ACTARMCD = c("TRTA", "TRTA", "SCRNFAIL", "TRTA"),
  RFSTDTC  = c("2024-01-10", "2024-01-15", NA, "2024-01-20"),
  RFENDTC  = c("2024-06-10", "2024-06-15", NA, "2024-06-20")
)

# ✅ sasif — condition written ONCE, governs all assignments
ADSL <- data_step(adsl,
  if_do(ACTARMCD == "TRTA",
    SAFFL   = "Y",
    SAFFLN  = 1,
    TRT01A  = "Treatment A",
    TRT01AN = 1,
    TRTSDT  = as.Date(RFSTDTC, "%Y-%m-%d"),
    TRTEDT  = as.Date(RFENDTC, "%Y-%m-%d"),
    ITTFL   = "Y",
    FASFL   = "Y",
    RANDFL  = "Y",
    PPFL    = "Y"
  )
)

print(ADSL[, .(USUBJID, ACTARMCD, SAFFL, TRT01A, TRT01AN, ITTFL, FASFL)])

## ----adsl_multiarm------------------------------------------------------------
adsl2 <- data.table(
  USUBJID  = c("S01", "S02", "S03", "S04", "S05"),
  ACTARMCD = c("TRTA", "TRTB", "TRTC", "TRTA", "TRTB"),
  AGE      = c(35, 52, 67, 44, 58)
)

ADSL2 <- data_step(adsl2,
  if_do(ACTARMCD == "TRTA",
    TRT01A  = "Treatment A",
    TRT01AN = 1
  ),
  else_if_do(ACTARMCD == "TRTB",
    TRT01A  = "Treatment B",
    TRT01AN = 2
  ),
  else_do(
    TRT01A  = "Placebo",
    TRT01AN = 99
  )
)

print(ADSL2[, .(USUBJID, ACTARMCD, TRT01A, TRT01AN)])

## ----adsl_agecat--------------------------------------------------------------
adsl3 <- data.table(
  USUBJID = c("S01", "S02", "S03", "S04", "S05"),
  AGE     = c(32, 45, 58, 71, 80)
)

ADSL3 <- data_step(adsl3,
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

print(ADSL3[, .(USUBJID, AGE, AGECAT, AGECATN)])

## ----adlb_example-------------------------------------------------------------
adlb <- data.table(
  USUBJID  = c("S01", "S01", "S02", "S02", "S03"),
  LBTESTCD = c("ALB", "ALB", "ALB", "ALB", "ALB"),
  AVAL     = c(2.8, 4.2, 5.6, 3.5, 1.9),
  ANRLO    = c(3.5, 3.5, 3.5, 3.5, 3.5),
  ANRHI    = c(5.0, 5.0, 5.0, 5.0, 5.0)
)

ADLB <- data_step(adlb,
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

print(ADLB[, .(USUBJID, LBTESTCD, AVAL, ANRLO, ANRHI, ALBCAT, ALBCATN)])

## ----adae_example-------------------------------------------------------------
adae <- data.table(
  USUBJID = c("S01", "S01", "S02", "S02", "S03"),
  AEDECOD = c("Headache", "Nausea", "Fatigue", "Dizziness", "Rash"),
  ASTDT   = as.Date(c("2024-01-15", "2023-12-01",
                       "2024-01-20", "2024-02-10", "2024-01-25")),
  TRTSDT  = as.Date(c("2024-01-10", "2024-01-10",
                       "2024-01-15", "2024-01-15", "2024-01-20")),
  TRTEDT  = as.Date(c("2024-06-10", "2024-06-10",
                       "2024-06-15", "2024-06-15", "2024-06-20"))
)

ADAE <- data_step(adae,
  if_do(ASTDT >= TRTSDT & ASTDT <= TRTEDT,
    TRTEMFL = "Y",
    TRTEMA  = AEDECOD
  )
)

print(ADAE[, .(USUBJID, AEDECOD, ASTDT, TRTSDT, TRTEMFL)])

## ----delete_example-----------------------------------------------------------
adlb2 <- data.table(
  USUBJID  = c("S01", "S02", "S03", "S04", "S05"),
  LBTESTCD = c("ALB", NA,    "ALB", "ALB", NA),
  VISIT    = c("WEEK 1", "WEEK 1", "UNSCHEDULED", "WEEK 2", "WEEK 4"),
  AVAL     = c(4.2, 3.8, 5.1, 4.0, 3.5)
)

ADLB2 <- data_step(adlb2,
  delete_if(is.na(LBTESTCD)),
  delete_if(VISIT == "UNSCHEDULED")
)

print(ADLB2)

## ----if_independent_example---------------------------------------------------
adsl4 <- data.table(
  USUBJID = c("S01", "S02", "S03", "S04"),
  AGE     = c(30, 68, 45, 72),
  WEIGHTKG = c(48, 72, 55, 43),
  DIABFL  = c("N", "Y", "N", "Y")
)

ADSL4 <- data_step(adsl4,
  if_independent(AGE > 65,       SENIORFL  = "Y"),
  if_independent(WEIGHTKG < 50,  LOWWTFL   = "Y"),
  if_independent(DIABFL == "Y",  COMORBFL  = "Y")
)

print(ADSL4)

