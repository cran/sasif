library(testthat)
library(sasif)
library(data.table)

# ─────────────────────────────────────────────
# TEST SUITE: sasif v0.1.2
# Tests for all 6 functions:
# data_step, if_do, else_if_do, else_do,
# delete_if, if_independent
# ─────────────────────────────────────────────


# ═══════════════════════════════════════════
# 1. data_step()
# ═══════════════════════════════════════════

test_that("data_step() returns a data.table", {
  dt <- data.table(USUBJID = c("S01", "S02"), AVAL = c(10, 20))
  result <- data_step(dt, if_do(AVAL > 5, FLAG = "Y"))
  expect_true(is.data.table(result))
})

test_that("data_step() does not lose rows", {
  dt <- data.table(USUBJID = c("S01", "S02", "S03"), AVAL = c(10, 20, 30))
  result <- data_step(dt, if_do(AVAL > 5, FLAG = "Y"))
  expect_equal(nrow(result), 3)
})

test_that("data_step() does not lose columns", {
  dt <- data.table(USUBJID = c("S01", "S02"), AVAL = c(10, 20))
  result <- data_step(dt, if_do(AVAL > 5, FLAG = "Y"))
  expect_true("USUBJID" %in% names(result))
  expect_true("AVAL" %in% names(result))
})

test_that("data_step() throws error for non data.table input", {
  df <- data.frame(USUBJID = c("S01", "S02"), AVAL = c(10, 20))
  expect_error(data_step(df, if_do(AVAL > 5, FLAG = "Y")))
})


# ═══════════════════════════════════════════
# 2. if_do()
# ═══════════════════════════════════════════

test_that("if_do() assigns value when condition is TRUE", {
  dt <- data.table(USUBJID = c("S01", "S02"), AVAL = c(10, 20))
  result <- data_step(dt, if_do(AVAL > 15, FLAG = "Y"))
  expect_equal(result[USUBJID == "S02", FLAG], "Y")
})

test_that("if_do() does not assign value when condition is FALSE", {
  dt <- data.table(USUBJID = c("S01", "S02"), AVAL = c(10, 20))
  result <- data_step(dt, if_do(AVAL > 15, FLAG = "Y"))
  expect_true(is.na(result[USUBJID == "S01", FLAG]))
})

test_that("if_do() assigns multiple variables simultaneously", {
  dt <- data.table(
    USUBJID  = c("S01", "S02", "S03"),
    ACTARMCD = c("TRTA", "TRTB", "TRTA")
  )
  result <- data_step(dt,
    if_do(ACTARMCD == "TRTA",
      SAFFL   = "Y",
      SAFFLN  = 1,
      TRT01A  = "Treatment A",
      TRT01AN = 1,
      ITTFL   = "Y",
      FASFL   = "Y"
    )
  )
  # All variables assigned correctly for TRTA rows
  trta_rows <- result[ACTARMCD == "TRTA"]
  expect_true(all(trta_rows$SAFFL == "Y"))
  expect_true(all(trta_rows$SAFFLN == 1))
  expect_true(all(trta_rows$TRT01A == "Treatment A"))
  expect_true(all(trta_rows$TRT01AN == 1))
  expect_true(all(trta_rows$ITTFL == "Y"))
  expect_true(all(trta_rows$FASFL == "Y"))
})

test_that("if_do() does not assign to rows where condition is FALSE", {
  dt <- data.table(
    USUBJID  = c("S01", "S02"),
    ACTARMCD = c("TRTA", "TRTB")
  )
  result <- data_step(dt,
    if_do(ACTARMCD == "TRTA", SAFFL = "Y")
  )
  expect_true(is.na(result[ACTARMCD == "TRTB", SAFFL]))
})

test_that("if_do() works with numeric assignments", {
  dt <- data.table(USUBJID = c("S01", "S02"), AGE = c(30, 60))
  result <- data_step(dt, if_do(AGE > 50, AGECATN = 2))
  expect_equal(result[USUBJID == "S02", AGECATN], 2)
})

test_that("if_do() works with date assignments", {
  dt <- data.table(
    USUBJID = c("S01", "S02"),
    ACTARMCD = c("TRTA", "TRTB"),
    RFSTDTC = c("2024-01-10", "2024-01-15")
  )
  result <- data_step(dt,
    if_do(ACTARMCD == "TRTA",
      TRTSDT = as.Date(RFSTDTC, "%Y-%m-%d")
    )
  )
  expect_equal(result[USUBJID == "S01", TRTSDT], as.Date("2024-01-10"))
})

test_that("if_do() handles missing values in condition", {
  dt <- data.table(USUBJID = c("S01", "S02", "S03"),
                   AVAL = c(10, NA, 30))
  result <- data_step(dt, if_do(AVAL > 15, FLAG = "Y"))
  expect_true(is.na(result[USUBJID == "S02", FLAG]))
})


# ═══════════════════════════════════════════
# 3. else_if_do()
# ═══════════════════════════════════════════

test_that("else_if_do() assigns when prior if_do() is FALSE", {
  dt <- data.table(USUBJID = c("S01", "S02", "S03"),
                   AVAL = c(5, 15, 25))
  result <- data_step(dt,
    if_do(AVAL > 20,      GRADE = "HIGH"),
    else_if_do(AVAL > 10, GRADE = "MED")
  )
  expect_equal(result[USUBJID == "S02", GRADE], "MED")
})

test_that("else_if_do() does not override prior if_do() match", {
  dt <- data.table(USUBJID = c("S01", "S02", "S03"),
                   AVAL = c(5, 15, 25))
  result <- data_step(dt,
    if_do(AVAL > 20,      GRADE = "HIGH"),
    else_if_do(AVAL > 10, GRADE = "MED")
  )
  expect_equal(result[USUBJID == "S03", GRADE], "HIGH")
})

test_that("else_if_do() assigns multiple variables simultaneously", {
  dt <- data.table(
    USUBJID  = c("S01", "S02", "S03"),
    ACTARMCD = c("TRTA", "TRTB", "TRTC")
  )
  result <- data_step(dt,
    if_do(ACTARMCD == "TRTA",
      TRT01A = "Treatment A", TRT01AN = 1),
    else_if_do(ACTARMCD == "TRTB",
      TRT01A = "Treatment B", TRT01AN = 2)
  )
  expect_equal(result[ACTARMCD == "TRTB", TRT01A], "Treatment B")
  expect_equal(result[ACTARMCD == "TRTB", TRT01AN], 2)
})

test_that("else_if_do() leaves unmatched rows as NA", {
  dt <- data.table(USUBJID = c("S01", "S02", "S03"),
                   AVAL = c(5, 15, 25))
  result <- data_step(dt,
    if_do(AVAL > 20,      GRADE = "HIGH"),
    else_if_do(AVAL > 10, GRADE = "MED")
  )
  expect_true(is.na(result[USUBJID == "S01", GRADE]))
})


# ═══════════════════════════════════════════
# 4. else_do()
# ═══════════════════════════════════════════

test_that("else_do() assigns default when all prior conditions FALSE", {
  dt <- data.table(USUBJID = c("S01", "S02", "S03"),
                   AVAL = c(5, 15, 25))
  result <- data_step(dt,
    if_do(AVAL > 20,       GRADE = "HIGH"),
    else_if_do(AVAL > 10,  GRADE = "MED"),
    else_do(               GRADE = "LOW")
  )
  expect_equal(result[USUBJID == "S01", GRADE], "LOW")
})

test_that("else_do() does not override matched rows", {
  dt <- data.table(USUBJID = c("S01", "S02", "S03"),
                   AVAL = c(5, 15, 25))
  result <- data_step(dt,
    if_do(AVAL > 20,       GRADE = "HIGH"),
    else_if_do(AVAL > 10,  GRADE = "MED"),
    else_do(               GRADE = "LOW")
  )
  expect_equal(result[USUBJID == "S03", GRADE], "HIGH")
  expect_equal(result[USUBJID == "S02", GRADE], "MED")
})

test_that("else_do() assigns multiple variables simultaneously", {
  dt <- data.table(
    USUBJID  = c("S01", "S02", "S03"),
    ACTARMCD = c("TRTA", "TRTB", "PBO")
  )
  result <- data_step(dt,
    if_do(ACTARMCD == "TRTA",
      TRT01A = "Treatment A", TRT01AN = 1),
    else_if_do(ACTARMCD == "TRTB",
      TRT01A = "Treatment B", TRT01AN = 2),
    else_do(
      TRT01A = "Placebo", TRT01AN = 99)
  )
  expect_equal(result[ACTARMCD == "PBO", TRT01A], "Placebo")
  expect_equal(result[ACTARMCD == "PBO", TRT01AN], 99)
})

test_that("Full IF/ELSE IF/ELSE chain covers all rows", {
  dt <- data.table(USUBJID = c("S01", "S02", "S03"),
                   AGE = c(32, 55, 75))
  result <- data_step(dt,
    if_do(AGE <= 45,       AGECAT = "YOUNG",  AGECATN = 1),
    else_if_do(AGE <= 70,  AGECAT = "MIDDLE", AGECATN = 2),
    else_do(               AGECAT = "OLD",    AGECATN = 3)
  )
  expect_equal(result[USUBJID == "S01", AGECAT], "YOUNG")
  expect_equal(result[USUBJID == "S02", AGECAT], "MIDDLE")
  expect_equal(result[USUBJID == "S03", AGECAT], "OLD")
  expect_equal(result[USUBJID == "S01", AGECATN], 1)
  expect_equal(result[USUBJID == "S02", AGECATN], 2)
  expect_equal(result[USUBJID == "S03", AGECATN], 3)
})


# ═══════════════════════════════════════════
# 5. delete_if()
# ═══════════════════════════════════════════

test_that("delete_if() removes rows where condition is TRUE", {
  dt <- data.table(USUBJID = c("S01", "S02", "S03"),
                   AVAL = c(5, 15, 25))
  result <- data_step(dt, delete_if(AVAL < 10))
  expect_equal(nrow(result), 2)
  expect_false("S01" %in% result$USUBJID)
})

test_that("delete_if() retains rows where condition is FALSE", {
  dt <- data.table(USUBJID = c("S01", "S02", "S03"),
                   AVAL = c(5, 15, 25))
  result <- data_step(dt, delete_if(AVAL < 10))
  expect_true("S02" %in% result$USUBJID)
  expect_true("S03" %in% result$USUBJID)
})

test_that("delete_if() removes rows with missing values", {
  dt <- data.table(
    USUBJID  = c("S01", "S02", "S03"),
    LBTESTCD = c("ALB", NA, "ALB")
  )
  result <- data_step(dt, delete_if(is.na(LBTESTCD)))
  expect_equal(nrow(result), 2)
  expect_false("S02" %in% result$USUBJID)
})

test_that("delete_if() removes screen failures", {
  dt <- data.table(
    USUBJID  = c("S01", "S02", "S03"),
    ACTARMCD = c("TRTA", "SCRNFAIL", "TRTA")
  )
  result <- data_step(dt, delete_if(ACTARMCD == "SCRNFAIL"))
  expect_equal(nrow(result), 2)
  expect_false("S02" %in% result$USUBJID)
})

test_that("delete_if() with multiple conditions removes correct rows", {
  dt <- data.table(
    USUBJID  = c("S01", "S02", "S03", "S04", "S05"),
    LBTESTCD = c("ALB", NA, "ALB", "ALB", NA),
    VISIT    = c("WEEK 1", "WEEK 1", "UNSCHEDULED", "WEEK 2", "WEEK 4")
  )
  result <- data_step(dt,
    delete_if(is.na(LBTESTCD)),
    delete_if(VISIT == "UNSCHEDULED")
  )
  expect_equal(nrow(result), 2)
  expect_true(all(result$USUBJID %in% c("S01", "S04")))
})

test_that("delete_if() returns empty data.table when all rows deleted", {
  dt <- data.table(USUBJID = c("S01", "S02"),
                   AVAL = c(5, 8))
  result <- data_step(dt, delete_if(AVAL < 10))
  expect_equal(nrow(result), 0)
})


# ═══════════════════════════════════════════
# 6. if_independent()
# ═══════════════════════════════════════════

test_that("if_independent() assigns when condition is TRUE", {
  dt <- data.table(USUBJID = c("S01", "S02"),
                   AGE = c(30, 70))
  result <- data_step(dt,
    if_independent(AGE > 65, SENIORFL = "Y")
  )
  expect_equal(result[USUBJID == "S02", SENIORFL], "Y")
})

test_that("if_independent() does not assign when condition is FALSE", {
  dt <- data.table(USUBJID = c("S01", "S02"),
                   AGE = c(30, 70))
  result <- data_step(dt,
    if_independent(AGE > 65, SENIORFL = "Y")
  )
  expect_true(is.na(result[USUBJID == "S01", SENIORFL]))
})

test_that("if_independent() allows multiple conditions on same row", {
  dt <- data.table(
    USUBJID  = c("S01", "S02"),
    AGE      = c(30, 70),
    WEIGHTKG = c(48, 72),
    DIABFL   = c("N", "Y")
  )
  result <- data_step(dt,
    if_independent(AGE > 65,       SENIORFL = "Y"),
    if_independent(WEIGHTKG < 50,  LOWWTFL  = "Y"),
    if_independent(DIABFL == "Y",  COMORBFL = "Y")
  )
  # S02 meets all three conditions
  expect_equal(result[USUBJID == "S02", SENIORFL], "Y")
  expect_true(is.na(result[USUBJID == "S02", LOWWTFL]))
  expect_equal(result[USUBJID == "S02", COMORBFL], "Y")
  # S01 meets only LOWWTFL
  expect_true(is.na(result[USUBJID == "S01", SENIORFL]))
  expect_equal(result[USUBJID == "S01", LOWWTFL], "Y")
  expect_true(is.na(result[USUBJID == "S01", COMORBFL]))
})

test_that("if_independent() assigns multiple variables per condition", {
  dt <- data.table(
    USUBJID  = c("S01", "S02", "S03"),
    AVAL     = c(2.8, 4.2, 5.6),
    ANRLO    = c(3.5, 3.5, 3.5),
    ANRHI    = c(5.0, 5.0, 5.0)
  )
  result <- data_step(dt,
    if_independent(AVAL < ANRLO,
      LOWNFL  = "Y",
      LOWN    = 1
    )
  )
  expect_equal(result[USUBJID == "S01", LOWNFL], "Y")
  expect_equal(result[USUBJID == "S01", LOWN], 1)
  expect_true(is.na(result[USUBJID == "S02", LOWNFL]))
})


# ═══════════════════════════════════════════
# 7. Clinical ADaM Integration Tests
# ═══════════════════════════════════════════

test_that("ADSL population flags derive correctly", {
  adsl <- data.table(
    USUBJID  = c("S01", "S02", "S03"),
    ACTARMCD = c("TRTA", "SCRNFAIL", "TRTA"),
    RFSTDTC  = c("2024-01-10", NA, "2024-01-20"),
    RFENDTC  = c("2024-06-10", NA, "2024-06-20")
  )
  result <- data_step(adsl,
    if_do(ACTARMCD == "TRTA",
      SAFFL   = "Y",
      SAFFLN  = 1,
      TRT01A  = "Treatment A",
      TRT01AN = 1,
      ITTFL   = "Y",
      FASFL   = "Y",
      RANDFL  = "Y",
      PPFL    = "Y"
    )
  )
  trta <- result[ACTARMCD == "TRTA"]
  expect_true(all(trta$SAFFL == "Y"))
  expect_true(all(trta$ITTFL == "Y"))
  expect_true(all(trta$FASFL == "Y"))
  expect_true(all(trta$RANDFL == "Y"))
  expect_true(all(trta$PPFL == "Y"))
  expect_true(is.na(result[ACTARMCD == "SCRNFAIL", SAFFL]))
})

test_that("ADLB lab categorisation derives correctly", {
  adlb <- data.table(
    USUBJID  = c("S01", "S02", "S03"),
    LBTESTCD = c("ALB", "ALB", "ALB"),
    AVAL     = c(2.8, 4.2, 5.6),
    ANRLO    = c(3.5, 3.5, 3.5),
    ANRHI    = c(5.0, 5.0, 5.0)
  )
  result <- data_step(adlb,
    if_do(LBTESTCD == "ALB" & AVAL < ANRLO,
      ALBCAT = "LOW", ALBCATN = 1),
    else_if_do(LBTESTCD == "ALB" & AVAL > ANRHI,
      ALBCAT = "HIGH", ALBCATN = 2),
    else_do(
      ALBCAT = "NORMAL", ALBCATN = 3)
  )
  expect_equal(result[USUBJID == "S01", ALBCAT], "LOW")
  expect_equal(result[USUBJID == "S02", ALBCAT], "NORMAL")
  expect_equal(result[USUBJID == "S03", ALBCAT], "HIGH")
  expect_equal(result[USUBJID == "S01", ALBCATN], 1)
  expect_equal(result[USUBJID == "S02", ALBCATN], 3)
  expect_equal(result[USUBJID == "S03", ALBCATN], 2)
})

test_that("ADAE treatment emergent flag derives correctly", {
  adae <- data.table(
    USUBJID = c("S01", "S01"),
    AEDECOD = c("Headache", "Nausea"),
    ASTDT   = as.Date(c("2024-01-15", "2023-12-01")),
    TRTSDT  = as.Date(c("2024-01-10", "2024-01-10")),
    TRTEDT  = as.Date(c("2024-06-10", "2024-06-10"))
  )
  result <- data_step(adae,
    if_do(ASTDT >= TRTSDT & ASTDT <= TRTEDT,
      TRTEMFL = "Y"
    )
  )
  expect_equal(result[AEDECOD == "Headache", TRTEMFL], "Y")
  expect_true(is.na(result[AEDECOD == "Nausea", TRTEMFL]))
})
