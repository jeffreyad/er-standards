# ===============================================================================
# Program: ADTRR.R
#
# Purpose: Create ADTRR (Tumor Response for Exposure-Response Analysis)
#
# Description: Derives tumor response analysis dataset combining longitudinal
#              tumor measurements, RECIST 1.1 evaluations, and exposure metrics
#              for exposure-response analysis.
#
# Input:
#   - ADSL: Subject-level analysis dataset
#   - ADTR: Tumor measurements (from admiralonco)
#   - ADRS: Response evaluations (from admiralonco)
#   - ADVS: Vital signs
#   - ADLB: Laboratory data
#
# Output:
#   - ADTRR: Tumor response for E-R analysis
#
# Structure: BDS (Basic Data Structure)
#   - Multiple records per subject (one per parameter per visit)
#   - PARAMCD identifies measurement type (TSIZE, BOR, NADIR)
#   - Longitudinal tumor measurements with exposure metrics
#
# Author: Jeff Dickinson
# Date: 2026-01-22
# Version: 1.1
#
# Note: Variable names limited to 8 characters for SAS compatibility
#
# ===============================================================================

# Load required packages
library(admiral)
library(admiralonco)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(metacore)
library(metatools)
library(xportr)

# Prevent namespace conflicts
select <- dplyr::select
filter <- dplyr::filter

# ===============================================================================
# LOAD SPECIFICATIONS (if available)
# ===============================================================================

# Load metacore specifications
if (file.exists("specifications/ADTRR_spec.xlsx")) {
  metacore <- spec_to_metacore("specifications/ADTRR_spec.xlsx",
    where_sep_sheet = FALSE
  )
  message("✓ Loaded metacore specifications from Excel")
} else if (file.exists("specifications/ADTRR_metacore.rds")) {
  metacore <- readRDS("specifications/ADTRR_metacore.rds")
  message("✓ Loaded metacore specifications from RDS")
} else {
  message("⚠ No metacore specifications found - proceeding without")
  metacore <- NULL
}

# ===============================================================================
# LOAD INPUT DATA
# ===============================================================================

# Use haven::read_sas() or similar to read in production data
# For illustration, using pharmaverseadam example data
library(pharmaverseadam)

adsl <- pharmaverseadam::adsl
adtr <- pharmaverseadam::adtr_onco # Has tumor measurements
adrs <- pharmaverseadam::adrs_onco # Has response evaluations
adlb <- pharmaverseadam::adlb
advs <- pharmaverseadam::advs

# ===============================================================================
# PREPARE ADSL - ADD DERIVED VARIABLES
# ===============================================================================

# Ensure TRT01P/TRT01A exist
if (!"TRT01P" %in% names(adsl)) {
  adsl <- adsl %>% mutate(TRT01P = ARM)
}
if (!"TRT01A" %in% names(adsl)) {
  adsl <- adsl %>% mutate(TRT01A = ACTARM)
}

# Create numeric treatment variables
adsl <- adsl %>%
  mutate(
    TRT01PN = case_when(
      TRT01P == "Placebo" ~ 0,
      TRT01P == "Xanomeline Low Dose" ~ 1,
      TRT01P == "Xanomeline High Dose" ~ 2,
      TRUE ~ 3
    ),
    TRT01AN = case_when(
      TRT01A == "Placebo" ~ 0,
      TRT01A == "Xanomeline Low Dose" ~ 1,
      TRT01A == "Xanomeline High Dose" ~ 2,
      TRUE ~ 3
    )
  )

# ===============================================================================
# DERIVE BASELINE COVARIATES
# ===============================================================================

## Numeric identifiers and demographics ----

adsl_cov <- adsl %>%
  mutate(
    # Study identifiers (numeric)
    STUDYIDN = as.numeric(word(USUBJID, 1, sep = fixed("-"))),
    SITEIDN = as.numeric(word(USUBJID, 2, sep = fixed("-"))),
    USUBJIDN = as.numeric(word(USUBJID, 3, sep = fixed("-"))),
    SUBJIDN = as.numeric(SUBJID),

    # Demographics (numeric)
    SEXN = case_when(SEX == "M" ~ 1, SEX == "F" ~ 2, TRUE ~ 3),
    RACEN = case_when(
      RACE == "AMERICAN INDIAN OR ALASKA NATIVE" ~ 1,
      RACE == "ASIAN" ~ 2,
      RACE == "BLACK OR AFRICAN AMERICAN" ~ 3,
      RACE == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ 4,
      RACE == "WHITE" ~ 5,
      TRUE ~ 6
    ),
    ETHNICN = case_when(
      ETHNIC == "HISPANIC OR LATINO" ~ 1,
      ETHNIC == "NOT HISPANIC OR LATINO" ~ 2,
      TRUE ~ 3
    ),

    # Age groups
    AGEGR1 = case_when(
      AGE < 65 ~ "<65",
      AGE >= 65 & AGE < 75 ~ "65-75",
      AGE >= 75 ~ ">75",
      TRUE ~ NA_character_
    ),
    AGEGR1N = case_when(
      AGE < 65 ~ 1,
      AGE >= 65 & AGE < 75 ~ 2,
      AGE >= 75 ~ 3,
      TRUE ~ NA_real_
    ),

    # Treatment (numeric)
    ARMN = case_when(
      ARM == "Placebo" ~ 0,
      ARM == "Xanomeline Low Dose" ~ 1,
      ARM == "Xanomeline High Dose" ~ 2,
      TRUE ~ 3
    ),
    ACTARMN = case_when(
      ACTARM == "Placebo" ~ 0,
      ACTARM == "Xanomeline Low Dose" ~ 1,
      ACTARM == "Xanomeline High Dose" ~ 2,
      TRUE ~ 3
    )
  )

## Add baseline vitals ----

adsl_vs <- adsl_cov %>%
  derive_vars_merged(
    dataset_add = advs,
    filter_add = PARAMCD == "HEIGHT" & ABLFL == "Y",
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(HTBL = AVAL)
  ) %>%
  derive_vars_merged(
    dataset_add = advs,
    filter_add = PARAMCD == "WEIGHT" & ABLFL == "Y",
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(WTBL = AVAL)
  ) %>%
  mutate(
    BMIBL = compute_bmi(height = HTBL, weight = WTBL),
    BSABL = compute_bsa(height = HTBL, weight = WTBL, method = "Mosteller"),
    WTBLGR1 = case_when(
      WTBL < 70 ~ "<70 kg",
      WTBL >= 70 ~ ">=70 kg",
      TRUE ~ NA_character_
    )
  )

## Add baseline labs ----

labs_bl <- adlb %>%
  filter(ABLFL == "Y" & PARAMCD %in% c("CREAT", "ALT", "AST", "BILI", "ALB")) %>%
  mutate(PARAMCDB = paste0(PARAMCD, "BL")) %>%
  select(STUDYID, USUBJID, PARAMCDB, AVAL)

adsl_vslb <- adsl_vs %>%
  derive_vars_transposed(
    dataset_merge = labs_bl,
    by_vars = exprs(STUDYID, USUBJID),
    key_var = PARAMCDB,
    value_var = AVAL
  ) %>%
  mutate(
    TBILBL = BILIBL,
    CRCLBL = compute_egfr(
      creat = CREATBL, creatu = "SI", age = AGE,
      weight = WTBL, sex = SEX, method = "CRCL"
    ),
    EGFRBL = compute_egfr(
      creat = CREATBL, creatu = "SI", age = AGE,
      weight = WTBL, sex = SEX, method = "CKD-EPI"
    )
  ) %>%
  select(-BILIBL)

# ===============================================================================
# DERIVE EXPOSURE METRICS
# ===============================================================================

source("config/exposure_config.R")
source("R/derive_exposure_metrics.R")

if (EXPOSURE_SOURCE == "adpc") {
  stop("ADPC dataset not configured. Set EXPOSURE_SOURCE to 'simulated' in config/exposure_config.R")
} else {
  adpc <- NULL
}

# Derive all exposure metrics
exposure_all <- derive_exposure_metrics(
  adsl_data = adsl_vslb,
  source = EXPOSURE_SOURCE,
  adpc_data = adpc,
  seed = EXPOSURE_SEED,
  tertile_var = TERTILE_VARIABLE
)

# Select ONLY subject-level variables (no tumor-related variables)
# This prevents conflicts with BASE, CHG, PCHG, AVAL, etc.
exposure_final <- exposure_all %>%
  select(
    # Keys
    STUDYID, STUDYIDN, USUBJID, USUBJIDN, SUBJID, SUBJIDN,
    SITEID, SITEIDN,

    # Treatment
    ARM, ARMN, ACTARM, ACTARMN,
    TRT01P, TRT01PN, TRT01A, TRT01AN,
    TRTSDT, TRTEDT, TRTDURD,

    # Demographics
    AGE, AGEGR1, AGEGR1N,
    SEX, SEXN,
    RACE, RACEN,
    ETHNIC, ETHNICN,

    # Exposure metrics (all 20 variables)
    DOSE, AUCSS, CMAXSS, CAVGSS, CMINSS, CLSS,
    AUCSLOG, CMXSLOG, CAVGLOG,
    AUCSSSTD, CMXSSSTD, AUCSSN,
    AUCSSDOS, CMXSSDOS,
    AUCSSCAT, AUCSCATN,
    AUCSSQ, AUCSSQN,
    AUCSSMED,

    # Baseline covariates
    WTBL, WTBLGR1, HTBL, BMIBL, BSABL,
    CREATBL, CRCLBL, EGFRBL,
    ALTBL, ASTBL, TBILBL, any_of("ALBBL")
  )

message("✓ Exposure dataset prepared: ", ncol(exposure_final), " variables")

# ===============================================================================
# CREATE TUMOR SIZE PARAMETER FROM ADTR
# ===============================================================================

# pharmaverseadam::adtr_onco already has BASE, CHG, PCHG calculated
# We'll use these directly rather than recalculating

# Get variable names for clean dropping
adsl_vars <- names(exposure_final)
adtr_vars <- names(adtr)
common_vars <- intersect(adsl_vars, adtr_vars)
vars_to_drop <- setdiff(common_vars, c("STUDYID", "USUBJID"))

message("Variables to drop from ADTR: ", paste(vars_to_drop, collapse = ", "))

tsize_final <- adtr %>%
  filter(PARAMCD == "SDIAM") %>%
  mutate(
    PARAMCD = "TSIZE",
    PARAM = "Target Lesions Sum of Diameters",
    PARAMN = 1
  ) %>%
  select(-any_of(vars_to_drop)) %>%
  derive_vars_merged(
    dataset_add = exposure_final,
    by_vars = exprs(STUDYID, USUBJID)
  )

message("✓ TSIZE records created: ", nrow(tsize_final))

# ===============================================================================
# ADD BOR FROM ADRS
# ===============================================================================

adrs_vars <- names(adrs)
common_vars_adrs <- intersect(adsl_vars, adrs_vars)
vars_to_drop_adrs <- setdiff(common_vars_adrs, c("STUDYID", "USUBJID"))

message("Variables to drop from ADRS: ", paste(vars_to_drop_adrs, collapse = ", "))

bor <- adrs %>%
  filter(PARAMCD == "BOR") %>%
  mutate(
    PARAMN = 2,
    # Create BORN from AVALC if AVALN doesn't exist
    BORN = if ("AVALN" %in% names(.)) {
      AVALN
    } else {
      case_when(
        AVALC == "CR" ~ 4,
        AVALC == "PR" ~ 3,
        AVALC == "SD" ~ 2,
        AVALC == "PD" ~ 1,
        AVALC == "NE" ~ 0,
        TRUE ~ NA_real_
      )
    }
  ) %>%
  select(-any_of(vars_to_drop_adrs)) %>%
  derive_vars_merged(
    dataset_add = exposure_final,
    by_vars = exprs(STUDYID, USUBJID)
  )

message("✓ BOR records created: ", nrow(bor))

# ===============================================================================
# DERIVE NADIR (8-CHAR VARIABLES)
# ===============================================================================

# Calculate nadir from TSIZE records
# Keep BASE, CHG, PCHG from the nadir timepoint
nadir <- tsize_final %>%
  filter(AVISITN > 0 & !is.na(AVAL)) %>%
  group_by(STUDYID, USUBJID) %>%
  filter(AVAL == min(AVAL, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    PARAMCD = "NADIR",
    PARAM = "Nadir Tumor Size",
    PARAMN = 3,
    NADIR = AVAL,
    NADPCHG = PCHG, # Keep PCHG at nadir
    NADVST = AVISIT # Keep visit of nadir
  )

message("✓ NADIR records created: ", nrow(nadir))

# ===============================================================================
# COMBINE ALL PARAMETERS
# ===============================================================================

adtrr_base <- bind_rows(
  tsize_final,
  bor,
  nadir
) %>%
  arrange(USUBJID, PARAMN, AVISITN)

message("✓ Combined dataset: ", nrow(adtrr_base), " records")

# ===============================================================================
# ADD ANALYSIS VARIABLES
# ===============================================================================

# Ensure AVALU exists before mutating
if (!"AVALU" %in% names(adtrr_base)) {
  adtrr_base <- adtrr_base %>%
    mutate(AVALU = NA_character_)
}

adtrr_prefinal <- adtrr_base %>%
  # Analysis flags
  mutate(
    # Baseline flag
    ABLFL = case_when(
      !is.na(ABLFL) ~ ABLFL,
      !is.na(AVISITN) & AVISITN == 0 ~ "Y",
      TRUE ~ ""
    ),

    # Post-baseline flag
    ANL01FL = if_else(!is.na(AVISITN) & AVISITN > 0, "Y", ""),

    # Responders (CR or PR)
    ANL02FL = if_else(!is.na(AVALC) & AVALC %in% c("CR", "PR"), "Y", ""),

    # Has change from baseline
    ANL03FL = if_else(!is.na(PCHG), "Y", "")
  ) %>%
  # Parameter categories
  mutate(
    PARCAT1 = "TUMOR RESPONSE",
    PARCAT2 = case_when(
      PARAMCD == "TSIZE" ~ "MEASUREMENT",
      PARAMCD == "BOR" ~ "OVERALL RESPONSE",
      PARAMCD == "NADIR" ~ "BEST RESPONSE",
      TRUE ~ NA_character_
    )
  ) %>%
  # Set AVALU (now guaranteed to exist)
  mutate(
    AVALU = case_when(
      !is.na(AVALU) & AVALU != "" ~ AVALU, # Keep existing non-empty
      PARAMCD == "TSIZE" ~ "mm",
      PARAMCD == "NADIR" ~ "mm",
      TRUE ~ NA_character_
    )
  ) %>%
  # Sequence number
  derive_var_obs_number(
    by_vars = exprs(STUDYID, USUBJID),
    order = exprs(PARAMN, AVISITN),
    new_var = ASEQ,
    check_type = "error"
  ) %>%
  # Select variables (8-char names)
  select(
    # Identifiers
    STUDYID, STUDYIDN, USUBJID, USUBJIDN, SUBJID, SUBJIDN,
    SITEID, SITEIDN,

    # Treatment
    ARM, ARMN, ACTARM, ACTARMN,
    TRT01P, TRT01PN, TRT01A, TRT01AN,
    TRTSDT, TRTEDT, TRTDURD,

    # Demographics
    AGE, AGEGR1, AGEGR1N,
    SEX, SEXN,
    RACE, RACEN,
    ETHNIC, ETHNICN,

    # Parameter information
    PARAMCD, PARAM, PARAMN,
    PARCAT1, PARCAT2,

    # Visit
    AVISITN, AVISIT,
    ADT, any_of("ADY"),

    # Analysis values
    AVAL, AVALU, AVALC, any_of("AVALN"),

    # Baseline and change
    BASE, CHG, PCHG,

    # Response (8-char names)
    any_of(c("BOR", "BORN", "NADIR", "NADPCHG", "NADVST")),

    # Flags
    ABLFL, ANL01FL, ANL02FL, ANL03FL,

    # Exposure - Primary
    DOSE, AUCSS, CMAXSS, CAVGSS, CMINSS, CLSS,

    # Exposure - Transformations (8-char names)
    AUCSLOG, CMXSLOG, CAVGLOG,
    AUCSSSTD, CMXSSSTD,
    AUCSSN,
    AUCSSDOS, CMXSSDOS,

    # Exposure - Categories (8-char names)
    AUCSSCAT, AUCSCATN,
    AUCSSQ, AUCSSQN,
    AUCSSMED,

    # Baseline covariates
    WTBL, WTBLGR1, HTBL, BMIBL, BSABL,
    CREATBL, CRCLBL, EGFRBL,
    ALTBL, ASTBL, TBILBL, any_of("ALBBL"),

    # Record identifiers
    ASEQ, any_of("DTYPE")
  ) %>%
  # Sort
  arrange(USUBJID, PARAMN, AVISITN)

# ===============================================================================
# APPLY METADATA AND PREPARE FOR EXPORT
# ===============================================================================

if (!is.null(metacore)) {
  # Apply metacore specifications
  adtrr <- adtrr_prefinal %>%
    drop_unspec_vars(metacore) %>%
    check_variables(metacore, strict = FALSE) %>%
    check_ct_data(metacore, na_acceptable = TRUE) %>%
    order_cols(metacore) %>%
    sort_by_key(metacore)

  message("✓ Metacore checks passed")
} else {
  # Use prefinal dataset if no metacore
  adtrr <- adtrr_prefinal
  message("⚠ Proceeding without metacore checks")
}

# ===============================================================================
# SAVE OUTPUT
# ===============================================================================

# Create output directory
dir <- "adam"
if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

# Save as RDS (R native format)
saveRDS(adtrr, file.path(dir, "adtrr.rds"))
message("✓ Saved: adam/adtrr.rds")

# Save as CSV (for review)
write.csv(adtrr, file.path(dir, "adtrr.csv"), row.names = FALSE, na = "")
message("✓ Saved: adam/adtrr.csv")

# Apply xportr and save as XPT
if (!is.null(metacore)) {
  # With metacore specifications
  adtrr_xpt <- adtrr %>%
    xportr_type(metacore, domain = "ADTRR") %>%
    xportr_length(metacore) %>%
    xportr_label(metacore) %>%
    xportr_format(metacore) %>%
    xportr_df_label(metacore) %>%
    xportr_write(file.path(dir, "adtrr.xpt"))

  message("✓ Saved: adam/adtrr.xpt (with metacore attributes)")
} else {
  # Without metacore specifications (basic XPT)
  if (requireNamespace("haven", quietly = TRUE)) {
    haven::write_xpt(adtrr, file.path(dir, "adtrr.xpt"), version = 5)
    message("✓ Saved: adam/adtrr.xpt (basic format)")
  }
}

# ===============================================================================
# SUMMARY OUTPUT
# ===============================================================================

cat("\n")
cat(strrep("=", 80), "\n", sep = "")
cat("ADTRR DERIVATION SUMMARY\n")
cat(strrep("=", 80), "\n", sep = "")
cat("\n")

cat("Dataset Structure:\n")
cat("  Total records:", nrow(adtrr), "\n")
cat("  Unique subjects:", length(unique(adtrr$USUBJID)), "\n")
cat("\n")

cat("Records by Parameter:\n")
param_summary <- adtrr %>%
  count(PARAMCD, PARAM) %>%
  arrange(PARAMCD)
print(param_summary)
cat("\n")

cat("Records by Visit (TSIZE only):\n")
visit_summary <- adtrr %>%
  filter(PARAMCD == "TSIZE") %>%
  count(AVISITN, AVISIT) %>%
  arrange(AVISITN)
print(visit_summary)
cat("\n")

cat("Response Distribution (BOR):\n")
if ("BOR" %in% adtrr$PARAMCD) {
  bor_summary <- adtrr %>%
    filter(PARAMCD == "BOR") %>%
    count(AVALC, name = "N") %>%
    mutate(Percent = round(100 * N / sum(N), 1))
  print(bor_summary)
} else {
  cat("  No BOR records found\n")
}
cat("\n")

cat("Exposure Categories:\n")
exp_summary <- adtrr %>%
  filter(!is.na(AUCSSCAT)) %>%
  distinct(USUBJID, .keep_all = TRUE) %>%
  count(AUCSSCAT, name = "N_Subjects")
print(exp_summary)
cat("\n")

cat("Analysis Flags:\n")
flag_summary <- adtrr %>%
  summarise(
    Baseline = sum(ABLFL == "Y", na.rm = TRUE),
    Post_Baseline = sum(ANL01FL == "Y", na.rm = TRUE),
    Responders = sum(ANL02FL == "Y", na.rm = TRUE),
    Has_Change = sum(ANL03FL == "Y", na.rm = TRUE)
  )
print(flag_summary)
cat("\n")

cat(strrep("=", 80), "\n", sep = "")
cat("ADTRR derivation complete!\n")
cat(strrep("=", 80), "\n", sep = "")

# ===============================================================================
# END OF PROGRAM
# ===============================================================================
