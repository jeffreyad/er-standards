#===============================================================================
# Program: ADEE.R
# 
# Purpose: Create ADEE (Exposure-Efficacy Analysis Dataset)
#
# Description: Derives exposure-efficacy analysis dataset following BDS 
#              structure with one record per subject per parameter per timepoint.
#              Combines time-to-event endpoints (OS, PFS, TTP) with exposure 
#              metrics from population PK analysis.
#
# Input: 
#   - ADSL: Subject-level analysis dataset
#   - ADTTE: Time-to-event analysis dataset
#   - ADVS: Vital signs
#   - ADLB: Laboratory data
#
# Output: 
#   - ADEE: Exposure-efficacy analysis dataset
#
# Structure: BDS (Basic Data Structure)
#   - One record per subject per parameter per analysis timepoint
#   - PARAMCD identifies endpoint (OS, PFS, TTP, etc.)
#   - Standard ADaM BDS variables
#   - Multiple exposure representations
#
# Author: Jeff Dickinson
# Date: 2026-01-22
# Version: 1.2
#
# Note: Variable names limited to 8 characters for SAS compatibility
#
#===============================================================================

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

#===============================================================================
# LOAD SPECIFICATIONS (if available)
#===============================================================================

# Load metacore specifications
if (file.exists("specifications/ADEE_spec.xlsx")) {
  metacore <- spec_to_metacore("specifications/ADEE_spec.xlsx", 
                               where_sep_sheet = FALSE)
  message("✓ Loaded metacore specifications from Excel")
} else if (file.exists("specifications/ADEE_metacore.rds")) {
  metacore <- readRDS("specifications/ADEE_metacore.rds")
  message("✓ Loaded metacore specifications from RDS")
} else {
  message("⚠ No metacore specifications found - proceeding without")
  metacore <- NULL
}

#===============================================================================
# LOAD INPUT DATA
#===============================================================================

# Use haven::read_sas() or similar to read in production data
# For illustration, using pharmaverseadam example data
library(pharmaverseadam)

adsl <- pharmaverseadam::adsl
adtte <- pharmaverseadam::adtte_onco
adlb <- pharmaverseadam::adlb
advs <- pharmaverseadam::advs

#===============================================================================
# PREPARE ADSL - ADD DERIVED VARIABLES
#===============================================================================

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

# Ensure PARAMN exists in ADTTE
if (!"PARAMN" %in% names(adtte)) {
  adtte <- adtte %>%
    mutate(
      PARAMN = case_when(
        PARAMCD == "PFS" ~ 1,
        PARAMCD == "OS" ~ 2,
        PARAMCD == "TTP" ~ 3,
        PARAMCD == "TTNT" ~ 4,
        TRUE ~ 99
      )
    )
}

#===============================================================================
# DERIVE BASELINE COVARIATES
#===============================================================================

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

#===============================================================================
# DERIVE EXPOSURE METRICS (8-CHAR NAMES)
#===============================================================================

# In production, exposure would come from ADPC or PopPK analysis
# For demonstration, simulating exposure based on dose and covariates

set.seed(12345)

exposure_data <- adsl_vslb %>%
  mutate(
    # Dose from treatment arm
    DOSE = case_when(
      ARM == "Placebo" ~ 0,
      ARM == "Xanomeline Low Dose" ~ 54,
      ARM == "Xanomeline High Dose" ~ 81,
      TRUE ~ NA_real_
    ),
    
    # Simulate individual clearance
    CL_EST = 5 * (CRCLBL / 100)^0.75 * (WTBL / 70)^(-0.25),
    
    # Simulate steady-state AUC
    AUCSS = if_else(DOSE > 0, (DOSE / CL_EST) * exp(rnorm(n(), 0, 0.3)), 0),
    CMAXSS = if_else(DOSE > 0, AUCSS * 0.18 * exp(rnorm(n(), 0, 0.25)), 0),
    CAVGSS = if_else(DOSE > 0, AUCSS / 24, 0),
    CMINSS = if_else(DOSE > 0, CAVGSS * 0.6 * exp(rnorm(n(), 0, 0.35)), 0),
    CLSS = if_else(DOSE > 0, DOSE / AUCSS, NA_real_)
  ) %>%
  select(-CL_EST)

## Exposure transformations and categories (8-char names) ----

# Calculate statistics from active subjects only
aucss_active <- exposure_data %>% filter(DOSE > 0) %>% pull(AUCSS)
aucss_mean <- mean(aucss_active, na.rm = TRUE)
aucss_sd <- sd(aucss_active, na.rm = TRUE)
aucss_median <- median(aucss_active, na.rm = TRUE)

cmaxss_active <- exposure_data %>% filter(DOSE > 0) %>% pull(CMAXSS)
cmaxss_mean <- mean(cmaxss_active, na.rm = TRUE)
cmaxss_sd <- sd(cmaxss_active, na.rm = TRUE)

tertile_breaks <- quantile(aucss_active, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
quartile_breaks <- quantile(aucss_active, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

exposure_final <- exposure_data %>%
  mutate(
    # Log transformations (8 chars: remove one 'S')
    AUCSLOG = log(AUCSS + 0.01),     # AUCSSLOG → AUCSLOG
    CMXSLOG = log(CMAXSS + 0.01),    # CMAXSSLOG → CMXSLOG
    CAVGLOG = log(CAVGSS + 0.01),    # CAVGSSLOG → CAVGLOG
    
    # Standardized (z-score) - 8 chars exactly
    AUCSSSTD = if_else(DOSE > 0, (AUCSS - aucss_mean) / aucss_sd, NA_real_),
    CMXSSSTD = if_else(DOSE > 0, (CMAXSS - cmaxss_mean) / cmaxss_sd, NA_real_),
    
    # Normalized (ratio to median)
    AUCSSN = if_else(DOSE > 0, AUCSS / aucss_median, NA_real_),
    
    # Dose-normalized (8 chars: remove 'E')
    AUCSSDOS = if_else(DOSE > 0, AUCSS / DOSE, NA_real_),     # AUCSSDOSE → AUCSSDOS
    CMXSSDOS = if_else(DOSE > 0, CMAXSS / DOSE, NA_real_),    # CMAXSSDOSE → CMXSSDOS
    
    # Tertiles (8 chars exactly)
    AUCSSCAT = if_else(
      DOSE > 0,
      as.character(cut(AUCSS, breaks = tertile_breaks,
                       labels = c("Low", "Medium", "High"),
                       include.lowest = TRUE)),
      NA_character_
    ),
    AUCSCATN = case_when(   # AUCSSCATN → AUCSCATN
      AUCSSCAT == "Low" ~ 1,
      AUCSSCAT == "Medium" ~ 2,
      AUCSSCAT == "High" ~ 3,
      TRUE ~ NA_real_
    ),
    
    # Quartiles
    AUCSSQ = if_else(
      DOSE > 0,
      as.character(cut(AUCSS, breaks = quartile_breaks,
                       labels = c("Q1", "Q2", "Q3", "Q4"),
                       include.lowest = TRUE)),
      NA_character_
    ),
    AUCSSQN = case_when(
      AUCSSQ == "Q1" ~ 1,
      AUCSSQ == "Q2" ~ 2,
      AUCSSQ == "Q3" ~ 3,
      AUCSSQ == "Q4" ~ 4,
      TRUE ~ NA_real_
    ),
    
    # Above/below median (8 chars exactly)
    AUCSSMED = if_else(
      DOSE > 0,
      if_else(AUCSS >= aucss_median, "Above Median", "Below Median"),
      NA_character_
    )
  )

#===============================================================================
# CREATE ADEE BASE DATASET
#===============================================================================

# Get variable names from both datasets
adsl_vars <- names(exposure_final)
adtte_vars <- names(adtte)

# Find common variables
common_vars <- intersect(adsl_vars, adtte_vars)

# Remove key variables to get variables to drop
vars_to_drop <- setdiff(common_vars, c("STUDYID", "USUBJID"))

adee_base <- adtte %>%
  # Filter to efficacy endpoints
  filter(PARAMCD %in% c("OS", "PFS", "TTP", "TTNT")) %>%
  
  # Add derived variables
  mutate(
    EVENT = 1 - CNSR,
    AVALU = if_else(!is.na(AVAL), "DAYS", NA_character_),
    AVALC = case_when(
      EVENT == 1 ~ "EVENT",
      CNSR == 1 ~ "CENSORED",
      TRUE ~ NA_character_
    )
  ) %>%
  
  # Remove overlapping variables (use clean method)
  select(-any_of(vars_to_drop)) %>%
  
  # Merge exposure and covariates
  derive_vars_merged(
    dataset_add = exposure_final,
    by_vars = exprs(STUDYID, USUBJID)
  )

#===============================================================================
# ADD ANALYSIS VARIABLES
#===============================================================================

adee_prefinal <- adee_base %>%
  # Analysis timepoint
  mutate(
    ATPT = "BASELINE",
    ATPTN = 0,
    AVISIT = "BASELINE",
    AVISITN = 0
  ) %>%
  
  # Analysis flags
  mutate(
    ANL01FL = if_else(PARAMCD == "PFS", "Y", ""),
    ANL02FL = if_else(PARAMCD == "OS", "Y", ""),
    ANL03FL = if_else(PARAMCD == "TTP", "Y", ""),
    ANL04FL = if_else(PARAMCD == "TTNT", "Y", "")
  ) %>%
  
  # Parameter categories
  mutate(
    PARCAT1 = "EFFICACY",
    PARCAT2 = "TIME TO EVENT"
  ) %>%
  
  # Sequence number
  derive_var_obs_number(
    by_vars = exprs(STUDYID, USUBJID),
    order = exprs(PARAMN, AVISITN),
    new_var = ASEQ,
    check_type = "error"
  ) %>%
  
  # Select variables (with 8-char names)
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
    
    # Analysis values
    AVAL, AVALU, AVALC,
    
    # Dates
    STARTDT, ADT, any_of(c("ADY", "ADTF")),
    
    # Time-to-event specific
    CNSR, EVENT, EVNTDESC,
    any_of(c("SRCDOM", "SRCVAR", "SRCSEQ")),
    
    # Analysis timepoint
    ATPT, ATPTN, AVISIT, AVISITN,
    
    # Exposure - Primary
    DOSE, AUCSS, CMAXSS, CAVGSS, CMINSS, CLSS,
    
    # Exposure - Transformations (8-char names)
    AUCSLOG, CMXSLOG, CAVGLOG,      # Shortened from *SSLOG
    AUCSSSTD, CMXSSSTD,              # Exactly 8 chars
    AUCSSN,
    AUCSSDOS, CMXSSDOS,              # Shortened from *SSDOSE
    
    # Exposure - Categories (8-char names)
    AUCSSCAT, AUCSCATN,              # AUCSCATN shortened
    AUCSSQ, AUCSSQN,
    AUCSSMED,
    
    # Baseline covariates - Vitals
    WTBL, WTBLGR1, HTBL, BMIBL, BSABL,
    
    # Baseline covariates - Labs
    CREATBL, CRCLBL, EGFRBL,
    ALTBL, ASTBL, TBILBL, any_of("ALBBL"),
    
    # Analysis flags
    ANL01FL, ANL02FL, ANL03FL, ANL04FL,
    
    # Record identifiers
    ASEQ, any_of("DTYPE")
  ) %>%
  
  # Sort
  arrange(USUBJID, PARAMN, AVISITN)

#===============================================================================
# APPLY METADATA AND PREPARE FOR EXPORT
#===============================================================================

if (!is.null(metacore)) {
  # Apply metacore specifications
  adee <- adee_prefinal %>%
    drop_unspec_vars(metacore) %>%
    check_variables(metacore, strict = FALSE) %>%
    check_ct_data(metacore, na_acceptable = TRUE) %>%
    order_cols(metacore) %>%
    sort_by_key(metacore)
  
  message("✓ Metacore checks passed")
} else {
  # Use prefinal dataset if no metacore
  adee <- adee_prefinal
  message("⚠ Proceeding without metacore checks")
}

#===============================================================================
# SAVE OUTPUT
#===============================================================================

# Create output directory
dir <- "adam"
if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

# Save as RDS (R native format)
saveRDS(adee, file.path(dir, "adee.rds"))
message("✓ Saved: adam/adee.rds")

# Save as CSV (for review)
write.csv(adee, file.path(dir, "adee.csv"), row.names = FALSE, na = "")
message("✓ Saved: adam/adee.csv")

# Apply xportr and save as XPT
if (!is.null(metacore)) {
  # With metacore specifications
  adee_xpt <- adee %>%
    xportr_type(metacore, domain = "ADEE") %>%
    xportr_length(metacore) %>%
    xportr_label(metacore) %>%
    xportr_format(metacore) %>%
    xportr_df_label(metacore) %>%
    xportr_write(file.path(dir, "adee.xpt"))
  
  message("✓ Saved: adam/adee.xpt (with metacore attributes)")
} else {
  # Without metacore specifications (basic XPT)
  if (requireNamespace("haven", quietly = TRUE)) {
    haven::write_xpt(adee, file.path(dir, "adee.xpt"), version = 5)
    message("✓ Saved: adam/adee.xpt (basic format)")
  }
}

#===============================================================================
# END OF PROGRAM
#===============================================================================

adee <- adee_prefinal %>%
  drop_unspec_vars(metacore) %>% # Drop unspecified variables from specs
  check_variables(metacore, strict = FALSE) %>% # Check all variables specified are present and no more
  check_ct_data(metacore) %>% # Checks all variables with CT only contain values within the CT
  order_cols(metacore) %>% # Orders the columns according to the spec
  sort_by_key(metacore) # Sorts the rows by the sort keys

dir <- "data/adam" # Change to whichever directory you want to save the dataset in

adee_xpt <- adee %>%
  xportr_type(metacore, domain = "ADEE") %>% # Coerce variable type to match spec
  xportr_length(metacore) %>% # Assigns SAS length from a variable level metadata
  xportr_label(metacore) %>% # Assigns variable label from metacore specifications
  xportr_format(metacore) %>% # Assigns variable format from metacore specifications
  xportr_df_label(metacore) %>% # Assigns dataset label from metacore specifications
  xportr_write(file.path(dir, "adee.xpt")) # Write xpt v5 transport file

# Save output ----

save(adee, file = file.path(dir, "adee.rda"), compress = "bzip2")
