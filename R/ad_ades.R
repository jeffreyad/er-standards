#===============================================================================
# Program: ADES.R
# 
# Purpose: Create ADES (Exposure-Safety Analysis Dataset)
#
# Description: Derives exposure-safety analysis dataset following BDS 
#              structure with multiple records per subject per adverse event.
#              Combines adverse event data with exposure metrics for 
#              exposure-safety analysis.
#
# Input: 
#   - ADSL: Subject-level analysis dataset
#   - ADAE: Adverse events analysis dataset
#   - ADVS: Vital signs (for baseline covariates)
#   - ADLB: Laboratory data (for baseline covariates)
#
# Output: 
#   - ADES: Exposure-safety analysis dataset
#
# Structure: BDS (Basic Data Structure)
#   - Multiple records per subject (one per parameter per event)
#   - PARAMCD identifies safety endpoint (TEAE, TRAE, SAE, etc.)
#   - Multi-level: subject-level + event-level parameters
#   - Exposure metrics at subject and event level
#
# Author: Jeff Dickinson
# Date: 2026-01-22
# Version: 1.1
#
# Note: Variable names limited to 8 characters for SAS compatibility
#       Updated for pharmaverseadam ADAE (uses ASEV/ASEVN not AETOXGR)
#
#===============================================================================

# Load required packages
library(admiral)
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
if (file.exists("specifications/ADES_spec.xlsx")) {
  metacore <- spec_to_metacore("specifications/ADES_spec.xlsx", 
                               where_sep_sheet = FALSE)
  message("✓ Loaded metacore specifications from Excel")
} else if (file.exists("specifications/ADES_metacore.rds")) {
  metacore <- readRDS("specifications/ADES_metacore.rds")
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
adae <- pharmaverseadam::adae
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

## Exposure transformations (8-char names) ----

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
    # Log transformations (8 chars)
    AUCSLOG = log(AUCSS + 0.01),
    CMXSLOG = log(CMAXSS + 0.01),
    CAVGLOG = log(CAVGSS + 0.01),
    
    # Standardized (8 chars exactly)
    AUCSSSTD = if_else(DOSE > 0, (AUCSS - aucss_mean) / aucss_sd, NA_real_),
    CMXSSSTD = if_else(DOSE > 0, (CMAXSS - cmaxss_mean) / cmaxss_sd, NA_real_),
    
    # Normalized
    AUCSSN = if_else(DOSE > 0, AUCSS / aucss_median, NA_real_),
    
    # Dose-normalized (8 chars)
    AUCSSDOS = if_else(DOSE > 0, AUCSS / DOSE, NA_real_),
    CMXSSDOS = if_else(DOSE > 0, CMAXSS / DOSE, NA_real_),
    
    # Tertiles (8 chars)
    AUCSSCAT = if_else(
      DOSE > 0,
      as.character(cut(AUCSS, breaks = tertile_breaks,
                       labels = c("Low", "Medium", "High"),
                       include.lowest = TRUE)),
      NA_character_
    ),
    AUCSCATN = case_when(
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
    
    # Median split (8 chars)
    AUCSSMED = if_else(
      DOSE > 0,
      if_else(AUCSS >= aucss_median, "Above Median", "Below Median"),
      NA_character_
    )
  )

#===============================================================================
# CREATE SUBJECT-LEVEL SAFETY PARAMETERS
#===============================================================================

# Derive subject-level summary parameters from ADAE
# NOTE: pharmaverseadam uses ASEV/ASEVN (severity) not AETOXGR/AETOXGRN

## Any TEAE (Treatment-Emergent Adverse Event) ----
teae_any <- adsl %>%
  select(STUDYID, USUBJID) %>%
  mutate(
    PARAMCD = "TEAE",
    PARAM = "Any Treatment-Emergent Adverse Event",
    PARAMN = 1,
    AVAL = if_else(
      USUBJID %in% (adae %>% filter(TRTEMFL == "Y") %>% pull(USUBJID)),
      1, 0
    ),
    AVALC = if_else(AVAL == 1, "Y", "N")
  )

## Any Severe TEAE (using ASEVN = 3 for SEVERE) ----
teae_sev <- adsl %>%
  select(STUDYID, USUBJID) %>%
  mutate(
    PARAMCD = "TEAESEV",  # Using ASEVN instead of AETOXGRN
    PARAM = "Any Severe Treatment-Emergent Adverse Event",
    PARAMN = 2,
    AVAL = if_else(
      USUBJID %in% (adae %>% filter(TRTEMFL == "Y" & ASEVN == 3) %>% pull(USUBJID)),
      1, 0
    ),
    AVALC = if_else(AVAL == 1, "Y", "N")
  )

## Any Serious AE ----
sae_any <- adsl %>%
  select(STUDYID, USUBJID) %>%
  mutate(
    PARAMCD = "SAE",
    PARAM = "Any Serious Adverse Event",
    PARAMN = 3,
    AVAL = if_else(
      USUBJID %in% (adae %>% filter(AESER == "Y") %>% pull(USUBJID)),
      1, 0
    ),
    AVALC = if_else(AVAL == 1, "Y", "N")
  )

## Any Treatment-Related AE (using AREL character values) ----
trae_any <- adsl %>%
  select(STUDYID, USUBJID) %>%
  mutate(
    PARAMCD = "TRAE",
    PARAM = "Any Treatment-Related Adverse Event",
    PARAMN = 4,
    AVAL = if_else(
      USUBJID %in% (adae %>% 
        filter(AREL %in% c("POSSIBLE", "PROBABLE", "RELATED")) %>% 
        pull(USUBJID)),
      1, 0
    ),
    AVALC = if_else(AVAL == 1, "Y", "N")
  )

## AE Leading to Treatment Discontinuation ----
aedcn <- adsl %>%
  select(STUDYID, USUBJID) %>%
  mutate(
    PARAMCD = "AEDCN",
    PARAM = "AE Leading to Treatment Discontinuation",
    PARAMN = 5,
    AVAL = if_else(
      USUBJID %in% (adae %>% filter(AEACN == "DRUG WITHDRAWN") %>% pull(USUBJID)),
      1, 0
    ),
    AVALC = if_else(AVAL == 1, "Y", "N")
  )

# Combine subject-level parameters
subject_params <- bind_rows(
  teae_any,
  teae_sev,
  sae_any,
  trae_any,
  aedcn
)

#===============================================================================
# CREATE EVENT-LEVEL PARAMETERS
#===============================================================================

# Get variable names for clean dropping
adsl_vars <- names(exposure_final)
adae_vars <- names(adae)
common_vars <- intersect(adsl_vars, adae_vars)
vars_to_drop <- setdiff(common_vars, c("STUDYID", "USUBJID"))

# Create event-level records from ADAE
# NOTE: Using actual pharmaverseadam variables (ASEV/ASEVN, AREL)
event_params <- adae %>%
  filter(TRTEMFL == "Y") %>%  # Treatment-emergent only
  mutate(
    PARAMCD = "AETERM",
    PARAM = "Adverse Event Term",
    PARAMN = 10,
    AVAL = 1,  # Event occurred
    AVALC = "Y",
    
    # Keep AE-specific variables (8-char names)
    # Using actual pharmaverseadam ADAE variables
    AEDECOD = AEDECOD,    # Preferred term
    AEBODSYS = AEBODSYS,  # System organ class
    AESEV = ASEV,         # Severity (char): MILD, MODERATE, SEVERE
    AESEVN = ASEVN,       # Severity (num): 1, 2, 3
    AESER = AESER,        # Serious flag: Y/N
    AEREL = AREL,         # Relationship (char): NOT RELATED, POSSIBLE, etc.
    
    # Create numeric relationship for analysis
    AERELN = case_when(
      AREL == "NOT RELATED" ~ 0,
      AREL == "UNLIKELY RELATED" ~ 1,
      AREL == "POSSIBLE" ~ 2,
      AREL == "PROBABLE" ~ 3,
      AREL == "RELATED" ~ 4,
      TRUE ~ NA_real_
    ),
    
    AESTDT = ASTDT,       # AE start date (8 chars)
    AEENDT = AENDT        # AE end date (8 chars)
  ) %>%
  select(-any_of(vars_to_drop))

#===============================================================================
# COMBINE SUBJECT AND EVENT LEVELS
#===============================================================================

# Ensure all AE-specific variables exist in subject_params (as NA)
# This prevents issues when binding with event_params
subject_params_complete <- subject_params %>%
  derive_vars_merged(
    dataset_add = exposure_final,
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  mutate(
    # Add event-level variables as NA for subject-level records
    AESTDT = as.Date(NA),
    AEENDT = as.Date(NA),
    AEDECOD = NA_character_,
    AEBODSYS = NA_character_,
    AESEV = NA_character_,
    AESEVN = NA_integer_,
    AESER = NA_character_,
    AEREL = NA_character_,
    AERELN = NA_real_
  )

event_params_complete <- event_params %>%
  derive_vars_merged(
    dataset_add = exposure_final,
    by_vars = exprs(STUDYID, USUBJID)
  )

# Combine both levels
ades_base <- bind_rows(
  subject_params_complete,
  event_params_complete
)

#===============================================================================
# ADD ANALYSIS VARIABLES
#===============================================================================

ades_prefinal <- ades_base %>%
  # Analysis flags
  mutate(
    ANL01FL = if_else(PARAMCD == "TEAE", "Y", ""),
    ANL02FL = if_else(PARAMCD == "TEAESEV", "Y", ""),
    ANL03FL = if_else(PARAMCD == "SAE", "Y", ""),
    ANL04FL = if_else(PARAMCD == "TRAE", "Y", ""),
    ANL05FL = if_else(PARAMCD == "AETERM", "Y", "")
  ) %>%
  
  # Parameter categories
  mutate(
    PARCAT1 = "SAFETY",
    PARCAT2 = case_when(
      PARAMN <= 5 ~ "SUBJECT-LEVEL",
      PARAMN >= 10 ~ "EVENT-LEVEL",
      TRUE ~ NA_character_
    )
  ) %>%
  
  # Analysis timepoint
  mutate(
    AVISIT = if_else(PARAMN <= 5, "OVERALL", "AT EVENT"),
    AVISITN = if_else(PARAMN <= 5, 99, 0)
  ) %>%
  
  # Sort and create sequence number
  # Use coalesce to handle NA dates (puts them first in sort)
  arrange(USUBJID, PARAMN, coalesce(AESTDT, as.Date("1900-01-01"))) %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup() %>%
  
  # Select and order variables
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
    
    # Visit/timepoint
    AVISIT, AVISITN,
    
    # Analysis values
    AVAL, AVALC,
    
    # AE-specific variables (all present now, NA for subject-level)
    AEDECOD, AEBODSYS, AESEV, AESEVN,
    AESER, AEREL, AERELN, AESTDT, AEENDT,
    
    # Analysis flags
    ANL01FL, ANL02FL, ANL03FL, ANL04FL, ANL05FL,
    
    # Exposure - Primary
    DOSE, AUCSS, CMAXSS, CAVGSS, CMINSS, CLSS,
    
    # Exposure - Transformations
    AUCSLOG, CMXSLOG, CAVGLOG,
    AUCSSSTD, CMXSSSTD,
    AUCSSN,
    AUCSSDOS, CMXSSDOS,
    
    # Exposure - Categories
    AUCSSCAT, AUCSCATN,
    AUCSSQ, AUCSSQN,
    AUCSSMED,
    
    # Baseline covariates
    WTBL, WTBLGR1, HTBL, BMIBL, BSABL,
    CREATBL, CRCLBL, EGFRBL,
    ALTBL, ASTBL, TBILBL, any_of("ALBBL"),
    
    # Record identifiers
    ASEQ, any_of("DTYPE")
  )

#===============================================================================
# APPLY METADATA AND PREPARE FOR EXPORT
#===============================================================================

if (!is.null(metacore)) {
  # Apply metacore specifications
  ades <- ades_prefinal %>%
    drop_unspec_vars(metacore) %>%
    check_variables(metacore, strict = FALSE) %>%
    check_ct_data(metacore, na_acceptable = TRUE) %>%
    order_cols(metacore) %>%
    sort_by_key(metacore)
  
  message("✓ Metacore checks passed")
} else {
  # Use prefinal dataset if no metacore
  ades <- ades_prefinal
  message("⚠ Proceeding without metacore checks")
}

#===============================================================================
# SAVE OUTPUT
#===============================================================================

# Create output directory
dir <- "adam"
if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

# Save as RDS (R native format)
saveRDS(ades, file.path(dir, "ades.rds"))
message("✓ Saved: adam/ades.rds")

# Save as CSV (for review)
write.csv(ades, file.path(dir, "ades.csv"), row.names = FALSE, na = "")
message("✓ Saved: adam/ades.csv")

# Apply xportr and save as XPT
if (!is.null(metacore)) {
  # With metacore specifications
  ades_xpt <- ades %>%
    xportr_type(metacore, domain = "ADES") %>%
    xportr_length(metacore) %>%
    xportr_label(metacore) %>%
    xportr_format(metacore) %>%
    xportr_df_label(metacore) %>%
    xportr_write(file.path(dir, "ades.xpt"))
  
  message("✓ Saved: adam/ades.xpt (with metacore attributes)")
} else {
  # Without metacore specifications (basic XPT)
  if (requireNamespace("haven", quietly = TRUE)) {
    haven::write_xpt(ades, file.path(dir, "ades.xpt"), version = 5)
    message("✓ Saved: adam/ades.xpt (basic format)")
  }
}

#===============================================================================
# END OF PROGRAM
#===============================================================================