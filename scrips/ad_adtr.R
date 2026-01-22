#===============================================================================
# Program: ADTRR.R
# 
# Purpose: Create ADTRR (Tumor Response Analysis Dataset)
#
# Description: Derives tumor response analysis dataset following BDS 
#              structure with longitudinal tumor measurements and RECIST 1.1
#              evaluations combined with exposure metrics.
#
# Input: 
#   - ADSL: Subject-level analysis dataset
#   - ADRS: Response analysis dataset (tumor measurements)
#   - ADVS: Vital signs (for baseline covariates)
#   - ADLB: Laboratory data (for baseline covariates)
#
# Output: 
#   - ADTRR: Tumor response analysis dataset
#
# Structure: BDS (Basic Data Structure)
#   - Multiple records per subject (one per parameter per visit)
#   - PARAMCD identifies measurement type (TUMSIZE, BOR, NADIR)
#   - Longitudinal tumor measurements with RECIST evaluations
#   - Exposure metrics at each assessment
#
# Author: Jeff Dickinson
# Date: 2026-01-21
# Version: 1.0
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
if (file.exists("specifications/ADTRR_spec.xlsx")) {
  metacore <- spec_to_metacore("specifications/ADTRR_spec.xlsx", 
                               where_sep_sheet = FALSE)
  message("✓ Loaded metacore specifications from Excel")
} else if (file.exists("specifications/ADTRR_metacore.rds")) {
  metacore <- readRDS("specifications/ADTRR_metacore.rds")
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
adrs <- pharmaverseadam::adrs_onco
adtr <- pharmaverseadam::adtr_onco
adlb <- pharmaverseadam::adlb
advs <- pharmaverseadam::advs

count(adrs, PARAMCD, PARAM)
count(adtr, PARAMCD, PARAM)

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
# DERIVE EXPOSURE METRICS
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

## Exposure transformations and categories ----

# Calculate statistics from active subjects only
aucss_active <- exposure_data %>% filter(DOSE > 0) %>% pull(AUCSS)
aucss_mean <- mean(aucss_active, na.rm = TRUE)
aucss_sd <- sd(aucss_active, na.rm = TRUE)
aucss_median <- median(aucss_active, na.rm = TRUE)

tertile_breaks <- quantile(aucss_active, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
quartile_breaks <- quantile(aucss_active, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

exposure_final <- exposure_data %>%
  mutate(
    # Log transformations
    AUCSSLOG = log(AUCSS + 0.01),
    CMAXSSLOG = log(CMAXSS + 0.01),
    CAVGSSLOG = log(CAVGSS + 0.01),
    
    # Standardized (z-score)
    AUCSSSTD = if_else(DOSE > 0, (AUCSS - aucss_mean) / aucss_sd, NA_real_),
    
    # Normalized (ratio to median)
    AUCSSN = if_else(DOSE > 0, AUCSS / aucss_median, NA_real_),
    
    # Dose-normalized
    AUCSSDOSE = if_else(DOSE > 0, AUCSS / DOSE, NA_real_),
    
    # Tertiles
    AUCSSCAT = if_else(
      DOSE > 0,
      as.character(cut(AUCSS, breaks = tertile_breaks,
                       labels = c("Low", "Medium", "High"),
                       include.lowest = TRUE)),
      NA_character_
    ),
    AUCSSCATN = case_when(
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
    
    # Above/below median
    AUCSSMED = if_else(
      DOSE > 0,
      if_else(AUCSS >= aucss_median, "Above Median", "Below Median"),
      NA_character_
    )
  )

#===============================================================================
# CREATE TUMOR SIZE PARAMETER (TUMSIZE)
#===============================================================================

# Filter to tumor assessment records from ADRS
# Assuming ADRS has PARAMCD for tumor measurements

adsl_vars <- names(adsl)
adtr_vars <- names(adtr)

# Get variables that are in both datasets
common_vars <- intersect(adsl_vars, adtr_vars)

# Remove the key variables you want to keep for joining
vars_to_drop <- setdiff(common_vars, c("STUDYID", "USUBJID"))

count(adtr, PARAMCD, PARAM)

tumsize_base <- adtr %>%
  # Filter to tumor size parameter (adjust PARAMCD as needed)
  filter(PARAMCD %in% c("SDIAM", "SLDINV", "SLDTRG", "SLDBSLN")) %>%
  
  # Create standardized TUMSIZE parameter
  mutate(
    PARAMCD = "TUMSIZE",
    PARAM = "Sum of Longest Diameters (mm)",
    PARAMN = 1
  ) %>%
  
  # Remove overlapping variables (keep ADSL versions)
  select(-any_of(vars_to_drop)) %>%
  # Merge exposure and covariates
  derive_vars_merged(
    dataset_add = exposure_final,
    by_vars = exprs(STUDYID, USUBJID)
  )

#===============================================================================
# DERIVE BASELINE AND CHANGE FROM BASELINE
#===============================================================================

tumsize_chg <- tumsize_base %>%
  # Derive baseline
  derive_var_base(
    by_vars = exprs(STUDYID, USUBJID, PARAMCD),
    source_var = AVAL,
    new_var = BASE,
    filter = ABLFL == "Y"
  ) %>%
  
  # Derive change from baseline
  derive_var_chg() %>%
  
  # Derive percent change from baseline
  derive_var_pchg()

#===============================================================================
# DERIVE BEST OVERALL RESPONSE (BOR)
#===============================================================================

# Derive confirmed BOR using RECIST 1.1
bor <- tumsize_chg %>%
  filter(AVISITN > 0) %>%  # Post-baseline only
  derive_param_confirmed_bor(
    dataset_adsl = adsl,
    filter_source = PARAMCD == "TUMSIZE",
    source_pd = AVALC == "PD",
    source_cr = AVALC == "CR",
    source_pr = AVALC == "PR",
    source_sd = AVALC == "SD",
    reference_date = TRTSDT,
    ref_start_window = 28,
    set_values_to = exprs(
      PARAMCD = "BOR",
      PARAM = "Best Overall Response",
      PARAMN = 2,
      AVAL = AVALN,
      AVALC = case_when(
        AVALN == 4 ~ "CR",
        AVALN == 3 ~ "PR",
        AVALN == 2 ~ "SD",
        AVALN == 1 ~ "PD",
        TRUE ~ NA_character_
      )
    )
  )

# Add BOR numeric and category
bor <- bor %>%
  mutate(
    BORN = AVALN,
    AVALC = case_when(
      BORN == 4 ~ "CR",
      BORN == 3 ~ "PR",
      BORN == 2 ~ "SD",
      BORN == 1 ~ "PD",
      TRUE ~ NA_character_
    )
  )

#===============================================================================
# DERIVE NADIR (BEST RESPONSE)
#===============================================================================

# Derive nadir (minimum tumor size post-baseline)
nadir <- tumsize_chg %>%
  filter(AVISITN > 0 & !is.na(AVAL)) %>%
  group_by(STUDYID, USUBJID) %>%
  filter(AVAL == min(AVAL, na.rm = TRUE)) %>%
  slice(1) %>%  # Take first occurrence if multiple
  ungroup() %>%
  mutate(
    PARAMCD = "NADIR",
    PARAM = "Nadir Tumor Size",
    PARAMN = 3,
    NADIR = AVAL,
    NADIR_PCHG = PCHG,
    NADIR_VISIT = AVISIT
  )

#===============================================================================
# COMBINE ALL PARAMETERS
#===============================================================================

adtrr_base <- bind_rows(
  tumsize_chg,
  bor,
  nadir
) %>%
  arrange(USUBJID, PARAMN, AVISITN)

#===============================================================================
# ADD ANALYSIS VARIABLES
#===============================================================================

adtrr_prefinal <- adtrr_base %>%
  # Analysis flags
  mutate(
    # ABLFL should exist from source data
    ABLFL = if_else(AVISITN == 0, "Y", ""),
    
    # Analysis flags
    ANL01FL = if_else(AVISITN > 0, "Y", ""),  # Post-baseline
    ANL02FL = if_else(AVALC %in% c("CR", "PR"), "Y", ""),  # Responders
    ANL03FL = if_else(!is.na(PCHG), "Y", "")  # Has change from baseline
  ) %>%
  
  # Parameter categories
  mutate(
    PARCAT1 = "TUMOR RESPONSE",
    PARCAT2 = case_when(
      PARAMCD == "TUMSIZE" ~ "MEASUREMENT",
      PARAMCD == "BOR" ~ "OVERALL RESPONSE",
      PARAMCD == "NADIR" ~ "BEST RESPONSE",
      TRUE ~ NA_character_
    )
  ) %>%
  
  # Ensure AVALU exists
  mutate(
    AVALU = if_else(PARAMCD == "TUMSIZE", "mm", NA_character_)
  ) %>%
  
  # Sequence number
  derive_var_obs_number(
    by_vars = exprs(STUDYID, USUBJID),
    order = exprs(PARAMN, AVISITN),
    new_var = ASEQ,
    check_type = "error"
  ) %>%
  
  # Select variables
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
    
    # Visit information
    AVISITN, AVISIT,
    ADT, any_of("ADY"),
    
    # Analysis values
    AVAL, AVALU, AVALC, any_of("AVALN"),
    
    # Baseline and change
    BASE, CHG, PCHG,
    
    # Response variables
    any_of(c("BOR", "BORN", "NADIR", "NADIR_PCHG", "NADIR_VISIT")),
    
    # Flags
    ABLFL, ANL01FL, ANL02FL, ANL03FL,
    
    # Exposure - Primary
    DOSE, AUCSS, CMAXSS, CAVGSS, CMINSS, CLSS,
    
    # Exposure - Transformations
    AUCSSLOG, AUCSSSTD, AUCSSN, AUCSSDOSE,
    
    # Exposure - Categories
    AUCSSCAT, AUCSSCATN,
    AUCSSQ, AUCSSQN,
    AUCSSMED,
    
    # Baseline covariates - Vitals
    WTBL, WTBLGR1, HTBL, BMIBL, BSABL,
    
    # Baseline covariates - Labs
    CREATBL, CRCLBL, EGFRBL,
    ALTBL, ASTBL, TBILBL, any_of("ALBBL"),
    
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

#===============================================================================
# SAVE OUTPUT
#===============================================================================

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
    xportr_write(file.path(dir, "adtr.xpt"))
  
  message("✓ Saved: adam/adtr.xpt (with metacore attributes)")
} else {
  # Without metacore specifications (basic XPT)
  if (requireNamespace("haven", quietly = TRUE)) {
    haven::write_xpt(adtr, file.path(dir, "adtr.xpt"), version = 5)
    message("✓ Saved: adam/adtr.xpt (basic format)")
  }
}

#===============================================================================
# END OF PROGRAM
#===============================================================================
```

---

## **Key Features of ADTRR:**

### **1. Three Parameter Types:**
- **TUMSIZE** (PARAMN=1): Longitudinal tumor measurements
  - Multiple visits per subject
  - BASE, CHG, PCHG calculated
  
- **BOR** (PARAMN=2): Best Overall Response
  - One record per subject
  - RECIST 1.1 criteria
  - CR, PR, SD, PD
  
- **NADIR** (PARAMN=3): Best response (minimum tumor size)
  - One record per subject
  - NADIR, NADIR_PCHG, NADIR_VISIT

### **2. RECIST 1.1 Implementation:**
- Uses `derive_param_confirmed_bor()` from admiralonco
- Requires confirmation of responses
- Handles progressive disease

### **3. Exposure Integration:**
- All exposure metrics included (AUCSS, categories, etc.)
- Enables E-R analysis at each assessment
- Waterfall plots by exposure category

### **4. Analysis Flags:**
- **ANL01FL**: Post-baseline records
- **ANL02FL**: Responders (CR/PR)
- **ANL03FL**: Records with change from baseline

---

## **Example Output Structure:**
```
USUBJID      PARAMCD   AVISITN  AVISIT     BASE  AVAL  CHG   PCHG   AVALC  AUCSS  BOR
001-001-001  TUMSIZE   0        BASELINE   100   100   0     0.0    SD     12.5   PR
001-001-001  TUMSIZE   1        WEEK 6     100   85    -15   -15.0  SD     12.5   PR
001-001-001  TUMSIZE   2        WEEK 12    100   68    -32   -32.0  PR     12.5   PR
001-001-001  TUMSIZE   3        WEEK 18    100   70    -30   -30.0  PR     12.5   PR
001-001-001  BOR       NA       NA         NA    3     NA    NA     PR     12.5   PR
001-001-001  NADIR     NA       WEEK 12    100   68    -32   -32.0  NA     12.5   PR