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
#   - ADTTE: Time-to-event analysis dataset (from ADaM)
#   - ADRS: Response analysis dataset (for additional endpoints)
#   - ADPC: Population PK dataset (or derived exposure metrics)
#   - ADVS: Vital signs (for baseline covariates)
#   - ADLB: Laboratory (for baseline covariates)
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
# Author: [Your Name]
# Date: 2026-01-20
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

# Prevent namespace conflicts
select <- dplyr::select
filter <- dplyr::filter

#===============================================================================
# SETUP
#===============================================================================

cat("\n")
cat(strrep("=", 80), "\n")
cat("ADEE DERIVATION - EXPOSURE-EFFICACY ANALYSIS DATASET\n")
cat(strrep("=", 80), "\n\n")

# Set ADaM date reference
reference_date <- ymd("2023-01-01")

#===============================================================================
# LOAD INPUT DATA
#===============================================================================

cat("Loading input datasets...\n\n")

# For demonstration, using pharmaverseadam example data
# In production, replace with actual data sources

library(pharmaverseadam)

adsl <- pharmaverseadam::adsl
adtte <- pharmaverseadam::adtte_onco
adrs <- pharmaverseadam::adrs_onco
adlb <- pharmaverseadam::adlb
advs <- pharmaverseadam::advs
adex <- pharmaverseadam::adex

cat("Input datasets loaded:\n")
cat("  ADSL:", nrow(adsl), "subjects\n")
cat("  ADTTE:", nrow(adtte), "records\n")
cat("  ADRS:", nrow(adrs), "records\n")
cat("  ADLB:", nrow(adlb), "records\n")
cat("  ADVS:", nrow(advs), "records\n")
cat("  ADEX:", nrow(adex), "records\n\n")

#===============================================================================
# DERIVE BASELINE COVARIATES
#===============================================================================

cat("Deriving baseline covariates...\n\n")

## Numeric versions of identifiers and demographics ----

adsl_cov <- adsl %>%
  mutate(
    # Study identifiers (numeric)
    STUDYIDN = as.numeric(word(USUBJID, 1, sep = fixed("-"))),
    SITEIDN = as.numeric(word(USUBJID, 2, sep = fixed("-"))),
    USUBJIDN = as.numeric(word(USUBJID, 3, sep = fixed("-"))),
    SUBJIDN = as.numeric(SUBJID),
    
    # Demographics (numeric)
    SEXN = case_when(
      SEX == "M" ~ 1,
      SEX == "F" ~ 2,
      TRUE ~ 3
    ),
    
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
    # Baseline BMI
    BMIBL = compute_bmi(height = HTBL, weight = WTBL),
    
    # Baseline BSA (Mosteller formula)
    BSABL = compute_bsa(
      height = HTBL,
      weight = WTBL,
      method = "Mosteller"
    ),
    
    # Weight groups
    WTBLGR1 = case_when(
      WTBL < 70 ~ "<70 kg",
      WTBL >= 70 ~ ">=70 kg",
      TRUE ~ NA_character_
    )
  )

## Add baseline labs ----

# Get baseline labs
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
    # Rename bilirubin
    TBILBL = BILIBL,
    
    # Creatinine clearance (Cockcroft-Gault)
    CRCLBL = compute_egfr(
      creat = CREATBL, 
      creatu = "SI", 
      age = AGE, 
      weight = WTBL, 
      sex = SEX,
      method = "CRCL"
    ),
    
    # eGFR (CKD-EPI)
    EGFRBL = compute_egfr(
      creat = CREATBL, 
      creatu = "SI", 
      age = AGE, 
      weight = WTBL, 
      sex = SEX,
      method = "CKD-EPI"
    )
  ) %>%
  select(-BILIBL)  # Remove original BILIBL, keep TBILBL

cat("Baseline covariates derived:\n")
cat("  Demographics: SEXN, RACEN, ETHNICN, AGEGR1\n")
cat("  Vitals: HTBL, WTBL, BMIBL, BSABL\n")
cat("  Labs: CREATBL, ALTBL, ASTBL, TBILBL, ALBBL\n")
cat("  Derived: CRCLBL, EGFRBL\n\n")

#===============================================================================
# DERIVE EXPOSURE METRICS
#===============================================================================

cat("Deriving exposure metrics...\n\n")

# In production, exposure metrics would come from ADPC or population PK analysis
# For this example, we'll simulate exposure based on dose and covariates

exposure_data <- adsl_vslb %>%
  mutate(
    # Get dose from treatment arm
    DOSE = case_when(
      ARM == "Placebo" ~ 0,
      ARM == "Xanomeline Low Dose" ~ 54,
      ARM == "Xanomeline High Dose" ~ 81,
      TRUE ~ NA_real_
    ),
    
    # Simulate individual clearance (L/h)
    # CL depends on renal function and weight
    CL_EST = 5 * (CRCLBL / 100)^0.75 * (WTBL / 70)^(-0.25),
    
    # Simulate steady-state AUC (dose/CL with variability)
    # In production, use actual ADPC values or PopPK predictions
    AUCSS = if_else(
      DOSE > 0,
      (DOSE / CL_EST) * exp(rnorm(n(), 0, 0.3)),  # 30% CV
      0
    ),
    
    # Simulate Cmax (proportional to AUC with different variability)
    CMAXSS = if_else(
      DOSE > 0,
      AUCSS * 0.18 * exp(rnorm(n(), 0, 0.25)),  # Ka-dependent
      0
    ),
    
    # Average concentration
    CAVGSS = if_else(
      DOSE > 0,
      AUCSS / 24,  # Assuming QD dosing
      0
    ),
    
    # Trough concentration (assume 2-compartment)
    CMINSS = if_else(
      DOSE > 0,
      CAVGSS * 0.6 * exp(rnorm(n(), 0, 0.35)),
      0
    ),
    
    # Individual clearance (for reference)
    CLSS = if_else(DOSE > 0, DOSE / AUCSS, NA_real_)
  ) %>%
  select(-CL_EST)  # Remove intermediate calculation

## Derive exposure transformations and categories ----

exposure_final <- exposure_data %>%
  mutate(
    # Log transformations
    AUCSSLOG = log(AUCSS + 0.01),  # Add small constant to handle zeros
    CMAXSSLOG = log(CMAXSS + 0.01),
    
    # Standardized (z-score) - only for non-placebo
    AUCSSSTD = if_else(
      DOSE > 0,
      scale(AUCSS[DOSE > 0])[,1],
      NA_real_
    ),
    
    # Normalized (ratio to median)
    AUCSSN = if_else(
      DOSE > 0,
      AUCSS / median(AUCSS[DOSE > 0], na.rm = TRUE),
      NA_real_
    ),
    
    # Dose-normalized
    AUCSSDOSE = if_else(DOSE > 0, AUCSS / DOSE, NA_real_)
  )

# Categorical exposure variables (only for non-placebo)
active_subjects <- exposure_final %>% filter(DOSE > 0)

# Tertiles
tertile_breaks <- quantile(active_subjects$AUCSS, 
                            probs = c(0, 1/3, 2/3, 1), 
                            na.rm = TRUE)

# Quartiles
quartile_breaks <- quantile(active_subjects$AUCSS,
                             probs = c(0, 0.25, 0.5, 0.75, 1),
                             na.rm = TRUE)

# Median
median_aucss <- median(active_subjects$AUCSS, na.rm = TRUE)

exposure_final <- exposure_final %>%
  mutate(
    # Tertiles
    AUCSSCAT = if_else(
      DOSE > 0,
      cut(AUCSS, 
          breaks = tertile_breaks,
          labels = c("Low", "Medium", "High"),
          include.lowest = TRUE),
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
      cut(AUCSS,
          breaks = quartile_breaks,
          labels = c("Q1", "Q2", "Q3", "Q4"),
          include.lowest = TRUE),
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
      if_else(AUCSS >= median_aucss, "Above Median", "Below Median"),
      NA_character_
    )
  )

cat("Exposure metrics derived:\n")
cat("  Primary: AUCSS, CMAXSS, CAVGSS, CMINSS, CLSS\n")
cat("  Transformations: AUCSSLOG, AUCSSSTD, AUCSSN, AUCSSDOSE\n")
cat("  Categories: AUCSSCAT, AUCSSQ, AUCSSMED\n\n")

#===============================================================================
# CREATE ADEE BASE DATASET
#===============================================================================

cat("Creating ADEE base dataset from ADTTE...\n\n")

## Filter to efficacy endpoints ----

adee_base <- adtte %>%
  # Keep time-to-event efficacy endpoints
  filter(PARAMCD %in% c("OS", "PFS", "TTP", "TTNT")) %>%
  
  # Add EVENT indicator (standard: 1=event, 0=censored)
  mutate(EVENT = 1 - CNSR) %>%
  
  # Merge exposure and covariates
  derive_vars_merged(
    dataset_add = exposure_final,
    by_vars = exprs(STUDYID, USUBJID)
  )

cat("ADEE base created:\n")
cat("  Records:", nrow(adee_base), "\n")
cat("  Subjects:", length(unique(adee_base$USUBJID)), "\n")
cat("  Parameters:", paste(unique(adee_base$PARAMCD), collapse = ", "), "\n\n")

#===============================================================================
# DERIVE ANALYSIS TIMEPOINT VARIABLES
#===============================================================================

cat("Deriving analysis timepoint variables...\n\n")

adee_timepoint <- adee_base %>%
  mutate(
    # Analysis timepoint (for this example, baseline)
    # In practice, could have multiple timepoints for time-varying exposure
    ATPT = "BASELINE",
    ATPTN = 0,
    
    # Analysis visit
    AVISIT = "BASELINE",
    AVISITN = 0
  )

#===============================================================================
# DERIVE ANALYSIS FLAGS
#===============================================================================

cat("Deriving analysis flags...\n\n")

adee_flags <- adee_timepoint %>%
  mutate(
    # ANL01FL: Primary efficacy endpoint (PFS)
    ANL01FL = if_else(PARAMCD == "PFS", "Y", ""),
    
    # ANL02FL: Secondary efficacy endpoint (OS)
    ANL02FL = if_else(PARAMCD == "OS", "Y", ""),
    
    # ANL03FL: Tertiary endpoint (TTP)
    ANL03FL = if_else(PARAMCD == "TTP", "Y", ""),
    
    # ANL04FL: Treatment discontinuation
    ANL04FL = if_else(PARAMCD == "TTNT", "Y", "")
  )

#===============================================================================
# ADD PARAMETER CATEGORIES
#===============================================================================

adee_parcat <- adee_flags %>%
  mutate(
    PARCAT1 = "EFFICACY",
    PARCAT2 = "TIME TO EVENT"
  )

#===============================================================================
# DERIVE SEQUENCE NUMBER
#===============================================================================

adee_seq <- adee_parcat %>%
  derive_var_obs_number(
    by_vars = exprs(STUDYID, USUBJID),
    order = exprs(PARAMN, AVISITN),
    new_var = ASEQ,
    check_type = "error"
  )

#===============================================================================
# SELECT AND ORDER VARIABLES
#===============================================================================

cat("Selecting and ordering final variables...\n\n")

adee <- adee_seq %>%
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
    
    # Analysis value (time-to-event)
    AVAL, AVALU, AVALC,
    
    # Dates and relative days
    STARTDT, ADT, ADTF, ADY,
    
    # Time-to-event specific
    CNSR, EVENT, EVNTDESC,
    SRCDOM, SRCVAR, SRCSEQ,
    
    # Analysis timepoint
    ATPT, ATPTN,
    AVISIT, AVISITN,
    
    # Exposure metrics - Primary
    DOSE,
    AUCSS, CMAXSS, CAVGSS, CMINSS, CLSS,
    
    # Exposure metrics - Transformations
    AUCSSLOG, AUCSSSTD, AUCSSN, AUCSSDOSE,
    CMAXSSLOG,
    
    # Exposure metrics - Categories
    AUCSSCAT, AUCSSCATN,
    AUCSSQ, AUCSSQN,
    AUCSSMED,
    
    # Baseline covariates - Vitals
    WTBL, WTBLGR1,
    HTBL, BMIBL, BSABL,
    
    # Baseline covariates - Labs
    CREATBL, CRCLBL, EGFRBL,
    ALTBL, ASTBL, TBILBL, ALBBL,
    
    # Analysis flags
    ANL01FL, ANL02FL, ANL03FL, ANL04FL,
    
    # Record identifiers
    ASEQ, DTYPE
  ) %>%
  arrange(USUBJID, PARAMN, AVISITN)

#===============================================================================
# DATA QUALITY CHECKS
#===============================================================================

cat("\n")
cat(strrep("=", 80), "\n")
cat("DATA QUALITY CHECKS\n")
cat(strrep("=", 80), "\n\n")

## Check 1: No duplicate keys ----
dup_check <- adee %>%
  count(USUBJID, PARAMCD, AVISITN) %>%
  filter(n > 1)

if (nrow(dup_check) > 0) {
  warning("DUPLICATE KEYS FOUND!")
  print(dup_check)
} else {
  cat("✓ No duplicate keys (USUBJID, PARAMCD, AVISITN)\n")
}

## Check 2: All subjects from ADSL present ----
missing_subj <- anti_join(
  adsl %>% select(USUBJID),
  adee %>% select(USUBJID) %>% distinct(),
  by = "USUBJID"
)

if (nrow(missing_subj) > 0) {
  warning("SUBJECTS FROM ADSL MISSING IN ADEE!")
  print(missing_subj)
} else {
  cat("✓ All ADSL subjects present in ADEE\n")
}

## Check 3: EVENT and CNSR consistency ----
event_check <- adee %>%
  filter(EVENT != (1 - CNSR))

if (nrow(event_check) > 0) {
  warning("EVENT AND CNSR INCONSISTENT!")
  print(event_check %>% select(USUBJID, PARAMCD, EVENT, CNSR))
} else {
  cat("✓ EVENT and CNSR are consistent (EVENT = 1 - CNSR)\n")
}

## Check 4: Exposure values reasonable ----
exposure_check <- adee %>%
  filter(DOSE > 0) %>%
  summarise(
    across(c(AUCSS, CMAXSS, CAVGSS),
           list(min = ~min(., na.rm = TRUE),
                max = ~max(., na.rm = TRUE),
                mean = ~mean(., na.rm = TRUE)))
  )

cat("\n✓ Exposure metrics summary (active treatment):\n")
print(exposure_check)

## Check 5: Analysis flags ----
flag_summary <- adee %>%
  summarise(
    ANL01FL_Y = sum(ANL01FL == "Y"),
    ANL02FL_Y = sum(ANL02FL == "Y"),
    ANL03FL_Y = sum(ANL03FL == "Y"),
    ANL04FL_Y = sum(ANL04FL == "Y")
  )

cat("\n✓ Analysis flags summary:\n")
print(flag_summary)

#===============================================================================
# SUMMARY STATISTICS
#===============================================================================

cat("\n")
cat(strrep("=", 80), "\n")
cat("ADEE SUMMARY\n")
cat(strrep("=", 80), "\n\n")

cat("Dataset: ADEE (Exposure-Efficacy Analysis Dataset)\n")
cat("Structure: BDS (Basic Data Structure)\n")
cat("Records:", nrow(adee), "\n")
cat("Subjects:", length(unique(adee$USUBJID)), "\n")
cat("Variables:", ncol(adee), "\n\n")

cat("Parameters (PARAMCD):\n")
param_summary <- adee %>%
  count(PARAMCD, PARAM, name = "N_Records") %>%
  mutate(N_Subjects = map_int(PARAMCD, ~length(unique(adee$USUBJID[adee$PARAMCD == .x]))))

print(param_summary)

cat("\n")
cat("Events by Parameter:\n")
event_summary <- adee %>%
  group_by(PARAMCD, PARAM) %>%
  summarise(
    N_Subjects = n(),
    N_Events = sum(EVENT),
    N_Censored = sum(CNSR),
    Event_Rate = mean(EVENT),
    .groups = "drop"
  )

print(event_summary)

cat("\n")
cat("Exposure Tertiles (Active Treatment):\n")
tertile_summary <- adee %>%
  filter(DOSE > 0 & !is.na(AUCSSCAT)) %>%
  count(AUCSSCAT, name = "N_Subjects") %>%
  mutate(Percent = round(N_Subjects / sum(N_Subjects) * 100, 1))

print(tertile_summary)

#===============================================================================
# SAVE OUTPUT
#===============================================================================

cat("\n")
cat(strrep("=", 80), "\n")
cat("SAVING OUTPUT\n")
cat(strrep("=", 80), "\n\n")

# Create output directory
if (!dir.exists("adam")) dir.create("adam", recursive = TRUE)

# Save as RDS (R native format)
saveRDS(adee, "adam/adee.rds")
cat("✓ Saved: adam/adee.rds\n")

# Save as SAS dataset (if haven is available)
if (requireNamespace("haven", quietly = TRUE)) {
  haven::write_xpt(adee, "adam/adee.xpt", version = 5)
  cat("✓ Saved: adam/adee.xpt\n")
}

# Save as CSV (for review)
write.csv(adee, "adam/adee.csv", row.names = FALSE, na = "")
cat("✓ Saved: adam/adee.csv\n")

# Save metadata
metadata <- list(
  dataset_name = "ADEE",
  dataset_label = "Exposure-Efficacy Analysis Dataset",
  creation_date = Sys.time(),
  n_records = nrow(adee),
  n_subjects = length(unique(adee$USUBJID)),
  n_variables = ncol(adee),
  parameters = unique(adee$PARAMCD),
  structure = "BDS (Basic Data Structure)",
  key_variables = c("USUBJID", "PARAMCD", "AVISITN")
)

saveRDS(metadata, "adam/adee_metadata.rds")
cat("✓ Saved: adam/adee_metadata.rds\n")

# Create basic data specs
data_specs <- data.frame(
  Variable = names(adee),
  Type = sapply(adee, class),
  Label = c(
    "Study Identifier", "Study Identifier (N)", 
    "Unique Subject Identifier", "Unique Subject Identifier (N)",
    "Subject Identifier", "Subject Identifier (N)",
    "Study Site Identifier", "Study Site Identifier (N)",
    rep("", ncol(adee) - 8)  # Placeholder for remaining
  )[1:ncol(adee)]
)

write.csv(data_specs, "adam/adee_specs.csv", row.names = FALSE)
cat("✓ Saved: adam/adee_specs.csv\n")

cat("\n")
cat(strrep("=", 80), "\n")
cat("ADEE DERIVATION COMPLETE\n")
cat(strrep("=", 80), "\n\n")

#===============================================================================
# END OF PROGRAM
#===============================================================================