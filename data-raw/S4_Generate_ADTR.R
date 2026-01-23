#===============================================================================
# Script: S4_Generate_ADTR.R
#
# Purpose: Generate simulated ADTR (Tumor Measurements Analysis Dataset)
#
# Description: Creates synthetic tumor measurement data with realistic 
#              longitudinal changes based on treatment and exposure.
#              Generates sum of diameters (SDIAM) at multiple timepoints.
#
# Author: Jeff Dickinson
# Date: 2026-01-22
#
# Input: data/adsl_simulated.rds
#
# Output: data/adtr_simulated.rds
#
# Note: This script is called by S0_Generate_Example_Data.R but can also
#       be run standalone. Only generates data for oncology patients
#       (subset of ADSL).
#
#===============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)

# Source helper functions
source("R/simulation_functions.R")

# Set seed for reproducibility
set.seed(12348)  # Different seed from previous scripts

#===============================================================================
# CONFIGURATION
#===============================================================================

# Assessment schedule (weeks from baseline)
VISIT_WEEKS <- c(0, 6, 12, 18, 24, 30, 36, 42, 48)
VISIT_NAMES <- c("BASELINE", "WEEK 6", "WEEK 12", "WEEK 18", "WEEK 24",
                 "WEEK 30", "WEEK 36", "WEEK 42", "WEEK 48")

# Baseline tumor size parameters
BASELINE_MEAN <- 100  # mm
BASELINE_SD <- 30     # mm

# Growth/shrinkage rates per visit (% change)
GROWTH_RATES <- list(
  "Placebo" = 0.05,           # 5% growth per visit
  "Drug Low Dose" = -0.10,    # 10% shrinkage per visit
  "Drug High Dose" = -0.15    # 15% shrinkage per visit
)

# Exposure effect (additional shrinkage per SD increase in AUC)
EXPOSURE_EFFECT <- -0.002  # -0.2% per SD

# Proportion of subjects with oncology data (not all subjects have tumors)
ONCOLOGY_PROPORTION <- 0.30  # 30% of subjects

cat("\n")
cat(strrep("-", 80), "\n")
cat("Generating ADTR (Tumor Measurements Analysis Dataset)\n")
cat(strrep("-", 80), "\n")
cat("Configuration:\n")
cat("  Visits:", length(VISIT_WEEKS), "\n")
cat("  Visit schedule:", paste(VISIT_NAMES, collapse = ", "), "\n")
cat("  Baseline tumor mean:", BASELINE_MEAN, "mm\n")
cat("  Oncology proportion:", ONCOLOGY_PROPORTION * 100, "%\n")
cat(strrep("-", 80), "\n\n")

#===============================================================================
# STEP 1: LOAD ADSL
#===============================================================================

cat("Step 1: Loading ADSL...\n")

if (!file.exists("data/adsl_simulated.rds")) {
  stop("ADSL not found. Please run S1_Generate_ADSL.R first.")
}

adsl <- readRDS("data/adsl_simulated.rds")

cat("  ✓ ADSL loaded:", nrow(adsl), "subjects\n\n")

#===============================================================================
# STEP 2: SELECT ONCOLOGY SUBSET
#===============================================================================

cat("Step 2: Selecting oncology subset...\n")

# Randomly select subjects for oncology cohort
oncology_subjects <- adsl %>%
  slice_sample(prop = ONCOLOGY_PROPORTION) %>%
  pull(USUBJID)

adsl_onco <- adsl %>%
  filter(USUBJID %in% oncology_subjects)

cat("  ✓ Oncology subset selected:", nrow(adsl_onco), "subjects\n\n")

#===============================================================================
# STEP 3: SIMULATE EXPOSURE METRICS
#===============================================================================

cat("Step 3: Simulating exposure metrics...\n")

# Simulate steady-state AUC based on dose and covariates
adsl_exposure <- adsl_onco %>%
  mutate(
    # Dose from treatment arm
    DOSE = case_when(
      ARM == "Placebo" ~ 0,
      ARM == "Drug Low Dose" ~ 54,
      ARM == "Drug High Dose" ~ 81,
      TRUE ~ NA_real_
    ),
    
    # Simulate individual clearance
    CL_EST = 5 * (CRCL / 100)^0.75 * (WEIGHT / 70)^(-0.25),
    
    # Steady-state AUC with inter-individual variability (30% CV)
    AUCSS = if_else(
      DOSE > 0,
      (DOSE / CL_EST) * exp(rnorm(n(), 0, 0.3)),
      0
    )
  ) %>%
  select(-CL_EST)

# Standardize AUC for active treatment subjects only
aucss_active <- adsl_exposure %>% 
  filter(DOSE > 0) %>% 
  pull(AUCSS)

aucss_mean <- mean(aucss_active)
aucss_sd <- sd(aucss_active)

adsl_exposure <- adsl_exposure %>%
  mutate(
    AUCSSSTD = if_else(DOSE > 0, (AUCSS - aucss_mean) / aucss_sd, 0)
  )

cat("  ✓ Exposure metrics simulated\n")
cat("    Mean AUC (active):", round(aucss_mean, 2), "\n\n")

#===============================================================================
# STEP 4: SIMULATE BASELINE TUMOR SIZE
#===============================================================================

cat("Step 4: Simulating baseline tumor measurements...\n")

baseline_tumors <- adsl_exposure %>%
  mutate(
    # Baseline sum of diameters
    BASELINE_SDIAM = simulate_baseline_tumor(
      n = n(),
      mean_size = BASELINE_MEAN,
      sd_size = BASELINE_SD
    )
  )

cat("  ✓ Baseline tumors simulated\n")
cat("    Mean baseline:", round(mean(baseline_tumors$BASELINE_SDIAM), 1), "mm\n")
cat("    Range:", round(min(baseline_tumors$BASELINE_SDIAM), 1), "-",
    round(max(baseline_tumors$BASELINE_SDIAM), 1), "mm\n\n")

#===============================================================================
# STEP 5: CREATE VISIT SCHEDULE
#===============================================================================

cat("Step 5: Creating visit schedule...\n")

# Create visit grid
visit_grid <- tibble(
  AVISITN = seq_along(VISIT_WEEKS),
  AVISIT = VISIT_NAMES,
  VISIT_WEEK = VISIT_WEEKS,
  VISIT_DAY = VISIT_WEEKS * 7
)

cat("  ✓ Visit schedule created:", nrow(visit_grid), "visits\n\n")

#===============================================================================
# STEP 6: GENERATE LONGITUDINAL TUMOR MEASUREMENTS
#===============================================================================

cat("Step 6: Generating longitudinal tumor measurements...\n")

# Function to simulate tumor trajectory for one subject
simulate_subject_trajectory <- function(subject_data, visit_grid) {
  
  # Extract subject characteristics
  baseline_size <- subject_data$BASELINE_SDIAM
  arm <- subject_data$ARM
  exposure_std <- subject_data$AUCSSSTD
  trt_duration <- subject_data$TRTDURD
  
  # Get growth rate for this arm
  growth_rate <- GROWTH_RATES[[arm]]
  
  # Add exposure effect (for active arms)
  if (arm != "Placebo") {
    growth_rate <- growth_rate + EXPOSURE_EFFECT * exposure_std
  }
  
  # Generate measurements for each visit
  visit_grid %>%
    mutate(
      # Only measure if within treatment duration (+ 30 days follow-up)
      MEASURABLE = VISIT_DAY <= (trt_duration + 30),
      
      # Simulate tumor size with exponential growth/shrinkage + noise
      AVAL = if_else(
        MEASURABLE,
        baseline_size * (1 + growth_rate)^(AVISITN - 1) * exp(rnorm(n(), 0, 0.1)),
        NA_real_
      ),
      
      # Floor at 0 (complete response)
      AVAL = pmax(0, AVAL, na.rm = TRUE)
    ) %>%
    select(-MEASURABLE)
}

# Apply to all subjects
adtr_measurements <- baseline_tumors %>%
  rowwise() %>%
  mutate(
    measurements = list(simulate_subject_trajectory(cur_data(), visit_grid))
  ) %>%
  ungroup() %>%
  select(USUBJID, measurements) %>%
  unnest(measurements) %>%
  # Join back with subject data
  left_join(baseline_tumors, by = "USUBJID")

cat("  ✓ Longitudinal measurements generated\n")
cat("    Total records:", nrow(adtr_measurements), "\n")
cat("    Records with measurements:", sum(!is.na(adtr_measurements$AVAL)), "\n\n")

#===============================================================================
# STEP 7: ADD PARAMETER INFORMATION
#===============================================================================

cat("Step 7: Adding parameter information...\n")

adtr_with_param <- adtr_measurements %>%
  mutate(
    # Parameter (Sum of Diameters)
    PARAMCD = "SDIAM",
    PARAM = "Target Lesions Sum of Diameters",
    PARAMN = 1,
    
    # Analysis value unit
    AVALU = "mm",
    
    # Parameter category
    PARCAT1 = "TUMOR ASSESSMENT",
    PARCAT2 = "TARGET LESIONS"
  )

cat("  ✓ Parameter information added\n\n")

#===============================================================================
# STEP 8: DERIVE BASELINE AND CHANGE VARIABLES
#===============================================================================

cat("Step 8: Deriving baseline and change variables...\n")

adtr_with_change <- adtr_with_param %>%
  group_by(USUBJID, PARAMCD) %>%
  mutate(
    # Baseline (first non-missing value where AVISITN = 1)
    BASE = if_else(AVISITN == 1, AVAL, NA_real_),
    BASE = first(na.omit(BASE)),
    
    # Change from baseline
    CHG = if_else(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_),
    
    # Percent change from baseline
    PCHG = if_else(!is.na(AVAL) & !is.na(BASE) & BASE != 0, 
                   100 * (AVAL - BASE) / BASE, 
                   NA_real_),
    
    # Baseline flag
    ABLFL = if_else(AVISITN == 1 & !is.na(AVAL), "Y", "")
  ) %>%
  ungroup()

cat("  ✓ Baseline and change variables derived\n\n")

#===============================================================================
# STEP 9: ADD DATES
#===============================================================================

cat("Step 9: Adding analysis dates...\n")

adtr_with_dates <- adtr_with_change %>%
  mutate(
    # Analysis date (TRTSDT + visit day)
    ADT = TRTSDT + VISIT_DAY,
    
    # Analysis day
    ADY = VISIT_DAY + 1,  # Day 1 = first day of treatment
    
    # Character date
    ADTC = as.character(ADT)
  )

cat("  ✓ Analysis dates added\n\n")

#===============================================================================
# STEP 10: ADD ANALYSIS FLAGS
#===============================================================================

cat("Step 10: Adding analysis flags...\n")

adtr_with_flags <- adtr_with_dates %>%
  mutate(
    # Analysis flag (non-missing measurements)
    ANL01FL = if_else(!is.na(AVAL), "Y", ""),
    
    # Post-baseline flag
    ANL02FL = if_else(AVISITN > 1 & !is.na(AVAL), "Y", ""),
    
    # Has change from baseline
    ANL03FL = if_else(!is.na(PCHG), "Y", "")
  )

cat("  ✓ Analysis flags added\n\n")

#===============================================================================
# STEP 11: ADD IDENTIFIERS AND REORDER
#===============================================================================

cat("Step 11: Finalizing dataset...\n")

adtr_simulated <- adtr_with_flags %>%
  # Add sequence number
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup() %>%
  
  # Add numeric identifiers
  mutate(
    STUDYIDN = as.numeric(gsub("STUDY", "", STUDYID)),
    SITEIDN = as.numeric(SITEID),
    SUBJIDN = as.numeric(SUBJID),
    USUBJIDN = match(USUBJID, unique(USUBJID))
  ) %>%
  
  # Select and reorder columns
  select(
    # Identifiers
    STUDYID, STUDYIDN, USUBJID, USUBJIDN, SUBJID, SUBJIDN,
    SITEID, SITEIDN,
    ASEQ,
    
    # Treatment
    ARM, ARMN, ARMCD,
    TRT01P, TRT01PN,
    TRT01A, TRT01AN,
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
    
    # Dates
    ADT, ADY, ADTC,
    
    # Analysis values
    AVAL, AVALU,
    BASE, CHG, PCHG,
    
    # Exposure
    DOSE, AUCSS, AUCSSSTD,
    
    # Baseline covariates
    WEIGHT, WTGR1, HEIGHT, BMI, BSA,
    CREAT, CRCL, EGFR,
    
    # Analysis flags
    ABLFL, ANL01FL, ANL02FL, ANL03FL,
    
    # Population flags
    SAFFL, ITTFL
  )

cat("  ✓ Dataset finalized\n")
cat("    Total variables:", ncol(adtr_simulated), "\n")
cat("    Total records:", nrow(adtr_simulated), "\n\n")

#===============================================================================
# STEP 12: SAVE OUTPUT
#===============================================================================

cat("Step 12: Saving ADTR...\n")

# Create data directory if needed
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

# Save as RDS
saveRDS(adtr_simulated, "data/adtr_simulated.rds")

cat("  ✓ Saved: data/adtr_simulated.rds\n\n")

#===============================================================================
# SUMMARY
#===============================================================================

cat(strrep("-", 80), "\n")
cat("ADTR GENERATION SUMMARY\n")
cat(strrep("-", 80), "\n\n")

# Overall statistics
cat("Overall Statistics:\n")
cat("  Total records:", nrow(adtr_simulated), "\n")
cat("  Unique subjects:", length(unique(adtr_simulated$USUBJID)), "\n")
cat("  Visits per subject:", nrow(adtr_simulated) / length(unique(adtr_simulated$USUBJID)), "\n")
cat("  Records with measurements:", sum(!is.na(adtr_simulated$AVAL)), 
    "(", round(100 * mean(!is.na(adtr_simulated$AVAL)), 1), "%)\n\n")

# Baseline statistics
cat("Baseline Tumor Burden:\n")
baseline_stats <- adtr_simulated %>%
  filter(ABLFL == "Y") %>%
  summarise(
    N = n(),
    Mean = round(mean(AVAL, na.rm = TRUE), 1),
    SD = round(sd(AVAL, na.rm = TRUE), 1),
    Median = round(median(AVAL, na.rm = TRUE), 1),
    Min = round(min(AVAL, na.rm = TRUE), 1),
    Max = round(max(AVAL, na.rm = TRUE), 1)
  )
print(baseline_stats)
cat("\n")

# Change from baseline by visit and arm
cat("Median Percent Change from Baseline by Visit:\n")
pchg_summary <- adtr_simulated %>%
  filter(!is.na(PCHG)) %>%
  group_by(ARM, AVISIT) %>%
  summarise(
    N = n(),
    Median_PCHG = round(median(PCHG, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = ARM,
    values_from = Median_PCHG,
    id_cols = AVISIT
  )
print(pchg_summary, n = Inf)
cat("\n")

# Best response by arm
cat("Best Percent Change from Baseline by Arm:\n")
best_response <- adtr_simulated %>%
  filter(!is.na(PCHG)) %>%
  group_by(USUBJID, ARM) %>%
  summarise(
    Best_PCHG = min(PCHG, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ARM) %>%
  summarise(
    N = n(),
    Mean = round(mean(Best_PCHG), 1),
    Median = round(median(Best_PCHG), 1),
    Min = round(min(Best_PCHG), 1),
    Max = round(max(Best_PCHG), 1),
    .groups = "drop"
  )
print(best_response)
cat("\n")

# Exposure effect
cat("Median Best Response by Exposure Tertile (Active Arms):\n")
exposure_effect <- adtr_simulated %>%
  filter(DOSE > 0 & !is.na(PCHG)) %>%
  mutate(
    EXP_TERT = cut(
      AUCSSSTD,
      breaks = quantile(AUCSSSTD, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      labels = c("Low", "Medium", "High"),
      include.lowest = TRUE
    )
  ) %>%
  group_by(USUBJID, EXP_TERT) %>%
  summarise(
    Best_PCHG = min(PCHG, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(EXP_TERT) %>%
  summarise(
    N = n(),
    Median = round(median(Best_PCHG), 1),
    .groups = "drop"
  )
print(exposure_effect)

cat("\n")
cat(strrep("-", 80), "\n")
cat("ADTR generation complete!\n")
cat(strrep("-", 80), "\n\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================