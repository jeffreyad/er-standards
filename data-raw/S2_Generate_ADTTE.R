# ===============================================================================
# Script: S2_Generate_ADTTE.R
#
# Purpose: Generate simulated ADTTE (Time-to-Event Analysis Dataset)
#
# Description: Creates synthetic time-to-event data with realistic survival
#              times and exposure-response relationships for OS, PFS, TTP,
#              and TTNT parameters.
#
# Author: Jeff Dickinson
# Date: 2026-01-22
#
# Input: data/adsl_simulated.rds
#
# Output: data/adtte_simulated.rds
#
# Note: This script is called by S0_Generate_Example_Data.R but can also
#       be run standalone.
#
# ===============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)

# Source helper functions
source("R/simulation_functions.R")

# Set seed for reproducibility
set.seed(12346) # Different seed from ADSL

# ===============================================================================
# CONFIGURATION
# ===============================================================================

# Event parameters to generate
EVENTS <- c("OS", "PFS", "TTP", "TTNT")

# Baseline median survival times by arm (days)
BASELINE_MEDIANS <- list(
  OS = c("Placebo" = 365, "Drug Low Dose" = 450, "Drug High Dose" = 540),
  PFS = c("Placebo" = 180, "Drug Low Dose" = 240, "Drug High Dose" = 300),
  TTP = c("Placebo" = 200, "Drug Low Dose" = 260, "Drug High Dose" = 320),
  TTNT = c("Placebo" = 210, "Drug Low Dose" = 270, "Drug High Dose" = 330)
)

# Hazard ratios per unit standardized exposure (benefit)
HR_PER_EXPOSURE <- list(
  OS = 0.97,
  PFS = 0.98,
  TTP = 0.98,
  TTNT = 0.98
)

# Administrative censoring times (days)
ADMIN_CENSOR <- list(
  OS = 1095, # 3 years
  PFS = 730, # 2 years
  TTP = 730, # 2 years
  TTNT = 730 # 2 years
)

cat("\n")
cat(strrep("-", 80), "\n")
cat("Generating ADTTE (Time-to-Event Analysis Dataset)\n")
cat(strrep("-", 80), "\n")
cat("Events:", paste(EVENTS, collapse = ", "), "\n")
cat(strrep("-", 80), "\n\n")

# ===============================================================================
# STEP 1: LOAD ADSL
# ===============================================================================

cat("Step 1: Loading ADSL...\n")

if (!file.exists("data/adsl_simulated.rds")) {
  stop("ADSL not found. Please run S1_Generate_ADSL.R first.")
}

adsl <- readRDS("data/adsl_simulated.rds")

cat("  ✓ ADSL loaded:", nrow(adsl), "subjects\n\n")

# ===============================================================================
# STEP 2: SIMULATE EXPOSURE METRICS
# ===============================================================================

cat("Step 2: Simulating exposure metrics...\n")

# Simulate steady-state AUC based on dose and covariates
adsl_exposure <- adsl %>%
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
    AUCSSSTD = if_else(DOSE > 0, (AUCSS - aucss_mean) / aucss_sd, NA_real_)
  )

cat("  ✓ Exposure metrics simulated\n")
cat("    Mean AUC (active):", round(aucss_mean, 2), "\n")
cat("    SD AUC (active):", round(aucss_sd, 2), "\n\n")

# ===============================================================================
# STEP 3: GENERATE TIME-TO-EVENT DATA
# ===============================================================================

cat("Step 3: Generating time-to-event parameters...\n")

# Function to generate one event type
generate_event <- function(event_type, adsl_data) {
  cat("  Generating", event_type, "...\n")

  # Get configuration for this event
  baseline_medians <- BASELINE_MEDIANS[[event_type]]
  hr_per_unit <- HR_PER_EXPOSURE[[event_type]]
  admin_censor <- ADMIN_CENSOR[[event_type]]

  # Generate event times by subject
  event_data <- adsl_data %>%
    mutate(
      # Baseline median for this subject's arm
      baseline_median = case_when(
        ARM == "Placebo" ~ baseline_medians["Placebo"],
        ARM == "Drug Low Dose" ~ baseline_medians["Drug Low Dose"],
        ARM == "Drug High Dose" ~ baseline_medians["Drug High Dose"],
        TRUE ~ baseline_medians["Placebo"]
      ),

      # Baseline hazard rate
      lambda_base = log(2) / baseline_median,

      # Adjust hazard by exposure (for active treatment)
      hr = if_else(
        !is.na(AUCSSSTD),
        hr_per_unit^AUCSSSTD,
        1.0
      ),
      lambda = lambda_base * hr,

      # Generate event time (exponential)
      event_time = rexp(n(), rate = lambda),

      # Administrative censoring (varies by subject)
      admin_censor_time = runif(n(), admin_censor * 0.8, admin_censor * 1.2),

      # Observed time and censor indicator
      AVAL = pmin(event_time, admin_censor_time, TRTDURD + 30), # Can't observe past treatment + 30 days
      CNSR = as.numeric(event_time > pmin(admin_censor_time, TRTDURD + 30)),

      # Event indicator (opposite of censor)
      EVENT = 1 - CNSR,

      # Analysis date (TRTSDT + AVAL)
      ADT = TRTSDT + AVAL,
      ADY = AVAL + 1, # Study day (start = 1)

      # Event description
      EVNTDESC = if_else(
        CNSR == 0,
        case_when(
          event_type == "OS" ~ "DEATH",
          event_type == "PFS" ~ "PROGRESSIVE DISEASE OR DEATH",
          event_type == "TTP" ~ "PROGRESSIVE DISEASE",
          event_type == "TTNT" ~ "NEXT TREATMENT",
          TRUE ~ "EVENT"
        ),
        "CENSORED"
      ),

      # Parameter information
      PARAMCD = event_type,
      PARAM = case_when(
        event_type == "OS" ~ "Overall Survival",
        event_type == "PFS" ~ "Progression-Free Survival",
        event_type == "TTP" ~ "Time to Progression",
        event_type == "TTNT" ~ "Time to Next Treatment",
        TRUE ~ event_type
      ),
      PARAMN = case_when(
        event_type == "OS" ~ 1,
        event_type == "PFS" ~ 2,
        event_type == "TTP" ~ 3,
        event_type == "TTNT" ~ 4,
        TRUE ~ 99
      )
    ) %>%
    select(
      # Identifiers
      STUDYID, USUBJID, SUBJID, SITEID,

      # Parameter
      PARAMCD, PARAM, PARAMN,

      # Analysis values
      AVAL, CNSR, EVENT, EVNTDESC,

      # Dates
      ADT, ADY,

      # Treatment
      ARM, ARMCD, TRT01P, TRT01A,
      TRTSDT, TRTEDT, TRTDURD,

      # Exposure
      DOSE, AUCSS, AUCSSSTD,

      # Demographics
      AGE, AGEGR1, SEX, RACE, ETHNIC,

      # Baseline covariates
      WEIGHT, HEIGHT, BMI, BSA,
      CREAT, CRCL, EGFR,

      # Flags
      SAFFL, ITTFL, EFFFL
    )

  # Event statistics
  n_events <- sum(event_data$EVENT)
  median_time <- median(event_data$AVAL)

  cat(
    "    Events:", n_events, "/", nrow(event_data),
    "(", round(100 * n_events / nrow(event_data), 1), "%)\n"
  )
  cat("    Median time:", round(median_time), "days\n")

  return(event_data)
}

# Generate all event types
adtte_list <- map(EVENTS, ~ generate_event(.x, adsl_exposure))

# Combine into single dataset
adtte_simulated <- bind_rows(adtte_list)

cat("\n  ✓ All time-to-event parameters generated\n")
cat("    Total records:", nrow(adtte_simulated), "\n\n")

# ===============================================================================
# STEP 4: ADD ADDITIONAL VARIABLES
# ===============================================================================

cat("Step 4: Adding additional analysis variables...\n")

adtte_simulated <- adtte_simulated %>%
  mutate(
    # Source variable
    SRCDOM = "ADSL",
    SRCVAR = "TRTSDT",

    # Analysis flag (all records for primary analysis)
    ANL01FL = "Y",

    # Parameter category
    PARCAT1 = "TIME-TO-EVENT",

    # AVALC (character version)
    AVALC = as.character(round(AVAL)),

    # Censor time description
    CNSDTDSC = if_else(
      CNSR == 1,
      "CENSORED AT LAST ASSESSMENT",
      NA_character_
    ),

    # Analysis visit
    AVISIT = "END OF TREATMENT",
    AVISITN = 99
  ) %>%
  # Add numeric identifiers
  left_join(
    adsl %>% select(USUBJID, STUDYIDN, SITEIDN, SUBJIDN, USUBJIDN),
    by = "USUBJID"
  )

cat("  ✓ Additional variables added\n\n")

# ===============================================================================
# STEP 5: REORDER COLUMNS
# ===============================================================================

cat("Step 5: Reordering columns...\n")

adtte_simulated <- adtte_simulated %>%
  select(
    # Identifiers
    STUDYID, STUDYIDN, USUBJID, USUBJIDN, SUBJID, SUBJIDN,
    SITEID, SITEIDN,

    # Treatment
    ARM, ARMCD,
    TRT01P, TRT01A,
    TRTSDT, TRTEDT, TRTDURD,

    # Demographics
    AGE, AGEGR1,
    SEX,
    RACE,
    ETHNIC,

    # Parameter information
    PARAMCD, PARAM, PARAMN,
    PARCAT1,

    # Visit
    AVISIT, AVISITN,

    # Analysis values
    AVAL, AVALC, CNSR, EVENT,

    # Dates
    ADT, ADY,

    # Event description
    EVNTDESC, CNSDTDSC,

    # Exposure
    DOSE, AUCSS, AUCSSSTD,

    # Baseline covariates
    WEIGHT, HEIGHT, BMI, BSA,
    CREAT, CRCL, EGFR,

    # Analysis flags
    ANL01FL, SAFFL, ITTFL, EFFFL,

    # Source
    SRCDOM, SRCVAR
  )

cat("  ✓ Columns reordered\n")
cat("    Total variables:", ncol(adtte_simulated), "\n\n")

# ===============================================================================
# STEP 6: SAVE OUTPUT
# ===============================================================================

cat("Step 6: Saving ADTTE...\n")

# Create data directory if needed
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

# Save as RDS
saveRDS(adtte_simulated, "data/adtte_simulated.rds")

cat("  ✓ Saved: data/adtte_simulated.rds\n\n")

# ===============================================================================
# SUMMARY
# ===============================================================================

cat(strrep("-", 80), "\n")
cat("ADTTE GENERATION SUMMARY\n")
cat(strrep("-", 80), "\n\n")

# Overall statistics
cat("Overall Statistics:\n")
cat("  Total records:", nrow(adtte_simulated), "\n")
cat("  Unique subjects:", length(unique(adtte_simulated$USUBJID)), "\n")
cat("  Parameters:", length(unique(adtte_simulated$PARAMCD)), "\n\n")

# Summary by parameter
cat("Summary by Parameter:\n")
param_summary <- adtte_simulated %>%
  group_by(PARAMCD, PARAM) %>%
  summarise(
    Records = n(),
    Events = sum(EVENT),
    Event_Rate = round(100 * sum(EVENT) / n(), 1),
    Median_Time = round(median(AVAL)),
    .groups = "drop"
  )

print(param_summary, n = Inf)

cat("\n")

# Summary by arm and parameter
cat("Event Rates by Treatment Arm:\n")
arm_summary <- adtte_simulated %>%
  group_by(ARM, PARAMCD) %>%
  summarise(
    Events = sum(EVENT),
    Total = n(),
    Event_Rate = round(100 * sum(EVENT) / n(), 1),
    Median = round(median(AVAL)),
    .groups = "drop"
  ) %>%
  arrange(PARAMCD, ARM)

print(arm_summary, n = Inf)

cat("\n")

# Exposure effect check (for active arms)
cat("Median Survival by Exposure Tertile (Active Arms Only):\n")
exposure_summary <- adtte_simulated %>%
  filter(!is.na(AUCSSSTD)) %>%
  mutate(
    EXP_TERT = cut(
      AUCSSSTD,
      breaks = quantile(AUCSSSTD, probs = c(0, 1 / 3, 2 / 3, 1), na.rm = TRUE),
      labels = c("Low", "Medium", "High"),
      include.lowest = TRUE
    )
  ) %>%
  group_by(PARAMCD, EXP_TERT) %>%
  summarise(
    N = n(),
    Events = sum(EVENT),
    Median = round(median(AVAL)),
    .groups = "drop"
  ) %>%
  arrange(PARAMCD, EXP_TERT)

print(exposure_summary, n = Inf)

cat("\n")
cat(strrep("-", 80), "\n")
cat("ADTTE generation complete!\n")
cat(strrep("-", 80), "\n\n")

# ===============================================================================
# END OF SCRIPT
# ===============================================================================
