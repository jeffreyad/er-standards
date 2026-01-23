#===============================================================================
# Script: S3_Generate_ADAE.R
#
# Purpose: Generate simulated ADAE (Adverse Events Analysis Dataset)
#
# Description: Creates synthetic adverse event data with realistic AE terms,
#              severity, relationship, and exposure-response relationships.
#
# Author: Jeff Dickinson
# Date: 2026-01-22
#
# Input: data/adsl_simulated.rds
#
# Output: data/adae_simulated.rds
#
# Note: This script is called by S0_Generate_Example_Data.R but can also
#       be run standalone.
#
#===============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)

# Source helper functions
source("R/simulation_functions.R")

# Set seed for reproducibility
set.seed(12347)  # Different seed from ADSL and ADTTE

#===============================================================================
# CONFIGURATION
#===============================================================================

# AE rate configuration
BASE_AE_RATE <- list(
  "Placebo" = 1.5,
  "Drug Low Dose" = 2.5,
  "Drug High Dose" = 3.5
)

# Exposure effect on AE rate (per unit standardized AUC)
EXPOSURE_EFFECT <- 0.015  # 1.5% increase per SD

cat("\n")
cat(strrep("-", 80), "\n")
cat("Generating ADAE (Adverse Events Analysis Dataset)\n")
cat(strrep("-", 80), "\n")
cat("Configuration:\n")
cat("  Base AE rate (Placebo):", BASE_AE_RATE[["Placebo"]], "\n")
cat("  Base AE rate (Low Dose):", BASE_AE_RATE[["Drug Low Dose"]], "\n")
cat("  Base AE rate (High Dose):", BASE_AE_RATE[["Drug High Dose"]], "\n")
cat("  Exposure effect:", EXPOSURE_EFFECT, "per SD\n")
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
# STEP 2: SIMULATE EXPOSURE METRICS
#===============================================================================

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
    AUCSSSTD = if_else(DOSE > 0, (AUCSS - aucss_mean) / aucss_sd, 0)
  )

cat("  ✓ Exposure metrics simulated\n")
cat("    Mean AUC (active):", round(aucss_mean, 2), "\n\n")

#===============================================================================
# STEP 3: SIMULATE NUMBER OF AEs PER SUBJECT
#===============================================================================

cat("Step 3: Simulating number of AEs per subject...\n")

adsl_with_ae_count <- adsl_exposure %>%
  mutate(
    # Base rate by arm
    base_rate = case_when(
      ARM == "Placebo" ~ BASE_AE_RATE[["Placebo"]],
      ARM == "Drug Low Dose" ~ BASE_AE_RATE[["Drug Low Dose"]],
      ARM == "Drug High Dose" ~ BASE_AE_RATE[["Drug High Dose"]],
      TRUE ~ BASE_AE_RATE[["Placebo"]]
    ),
    
    # Exposure effect (higher exposure = more AEs for active arms)
    exp_multiplier = if_else(
      DOSE > 0,
      1 + EXPOSURE_EFFECT * AUCSSSTD,
      1.0
    ),
    
    # Lambda for Poisson
    lambda = base_rate * exp_multiplier * (TRTDURD / 180),  # Adjust for treatment duration
    
    # Number of AEs (Poisson)
    N_AES = rpois(n(), lambda)
  )

total_aes <- sum(adsl_with_ae_count$N_AES)
subjects_with_aes <- sum(adsl_with_ae_count$N_AES > 0)

cat("  ✓ AE counts simulated\n")
cat("    Total AEs:", total_aes, "\n")
cat("    Subjects with AEs:", subjects_with_aes, "/", nrow(adsl), 
    "(", round(100 * subjects_with_aes / nrow(adsl), 1), "%)\n\n")

#===============================================================================
# STEP 4: EXPAND TO INDIVIDUAL AE RECORDS
#===============================================================================

cat("Step 4: Creating individual AE records...\n")

# Expand subjects to individual AE records
adae_base <- adsl_with_ae_count %>%
  filter(N_AES > 0) %>%
  rowwise() %>%
  mutate(AE_NUM = list(1:N_AES)) %>%
  unnest(AE_NUM) %>%
  ungroup() %>%
  select(-base_rate, -exp_multiplier, -lambda, -N_AES)

cat("  ✓ Expanded to", nrow(adae_base), "AE records\n\n")

#===============================================================================
# STEP 5: ASSIGN AE CHARACTERISTICS
#===============================================================================

cat("Step 5: Assigning AE characteristics...\n")

# Generate AE terms
ae_terms <- generate_ae_terms(nrow(adae_base))

# Assign severity
ae_severity <- assign_ae_severity(nrow(adae_base))

# Assign relationship (more likely related for active treatment)
ae_relationship <- adae_base %>%
  mutate(
    relationship_prob = case_when(
      ARM == "Placebo" ~ "low",
      ARM == "Drug Low Dose" ~ "medium",
      ARM == "Drug High Dose" ~ "high",
      TRUE ~ "low"
    )
  ) %>%
  rowwise() %>%
  mutate(
    AREL = case_when(
      relationship_prob == "low" ~ sample(
        c("NOT RELATED", "UNLIKELY RELATED", "POSSIBLE", "PROBABLE", "RELATED"),
        1, prob = c(0.40, 0.30, 0.20, 0.07, 0.03)
      ),
      relationship_prob == "medium" ~ sample(
        c("NOT RELATED", "UNLIKELY RELATED", "POSSIBLE", "PROBABLE", "RELATED"),
        1, prob = c(0.20, 0.25, 0.30, 0.15, 0.10)
      ),
      relationship_prob == "high" ~ sample(
        c("NOT RELATED", "UNLIKELY RELATED", "POSSIBLE", "PROBABLE", "RELATED"),
        1, prob = c(0.15, 0.20, 0.35, 0.20, 0.10)
      ),
      TRUE ~ "NOT RELATED"
    )
  ) %>%
  ungroup() %>%
  pull(AREL)

# Assign serious flag
ae_serious <- assign_serious(ae_severity)

# Combine AE characteristics
adae_with_ae_info <- adae_base %>%
  bind_cols(ae_terms) %>%
  mutate(
    # Severity
    ASEV = ae_severity,
    ASEVN = case_when(
      ASEV == "MILD" ~ 1,
      ASEV == "MODERATE" ~ 2,
      ASEV == "SEVERE" ~ 3,
      TRUE ~ NA_real_
    ),
    
    # Relationship
    AREL = ae_relationship,
    AERELN = case_when(
      AREL == "NOT RELATED" ~ 0,
      AREL == "UNLIKELY RELATED" ~ 1,
      AREL == "POSSIBLE" ~ 2,
      AREL == "PROBABLE" ~ 3,
      AREL == "RELATED" ~ 4,
      TRUE ~ NA_real_
    ),
    
    # Serious
    AESER = ae_serious,
    
    # Action taken
    AEACN = case_when(
      AESER == "Y" ~ sample(
        c("DOSE NOT CHANGED", "DOSE REDUCED", "DRUG INTERRUPTED", "DRUG WITHDRAWN"),
        n(), replace = TRUE, prob = c(0.30, 0.25, 0.30, 0.15)
      ),
      ASEV == "SEVERE" ~ sample(
        c("DOSE NOT CHANGED", "DOSE REDUCED", "DRUG INTERRUPTED", "DRUG WITHDRAWN"),
        n(), replace = TRUE, prob = c(0.40, 0.25, 0.25, 0.10)
      ),
      TRUE ~ sample(
        c("DOSE NOT CHANGED", "DOSE REDUCED", "DRUG INTERRUPTED"),
        n(), replace = TRUE, prob = c(0.80, 0.15, 0.05)
      )
    ),
    
    # Outcome
    AEOUT = case_when(
      AESER == "Y" ~ sample(
        c("RECOVERED/RESOLVED", "RECOVERING/RESOLVING", "NOT RECOVERED/NOT RESOLVED", "FATAL"),
        n(), replace = TRUE, prob = c(0.50, 0.30, 0.18, 0.02)
      ),
      TRUE ~ sample(
        c("RECOVERED/RESOLVED", "RECOVERING/RESOLVING", "NOT RECOVERED/NOT RESOLVED"),
        n(), replace = TRUE, prob = c(0.70, 0.20, 0.10)
      )
    )
  )

cat("  ✓ AE characteristics assigned\n")
cat("    Severity distribution:\n")
print(table(adae_with_ae_info$ASEV))
cat("\n    Serious AEs:", sum(adae_with_ae_info$AESER == "Y"), 
    "(", round(100 * mean(adae_with_ae_info$AESER == "Y"), 1), "%)\n\n")

#===============================================================================
# STEP 6: ASSIGN AE DATES
#===============================================================================

cat("Step 6: Assigning AE dates...\n")

adae_with_dates <- adae_with_ae_info %>%
  rowwise() %>%
  mutate(
    # AE start day (during treatment period)
    # Sample from 1 to minimum of treatment duration or 365 days
    AE_START_DAY = sample(1:min(TRTDURD, 365), 1),
    
    # AE duration (days) - longer for severe
    AE_DURATION = case_when(
      ASEV == "MILD" ~ rpois(1, 3) + 1,
      ASEV == "MODERATE" ~ rpois(1, 7) + 2,
      ASEV == "SEVERE" ~ rpois(1, 14) + 5,
      TRUE ~ 3
    )
  ) %>%
  ungroup() %>%
  mutate(
    # AE start date
    ASTDT = TRTSDT + AE_START_DAY,
    ASTDY = AE_START_DAY,
    
    # AE end date
    AENDT = ASTDT + AE_DURATION,
    AENDY = ASTDY + AE_DURATION,
    
    # Character versions
    AESTDTC = as.character(ASTDT),
    AEENDTC = as.character(AENDT),
    
    # Treatment-emergent flag (AE started during or after treatment)
    TRTEMFL = if_else(ASTDT >= TRTSDT & ASTDT <= TRTEDT + 30, "Y", "")
  ) %>%
  select(-AE_START_DAY, -AE_DURATION)

cat("  ✓ AE dates assigned\n")
cat("    Treatment-emergent AEs:", sum(adae_with_dates$TRTEMFL == "Y"),
    "(", round(100 * mean(adae_with_dates$TRTEMFL == "Y"), 1), "%)\n\n")

#===============================================================================
# STEP 8: REORDER COLUMNS
#===============================================================================

cat("Step 8: Reordering columns...\n")

adae_simulated <- adae_simulated %>%
  select(
    # Identifiers
    STUDYID, STUDYIDN, DOMAIN,
    USUBJID, USUBJIDN, SUBJID, SUBJIDN,
    SITEID, SITEIDN,
    AESEQ,
    
    # AE term
    AEDECOD, AEBODSYS,
    
    # Severity and relationship
    ASEV, ASEVN,
    AREL, AERELN,
    AESER,
    
    # Actions and outcome
    AEACN, AEOUT,
    
    # Dates
    ASTDT, AENDT,
    ASTDY, AENDY,
    AESTDTC, AEENDTC,
    
    # Flags
    TRTEMFL, AOCCFL,
    
    # Treatment
    ARM, ARMN, ARMCD,
    ACTARM, ACTARMN, ACTARMCD,
    TRT01P, TRT01PN,
    TRT01A, TRT01AN,
    TRTSDT, TRTEDT, TRTDURD,
    
    # Demographics
    AGE, AGEGR1, AGEGR1N,
    SEX, SEXN,
    RACE, RACEN,
    ETHNIC, ETHNICN,
    
    # Exposure
    DOSE, AUCSS, AUCSSSTD,
    
    # Baseline covariates
    WEIGHT, WTGR1, HEIGHT, BMI, BSA,
    CREAT, CRCL, EGFR,
    ALT, AST, TBILI, ALB,
    
    # Population flags
    SAFFL, ITTFL
  )

cat("  ✓ Columns reordered\n")
cat("    Total variables:", ncol(adae_simulated), "\n\n")

#===============================================================================
# STEP 9: SAVE OUTPUT
#===============================================================================

cat("Step 9: Saving ADAE...\n")

# Create data directory if needed
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

# Save as RDS
saveRDS(adae_simulated, "data/adae_simulated.rds")

cat("  ✓ Saved: data/adae_simulated.rds\n\n")

#===============================================================================
# SUMMARY
#===============================================================================

cat(strrep("-", 80), "\n")
cat("ADAE GENERATION SUMMARY\n")
cat(strrep("-", 80), "\n\n")

# Overall statistics
cat("Overall Statistics:\n")
cat("  Total AE records:", nrow(adae_simulated), "\n")
cat("  Unique subjects:", length(unique(adae_simulated$USUBJID)), "\n")
cat("  Subjects with AEs:", length(unique(adae_simulated$USUBJID)), "/", 
    nrow(adsl), "(", 
    round(100 * length(unique(adae_simulated$USUBJID)) / nrow(adsl), 1), "%)\n")
cat("  Mean AEs per subject (overall):", 
    round(nrow(adae_simulated) / nrow(adsl), 2), "\n")
cat("  Mean AEs per subject (with AEs):", 
    round(nrow(adae_simulated) / length(unique(adae_simulated$USUBJID)), 2), "\n\n")

# Severity distribution
cat("Severity Distribution:\n")
severity_dist <- adae_simulated %>%
  count(ASEV) %>%
  mutate(Percent = round(100 * n / sum(n), 1))
print(severity_dist)
cat("\n")

# Relationship distribution
cat("Relationship Distribution:\n")
rel_dist <- adae_simulated %>%
  count(AREL) %>%
  mutate(Percent = round(100 * n / sum(n), 1)) %>%
  arrange(desc(Percent))
print(rel_dist)
cat("\n")

# Serious AEs
cat("Serious AEs:\n")
cat("  Total serious:", sum(adae_simulated$AESER == "Y"), 
    "(", round(100 * mean(adae_simulated$AESER == "Y"), 1), "%)\n")
cat("  Subjects with SAE:", 
    length(unique(adae_simulated$USUBJID[adae_simulated$AESER == "Y"])), "\n\n")

# Top 10 AEs
cat("Top 10 Most Common AEs:\n")
top_aes <- adae_simulated %>%
  count(AEDECOD, AEBODSYS, sort = TRUE) %>%
  head(10) %>%
  mutate(Percent = round(100 * n / nrow(adae_simulated), 1))
print(top_aes, n = Inf)
cat("\n")

# AE rate by arm
cat("AE Rate by Treatment Arm:\n")
arm_summary <- adae_simulated %>%
  group_by(ARM) %>%
  summarise(
    Subjects = n_distinct(USUBJID),
    Total_AEs = n(),
    Mean_per_Subject = round(n() / n_distinct(USUBJID), 2),
    Serious_AEs = sum(AESER == "Y"),
    Severe_AEs = sum(ASEV == "SEVERE"),
    .groups = "drop"
  ) %>%
  arrange(ARM)
print(arm_summary)
cat("\n")

# Exposure effect check
cat("Mean AE Count by Exposure Tertile (Active Arms Only):\n")
exposure_summary <- adae_simulated %>%
  filter(DOSE > 0) %>%
  mutate(
    EXP_TERT = cut(
      AUCSSSTD,
      breaks = quantile(AUCSSSTD, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      labels = c("Low", "Medium", "High"),
      include.lowest = TRUE
    )
  ) %>%
  group_by(EXP_TERT) %>%
  summarise(
    Subjects = n_distinct(USUBJID),
    Total_AEs = n(),
    Mean_per_Subject = round(n() / n_distinct(USUBJID), 2),
    .groups = "drop"
  )
print(exposure_summary)

cat("\n")
cat(strrep("-", 80), "\n")
cat("ADAE generation complete!\n")
cat(strrep("-", 80), "\n\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================