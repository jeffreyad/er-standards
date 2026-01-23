#===============================================================================
# Script: S5_Generate_ADRS.R
#
# Purpose: Generate simulated ADRS (Response Analysis Dataset)
#
# Description: Creates synthetic response evaluation data based on ADTR tumor
#              measurements. Derives RECIST 1.1 response assessments including
#              BOR (Best Overall Response), confirmed response, and response
#              at each timepoint.
#
# Author: Jeff Dickinson
# Date: 2026-01-22
#
# Input: 
#   - data/adsl_simulated.rds
#   - data/adtr_simulated.rds
#
# Output: data/adrs_simulated.rds
#
# Note: This script is called by S0_Generate_Example_Data.R but can also
#       be run standalone. Requires ADTR to be generated first.
#
#===============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)

# Source helper functions
source("R/simulation_functions.R")

# Set seed for reproducibility
set.seed(12349)  # Different seed from previous scripts

#===============================================================================
# CONFIGURATION
#===============================================================================

# RECIST 1.1 criteria
RECIST_CR_THRESHOLD <- -100  # Complete response (100% reduction)
RECIST_PR_THRESHOLD <- -30   # Partial response (≥30% reduction)
RECIST_PD_THRESHOLD <- 20    # Progressive disease (≥20% increase)
RECIST_PD_ABSOLUTE <- 5      # Absolute increase of ≥5mm

# Response confirmation window (weeks)
CONFIRMATION_WINDOW <- 4

cat("\n")
cat(strrep("-", 80), "\n")
cat("Generating ADRS (Response Analysis Dataset)\n")
cat(strrep("-", 80), "\n")
cat("Configuration:\n")
cat("  RECIST 1.1 criteria:\n")
cat("    CR:", RECIST_CR_THRESHOLD, "%\n")
cat("    PR: ≤", RECIST_PR_THRESHOLD, "%\n")
cat("    PD: ≥", RECIST_PD_THRESHOLD, "% and ≥", RECIST_PD_ABSOLUTE, "mm\n")
cat("    SD: Between PR and PD\n")
cat("  Confirmation window:", CONFIRMATION_WINDOW, "weeks\n")
cat(strrep("-", 80), "\n\n")

#===============================================================================
# STEP 1: LOAD ADSL AND ADTR
#===============================================================================

cat("Step 1: Loading input datasets...\n")

if (!file.exists("data/adsl_simulated.rds")) {
  stop("ADSL not found. Please run S1_Generate_ADSL.R first.")
}

if (!file.exists("data/adtr_simulated.rds")) {
  stop("ADTR not found. Please run S4_Generate_ADTR.R first.")
}

adsl <- readRDS("data/adsl_simulated.rds")
adtr <- readRDS("data/adtr_simulated.rds")

cat("  ✓ ADSL loaded:", nrow(adsl), "subjects\n")
cat("  ✓ ADTR loaded:", nrow(adtr), "records\n")
cat("  ✓ ADTR subjects:", length(unique(adtr$USUBJID)), "\n\n")

#===============================================================================
# STEP 2: DERIVE RESPONSE AT EACH VISIT (RECIST 1.1)
#===============================================================================

cat("Step 2: Deriving RECIST 1.1 response at each visit...\n")

# Calculate nadir (minimum tumor size so far) for PD assessment
adrs_visit <- adtr %>%
  filter(PARAMCD == "SDIAM" & !is.na(AVAL)) %>%
  arrange(USUBJID, AVISITN) %>%
  group_by(USUBJID) %>%
  mutate(
    # Step 1: Calculate cumulative minimum (includes current visit)
    CUM_MIN = cummin(AVAL),
    
    # Step 2: Nadir is previous visit's cumulative minimum
    # For visit 1, nadir = baseline
    # For visit 2+, nadir = lag(cumulative min)
    NADIR = case_when(
      AVISITN == 1 ~ BASE,                    # First visit
      TRUE ~ lag(CUM_MIN, default = BASE[1])  # Subsequent visits
    )
  ) %>%
  select(-CUM_MIN) %>%  # Remove temporary variable
  ungroup() %>%
  mutate(
    # Percent change from baseline
    PCHG_BASE = if_else(BASE > 0, 100 * (AVAL - BASE) / BASE, NA_real_),
    
    # Percent change from nadir
    PCHG_NADIR = if_else(NADIR > 0, 100 * (AVAL - NADIR) / NADIR, NA_real_),
    
    # Absolute change from nadir
    CHG_NADIR = AVAL - NADIR,
    
    # RECIST 1.1 response determination
    AVALC = case_when(
      # Complete Response: All lesions disappeared
      AVAL == 0 ~ "CR",
      
      # Partial Response: ≥30% decrease from baseline
      PCHG_BASE <= RECIST_PR_THRESHOLD ~ "PR",
      
      # Progressive Disease: ≥20% increase from nadir AND ≥5mm absolute increase
      PCHG_NADIR >= RECIST_PD_THRESHOLD & CHG_NADIR >= RECIST_PD_ABSOLUTE ~ "PD",
      
      # Stable Disease: Everything else
      TRUE ~ "SD"
    ),
    
    # Numeric version
    AVALN = case_when(
      AVALC == "CR" ~ 4,
      AVALC == "PR" ~ 3,
      AVALC == "SD" ~ 2,
      AVALC == "PD" ~ 1,
      TRUE ~ 0
    ),
    
    # Parameter
    PARAMCD = "OVR",
    PARAM = "Overall Response by Visit",
    PARAMN = 1
  ) %>%
  select(
    # Identifiers
    STUDYID, USUBJID, SUBJID, SITEID,
    
    # Parameter
    PARAMCD, PARAM, PARAMN,
    
    # Visit
    AVISITN, AVISIT,
    
    # Dates
    ADT, ADY,
    
    # Response
    AVALC, AVALN,
    
    # Supporting values
    AVAL, BASE, NADIR, PCHG_BASE, PCHG_NADIR,
    
    # Treatment
    ARM, ARMN, TRT01P, TRT01A,
    TRTSDT, TRTEDT, TRTDURD,
    
    # Exposure
    DOSE, AUCSS, AUCSSSTD,
    
    # Demographics
    AGE, AGEGR1, SEX, RACE, ETHNIC,
    
    # Baseline covariates
    WEIGHT, HEIGHT, BMI, BSA,
    CREAT, CRCL, EGFR,
    
    # Flags
    SAFFL, ITTFL
  )

cat("  ✓ Response at each visit derived\n")
cat("    Total visit records:", nrow(adrs_visit), "\n\n")

# Response distribution at last visit
cat("  Response distribution at last visit:\n")
last_visit_response <- adrs_visit %>%
  group_by(USUBJID) %>%
  filter(AVISITN == max(AVISITN)) %>%
  ungroup() %>%
  count(AVALC, ARM) %>%
  pivot_wider(names_from = ARM, values_from = n, values_fill = 0)
print(last_visit_response)
cat("\n")

#===============================================================================
# STEP 3: DERIVE CONFIRMED RESPONSE
#===============================================================================

cat("Step 3: Deriving confirmed response...\n")

# Confirmed response requires consecutive assessments ≥4 weeks apart
adrs_confirmed <- adrs_visit %>%
  arrange(USUBJID, AVISITN) %>%
  group_by(USUBJID) %>%
  mutate(
    # Next visit response
    NEXT_AVALC = lead(AVALC),
    NEXT_AVISITN = lead(AVISITN),
    
    # Weeks between visits
    WEEKS_BETWEEN = (NEXT_AVISITN - AVISITN) * 6,  # Approximate (visits are ~6 weeks apart)
    
    # Confirmed if same response at next visit and ≥4 weeks apart
    CONFIRMED = if_else(
      !is.na(NEXT_AVALC) & AVALC == NEXT_AVALC & WEEKS_BETWEEN >= CONFIRMATION_WINDOW,
      "Y",
      "N"
    ),
    
    # Confirmed response (only CR/PR can be confirmed per RECIST)
    AVALC_CONF = case_when(
      AVALC %in% c("CR", "PR") & CONFIRMED == "Y" ~ paste0(AVALC, " (Confirmed)"),
      TRUE ~ AVALC
    )
  ) %>%
  select(-NEXT_AVALC, -NEXT_AVISITN, -WEEKS_BETWEEN) %>%
  ungroup()

cat("  ✓ Confirmed response derived\n\n")

#===============================================================================
# STEP 4: DERIVE BEST OVERALL RESPONSE (BOR)
#===============================================================================

cat("Step 4: Deriving Best Overall Response (BOR)...\n")

# BOR is the best response recorded from start to end of treatment
adrs_bor <- adrs_confirmed %>%
  group_by(USUBJID) %>%
  summarise(
    # Best response (highest AVALN)
    AVALN = max(AVALN, na.rm = TRUE),
    AVALC = case_when(
      AVALN == 4 ~ "CR",
      AVALN == 3 ~ "PR",
      AVALN == 2 ~ "SD",
      AVALN == 1 ~ "PD",
      TRUE ~ "NE"
    ),
    
    # Was it confirmed?
    BOR_CONFIRMED = if_else(
      any(AVALC_CONF %in% c("CR (Confirmed)", "PR (Confirmed)")),
      "Y",
      "N"
    ),
    
    # Best percent change
    BEST_PCHG = min(PCHG_BASE, na.rm = TRUE),
    
    # Visit of best response
    BOR_VISIT = AVISIT[which.max(AVALN)],
    BOR_AVISITN = AVISITN[which.max(AVALN)],
    BOR_ADT = ADT[which.max(AVALN)],
    
    # Keep first values for subject-level variables
    STUDYID = first(STUDYID),
    SUBJID = first(SUBJID),
    SITEID = first(SITEID),
    ARM = first(ARM),
    ARMN = first(ARMN),
    TRT01P = first(TRT01P),
    TRT01A = first(TRT01A),
    TRTSDT = first(TRTSDT),
    TRTEDT = first(TRTEDT),
    TRTDURD = first(TRTDURD),
    DOSE = first(DOSE),
    AUCSS = first(AUCSS),
    AUCSSSTD = first(AUCSSSTD),
    AGE = first(AGE),
    AGEGR1 = first(AGEGR1),
    SEX = first(SEX),
    RACE = first(RACE),
    ETHNIC = first(ETHNIC),
    WEIGHT = first(WEIGHT),
    HEIGHT = first(HEIGHT),
    BMI = first(BMI),
    BSA = first(BSA),
    CREAT = first(CREAT),
    CRCL = first(CRCL),
    EGFR = first(EGFR),
    SAFFL = first(SAFFL),
    ITTFL = first(ITTFL),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Parameter
    PARAMCD = "BOR",
    PARAM = "Best Overall Response",
    PARAMN = 2,
    
    # Visit (overall)
    AVISIT = "OVERALL",
    AVISITN = 99,
    
    # Date is date of BOR
    ADT = BOR_ADT,
    ADY = as.numeric(BOR_ADT - TRTSDT) + 1
  )

cat("  ✓ Best Overall Response derived\n")
cat("    Total BOR records:", nrow(adrs_bor), "\n\n")

# BOR distribution
cat("  BOR distribution by treatment arm:\n")
bor_dist <- adrs_bor %>%
  count(AVALC, ARM) %>%
  pivot_wider(names_from = ARM, values_from = n, values_fill = 0)
print(bor_dist)
cat("\n")

#===============================================================================
# STEP 5: COMBINE VISIT-LEVEL AND BOR RECORDS
#===============================================================================

cat("Step 5: Combining visit-level and BOR records...\n")

# Prepare visit-level records
adrs_visit_final <- adrs_confirmed %>%
  mutate(
    PARAMCD = "OVR",
    PARAM = "Overall Response by Visit",
    PARAMN = 1
  ) %>%
  select(
    STUDYID, USUBJID, SUBJID, SITEID,
    PARAMCD, PARAM, PARAMN,
    AVISITN, AVISIT,
    ADT, ADY,
    AVALC, AVALN,
    ARM, ARMN, TRT01P, TRT01A,
    TRTSDT, TRTEDT, TRTDURD,
    DOSE, AUCSS, AUCSSSTD,
    AGE, AGEGR1, SEX, RACE, ETHNIC,
    WEIGHT, HEIGHT, BMI, BSA,
    CREAT, CRCL, EGFR,
    SAFFL, ITTFL
  )

# Prepare BOR records
adrs_bor_final <- adrs_bor %>%
  select(
    STUDYID, USUBJID, SUBJID, SITEID,
    PARAMCD, PARAM, PARAMN,
    AVISITN, AVISIT,
    ADT, ADY,
    AVALC, AVALN,
    ARM, ARMN, TRT01P, TRT01A,
    TRTSDT, TRTEDT, TRTDURD,
    DOSE, AUCSS, AUCSSSTD,
    AGE, AGEGR1, SEX, RACE, ETHNIC,
    WEIGHT, HEIGHT, BMI, BSA,
    CREAT, CRCL, EGFR,
    SAFFL, ITTFL
  )

# Combine
adrs_combined <- bind_rows(
  adrs_visit_final,
  adrs_bor_final
)

cat("  ✓ Records combined\n")
cat("    Total records:", nrow(adrs_combined), "\n\n")

#===============================================================================
# STEP 6: ADD ADDITIONAL VARIABLES
#===============================================================================

cat("Step 6: Adding additional variables...\n")

adrs_simulated <- adrs_combined %>%
  # Add numeric identifiers
  mutate(
    STUDYIDN = as.numeric(gsub("STUDY", "", STUDYID)),
    SITEIDN = as.numeric(SITEID),
    SUBJIDN = as.numeric(SUBJID),
    USUBJIDN = match(USUBJID, unique(USUBJID))
  ) %>%
  
  # Add analysis flags
  mutate(
    # All records for primary analysis
    ANL01FL = "Y",
    
    # BOR records only
    ANL02FL = if_else(PARAMCD == "BOR", "Y", ""),
    
    # Confirmed response (CR or PR)
    ANL03FL = if_else(AVALC %in% c("CR", "PR"), "Y", ""),
    
    # Parameter category
    PARCAT1 = "TUMOR RESPONSE",
    PARCAT2 = if_else(PARAMCD == "BOR", "OVERALL", "BY VISIT")
  ) %>%
  
  # Add sequence number
  group_by(STUDYID, USUBJID) %>%
  arrange(PARAMN, AVISITN) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup() %>%
  
  # Add character date
  mutate(ADTC = as.character(ADT)) %>%
  
  # Reorder columns
  select(
    # Identifiers
    STUDYID, STUDYIDN, USUBJID, USUBJIDN, SUBJID, SUBJIDN,
    SITEID, SITEIDN,
    ASEQ,
    
    # Treatment
    ARM, ARMN,
    TRT01P, TRT01A,
    TRTSDT, TRTEDT, TRTDURD,
    
    # Demographics
    AGE, AGEGR1,
    SEX,
    RACE,
    ETHNIC,
    
    # Parameter information
    PARAMCD, PARAM, PARAMN,
    PARCAT1, PARCAT2,
    
    # Visit
    AVISITN, AVISIT,
    
    # Dates
    ADT, ADY, ADTC,
    
    # Analysis values
    AVALC, AVALN,
    
    # Exposure
    DOSE, AUCSS, AUCSSSTD,
    
    # Baseline covariates
    WEIGHT, HEIGHT, BMI, BSA,
    CREAT, CRCL, EGFR,
    
    # Analysis flags
    ANL01FL, ANL02FL, ANL03FL,
    SAFFL, ITTFL
  )

cat("  ✓ Additional variables added\n")
cat("    Total variables:", ncol(adrs_simulated), "\n\n")

#===============================================================================
# STEP 7: SAVE OUTPUT
#===============================================================================

cat("Step 7: Saving ADRS...\n")

# Create data directory if needed
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

# Save as RDS
saveRDS(adrs_simulated, "data/adrs_simulated.rds")

cat("  ✓ Saved: data/adrs_simulated.rds\n\n")

#===============================================================================
# SUMMARY
#===============================================================================

cat(strrep("-", 80), "\n")
cat("ADRS GENERATION SUMMARY\n")
cat(strrep("-", 80), "\n\n")

# Overall statistics
cat("Overall Statistics:\n")
cat("  Total records:", nrow(adrs_simulated), "\n")
cat("  Unique subjects:", length(unique(adrs_simulated$USUBJID)), "\n")
cat("  Parameters:\n")
param_counts <- adrs_simulated %>%
  count(PARAMCD, PARAM)
print(param_counts)
cat("\n")

# BOR distribution
cat("Best Overall Response Distribution:\n")
bor_dist <- adrs_simulated %>%
  filter(PARAMCD == "BOR") %>%
  count(ARM, AVALC) %>%
  pivot_wider(names_from = AVALC, values_from = n, values_fill = 0)
print(bor_dist)
cat("\n")

# Overall response rate (ORR: CR + PR)
cat("Overall Response Rate by Arm:\n")
orr_summary <- adrs_simulated %>%
  filter(PARAMCD == "BOR") %>%
  group_by(ARM) %>%
  summarise(
    N = n(),
    CR = sum(AVALC == "CR"),
    PR = sum(AVALC == "PR"),
    SD = sum(AVALC == "SD"),
    PD = sum(AVALC == "PD"),
    .groups = "drop"
  ) %>%
  mutate(
    ORR = round(100 * (CR + PR) / N, 1),
    DCR = round(100 * (CR + PR + SD) / N, 1)
  )
print(orr_summary)
cat("\n")

# Exposure effect on response
cat("Response Rate by Exposure Tertile (Active Arms):\n")
exp_response <- adrs_simulated %>%
  filter(PARAMCD == "BOR" & DOSE > 0) %>%
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
    N = n(),
    CR_PR = sum(AVALC %in% c("CR", "PR")),
    .groups = "drop"
  ) %>%
  mutate(
    ORR = round(100 * CR_PR / N, 1)
  )
print(exp_response)

cat("\n")
cat(strrep("-", 80), "\n")
cat("ADRS generation complete!\n")
cat(strrep("-", 80), "\n\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================