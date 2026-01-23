# ===============================================================================
# Script: S1_Generate_ADSL.R
#
# Purpose: Generate simulated ADSL (Subject-Level Analysis Dataset)
#
# Description: Creates synthetic subject-level data with realistic demographics,
#              baseline characteristics, treatment assignments, and exposure
#              metrics for the ER Standards Framework examples.
#
# Author: Jeff Dickinson
# Date: 2026-01-22
#
# Input: None (generates from scratch)
#
# Output: data/adsl_simulated.rds
#
# Note: This script is called by S0_Generate_Example_Data.R but can also
#       be run standalone.
#
# ===============================================================================

library(dplyr)
library(tidyr)
library(lubridate)

# Source helper functions
source("R/simulation_functions.R")

# Set seed for reproducibility
set.seed(12345)

# ===============================================================================
# CONFIGURATION
# ===============================================================================

# Study parameters
STUDY_ID <- "STUDY001"
N_SUBJECTS <- 300
N_SITES <- 10
STUDY_START <- as.Date("2024-01-01")
STUDY_END <- as.Date("2025-12-31")

# Treatment arms
ARMS <- c("Placebo", "Drug Low Dose", "Drug High Dose")
ARM_RATIO <- c(1, 1, 1) # 1:1:1 randomization

# Doses (mg)
DOSE_MAP <- c(
  "Placebo" = 0,
  "Drug Low Dose" = 54,
  "Drug High Dose" = 81
)

cat("\n")
cat(strrep("-", 80), "\n")
cat("Generating ADSL (Subject-Level Analysis Dataset)\n")
cat(strrep("-", 80), "\n")
cat("Configuration:\n")
cat("  Study ID:", STUDY_ID, "\n")
cat("  Subjects:", N_SUBJECTS, "\n")
cat("  Sites:", N_SITES, "\n")
cat("  Treatment arms:", paste(ARMS, collapse = ", "), "\n")
cat(strrep("-", 80), "\n\n")

# ===============================================================================
# STEP 1: GENERATE SUBJECT IDENTIFIERS
# ===============================================================================

cat("Step 1: Generating subject identifiers...\n")

adsl_ids <- generate_subject_ids(
  n_subjects = N_SUBJECTS,
  n_sites = N_SITES,
  study_id = STUDY_ID
)

cat("  ✓ Created", nrow(adsl_ids), "subject IDs\n\n")

# ===============================================================================
# STEP 2: GENERATE DEMOGRAPHICS
# ===============================================================================

cat("Step 2: Generating demographics...\n")

adsl_demo <- generate_demographics(
  usubjid = adsl_ids$USUBJID,
  age_mean = 62,
  age_sd = 9
)

# Add derived demographic variables
adsl_demo <- adsl_demo %>%
  mutate(
    # Numeric versions
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

    # Country (all US for this example)
    COUNTRY = "USA"
  )

cat("  ✓ Demographics generated\n")
cat("    Age range:", min(adsl_demo$AGE), "-", max(adsl_demo$AGE), "\n")
cat(
  "    Sex distribution:",
  paste0(round(100 * mean(adsl_demo$SEX == "M")), "% Male"), "\n\n"
)

# ===============================================================================
# STEP 3: GENERATE VITALS
# ===============================================================================

cat("Step 3: Generating baseline vitals...\n")

adsl_vitals <- generate_vitals(usubjid = adsl_ids$USUBJID)

# Add BSA
adsl_vitals <- adsl_vitals %>%
  mutate(
    BSA = compute_bsa(HEIGHT, WEIGHT),

    # Weight group
    WTGR1 = case_when(
      WEIGHT < 70 ~ "<70 kg",
      WEIGHT >= 70 ~ ">=70 kg",
      TRUE ~ NA_character_
    )
  )

cat("  ✓ Vitals generated\n")
cat(
  "    Weight range:", round(min(adsl_vitals$WEIGHT), 1), "-",
  round(max(adsl_vitals$WEIGHT), 1), "kg\n"
)
cat("    Mean BMI:", round(mean(adsl_vitals$BMI), 1), "\n\n")

# ===============================================================================
# STEP 4: GENERATE LABORATORY VALUES
# ===============================================================================

cat("Step 4: Generating baseline laboratory values...\n")

adsl_labs <- generate_labs(usubjid = adsl_ids$USUBJID)

# Join with demographics and vitals for eGFR/CrCL calculation
adsl_labs <- adsl_labs %>%
  left_join(
    adsl_demo %>% select(USUBJID, AGE, SEX),
    by = "USUBJID"
  ) %>%
  left_join(
    adsl_vitals %>% select(USUBJID, WEIGHT),
    by = "USUBJID"
  ) %>%
  mutate(
    # Calculate eGFR (CKD-EPI)
    EGFR = compute_egfr_ckdepi(CREAT, AGE, SEX),

    # Calculate CrCL (Cockcroft-Gault)
    CRCL = compute_crcl(CREAT, AGE, WEIGHT, SEX)
  ) %>%
  select(-AGE, -SEX, -WEIGHT) # Remove temporary variables

cat("  ✓ Laboratory values generated\n")
cat("    Mean eGFR:", round(mean(adsl_labs$EGFR), 1), "mL/min/1.73m²\n")
cat("    Mean CrCL:", round(mean(adsl_labs$CRCL), 1), "mL/min\n\n")

# ===============================================================================
# STEP 5: TREATMENT ASSIGNMENT
# ===============================================================================

cat("Step 5: Assigning treatment...\n")

adsl_treatment <- randomize_treatment(
  usubjid = adsl_ids$USUBJID,
  arms = ARMS,
  ratio = ARM_RATIO
)

# Add planned and actual treatment (assume same for this example)
adsl_treatment <- adsl_treatment %>%
  mutate(
    # Planned treatment
    TRT01P = ARM,
    TRT01PN = as.numeric(factor(ARM, levels = ARMS)),

    # Actual treatment (same as planned for this example)
    TRT01A = ARM,
    TRT01AN = TRT01PN,

    # Actual arm (same as randomized)
    ACTARM = ARM,
    ACTARMCD = ARMCD,
    ACTARMN = as.numeric(factor(ACTARM, levels = ARMS)),

    # Numeric ARM
    ARMN = as.numeric(factor(ARM, levels = ARMS))
  )

# Distribution by arm
arm_dist <- adsl_treatment %>%
  count(ARM) %>%
  mutate(Percent = round(100 * n / sum(n), 1))

cat("  ✓ Treatment assigned\n")
cat("    Distribution:\n")
for (i in 1:nrow(arm_dist)) {
  cat(
    "      ", arm_dist$ARM[i], ":", arm_dist$n[i],
    "(", arm_dist$Percent[i], "%)\n"
  )
}
cat("\n")

# ===============================================================================
# STEP 6: TREATMENT DATES
# ===============================================================================

cat("Step 6: Generating treatment dates...\n")

adsl_dates <- generate_treatment_dates(
  usubjid = adsl_ids$USUBJID,
  study_start = STUDY_START,
  enrollment_period_days = 365
)

# Add reference dates
adsl_dates <- adsl_dates %>%
  mutate(
    RFSTDTC = as.character(TRTSDT),
    RFENDTC = as.character(TRTEDT),
    RFXSTDTC = as.character(TRTSDT), # First exposure
    RFXENDTC = as.character(TRTEDT), # Last exposure

    # Screening date (7-30 days before treatment)
    SCRFDT = TRTSDT - sample(7:30, n(), replace = TRUE),

    # Randomization date (same as treatment start for this example)
    RANDDT = TRTSDT
  )

cat("  ✓ Treatment dates generated\n")
cat(
  "    Enrollment period:", min(adsl_dates$TRTSDT), "to",
  max(adsl_dates$TRTSDT), "\n"
)
cat(
  "    Mean treatment duration:",
  round(mean(adsl_dates$TRTDURD)), "days\n\n"
)

# ===============================================================================
# STEP 7: SAFETY POPULATION FLAGS
# ===============================================================================

cat("Step 7: Assigning population flags...\n")

adsl_flags <- adsl_ids %>%
  mutate(
    # All subjects enrolled
    SAFFL = "Y",

    # ITT population (all randomized - same as safety for this example)
    ITTFL = "Y",

    # Modified ITT (received at least one dose)
    MITTFL = "Y",

    # Per protocol (assume 95% compliance)
    PPROTFL = sample(c("Y", "N"), n(), replace = TRUE, prob = c(0.95, 0.05)),

    # Efficacy evaluable (same as ITT for this example)
    EFFFL = "Y"
  )

cat("  ✓ Population flags assigned\n\n")

# ===============================================================================
# STEP 8: DISPOSITION (SIMPLIFIED)
# ===============================================================================

cat("Step 8: Generating disposition...\n")

# Join with treatment to get duration
adsl_disposition <- adsl_ids %>%
  left_join(adsl_dates %>% select(USUBJID, TRTDURD), by = "USUBJID") %>%
  mutate(
    # End of study status
    EOSSTT = sample(
      c("COMPLETED", "ADVERSE EVENT", "PROGRESSIVE DISEASE", "WITHDRAWAL BY SUBJECT"),
      n(),
      replace = TRUE,
      prob = c(0.70, 0.10, 0.15, 0.05)
    ),

    # Death flag (small proportion)
    DTHFL = sample(c("Y", "N"), n(), replace = TRUE, prob = c(0.05, 0.95))
  ) %>%
  select(-TRTDURD)

# Add death date for those who died (after treatment end)
adsl_disposition <- adsl_disposition %>%
  left_join(adsl_dates %>% select(USUBJID, TRTEDT), by = "USUBJID") %>%
  mutate(
    DTHDT = if_else(
      DTHFL == "Y",
      TRTEDT + sample(1:365, n(), replace = TRUE),
      as.Date(NA)
    ),
    DTHDTC = as.character(DTHDT),

    # Death cause (for those who died)
    DTHCAUS = if_else(
      DTHFL == "Y",
      sample(c("PROGRESSIVE DISEASE", "ADVERSE EVENT", "OTHER"),
        n(),
        replace = TRUE, prob = c(0.70, 0.20, 0.10)
      ),
      NA_character_
    )
  ) %>%
  select(-TRTEDT)

cat("  ✓ Disposition generated\n")
cat(
  "    Completion rate:",
  paste0(round(100 * mean(adsl_disposition$EOSSTT == "COMPLETED")), "%"), "\n"
)
cat(
  "    Death rate:",
  paste0(round(100 * mean(adsl_disposition$DTHFL == "Y")), "%"), "\n\n"
)

# ===============================================================================
# STEP 9: COMBINE ALL COMPONENTS
# ===============================================================================

cat("Step 9: Combining all components...\n")

adsl_simulated <- adsl_ids %>%
  # Add numeric identifiers
  mutate(
    STUDYIDN = as.numeric(gsub("STUDY", "", STUDYID)),
    SITEIDN = as.numeric(SITEID),
    SUBJIDN = as.numeric(SUBJID),
    USUBJIDN = row_number()
  ) %>%
  # Join all components
  left_join(adsl_demo, by = "USUBJID") %>%
  left_join(adsl_vitals, by = "USUBJID") %>%
  left_join(adsl_labs, by = "USUBJID") %>%
  left_join(adsl_treatment, by = "USUBJID") %>%
  left_join(adsl_dates, by = "USUBJID") %>%
  left_join(adsl_flags, by = "USUBJID") %>%
  left_join(adsl_disposition, by = "USUBJID") %>%
  # Reorder columns logically
  select(
    # Identifiers
    STUDYID, STUDYIDN, USUBJID, USUBJIDN, SUBJID, SUBJIDN,
    SITEID, SITEIDN,

    # Demographics
    AGE, AGEU, AGEGR1, AGEGR1N,
    SEX, SEXN,
    RACE, RACEN,
    ETHNIC, ETHNICN,
    COUNTRY,

    # Vitals
    HEIGHT, WEIGHT, BMI, BSA, WTGR1,

    # Labs
    CREAT, ALT, AST, TBILI, ALB,
    EGFR, CRCL,

    # Treatment
    ARM, ARMN, ARMCD,
    ACTARM, ACTARMN, ACTARMCD,
    TRT01P, TRT01PN,
    TRT01A, TRT01AN,

    # Dates
    RFSTDTC, RFENDTC, RFXSTDTC, RFXENDTC,
    TRTSDT, TRTEDT, TRTDURD,
    SCRFDT, RANDDT,

    # Population flags
    SAFFL, ITTFL, MITTFL, PPROTFL, EFFFL,

    # Disposition
    EOSSTT,
    DTHFL, DTHDT, DTHDTC, DTHCAUS
  )

cat("  ✓ ADSL combined\n")
cat("    Total variables:", ncol(adsl_simulated), "\n")
cat("    Total records:", nrow(adsl_simulated), "\n\n")

# ===============================================================================
# STEP 10: SAVE OUTPUT
# ===============================================================================

cat("Step 10: Saving ADSL...\n")

# Create data directory if needed
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

# Save as RDS
saveRDS(adsl_simulated, "data/adsl_simulated.rds")

cat("  ✓ Saved: data/adsl_simulated.rds\n\n")

# ===============================================================================
# SUMMARY
# ===============================================================================

cat(strrep("-", 80), "\n")
cat("ADSL GENERATION SUMMARY\n")
cat(strrep("-", 80), "\n")

summary_stats <- tibble(
  Metric = c(
    "Total Subjects",
    "Sites",
    "Treatment Arms",
    "Age (mean ± SD)",
    "Male %",
    "Weight (mean ± SD) kg",
    "BMI (mean ± SD)",
    "eGFR (mean ± SD)",
    "Completion Rate %",
    "Death Rate %"
  ),
  Value = c(
    nrow(adsl_simulated),
    length(unique(adsl_simulated$SITEID)),
    length(unique(adsl_simulated$ARM)),
    paste0(
      round(mean(adsl_simulated$AGE), 1), " ± ",
      round(sd(adsl_simulated$AGE), 1)
    ),
    round(100 * mean(adsl_simulated$SEX == "M"), 1),
    paste0(
      round(mean(adsl_simulated$WEIGHT), 1), " ± ",
      round(sd(adsl_simulated$WEIGHT), 1)
    ),
    paste0(
      round(mean(adsl_simulated$BMI), 1), " ± ",
      round(sd(adsl_simulated$BMI), 1)
    ),
    paste0(
      round(mean(adsl_simulated$EGFR), 1), " ± ",
      round(sd(adsl_simulated$EGFR), 1)
    ),
    round(100 * mean(adsl_simulated$EOSSTT == "COMPLETED"), 1),
    round(100 * mean(adsl_simulated$DTHFL == "Y"), 1)
  )
)

print(summary_stats, n = Inf)

cat("\n")
cat("Treatment Distribution:\n")
print(
  adsl_simulated %>%
    count(ARM) %>%
    mutate(Percent = round(100 * n / sum(n), 1))
)

cat("\n")
cat(strrep("-", 80), "\n")
cat("ADSL generation complete!\n")
cat(strrep("-", 80), "\n\n")

# ===============================================================================
# END OF SCRIPT
# ===============================================================================
