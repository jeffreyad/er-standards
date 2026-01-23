# ===============================================================================
# Simulation Helper Functions
#
# Purpose: Utility functions for generating synthetic clinical trial data
#
# Description: Functions to create realistic patient demographics, treatment
#              assignments, event times, adverse events, and tumor measurements
#              for the ER Standards Framework examples.
#
# Author: Jeff Dickinson
# Date: 2026-01-22
#
# ===============================================================================

# ===============================================================================
# DEMOGRAPHICS
# ===============================================================================

#' Generate Subject IDs
#'
#' @param n_subjects Number of subjects
#' @param n_sites Number of sites
#' @param study_id Study identifier (default: "STUDY001")
#'
#' @return Data frame with STUDYID, SITEID, SUBJID, USUBJID
#'
#' @examples
#' ids <- generate_subject_ids(300, 10)
generate_subject_ids <- function(n_subjects, n_sites, study_id = "STUDY001") {
  # Distribute subjects across sites
  subjects_per_site <- ceiling(n_subjects / n_sites)

  # Create site IDs
  site_ids <- sprintf("%03d", 1:n_sites)

  # Generate subject IDs
  ids <- expand.grid(
    SITEID = site_ids,
    SUBJID_NUM = 1:subjects_per_site
  ) %>%
    mutate(
      STUDYID = study_id,
      SUBJID = sprintf("%04d", SUBJID_NUM),
      USUBJID = paste(STUDYID, SITEID, SUBJID, sep = "-")
    ) %>%
    select(STUDYID, SITEID, SUBJID, USUBJID) %>%
    slice(1:n_subjects)

  return(ids)
}

#' Generate Demographics
#'
#' @param usubjid Vector of unique subject IDs
#' @param age_mean Mean age (default: 60)
#' @param age_sd Standard deviation of age (default: 10)
#'
#' @return Data frame with AGE, SEX, RACE, ETHNIC
generate_demographics <- function(usubjid, age_mean = 60, age_sd = 10) {
  n <- length(usubjid)

  tibble(
    USUBJID = usubjid,

    # Age (truncated normal)
    AGE = round(pmax(18, pmin(85, rnorm(n, age_mean, age_sd)))),
    AGEU = "YEARS",

    # Sex (55% male)
    SEX = sample(c("M", "F"), n, replace = TRUE, prob = c(0.55, 0.45)),

    # Race (US distribution)
    RACE = sample(
      c(
        "WHITE", "BLACK OR AFRICAN AMERICAN", "ASIAN",
        "AMERICAN INDIAN OR ALASKA NATIVE", "OTHER"
      ),
      n,
      replace = TRUE,
      prob = c(0.70, 0.15, 0.10, 0.02, 0.03)
    ),

    # Ethnicity
    ETHNIC = sample(
      c("HISPANIC OR LATINO", "NOT HISPANIC OR LATINO"),
      n,
      replace = TRUE,
      prob = c(0.15, 0.85)
    )
  )
}

#' Generate Vital Signs
#'
#' @param usubjid Vector of unique subject IDs
#'
#' @return Data frame with HEIGHT, WEIGHT, BMI
generate_vitals <- function(usubjid) {
  n <- length(usubjid)

  tibble(
    USUBJID = usubjid,

    # Height in cm (mean 170, sd 10)
    HEIGHT = round(rnorm(n, 170, 10), 1),

    # Weight in kg (mean 75, sd 15)
    WEIGHT = round(pmax(45, rnorm(n, 75, 15)), 1),

    # BMI calculated
    BMI = round(WEIGHT / (HEIGHT / 100)^2, 1)
  )
}

#' Generate Laboratory Values
#'
#' @param usubjid Vector of unique subject IDs
#'
#' @return Data frame with baseline lab values
generate_labs <- function(usubjid) {
  n <- length(usubjid)

  tibble(
    USUBJID = usubjid,

    # Creatinine (mg/dL) - mean 1.0, sd 0.2
    CREAT = round(pmax(0.5, rnorm(n, 1.0, 0.2)), 2),

    # ALT (U/L) - mean 25, sd 10
    ALT = round(pmax(5, rnorm(n, 25, 10))),

    # AST (U/L) - mean 28, sd 12
    AST = round(pmax(5, rnorm(n, 28, 12))),

    # Bilirubin (mg/dL) - mean 0.8, sd 0.3
    TBILI = round(pmax(0.2, rnorm(n, 0.8, 0.3)), 2),

    # Albumin (g/dL) - mean 4.2, sd 0.4
    ALB = round(pmax(2.5, rnorm(n, 4.2, 0.4)), 1)
  )
}

# ===============================================================================
# TREATMENT ASSIGNMENT
# ===============================================================================

#' Randomize to Treatment Arms
#'
#' @param usubjid Vector of unique subject IDs
#' @param arms Vector of treatment arm names
#' @param ratio Vector of randomization ratios (default: equal)
#'
#' @return Data frame with ARM, ARMCD
randomize_treatment <- function(usubjid, arms, ratio = NULL) {
  n <- length(usubjid)

  if (is.null(ratio)) {
    ratio <- rep(1, length(arms))
  }

  # Normalize ratio
  prob <- ratio / sum(ratio)

  tibble(
    USUBJID = usubjid,
    ARM = sample(arms, n, replace = TRUE, prob = prob),
    ARMCD = factor(ARM,
      levels = arms,
      labels = paste0("ARM", 1:length(arms))
    )
  )
}

#' Generate Treatment Dates
#'
#' @param usubjid Vector of unique subject IDs
#' @param study_start Study start date
#' @param enrollment_period_days Days to enroll all subjects
#'
#' @return Data frame with TRTSDT, TRTEDT, TRTDURD
generate_treatment_dates <- function(usubjid, study_start,
                                     enrollment_period_days = 365) {
  n <- length(usubjid)

  # Random enrollment dates
  enroll_days <- sample(0:enrollment_period_days, n, replace = TRUE)
  trtsdt <- study_start + enroll_days

  # Treatment duration (30 to 365 days, heavier at longer durations)
  duration <- round(rbeta(n, 2, 1.5) * 335 + 30)

  tibble(
    USUBJID = usubjid,
    TRTSDT = trtsdt,
    TRTEDT = trtsdt + duration,
    TRTDURD = duration
  )
}

# ===============================================================================
# TIME-TO-EVENT
# ===============================================================================

#' Simulate Survival Times with Exposure Effect
#'
#' @param n Number of subjects
#' @param baseline_median Baseline median survival (days)
#' @param hr_per_unit Hazard ratio per unit increase in exposure
#' @param exposure Exposure metric (e.g., AUC)
#' @param admin_censor_time Administrative censoring time
#'
#' @return Data frame with AVAL (time) and CNSR (censor indicator)
simulate_survival <- function(n, baseline_median = 365, hr_per_unit = 0.95,
                              exposure = NULL, admin_censor_time = 730) {
  # Baseline hazard rate
  lambda_base <- log(2) / baseline_median

  if (!is.null(exposure)) {
    # Adjust hazard by exposure (log-linear model)
    hr <- hr_per_unit^exposure
    lambda <- lambda_base * hr
  } else {
    lambda <- rep(lambda_base, n)
  }

  # Generate event times (exponential)
  event_time <- rexp(n, rate = lambda)

  # Administrative censoring
  admin_censor <- runif(n, admin_censor_time * 0.8, admin_censor_time)

  # Observed time and censor indicator
  tibble(
    AVAL = pmin(event_time, admin_censor),
    CNSR = as.numeric(event_time > admin_censor)
  )
}

#' Simulate PFS with Treatment Effect
#'
#' @param arm Treatment arm
#' @param exposure Exposure metric
#'
#' @return Data frame with PFS time and event
simulate_pfs <- function(arm, exposure = NULL) {
  n <- length(arm)

  # Baseline median by arm
  baseline_median <- case_when(
    arm == "Placebo" ~ 180,
    arm == "Drug Low Dose" ~ 240,
    arm == "Drug High Dose" ~ 300,
    TRUE ~ 180
  )

  # HR per unit exposure (benefit)
  hr_per_unit <- 0.98

  # Simulate
  events <- map2_dfr(
    baseline_median, if (!is.null(exposure)) exposure else rep(NA, n),
    ~ simulate_survival(1, .x, hr_per_unit, .y, admin_censor_time = 730)
  )

  return(events)
}

#' Simulate OS with Treatment Effect
#'
#' @param arm Treatment arm
#' @param exposure Exposure metric
#'
#' @return Data frame with OS time and event
simulate_os <- function(arm, exposure = NULL) {
  n <- length(arm)

  # Baseline median by arm (longer than PFS)
  baseline_median <- case_when(
    arm == "Placebo" ~ 365,
    arm == "Drug Low Dose" ~ 450,
    arm == "Drug High Dose" ~ 540,
    TRUE ~ 365
  )

  # HR per unit exposure (benefit)
  hr_per_unit <- 0.97

  # Simulate
  events <- map2_dfr(
    baseline_median, if (!is.null(exposure)) exposure else rep(NA, n),
    ~ simulate_survival(1, .x, hr_per_unit, .y, admin_censor_time = 1095)
  )

  return(events)
}

# ===============================================================================
# ADVERSE EVENTS
# ===============================================================================

#' Simulate Number of AEs per Subject
#'
#' @param n Number of subjects
#' @param arm Treatment arm
#' @param exposure Exposure metric
#'
#' @return Vector of AE counts
simulate_ae_count <- function(n, arm, exposure = NULL) {
  # Base rate by arm
  lambda_base <- case_when(
    arm == "Placebo" ~ 1.5,
    arm == "Drug Low Dose" ~ 2.5,
    arm == "Drug High Dose" ~ 3.5,
    TRUE ~ 2.0
  )

  # Exposure effect (higher exposure = more AEs)
  if (!is.null(exposure)) {
    lambda <- lambda_base * (1 + 0.01 * exposure)
  } else {
    lambda <- lambda_base
  }

  # Poisson count
  rpois(n, lambda)
}

#' Generate AE Terms
#'
#' @param n Number of AEs to generate
#'
#' @return Data frame with AEDECOD, AEBODSYS
generate_ae_terms <- function(n) {
  # Common AE terms with SOC
  ae_library <- tribble(
    ~AEDECOD, ~AEBODSYS,
    "NAUSEA", "GASTROINTESTINAL DISORDERS",
    "VOMITING", "GASTROINTESTINAL DISORDERS",
    "DIARRHEA", "GASTROINTESTINAL DISORDERS",
    "CONSTIPATION", "GASTROINTESTINAL DISORDERS",
    "FATIGUE", "GENERAL DISORDERS",
    "HEADACHE", "NERVOUS SYSTEM DISORDERS",
    "DIZZINESS", "NERVOUS SYSTEM DISORDERS",
    "RASH", "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
    "PRURITUS", "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
    "COUGH", "RESPIRATORY DISORDERS",
    "DYSPNEA", "RESPIRATORY DISORDERS",
    "FEVER", "GENERAL DISORDERS",
    "PAIN", "GENERAL DISORDERS",
    "ANEMIA", "BLOOD AND LYMPHATIC DISORDERS",
    "NEUTROPENIA", "BLOOD AND LYMPHATIC DISORDERS"
  )

  # Sample with realistic frequencies
  weights <- c(
    0.15, 0.10, 0.12, 0.08, 0.20, 0.10, 0.08, 0.05, 0.04,
    0.06, 0.05, 0.04, 0.03, 0.02, 0.01
  )

  ae_library %>%
    slice_sample(n = n, replace = TRUE, weight_by = weights)
}

#' Assign AE Severity
#'
#' @param n Number of AEs
#'
#' @return Character vector of severity (MILD, MODERATE, SEVERE)
assign_ae_severity <- function(n) {
  sample(
    c("MILD", "MODERATE", "SEVERE"),
    n,
    replace = TRUE,
    prob = c(0.60, 0.30, 0.10)
  )
}

#' Assign AE Relationship
#'
#' @param n Number of AEs
#'
#' @return Character vector of relationship
assign_ae_relationship <- function(n) {
  sample(
    c("NOT RELATED", "UNLIKELY RELATED", "POSSIBLE", "PROBABLE", "RELATED"),
    n,
    replace = TRUE,
    prob = c(0.20, 0.25, 0.30, 0.15, 0.10)
  )
}

#' Assign Serious AE Flag
#'
#' @param severity Severity vector
#'
#' @return Character vector "Y" or "N"
assign_serious <- function(severity) {
  # Severe AEs more likely to be serious
  ifelse(
    severity == "SEVERE",
    sample(c("Y", "N"), length(severity), replace = TRUE, prob = c(0.40, 0.60)),
    sample(c("Y", "N"), length(severity), replace = TRUE, prob = c(0.05, 0.95))
  )
}

# ===============================================================================
# TUMOR MEASUREMENTS
# ===============================================================================

#' Simulate Baseline Tumor Size
#'
#' @param n Number of subjects
#' @param mean_size Mean baseline sum of diameters (mm)
#' @param sd_size Standard deviation
#'
#' @return Vector of baseline tumor sizes
simulate_baseline_tumor <- function(n, mean_size = 100, sd_size = 30) {
  round(pmax(20, rnorm(n, mean_size, sd_size)), 1)
}

#' Simulate Tumor Growth/Shrinkage
#'
#' @param baseline Baseline tumor size
#' @param arm Treatment arm
#' @param exposure Exposure metric
#' @param visit_number Visit number (1, 2, 3, ...)
#'
#' @return Vector of tumor sizes at visit
simulate_tumor_change <- function(baseline, arm, exposure = NULL, visit_number) {
  n <- length(baseline)

  # Growth rate per visit (% change)
  growth_rate <- case_when(
    arm == "Placebo" ~ 0.05, # 5% growth per visit
    arm == "Drug Low Dose" ~ -0.10, # 10% shrinkage per visit
    arm == "Drug High Dose" ~ -0.15, # 15% shrinkage per visit
    TRUE ~ 0
  )

  # Exposure effect (higher exposure = more shrinkage for active arms)
  if (!is.null(exposure) && any(arm != "Placebo")) {
    exposure_effect <- ifelse(arm == "Placebo", 0, -0.002 * exposure)
    growth_rate <- growth_rate + exposure_effect
  }

  # Apply exponential growth/shrinkage with noise
  tumor_size <- baseline * (1 + growth_rate)^visit_number * exp(rnorm(n, 0, 0.1))

  # Floor at 0
  round(pmax(0, tumor_size), 1)
}

#' Derive RECIST Response
#'
#' @param baseline Baseline tumor size
#' @param current Current tumor size
#' @param nadir Nadir (minimum) tumor size
#'
#' @return RECIST response category
derive_recist <- function(baseline, current, nadir) {
  # Percent change from baseline
  pchg_baseline <- 100 * (current - baseline) / baseline

  # Percent change from nadir
  pchg_nadir <- 100 * (current - nadir) / nadir

  # RECIST 1.1 criteria
  case_when(
    current == 0 ~ "CR", # Complete response
    pchg_baseline <= -30 ~ "PR", # Partial response
    pchg_nadir >= 20 & current >= nadir + 5 ~ "PD", # Progressive disease
    TRUE ~ "SD" # Stable disease
  )
}

# ===============================================================================
# UTILITY FUNCTIONS
# ===============================================================================

#' Compute eGFR (CKD-EPI)
#'
#' @param creat Serum creatinine (mg/dL)
#' @param age Age (years)
#' @param sex Sex ("M" or "F")
#'
#' @return eGFR (mL/min/1.73m²)
compute_egfr_ckdepi <- function(creat, age, sex) {
  kappa <- ifelse(sex == "F", 0.7, 0.9)
  alpha <- ifelse(sex == "F", -0.329, -0.411)
  sex_factor <- ifelse(sex == "F", 1.018, 1)

  egfr <- 141 * pmin(creat / kappa, 1)^alpha *
    pmax(creat / kappa, 1)^(-1.209) *
    0.993^age *
    sex_factor

  round(egfr, 1)
}

#' Compute CrCL (Cockcroft-Gault)
#'
#' @param creat Serum creatinine (mg/dL)
#' @param age Age (years)
#' @param weight Weight (kg)
#' @param sex Sex ("M" or "F")
#'
#' @return CrCL (mL/min)
compute_crcl <- function(creat, age, weight, sex) {
  crcl <- ((140 - age) * weight) / (72 * creat)
  crcl <- ifelse(sex == "F", crcl * 0.85, crcl)

  round(crcl, 1)
}

#' Compute BSA (Mosteller)
#'
#' @param height Height (cm)
#' @param weight Weight (kg)
#'
#' @return BSA (m²)
compute_bsa <- function(height, weight) {
  round(sqrt(height * weight / 3600), 2)
}

# ===============================================================================
# END OF FILE
# ===============================================================================
