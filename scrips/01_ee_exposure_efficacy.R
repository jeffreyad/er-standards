# Exposure-Efficacy (EE) Dataset Creation
# Demonstrating standardization for time-to-event endpoints

library(admiral)
library(admiraldev)
library(dplyr)
library(lubridate)

# ============================================================================
# Setup: Create example SDTM-like data
# ============================================================================

# Example ADSL with exposure metrics
adsl <- tibble::tribble(
  ~USUBJID, ~STUDYID, ~ARM, ~TRTSDT, ~TRTEDT, ~AUC0_24, ~CMAX, ~CAVG,
  "001-001", "STUDY-01", "Dose A", "2024-01-01", "2024-06-30", 450, 125, 18.5,
  "001-002", "STUDY-01", "Dose A", "2024-01-05", "2024-08-15", 520, 145, 21.2,
  "001-003", "STUDY-01", "Dose B", "2024-01-10", "2024-12-01", 680, 178, 28.4,
  "001-004", "STUDY-01", "Dose B", "2024-01-15", "2024-05-20", 710, 192, 29.6,
  "001-005", "STUDY-01", "Dose C", "2024-01-20", "2024-11-30", 890, 235, 37.1
) %>%
  mutate(
    TRTSDT = as.Date(TRTSDT),
    TRTEDT = as.Date(TRTEDT)
  )

# Example time-to-event data (e.g., progression-free survival)
adtte_source <- tibble::tribble(
  ~USUBJID, ~PARAMCD, ~PARAM, ~ADT, ~CNSR, ~EVNTDESC,
  "001-001", "PFS", "Progression-Free Survival", "2024-04-15", 1, "CENSORED",
  "001-002", "PFS", "Progression-Free Survival", "2024-05-20", 0, "PROGRESSIVE DISEASE",
  "001-003", "PFS", "Progression-Free Survival", "2024-08-10", 0, "PROGRESSIVE DISEASE", 
  "001-004", "PFS", "Progression-Free Survival", "2024-03-01", 0, "DEATH",
  "001-005", "PFS", "Progression-Free Survival", "2024-09-15", 0, "PROGRESSIVE DISEASE"
) %>%
  mutate(ADT = as.Date(ADT))

# ============================================================================
# Step 1: Derive relative time variables (AVAL = days from treatment start)
# ============================================================================

adtte <- adtte_source %>%
  derive_vars_merged(
    dataset_add = adsl,
    by_vars = exprs(USUBJID),
    new_vars = exprs(STUDYID, TRTSDT, AUC0_24, CMAX, CAVG, ARM)
  ) %>%
  mutate(
    AVAL = as.numeric(ADT - TRTSDT),
    AVALU = "DAYS"
  )

# ============================================================================
# Step 2: Categorize subjects by exposure metrics
# ============================================================================

# Create exposure quintiles for AUC
exposure_categories <- adsl %>%
  mutate(
    AUC_QUINTILE = cut(
      AUC0_24,
      breaks = quantile(AUC0_24, probs = seq(0, 1, 0.2)),
      labels = c("Q1", "Q2", "Q3", "Q4", "Q5"),
      include.lowest = TRUE
    ),
    # Tertile for simpler categorization
    AUC_TERTILE = cut(
      AUC0_24,
      breaks = quantile(AUC0_24, probs = c(0, 1/3, 2/3, 1)),
      labels = c("Low", "Medium", "High"),
      include.lowest = TRUE
    ),
    # Binary above/below median
    AUC_MEDIAN = if_else(AUC0_24 >= median(AUC0_24), "Above Median", "Below Median")
  ) %>%
  select(USUBJID, AUC_QUINTILE, AUC_TERTILE, AUC_MEDIAN)

# Merge exposure categories
adtte <- adtte %>%
  derive_vars_merged(
    dataset_add = exposure_categories,
    by_vars = exprs(USUBJID)
  )

# ============================================================================
# Step 3: Create analysis flags
# ============================================================================

adtte <- adtte %>%
  mutate(
    # Primary analysis population
    ANL01FL = if_else(!is.na(AVAL) & !is.na(AUC0_24), "Y", NA_character_),
    # Sensitivity analysis (complete cases)
    ANL02FL = if_else(ANL01FL == "Y" & CNSR == 0, "Y", NA_character_)
  )

# ============================================================================
# Step 4: Add analysis-ready variables for modeling
# ============================================================================

adtte <- adtte %>%
  mutate(
    # Event indicator (1 = event, 0 = censored) - standard for survival modeling
    EVENT = if_else(CNSR == 0, 1, 0),
    
    # Log-transformed exposure for continuous analysis
    LOGAUC = log(AUC0_24),
    LOGCMAX = log(CMAX),
    
    # Standardized exposure metrics (mean 0, SD 1)
    AUC_STD = (AUC0_24 - mean(AUC0_24, na.rm = TRUE)) / sd(AUC0_24, na.rm = TRUE),
    CMAX_STD = (CMAX - mean(CMAX, na.rm = TRUE)) / sd(CMAX, na.rm = TRUE)
  )

# ============================================================================
# Step 5: Create ADEE (Analysis Dataset for Exposure-Efficacy)
# ============================================================================

adee <- adtte %>%
  select(
    # Identifiers
    STUDYID, USUBJID, ARM,
    # Parameters
    PARAMCD, PARAM,
    # Analysis value and timing
    AVAL, AVALU, ADT, TRTSDT,
    # Event information
    CNSR, EVENT, EVNTDESC,
    # Exposure metrics (continuous)
    AUC0_24, CMAX, CAVG, LOGAUC, LOGCMAX, AUC_STD, CMAX_STD,
    # Exposure categories
    AUC_QUINTILE, AUC_TERTILE, AUC_MEDIAN,
    # Analysis flags
    ANL01FL, ANL02FL
  ) %>%
  arrange(USUBJID, ADT)

# ============================================================================
# Output summary
# ============================================================================

cat("\n=== ADEE Dataset Summary ===\n")
cat("Total records:", nrow(adee), "\n")
cat("Subjects:", n_distinct(adee$USUBJID), "\n")
cat("Events:", sum(adee$EVENT), "\n")
cat("Censored:", sum(adee$CNSR), "\n\n")

cat("Exposure distribution (AUC0-24):\n")
print(summary(adee$AUC0_24))

cat("\nExposure categories:\n")
print(table(adee$AUC_TERTILE))

# Display first few records
cat("\n=== Sample Records ===\n")
print(head(adee))

# ============================================================================
# Optional: Create additional TTE parameters
# ============================================================================

# This demonstrates how to add multiple TTE endpoints
# (e.g., Overall Survival, Disease-Free Survival)

# Example: Overall Survival
# adtte_os <- derive_param_tte(
#   dataset_adsl = adsl,
#   source_datasets = list(
#     ae = ae,      # adverse events for death
#     ds = ds       # disposition for death
#   ),
#   event_conditions = list(
#     death_ae = ae_death_cond,
#     death_ds = ds_death_cond
#   ),
#   censor_conditions = list(...),
#   set_values_to = exprs(
#     PARAMCD = "OS",
#     PARAM = "Overall Survival"
#   )
# )

# ============================================================================
# Notes for standardization
# ============================================================================

# Key principles demonstrated:
# 1. AVAL = time from treatment start (days)
# 2. CNSR = censoring indicator (1 = censored, 0 = event)
# 3. EVENT = inverse of CNSR for modeling convenience
# 4. Exposure metrics available in both continuous and categorical forms
# 5. Log-transformed and standardized exposure for modeling
# 6. Multiple analysis flags for sensitivity analyses
# 7. Traceability to treatment dates

# Proposed variable naming conventions (aligned with CDISC):
# - Exposure metrics: Descriptive names (AUC0_24, CMAX, CAVG)
# - Categories: [METRIC]_[CATEGORY] (e.g., AUC_TERTILE)
# - Transformations: LOG[METRIC], [METRIC]_STD
# - Analysis flags: ANL##FL pattern
