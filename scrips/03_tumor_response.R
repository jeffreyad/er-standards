# Tumor Response Dataset Creation
# Demonstrating standardization for longitudinal tumor measurements

library(admiral)
library(admiraldev)
library(dplyr)
library(lubridate)

# ============================================================================
# Setup: Create example SDTM-like data
# ============================================================================

# Subject-level data with exposure metrics
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

# Tumor measurements (SDTM TR domain or derived from TU)
# Simulating sum of target lesions measurements
tr_raw <- tibble::tribble(
  ~USUBJID, ~VISIT, ~VISITNUM, ~TRDT, ~TRORRES, ~TRORRESU,
  # Subject 001-001: Stable disease
  "001-001", "BASELINE", 0, "2024-01-01", 50, "mm",
  "001-001", "WEEK 6", 6, "2024-02-12", 48, "mm",
  "001-001", "WEEK 12", 12, "2024-03-25", 52, "mm",
  "001-001", "WEEK 18", 18, "2024-05-06", 49, "mm",
  # Subject 001-002: Partial response
  "001-002", "BASELINE", 0, "2024-01-05", 65, "mm",
  "001-002", "WEEK 6", 6, "2024-02-16", 55, "mm",
  "001-002", "WEEK 12", 12, "2024-03-29", 35, "mm",
  "001-002", "WEEK 18", 18, "2024-05-10", 32, "mm",
  # Subject 001-003: Progressive disease
  "001-003", "BASELINE", 0, "2024-01-10", 45, "mm",
  "001-003", "WEEK 6", 6, "2024-02-21", 48, "mm",
  "001-003", "WEEK 12", 12, "2024-04-03", 60, "mm",
  "001-003", "WEEK 18", 18, "2024-05-15", 75, "mm",
  # Subject 001-004: Complete response
  "001-004", "BASELINE", 0, "2024-01-15", 38, "mm",
  "001-004", "WEEK 6", 6, "2024-02-26", 20, "mm",
  "001-004", "WEEK 12", 12, "2024-04-08", 8, "mm",
  "001-004", "WEEK 18", 18, "2024-05-20", 0, "mm",
  # Subject 001-005: Initial response then progression
  "001-005", "BASELINE", 0, "2024-01-20", 72, "mm",
  "001-005", "WEEK 6", 6, "2024-03-02", 50, "mm",
  "001-005", "WEEK 12", 12, "2024-04-13", 45, "mm",
  "001-005", "WEEK 18", 18, "2024-05-25", 68, "mm"
) %>%
  mutate(TRDT = as.Date(TRDT))

# ============================================================================
# Step 1: Create analysis value and baseline
# ============================================================================

adtr <- tr_raw %>%
  # Merge subject-level data
  left_join(adsl, by = "USUBJID") %>%
  # Rename to ADaM conventions
  mutate(
    PARAM = "Sum of Target Lesion Diameters",
    PARAMCD = "STDIAM",
    AVAL = TRORRES,
    AVALU = TRORRESU,
    ADT = TRDT,
    ATPT = VISIT,
    ATPTN = VISITNUM
  ) %>%
  # Calculate analysis day
  mutate(ADY = as.numeric(ADT - TRTSDT) + 1)

# Identify baseline
adtr <- adtr %>%
  group_by(USUBJID, PARAMCD) %>%
  mutate(
    ABLFL = if_else(ATPTN == 0, "Y", NA_character_),
    BASE = AVAL[ABLFL == "Y"]
  ) %>%
  ungroup()

# ============================================================================
# Step 2: Calculate change from baseline and percent change
# ============================================================================

adtr <- adtr %>%
  mutate(
    CHG = if_else(!is.na(BASE) & ABLFL != "Y", AVAL - BASE, NA_real_),
    PCHG = if_else(!is.na(BASE) & ABLFL != "Y" & BASE != 0, 
                   (AVAL - BASE) / BASE * 100, 
                   NA_real_)
  )

# ============================================================================
# Step 3: Add RECIST 1.1 response criteria
# ============================================================================

adtr <- adtr %>%
  mutate(
    # Individual visit response (based on change from baseline)
    AVALC = case_when(
      ABLFL == "Y" ~ "BASELINE",
      is.na(PCHG) ~ NA_character_,
      AVAL == 0 ~ "CR",  # Complete response
      PCHG <= -30 ~ "PR",  # Partial response
      PCHG >= 20 ~ "PD",  # Progressive disease
      TRUE ~ "SD"  # Stable disease
    ),
    
    # Numeric code for response
    AVALN = case_when(
      AVALC == "CR" ~ 4,
      AVALC == "PR" ~ 3,
      AVALC == "SD" ~ 2,
      AVALC == "PD" ~ 1,
      AVALC == "BASELINE" ~ 0,
      TRUE ~ NA_real_
    )
  )

# ============================================================================
# Step 4: Derive best overall response (BOR)
# ============================================================================

# Identify best response per subject (excluding baseline)
bor <- adtr %>%
  filter(ABLFL != "Y" | is.na(ABLFL)) %>%
  group_by(USUBJID) %>%
  summarise(
    BOR = case_when(
      any(AVALC == "CR", na.rm = TRUE) ~ "CR",
      any(AVALC == "PR", na.rm = TRUE) ~ "PR",
      any(AVALC == "PD", na.rm = TRUE) ~ "PD",
      any(AVALC == "SD", na.rm = TRUE) ~ "SD",
      TRUE ~ NA_character_
    ),
    BORN = case_when(
      BOR == "CR" ~ 4,
      BOR == "PR" ~ 3,
      BOR == "SD" ~ 2,
      BOR == "PD" ~ 1,
      TRUE ~ NA_real_
    ),
    # Minimum tumor size achieved
    NADIR = min(AVAL, na.rm = TRUE),
    # Maximum tumor size
    MAX_SIZE = max(AVAL, na.rm = TRUE),
    # Best percent change
    BEST_PCHG = min(PCHG, na.rm = TRUE),
    # Time to best response
    TTR_DAYS = ADY[which.min(AVAL)],
    .groups = "drop"
  )

# Merge BOR back to main dataset
adtr <- adtr %>%
  left_join(
    bor %>% select(USUBJID, BOR, BORN, NADIR, BEST_PCHG),
    by = "USUBJID"
  )

# ============================================================================
# Step 5: Add exposure categories
# ============================================================================

exposure_cats <- adsl %>%
  mutate(
    AUC_TERTILE = cut(
      AUC0_24,
      breaks = quantile(AUC0_24, probs = c(0, 1/3, 2/3, 1)),
      labels = c("Low", "Medium", "High"),
      include.lowest = TRUE
    ),
    LOGAUC = log(AUC0_24),
    AUC_STD = (AUC0_24 - mean(AUC0_24)) / sd(AUC0_24)
  ) %>%
  select(USUBJID, AUC_TERTILE, LOGAUC, AUC_STD)

adtr <- adtr %>%
  left_join(exposure_cats, by = "USUBJID")

# ============================================================================
# Step 6: Create derived parameters for analysis
# ============================================================================

# Create parameter records for nadir and best percent change
param_nadir <- adtr %>%
  filter(ABLFL != "Y" | is.na(ABLFL)) %>%
  group_by(USUBJID) %>%
  filter(AVAL == min(AVAL, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    PARAMCD = "NADIR",
    PARAM = "Nadir (Minimum Tumor Size)",
    DTYPE = "DERIVED"
  )

param_best_pchg <- bor %>%
  left_join(adsl, by = "USUBJID") %>%
  left_join(exposure_cats, by = "USUBJID") %>%
  mutate(
    PARAMCD = "BPCHG",
    PARAM = "Best Percent Change from Baseline",
    AVAL = BEST_PCHG,
    AVALU = "%",
    DTYPE = "DERIVED"
  ) %>%
  select(USUBJID, STUDYID, PARAMCD, PARAM, AVAL, AVALU, 
         BOR, BORN, AUC0_24, CMAX, AUC_TERTILE, LOGAUC, DTYPE)

# ============================================================================
# Step 7: Add analysis flags
# ============================================================================

adtr <- adtr %>%
  mutate(
    # Primary analysis population
    ANL01FL = if_else(!is.na(AVAL), "Y", NA_character_),
    # Post-baseline analysis
    ANL02FL = if_else(ANL01FL == "Y" & ABLFL != "Y", "Y", NA_character_),
    # Objective response (CR or PR)
    RESPFL = if_else(AVALC %in% c("CR", "PR"), "Y", "N"),
    # Disease control (CR, PR, or SD)
    DCBFL = if_else(AVALC %in% c("CR", "PR", "SD"), "Y", "N")
  )

# ============================================================================
# Output summaries
# ============================================================================

cat("\n=== Tumor Response Dataset Summary ===\n")
cat("Total records:", nrow(adtr), "\n")
cat("Subjects:", n_distinct(adtr$USUBJID), "\n")
cat("Visits per subject:", nrow(adtr) / n_distinct(adtr$USUBJID), "\n\n")

cat("Best Overall Response Distribution:\n")
print(table(bor$BOR))

cat("\n\nMean Change from Baseline by Visit:\n")
print(
  adtr %>%
    filter(ABLFL != "Y") %>%
    group_by(VISIT) %>%
    summarise(
      N = n(),
      Mean_CHG = mean(CHG, na.rm = TRUE),
      Mean_PCHG = mean(PCHG, na.rm = TRUE)
    )
)

cat("\n\nBest Response by Exposure Tertile:\n")
print(
  adtr %>%
    filter(ABLFL == "Y") %>%
    select(USUBJID, AUC_TERTILE, BOR) %>%
    distinct() %>%
    count(AUC_TERTILE, BOR) %>%
    tidyr::pivot_wider(names_from = BOR, values_from = n, values_fill = 0)
)

# Display sample records
cat("\n=== Sample Longitudinal Records (Subject 001-002) ===\n")
print(
  adtr %>%
    filter(USUBJID == "001-002") %>%
    select(USUBJID, VISIT, ADY, BASE, AVAL, CHG, PCHG, AVALC, BOR)
)

# ============================================================================
# Notes for standardization
# ============================================================================

# Key principles demonstrated:
# 1. Longitudinal structure with repeated measures
# 2. AVAL = raw measurement, BASE = baseline value
# 3. CHG and PCHG = absolute and percent change from baseline
# 4. AVALC = categorical response (RECIST criteria)
# 5. BOR = best overall response across all timepoints
# 6. Derived parameters (NADIR, BPCHG) as separate records
# 7. Analysis flags for different populations and endpoints
# 8. Exposure metrics available at subject level
# 9. Time variables: ADT (date), ADY (study day), TTR (time to response)
#
# Proposed dataset structure:
# - ADTR: Longitudinal tumor measurements
#   - PARAMCD = "STDIAM": Raw measurements
#   - PARAMCD = "NADIR": Minimum tumor size
#   - PARAMCD = "BPCHG": Best percent change
# - One record per subject-visit-parameter
# - BOR and exposure metrics available on all records
#
# RECIST 1.1 criteria:
# - CR (Complete Response): Disappearance of all target lesions
# - PR (Partial Response): ≥30% decrease from baseline
# - PD (Progressive Disease): ≥20% increase from nadir
# - SD (Stable Disease): Neither PR nor PD criteria met
#
# Extensions for more complex analyses:
# - Multiple tumor assessment methods (RECIST, irRECIST, iRECIST)
# - Non-target lesions
# - New lesion appearance
# - Confirmation of response (repeat assessment)
# - Time-dependent covariates for modeling
