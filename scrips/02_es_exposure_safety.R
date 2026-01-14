# Exposure-Safety (ES) Dataset Creation
# Demonstrating standardization for adverse event frequency by exposure

library(admiral)
library(admiraldev)
library(dplyr)
library(tidyr)
library(lubridate)

# ============================================================================
# Setup: Create example SDTM-like data
# ============================================================================

# Subject-level data with exposure metrics
adsl <- tibble::tribble(
  ~USUBJID, ~STUDYID, ~ARM, ~TRTSDT, ~TRTEDT, ~AUC0_24, ~CMAX, ~CAVG, ~TRTDURD,
  "001-001", "STUDY-01", "Dose A", "2024-01-01", "2024-06-30", 450, 125, 18.5, 181,
  "001-002", "STUDY-01", "Dose A", "2024-01-05", "2024-08-15", 520, 145, 21.2, 223,
  "001-003", "STUDY-01", "Dose B", "2024-01-10", "2024-12-01", 680, 178, 28.4, 327,
  "001-004", "STUDY-01", "Dose B", "2024-01-15", "2024-05-20", 710, 192, 29.6, 126,
  "001-005", "STUDY-01", "Dose C", "2024-01-20", "2024-11-30", 890, 235, 37.1, 315
) %>%
  mutate(
    TRTSDT = as.Date(TRTSDT),
    TRTEDT = as.Date(TRTEDT)
  )

# Adverse events data (SDTM AE domain)
ae <- tibble::tribble(
  ~USUBJID, ~AETERM, ~AEDECOD, ~AEBODSYS, ~AESTDT, ~AEENDT, ~AESEV, ~AESER, ~AEREL,
  "001-001", "Nausea", "Nausea", "GASTROINTESTINAL DISORDERS", "2024-01-15", "2024-01-17", "MILD", "N", "POSSIBLE",
  "001-001", "Fatigue", "Fatigue", "GENERAL DISORDERS", "2024-02-01", "2024-02-10", "MODERATE", "N", "PROBABLE",
  "001-002", "Diarrhea", "Diarrhea", "GASTROINTESTINAL DISORDERS", "2024-01-20", "2024-01-25", "MODERATE", "N", "POSSIBLE",
  "001-002", "Neutropenia", "Neutropenia", "BLOOD AND LYMPHATIC SYSTEM DISORDERS", "2024-03-01", "2024-03-15", "SEVERE", "Y", "PROBABLE",
  "001-003", "Fatigue", "Fatigue", "GENERAL DISORDERS", "2024-02-15", "2024-03-01", "MILD", "N", "POSSIBLE",
  "001-003", "Nausea", "Nausea", "GASTROINTESTINAL DISORDERS", "2024-01-25", "2024-01-28", "MODERATE", "N", "PROBABLE",
  "001-004", "Neutropenia", "Neutropenia", "BLOOD AND LYMPHATIC SYSTEM DISORDERS", "2024-02-01", "2024-02-20", "SEVERE", "Y", "PROBABLE",
  "001-004", "Febrile Neutropenia", "Febrile neutropenia", "BLOOD AND LYMPHATIC SYSTEM DISORDERS", "2024-02-10", "2024-02-15", "SEVERE", "Y", "PROBABLE",
  "001-005", "Diarrhea", "Diarrhea", "GASTROINTESTINAL DISORDERS", "2024-02-05", "2024-02-12", "SEVERE", "N", "PROBABLE"
) %>%
  mutate(
    AESTDT = as.Date(AESTDT),
    AEENDT = as.Date(AEENDT),
    # Add CTCAE grade based on severity
    AETOXGR = case_when(
      AESEV == "MILD" ~ "1",
      AESEV == "MODERATE" ~ "2",
      AESEV == "SEVERE" ~ "3",
      TRUE ~ NA_character_
    )
  )

# ============================================================================
# Step 1: Calculate subject-level AE summary metrics
# ============================================================================

# Count AEs per subject by different criteria
ae_summary <- ae %>%
  group_by(USUBJID) %>%
  summarise(
    N_AES = n(),                                          # Total AEs
    N_SAE = sum(AESER == "Y"),                            # Serious AEs
    N_GRADE3 = sum(AETOXGR >= "3", na.rm = TRUE),        # Grade 3+ AEs
    N_DRUG_RELATED = sum(AEREL %in% c("PROBABLE", "POSSIBLE")), # Drug-related AEs
    N_GI_AES = sum(AEBODSYS == "GASTROINTESTINAL DISORDERS"), # GI AEs
    .groups = "drop"
  )

# ============================================================================
# Step 2: Create exposure categories
# ============================================================================

exposure_cats <- adsl %>%
  mutate(
    # Tertile categorization
    AUC_TERTILE = cut(
      AUC0_24,
      breaks = quantile(AUC0_24, probs = c(0, 1/3, 2/3, 1)),
      labels = c("Low", "Medium", "High"),
      include.lowest = TRUE
    ),
    CMAX_TERTILE = cut(
      CMAX,
      breaks = quantile(CMAX, probs = c(0, 1/3, 2/3, 1)),
      labels = c("Low", "Medium", "High"),
      include.lowest = TRUE
    )
  )

# ============================================================================
# Step 3: Create ADES with subject-level metrics
# ============================================================================

# Subject-level Exposure-Safety dataset
ades_subj <- exposure_cats %>%
  left_join(ae_summary, by = "USUBJID") %>%
  mutate(
    # Handle subjects with no AEs
    across(starts_with("N_"), ~replace_na(.x, 0)),
    
    # Calculate rates (events per 100 patient-days)
    RATE_AES = (N_AES / TRTDURD) * 100,
    RATE_SAE = (N_SAE / TRTDURD) * 100,
    RATE_GRADE3 = (N_GRADE3 / TRTDURD) * 100,
    
    # Binary indicators for any event
    ANY_SAE = if_else(N_SAE > 0, "Y", "N"),
    ANY_GRADE3 = if_else(N_GRADE3 > 0, "Y", "N"),
    
    # Continuous exposure metrics
    LOGAUC = log(AUC0_24),
    AUC_STD = (AUC0_24 - mean(AUC0_24)) / sd(AUC0_24)
  )

# ============================================================================
# Step 4: Create event-level ADES
# ============================================================================

# Event-level dataset for specific AE analysis
ades_event <- ae %>%
  # Merge exposure data
  left_join(
    exposure_cats %>% select(USUBJID, TRTSDT, AUC0_24, CMAX, AUC_TERTILE, CMAX_TERTILE),
    by = "USUBJID"
  ) %>%
  mutate(
    # Time from treatment start to AE onset
    ASTDY = as.numeric(AESTDT - TRTSDT) + 1,  # Study day
    AENDY = as.numeric(AEENDT - TRTSDT) + 1,
    
    # Duration of AE
    AEDUR = as.numeric(AEENDT - AESTDT) + 1,
    
    # Flags for analysis
    SAFFL = "Y",  # Safety population
    
    # Grade 3+ flag
    GRADE3FL = if_else(AETOXGR >= "3", "Y", "N"),
    
    # Serious AE flag
    SERFL = AESER,
    
    # Drug-related flag
    RELFL = if_else(AEREL %in% c("PROBABLE", "POSSIBLE"), "Y", "N")
  )

# ============================================================================
# Step 5: Create parameters for specific AE types
# ============================================================================

# Count by preferred term
ae_by_pt <- ades_event %>%
  group_by(USUBJID, AEDECOD) %>%
  summarise(
    AVAL = n(),  # Number of occurrences
    MAX_GRADE = max(as.numeric(AETOXGR), na.rm = TRUE),
    ANY_SERIOUS = if_else(any(AESER == "Y"), 1, 0),
    .groups = "drop"
  ) %>%
  mutate(
    PARAMCD = paste0("AE_", gsub(" ", "_", toupper(AEDECOD))),
    PARAM = paste("Number of", AEDECOD, "events"),
    AVALU = "COUNT"
  )

# Merge with exposure data
ae_by_pt_full <- ae_by_pt %>%
  left_join(
    exposure_cats %>% select(USUBJID, AUC0_24, CMAX, AUC_TERTILE, LOGAUC = AUC0_24),
    by = "USUBJID"
  ) %>%
  mutate(LOGAUC = log(LOGAUC))

# ============================================================================
# Step 6: Summary by system organ class
# ============================================================================

ae_by_soc <- ades_event %>%
  group_by(USUBJID, AEBODSYS) %>%
  summarise(
    AVAL = n(),
    MAX_GRADE = max(as.numeric(AETOXGR), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    PARAMCD = paste0("SOC_", gsub(" ", "_", substr(AEBODSYS, 1, 10))),
    PARAM = paste(AEBODSYS, "- Total Events"),
    AVALU = "COUNT"
  )

# ============================================================================
# Output summaries
# ============================================================================

cat("\n=== Subject-Level ADES Summary ===\n")
cat("Total subjects:", nrow(ades_subj), "\n")
cat("Subjects with any AE:", sum(ades_subj$N_AES > 0), "\n")
cat("Subjects with SAE:", sum(ades_subj$ANY_SAE == "Y"), "\n")
cat("Subjects with Grade 3+ AE:", sum(ades_subj$ANY_GRADE3 == "Y"), "\n\n")

cat("AE Rate by Exposure Tertile:\n")
print(
  ades_subj %>%
    group_by(AUC_TERTILE) %>%
    summarise(
      N = n(),
      Mean_AE_Rate = mean(RATE_AES),
      Mean_SAE_Rate = mean(RATE_SAE)
    )
)

cat("\n=== Event-Level ADES Summary ===\n")
cat("Total AE records:", nrow(ades_event), "\n")
cat("Grade 3+ events:", sum(ades_event$GRADE3FL == "Y"), "\n")
cat("Serious events:", sum(ades_event$SERFL == "Y"), "\n\n")

cat("Top 3 Most Common AEs:\n")
print(
  ades_event %>%
    count(AEDECOD, sort = TRUE) %>%
    head(3)
)

# Display sample records
cat("\n=== Sample Subject-Level Records ===\n")
print(
  ades_subj %>%
    select(USUBJID, AUC_TERTILE, N_AES, N_SAE, RATE_AES, ANY_GRADE3) %>%
    head()
)

# ============================================================================
# Notes for standardization
# ============================================================================

# Key principles demonstrated:
# 1. Multiple analysis levels:
#    - Subject-level: overall AE burden and rates
#    - Event-level: individual AE occurrences
#    - Parameter-level: specific AE types
# 
# 2. Exposure metrics available as:
#    - Continuous (AUC0_24, CMAX)
#    - Categorical (tertiles)
#    - Transformed (log, standardized)
#
# 3. Safety outcomes as:
#    - Counts (N_AES, N_SAE)
#    - Rates (per patient-day)
#    - Binary indicators (ANY_SAE)
#    - Grade/severity
#
# 4. Time variables:
#    - ASTDY/AENDY: study days for onset/end
#    - AEDUR: duration of event
#    - Traceability to treatment dates
#
# 5. Analysis flags:
#    - GRADE3FL: Grade 3 or higher
#    - SERFL: Serious AE
#    - RELFL: Drug-related

# Proposed dataset structure:
# - ADES_SUBJ: One record per subject with overall metrics
# - ADES_EVENT: One record per AE occurrence
# - ADES_PARAM: One record per subject-parameter (specific AE types)
