#===============================================================================
# Program: create_adee_metacore_from_p21.R
# 
# Purpose: Create complete metacore specification for ADEE from P21 specs
#
# Input: specifications/ADEE_P21_Specifications.csv
# Output: specifications/ADEE_metacore.rds
#         specifications/ADEE_spec.xlsx
#
#===============================================================================

library(metacore)
library(dplyr)
library(readr)
library(writexl)
library(tidyr)

# Create specifications directory
if (!dir.exists("specifications")) dir.create("specifications", recursive = TRUE)

#===============================================================================
# LOAD P21 SPECIFICATIONS
#===============================================================================

# Read the P21 spec CSV created earlier
p21_spec <- read_csv("specifications/ADEE_P21_Specifications.csv")

#===============================================================================
# DATASET SPECIFICATION
#===============================================================================

ds_spec <- tibble::tribble(
  ~dataset, ~structure, ~label, ~keys,
  "ADEE", 
  "One record per subject per parameter per analysis timepoint", 
  "Exposure-Efficacy Analysis Dataset", 
  "USUBJID, PARAMCD, AVISITN"
)

#===============================================================================
# VARIABLE SPECIFICATION
#===============================================================================

# Convert P21 to metacore variable spec format
var_spec <- p21_spec %>%
  mutate(
    # Map data types
    type = case_when(
      Data_Type == "text" ~ "text",
      Data_Type == "integer" ~ "integer",
      Data_Type == "float" ~ "float",
      TRUE ~ "text"
    ),
    
    # Determine if common variable (appears across multiple datasets)
    common = case_when(
      Variable %in% c("STUDYID", "USUBJID", "SUBJID", "SITEID", 
                      "AGE", "SEX", "RACE", "ETHNIC",
                      "ARM", "ARMN", "ACTARM", "ACTARMN",
                      "TRT01P", "TRT01PN", "TRT01A", "TRT01AN") ~ "Y",
      TRUE ~ "N"
    ),
    
    # Assign formats (SAS formats)
    format = case_when(
      Variable %in% c("TRTSDT", "TRTEDT", "ADT", "STARTDT") ~ "DATE9.",
      Variable == "AVALU" ~ "$20.",
      Variable == "AVALC" ~ "$40.",
      type == "float" ~ "8.2",
      TRUE ~ NA_character_
    ),
    
    # Order (preserve original order)
    order = row_number()
  ) %>%
  select(
    variable = Variable,
    type,
    length = Length,
    label = Label,
    common,
    format,
    order
  )

#===============================================================================
# CONTROLLED TERMINOLOGY SPECIFICATION
#===============================================================================

# Create controlled terminology specification
# Origin values: "CDISC", "Sponsor", "NCI", or other sources

ct_spec <- tibble::tribble(
  ~variable, ~code, ~decode, ~origin,
  
  # ---- CDISC Standard Terms ----
  
  # PARAMCD (CDISC standard codes where applicable)
  "PARAMCD", "PFS", "Progression-Free Survival", "CDISC",
  "PARAMCD", "OS", "Overall Survival", "CDISC",
  "PARAMCD", "TTP", "Time to Progression", "CDISC",
  "PARAMCD", "TTNT", "Time to Next Treatment", "Sponsor",  # Sponsor-defined
  
  # AVALU (CDISC standard units)
  "AVALU", "DAYS", "Days", "CDISC",
  
  # SEX (CDISC CT)
  "SEX", "M", "Male", "CDISC",
  "SEX", "F", "Female", "CDISC",
  "SEX", "U", "Unknown", "CDISC",
  
  # NY codelist (standard CDISC for flags)
  "ANL01FL", "Y", "Yes", "CDISC",
  "ANL01FL", "", "No", "CDISC",
  "ANL02FL", "Y", "Yes", "CDISC",
  "ANL02FL", "", "No", "CDISC",
  "ANL03FL", "Y", "Yes", "CDISC",
  "ANL03FL", "", "No", "CDISC",
  "ANL04FL", "Y", "Yes", "CDISC",
  "ANL04FL", "", "No", "CDISC",
  
  # ATPT / AVISIT (CDISC standard timepoints)
  "ATPT", "BASELINE", "Baseline", "CDISC",
  "AVISIT", "BASELINE", "Baseline", "CDISC",
  
  # DTYPE (CDISC derivation types)
  "DTYPE", "AVERAGE", "Average", "CDISC",
  "DTYPE", "MINIMUM", "Minimum", "CDISC",
  "DTYPE", "MAXIMUM", "Maximum", "CDISC",
  
  # ADTF (CDISC date imputation flags)
  "ADTF", "D", "Day", "CDISC",
  "ADTF", "M", "Month", "CDISC",
  "ADTF", "Y", "Year", "CDISC",
  
  # ---- Sponsor-Specific Terms ----
  
  # Parameter categories (sponsor-defined)
  "PARCAT1", "EFFICACY", "Efficacy", "Sponsor",
  "PARCAT2", "TIME TO EVENT", "Time to Event", "Sponsor",
  
  # Event status (sponsor-defined)
  "AVALC", "EVENT", "Event", "Sponsor",
  "AVALC", "CENSORED", "Censored", "Sponsor",
  
  # Exposure categories (sponsor-defined tertiles)
  "AUCSSCAT", "Low", "Low Exposure (Tertile 1)", "Sponsor",
  "AUCSSCAT", "Medium", "Medium Exposure (Tertile 2)", "Sponsor",
  "AUCSSCAT", "High", "High Exposure (Tertile 3)", "Sponsor",
  
  # Exposure quartiles (sponsor-defined)
  "AUCSSQ", "Q1", "Quartile 1", "Sponsor",
  "AUCSSQ", "Q2", "Quartile 2", "Sponsor",
  "AUCSSQ", "Q3", "Quartile 3", "Sponsor",
  "AUCSSQ", "Q4", "Quartile 4", "Sponsor",
  
  # Exposure median split (sponsor-defined)
  "AUCSSMED", "Below Median", "Below Median Exposure", "Sponsor",
  "AUCSSMED", "Above Median", "Above Median Exposure", "Sponsor",
  
  # Age groups (sponsor-defined)
  "AGEGR1", "<65", "Less than 65 years", "Sponsor",
  "AGEGR1", "65-75", "65 to 75 years", "Sponsor",
  "AGEGR1", ">75", "Greater than 75 years", "Sponsor",
  
  # Weight groups (sponsor-defined)
  "WTBLGR1", "<70 kg", "Less than 70 kg", "Sponsor",
  "WTBLGR1", ">=70 kg", "Greater than or equal to 70 kg", "Sponsor",
  
  # Treatment arms (study-specific)
  "TRT01P", "Placebo", "Placebo", "Sponsor",
  "TRT01P", "Xanomeline Low Dose", "Xanomeline Low Dose", "Sponsor",
  "TRT01P", "Xanomeline High Dose", "Xanomeline High Dose", "Sponsor",
  "TRT01A", "Placebo", "Placebo", "Sponsor",
  "TRT01A", "Xanomeline Low Dose", "Xanomeline Low Dose", "Sponsor",
  "TRT01A", "Xanomeline High Dose", "Xanomeline High Dose", "Sponsor",
  "ARM", "Placebo", "Placebo", "Sponsor",
  "ARM", "Xanomeline Low Dose", "Xanomeline Low Dose", "Sponsor",
  "ARM", "Xanomeline High Dose", "Xanomeline High Dose", "Sponsor",
  "ACTARM", "Placebo", "Placebo", "Sponsor",
  "ACTARM", "Xanomeline Low Dose", "Xanomeline Low Dose", "Sponsor",
  "ACTARM", "Xanomeline High Dose", "Xanomeline High Dose", "Sponsor"
)

# Note: Add study-specific RACE, ETHNIC terms if needed based on actual data
#===============================================================================
# VALUE LEVEL METADATA
#===============================================================================

# Variables that are conditional or have special rules
value_spec <- tibble::tribble(
  ~variable, ~where, ~type, ~length, ~label, ~format,
  
  # Optional variables (may not be present in all records)
  "ADTF", "!is.na(ADTF)", "text", 1, "Analysis Date Imputation Flag", "$1.",
  "DTYPE", "!is.na(DTYPE)", "text", 8, "Derivation Type", "$8.",
  "SRCDOM", "!is.na(SRCDOM)", "text", 8, "Source Data", "$8.",
  "SRCVAR", "!is.na(SRCVAR)", "text", 40, "Source Variable", "$40.",
  "SRCSEQ", "!is.na(SRCSEQ)", "integer", 8, "Source Sequence Number", "8.",
  "ALBBL", "!is.na(ALBBL)", "float", 8, "Baseline Albumin (g/dL)", "8.2",
  
  # Exposure variables (only for active treatment)
  "AUCSS", "DOSE > 0", "float", 8, "Steady-State AUC (ug*h/mL)", "8.2",
  "CMAXSS", "DOSE > 0", "float", 8, "Steady-State Cmax (ug/mL)", "8.3",
  "AUCSSCAT", "DOSE > 0", "text", 20, "AUC Category (Tertiles)", "$20.",
  "AUCSSQ", "DOSE > 0", "text", 20, "AUC Quartile", "$20."
)

#===============================================================================
# DERIVATIONS
#===============================================================================

derivation_spec <- tibble::tribble(
  ~variable, ~derivation_type, ~derivation,
  
  # Core derivations
  "EVENT", "Formula", "1 - CNSR",
  "AVALU", "Assigned", "DAYS (for time-to-event)",
  "AVALC", "Conditional", "EVENT if EVENT=1, CENSORED if CNSR=1",
  
  # Numeric identifiers
  "STUDYIDN", "Parsed", "Numeric from USUBJID word 1",
  "SITEIDN", "Parsed", "Numeric from USUBJID word 2",
  "USUBJIDN", "Parsed", "Numeric from USUBJID word 3",
  "SUBJIDN", "Numeric", "Numeric version of SUBJID",
  
  # Demographics numeric
  "SEXN", "Coded", "1=M, 2=F, 3=U",
  "RACEN", "Coded", "1-6 coded values",
  "ETHNICN", "Coded", "1-3 coded values",
  "AGEGR1", "Conditional", "<65, 65-75, >75 based on AGE",
  "AGEGR1N", "Coded", "1=<65, 2=65-75, 3=>75",
  
  # Treatment numeric
  "ARMN", "Coded", "0=Placebo, 1=Low, 2=High, 3=Other",
  "ACTARMN", "Coded", "0=Placebo, 1=Low, 2=High, 3=Other",
  "TRT01PN", "Coded", "Numeric version of TRT01P",
  "TRT01AN", "Coded", "Numeric version of TRT01A",
  
  # Vitals
  "BMIBL", "Computed", "WTBL / (HTBL/100)^2",
  "BSABL", "Computed", "Mosteller formula",
  "WTBLGR1", "Conditional", "<70 kg vs >=70 kg",
  
  # Labs
  "TBILBL", "Renamed", "BILIBL",
  "CRCLBL", "Computed", "Cockcroft-Gault equation",
  "EGFRBL", "Computed", "CKD-EPI equation",
  
  # Exposure transformations
  "AUCSSLOG", "Computed", "log(AUCSS + 0.01)",
  "CMAXSSLOG", "Computed", "log(CMAXSS + 0.01)",
  "CAVGSSLOG", "Computed", "log(CAVGSS + 0.01)",
  "AUCSSSTD", "Computed", "(AUCSS - mean) / SD (active subjects only)",
  "CMAXSSSTD", "Computed", "(CMAXSS - mean) / SD (active subjects only)",
  "AUCSSN", "Computed", "AUCSS / median(AUCSS) (active subjects only)",
  "AUCSSDOSE", "Computed", "AUCSS / DOSE",
  "CMAXSSDOSE", "Computed", "CMAXSS / DOSE",
  
  # Exposure categories
  "AUCSSCAT", "Categorical", "Tertiles of AUCSS (active subjects)",
  "AUCSSCATN", "Coded", "1=Low, 2=Medium, 3=High",
  "AUCSSQ", "Categorical", "Quartiles of AUCSS (active subjects)",
  "AUCSSQN", "Coded", "1-4 for Q1-Q4",
  "AUCSSMED", "Conditional", "Below/Above median AUCSS",
  
  # Analysis variables
  "ATPT", "Assigned", "BASELINE",
  "ATPTN", "Assigned", "0",
  "AVISIT", "Assigned", "BASELINE",
  "AVISITN", "Assigned", "0",
  
  # Analysis flags
  "ANL01FL", "Conditional", "Y if PARAMCD=PFS",
  "ANL02FL", "Conditional", "Y if PARAMCD=OS",
  "ANL03FL", "Conditional", "Y if PARAMCD=TTP",
  "ANL04FL", "Conditional", "Y if PARAMCD=TTNT",
  
  # Categories
  "PARCAT1", "Assigned", "EFFICACY",
  "PARCAT2", "Assigned", "TIME TO EVENT",
  
  # Sequence
  "ASEQ", "Derived", "Sequence number within USUBJID"
)

#===============================================================================
# CREATE METACORE OBJECT
#===============================================================================

# Build metacore object
metacore <- metacore(
  ds_spec = ds_spec,
  var_spec = var_spec,
  codelist = ct_spec,
  value_spec = value_spec,
  derivations = derivation_spec,
  supp = NULL
)

# Save as RDS
saveRDS(metacore, "specifications/ADEE_metacore.rds")

message("✓ Saved: specifications/ADEE_metacore.rds")

#===============================================================================
# SAVE AS EXCEL FOR EDITING
#===============================================================================

spec_list <- list(
  Dataset = ds_spec,
  Variables = var_spec,
  Controlled_Terms = ct_spec,
  Value_Level = value_spec,
  Derivations = derivation_spec
)

writexl::write_xlsx(spec_list, "specifications/ADEE_spec.xlsx")

message("✓ Saved: specifications/ADEE_spec.xlsx")

#===============================================================================
# CREATE SUMMARY REPORT
#===============================================================================

cat("\n")
cat(strrep("=", 80), "\n")
cat("ADEE METACORE SPECIFICATION SUMMARY\n")
cat(strrep("=", 80), "\n\n")

cat("Dataset:", ds_spec$dataset, "\n")
cat("Label:", ds_spec$label, "\n")
cat("Structure:", ds_spec$structure, "\n")
cat("Keys:", ds_spec$keys, "\n\n")

cat("Variables:", nrow(var_spec), "\n")
cat("  Common variables:", sum(var_spec$common == "Y"), "\n")
cat("  Dataset-specific:", sum(var_spec$common == "N"), "\n\n")

cat("Controlled Terms:", nrow(ct_spec), "\n")
cat("  Variables with CT:", length(unique(ct_spec$variable)), "\n\n")

cat("Value-Level Metadata:", nrow(value_spec), "\n")
cat("Derivations:", nrow(derivation_spec), "\n\n")

cat("Variable Breakdown:\n")
cat("  Identifiers:", sum(grepl("^(STUDYID|USUBJID|SUBJID|SITEID)", var_spec$variable)), "\n")
cat("  Treatment:", sum(grepl("^(ARM|TRT01|ACT)", var_spec$variable)), "\n")
cat("  Demographics:", sum(grepl("^(AGE|SEX|RACE|ETHNIC)", var_spec$variable)), "\n")
cat("  Parameters:", sum(grepl("^(PARAM|PARCAT)", var_spec$variable)), "\n")
cat("  Analysis values:", sum(grepl("^(AVAL|ADT|ADY)", var_spec$variable)), "\n")
cat("  Exposure metrics:", sum(grepl("^(AUCSS|CMAXSS|CAVGSS|CMINSS|CLSS)", var_spec$variable)), "\n")
cat("  Baseline covariates:", sum(grepl("BL$", var_spec$variable)), "\n")
cat("  Analysis flags:", sum(grepl("^ANL", var_spec$variable)), "\n\n")

cat("Next steps:\n")
cat("  1. Review specifications/ADEE_spec.xlsx\n")
cat("  2. Edit/refine as needed\n")
cat("  3. Reload: metacore <- readRDS('specifications/ADEE_metacore.rds')\n")
cat("  4. Use in ADEE.R program\n\n")

#===============================================================================
# END OF PROGRAM
#===============================================================================