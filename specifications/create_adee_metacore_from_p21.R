# ===============================================================================
# Program: create_adee_metacore_from_p21.R
#
# Purpose: Create complete metacore specification for ADEE from P21 specs
#
# Input: specifications/ADEE_P21_Specifications.csv
# Output: specifications/ADEE_metacore.rds
#         specifications/ADEE_spec.xlsx
#
# ===============================================================================

library(metacore)
library(dplyr)
library(readr)
library(writexl)
library(tidyr)
library(purrr)

# Create specifications directory
if (!dir.exists("specifications")) dir.create("specifications", recursive = TRUE)

# ===============================================================================
# LOAD P21 SPECIFICATIONS
# ===============================================================================

# Read the P21 spec CSV created earlier
p21_spec <- read_csv("specifications/ADEE_P21_Specifications.csv")

# ===============================================================================
# DATASET SPECIFICATION
# ===============================================================================

# Correct column names: dataset, structure, label (no 'keys' column)
ds_spec <- tibble::tribble(
  ~dataset, ~structure, ~label,
  "ADEE",
  "One record per subject per parameter per analysis timepoint",
  "Exposure-Efficacy Analysis Dataset"
)

# ===============================================================================
# VARIABLE SPECIFICATION
# ===============================================================================

# Correct column names: variable, length, label, type, common, format
# Note: Order matters - keep this exact order
var_spec <- p21_spec %>%
  mutate(
    # Map data types (must match exactly)
    type = case_when(
      Data_Type == "text" ~ "text",
      Data_Type == "integer" ~ "integer",
      Data_Type == "float" ~ "float",
      TRUE ~ "text"
    ),

    # Common flag (Y or N only)
    common = case_when(
      Variable %in% c(
        "STUDYID", "USUBJID", "SUBJID", "SITEID",
        "AGE", "SEX", "RACE", "ETHNIC",
        "ARM", "ARMN", "ACTARM", "ACTARMN",
        "TRT01P", "TRT01PN", "TRT01A", "TRT01AN"
      ) ~ "Y",
      TRUE ~ NA_character_ # Use NA for non-common variables
    ),

    # Assign formats (can be NA)
    format = case_when(
      Variable %in% c("TRTSDT", "TRTEDT", "ADT", "STARTDT") ~ "DATE9.",
      Variable == "AVALU" ~ "$20.",
      Variable == "AVALC" ~ "$40.",
      type == "float" ~ NA_character_, # No default format for float
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    variable = Variable,
    length = Length,
    label = Label,
    type,
    common,
    format
  ) %>%
  # Ensure correct column order
  select(variable, length, label, type, common, format)

# ===============================================================================
# CONTROLLED TERMINOLOGY (CODE LIST) SPECIFICATION
# ===============================================================================

# Correct column names: code_id, name, type, codes
# Need to restructure into long format with code_id

# First create individual codelists
ct_spec <- tibble::tribble(
  ~code_id, ~name, ~type, ~code, ~decode, ~origin,

  # PARAMCD codelist
  "CL.PARAMCD", "Parameter Code", "PARAMCD", "PFS", "Progression-Free Survival", "CDISC",
  "CL.PARAMCD", "Parameter Code", "PARAMCD", "OS", "Overall Survival", "CDISC",
  "CL.PARAMCD", "Parameter Code", "PARAMCD", "TTP", "Time to Progression", "CDISC",
  "CL.PARAMCD", "Parameter Code", "PARAMCD", "TTNT", "Time to Next Treatment", "Sponsor",

  # PARCAT1 codelist
  "CL.PARCAT1", "Parameter Category 1", "PARCAT1", "EFFICACY", "Efficacy", "Sponsor",

  # PARCAT2 codelist
  "CL.PARCAT2", "Parameter Category 2", "PARCAT2", "TIME TO EVENT", "Time to Event", "Sponsor",

  # AVALU codelist
  "CL.AVALU", "Analysis Value Unit", "AVALU", "DAYS", "Days", "CDISC",

  # AVALC codelist
  "CL.AVALC", "Analysis Value Character", "AVALC", "EVENT", "Event", "Sponsor",
  "CL.AVALC", "Analysis Value Character", "AVALC", "CENSORED", "Censored", "Sponsor",

  # AUCSSCAT codelist
  "CL.AUCSSCAT", "AUC Category", "AUCSSCAT", "Low", "Low Exposure", "Sponsor",
  "CL.AUCSSCAT", "AUC Category", "AUCSSCAT", "Medium", "Medium Exposure", "Sponsor",
  "CL.AUCSSCAT", "AUC Category", "AUCSSCAT", "High", "High Exposure", "Sponsor",

  # AUCSSQ codelist
  "CL.AUCSSQ", "AUC Quartile", "AUCSSQ", "Q1", "Quartile 1", "Sponsor",
  "CL.AUCSSQ", "AUC Quartile", "AUCSSQ", "Q2", "Quartile 2", "Sponsor",
  "CL.AUCSSQ", "AUC Quartile", "AUCSSQ", "Q3", "Quartile 3", "Sponsor",
  "CL.AUCSSQ", "AUC Quartile", "AUCSSQ", "Q4", "Quartile 4", "Sponsor",

  # AUCSSMED codelist
  "CL.AUCSSMED", "AUC Median Split", "AUCSSMED", "Below Median", "Below Median", "Sponsor",
  "CL.AUCSSMED", "AUC Median Split", "AUCSSMED", "Above Median", "Above Median", "Sponsor",

  # AGEGR1 codelist
  "CL.AGEGR1", "Age Group 1", "AGEGR1", "<65", "Less than 65 years", "Sponsor",
  "CL.AGEGR1", "Age Group 1", "AGEGR1", "65-75", "65 to 75 years", "Sponsor",
  "CL.AGEGR1", "Age Group 1", "AGEGR1", ">75", "Greater than 75 years", "Sponsor",

  # WTBLGR1 codelist
  "CL.WTBLGR1", "Weight Group 1", "WTBLGR1", "<70 kg", "Less than 70 kg", "Sponsor",
  "CL.WTBLGR1", "Weight Group 1", "WTBLGR1", ">=70 kg", "Greater than or equal to 70 kg", "Sponsor",

  # NY codelist (standard for flags)
  "CL.NY", "No Yes Response", "NY", "Y", "Yes", "CDISC",
  "CL.NY", "No Yes Response", "NY", "", "No", "CDISC",

  # SEX codelist (standard CDISC)
  "CL.SEX", "Sex", "SEX", "M", "Male", "CDISC",
  "CL.SEX", "Sex", "SEX", "F", "Female", "CDISC",
  "CL.SEX", "Sex", "SEX", "U", "Unknown", "CDISC",

  # ATPT codelist
  "CL.ATPT", "Analysis Timepoint", "ATPT", "BASELINE", "Baseline", "CDISC",

  # AVISIT codelist
  "CL.AVISIT", "Analysis Visit", "AVISIT", "BASELINE", "Baseline", "CDISC",

  # Treatment codelists (study-specific)
  "CL.TRT", "Treatment", "TRT", "Placebo", "Placebo", "Sponsor",
  "CL.TRT", "Treatment", "TRT", "Xanomeline Low Dose", "Xanomeline Low Dose", "Sponsor",
  "CL.TRT", "Treatment", "TRT", "Xanomeline High Dose", "Xanomeline High Dose", "Sponsor"
)

# Now create the codes dataframe in the format metacore expects
# Format: code_id, code (value), decode (label)
code_list <- ct_spec %>%
  select(code_id, code, decode) %>%
  group_by(code_id) %>%
  nest(codes = c(code, decode)) %>%
  mutate(
    # Get name and type from first occurrence
    name = map_chr(code_id, ~ ct_spec$name[ct_spec$code_id == .x][1]),
    type = map_chr(code_id, ~ ct_spec$type[ct_spec$code_id == .x][1])
  ) %>%
  select(code_id, name, type, codes)

# ===============================================================================
# VALUE LEVEL METADATA SPECIFICATION
# ===============================================================================

# Correct format: variable, where, type, length, label, common, format, origin
# Origin column IS needed here
value_spec <- tibble::tribble(
  ~variable, ~where, ~type, ~length, ~label, ~common, ~format, ~origin,

  # Optional variables (may not be present in all records)
  "ADTF", "!is.na(ADTF)", "text", 1, "Analysis Date Imputation Flag", NA, "$1.", "Assigned",
  "DTYPE", "!is.na(DTYPE)", "text", 8, "Derivation Type", NA, "$8.", "Assigned",
  "SRCDOM", "!is.na(SRCDOM)", "text", 8, "Source Data", NA, "$8.", "Predecessor",
  "SRCVAR", "!is.na(SRCVAR)", "text", 40, "Source Variable", NA, "$40.", "Predecessor",
  "SRCSEQ", "!is.na(SRCSEQ)", "integer", 8, "Source Sequence Number", NA, NA, "Predecessor",
  "ALBBL", "!is.na(ALBBL)", "float", 8, "Baseline Albumin (g/dL)", NA, NA, "Predecessor",

  # Exposure variables (conditional on active treatment)
  "AUCSS", "DOSE > 0", "float", 8, "Steady-State AUC (ug*h/mL)", "N", NA, "Assigned",
  "CMAXSS", "DOSE > 0", "float", 8, "Steady-State Cmax (ug/mL)", "N", NA, "Assigned",
  "AUCSSCAT", "DOSE > 0", "text", 20, "AUC Category (Tertiles)", "N", "$20.", "Assigned",
  "AUCSSQ", "DOSE > 0", "text", 20, "AUC Quartile", "N", "$20.", "Assigned"
)

# ===============================================================================
# DERIVATIONS SPECIFICATION
# ===============================================================================

# Correct column names: derivation_id, derivation
# derivation_id should match variable name
derivations_spec <- tibble::tribble(
  ~derivation_id, ~derivation,

  # Core derivations
  "EVENT", "1 - CNSR",
  "AVALU", "Assigned value: DAYS (for time-to-event)",
  "AVALC", "EVENT if EVENT=1, CENSORED if CNSR=1",

  # Numeric identifiers
  "STUDYIDN", "Numeric from USUBJID word 1 (separated by hyphen)",
  "SITEIDN", "Numeric from USUBJID word 2 (separated by hyphen)",
  "USUBJIDN", "Numeric from USUBJID word 3 (separated by hyphen)",
  "SUBJIDN", "Numeric version of SUBJID",

  # Demographics numeric
  "SEXN", "1=M, 2=F, 3=U",
  "RACEN", "1=AMERICAN INDIAN OR ALASKA NATIVE, 2=ASIAN, 3=BLACK OR AFRICAN AMERICAN, 4=NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER, 5=WHITE, 6=OTHER",
  "ETHNICN", "1=HISPANIC OR LATINO, 2=NOT HISPANIC OR LATINO, 3=OTHER",
  "AGEGR1", "Conditional: <65, 65-75, >75 based on AGE",
  "AGEGR1N", "1=<65, 2=65-75, 3=>75",

  # Treatment numeric
  "ARMN", "0=Placebo, 1=Xanomeline Low Dose, 2=Xanomeline High Dose, 3=OTHER",
  "ACTARMN", "0=Placebo, 1=Xanomeline Low Dose, 2=Xanomeline High Dose, 3=OTHER",
  "TRT01PN", "Numeric version of TRT01P (same mapping as ARMN)",
  "TRT01AN", "Numeric version of TRT01A (same mapping as ACTARMN)",

  # Vitals
  "BMIBL", "WTBL / (HTBL/100)^2",
  "BSABL", "Mosteller formula: 0.007184 * WTBL^0.425 * HTBL^0.725",
  "WTBLGR1", "Conditional: <70 kg vs >=70 kg based on WTBL",

  # Labs
  "TBILBL", "Renamed from BILIBL",
  "CRCLBL", "Cockcroft-Gault equation based on CREATBL, AGE, WTBL, SEX",
  "EGFRBL", "CKD-EPI equation based on CREATBL, AGE, SEX",

  # Exposure transformations
  "AUCSSLOG", "log(AUCSS + 0.01)",
  "CMAXSSLOG", "log(CMAXSS + 0.01)",
  "CAVGSSLOG", "log(CAVGSS + 0.01)",
  "AUCSSSTD", "(AUCSS - mean(AUCSS)) / sd(AUCSS) for active subjects only",
  "CMAXSSSTD", "(CMAXSS - mean(CMAXSS)) / sd(CMAXSS) for active subjects only",
  "AUCSSN", "AUCSS / median(AUCSS) for active subjects only",
  "AUCSSDOSE", "AUCSS / DOSE",
  "CMAXSSDOSE", "CMAXSS / DOSE",

  # Exposure categories
  "AUCSSCAT", "Tertiles of AUCSS for active subjects: Low, Medium, High",
  "AUCSSCATN", "1=Low, 2=Medium, 3=High",
  "AUCSSQ", "Quartiles of AUCSS for active subjects: Q1, Q2, Q3, Q4",
  "AUCSSQN", "1=Q1, 2=Q2, 3=Q3, 4=Q4",
  "AUCSSMED", "Below Median if AUCSS < median(AUCSS), Above Median otherwise",

  # Analysis variables
  "ATPT", "Assigned: BASELINE",
  "ATPTN", "Assigned: 0",
  "AVISIT", "Assigned: BASELINE",
  "AVISITN", "Assigned: 0",

  # Analysis flags
  "ANL01FL", "Y if PARAMCD=PFS (primary endpoint), blank otherwise",
  "ANL02FL", "Y if PARAMCD=OS (secondary endpoint), blank otherwise",
  "ANL03FL", "Y if PARAMCD=TTP (tertiary endpoint), blank otherwise",
  "ANL04FL", "Y if PARAMCD=TTNT (treatment discontinuation), blank otherwise",

  # Categories
  "PARCAT1", "Assigned: EFFICACY",
  "PARCAT2", "Assigned: TIME TO EVENT",

  # Sequence
  "ASEQ", "Sequential numbering within USUBJID ordered by PARAMN, AVISITN"
)

# ===============================================================================
# CREATE METACORE OBJECT
# ===============================================================================

# Build metacore object with correct argument names
metacore <- metacore(
  ds_spec = ds_spec,
  var_spec = var_spec,
  codelist = code_list,
  value_spec = value_spec,
  derivations = derivations_spec,
  supp = NULL
)

# Save as RDS
saveRDS(metacore, "specifications/ADEE_metacore.rds")

message("✓ Saved: specifications/ADEE_metacore.rds")

# ===============================================================================
# SAVE AS EXCEL FOR EDITING
# ===============================================================================

# Save human-readable version with origin column
ct_spec_excel <- ct_spec %>%
  select(code_id, name, type, code, decode, origin)

spec_list <- list(
  Dataset = ds_spec,
  Variables = var_spec,
  Code_Lists = ct_spec_excel,
  Value_Level = value_spec,
  Derivations = derivations_spec
)

writexl::write_xlsx(spec_list, "specifications/ADEE_spec.xlsx")

message("✓ Saved: specifications/ADEE_spec.xlsx")

# ===============================================================================
# CREATE SUMMARY REPORT
# ===============================================================================

cat("\n")
cat(strrep("=", 80), "\n")
cat("ADEE METACORE SPECIFICATION SUMMARY\n")
cat(strrep("=", 80), "\n\n")

cat("Dataset:", ds_spec$dataset, "\n")
cat("Label:", ds_spec$label, "\n")
cat("Structure:", ds_spec$structure, "\n\n")

cat("Variables:", nrow(var_spec), "\n")
cat("  Common variables:", sum(var_spec$common == "Y", na.rm = TRUE), "\n")
cat("  Dataset-specific:", sum(is.na(var_spec$common)), "\n\n")

cat("Code Lists:", nrow(code_list), "\n")
cat("  Total controlled terms:", nrow(ct_spec), "\n")
cat("  CDISC terms:", sum(ct_spec$origin == "CDISC"), "\n")
cat("  Sponsor terms:", sum(ct_spec$origin == "Sponsor"), "\n\n")

cat("Value-Level Metadata:", nrow(value_spec), "\n")
cat("Derivations:", nrow(derivations_spec), "\n\n")

cat("Code List Summary:\n")
code_summary <- ct_spec %>%
  group_by(code_id, name) %>%
  summarise(n_codes = n(), .groups = "drop") %>%
  arrange(code_id)

print(code_summary)

cat("\n")
cat("Next steps:\n")
cat("  1. Review specifications/ADEE_spec.xlsx\n")
cat("  2. Edit/refine as needed\n")
cat("  3. Reload: metacore <- readRDS('specifications/ADEE_metacore.rds')\n")
cat("  4. Use in ADEE.R program\n\n")

# ===============================================================================
# END OF PROGRAM
# ===============================================================================
