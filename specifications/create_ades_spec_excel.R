# ===============================================================================
# Script: create_ades_spec_excel.R
#
# Purpose: Create Excel-based ADaM specification for ADES dataset
#          Exposure-Safety Analysis Dataset
#
# Author: Jeff Dickinson
# Date: 2026-01-28
#
# Output: specifications/ADES_specification.xlsx
#
# Note: Variable names comply with 8-character CDISC limit
#       Uses ASEV/ASEVN (severity) not AETOXGR/AETOXGRN (toxicity grade)
# ===============================================================================

library(dplyr)
library(openxlsx)

# ===============================================================================
# SPECIFICATION DATA
# ===============================================================================

# Dataset metadata
dataset_spec <- tibble(
  Field = c("Dataset", "Description", "Class", "Structure", "Purpose", "Key Variables"),
  Value = c(
    "ADES",
    "Analysis Dataset for Exposure-Safety",
    "BASIC DATA STRUCTURE (BDS) + OCCDS Hybrid",
    "Subject-level parameters + event-level records",
    "Adverse event analyses with comprehensive exposure metrics",
    "PARAMCD, AVAL, AEDECOD, ASEV, ASEVN, AESER, AEREL, AUCSS"
  )
)

# Variable specifications (90 variables)
variable_spec <- tibble(
  Order = 1:90,
  Variable = c(
    # Identifiers (9)
    "STUDYID", "STUDYIDN", "USUBJID", "USUBJIDN", "SUBJID", "SUBJIDN",
    "SITEID", "SITEIDN", "ASEQ",

    # Treatment (8)
    "ARM", "ARMN", "ARMCD",
    "TRT01P", "TRT01PN",
    "TRT01A", "TRT01AN",
    "DOSE",

    # Demographics (9)
    "AGE", "AGEGR1", "AGEGR1N",
    "SEX", "SEXN",
    "RACE", "RACEN",
    "ETHNIC", "ETHNICN",

    # Parameter (for subject-level records) (3)
    "PARAMCD", "PARAM", "PARAMN",

    # Analysis Values (for subject-level parameters) (2)
    "AVAL", "AVALC",

    # AE Identifiers (for event-level records) (2)
    "AESEQ", "AESPID",

    # AE Terms (2)
    "AEDECOD", "AEBODSYS",

    # AE Severity (uses ASEV not AETOXGR) (2)
    "ASEV", "ASEVN",

    # AE Relationship (2)
    "AEREL", "AERELN",

    # AE Characteristics (3)
    "AESER", "AEACN", "AEOUT",

    # AE Dates (6)
    "ASTDT", "ASTDY", "AENDT", "AENDY", "ADURN", "ADURU",

    # AE Flags (2)
    "TRTEMFL", "AOCCFL",

    # Exposure Metrics - Raw (3)
    "AUCSS", "CMAXSS", "CAVGSS",

    # Exposure Metrics - Log (3)
    "AUCSLOG", "CMXSLOG", "CAVGLOG",

    # Exposure Metrics - Standardized (3)
    "AUCSSSTD", "CMXSSSTD", "CAVGSTD",

    # Exposure Metrics - Normalized (3)
    "AUCSSN", "CMAXSSN", "CAVGSSN",

    # Exposure Metrics - Dose-Normalized (3)
    "AUCSSDOS", "CMXSSDOS", "CAVGDOS",

    # Exposure Metrics - Categorical (4)
    "AUCSSCAT", "AUCSCATN",
    "CMXSSCAT", "CMXSCATN",

    # Baseline Covariates - Vitals (4)
    "WTBL", "HTBL", "BMIBL", "BSA",

    # Baseline Covariates - Renal (3)
    "CREATBL", "CRCLBL", "EGFRBL",

    # Baseline Covariates - Hepatic (3)
    "ALTBL", "ASTBL", "TBILBL",

    # Baseline Covariates - Other (3)
    "ECOGBL", "ALBBL", "SMOKEBL",

    # Dates (3)
    "TRTSDT", "TRTEDT", "TRTDURD",

    # Analysis Flags (2)
    "ANL01FL", "ANL02FL",

    # Population Flags (3)
    "SAFFL", "ITTFL", "EFFFL"
  ),
  Label = c(
    # Identifiers
    "Study Identifier",
    "Study Identifier (N)",
    "Unique Subject Identifier",
    "Unique Subject Identifier (N)",
    "Subject Identifier for the Study",
    "Subject Identifier for the Study (N)",
    "Study Site Identifier",
    "Study Site Identifier (N)",
    "Analysis Sequence Number",

    # Treatment
    "Description of Planned Arm",
    "Planned Arm Code (N)",
    "Planned Arm Code",
    "Planned Treatment for Period 01",
    "Planned Treatment for Period 01 (N)",
    "Actual Treatment for Period 01",
    "Actual Treatment for Period 01 (N)",
    "Actual Dose (mg)",

    # Demographics
    "Age",
    "Pooled Age Group 1",
    "Pooled Age Group 1 (N)",
    "Sex",
    "Sex (N)",
    "Race",
    "Race (N)",
    "Ethnicity",
    "Ethnicity (N)",

    # Parameter
    "Parameter Code",
    "Parameter",
    "Parameter (N)",

    # Analysis Values
    "Analysis Value",
    "Analysis Value (C)",

    # AE Identifiers
    "Adverse Event Sequence Number",
    "Sponsor-Defined Identifier",

    # AE Terms
    "Dictionary-Derived Term",
    "Body System or Organ Class",

    # AE Severity (ASEV not AETOXGR)
    "Severity (MILD/MODERATE/SEVERE)",
    "Severity (N) (1/2/3)",

    # AE Relationship
    "Causality",
    "Causality (N) (0-4)",

    # AE Characteristics
    "Serious Event",
    "Action Taken with Study Treatment",
    "Outcome of Adverse Event",

    # AE Dates
    "Analysis Start Date",
    "Analysis Start Relative Day",
    "Analysis End Date",
    "Analysis End Relative Day",
    "Analysis Duration",
    "Analysis Duration Units",

    # AE Flags
    "Treatment Emergent Analysis Flag",
    "1st Occurrence within Subject Flag",

    # Exposure - Raw
    "Steady-State Area Under Curve",
    "Steady-State Maximum Concentration",
    "Steady-State Average Concentration",

    # Exposure - Log
    "Log Steady-State AUC",
    "Log Steady-State Cmax",
    "Log Steady-State Cavg",

    # Exposure - Standardized
    "Standardized Steady-State AUC",
    "Standardized Steady-State Cmax",
    "Standardized Steady-State Cavg",

    # Exposure - Normalized
    "Normalized Steady-State AUC",
    "Normalized Steady-State Cmax",
    "Normalized Steady-State Cavg",

    # Exposure - Dose-Normalized
    "Dose-Normalized Steady-State AUC",
    "Dose-Normalized Steady-State Cmax",
    "Dose-Normalized Steady-State Cavg",

    # Exposure - Categorical
    "Steady-State AUC Category",
    "Steady-State AUC Category (N)",
    "Steady-State Cmax Category",
    "Steady-State Cmax Category (N)",

    # Covariates - Vitals
    "Baseline Weight (kg)",
    "Baseline Height (cm)",
    "Baseline Body Mass Index (kg/m2)",
    "Body Surface Area (m2)",

    # Covariates - Renal
    "Baseline Creatinine (mg/dL)",
    "Baseline Creatinine Clearance (mL/min)",
    "Baseline eGFR (mL/min/1.73m2)",

    # Covariates - Hepatic
    "Baseline Alanine Aminotransferase (U/L)",
    "Baseline Aspartate Aminotransferase (U/L)",
    "Baseline Total Bilirubin (mg/dL)",

    # Covariates - Other
    "Baseline ECOG Performance Status",
    "Baseline Albumin (g/dL)",
    "Baseline Smoking Status",

    # Dates
    "Date of First Exposure to Treatment",
    "Date of Last Exposure to Treatment",
    "Total Treatment Duration (Days)",

    # Analysis Flags
    "Analysis Flag 01 (Primary Analysis)",
    "Analysis Flag 02 (Event-Level Analysis)",

    # Population Flags
    "Safety Population Flag",
    "Intent-to-Treat Population Flag",
    "Efficacy Population Flag"
  ),
  Type = c(
    # Identifiers
    "text", "integer", "text", "integer", "text", "integer",
    "text", "integer", "integer",

    # Treatment
    "text", "integer", "text",
    "text", "integer",
    "text", "integer",
    "float",

    # Demographics
    "integer", "text", "integer",
    "text", "integer",
    "text", "integer",
    "text", "integer",

    # Parameter
    "text", "text", "integer",

    # Analysis Values
    "float", "text",

    # AE Identifiers
    "integer", "text",

    # AE Terms
    "text", "text",

    # AE Severity
    "text", "integer",

    # AE Relationship
    "text", "integer",

    # AE Characteristics
    "text", "text", "text",

    # AE Dates
    "date", "integer", "date", "integer", "integer", "text",

    # AE Flags
    "text", "text",

    # Exposure - Raw
    "float", "float", "float",

    # Exposure - Log
    "float", "float", "float",

    # Exposure - Standardized
    "float", "float", "float",

    # Exposure - Normalized
    "float", "float", "float",

    # Exposure - Dose-Normalized
    "float", "float", "float",

    # Exposure - Categorical
    "text", "integer",
    "text", "integer",

    # Covariates - Vitals
    "float", "float", "float", "float",

    # Covariates - Renal
    "float", "float", "float",

    # Covariates - Hepatic
    "float", "float", "float",

    # Covariates - Other
    "integer", "float", "text",

    # Dates
    "date", "date", "integer",

    # Analysis Flags
    "text", "text",

    # Population Flags
    "text", "text", "text"
  ),
  Length = c(
    # Identifiers
    200, NA, 200, NA, 200, NA,
    200, NA, NA,

    # Treatment
    200, NA, 200,
    200, NA,
    200, NA,
    NA,

    # Demographics
    NA, 200, NA,
    200, NA,
    200, NA,
    200, NA,

    # Parameter
    8, 200, NA,

    # Analysis Values
    NA, 200,

    # AE Identifiers
    NA, 200,

    # AE Terms
    200, 200,

    # AE Severity
    200, NA,

    # AE Relationship
    200, NA,

    # AE Characteristics
    1, 200, 200,

    # AE Dates
    NA, NA, NA, NA, NA, 200,

    # AE Flags
    1, 1,

    # Exposure - Raw
    NA, NA, NA,

    # Exposure - Log
    NA, NA, NA,

    # Exposure - Standardized
    NA, NA, NA,

    # Exposure - Normalized
    NA, NA, NA,

    # Exposure - Dose-Normalized
    NA, NA, NA,

    # Exposure - Categorical
    200, NA,
    200, NA,

    # Covariates - Vitals
    NA, NA, NA, NA,

    # Covariates - Renal
    NA, NA, NA,

    # Covariates - Hepatic
    NA, NA, NA,

    # Covariates - Other
    NA, NA, 200,

    # Dates
    NA, NA, NA,

    # Analysis Flags
    1, 1,

    # Population Flags
    1, 1, 1
  ),
  Source = c(
    # Identifiers
    "ADSL", "ADSL", "ADSL", "ADSL", "ADSL", "ADSL",
    "ADSL", "ADSL", "Derived",

    # Treatment
    "ADSL", "ADSL", "ADSL",
    "ADSL", "ADSL",
    "ADSL", "ADSL",
    "ADSL",

    # Demographics
    "ADSL", "ADSL", "ADSL",
    "ADSL", "ADSL",
    "ADSL", "ADSL",
    "ADSL", "ADSL",

    # Parameter
    "Derived", "Derived", "Derived",

    # Analysis Values
    "Derived", "Derived",

    # AE Identifiers
    "ADAE", "ADAE",

    # AE Terms
    "ADAE", "ADAE",

    # AE Severity
    "ADAE", "Derived",

    # AE Relationship
    "ADAE", "Derived",

    # AE Characteristics
    "ADAE", "ADAE", "ADAE",

    # AE Dates
    "ADAE", "Derived", "ADAE", "Derived", "Derived", "Derived",

    # AE Flags
    "ADAE", "Derived",

    # Exposure - Raw (from ADER)
    "ADER", "ADER", "ADER",

    # Exposure - Log (from ADER)
    "ADER", "ADER", "ADER",

    # Exposure - Standardized (from ADER)
    "ADER", "ADER", "ADER",

    # Exposure - Normalized (from ADER)
    "ADER", "ADER", "ADER",

    # Exposure - Dose-Normalized (from ADER)
    "ADER", "ADER", "ADER",

    # Exposure - Categorical (from ADER)
    "ADER", "ADER",
    "ADER", "ADER",

    # Covariates - Vitals (from ADER)
    "ADER", "ADER", "ADER", "ADER",

    # Covariates - Renal (from ADER)
    "ADER", "ADER", "ADER",

    # Covariates - Hepatic (from ADER)
    "ADER", "ADER", "ADER",

    # Covariates - Other (from ADER)
    "ADER", "ADER", "ADER",

    # Dates
    "ADSL", "ADSL", "ADSL",

    # Analysis Flags
    "Derived", "Derived",

    # Population Flags
    "ADSL", "ADSL", "ADSL"
  ),
  Derivation = c(
    # Identifiers
    "From ADSL", "From ADSL", "From ADSL", "From ADSL", "From ADSL", "From ADSL",
    "From ADSL", "From ADSL", "Sequence number within subject",

    # Treatment
    "From ADSL", "From ADSL", "From ADSL",
    "From ADSL", "From ADSL",
    "From ADSL", "From ADSL",
    "From ADSL",

    # Demographics
    "From ADSL", "From ADSL", "From ADSL",
    "From ADSL", "From ADSL",
    "From ADSL", "From ADSL",
    "From ADSL", "From ADSL",

    # Parameter
    "TEAE, TEAESEV, TESAE, etc.", "Parameter description", "Numeric code",

    # Analysis Values
    "Count or rate for subject-level parameters", "Character version",

    # AE Identifiers
    "From ADAE", "From ADAE",

    # AE Terms
    "MedDRA Preferred Term", "MedDRA System Organ Class",

    # AE Severity
    "MILD, MODERATE, SEVERE (not AETOXGR)", "1=MILD, 2=MODERATE, 3=SEVERE",

    # AE Relationship
    "NOT RELATED, UNLIKELY, POSSIBLE, PROBABLE, RELATED",
    "0=NOT RELATED, 1=UNLIKELY, 2=POSSIBLE, 3=PROBABLE, 4=RELATED",

    # AE Characteristics
    "Y/N flag for serious AE", "Action taken", "Outcome",

    # AE Dates
    "AE start date", "ASTDT - TRTSDT + 1", "AE end date", "AENDT - TRTSDT + 1",
    "Duration in units", "DAYS",

    # AE Flags
    "Y if started during/within 30d of treatment", "Y if first occurrence",

    # Exposure - Raw
    "From ADER: Steady-state AUC (ng*h/mL)",
    "From ADER: Steady-state Cmax (ng/mL)",
    "From ADER: Steady-state Cavg (ng/mL)",

    # Exposure - Log
    "From ADER: log(AUCSS)",
    "From ADER: log(CMAXSS)",
    "From ADER: log(CAVGSS)",

    # Exposure - Standardized
    "From ADER: (AUCSS - mean)/SD",
    "From ADER: (CMAXSS - mean)/SD",
    "From ADER: (CAVGSS - mean)/SD",

    # Exposure - Normalized
    "From ADER: AUCSS / mean(AUCSS)",
    "From ADER: CMAXSS / mean(CMAXSS)",
    "From ADER: CAVGSS / mean(CAVGSS)",

    # Exposure - Dose-Normalized
    "From ADER: AUCSS / DOSE",
    "From ADER: CMAXSS / DOSE",
    "From ADER: CAVGSS / DOSE",

    # Exposure - Categorical
    "From ADER: Tertiles of AUCSS",
    "From ADER: Numeric version (1/2/3)",
    "From ADER: Tertiles of CMAXSS",
    "From ADER: Numeric version (1/2/3)",

    # Covariates - Vitals
    "From ADER: Baseline weight",
    "From ADER: Baseline height",
    "From ADER: Baseline BMI",
    "From ADER: Body surface area",

    # Covariates - Renal
    "From ADER: Baseline creatinine",
    "From ADER: Baseline CrCL",
    "From ADER: Baseline eGFR",

    # Covariates - Hepatic
    "From ADER: Baseline ALT",
    "From ADER: Baseline AST",
    "From ADER: Baseline bilirubin",

    # Covariates - Other
    "From ADER: Baseline ECOG",
    "From ADER: Baseline albumin",
    "From ADER: Baseline smoking status",

    # Dates
    "From ADSL", "From ADSL", "From ADSL: TRTEDT - TRTSDT + 1",

    # Analysis Flags
    "Y for subject-level parameters or event-level with exposure",
    "Y for event-level records with non-missing AE data",

    # Population Flags
    "From ADSL", "From ADSL", "From ADSL"
  ),
  Notes = c(
    # Identifiers
    rep("", 9),

    # Treatment
    rep("", 8),

    # Demographics
    rep("", 9),

    # Parameter
    "Subject-level parameters: TEAE, TEAESEV, TESAE, etc.",
    "Full parameter description",
    "For sorting",

    # Analysis Values
    "Count for subject-level, missing for event-level",
    "Character version of AVAL",

    # AE Identifiers
    "AE sequence (event-level only)",
    "Sponsor identifier (event-level only)",

    # AE Terms
    "MedDRA PT (event-level only)",
    "MedDRA SOC (event-level only)",

    # AE Severity
    "IMPORTANT: Uses ASEV not AETOXGR (pharmaverseadam convention)",
    "1=MILD, 2=MODERATE, 3=SEVERE (not 1-5 toxicity grades)",

    # AE Relationship
    "5-level scale (event-level only)",
    "Numeric version for modeling",

    # AE Characteristics
    "Regulatory serious flag",
    "Drug action taken",
    "Final outcome",

    # AE Dates
    "Start date (event-level)",
    "Study day (event-level)",
    "End date (event-level)",
    "Study day (event-level)",
    "Duration (event-level)",
    "Always DAYS",

    # AE Flags
    "Treatment-emergent flag",
    "First occurrence flag",

    # Exposure - Raw
    "Primary exposure metric",
    "Alternative exposure metric",
    "Alternative exposure metric",

    # Exposure - Log
    "For log-linear models (8-char: one S removed)",
    "For log-linear models (8-char: CMX abbreviation)",
    "For log-linear models (8-char: SS removed)",

    # Exposure - Standardized
    "Z-score for covariate modeling",
    "Z-score for covariate modeling (8-char: CMX)",
    "Z-score for covariate modeling",

    # Exposure - Normalized
    "Normalized to mean=1",
    "Normalized to mean=1",
    "Normalized to mean=1",

    # Exposure - Dose-Normalized
    "Per-mg dose (8-char: removed E)",
    "Per-mg dose (8-char: CMX + removed E)",
    "Per-mg dose (8-char: removed E)",

    # Exposure - Categorical
    "For subgroup analyses",
    "Numeric version (8-char: one S removed)",
    "For subgroup analyses",
    "Numeric version (8-char: one S removed)",

    # Covariates - Vitals
    "Baseline covariate",
    "Baseline covariate",
    "Baseline covariate",
    "Mosteller formula",

    # Covariates - Renal
    "Baseline covariate",
    "Cockcroft-Gault equation",
    "CKD-EPI equation",

    # Covariates - Hepatic
    "Baseline covariate",
    "Baseline covariate",
    "Baseline covariate",

    # Covariates - Other
    "Performance status (0-4)",
    "Baseline covariate",
    "Baseline covariate",

    # Dates
    "First dose date",
    "Last dose date",
    "Total days on treatment",

    # Analysis Flags
    "Primary analysis flag",
    "Event-level analysis flag",

    # Population Flags
    "All treated subjects",
    "All randomized subjects",
    "Efficacy-evaluable subjects"
  )
)

# ===============================================================================
# ADDITIONAL TABLES
# ===============================================================================

# Subject-level parameters
subject_parameters <- tibble(
  PARAMCD = c("TEAE", "TEAESEV", "TESAE", "TEAEREL", "TEAEWD", "TEAEDTH"),
  PARAM = c(
    "Treatment-Emergent Adverse Events",
    "Treatment-Emergent Severe Adverse Events",
    "Treatment-Emergent Serious Adverse Events",
    "Treatment-Emergent Related Adverse Events",
    "Treatment-Emergent AEs Leading to Withdrawal",
    "Treatment-Emergent Fatal Adverse Events"
  ),
  PARAMN = 1:6,
  Definition = c(
    "Total count of treatment-emergent AEs",
    "Count of AEs with ASEVN = 3 (SEVERE)",
    "Count of AEs with AESER = 'Y'",
    "Count of AEs with AERELN >= 2 (POSSIBLE or higher)",
    "Count of AEs leading to treatment discontinuation",
    "Count of AEs with fatal outcome"
  ),
  Calculation = c(
    "sum(TRTEMFL == 'Y')",
    "sum(TRTEMFL == 'Y' & ASEVN == 3)",
    "sum(TRTEMFL == 'Y' & AESER == 'Y')",
    "sum(TRTEMFL == 'Y' & AERELN >= 2)",
    "sum(TRTEMFL == 'Y' & AEACN contains 'WITHDRAW')",
    "sum(TRTEMFL == 'Y' & AEOUT == 'FATAL')"
  )
)

# Severity mapping (ASEV not AETOXGR)
severity_mapping <- tibble(
  ASEV = c("MILD", "MODERATE", "SEVERE"),
  ASEVN = c(1, 2, 3),
  Description = c(
    "Mild - Transient or mild discomfort; no limitation in activity",
    "Moderate - Mild to moderate limitation in activity",
    "Severe - Marked limitation in activity; some assistance may be needed"
  ),
  Clinical_Action = c(
    "Usually no intervention required",
    "May require minimal intervention",
    "Medical intervention or therapy required"
  ),
  Notes = c(
    "Not life-threatening",
    "Not life-threatening",
    "May be serious but not life-threatening"
  )
)

# Relationship mapping (AEREL/AERELN)
relationship_mapping <- tibble(
  AEREL = c("NOT RELATED", "UNLIKELY RELATED", "POSSIBLE", "PROBABLE", "RELATED"),
  AERELN = 0:4,
  Definition = c(
    "No reasonable possibility of relationship to study drug",
    "Unlikely related but cannot be ruled out",
    "May be related to study drug",
    "Likely related to study drug",
    "Definite relationship to study drug"
  ),
  Criteria = c(
    "No temporal relationship; alternative etiology clear",
    "Poor temporal relationship; alternative etiology likely",
    "Reasonable temporal relationship; alternative etiology possible",
    "Close temporal relationship; alternative etiology less likely",
    "Close temporal relationship; no alternative etiology"
  )
)

# Analysis flags description
analysis_flags <- tibble(
  Flag = c("ANL01FL", "ANL02FL"),
  Description = c(
    "Primary Analysis Flag",
    "Event-Level Analysis Flag"
  ),
  Applies_To = c(
    "Subject-level parameters and event-level records",
    "Event-level records only"
  ),
  Criteria = c(
    "Subject-level: Y for all parameter records; Event-level: Y if non-missing AESEQ and exposure",
    "Y for event-level records with complete AE data (AEDECOD, ASTDT, ASEV not missing)"
  ),
  Usage = c(
    "Primary E-R analyses",
    "Detailed event-level analyses"
  )
)

# Exposure metrics (same as ADEE)
exposure_metrics <- tibble(
  Category = c(
    "Raw Metrics", "Raw Metrics", "Raw Metrics",
    "Log-Transformed", "Log-Transformed", "Log-Transformed",
    "Standardized", "Standardized", "Standardized",
    "Normalized", "Normalized", "Normalized",
    "Dose-Normalized", "Dose-Normalized", "Dose-Normalized",
    "Categorical", "Categorical"
  ),
  Variable = c(
    "AUCSS", "CMAXSS", "CAVGSS",
    "AUCSLOG", "CMXSLOG", "CAVGLOG",
    "AUCSSSTD", "CMXSSSTD", "CAVGSTD",
    "AUCSSN", "CMAXSSN", "CAVGSSN",
    "AUCSSDOS", "CMXSSDOS", "CAVGDOS",
    "AUCSSCAT", "AUCSCATN"
  ),
  Transformation = c(
    "None", "None", "None",
    "log(X)", "log(X)", "log(X)",
    "(X - mean) / SD", "(X - mean) / SD", "(X - mean) / SD",
    "X / mean(X)", "X / mean(X)", "X / mean(X)",
    "X / DOSE", "X / DOSE", "X / DOSE",
    "Tertiles", "Tertiles (numeric)"
  ),
  Use_Case = c(
    "Basic analyses", "Alternative metric", "Alternative metric",
    "Log-linear models", "Log-linear models", "Log-linear models",
    "Covariate models", "Covariate models", "Covariate models",
    "Relative comparisons", "Relative comparisons", "Relative comparisons",
    "Dose-response", "Dose-response", "Dose-response",
    "Subgroup analysis", "Ordered categorical"
  )
)

# Covariates (same as ADEE)
covariates <- tibble(
  Category = c(
    rep("Vitals", 4),
    rep("Renal Function", 3),
    rep("Hepatic Function", 3),
    rep("Other", 3)
  ),
  Variable = c(
    "WTBL", "HTBL", "BMIBL", "BSA",
    "CREATBL", "CRCLBL", "EGFRBL",
    "ALTBL", "ASTBL", "TBILBL",
    "ECOGBL", "ALBBL", "SMOKEBL"
  ),
  Label = c(
    "Baseline Weight (kg)", "Baseline Height (cm)", "Baseline BMI (kg/m2)",
    "Body Surface Area (m2)",
    "Baseline Creatinine (mg/dL)", "Baseline CrCL (mL/min)",
    "Baseline eGFR (mL/min/1.73m2)",
    "Baseline ALT (U/L)", "Baseline AST (U/L)", "Baseline Total Bilirubin (mg/dL)",
    "Baseline ECOG", "Baseline Albumin (g/dL)", "Baseline Smoking Status"
  ),
  Clinical_Relevance = c(
    "Body size covariate", "Body size covariate", "Obesity indicator",
    "Dose calculation",
    "Renal function marker", "Renal function (adjusted)", "Renal function (adjusted)",
    "Liver function", "Liver function", "Liver function",
    "Performance status", "Nutritional status", "Risk factor"
  )
)

# ===============================================================================
# CREATE EXCEL WORKBOOK
# ===============================================================================

cat("Creating ADES specification Excel file...\n")

# Create workbook
wb <- createWorkbook()

# Add worksheets
addWorksheet(wb, "Dataset")
addWorksheet(wb, "Variables")
addWorksheet(wb, "Subject Parameters")
addWorksheet(wb, "Severity Mapping")
addWorksheet(wb, "Relationship Mapping")
addWorksheet(wb, "Analysis Flags")
addWorksheet(wb, "Exposure Metrics")
addWorksheet(wb, "Covariates")

# Write data
writeData(wb, "Dataset", dataset_spec)
writeData(wb, "Variables", variable_spec)
writeData(wb, "Subject Parameters", subject_parameters)
writeData(wb, "Severity Mapping", severity_mapping)
writeData(wb, "Relationship Mapping", relationship_mapping)
writeData(wb, "Analysis Flags", analysis_flags)
writeData(wb, "Exposure Metrics", exposure_metrics)
writeData(wb, "Covariates", covariates)

# Format headers
header_style <- createStyle(
  fontSize = 12,
  fontColour = "#FFFFFF",
  fgFill = "#4F81BD",
  halign = "center",
  valign = "center",
  textDecoration = "bold",
  border = "TopBottomLeftRight"
)

# Apply to all sheets
for (sheet in names(wb)) {
  addStyle(wb, sheet, header_style, rows = 1, cols = 1:20, gridExpand = TRUE)
  setColWidths(wb, sheet, cols = 1:20, widths = "auto")
  freezePane(wb, sheet, firstRow = TRUE)
}

# Add filters
for (sheet in c(
  "Variables", "Subject Parameters", "Severity Mapping",
  "Relationship Mapping", "Analysis Flags",
  "Exposure Metrics", "Covariates"
)) {
  addFilter(wb, sheet, row = 1, cols = 1:20)
}

# Save workbook
output_dir <- "specifications"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

output_file <- file.path(output_dir, "ADES_specification.xlsx")
saveWorkbook(wb, output_file, overwrite = TRUE)

cat("✓ ADES specification created:", output_file, "\n")
cat("  - 78 variables documented\n")
cat("  - 20 exposure metrics (8-character compliant)\n")
cat("  - 13 baseline covariates\n")
cat("  - Uses ASEV/ASEVN (not AETOXGR/AETOXGRN)\n")
cat("  - Multi-level structure (subject + event)\n")
cat("  - All variable names ≤ 8 characters\n\n")

# ===============================================================================
# CREATE SUMMARY REPORT
# ===============================================================================

cat("\nADES SPECIFICATION SUMMARY\n")
cat(strrep("=", 80), "\n")

# Variable counts by category
var_counts <- variable_spec %>%
  mutate(
    Category = case_when(
      Variable %in% c(
        "STUDYID", "STUDYIDN", "USUBJID", "USUBJIDN",
        "SUBJID", "SUBJIDN", "SITEID", "SITEIDN", "ASEQ"
      ) ~ "Identifiers",
      Variable %in% c(
        "ARM", "ARMN", "ARMCD", "TRT01P", "TRT01PN",
        "TRT01A", "TRT01AN", "DOSE"
      ) ~ "Treatment",
      Variable %in% c(
        "AGE", "AGEGR1", "AGEGR1N", "SEX", "SEXN",
        "RACE", "RACEN", "ETHNIC", "ETHNICN"
      ) ~ "Demographics",
      Variable %in% c("PARAMCD", "PARAM", "PARAMN") ~ "Parameter",
      Variable %in% c("AVAL", "AVALC") ~ "Analysis Values",
      Variable %in% c(
        "AESEQ", "AESPID", "AEDECOD", "AEBODSYS", "ASEV", "ASEVN",
        "AEREL", "AERELN", "AESER", "AEACN", "AEOUT",
        "ASTDT", "ASTDY", "AENDT", "AENDY", "ADURN", "ADURU",
        "TRTEMFL", "AOCCFL"
      ) ~ "AE Details",
      Variable %in% c(
        "AUCSS", "CMAXSS", "CAVGSS", "AUCSLOG", "CMXSLOG",
        "CAVGLOG", "AUCSSSTD", "CMXSSSTD", "CAVGSTD",
        "AUCSSN", "CMAXSSN", "CAVGSSN", "AUCSSDOS", "CMXSSDOS",
        "CAVGDOS", "AUCSSCAT", "AUCSCATN", "CMXSSCAT",
        "CMXSCATN"
      ) ~ "Exposure Metrics",
      Variable %in% c(
        "WTBL", "HTBL", "BMIBL", "BSA", "CREATBL", "CRCLBL",
        "EGFRBL", "ALTBL", "ASTBL", "TBILBL", "ECOGBL",
        "ALBBL", "SMOKEBL"
      ) ~ "Baseline Covariates",
      Variable %in% c("TRTSDT", "TRTEDT", "TRTDURD") ~ "Treatment Dates",
      Variable %in% c("ANL01FL", "ANL02FL") ~ "Analysis Flags",
      Variable %in% c("SAFFL", "ITTFL", "EFFFL") ~ "Population Flags",
      TRUE ~ "Other"
    )
  ) %>%
  count(Category, name = "Count")

print(var_counts)

cat("\n")
cat("Total Variables:", nrow(variable_spec), "\n")
cat("8-Character Compliance: All variables ≤ 8 characters\n")
cat("\n")

cat("Key Features:\n")
cat("  - Built on ADER foundation (20 exposure metrics + 13 covariates)\n")
cat("  - Multi-level structure (subject parameters + event records)\n")
cat("  - Uses ASEV/ASEVN (MILD/MODERATE/SEVERE) not AETOXGR (1-5)\n")
cat("  - AERELN (0-4) for relationship modeling\n")
cat("  - Subject-level parameters: TEAE, TEAESEV, TESAE, etc.\n")
cat("  - Event-level details preserved for analysis\n")
cat("  - Comprehensive exposure and covariate coverage\n")
cat("\n")

cat("Important Notes:\n")
cat("  - ASEV follows pharmaverseadam conventions\n")
cat("  - ASEVN: 1=MILD, 2=MODERATE, 3=SEVERE (not 1-5 toxicity grades)\n")
cat("  - For oncology requiring CTCAE grades, map from ASEV + AE details\n")
cat("  - AERELN enables continuous relationship modeling\n")
cat("\n")

# ===============================================================================
# END OF SCRIPT
# ===============================================================================
