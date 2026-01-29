#===============================================================================
# Script: create_adtrr_spec_excel.R
#
# Purpose: Create Excel-based ADaM specification for ADTRR dataset
#          Tumor Response for Exposure-Response Analysis Dataset
#
# Author: Jeff Dickinson
# Date: 2026-01-28
#
# Output: specifications/ADTRR_specification.xlsx
#
# Note: Variable names comply with 8-character CDISC limit
#       NADPCHG (not NADIR_PCHG), BORN (numeric BOR)
#===============================================================================

library(dplyr)
library(openxlsx)

#===============================================================================
# SPECIFICATION DATA
#===============================================================================

# Dataset metadata
dataset_spec <- tibble(
  Field = c("Dataset", "Description", "Class", "Structure", "Purpose", "Key Variables"),
  Value = c(
    "ADTRR",
    "Analysis Dataset for Tumor Response for E-R Analysis",
    "BASIC DATA STRUCTURE (BDS)",
    "One record per subject per parameter per visit",
    "Longitudinal tumor measurements and RECIST 1.1 response with comprehensive exposure metrics",
    "PARAMCD, AVISIT, AVAL, BASE, CHG, PCHG, AVALC, NADIR, BOR, AUCSS"
  )
)

# Variable specifications (86 variables)
variable_spec <- tibble(
  Order = 1:86,
  
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
    
    # Parameter (3)
    "PARAMCD", "PARAM", "PARAMN",
    
    # Visit (2)
    "AVISITN", "AVISIT",
    
    # Analysis Values (10)
    "AVAL", "AVALC", "AVALN", "AVALU",
    "BASE", "CHG", "PCHG",
    "NADIR", "NADPCHG",
    "BORN",
    
    # Dates (3)
    "ADT", "ADY", "ADTC",
    
    # Baseline Flag (1)
    "ABLFL",
    
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
    
    # Treatment Dates (3)
    "TRTSDT", "TRTEDT", "TRTDURD",
    
    # Analysis Flags (3)
    "ANL01FL", "ANL02FL", "ANL03FL",
    
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
    
    # Visit
    "Analysis Visit (N)",
    "Analysis Visit",
    
    # Analysis Values
    "Analysis Value",
    "Analysis Value (C)",
    "Analysis Value (N)",
    "Analysis Value Unit",
    "Baseline Value",
    "Change from Baseline",
    "Percent Change from Baseline",
    "Nadir Value",
    "Percent Change from Nadir",
    "Best Overall Response (N)",
    
    # Dates
    "Analysis Date",
    "Analysis Relative Day",
    "Analysis Date (C)",
    
    # Baseline Flag
    "Baseline Record Flag",
    
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
    "Analysis Flag 02 (Post-Baseline Records)",
    "Analysis Flag 03 (Confirmed Response)",
    
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
    
    # Visit
    "integer", "text",
    
    # Analysis Values
    "float", "text", "integer", "text",
    "float", "float", "float",
    "float", "float",
    "integer",
    
    # Dates
    "date", "integer", "text",
    
    # Baseline Flag
    "text",
    
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
    "text", "text", "text",
    
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
    
    # Visit
    NA, 200,
    
    # Analysis Values
    NA, 200, NA, 20,
    NA, NA, NA,
    NA, NA,
    NA,
    
    # Dates
    NA, NA, 200,
    
    # Baseline Flag
    1,
    
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
    1, 1, 1,
    
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
    
    # Visit
    "TR", "TR",
    
    # Analysis Values
    "TR", "Derived", "Derived", "Derived",
    "Derived", "Derived", "Derived",
    "Derived", "Derived",
    "Derived",
    
    # Dates
    "TR", "Derived", "Derived",
    
    # Baseline Flag
    "Derived",
    
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
    "Derived", "Derived", "Derived",
    
    # Population Flags
    "ADSL", "ADSL", "ADSL"
  ),
  
  Derivation = c(
    # Identifiers
    "From ADSL", "From ADSL", "From ADSL", "From ADSL", "From ADSL", "From ADSL",
    "From ADSL", "From ADSL", "Sequence number within subject-parameter",
    
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
    "TSIZE, BOR, NADIR", "Parameter description", "1=TSIZE, 2=BOR, 3=NADIR",
    
    # Visit
    "Visit number (1=Baseline, 99=Overall)", "Visit name",
    
    # Analysis Values
    "Tumor size (mm) or response value", "RECIST category (CR/PR/SD/PD)", 
    "Numeric response (4=CR, 3=PR, 2=SD, 1=PD)", "mm or blank",
    "Baseline tumor size", "AVAL - BASE", "100 * (AVAL - BASE) / BASE",
    "Minimum tumor size from baseline forward", "100 * (AVAL - NADIR) / NADIR",
    "Best response numeric (4=CR, 3=PR, 2=SD, 1=PD)",
    
    # Dates
    "Assessment date", "ADT - TRTSDT + 1", "Character version of ADT",
    
    # Baseline Flag
    "Y if AVISITN = 1",
    
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
    "Y if !is.na(AVAL) & !is.na(AUCSS)", 
    "Y if ABLFL != 'Y' (post-baseline only)", 
    "Y if confirmed response (same response ≥4 weeks apart)",
    
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
    "TSIZE=longitudinal, BOR=best response, NADIR=minimum",
    "Full parameter description",
    "For sorting",
    
    # Visit
    "1=Baseline, 2-9=scheduled, 99=Overall",
    "BASELINE, WEEK 6, WEEK 12, etc., OVERALL",
    
    # Analysis Values
    "Tumor size in mm (TSIZE parameter)",
    "RECIST 1.1 category (CR/PR/SD/PD/BASELINE)",
    "Numeric version for BOR (4/3/2/1)",
    "mm for TSIZE/NADIR, blank for BOR",
    "Baseline tumor size (TSIZE only)",
    "Change from baseline (TSIZE only)",
    "Percent change from baseline (TSIZE only)",
    "Minimum tumor size so far (for PD assessment)",
    "Percent change from nadir (8-char compliant)",
    "Numeric BOR (4=CR, 3=PR, 2=SD, 1=PD)",
    
    # Dates
    "Date of tumor assessment",
    "Study day of assessment",
    "ISO 8601 format",
    
    # Baseline Flag
    "Identifies baseline record",
    
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
    "Primary analysis (non-missing tumor and exposure)",
    "Post-baseline records only",
    "Confirmed response (for response duration)",
    
    # Population Flags
    "All treated subjects",
    "All randomized subjects",
    "Efficacy-evaluable subjects"
  )
)

#===============================================================================
# ADDITIONAL TABLES
#===============================================================================

# Parameters table
parameters <- tibble(
  PARAMCD = c("TSIZE", "BOR", "NADIR"),
  PARAM = c(
    "Target Lesion Size",
    "Best Overall Response",
    "Nadir Tumor Size"
  ),
  PARAMN = 1:3,
  Structure = c(
    "Longitudinal (one record per visit)",
    "Summary (one record per subject, AVISITN=99)",
    "Summary (one record per subject, AVISITN=99)"
  ),
  Definition = c(
    "Sum of diameters of target lesions at each assessment",
    "Best response achieved across all post-baseline assessments",
    "Minimum tumor size achieved from baseline forward"
  ),
  Primary_Use = c(
    "Spider plots, longitudinal modeling, percent change",
    "Response rate analysis, waterfall plots",
    "Progressive disease assessment, depth of response"
  )
)

# RECIST 1.1 criteria
recist_criteria <- tibble(
  Response = c("CR", "PR", "SD", "PD"),
  AVALC = c("CR", "PR", "SD", "PD"),
  AVALN = c(4, 3, 2, 1),
  Definition = c(
    "Complete Response: Disappearance of all target lesions",
    "Partial Response: ≥30% decrease in sum of diameters (baseline reference)",
    "Stable Disease: Neither PR nor PD criteria met",
    "Progressive Disease: ≥20% increase in sum AND ≥5mm absolute increase (nadir reference)"
  ),
  Criteria_Formula = c(
    "AVAL = 0",
    "PCHG ≤ -30",
    "PCHG > -30 AND (NADPCHG < 20 OR CHG < 5)",
    "NADPCHG ≥ 20 AND (AVAL - NADIR) ≥ 5"
  ),
  Confirmation_Required = c(
    "Yes (≥4 weeks)",
    "Yes (≥4 weeks)",
    "No",
    "No"
  )
)

# BOR derivation rules
bor_rules <- tibble(
  Rule = c(1, 2, 3, 4, 5),
  Logic = c(
    "If any CR at any visit → BOR = CR",
    "Else if any PR at any visit → BOR = PR",
    "Else if any PD at any visit → BOR = PD",
    "Else if all visits are SD → BOR = SD",
    "Baseline only (no post-baseline) → BOR = NE (Not Evaluable)"
  ),
  BORN = c(4, 3, 1, 2, NA),
  Notes = c(
    "CR takes precedence over all other responses",
    "PR requires no PD and no CR",
    "PD if never achieved PR/CR",
    "SD if never achieved PR/CR/PD",
    "Requires at least one post-baseline assessment"
  )
)

# Visit schedule
visit_schedule <- tibble(
  AVISITN = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 99),
  AVISIT = c(
    "BASELINE",
    "WEEK 6",
    "WEEK 12",
    "WEEK 18",
    "WEEK 24",
    "WEEK 30",
    "WEEK 36",
    "WEEK 42",
    "WEEK 48",
    "OVERALL"
  ),
  Planned_Day = c(1, 43, 85, 127, 169, 211, 253, 295, 337, NA),
  Window_Start = c(NA, 36, 78, 120, 162, 204, 246, 288, 330, NA),
  Window_End = c(NA, 50, 92, 134, 176, 218, 260, 302, 344, NA),
  Purpose = c(
    "Baseline assessment (pre-treatment)",
    "First on-treatment assessment",
    "Second assessment",
    "Third assessment",
    "Fourth assessment",
    "Fifth assessment",
    "Sixth assessment",
    "Seventh assessment",
    "Final assessment",
    "Best overall response (derived)"
  )
)

# Analysis flags description
analysis_flags <- tibble(
  Flag = c("ANL01FL", "ANL02FL", "ANL03FL"),
  Description = c(
    "Primary Analysis Population",
    "Post-Baseline Records",
    "Confirmed Response"
  ),
  Criteria = c(
    "Non-missing AVAL and non-missing AUCSS",
    "ABLFL != 'Y' (excludes baseline)",
    "Response confirmed at consecutive visit ≥4 weeks later"
  ),
  Usage = c(
    "All E-R tumor analyses",
    "Response rate, waterfall plots (exclude baseline)",
    "Duration of response, time to response analyses"
  )
)

# 8-character compliance notes
char8_compliance <- tibble(
  Original_Concept = c(
    "NADIR_PCHG",
    "NADIR_VISIT",
    "BOR_NUMERIC",
    "All exposure variables"
  ),
  CDISC_Name = c(
    "NADPCHG",
    "NADVST (if needed)",
    "BORN",
    "See exposure table"
  ),
  Abbreviation_Rule = c(
    "Removed 'IR' from NADIR",
    "Abbreviated VISIT to VST",
    "Added N suffix for numeric",
    "Systematic abbreviations"
  ),
  Label = c(
    "Percent Change from Nadir",
    "Visit of Nadir",
    "Best Overall Response (N)",
    "Various (see labels)"
  ),
  Notes = c(
    "8-char compliant",
    "Optional variable",
    "4=CR, 3=PR, 2=SD, 1=PD",
    "All ≤8 characters"
  )
)

# Exposure metrics (same as ADEE/ADES)
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

# Covariates (same as ADEE/ADES)
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

#===============================================================================
# CREATE EXCEL WORKBOOK
#===============================================================================

cat("Creating ADTRR specification Excel file...\n")

# Create workbook
wb <- createWorkbook()

# Add worksheets
addWorksheet(wb, "Dataset")
addWorksheet(wb, "Variables")
addWorksheet(wb, "Parameters")
addWorksheet(wb, "RECIST Criteria")
addWorksheet(wb, "BOR Rules")
addWorksheet(wb, "Visit Schedule")
addWorksheet(wb, "Analysis Flags")
addWorksheet(wb, "8-Char Compliance")
addWorksheet(wb, "Exposure Metrics")
addWorksheet(wb, "Covariates")

# Write data
writeData(wb, "Dataset", dataset_spec)
writeData(wb, "Variables", variable_spec)
writeData(wb, "Parameters", parameters)
writeData(wb, "RECIST Criteria", recist_criteria)
writeData(wb, "BOR Rules", bor_rules)
writeData(wb, "Visit Schedule", visit_schedule)
writeData(wb, "Analysis Flags", analysis_flags)
writeData(wb, "8-Char Compliance", char8_compliance)
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
for (sheet in c("Variables", "Parameters", "RECIST Criteria", "BOR Rules",
                "Visit Schedule", "Analysis Flags", "8-Char Compliance",
                "Exposure Metrics", "Covariates")) {
  addFilter(wb, sheet, row = 1, cols = 1:20)
}

# Save workbook
output_dir <- "specifications"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

output_file <- file.path(output_dir, "ADTRR_specification.xlsx")
saveWorkbook(wb, output_file, overwrite = TRUE)

cat("✓ ADTRR specification created:", output_file, "\n")
cat("  - 80 variables documented\n")
cat("  - 20 exposure metrics (8-character compliant)\n")
cat("  - 13 baseline covariates\n")
cat("  - 3 parameters: TSIZE, BOR, NADIR\n")
cat("  - RECIST 1.1 criteria documented\n")
cat("  - NADPCHG, BORN (8-char compliant)\n")
cat("  - All variable names ≤ 8 characters\n\n")

#===============================================================================
# CREATE SUMMARY REPORT
#===============================================================================

cat("\nADTRR SPECIFICATION SUMMARY\n")
cat(strrep("=", 80), "\n")

# Variable counts by category
var_counts <- variable_spec %>%
  mutate(
    Category = case_when(
      Variable %in% c("STUDYID", "STUDYIDN", "USUBJID", "USUBJIDN", 
                      "SUBJID", "SUBJIDN", "SITEID", "SITEIDN", "ASEQ") ~ "Identifiers",
      Variable %in% c("ARM", "ARMN", "ARMCD", "TRT01P", "TRT01PN", 
                      "TRT01A", "TRT01AN", "DOSE") ~ "Treatment",
      Variable %in% c("AGE", "AGEGR1", "AGEGR1N", "SEX", "SEXN", 
                      "RACE", "RACEN", "ETHNIC", "ETHNICN") ~ "Demographics",
      Variable %in% c("PARAMCD", "PARAM", "PARAMN") ~ "Parameter",
      Variable %in% c("AVISITN", "AVISIT") ~ "Visit",
      Variable %in% c("AVAL", "AVALC", "AVALN", "AVALU", "BASE", "CHG", "PCHG",
                      "NADIR", "NADPCHG", "BORN") ~ "Analysis Values",
      Variable %in% c("ADT", "ADY", "ADTC") ~ "Dates",
      Variable == "ABLFL" ~ "Baseline Flag",
      Variable %in% c("AUCSS", "CMAXSS", "CAVGSS", "AUCSLOG", "CMXSLOG", 
                      "CAVGLOG", "AUCSSSTD", "CMXSSSTD", "CAVGSTD",
                      "AUCSSN", "CMAXSSN", "CAVGSSN", "AUCSSDOS", "CMXSSDOS",
                      "CAVGDOS", "AUCSSCAT", "AUCSCATN", "CMXSSCAT", 
                      "CMXSCATN") ~ "Exposure Metrics",
      Variable %in% c("WTBL", "HTBL", "BMIBL", "BSA", "CREATBL", "CRCLBL",
                      "EGFRBL", "ALTBL", "ASTBL", "TBILBL", "ECOGBL", 
                      "ALBBL", "SMOKEBL") ~ "Baseline Covariates",
      Variable %in% c("TRTSDT", "TRTEDT", "TRTDURD") ~ "Treatment Dates",
      Variable %in% c("ANL01FL", "ANL02FL", "ANL03FL") ~ "Analysis Flags",
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
cat("  - Longitudinal structure (one record per subject-parameter-visit)\n")
cat("  - Three parameters: TSIZE (longitudinal), BOR (summary), NADIR (summary)\n")
cat("  - RECIST 1.1 compliant response assessment\n")
cat("  - Comprehensive change variables (BASE, CHG, PCHG, NADIR, NADPCHG)\n")
cat("  - BORN provides numeric BOR (4=CR, 3=PR, 2=SD, 1=PD)\n")
cat("  - 9 scheduled visits plus baseline\n")
cat("\n")

cat("Important 8-Character Notes:\n")
cat("  - NADPCHG: Percent change from nadir (8-char: removed 'IR')\n")
cat("  - BORN: Best overall response numeric (8-char: added 'N')\n")
cat("  - CMXSLOG: Log Cmax (8-char: abbreviated CMAX to CMX)\n")
cat("  - AUCSLOG: Log AUC (8-char: removed one 'S')\n")
cat("\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================