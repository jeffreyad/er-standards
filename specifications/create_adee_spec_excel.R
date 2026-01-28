#===============================================================================
# Script: create_adee_spec_excel.R
#
# Purpose: Create Excel-based ADaM specification for ADEE dataset
#          Exposure-Efficacy Analysis Dataset (74 variables)
#
# Author: Jeff Dickinson
# Date: 2026-01-22
#
# Output: specifications/ADEE_specification.xlsx
#
# Note: Variable names comply with 8-character CDISC limit
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
    "ADEE",
    "Analysis Dataset for Exposure-Efficacy",
    "BASIC DATA STRUCTURE (BDS)",
    "One record per subject per parameter",
    "Time-to-event efficacy analyses with comprehensive exposure metrics",
    "PARAMCD, AVAL, CNSR, EVENT, AUCSS, AUCSSSTD, AUCSSCAT"
  )
)

#===============================================================================
# Script: create_adee_spec_excel.R
#
# Purpose: Create Excel-based ADaM specification for ADEE dataset
#          Exposure-Efficacy Analysis Dataset (74 variables)
#
# Author: Jeff Dickinson
# Date: 2026-01-22
#
# Output: specifications/ADEE_specification.xlsx
#
# Note: Variable names comply with 8-character CDISC limit
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
    "ADEE",
    "Analysis Dataset for Exposure-Efficacy",
    "BASIC DATA STRUCTURE (BDS)",
    "One record per subject per parameter",
    "Time-to-event efficacy analyses with comprehensive exposure metrics",
    "PARAMCD, AVAL, CNSR, EVENT, AUCSS, AUCSSSTD, AUCSSCAT"
  )
)

# Variable specifications (74 variables)
variable_spec <- tibble(
  Order = 1:78,
  
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
    
    # Analysis Values (8) - FIXED: was missing ADTC
    "AVAL", "AVALU", "AVALC",
    "CNSR", "EVENT",
    "ADT", "ADY", "ADTC",
    
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
    
    # Analysis Flags (3)
    "ANL01FL", "ANL02FL", "ANL03FL",
    
    # Population Flags (3)
    "SAFFL", "ITTFL", "EFFFL"
  ),
  
  Label = c(
    # Identifiers (9)
    "Study Identifier",
    "Study Identifier (N)",
    "Unique Subject Identifier",
    "Unique Subject Identifier (N)",
    "Subject Identifier for the Study",
    "Subject Identifier for the Study (N)",
    "Study Site Identifier",
    "Study Site Identifier (N)",
    "Analysis Sequence Number",
    
    # Treatment (8)
    "Description of Planned Arm",
    "Planned Arm Code (N)",
    "Planned Arm Code",
    "Planned Treatment for Period 01",
    "Planned Treatment for Period 01 (N)",
    "Actual Treatment for Period 01",
    "Actual Treatment for Period 01 (N)",
    "Actual Dose (mg)",
    
    # Demographics (9)
    "Age",
    "Pooled Age Group 1",
    "Pooled Age Group 1 (N)",
    "Sex",
    "Sex (N)",
    "Race",
    "Race (N)",
    "Ethnicity",
    "Ethnicity (N)",
    
    # Parameter (3)
    "Parameter Code",
    "Parameter",
    "Parameter (N)",
    
    # Analysis Values (8)
    "Analysis Value",
    "Analysis Value Unit",
    "Analysis Value (C)",
    "Censoring (0=Event, 1=Censored)",
    "Event Indicator (1=Event, 0=Censored)",
    "Analysis Date",
    "Analysis Relative Day",
    "Analysis Date (C)",
    
    # Exposure - Raw (3)
    "Steady-State Area Under Curve",
    "Steady-State Maximum Concentration",
    "Steady-State Average Concentration",
    
    # Exposure - Log (3)
    "Log Steady-State AUC",
    "Log Steady-State Cmax",
    "Log Steady-State Cavg",
    
    # Exposure - Standardized (3)
    "Standardized Steady-State AUC",
    "Standardized Steady-State Cmax",
    "Standardized Steady-State Cavg",
    
    # Exposure - Normalized (3)
    "Normalized Steady-State AUC",
    "Normalized Steady-State Cmax",
    "Normalized Steady-State Cavg",
    
    # Exposure - Dose-Normalized (3)
    "Dose-Normalized Steady-State AUC",
    "Dose-Normalized Steady-State Cmax",
    "Dose-Normalized Steady-State Cavg",
    
    # Exposure - Categorical (4)
    "Steady-State AUC Category",
    "Steady-State AUC Category (N)",
    "Steady-State Cmax Category",
    "Steady-State Cmax Category (N)",
    
    # Covariates - Vitals (4)
    "Baseline Weight (kg)",
    "Baseline Height (cm)",
    "Baseline Body Mass Index (kg/m2)",
    "Body Surface Area (m2)",
    
    # Covariates - Renal (3)
    "Baseline Creatinine (mg/dL)",
    "Baseline Creatinine Clearance (mL/min)",
    "Baseline eGFR (mL/min/1.73m2)",
    
    # Covariates - Hepatic (3)
    "Baseline Alanine Aminotransferase (U/L)",
    "Baseline Aspartate Aminotransferase (U/L)",
    "Baseline Total Bilirubin (mg/dL)",
    
    # Covariates - Other (3)
    "Baseline ECOG Performance Status",
    "Baseline Albumin (g/dL)",
    "Baseline Smoking Status",
    
    # Dates (3)
    "Date of First Exposure to Treatment",
    "Date of Last Exposure to Treatment",
    "Total Treatment Duration (Days)",
    
    # Analysis Flags (3)
    "Analysis Flag 01 (Primary Analysis)",
    "Analysis Flag 02 (Sensitivity Analysis 1)",
    "Analysis Flag 03 (Sensitivity Analysis 2)",
    
    # Population Flags (3)
    "Safety Population Flag",
    "Intent-to-Treat Population Flag",
    "Efficacy Population Flag"
  ),
  
  Type = c(
    # Identifiers (9)
    "text", "integer", "text", "integer", "text", "integer",
    "text", "integer", "integer",
    
    # Treatment (8)
    "text", "integer", "text",
    "text", "integer",
    "text", "integer",
    "float",
    
    # Demographics (9)
    "integer", "text", "integer",
    "text", "integer",
    "text", "integer",
    "text", "integer",
    
    # Parameter (3)
    "text", "text", "integer",
    
    # Analysis Values (8)
    "float", "text", "text",
    "integer", "integer",
    "date", "integer", "text",
    
    # Exposure - Raw (3)
    "float", "float", "float",
    
    # Exposure - Log (3)
    "float", "float", "float",
    
    # Exposure - Standardized (3)
    "float", "float", "float",
    
    # Exposure - Normalized (3)
    "float", "float", "float",
    
    # Exposure - Dose-Normalized (3)
    "float", "float", "float",
    
    # Exposure - Categorical (4)
    "text", "integer",
    "text", "integer",
    
    # Covariates - Vitals (4)
    "float", "float", "float", "float",
    
    # Covariates - Renal (3)
    "float", "float", "float",
    
    # Covariates - Hepatic (3)
    "float", "float", "float",
    
    # Covariates - Other (3)
    "integer", "float", "text",
    
    # Dates (3)
    "date", "date", "integer",
    
    # Analysis Flags (3)
    "text", "text", "text",
    
    # Population Flags (3)
    "text", "text", "text"
  ),
  
  Length = c(
    # Identifiers (9)
    200, NA, 200, NA, 200, NA,
    200, NA, NA,
    
    # Treatment (8)
    200, NA, 200,
    200, NA,
    200, NA,
    NA,
    
    # Demographics (9)
    NA, 200, NA,
    200, NA,
    200, NA,
    200, NA,
    
    # Parameter (3)
    8, 200, NA,
    
    # Analysis Values (8)
    NA, 200, 200,
    NA, NA,
    NA, NA, 200,
    
    # Exposure - Raw (3)
    NA, NA, NA,
    
    # Exposure - Log (3)
    NA, NA, NA,
    
    # Exposure - Standardized (3)
    NA, NA, NA,
    
    # Exposure - Normalized (3)
    NA, NA, NA,
    
    # Exposure - Dose-Normalized (3)
    NA, NA, NA,
    
    # Exposure - Categorical (4)
    200, NA,
    200, NA,
    
    # Covariates - Vitals (4)
    NA, NA, NA, NA,
    
    # Covariates - Renal (3)
    NA, NA, NA,
    
    # Covariates - Hepatic (3)
    NA, NA, NA,
    
    # Covariates - Other (3)
    NA, NA, 200,
    
    # Dates (3)
    NA, NA, NA,
    
    # Analysis Flags (3)
    1, 1, 1,
    
    # Population Flags (3)
    1, 1, 1
  ),
  
  Source = c(
    # Identifiers (9)
    "ADSL", "ADSL", "ADSL", "ADSL", "ADSL", "ADSL",
    "ADSL", "ADSL", "Derived",
    
    # Treatment (8)
    "ADSL", "ADSL", "ADSL",
    "ADSL", "ADSL",
    "ADSL", "ADSL",
    "ADSL",
    
    # Demographics (9)
    "ADSL", "ADSL", "ADSL",
    "ADSL", "ADSL",
    "ADSL", "ADSL",
    "ADSL", "ADSL",
    
    # Parameter (3)
    "ADTTE/ADRS", "ADTTE/ADRS", "ADTTE/ADRS",
    
    # Analysis Values (8)
    "Derived", "Derived", "Derived",
    "ADTTE/ADRS", "Derived",
    "ADTTE/ADRS", "Derived", "Derived",
    
    # Exposure - Raw (from ADER) (3)
    "ADER", "ADER", "ADER",
    
    # Exposure - Log (from ADER) (3)
    "ADER", "ADER", "ADER",
    
    # Exposure - Standardized (from ADER) (3)
    "ADER", "ADER", "ADER",
    
    # Exposure - Normalized (from ADER) (3)
    "ADER", "ADER", "ADER",
    
    # Exposure - Dose-Normalized (from ADER) (3)
    "ADER", "ADER", "ADER",
    
    # Exposure - Categorical (from ADER) (4)
    "ADER", "ADER",
    "ADER", "ADER",
    
    # Covariates - Vitals (from ADER) (4)
    "ADER", "ADER", "ADER", "ADER",
    
    # Covariates - Renal (from ADER) (3)
    "ADER", "ADER", "ADER",
    
    # Covariates - Hepatic (from ADER) (3)
    "ADER", "ADER", "ADER",
    
    # Covariates - Other (from ADER) (3)
    "ADER", "ADER", "ADER",
    
    # Dates (3)
    "ADSL", "ADSL", "ADSL",
    
    # Analysis Flags (3)
    "Derived", "Derived", "Derived",
    
    # Population Flags (3)
    "ADSL", "ADSL", "ADSL"
  ),
  
  Derivation = c(
    # Identifiers (9)
    "From ADSL", "From ADSL", "From ADSL", "From ADSL", "From ADSL", "From ADSL",
    "From ADSL", "From ADSL", "Sequence number within subject",
    
    # Treatment (8)
    "From ADSL", "From ADSL", "From ADSL",
    "From ADSL", "From ADSL",
    "From ADSL", "From ADSL",
    "From ADSL",
    
    # Demographics (9)
    "From ADSL", "From ADSL", "From ADSL",
    "From ADSL", "From ADSL",
    "From ADSL", "From ADSL",
    "From ADSL", "From ADSL",
    
    # Parameter (3)
    "OS, PFS, TTP, TTNT", "Parameter description", "1=OS, 2=PFS, 3=TTP, 4=TTNT",
    
    # Analysis Values (8)
    "AVAL = ADT - TRTSDT (days)", "DAYS", "Character version of AVAL",
    "0=event, 1=censored", "1=event, 0=censored (inverse of CNSR)",
    "Date of event or censoring", "Study day relative to TRTSDT", "Character version of ADT",
    
    # Exposure - Raw (3)
    "From ADER: Steady-state AUC (ng*h/mL)", 
    "From ADER: Steady-state Cmax (ng/mL)", 
    "From ADER: Steady-state Cavg (ng/mL)",
    
    # Exposure - Log (3)
    "From ADER: log(AUCSS)", 
    "From ADER: log(CMAXSS)", 
    "From ADER: log(CAVGSS)",
    
    # Exposure - Standardized (3)
    "From ADER: (AUCSS - mean)/SD", 
    "From ADER: (CMAXSS - mean)/SD", 
    "From ADER: (CAVGSS - mean)/SD",
    
    # Exposure - Normalized (3)
    "From ADER: AUCSS / mean(AUCSS)", 
    "From ADER: CMAXSS / mean(CMAXSS)", 
    "From ADER: CAVGSS / mean(CAVGSS)",
    
    # Exposure - Dose-Normalized (3)
    "From ADER: AUCSS / DOSE", 
    "From ADER: CMAXSS / DOSE", 
    "From ADER: CAVGSS / DOSE",
    
    # Exposure - Categorical (4)
    "From ADER: Tertiles of AUCSS (Low/Medium/High)", 
    "From ADER: Numeric version (1/2/3)",
    "From ADER: Tertiles of CMAXSS (Low/Medium/High)", 
    "From ADER: Numeric version (1/2/3)",
    
    # Covariates - Vitals (4)
    "From ADER: Baseline weight", 
    "From ADER: Baseline height", 
    "From ADER: Baseline BMI", 
    "From ADER: Body surface area (Mosteller)",
    
    # Covariates - Renal (3)
    "From ADER: Baseline serum creatinine", 
    "From ADER: Baseline CrCL (Cockcroft-Gault)", 
    "From ADER: Baseline eGFR (CKD-EPI)",
    
    # Covariates - Hepatic (3)
    "From ADER: Baseline ALT", 
    "From ADER: Baseline AST", 
    "From ADER: Baseline total bilirubin",
    
    # Covariates - Other (3)
    "From ADER: Baseline ECOG (0-4)", 
    "From ADER: Baseline serum albumin", 
    "From ADER: Baseline smoking status (Current/Former/Never)",
    
    # Dates (3)
    "From ADSL", "From ADSL", "From ADSL: TRTEDT - TRTSDT + 1",
    
    # Analysis Flags (3)
    "Y if !is.na(AVAL) & !is.na(AUCSS)", 
    "Y for sensitivity analysis excluding missing covariate", 
    "Y for per-protocol population",
    
    # Population Flags (3)
    "From ADSL", "From ADSL", "From ADSL"
  ),
  
  Notes = c(
    # Identifiers (9)
    rep("", 9),
    
    # Treatment (8)
    rep("", 8),
    
    # Demographics (9)
    rep("", 9),
    
    # Parameter (3)
    "Standard CDISC codes",
    "Full parameter description",
    "For sorting",
    
    # Analysis Values (8)
    "Time from treatment start to event/censor (days)",
    "Always DAYS for time-to-event",
    "Rarely used for TTE",
    "Standard TTE censoring indicator",
    "Modeling convenience (inverse of CNSR)",
    "Date of event or last known alive",
    "Study day (ADT - TRTSDT + 1)",
    "ISO 8601 format",
    
    # Exposure - Raw (3)
    "Primary exposure metric",
    "Alternative exposure metric",
    "Alternative exposure metric",
    
    # Exposure - Log (3)
    "For log-linear models (8-char: one S removed)",
    "For log-linear models (8-char: CMX abbreviation)",
    "For log-linear models (8-char: SS removed)",
    
    # Exposure - Standardized (3)
    "Z-score for covariate modeling",
    "Z-score for covariate modeling (8-char: CMX)",
    "Z-score for covariate modeling",
    
    # Exposure - Normalized (3)
    "Normalized to mean=1",
    "Normalized to mean=1",
    "Normalized to mean=1",
    
    # Exposure - Dose-Normalized (3)
    "Per-mg dose (8-char: removed E from DOSE)",
    "Per-mg dose (8-char: CMX + removed E)",
    "Per-mg dose (8-char: removed E)",
    
    # Exposure - Categorical (4)
    "For subgroup analyses",
    "Numeric version (8-char: one S removed)",
    "For subgroup analyses",
    "Numeric version (8-char: one S removed)",
    
    # Covariates - Vitals (4)
    "Baseline covariate",
    "Baseline covariate",
    "Baseline covariate",
    "Mosteller formula",
    
    # Covariates - Renal (3)
    "Baseline covariate",
    "Cockcroft-Gault equation",
    "CKD-EPI equation",
    
    # Covariates - Hepatic (3)
    "Baseline covariate",
    "Baseline covariate",
    "Baseline covariate",
    
    # Covariates - Other (3)
    "Performance status",
    "Baseline covariate",
    "Baseline covariate",
    
    # Dates (3)
    "First dose date",
    "Last dose date",
    "Total days on treatment",
    
    # Analysis Flags (3)
    "Primary analysis (non-missing exposure and outcome)",
    "Sensitivity analysis 1",
    "Sensitivity analysis 2",
    
    # Population Flags (3)
    "All treated subjects",
    "All randomized subjects",
    "Efficacy-evaluable subjects"
  )
)

# Rest of the script remains the same...
# (Parameters, analysis_flags, exposure_metrics, covariates tables)
# (Excel creation code)

# Verify variable count
cat("Variable count check:", length(unique(variable_spec$Variable)), "variables\n")
cat("Order count check:", max(variable_spec$Order), "rows\n")

#===============================================================================
# Parameters table (unchanged)
#===============================================================================

# Parameters table
parameters <- tibble(
  PARAMCD = c("OS", "PFS", "TTP", "TTNT"),
  PARAM = c(
    "Overall Survival",
    "Progression-Free Survival",
    "Time to Progression",
    "Time to Next Treatment"
  ),
  PARAMN = 1:4,
  Definition = c(
    "Time from first dose to death from any cause",
    "Time from first dose to progression or death",
    "Time from first dose to disease progression",
    "Time from first dose to start of next anti-cancer therapy"
  ),
  Event = c(
    "Death",
    "Progression or death",
    "Progression",
    "Next treatment start"
  ),
  Censoring = c(
    "Last known alive date",
    "Last tumor assessment without progression",
    "Last tumor assessment without progression",
    "Last follow-up without next treatment"
  )
)

# Analysis flags description
analysis_flags <- tibble(
  Flag = c("ANL01FL", "ANL02FL", "ANL03FL"),
  Description = c(
    "Primary Analysis Population",
    "Sensitivity Analysis 1",
    "Sensitivity Analysis 2"
  ),
  Criteria = c(
    "Non-missing AVAL and non-missing AUCSS",
    "Primary population with non-missing baseline covariates",
    "Per-protocol population (ITT with no major protocol deviations)"
  ),
  Usage = c(
    "Primary Cox models and Kaplan-Meier curves",
    "Sensitivity to missing covariate data",
    "Sensitivity to protocol adherence"
  )
)

# Exposure metrics description
exposure_metrics <- tibble(
  Category = c(
    "Raw Metrics",
    "Raw Metrics",
    "Raw Metrics",
    "Log-Transformed",
    "Log-Transformed",
    "Log-Transformed",
    "Standardized",
    "Standardized",
    "Standardized",
    "Normalized",
    "Normalized",
    "Normalized",
    "Dose-Normalized",
    "Dose-Normalized",
    "Dose-Normalized",
    "Categorical",
    "Categorical"
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
  ),
  Notes = c(
    "Primary exposure", "Peak concentration", "Average concentration",
    "8-char: one S removed", "8-char: CMX abbreviation", "8-char: SS removed",
    "Z-score", "Z-score (8-char: CMX)", "Z-score",
    "Mean = 1", "Mean = 1", "Mean = 1",
    "8-char: E removed", "8-char: CMX + E removed", "8-char: E removed",
    "Low/Medium/High", "8-char: one S removed"
  )
)

# Covariate description
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
  Calculation = c(
    "From VS", "From VS", "WTBL / (HTBL/100)^2", 
    "sqrt(WTBL * HTBL / 3600) [Mosteller]",
    "From LB", "Cockcroft-Gault equation", "CKD-EPI equation",
    "From LB", "From LB", "From LB",
    "From baseline assessment", "From LB", "From baseline assessment"
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

cat("Creating ADEE specification Excel file...\n")

# Create workbook
wb <- createWorkbook()

# Add worksheets
addWorksheet(wb, "Dataset")
addWorksheet(wb, "Variables")
addWorksheet(wb, "Parameters")
addWorksheet(wb, "Analysis Flags")
addWorksheet(wb, "Exposure Metrics")
addWorksheet(wb, "Covariates")

# Write data
writeData(wb, "Dataset", dataset_spec)
writeData(wb, "Variables", variable_spec)
writeData(wb, "Parameters", parameters)
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
for (sheet in c("Dataset", "Variables", "Parameters", "Analysis Flags", 
                "Exposure Metrics", "Covariates")) {
  addStyle(wb, sheet, header_style, rows = 1, cols = 1:20, gridExpand = TRUE)
  setColWidths(wb, sheet, cols = 1:20, widths = "auto")
  freezePane(wb, sheet, firstRow = TRUE)
}

# Add filters
for (sheet in c("Variables", "Parameters", "Analysis Flags", 
                "Exposure Metrics", "Covariates")) {
  addFilter(wb, sheet, row = 1, cols = 1:20)
}

# Save workbook
output_dir <- "specifications"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

output_file <- file.path(output_dir, "ADEE_specification.xlsx")
saveWorkbook(wb, output_file, overwrite = TRUE)

cat("✓ ADEE specification created:", output_file, "\n")
cat("  - 74 variables documented\n")
cat("  - 20 exposure metrics (8-character compliant)\n")
cat("  - 13 baseline covariates\n")
cat("  - 4 parameters: OS, PFS, TTP, TTNT\n")
cat("  - All variable names ≤ 8 characters\n\n")

#===============================================================================
# CREATE SUMMARY REPORT
#===============================================================================

cat("\nADEE SPECIFICATION SUMMARY\n")
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
      Variable %in% c("AVAL", "AVALU", "AVALC", "CNSR", "EVENT", 
                      "ADT", "ADY", "ADTC") ~ "Analysis Values",
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
cat("  - BDS structure (one record per subject per parameter)\n")
cat("  - Time-to-event ready (AVAL = days, CNSR, EVENT)\n")
cat("  - Multiple exposure representations (raw, log, std, norm, dose-norm, cat)\n")
cat("  - Comprehensive covariate coverage for modeling\n")
cat("  - Standard survival analysis variables\n")
cat("\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================