# ===============================================================================
# Generate P21 Specification Files for ER Standards
#
# Purpose: Create CSV and Excel files with variable specifications for
#          ADER, ADEE, ADES, and ADTR datasets
#
# Output:
#   - CSV files (one per dataset)
#   - Combined Excel workbook (one sheet per dataset)
#   - Define-XML snippets
# ===============================================================================

library(dplyr)
library(writexl)
library(openxlsx)

# Create output directory
if (!dir.exists("specifications")) dir.create("specifications", recursive = TRUE)

# ===============================================================================
# ADER SPECIFICATIONS
# ===============================================================================

ader_specs <- tribble(
  ~Variable, ~Label, ~Data_Type, ~Length, ~Controlled_Terms, ~Core, ~Notes,

  # Identifiers
  "STUDYID", "Study Identifier", "text", 20, "", "Req", "",
  "STUDYIDN", "Study Identifier (N)", "integer", 8, "", "", "Numeric version",
  "SITEID", "Study Site Identifier", "text", 10, "", "", "",
  "SITEIDN", "Study Site Identifier (N)", "integer", 8, "", "", "Numeric version",
  "USUBJID", "Unique Subject Identifier", "text", 40, "", "Req", "",
  "USUBJIDN", "Unique Subject Identifier (N)", "integer", 8, "", "", "Numeric version",
  "SUBJID", "Subject Identifier for the Study", "text", 20, "", "Req", "",
  "SUBJIDN", "Subject Identifier (N)", "integer", 8, "", "", "Numeric version",

  # Demographics
  "AGE", "Age", "integer", 8, "", "", "Years",
  "SEX", "Sex", "text", 1, "(SEX)", "", "M, F, U",
  "SEXN", "Sex (N)", "integer", 8, "", "", "1=M, 2=F, 3=U",
  "RACE", "Race", "text", 40, "(RACE)", "", "",
  "RACEN", "Race (N)", "integer", 8, "", "", "1-6 coded",
  "ETHNIC", "Ethnicity", "text", 40, "(ETHNIC)", "", "",
  "ETHNICN", "Ethnicity (N)", "integer", 8, "", "", "1-3 coded",

  # Treatment
  "COHORT", "Cohort", "integer", 8, "", "", "Treatment group number",
  "COHORTC", "Cohort (C)", "text", 40, "", "", "Treatment group description",
  "ARMN", "Planned Arm (N)", "integer", 8, "", "", "0-3 coded",
  "ACTARMN", "Actual Arm (N)", "integer", 8, "", "", "0-3 coded",
  "ROUTE", "Route of Administration", "text", 40, "(ROUTE)", "", "",
  "ROUTEN", "Route of Administration (N)", "integer", 8, "", "", "",
  "FORM", "Dosage Form", "text", 40, "(FRM)", "", "",
  "FORMN", "Dosage Form (N)", "integer", 8, "", "", "",

  # Geography
  "COUNTRY", "Country", "text", 3, "(COUNTRY)", "", "ISO 3-char code",
  "COUNTRYN", "Country (N)", "integer", 8, "", "", "Numeric code",
  "COUNTRYL", "Country Name", "text", 40, "", "", "Full country name",

  # Efficacy Endpoints (Wide Format)
  "OS", "Overall Survival (Days)", "float", 8, "", "", "Time to death/censor",
  "OSIND", "Overall Survival Event Indicator", "integer", 8, "", "", "1=event, 0=censored",
  "PFS", "Progression-Free Survival (Days)", "float", 8, "", "", "Time to progression/death",
  "PFSIND", "PFS Event Indicator", "integer", 8, "", "", "1=event, 0=censored",
  "BOR", "Best Overall Response", "text", 8, "(OVRLRESP)", "", "CR, PR, SD, PD",

  # Baseline Vitals
  "WTBL", "Baseline Weight (kg)", "float", 8, "", "", "",
  "HTBL", "Baseline Height (cm)", "float", 8, "", "", "",
  "BMIBL", "Baseline BMI (kg/m^2)", "float", 8, "", "", "Computed from WTBL, HTBL",
  "BSABL", "Baseline BSA (m^2)", "float", 8, "", "", "Mosteller formula",

  # Baseline Labs
  "CREATBL", "Baseline Serum Creatinine (mg/dL)", "float", 8, "", "", "",
  "CRCLBL", "Baseline Creatinine Clearance (mL/min)", "float", 8, "", "", "Cockcroft-Gault",
  "EGFRBL", "Baseline eGFR (mL/min/1.73m^2)", "float", 8, "", "", "CKD-EPI equation",
  "ALTBL", "Baseline ALT (U/L)", "float", 8, "", "", "",
  "ASTBL", "Baseline AST (U/L)", "float", 8, "", "", "",
  "TBILBL", "Baseline Total Bilirubin (mg/dL)", "float", 8, "", "", ""
)

# ===============================================================================
# ADEE SPECIFICATIONS
# ===============================================================================

adee_specs <- tribble(
  ~Variable, ~Label, ~Data_Type, ~Length, ~Controlled_Terms, ~Core, ~Notes,

  # Identifiers
  "STUDYID", "Study Identifier", "text", 20, "", "Req", "",
  "USUBJID", "Unique Subject Identifier", "text", 40, "", "Req", "",
  "SUBJID", "Subject Identifier for the Study", "text", 20, "", "Req", "",
  "SITEID", "Study Site Identifier", "text", 10, "", "", "",

  # Demographics
  "AGE", "Age", "integer", 8, "", "", "Years",
  "AGEGR1", "Pooled Age Group 1", "text", 20, "(AGEGR1)", "", "<65, 65-75, >75",
  "AGEGR1N", "Pooled Age Group 1 (N)", "integer", 8, "", "", "1, 2, 3",
  "SEX", "Sex", "text", 1, "(SEX)", "", "M, F, U",
  "SEXN", "Sex (N)", "integer", 8, "", "", "1=M, 2=F, 3=U",
  "RACE", "Race", "text", 40, "(RACE)", "", "",
  "RACEN", "Race (N)", "integer", 8, "", "", "1-6 coded",
  "ETHNIC", "Ethnicity", "text", 40, "(ETHNIC)", "", "",

  # Treatment
  "ARM", "Description of Planned Arm", "text", 40, "", "", "",
  "ARMN", "Planned Arm (N)", "integer", 8, "", "", "",
  "ACTARM", "Description of Actual Arm", "text", 40, "", "", "",
  "ACTARMN", "Actual Arm (N)", "integer", 8, "", "", "",
  "TRTSDT", "Date of First Exposure to Treatment", "integer", 8, "", "", "SAS date",
  "TRTEDT", "Date of Last Exposure to Treatment", "integer", 8, "", "", "SAS date",
  "TRTDURD", "Total Treatment Duration (Days)", "integer", 8, "", "", "",

  # Parameter Information
  "PARAMCD", "Parameter Code", "text", 8, "", "Req", "OS, PFS, TTP, TTNT",
  "PARAM", "Parameter", "text", 200, "", "Req", "Full description",
  "PARAMN", "Parameter (N)", "integer", 8, "", "", "For sorting",
  "PARCAT1", "Parameter Category 1", "text", 40, "", "", "EFFICACY",
  "PARCAT2", "Parameter Category 2", "text", 40, "", "", "TIME TO EVENT",

  # Analysis Values
  "AVAL", "Analysis Value", "float", 8, "", "Cond", "Time in days",
  "AVALU", "Analysis Value Unit", "text", 20, "(UNIT)", "", "DAYS",
  "AVALC", "Analysis Value (C)", "text", 40, "", "", "CENSORED, EVENT",
  "ADT", "Analysis Date", "integer", 8, "", "", "SAS date",
  "ADTF", "Analysis Date Imputation Flag", "text", 1, "(STENRF)", "", "",
  "ADY", "Analysis Relative Day", "integer", 8, "", "", "Relative to TRTSDT",
  "STARTDT", "Time to Event Origin Date for Subject", "integer", 8, "", "", "Usually TRTSDT",

  # Time-to-Event Specific
  "CNSR", "Censor", "integer", 8, "", "Cond", "1=censored, 0=event",
  "EVENT", "Event Indicator", "integer", 8, "", "Cond", "1=event, 0=censored",
  "EVNTDESC", "Event or Censoring Description", "text", 200, "", "", "",
  "SRCDOM", "Source Data", "text", 8, "", "", "Source domain",
  "SRCVAR", "Source Variable", "text", 40, "", "", "Source variable name",
  "SRCSEQ", "Source Sequence Number", "integer", 8, "", "", "Source AESEQ, etc.",

  # Analysis Timepoint
  "ATPT", "Analysis Timepoint", "text", 40, "", "", "BASELINE, MONTH 6",
  "ATPTN", "Analysis Timepoint (N)", "integer", 8, "", "", "0, 6",
  "AVISIT", "Analysis Visit", "text", 40, "", "", "BASELINE, WEEK 24",
  "AVISITN", "Analysis Visit (N)", "integer", 8, "", "", "0, 24",

  # Exposure Variables - Primary
  "AUCSS", "Steady-State AUC (ug·h/mL)", "float", 8, "", "", "Primary exposure metric",
  "AUCSSLOG", "Log-Transformed AUC", "float", 8, "", "", "ln(AUCSS)",
  "AUCSSSTD", "Standardized AUC", "float", 8, "", "", "(AUCSS-mean)/SD",
  "AUCSSN", "Normalized AUC", "float", 8, "", "", "AUCSS/median",
  "AUCSSDOSE", "Dose-Normalized AUC (ug·h/mL per mg)", "float", 8, "", "", "AUCSS/DOSE",
  "AUCSSCAT", "AUC Category (Tertiles)", "text", 20, "", "", "Low, Medium, High",
  "AUCSSCATN", "AUC Category (N)", "integer", 8, "", "", "1, 2, 3",
  "AUCSSQ", "AUC Quartile", "text", 20, "", "", "Q1, Q2, Q3, Q4",
  "AUCSSQN", "AUC Quartile (N)", "integer", 8, "", "", "1, 2, 3, 4",

  # Exposure Variables - Alternative
  "CMAXSS", "Steady-State Cmax (ug/mL)", "float", 8, "", "", "Alternative exposure",
  "CAVGSS", "Steady-State Cavg (ug/mL)", "float", 8, "", "", "Alternative exposure",
  "CMINSS", "Steady-State Cmin (ug/mL)", "float", 8, "", "", "Trough concentration",

  # Baseline Covariates
  "WTBL", "Baseline Weight (kg)", "float", 8, "", "", "",
  "WTBLGR1", "Baseline Weight Group 1", "text", 20, "", "", "<70 kg, ≥70 kg",
  "HTBL", "Baseline Height (cm)", "float", 8, "", "", "",
  "BMIBL", "Baseline BMI (kg/m^2)", "float", 8, "", "", "",
  "BSABL", "Baseline BSA (m^2)", "float", 8, "", "", "",
  "CREATBL", "Baseline Serum Creatinine (mg/dL)", "float", 8, "", "", "",
  "CRCLBL", "Baseline Creatinine Clearance (mL/min)", "float", 8, "", "", "",
  "EGFRBL", "Baseline eGFR (mL/min/1.73m^2)", "float", 8, "", "", "",
  "ALTBL", "Baseline ALT (U/L)", "float", 8, "", "", "",
  "ASTBL", "Baseline AST (U/L)", "float", 8, "", "", "",
  "TBILBL", "Baseline Total Bilirubin (mg/dL)", "float", 8, "", "", "",
  "ALBBL", "Baseline Albumin (g/dL)", "float", 8, "", "", "",

  # Analysis Flags
  "ANL01FL", "Analysis Flag 01", "text", 1, "(NY)", "", "Y=primary endpoint (PFS)",
  "ANL02FL", "Analysis Flag 02", "text", 1, "(NY)", "", "Y=secondary endpoint (OS)",
  "ANL03FL", "Analysis Flag 03", "text", 1, "(NY)", "", "Y=sensitivity analysis",
  "ANL04FL", "Analysis Flag 04", "text", 1, "(NY)", "", "Y=subgroup analysis",

  # Record Identifiers
  "ASEQ", "Analysis Sequence Number", "integer", 8, "", "", "Unique within USUBJID",
  "DTYPE", "Derivation Type", "text", 8, "", "", "AVERAGE, MINIMUM, etc."
)

# ===============================================================================
# ADES SPECIFICATIONS
# ===============================================================================

ades_specs <- tribble(
  ~Variable, ~Label, ~Data_Type, ~Length, ~Controlled_Terms, ~Core, ~Notes,

  # Identifiers
  "STUDYID", "Study Identifier", "text", 20, "", "Req", "",
  "USUBJID", "Unique Subject Identifier", "text", 40, "", "Req", "",
  "SUBJID", "Subject Identifier for the Study", "text", 20, "", "Req", "",
  "AGE", "Age", "integer", 8, "", "", "Years",
  "SEX", "Sex", "text", 1, "(SEX)", "", "",
  "SEXN", "Sex (N)", "integer", 8, "", "", "",
  "RACE", "Race", "text", 40, "(RACE)", "", "",
  "WTBL", "Baseline Weight (kg)", "float", 8, "", "", "",
  "BMIBL", "Baseline BMI (kg/m^2)", "float", 8, "", "", "",

  # Parameter Information
  "PARAMCD", "Parameter Code", "text", 8, "", "Req", "SUBJSUM, AEVENT, or AEDECOD",
  "PARAM", "Parameter", "text", 200, "", "Req", "Parameter description",

  # Analysis Values
  "AVAL", "Analysis Value", "float", 8, "", "Cond", "Count or indicator",
  "AVALU", "Analysis Value Unit", "text", 20, "", "", "COUNT, EVENT, RATE",
  "ADT", "Analysis Date", "integer", 8, "", "", "SAS date (event level)",
  "ADY", "Analysis Relative Day", "integer", 8, "", "", "Relative to TRTSDT",
  "TRTDURD", "Total Treatment Duration (Days)", "integer", 8, "", "", "Exposure time",

  # Subject-Level Summary
  "N_AES", "Total Number of AEs", "integer", 8, "", "", "Subject-level summary",
  "N_SAE", "Number of Serious AEs", "integer", 8, "", "", "Subject-level summary",
  "N_GRADE3", "Number of Grade 3+ AEs", "integer", 8, "", "", "Subject-level summary",
  "N_GRADE4", "Number of Grade 4 AEs", "integer", 8, "", "", "Subject-level summary",
  "RATE_AES", "AE Rate (per 100 patient-days)", "float", 8, "", "", "N_AES/TRTDURD*100",
  "RATE_SAE", "SAE Rate (per 100 patient-days)", "float", 8, "", "", "N_SAE/TRTDURD*100",
  "ANY_AE", "Any AE Flag", "text", 1, "(NY)", "", "Y if N_AES > 0",
  "ANY_SAE", "Any SAE Flag", "text", 1, "(NY)", "", "Y if N_SAE > 0",

  # Event-Level Details
  "AEDECOD", "Adverse Event Dictionary-Derived Term", "text", 200, "(AEDECOD)", "", "MedDRA Preferred Term",
  "AEBODSYS", "Body System or Organ Class", "text", 200, "(AEBODSYS)", "", "MedDRA System Organ Class",
  "AESTDY", "Study Day of Start of Adverse Event", "integer", 8, "", "", "Relative to TRTSDT",
  "AEENDY", "Study Day of End of Adverse Event", "integer", 8, "", "", "Relative to TRTSDT",
  "AETOXGR", "Standard Toxicity Grade", "text", 2, "(AETOXGR)", "", "CTCAE grade (1-5)",
  "AESER", "Serious Event", "text", 1, "(NY)", "", "Regulatory serious",
  "AEREL", "Causality", "text", 40, "(AEREL)", "", "Investigator assessment",

  # Parameter-Level Summary
  "N_EVENTS", "Number of Event Occurrences", "integer", 8, "", "", "Parameter-level count",
  "MAX_GRADE", "Maximum Toxicity Grade", "integer", 8, "", "", "Maximum across events",
  "RATE", "Event Rate (per 100 patient-days)", "float", 8, "", "", "Parameter-level rate",

  # Exposure Variables
  "AUCSS", "Steady-State AUC (ug·h/mL)", "float", 8, "", "", "Primary exposure metric",
  "AUCSSCAT", "AUC Category (Tertiles)", "text", 20, "", "", "Low, Medium, High",

  # Analysis Flags
  "ANL01FL", "Analysis Flag 01", "text", 1, "(NY)", "", "Y=all AEs",
  "ANL02FL", "Analysis Flag 02", "text", 1, "(NY)", "", "Y=serious AEs only",
  "ANL03FL", "Analysis Flag 03", "text", 1, "(NY)", "", "Y=grade 3+ only",

  # Record Identifiers
  "ASEQ", "Analysis Sequence Number", "integer", 8, "", "", "Unique within USUBJID"
)

# ===============================================================================
# ADTR SPECIFICATIONS
# ===============================================================================

adtr_specs <- tribble(
  ~Variable, ~Label, ~Data_Type, ~Length, ~Controlled_Terms, ~Core, ~Notes,

  # Identifiers
  "STUDYID", "Study Identifier", "text", 20, "", "Req", "",
  "USUBJID", "Unique Subject Identifier", "text", 40, "", "Req", "",
  "SUBJID", "Subject Identifier for the Study", "text", 20, "", "Req", "",
  "AGE", "Age", "integer", 8, "", "", "Years",
  "SEX", "Sex", "text", 1, "(SEX)", "", "",
  "WTBL", "Baseline Weight (kg)", "float", 8, "", "", "",
  "BMIBL", "Baseline BMI (kg/m^2)", "float", 8, "", "", "",
  "DOSE", "Dose (mg)", "float", 8, "", "", "Administered dose",

  # Parameter Information
  "PARAMCD", "Parameter Code", "text", 8, "", "Req", "TUMSIZE, BOR, NADIR",
  "PARAM", "Parameter", "text", 200, "", "Req", "Parameter description",

  # Visit Information
  "AVISITN", "Analysis Visit (N)", "integer", 8, "", "", "0, 1, 2, 3, ...",
  "AVISIT", "Analysis Visit", "text", 40, "", "", "BASELINE, WEEK 6, ...",
  "ADT", "Analysis Date", "integer", 8, "", "", "SAS date",
  "ADY", "Analysis Relative Day", "integer", 8, "", "", "Relative to TRTSDT",

  # Analysis Values
  "AVAL", "Analysis Value", "float", 8, "", "Cond", "Tumor size (mm) or response",
  "AVALU", "Analysis Value Unit", "text", 20, "(UNIT)", "", "mm (for TUMSIZE)",
  "AVALC", "Analysis Value (C)", "text", 8, "", "", "CR, PR, SD, PD",
  "AVALN", "Analysis Value (N)", "integer", 8, "", "", "4=CR, 3=PR, 2=SD, 1=PD",

  # Baseline and Change
  "BASE", "Baseline Value", "float", 8, "", "", "Baseline tumor size (mm)",
  "CHG", "Change from Baseline", "float", 8, "", "", "AVAL - BASE",
  "PCHG", "Percent Change from Baseline", "float", 8, "", "", "((AVAL-BASE)/BASE)*100",

  # Response Variables
  "BOR", "Best Overall Response", "text", 8, "(OVRLRESP)", "", "CR, PR, SD, PD",
  "BORN", "Best Overall Response (N)", "integer", 8, "", "", "4=CR, 3=PR, 2=SD, 1=PD",
  "NADIR", "Nadir Tumor Size (mm)", "float", 8, "", "", "Minimum post-baseline",
  "NADIR_PCHG", "Nadir Percent Change", "float", 8, "", "", "% change at nadir",
  "NADIR_VISIT", "Nadir Visit", "text", 40, "", "", "Visit of minimum value",

  # Flags
  "ABLFL", "Baseline Record Flag", "text", 1, "(NY)", "", "Y for baseline (AVISITN=0)",
  "ANL01FL", "Analysis Flag 01", "text", 1, "(NY)", "", "Y for post-baseline",
  "ANL02FL", "Analysis Flag 02", "text", 1, "(NY)", "", "Y for responders (CR/PR)",

  # Exposure Variables
  "AUCSS", "Steady-State AUC (ug·h/mL)", "float", 8, "", "", "Primary exposure metric",
  "AUCSSLOG", "Log-Transformed AUC", "float", 8, "", "", "ln(AUCSS)",
  "AUCSSCAT", "AUC Category (Tertiles)", "text", 20, "", "", "Low, Medium, High",

  # Record Identifiers
  "ASEQ", "Analysis Sequence Number", "integer", 8, "", "", "Unique within USUBJID"
)

# ===============================================================================
# SAVE AS CSV FILES
# ===============================================================================

write.csv(ader_specs, "specifications/ADER_P21_Specifications.csv", row.names = FALSE)
write.csv(adee_specs, "specifications/ADEE_P21_Specifications.csv", row.names = FALSE)
write.csv(ades_specs, "specifications/ADES_P21_Specifications.csv", row.names = FALSE)
write.csv(adtr_specs, "specifications/ADTR_P21_Specifications.csv", row.names = FALSE)

cat("CSV files saved to specifications/ directory\n")

# ===============================================================================
# SAVE AS COMBINED EXCEL WORKBOOK
# ===============================================================================

# Create workbook
wb <- createWorkbook()

# Add sheets
addWorksheet(wb, "ADER")
addWorksheet(wb, "ADEE")
addWorksheet(wb, "ADES")
addWorksheet(wb, "ADTR")
addWorksheet(wb, "README")

# Write data
writeData(wb, "ADER", ader_specs)
writeData(wb, "ADEE", adee_specs)
writeData(wb, "ADES", ades_specs)
writeData(wb, "ADTR", adtr_specs)

# Create README
readme_content <- data.frame(
  Section = c(
    "Title",
    "Version",
    "Date",
    "Author",
    "",
    "Description",
    "",
    "",
    "",
    "Sheets",
    "",
    "",
    "",
    "",
    "Columns",
    "",
    "",
    "",
    "",
    "",
    "",
    "Notes"
  ),
  Content = c(
    "ER Standards Framework - P21 Variable Specifications",
    "1.0",
    as.character(Sys.Date()),
    "[Your Name]",
    "",
    "This workbook contains P21-style variable specifications for the proposed",
    "Exposure-Response (ER) data standards framework, including ADEE, ADES, and ADTR",
    "datasets, along with the original ADER template for comparison.",
    "",
    "ADER - Original pharmaverseadam template (wide format)",
    "ADEE - Proposed Exposure-Efficacy dataset (BDS format)",
    "ADES - Proposed Exposure-Safety dataset (multi-level BDS)",
    "ADTR - Proposed Tumor Response dataset (longitudinal BDS)",
    "",
    "Variable - Variable name",
    "Label - Variable label",
    "Data_Type - text, integer, float",
    "Length - Storage length",
    "Controlled_Terms - CDISC CT reference (e.g., (SEX), (RACE))",
    "Core - Req (required), Cond (conditional), blank (optional)",
    "Notes - Additional information, derivation rules, examples",
    ""
  )
)

writeData(wb, "README", readme_content)

# Format headers
headerStyle <- createStyle(
  fontColour = "#FFFFFF",
  fgFill = "#4F81BD",
  halign = "center",
  valign = "center",
  textDecoration = "bold",
  border = "TopBottomLeftRight"
)

addStyle(wb, "ADER", headerStyle, rows = 1, cols = 1:7, gridExpand = TRUE)
addStyle(wb, "ADEE", headerStyle, rows = 1, cols = 1:7, gridExpand = TRUE)
addStyle(wb, "ADES", headerStyle, rows = 1, cols = 1:7, gridExpand = TRUE)
addStyle(wb, "ADTR", headerStyle, rows = 1, cols = 1:7, gridExpand = TRUE)

# Set column widths
setColWidths(wb, "ADER", cols = 1:7, widths = c(15, 35, 12, 8, 18, 8, 40))
setColWidths(wb, "ADEE", cols = 1:7, widths = c(15, 35, 12, 8, 18, 8, 40))
setColWidths(wb, "ADES", cols = 1:7, widths = c(15, 35, 12, 8, 18, 8, 40))
setColWidths(wb, "ADTR", cols = 1:7, widths = c(15, 35, 12, 8, 18, 8, 40))

# Freeze panes
freezePane(wb, "ADER", firstRow = TRUE)
freezePane(wb, "ADEE", firstRow = TRUE)
freezePane(wb, "ADES", firstRow = TRUE)
freezePane(wb, "ADTR", firstRow = TRUE)

# Save workbook
saveWorkbook(wb, "specifications/ER_Standards_P21_Specifications.xlsx", overwrite = TRUE)

cat("Excel workbook saved: specifications/ER_Standards_P21_Specifications.xlsx\n")

# ===============================================================================
# CREATE DEFINE-XML SNIPPET GENERATOR
# ===============================================================================

generate_definexml_snippet <- function(specs, dataset_name, dataset_label, dataset_structure) {
  # Helper function to escape XML special characters
  escape_xml <- function(text) {
    text <- gsub("&", "&amp;", text)
    text <- gsub("<", "&lt;", text)
    text <- gsub(">", "&gt;", text)
    text <- gsub('"', "&quot;", text)
    text <- gsub("'", "&apos;", text)
    # Replace special characters that cause issues
    text <- gsub("μ", "micro", text)
    text <- gsub("·", "*", text)
    text <- gsub("²", "^2", text)
    text <- gsub("≥", "&gt;=", text)
    text <- gsub("≤", "&lt;=", text)
    return(text)
  }

  xml_header <- paste0('<?xml version="1.0" encoding="UTF-8"?>
<ODM xmlns="http://www.cdisc.org/ns/odm/v1.3"
     xmlns:def="http://www.cdisc.org/ns/def/v2.0"
     xmlns:xlink="http://www.w3.org/1999/xlink"
     ODMVersion="1.3.2"
     FileOID="', dataset_name, '_Define"
     FileType="Snapshot"
     CreationDateTime="', format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), '">

  <Study OID="STUDY001">
    <GlobalVariables>
      <StudyName>ER Standards Framework</StudyName>
      <StudyDescription>Exposure-Response Data Standards</StudyDescription>
      <ProtocolName>ER Standards</ProtocolName>
    </GlobalVariables>

    <MetaDataVersion OID="MDV.001" Name="ER Standards Metadata">

      <!-- ItemGroupDef for ', dataset_name, ' -->
      <ItemGroupDef OID="IG.', dataset_name, '"
                    Name="', dataset_name, '"
                    Repeating="Yes"
                    IsReferenceData="No"
                    SASDatasetName="', dataset_name, '"
                    Domain="', dataset_name, '"
                    def:Class="', escape_xml(dataset_structure), '"
                    def:Structure="', escape_xml(dataset_structure), '">
        <Description>
          <TranslatedText xml:lang="en">', escape_xml(dataset_label), "</TranslatedText>
        </Description>
        \n")

  # Generate ItemRefs
  item_refs <- ""
  for (i in 1:nrow(specs)) {
    # Clean variable name for OID (remove special characters)
    clean_varname <- gsub("[^A-Za-z0-9_]", "", specs$Variable[i])

    item_refs <- paste0(
      item_refs,
      '        <ItemRef ItemOID="IT.', dataset_name, ".", clean_varname, '" ',
      'OrderNumber="', i, '" ',
      'Mandatory="', ifelse(specs$Core[i] == "Req", "Yes", "No"), '" />\n'
    )
  }

  xml_middle <- paste0("      </ItemGroupDef>

      <!-- ItemDefs for ", dataset_name, " -->\n")

  # Generate ItemDefs
  item_defs <- ""
  for (i in 1:nrow(specs)) {
    # Clean variable name for OID
    clean_varname <- gsub("[^A-Za-z0-9_]", "", specs$Variable[i])

    data_type_map <- c(
      "text" = "text",
      "integer" = "integer",
      "float" = "float"
    )

    # Clean controlled terms reference
    clean_ct <- gsub("[^A-Za-z0-9_]", "", specs$Controlled_Terms[i])

    item_defs <- paste0(
      item_defs,
      '      <ItemDef OID="IT.', dataset_name, ".", clean_varname, '" ',
      'Name="', specs$Variable[i], '" ',
      'DataType="', data_type_map[specs$Data_Type[i]], '" ',
      'Length="', specs$Length[i], '"',
      ifelse(specs$Controlled_Terms[i] != "",
        paste0(' def:CodeListOID="CL.', clean_ct, '"'),
        ""
      ),
      ">\n",
      "        <Description>\n",
      '          <TranslatedText xml:lang="en">', escape_xml(specs$Label[i]), "</TranslatedText>\n",
      "        </Description>\n"
    )

    if (specs$Notes[i] != "") {
      item_defs <- paste0(
        item_defs,
        '        <def:Origin Type="Assigned">\n',
        "          <Description>\n",
        '            <TranslatedText xml:lang="en">', escape_xml(specs$Notes[i]), "</TranslatedText>\n",
        "          </Description>\n",
        "        </def:Origin>\n"
      )
    }

    item_defs <- paste0(item_defs, "      </ItemDef>\n\n")
  }

  xml_footer <- "    </MetaDataVersion>
  </Study>
</ODM>"

  # Combine all parts
  full_xml <- paste0(xml_header, item_refs, xml_middle, item_defs, xml_footer)

  return(full_xml)
}

# Generate Define-XML snippets
ader_xml <- generate_definexml_snippet(
  ader_specs,
  "ADER",
  "Exposure-Response Analysis Dataset",
  "Custom (Wide Format)"
)

adee_xml <- generate_definexml_snippet(
  adee_specs,
  "ADEE",
  "Exposure-Efficacy Analysis Dataset",
  "Basic Data Structure"
)

ades_xml <- generate_definexml_snippet(
  ades_specs,
  "ADES",
  "Exposure-Safety Analysis Dataset",
  "Basic Data Structure (Multi-Level)"
)

adtr_xml <- generate_definexml_snippet(
  adtr_specs,
  "ADTR",
  "Tumor Response Analysis Dataset",
  "Basic Data Structure"
)

# Save Define-XML snippets
writeLines(ader_xml, "specifications/ADER_DefineXML.xml")
writeLines(adee_xml, "specifications/ADEE_DefineXML.xml")
writeLines(ades_xml, "specifications/ADES_DefineXML.xml")
writeLines(adtr_xml, "specifications/ADTR_DefineXML.xml")

cat("\nDefine-XML snippets saved to specifications/ directory\n")

# ===============================================================================
# SUMMARY
# ===============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("SPECIFICATION FILES GENERATED\n")
cat(strrep("=", 70), "\n\n")

cat("CSV Files (individual datasets):\n")
cat("  - ADER_P21_Specifications.csv\n")
cat("  - ADEE_P21_Specifications.csv\n")
cat("  - ADES_P21_Specifications.csv\n")
cat("  - ADTR_P21_Specifications.csv\n\n")

cat("Excel Workbook (all datasets):\n")
cat("  - ER_Standards_P21_Specifications.xlsx\n")
cat("    • ADER sheet:", nrow(ader_specs), "variables\n")
cat("    • ADEE sheet:", nrow(adee_specs), "variables\n")
cat("    • ADES sheet:", nrow(ades_specs), "variables\n")
cat("    • ADTR sheet:", nrow(adtr_specs), "variables\n")
cat("    • README sheet\n\n")

cat("Define-XML Snippets:\n")
cat("  - ADER_DefineXML.xml\n")
cat("  - ADEE_DefineXML.xml\n")
cat("  - ADES_DefineXML.xml\n")
cat("  - ADTR_DefineXML.xml\n\n")

cat("All files saved in: specifications/\n")

# ===============================================================================
# END
# ===============================================================================
