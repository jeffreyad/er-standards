#===============================================================================
# Program: create_adee_spec_excel.R
# 
# Purpose: Create ADEE specification in metacore-compatible Excel format
#
# Input: specifications/ADEE_P21_Specifications.csv
# Output: specifications/ADEE_spec.xlsx (compatible with spec_to_metacore)
#
#===============================================================================

library(dplyr)
library(readr)
library(writexl)

# Create specifications directory
if (!dir.exists("specifications")) dir.create("specifications", recursive = TRUE)

#===============================================================================
# LOAD P21 SPECIFICATIONS
#===============================================================================

p21_spec <- read_csv("specifications/ADEE_P21_Specifications.csv")

#===============================================================================
# SHEET 1: DEFINE
#===============================================================================

define_sheet <- tibble::tribble(
  ~Attribute, ~Value,
  "StudyName", "ER Standards Framework",
  "StudyDescription", "Exposure-Response Data Standards",
  "ProtocolName", "ER Standards",
  "DefineVersion", "2.0",
  "StandardName", "ADaM-IG",
  "StandardVersion", "1.3",
  "CommentOID", NA_character_,
  "Context", NA_character_,
  "OriginDescription", NA_character_
)

#===============================================================================
# SHEET 2: DATASETS
#===============================================================================

datasets_sheet <- tibble::tribble(
  ~Dataset, ~Label, ~Class, ~SubClass, ~Structure, ~`Key Variables`, ~Standard, ~`Has No Data`, ~Repeating, ~`Reference Data`, ~Comment, ~`Developer Notes`,
  "ADEE", "Exposure-Efficacy Analysis Dataset", "BASIC DATA STRUCTURE", NA_character_, 
  "One record per subject per parameter per analysis timepoint", 
  "USUBJID, PARAMCD, AVISITN", "ADaM-IG v1.3", "No", "Yes", "No", NA_character_, NA_character_
)

#===============================================================================
# SHEET 3: VARIABLES
#===============================================================================

# Map P21 specs to Variables sheet format
variables_sheet <- p21_spec %>%
  mutate(
    Order = row_number(),
    Dataset = "ADEE",
    
    # Data Type mapping
    `Data Type` = case_when(
      Data_Type == "text" ~ "text",
      Data_Type == "integer" ~ "integer",
      Data_Type == "float" ~ "float",
      TRUE ~ "text"
    ),
    
    # Length
    Length = Length,
    
    # Significant Digits (only for float)
    `Significant Digits` = if_else(Data_Type == "float", 2L, NA_integer_),
    
    # Format (SAS format)
    Format = case_when(
      Variable %in% c("TRTSDT", "TRTEDT", "ADT", "STARTDT") ~ "DATE9.",
      Variable == "AVALU" ~ "$20.",
      Variable == "AVALC" ~ "$40.",
      Variable %in% c("PARAMCD", "PARAM") ~ "",
      `Data Type` == "float" ~ "8.2",
      TRUE ~ NA_character_
    ),
    
    # Mandatory
    Mandatory = if_else(Core == "Req", "Yes", "No"),
    
    # Assigned Value (for constants)
    `Assigned Value` = case_when(
      Variable == "PARCAT1" ~ "EFFICACY",
      Variable == "PARCAT2" ~ "TIME TO EVENT",
      Variable == "AVALU" ~ "DAYS",
      Variable == "ATPT" ~ "BASELINE",
      Variable == "AVISIT" ~ "BASELINE",
      TRUE ~ NA_character_
    ),
    
    # Codelist (reference to codelist ID)
    Codelist = case_when(
      Variable == "PARAMCD" ~ "CL.PARAMCD",
      Variable == "PARCAT1" ~ "CL.PARCAT1",
      Variable == "PARCAT2" ~ "CL.PARCAT2",
      Variable == "AVALU" ~ "CL.AVALU",
      Variable == "AVALC" ~ "CL.AVALC",
      Variable == "AUCSSCAT" ~ "CL.AUCSSCAT",
      Variable == "AUCSSQ" ~ "CL.AUCSSQ",
      Variable == "AUCSSMED" ~ "CL.AUCSSMED",
      Variable == "AGEGR1" ~ "CL.AGEGR1",
      Variable == "WTBLGR1" ~ "CL.WTBLGR1",
      Variable == "SEX" ~ "CL.SEX",
      Variable == "ATPT" ~ "CL.ATPT",
      Variable == "AVISIT" ~ "CL.AVISIT",
      Variable %in% c("ANL01FL", "ANL02FL", "ANL03FL", "ANL04FL") ~ "CL.NY",
      Variable %in% c("ARM", "ACTARM", "TRT01P", "TRT01A") ~ "CL.TRT",
      TRUE ~ NA_character_
    ),
    
    # Common (Y if common across datasets)
    Common = case_when(
      Variable %in% c("STUDYID", "USUBJID", "SUBJID", "SITEID",
                      "AGE", "SEX", "RACE", "ETHNIC",
                      "ARM", "ARMN", "ACTARM", "ACTARMN",
                      "TRT01P", "TRT01PN", "TRT01A", "TRT01AN") ~ "Yes",
      TRUE ~ "No"
    ),
    
    # Origin
    Origin = case_when(
      Variable %in% c("STUDYID", "USUBJID", "SUBJID", "AGE", "SEX", "RACE", "ETHNIC",
                      "ARM", "ACTARM", "TRTSDT", "TRTEDT") ~ "Predecessor",
      Variable %in% c("PARAMCD", "PARAM", "AVAL", "ADT", "CNSR", "STARTDT") ~ "Predecessor",
      Variable %in% c("PARCAT1", "PARCAT2", "AVALU", "ATPT", "AVISIT") ~ "Assigned",
      Variable %in% c("EVENT", "AVALC", "ASEQ") ~ "Derived",
      grepl("BL$", Variable) ~ "Predecessor",
      grepl("^(AUCSS|CMAXSS|CAVGSS|CMINSS|CLSS)", Variable) ~ "Assigned",
      grepl("N$", Variable) ~ "Derived",
      TRUE ~ "Derived"
    ),
    
    # Source (only for Predecessor origin)
    Source = case_when(
      Origin == "Predecessor" & Variable %in% c("PARAMCD", "PARAM", "AVAL", "ADT", "CNSR", "STARTDT") ~ "ADTTE",
      Origin == "Predecessor" & grepl("BL$", Variable) ~ "ADSL",
      Origin == "Predecessor" ~ "ADSL",
      TRUE ~ NA_character_
    ),
    
    # Pages
    Pages = NA_character_,
    
    # Method (reference to Methods sheet for complex derivations)
    Method = case_when(
      Variable == "EVENT" ~ "MT.EVENT",
      Variable == "ASEQ" ~ "MT.ASEQ",
      Variable == "BMIBL" ~ "MT.BMI",
      Variable == "BSABL" ~ "MT.BSA",
      Variable == "CRCLBL" ~ "MT.CRCL",
      Variable == "EGFRBL" ~ "MT.EGFR",
      Variable %in% c("AUCSSSTD", "CMAXSSSTD") ~ "MT.STANDARDIZE",
      Variable %in% c("AUCSSCAT", "AUCSSQ") ~ "MT.CATEGORIZE",
      TRUE ~ NA_character_
    ),
    
    # Predecessor (specific predecessor variable)
    Predecessor = case_when(
      Variable == "TBILBL" ~ "ADLB.BILI",
      Variable == "EVENT" ~ "CNSR",
      Variable %in% c("STUDYIDN", "SITEIDN", "USUBJIDN") ~ "USUBJID",
      Variable == "SUBJIDN" ~ "SUBJID",
      TRUE ~ NA_character_
    ),
    
    # Role (ADaM variable role)
    Role = case_when(
      Variable %in% c("STUDYID", "USUBJID", "SUBJID") ~ "Identifier",
      Variable %in% c("ARM", "ACTARM", "TRT01P", "TRT01A") ~ "Treatment",
      Variable %in% c("PARAMCD", "PARAM") ~ "Topic",
      Variable == "AVAL" ~ "Qualifier",
      Variable %in% c("ADT", "TRTSDT", "TRTEDT", "STARTDT") ~ "Timing",
      Variable %in% c("ANL01FL", "ANL02FL", "ANL03FL", "ANL04FL") ~ "Record Qualifier",
      TRUE ~ NA_character_
    ),
    
    # Has No Data
    `Has No Data` = "No",
    
    # Comment
    Comment = NA_character_,
    
    # Developer Notes
    `Developer Notes` = Notes
  ) %>%
  select(
    Order, Dataset, Variable = Variable, Label = Label,
    `Data Type`, Length, `Significant Digits`, Format,
    Mandatory, `Assigned Value`, Codelist, Common,
    Origin, Source, Pages, Method, Predecessor, Role,
    `Has No Data`, Comment, `Developer Notes`
  )

#===============================================================================
# SHEET 4: VALUE LEVEL (conditional variables)
#===============================================================================

value_level_sheet <- tibble::tribble(
  ~Order, ~Dataset, ~Variable, ~`Where Clause`, ~Label, ~`Data Type`, ~Length, ~`Significant Digits`, ~Format, ~Mandatory, ~`Assigned Value`, ~Codelist, ~Origin, ~Source, ~Pages, ~Method, ~Predecessor, ~Comment, ~`Developer Notes`,
  
  1, "ADEE", "ADTF", "ADTF is not missing", "Analysis Date Imputation Flag", "text", 1, NA, "$1.", "No", NA, "CL.ADTF", "Assigned", NA, NA, NA, NA, NA, NA,
  2, "ADEE", "DTYPE", "DTYPE is not missing", "Derivation Type", "text", 8, NA, "$8.", "No", NA, "CL.DTYPE", "Assigned", NA, NA, NA, NA, NA, NA,
  3, "ADEE", "SRCDOM", "SRCDOM is not missing", "Source Data", "text", 8, NA, "$8.", "No", NA, NA, "Predecessor", "ADTTE", NA, NA, NA, NA, NA,
  4, "ADEE", "SRCVAR", "SRCVAR is not missing", "Source Variable", "text", 40, NA, "$40.", "No", NA, NA, "Predecessor", "ADTTE", NA, NA, NA, NA, NA,
  5, "ADEE", "SRCSEQ", "SRCSEQ is not missing", "Source Sequence Number", "integer", 8, NA, NA, "No", NA, NA, "Predecessor", "ADTTE", NA, NA, NA, NA, NA,
  6, "ADEE", "ALBBL", "ALBBL is not missing", "Baseline Albumin (g/dL)", "float", 8, 2, "8.2", "No", NA, NA, "Predecessor", "ADSL", NA, NA, NA, NA, NA
)

#===============================================================================
# SHEET 5: CODELISTS
#===============================================================================

codelists_sheet <- tibble::tribble(
  ~ID, ~Name, ~`NCI Codelist Code`, ~`Data Type`, ~Terminology, ~Comment, ~Order, ~Term, ~`NCI Term Code`, ~`Decoded Value`,
  
  # PARAMCD
  "CL.PARAMCD", "Parameter Code", NA, "text", "CDISC", NA, 1, "PFS", NA, "Progression-Free Survival",
  "CL.PARAMCD", "Parameter Code", NA, "text", "CDISC", NA, 2, "OS", NA, "Overall Survival",
  "CL.PARAMCD", "Parameter Code", NA, "text", "CDISC", NA, 3, "TTP", NA, "Time to Progression",
  "CL.PARAMCD", "Parameter Code", NA, "text", "Sponsor", NA, 4, "TTNT", NA, "Time to Next Treatment",
  
  # PARCAT1
  "CL.PARCAT1", "Parameter Category 1", NA, "text", "Sponsor", NA, 1, "EFFICACY", NA, "Efficacy",
  
  # PARCAT2
  "CL.PARCAT2", "Parameter Category 2", NA, "text", "Sponsor", NA, 1, "TIME TO EVENT", NA, "Time to Event",
  
  # AVALU
  "CL.AVALU", "Analysis Value Unit", NA, "text", "CDISC", NA, 1, "DAYS", NA, "Days",
  
  # AVALC
  "CL.AVALC", "Analysis Value Character", NA, "text", "Sponsor", NA, 1, "EVENT", NA, "Event",
  "CL.AVALC", "Analysis Value Character", NA, "text", "Sponsor", NA, 2, "CENSORED", NA, "Censored",
  
  # AUCSSCAT
  "CL.AUCSSCAT", "AUC Category", NA, "text", "Sponsor", NA, 1, "Low", NA, "Low Exposure",
  "CL.AUCSSCAT", "AUC Category", NA, "text", "Sponsor", NA, 2, "Medium", NA, "Medium Exposure",
  "CL.AUCSSCAT", "AUC Category", NA, "text", "Sponsor", NA, 3, "High", NA, "High Exposure",
  
  # AUCSSQ
  "CL.AUCSSQ", "AUC Quartile", NA, "text", "Sponsor", NA, 1, "Q1", NA, "Quartile 1",
  "CL.AUCSSQ", "AUC Quartile", NA, "text", "Sponsor", NA, 2, "Q2", NA, "Quartile 2",
  "CL.AUCSSQ", "AUC Quartile", NA, "text", "Sponsor", NA, 3, "Q3", NA, "Quartile 3",
  "CL.AUCSSQ", "AUC Quartile", NA, "text", "Sponsor", NA, 4, "Q4", NA, "Quartile 4",
  
  # AUCSSMED
  "CL.AUCSSMED", "AUC Median Split", NA, "text", "Sponsor", NA, 1, "Below Median", NA, "Below Median",
  "CL.AUCSSMED", "AUC Median Split", NA, "text", "Sponsor", NA, 2, "Above Median", NA, "Above Median",
  
  # AGEGR1
  "CL.AGEGR1", "Age Group 1", NA, "text", "Sponsor", NA, 1, "<65", NA, "Less than 65 years",
  "CL.AGEGR1", "Age Group 1", NA, "text", "Sponsor", NA, 2, "65-75", NA, "65 to 75 years",
  "CL.AGEGR1", "Age Group 1", NA, "text", "Sponsor", NA, 3, ">75", NA, "Greater than 75 years",
  
  # WTBLGR1
  "CL.WTBLGR1", "Weight Group 1", NA, "text", "Sponsor", NA, 1, "<70 kg", NA, "Less than 70 kg",
  "CL.WTBLGR1", "Weight Group 1", NA, "text", "Sponsor", NA, 2, ">=70 kg", NA, "Greater than or equal to 70 kg",
  
  # NY (standard)
  "CL.NY", "No Yes Response", "C66742", "text", "CDISC", NA, 1, "Y", "C49488", "Yes",
  "CL.NY", "No Yes Response", "C66742", "text", "CDISC", NA, 2, "", "C49487", "No",
  
  # SEX
  "CL.SEX", "Sex", "C66731", "text", "CDISC", NA, 1, "M", "C20197", "Male",
  "CL.SEX", "Sex", "C66731", "text", "CDISC", NA, 2, "F", "C16576", "Female",
  "CL.SEX", "Sex", "C66731", "text", "CDISC", NA, 3, "U", "C17998", "Unknown",
  
  # ATPT
  "CL.ATPT", "Analysis Timepoint", NA, "text", "CDISC", NA, 1, "BASELINE", NA, "Baseline",
  
  # AVISIT
  "CL.AVISIT", "Analysis Visit", NA, "text", "CDISC", NA, 1, "BASELINE", NA, "Baseline",
  
  # TRT (treatment)
  "CL.TRT", "Treatment", NA, "text", "Sponsor", NA, 1, "Placebo", NA, "Placebo",
  "CL.TRT", "Treatment", NA, "text", "Sponsor", NA, 2, "Xanomeline Low Dose", NA, "Xanomeline Low Dose",
  "CL.TRT", "Treatment", NA, "text", "Sponsor", NA, 3, "Xanomeline High Dose", NA, "Xanomeline High Dose",
  
  # ADTF (optional)
  "CL.ADTF", "Date Imputation Flag", NA, "text", "CDISC", NA, 1, "D", NA, "Day",
  "CL.ADTF", "Date Imputation Flag", NA, "text", "CDISC", NA, 2, "M", NA, "Month",
  "CL.ADTF", "Date Imputation Flag", NA, "text", "CDISC", NA, 3, "Y", NA, "Year",
  
  # DTYPE (optional)
  "CL.DTYPE", "Derivation Type", NA, "text", "CDISC", NA, 1, "AVERAGE", NA, "Average",
  "CL.DTYPE", "Derivation Type", NA, "text", "CDISC", NA, 2, "MINIMUM", NA, "Minimum",
  "CL.DTYPE", "Derivation Type", NA, "text", "CDISC", NA, 3, "MAXIMUM", NA, "Maximum"
)

#===============================================================================
# SHEET 6: DICTIONARIES (empty for now)
#===============================================================================

dictionaries_sheet <- tibble::tribble(
  ~ID, ~Name, ~`Data Type`, ~Dictionary, ~Version
)

#===============================================================================
# SHEET 7: METHODS
#===============================================================================

methods_sheet <- tibble::tribble(
  ~ID, ~Name, ~Type, ~Description, ~`Expression Context`, ~`Expression Code`, ~Document, ~Pages,
  
  "MT.EVENT", "Event Indicator", "Computation", "Derived as 1 - CNSR", "R", "EVENT = 1 - CNSR", NA, NA,
  "MT.ASEQ", "Analysis Sequence Number", "Computation", "Sequential numbering within USUBJID ordered by PARAMN, AVISITN", "R", "derive_var_obs_number(by_vars = exprs(STUDYID, USUBJID), order = exprs(PARAMN, AVISITN), new_var = ASEQ)", NA, NA,
  "MT.BMI", "Body Mass Index", "Computation", "Weight (kg) / (Height (m))^2", "R", "compute_bmi(height = HTBL, weight = WTBL)", NA, NA,
  "MT.BSA", "Body Surface Area", "Computation", "Mosteller formula", "R", "compute_bsa(height = HTBL, weight = WTBL, method = 'Mosteller')", NA, NA,
  "MT.CRCL", "Creatinine Clearance", "Computation", "Cockcroft-Gault equation", "R", "compute_egfr(creat = CREATBL, creatu = 'SI', age = AGE, weight = WTBL, sex = SEX, method = 'CRCL')", NA, NA,
  "MT.EGFR", "eGFR", "Computation", "CKD-EPI equation", "R", "compute_egfr(creat = CREATBL, creatu = 'SI', age = AGE, weight = WTBL, sex = SEX, method = 'CKD-EPI')", NA, NA,
  "MT.STANDARDIZE", "Standardize", "Computation", "(Value - mean) / SD for active subjects only", "R", "(AUCSS - mean(AUCSS[DOSE > 0])) / sd(AUCSS[DOSE > 0])", NA, NA,
  "MT.CATEGORIZE", "Categorize by Quantiles", "Computation", "Cut into tertiles or quartiles based on active subjects", "R", "cut(AUCSS, breaks = quantile(AUCSS[DOSE > 0], probs = c(0, 1/3, 2/3, 1)))", NA, NA
)

#===============================================================================
# SHEET 8: COMMENTS (empty for now)
#===============================================================================

comments_sheet <- tibble::tribble(
  ~ID, ~Description, ~Document, ~Pages
)

#===============================================================================
# SHEET 9: DOCUMENTS (empty for now)
#===============================================================================

documents_sheet <- tibble::tribble(
  ~ID, ~Title, ~Href
)

#===============================================================================
# SHEET 10: ANALYSIS DISPLAYS (empty for now)
#===============================================================================

analysis_displays_sheet <- tibble::tribble(
  ~ID, ~Title, ~Document, ~Pages
)

#===============================================================================
# SHEET 11: ANALYSIS RESULTS (empty for now)
#===============================================================================

analysis_results_sheet <- tibble::tribble(
  ~Display, ~ID, ~Description, ~Variables, ~Reason, ~Purpose, ~`Selection Criteria`, ~`Join Comment`, ~Documentation, ~`Documentation Refs`, ~`Programming Context`, ~`Programming Code`, ~`Programming Document`, ~Pages
)

#===============================================================================
# WRITE EXCEL FILE
#===============================================================================

spec_excel <- list(
  Define = define_sheet,
  Datasets = datasets_sheet,
  Variables = variables_sheet,
  ValueLevel = value_level_sheet,
  Codelists = codelists_sheet,
  Dictionaries = dictionaries_sheet,
  Methods = methods_sheet,
  Comments = comments_sheet,
  Documents = documents_sheet,
  `Analysis Displays` = analysis_displays_sheet,
  `Analysis Results` = analysis_results_sheet
)

writexl::write_xlsx(spec_excel, "specifications/ADEE_spec.xlsx")

message("✓ Created: specifications/ADEE_spec.xlsx")
message("  Format compatible with spec_to_metacore()")
message("  Total variables: ", nrow(variables_sheet))
message("  Codelists: ", length(unique(codelists_sheet$ID)))

#===============================================================================
# END
#===============================================================================