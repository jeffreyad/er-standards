# Name: ADEE
#
# Label: Exposure Response Data
#
# Input: adsl, adae, adlb, advs, adex, adpp
# Load required packages
library(admiral)
library(admiralonco)
# pharmaverseadam contains example datasets generated from the CDISC pilot
# project SDTM ran through admiral templates
library(pharmaverseadam)
library(dplyr)
library(lubridate)
library(stringr)
library(metacore)
library(metatools)
library(xportr)

# Load Specs for Metacore ----
# Load Specs for Metacore ----

metacore <- spec_to_metacore("specifications/er_spec.xlsx",
    where_sep_sheet = FALSE  )%>%
  select_dataset("ADES")

# Load source datasets ----

# Use e.g. haven::read_sas to read in .sas7bdat, or other suitable functions
# as needed and assign to the variables below.
# For illustration purposes read in admiral test data

# Use haven::read_sas() or similar to read in production data
# For illustration, using pharmaverseadam example data
library(pharmaverseadam)

adsl <- pharmaverseadam::adsl
adae <- pharmaverseadam::adae
adlb <- pharmaverseadam::adlb
advs <- pharmaverseadam::advs

adpp <- pharmaverseadam::adpp

# ---- Prepare adsl - add derived variables

# Ensure TRT01P/TRT01A exist
if (!"TRT01P" %in% names(adsl)) {
  adsl <- adsl %>% mutate(TRT01P = ARM)
}
if (!"TRT01A" %in% names(adsl)) {
  adsl <- adsl %>% mutate(TRT01A = ACTARM)
}

# Create numeric treatment variables
adsl <- adsl %>%
  mutate(
    TRT01PN = case_when(
      TRT01P == "Placebo" ~ 0,
      TRT01P == "Xanomeline Low Dose" ~ 1,
      TRT01P == "Xanomeline High Dose" ~ 2,
      TRUE ~ 3
    ),
    TRT01AN = case_when(
      TRT01A == "Placebo" ~ 0,
      TRT01A == "Xanomeline Low Dose" ~ 1,
      TRT01A == "Xanomeline High Dose" ~ 2,
      TRUE ~ 3
    )
  )

# ---- Derive Baseline Covariates

## Numeric identifiers and demographics ----

adsl_cov <- adsl %>%
  mutate(
    # Study identifiers (numeric)
    STUDYIDN = as.numeric(word(USUBJID, 1, sep = fixed("-"))),
    SITEIDN = as.numeric(word(USUBJID, 2, sep = fixed("-"))),
    USUBJIDN = as.numeric(word(USUBJID, 3, sep = fixed("-"))),
    SUBJIDN = as.numeric(SUBJID),

    # Demographics (numeric)
    SEXN = case_when(SEX == "M" ~ 1, SEX == "F" ~ 2, TRUE ~ 3),
    RACEN = case_when(
      RACE == "AMERICAN INDIAN OR ALASKA NATIVE" ~ 1,
      RACE == "ASIAN" ~ 2,
      RACE == "BLACK OR AFRICAN AMERICAN" ~ 3,
      RACE == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ 4,
      RACE == "WHITE" ~ 5,
      TRUE ~ 6
    ),
    ETHNICN = case_when(
      ETHNIC == "HISPANIC OR LATINO" ~ 1,
      ETHNIC == "NOT HISPANIC OR LATINO" ~ 2,
      TRUE ~ 3
    ),

    # Age groups
    AGEGR1 = case_when(
      AGE < 65 ~ "<65",
      AGE >= 65 & AGE < 75 ~ "65-75",
      AGE >= 75 ~ ">75",
      TRUE ~ NA_character_
    ),
    AGEGR1N = case_when(
      AGE < 65 ~ 1,
      AGE >= 65 & AGE < 75 ~ 2,
      AGE >= 75 ~ 3,
      TRUE ~ NA_real_
    ),

    # Treatment (numeric)
    ARMN = case_when(
      ARM == "Placebo" ~ 0,
      ARM == "Xanomeline Low Dose" ~ 1,
      ARM == "Xanomeline High Dose" ~ 2,
      TRUE ~ 3
    ),
    ACTARMN = case_when(
      ACTARM == "Placebo" ~ 0,
      ACTARM == "Xanomeline Low Dose" ~ 1,
      ACTARM == "Xanomeline High Dose" ~ 2,
      TRUE ~ 3
    )
  )

## Add baseline vitals ----

adsl_vs <- adsl_cov %>%
  derive_vars_merged(
    dataset_add = advs,
    filter_add = PARAMCD == "HEIGHT" & ABLFL == "Y",
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(HTBL = AVAL)
  ) %>%
  derive_vars_merged(
    dataset_add = advs,
    filter_add = PARAMCD == "WEIGHT" & ABLFL == "Y",
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(WTBL = AVAL)
  ) %>%
  mutate(
    BMIBL = compute_bmi(height = HTBL, weight = WTBL),
    BSABL = compute_bsa(height = HTBL, weight = WTBL, method = "Mosteller"),
    WTBLGR1 = case_when(
      WTBL < 70 ~ "<70 kg",
      WTBL >= 70 ~ ">=70 kg",
      TRUE ~ NA_character_
    )
  )

## Add baseline labs ----

labs_bl <- adlb %>%
  filter(ABLFL == "Y" & PARAMCD %in% c("CREAT", "ALT", "AST", "BILI", "ALB")) %>%
  mutate(PARAMCDB = paste0(PARAMCD, "BL")) %>%
  select(STUDYID, USUBJID, PARAMCDB, AVAL)

adsl_vslb <- adsl_vs %>%
  derive_vars_transposed(
    dataset_merge = labs_bl,
    by_vars = exprs(STUDYID, USUBJID),
    key_var = PARAMCDB,
    value_var = AVAL
  ) %>%
  mutate(
    TBILBL = BILIBL,
    CRCLBL = compute_egfr(
      creat = CREATBL, creatu = "SI", age = AGE,
      weight = WTBL, sex = SEX, method = "CRCL"
    ),
    EGFRBL = compute_egfr(
      creat = CREATBL, creatu = "SI", age = AGE,
      weight = WTBL, sex = SEX, method = "CKD-EPI"
    )
  ) %>%
  select(-BILIBL)

# ---- Derive Exposure Metrics

exposure_final <- adsl_vslb %>%
  derive_vars_transposed(
    dataset_merge = adpp,
    filter = PARAMCD %in% c("AUCLST", "CMAX"),
    by_vars = get_admiral_option("subject_keys"),
    key_var = PARAMCD,
    value_var = AVAL
  ) %>%
  rename(AUCSS = AUCLST, CMAXSS = CMAX)

# ---- Create ades base dataset

# Derive subject-level summary parameters from ADAE
# NOTE: pharmaverseadam uses ASEV/ASEVN (severity) not AETOXGR/AETOXGRN

## Any TEAE (Treatment-Emergent Adverse Event) ----
teae_any <- adsl %>%
  select(STUDYID, USUBJID) %>%
  mutate(
    PARAMCD = "TEAE",
    PARAM = "Any Treatment-Emergent Adverse Event",
    PARAMN = 1,
    AVAL = if_else(
      USUBJID %in% (adae %>% filter(TRTEMFL == "Y") %>% pull(USUBJID)),
      1, 0
    ),
    AVALC = if_else(AVAL == 1, "Y", "N")
  )

## Any Severe TEAE (using ASEVN = 3 for SEVERE) ----
teae_sev <- adsl %>%
  select(STUDYID, USUBJID) %>%
  mutate(
    PARAMCD = "TEAESEV", # Using ASEVN instead of AETOXGRN
    PARAM = "Any Severe Treatment-Emergent Adverse Event",
    PARAMN = 2,
    AVAL = if_else(
      USUBJID %in% (adae %>% filter(TRTEMFL == "Y" & ASEVN == 3) %>% pull(USUBJID)),
      1, 0
    ),
    AVALC = if_else(AVAL == 1, "Y", "N")
  )

## Any Serious AE ----
sae_any <- adsl %>%
  select(STUDYID, USUBJID) %>%
  mutate(
    PARAMCD = "SAE",
    PARAM = "Any Serious Adverse Event",
    PARAMN = 3,
    AVAL = if_else(
      USUBJID %in% (adae %>% filter(AESER == "Y") %>% pull(USUBJID)),
      1, 0
    ),
    AVALC = if_else(AVAL == 1, "Y", "N")
  )

## Any Treatment-Related AE (using AREL character values) ----
trae_any <- adsl %>%
  select(STUDYID, USUBJID) %>%
  mutate(
    PARAMCD = "TRAE",
    PARAM = "Any Treatment-Related Adverse Event",
    PARAMN = 4,
    AVAL = if_else(
      USUBJID %in% (adae %>%
        filter(AREL %in% c("POSSIBLE", "PROBABLE", "RELATED")) %>%
        pull(USUBJID)),
      1, 0
    ),
    AVALC = if_else(AVAL == 1, "Y", "N")
  )

## AE Leading to Treatment Discontinuation ----
aedcn <- adsl %>%
  select(STUDYID, USUBJID) %>%
  mutate(
    PARAMCD = "AEDCN",
    PARAM = "AE Leading to Treatment Discontinuation",
    PARAMN = 5,
    AVAL = if_else(
      USUBJID %in% (adae %>% filter(AEACN == "DRUG WITHDRAWN") %>% pull(USUBJID)),
      1, 0
    ),
    AVALC = if_else(AVAL == 1, "Y", "N")
  )

# Combine subject-level parameters
subject_params <- bind_rows(
  teae_any,
  teae_sev,
  sae_any,
  trae_any,
  aedcn
)

# ---- Create event level parameters

# Get variable names for clean dropping
adsl_vars <- names(exposure_final)
adae_vars <- names(adae)
common_vars <- intersect(adsl_vars, adae_vars)
vars_to_drop <- setdiff(common_vars, c("STUDYID", "USUBJID"))

# Create event-level records from ADAE
# NOTE: Using actual pharmaverseadam variables (ASEV/ASEVN, AREL)
event_params <- adae %>%
  filter(TRTEMFL == "Y") %>% # Treatment-emergent only
  mutate(
    PARAMCD = "AETERM",
    PARAM = "Adverse Event Term",
    PARAMN = 10,
    AVAL = 1, # Event occurred
    AVALC = "Y",

    # Keep AE-specific variables (8-char names)
    # Using actual pharmaverseadam ADAE variables
    AEDECOD = AEDECOD, # Preferred term
    AEBODSYS = AEBODSYS, # System organ class
    AESEV = ASEV, # Severity (char): MILD, MODERATE, SEVERE
    AESEVN = ASEVN, # Severity (num): 1, 2, 3
    AESER = AESER, # Serious flag: Y/N
    AEREL = AREL, # Relationship (char): NOT RELATED, POSSIBLE, etc.

    # Create numeric relationship for analysis
    AERELN = case_when(
      AREL == "NOT RELATED" ~ 0,
      AREL == "UNLIKELY RELATED" ~ 1,
      AREL == "POSSIBLE" ~ 2,
      AREL == "PROBABLE" ~ 3,
      AREL == "RELATED" ~ 4,
      TRUE ~ NA_real_
    ),
    AESTDT = ASTDT, # AE start date (8 chars)
    AEENDT = AENDT # AE end date (8 chars)
  ) %>%
  select(-any_of(vars_to_drop))

# ---- Combine subject and event levels

# Ensure all AE-specific variables exist in subject_params (as NA)
# This prevents issues when binding with event_params
subject_params_complete <- subject_params %>%
  derive_vars_merged(
    dataset_add = exposure_final,
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  mutate(
    # Add event-level variables as NA for subject-level records
    AESTDT = as.Date(NA),
    AEENDT = as.Date(NA),
    AEDECOD = NA_character_,
    AEBODSYS = NA_character_,
    AESEV = NA_character_,
    AESEVN = NA_integer_,
    AESER = NA_character_,
    AEREL = NA_character_,
    AERELN = NA_real_
  )

event_params_complete <- event_params %>%
  derive_vars_merged(
    dataset_add = exposure_final,
    by_vars = exprs(STUDYID, USUBJID)
  )

# Combine both levels
ades_base <- bind_rows(
  subject_params_complete,
  event_params_complete
)

# ---- Add analysis variables

ades_prefinal <- ades_base %>%
  # Analysis flags
  mutate(
    ANL01FL = if_else(PARAMCD == "TEAE", "Y", ""),
    ANL02FL = if_else(PARAMCD == "TEAESEV", "Y", ""),
    ANL03FL = if_else(PARAMCD == "SAE", "Y", ""),
    ANL04FL = if_else(PARAMCD == "TRAE", "Y", ""),
    ANL05FL = if_else(PARAMCD == "AETERM", "Y", "")
  ) %>%
  # Parameter categories
  mutate(
    PARCAT1 = "SAFETY",
    PARCAT2 = case_when(
      PARAMN <= 5 ~ "SUBJECT-LEVEL",
      PARAMN >= 10 ~ "EVENT-LEVEL",
      TRUE ~ NA_character_
    )
  ) %>%
  # Analysis timepoint
  mutate(
    AVISIT = if_else(PARAMN <= 5, "OVERALL", "AT EVENT"),
    AVISITN = if_else(PARAMN <= 5, 99, 0)
  ) %>%
  # Sort and create sequence number
  # Use coalesce to handle NA dates (puts them first in sort)
  arrange(USUBJID, PARAMN, coalesce(AESTDT, as.Date("1900-01-01"))) %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup() 

## Check Data With metacore and metatools

ades <- ades_prefinal %>%
  drop_unspec_vars(metacore) %>% # Drop unspecified variables from specs
  check_variables(metacore, strict = FALSE) %>% # Check all variables specified are present and no more
  check_ct_data(metacore) %>% # Checks all variables with CT only contain values within the CT
  order_cols(metacore) %>% # Orders the columns according to the spec
  sort_by_key(metacore) # Sorts the rows by the sort keys

dir <- "adam" # Change to whichever directory you want to save the dataset in

ades_xpt <- ades %>%
  xportr_type(metacore, domain = "ADES") %>% # Coerce variable type to match spec
  xportr_length(metacore) %>% # Assigns SAS length from a variable level metadata
  xportr_label(metacore) %>% # Assigns variable label from metacore specifications
  xportr_format(metacore) %>% # Assigns variable format from metacore specifications
  xportr_df_label(metacore, domain = "ADES") %>% # Assigns dataset label from metacore specifications
  xportr_write(file.path(dir, "ades.xpt")) # Write xpt v5 transport file

# Save output ----

save(ades, file = file.path(dir, "ades.rda"), compress = "bzip2")
