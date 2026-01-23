#===============================================================================
# Script: Update All Specifications
#
# Purpose: Update variable names in all P21 specification files to match
#          implemented code changes (8-char limits, pharmaverseadam compatibility)
#
# Author: Jeff Dickinson
# Date: 2026-01-22
#
#===============================================================================

library(dplyr)
library(readr)

#===============================================================================
# DEFINE VARIABLE NAME MAPPINGS
#===============================================================================

# Common exposure variable name changes (all datasets)
common_exposure_changes <- tribble(
  ~Old_Variable, ~New_Variable, ~Reason,
  "AUCSSLOG",    "AUCSLOG",     "8-char limit: removed one S",
  "CMAXSSLOG",   "CMXSLOG",     "8-char limit: abbreviated CMAX",
  "CAVGSSLOG",   "CAVGLOG",     "8-char limit: removed SS",
  "CMAXSSSTD",   "CMXSSSTD",    "8-char limit: abbreviated CMAX",
  "AUCSSDOSE",   "AUCSSDOS",    "8-char limit: removed E",
  "CMAXSSDOSE",  "CMXSSDOS",    "8-char limit: abbreviated CMAX + removed E",
  "AUCSSCATN",   "AUCSCATN",    "8-char limit: removed one S"
)

# ADES-specific changes
ades_changes <- tribble(
  ~Old_Variable, ~New_Variable, ~Reason,
  "TEAEGR3",     "TEAESEV",     "Changed from grade to severity",
  "AETOXGR",     "AESEV",       "pharmaverseadam uses ASEV not AETOXGR",
  "AETOXGRN",    "AESEVN",      "pharmaverseadam uses ASEVN not AETOXGRN"
)

# ADTRR-specific changes
adtrr_changes <- tribble(
  ~Old_Variable, ~New_Variable, ~Reason,
  "NADIR_PCHG",  "NADPCHG",     "8-char limit",
  "NADIR_VISIT", "NADVST",      "8-char limit"
)

#===============================================================================
# FUNCTION: Update Specification File
#===============================================================================

update_spec_file <- function(file_path, variable_changes) {
  
  cat("\nUpdating:", file_path, "\n")
  
  # Check file exists
  if (!file.exists(file_path)) {
    cat("  ✗ File not found\n")
    return(FALSE)
  }
  
  # Read specification
  spec <- read_csv(file_path, show_col_types = FALSE)
  
  # Track changes
  changes_made <- 0
  
  # Update variable names
  for (i in 1:nrow(variable_changes)) {
    old_var <- variable_changes$Old_Variable[i]
    new_var <- variable_changes$New_Variable[i]
    reason <- variable_changes$Reason[i]
    
    # Check if old variable exists
    if (old_var %in% spec$Variable) {
      spec <- spec %>%
        mutate(
          Variable = if_else(Variable == old_var, new_var, Variable),
          Notes = if_else(
            Variable == new_var & is.na(Notes),
            paste("Updated from", old_var, "-", reason),
            Notes
          )
        )
      changes_made <- changes_made + 1
      cat("  ✓ Changed:", old_var, "→", new_var, "\n")
    }
  }
  
  if (changes_made > 0) {
    # Write updated file
    write_csv(spec, file_path)
    cat("  ✓ Saved with", changes_made, "changes\n")
    return(TRUE)
  } else {
    cat("  ℹ No changes needed\n")
    return(FALSE)
  }
}

#===============================================================================
# FUNCTION: Add New Variables
#===============================================================================

add_new_variables <- function(file_path, new_vars_df) {
  
  cat("\nAdding new variables to:", file_path, "\n")
  
  if (!file.exists(file_path)) {
    cat("  ✗ File not found\n")
    return(FALSE)
  }
  
  spec <- read_csv(file_path, show_col_types = FALSE)
  
  # Check which variables need to be added
  vars_to_add <- new_vars_df %>%
    filter(!Variable %in% spec$Variable)
  
  if (nrow(vars_to_add) > 0) {
    spec <- bind_rows(spec, vars_to_add)
    write_csv(spec, file_path)
    cat("  ✓ Added", nrow(vars_to_add), "new variables\n")
    return(TRUE)
  } else {
    cat("  ℹ All variables already present\n")
    return(FALSE)
  }
}

#===============================================================================
# UPDATE ADER
#===============================================================================

cat(strrep("=", 80), "\n")
cat("UPDATING ADER SPECIFICATIONS\n")
cat(strrep("=", 80), "\n")

ader_updated <- update_spec_file(
  "specifications/ADER_P21_Specifications.csv",
  common_exposure_changes
)

#===============================================================================
# UPDATE ADEE
#===============================================================================

cat(strrep("=", 80), "\n")
cat("UPDATING ADEE SPECIFICATIONS\n")
cat(strrep("=", 80), "\n")

adee_updated <- update_spec_file(
  "specifications/ADEE_P21_Specifications.csv",
  common_exposure_changes
)

#===============================================================================
# UPDATE ADES
#===============================================================================

cat(strrep("=", 80), "\n")
cat("UPDATING ADES SPECIFICATIONS\n")
cat(strrep("=", 80), "\n")

# Combine common and ADES-specific changes
ades_all_changes <- bind_rows(
  common_exposure_changes,
  ades_changes
)

ades_updated <- update_spec_file(
  "specifications/ADES_P21_Specifications.csv",
  ades_all_changes
)

# Add AERELN if missing
ades_new_vars <- tribble(
  ~Variable, ~Label, ~Data_Type, ~Length, ~Core, ~Notes,
  "AERELN", "Relationship to Treatment (N)", "integer", 8, "Perm",
  "Numeric version of AEREL: 0=NOT RELATED, 1=UNLIKELY, 2=POSSIBLE, 3=PROBABLE, 4=RELATED"
)

add_new_variables(
  "specifications/ADES_P21_Specifications.csv",
  ades_new_vars
)

#===============================================================================
# UPDATE ADTRR
#===============================================================================

cat(strrep("=", 80), "\n")
cat("UPDATING ADTRR SPECIFICATIONS\n")
cat(strrep("=", 80), "\n")

# Combine common and ADTRR-specific changes
adtrr_all_changes <- bind_rows(
  common_exposure_changes,
  adtrr_changes
)

adtrr_updated <- update_spec_file(
  "specifications/ADTRR_P21_Specifications.csv",
  adtrr_all_changes
)

# Add new ADTRR variables
adtrr_new_vars <- tribble(
  ~Variable, ~Label, ~Data_Type, ~Length, ~Core, ~Notes,
  "BORN", "Best Overall Response (N)", "integer", 8, "Perm",
  "Numeric: 4=CR, 3=PR, 2=SD, 1=PD, 0=NE",
  "AVALN", "Analysis Value (N)", "integer", 8, "Perm",
  "Numeric version of AVALC for BOR parameter",
  "NADIR", "Nadir Tumor Size", "float", 8, "Perm",
  "Minimum post-baseline tumor size value"
)

add_new_variables(
  "specifications/ADTRR_P21_Specifications.csv",
  adtrr_new_vars
)

#===============================================================================
# SUMMARY
#===============================================================================

cat("\n")
cat(strrep("=", 80), "\n")
cat("SPECIFICATION UPDATE SUMMARY\n")
cat(strrep("=", 80), "\n")

summary_df <- tribble(
  ~Dataset, ~Updated,
  "ADER", ader_updated,
  "ADEE", adee_updated,
  "ADES", ades_updated,
  "ADTRR", adtrr_updated
)

print(summary_df)

cat("\n")
cat("Next steps:\n")
cat("  1. Review updated CSV files in specifications/\n")
cat("  2. Regenerate Excel specs: source('specifications/create_*_spec_excel.R')\n")
cat("  3. Regenerate metacore RDS: metacore <- spec_to_metacore(...)\n")
cat(strrep("=", 80), "\n\n")

#===============================================================================
# GENERATE CHANGE LOG
#===============================================================================

change_log_file <- file.path("specifications", "CHANGELOG.md")

sink(change_log_file)
cat("# Specification Change Log\n\n")
cat("## Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

cat("## Variable Name Changes\n\n")
cat("### Common Changes (All Datasets)\n\n")
cat("| Old Variable | New Variable | Reason |\n")
cat("|--------------|--------------|--------|\n")
for (i in 1:nrow(common_exposure_changes)) {
  cat(sprintf("| %s | %s | %s |\n",
              common_exposure_changes$Old_Variable[i],
              common_exposure_changes$New_Variable[i],
              common_exposure_changes$Reason[i]))
}

cat("\n### ADES-Specific Changes\n\n")
cat("| Old Variable | New Variable | Reason |\n")
cat("|--------------|--------------|--------|\n")
for (i in 1:nrow(ades_changes)) {
  cat(sprintf("| %s | %s | %s |\n",
              ades_changes$Old_Variable[i],
              ades_changes$New_Variable[i],
              ades_changes$Reason[i]))
}

cat("\n### ADTRR-Specific Changes\n\n")
cat("| Old Variable | New Variable | Reason |\n")
cat("|--------------|--------------|--------|\n")
for (i in 1:nrow(adtrr_changes)) {
  cat(sprintf("| %s | %s | %s |\n",
              adtrr_changes$Old_Variable[i],
              adtrr_changes$New_Variable[i],
              adtrr_changes$Reason[i]))
}

cat("\n## New Variables Added\n\n")
cat("### ADES\n\n")
cat("- AERELN: Numeric version of AEREL\n\n")

cat("### ADTRR\n\n")
cat("- BORN: Numeric version of BOR\n")
cat("- AVALN: Numeric analysis value\n")
cat("- NADIR: Nadir tumor size value\n")

sink()

cat("Change log saved to:", change_log_file, "\n\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================