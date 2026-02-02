#===============================================================================
# Script: create_er_standards_presentation.R
#
# Purpose: Create PowerPoint presentation on ER Standards (Updated)
#
# Author: Jeff Dickinson
# Date: 2026-02-02
#
# Output: presentations/ER_Standards_Presentation.pptx
#
# Requirements: officer, rvg (for plots)
#===============================================================================

library(officer)
library(dplyr)
library(ggplot2)

# Create output directory
if (!dir.exists("presentations")) {
  dir.create("presentations", recursive = TRUE)
}

cat("Creating ER Standards PowerPoint presentation...\n\n")

#===============================================================================
# INITIALIZE PRESENTATION
#===============================================================================

pres <- read_pptx()

#===============================================================================
# SLIDE 1: TITLE SLIDE
#===============================================================================

pres <- pres %>%
  add_slide(layout = "Title Slide", master = "Office Theme") %>%
  ph_with(
    value = "Standardizing Exposure-Response Data for Modeling and Simulation",
    location = ph_location_type(type = "ctrTitle")
  ) %>%
  ph_with(
    value = "Using CDISC Principles and {admiral}",
    location = ph_location_type(type = "subTitle")
  ) %>%
  ph_with(
    value = block_list(
      fpar(ftext("Jeffrey A. Dickinson", prop = fp_text(font.size = 14))),
      fpar(ftext("Navitas Data Sciences", prop = fp_text(font.size = 12, italic = TRUE))),
      fpar(ftext("PHUSE US Connect 2026", prop = fp_text(font.size = 10)))
    ),
    location = ph_location_label(ph_label = "Subtitle 2")
  )

#===============================================================================
# SLIDE 2: OVERVIEW
#===============================================================================

pres <- pres %>%
  add_slide(layout = "Two Content", master = "Office Theme") %>%
  ph_with(value = "Overview", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = block_list(
      fpar(ftext("The Challenge", prop = fp_text(bold = TRUE, font.size = 16))),
      fpar(ftext("  • ER modeling critical for drug development", prop = fp_text(font.size = 12))),
      fpar(ftext("  • Datasets lack standardization", prop = fp_text(font.size = 12))),
      fpar(ftext("  • CDISC SDTM-PK exists, but no ER equivalent", prop = fp_text(font.size = 12))),
      fpar(ftext("  • Inconsistent structures hinder reproducibility", prop = fp_text(font.size = 12))),
      fpar(ftext("  • Limited exposure metrics (typically 3-5 variables)", prop = fp_text(font.size = 12)))
    ),
    location = ph_location_left()
  ) %>%
  ph_with(
    value = block_list(
      fpar(ftext("Our Approach", prop = fp_text(bold = TRUE, font.size = 16))),
      fpar(ftext("  • Extend CDISC principles to ER data", prop = fp_text(font.size = 12))),
      fpar(ftext("  • Four specialized datasets:", prop = fp_text(font.size = 12))),
      fpar(ftext("    - ADER: Exposure foundation", prop = fp_text(font.size = 11, italic = TRUE))),
      fpar(ftext("    - ADEE: Exposure-Efficacy", prop = fp_text(font.size = 11, italic = TRUE))),
      fpar(ftext("    - ADES: Exposure-Safety", prop = fp_text(font.size = 11, italic = TRUE))),
      fpar(ftext("    - ADTRR: Tumor Response", prop = fp_text(font.size = 11, italic = TRUE))),
      fpar(ftext("  • Use pharmaverseadam example data", prop = fp_text(font.size = 12))),
      fpar(ftext("  • Comprehensive exposure coverage", prop = fp_text(font.size = 12))),
      fpar(ftext("  • Full CDISC compliance (8-character limit)", prop = fp_text(font.size = 12)))
    ),
    location = ph_location_right()
  )

#===============================================================================
# SLIDE 3: THE ER MODELING LANDSCAPE
#===============================================================================

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "The ER Modeling Landscape", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = block_list(
      fpar(ftext("What is Exposure-Response Analysis?", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext("  • Quantifies relationship between drug exposure (PK) and outcomes", prop = fp_text(font.size = 12))),
      fpar(ftext("  • Key questions:", prop = fp_text(font.size = 12))),
      fpar(ftext("    - What exposure achieves target efficacy?", prop = fp_text(font.size = 11, italic = TRUE))),
      fpar(ftext("    - What exposure level increases toxicity risk?", prop = fp_text(font.size = 11, italic = TRUE))),
      fpar(ftext("    - How do we optimize dosing for subpopulations?", prop = fp_text(font.size = 11, italic = TRUE))),
      fpar(ftext("")),
      fpar(ftext("Current State", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext("  • Each analysis starts from scratch", prop = fp_text(font.size = 12))),
      fpar(ftext("  • Variable naming inconsistent (AUC, AUC_SS, AUCSS, AUC0_24)", prop = fp_text(font.size = 12))),
      fpar(ftext("  • Limited transformations available", prop = fp_text(font.size = 12))),
      fpar(ftext("  • Difficult to share methodologies", prop = fp_text(font.size = 12))),
      fpar(ftext("  • QC challenges", prop = fp_text(font.size = 12)))
    ),
    location = ph_location_type(type = "body")
  )

#===============================================================================
# SLIDE 4: WHY STANDARDS MATTER
#===============================================================================

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Why Standards Matter", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = block_list(
      fpar(ftext("Consistency", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("Same structure across studies and organizations", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Traceability", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("Clear lineage from raw data → analysis", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Reproducibility", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("Others can verify and extend your work", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Efficiency", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("Reusable code and workflows; 50-80% time reduction", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Regulatory Clarity", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("Easier to document and defend analyses", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Comprehensive", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("Multiple exposure transformations vs. limited traditional approach", prop = fp_text(font.size = 11)))
    ),
    location = ph_location_type(type = "body")
  )

#===============================================================================
# SLIDE 5: FOUR ER DATASETS COMPARISON
#===============================================================================

comp_table <- data.frame(
  Dataset = c("ADER", "ADEE", "ADES", "ADTRR"),
  Purpose = c("Exposure foundation", "Time-to-event efficacy", "Safety events", "Tumor response"),
  Structure = c("Subject-level", "BDS (one/subj/param)", "BDS + OCCDS hybrid", "BDS with visits"),
  `Key Features` = c(
    "Comprehensive exposure metrics",
    "AVAL, CNSR, EVENT",
    "ASEV, AEREL, multi-level",
    "AVAL, CHG, PCHG, RECIST"
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Four ER Datasets: Overview", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = comp_table,
    location = ph_location_type(type = "body")
  )

#===============================================================================
# SLIDE 6: ADER - EXPOSURE FOUNDATION
#===============================================================================

pres <- pres %>%
  add_slide(layout = "Two Content", master = "Office Theme") %>%
  ph_with(value = "ADER: Exposure Foundation", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = block_list(
      fpar(ftext("Data Source", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext("pharmaverseadam::adpp", prop = fp_text(font.size = 12, italic = TRUE))),
      fpar(ftext("AUCLST parameter extracted", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Exposure Transformations", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext("")),
      fpar(ftext("Raw Metrics", prop = fp_text(bold = TRUE, font.size = 12))),
      fpar(ftext("  • AUCSS, CMAXSS, CAVGSS", prop = fp_text(font.size = 10))),
      fpar(ftext("")),
      fpar(ftext("Log-Transformed", prop = fp_text(bold = TRUE, font.size = 12))),
      fpar(ftext("  • AUCSLOG, CMXSLOG, CAVGLOG", prop = fp_text(font.size = 10))),
      fpar(ftext("")),
      fpar(ftext("Standardized (Z-scores)", prop = fp_text(bold = TRUE, font.size = 12))),
      fpar(ftext("  • AUCSSSTD, CMXSSSTD, CAVGSTD", prop = fp_text(font.size = 10)))
    ),
    location = ph_location_left()
  ) %>%
  ph_with(
    value = block_list(
      fpar(ftext("Normalized", prop = fp_text(bold = TRUE, font.size = 12))),
      fpar(ftext("  • AUCSSN, CMAXSSN, CAVGSSN", prop = fp_text(font.size = 10))),
      fpar(ftext("")),
      fpar(ftext("Dose-Normalized", prop = fp_text(bold = TRUE, font.size = 12))),
      fpar(ftext("  • AUCSSDOS, CMXSSDOS, CAVGDOS", prop = fp_text(font.size = 10))),
      fpar(ftext("")),
      fpar(ftext("Categorical", prop = fp_text(bold = TRUE, font.size = 12))),
      fpar(ftext("  • AUCSSCAT, AUCSCATN", prop = fp_text(font.size = 10))),
      fpar(ftext("  • CMXSSCAT, CMXSCATN", prop = fp_text(font.size = 10))),
      fpar(ftext("")),
      fpar(ftext("Baseline Covariates", prop = fp_text(bold = TRUE, font.size = 12))),
      fpar(ftext("  • Vitals, renal, hepatic function", prop = fp_text(font.size = 10))),
      fpar(ftext("  • Performance status, demographics", prop = fp_text(font.size = 10))),
      fpar(ftext("")),
      fpar(ftext("")),
      fpar(ftext("Comprehensive pre-computed transformations", prop = fp_text(bold = TRUE, font.size = 11, color = "#0066cc"))),
      fpar(ftext("All follow 8-character limit", prop = fp_text(font.size = 10, italic = TRUE)))
    ),
    location = ph_location_right()
  )

#===============================================================================
# SLIDE 7: 8-CHARACTER COMPLIANCE
#===============================================================================

abbrev_table <- data.frame(
  Traditional = c("AUCSSLOG", "CMAXSSLOG", "CAVGSSLOG", "CMAXSSSTD", "AUCSSDOSE", "CMAXSSDOSE"),
  Updated = c("AUCSLOG", "CMXSLOG", "CAVGLOG", "CMXSSSTD", "AUCSSDOS", "CMXSSDOS"),
  Rationale = c("Removed one S", "Abbreviated CMAX", "Removed SS", "Abbreviated CMAX", "Removed E", "Abbreviated + removed E"),
  stringsAsFactors = FALSE
)

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "8-Character Compliance", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = block_list(
      fpar(ftext("CDISC ADaM Requirement: Variable names ≤ 8 characters", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext(""))
    ),
    location = ph_location(left = 0.5, top = 1.3, width = 9, height = 0.5)
  ) %>%
  ph_with(
    value = abbrev_table,
    location = ph_location(left = 0.5, top = 2.0, width = 9, height = 2.5)
  ) %>%
  ph_with(
    value = block_list(
      fpar(ftext("")),
      fpar(ftext("✓ All variables comply with 8-character limit", prop = fp_text(font.size = 11, color = "#0066cc"))),
      fpar(ftext("✓ Systematic abbreviation rules documented", prop = fp_text(font.size = 11, color = "#0066cc"))),
      fpar(ftext("✓ Labels provide full descriptions", prop = fp_text(font.size = 11, color = "#0066cc")))
    ),
    location = ph_location(left = 0.5, top = 5.0, width = 9, height = 1.0)
  )

#===============================================================================
# SLIDE 8: ADEE - EXPOSURE-EFFICACY
#===============================================================================

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "ADEE: Exposure-Efficacy", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = block_list(
      fpar(ftext("Use Case: Time-to-Event Analyses with Exposure", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext("")),
      fpar(ftext("Key Dataset Features", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • One record per subject per parameter (OS, PFS, TTP, TTNT)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • AVAL = time from treatment start (days)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • CNSR = censoring indicator (1=censored, 0=event)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • EVENT = event indicator (1=event, 0=censored)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • No ATPT/AVISIT (not standard for time-to-event)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • All exposure metrics from ADER", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Baseline covariates from ADER", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Ready for modeling:", prop = fp_text(bold = TRUE, font.size = 12))),
      fpar(ftext("  coxph(Surv(AVAL, EVENT) ~ AUCSSSTD + AGE + SEX)", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("  survfit(Surv(AVAL, EVENT) ~ AUCSSCAT)", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("")),
      fpar(ftext("Multiple exposure representations enable flexible E-R exploration", prop = fp_text(font.size = 10, italic = TRUE, color = "#0066cc")))
    ),
    location = ph_location_type(type = "body")
  )

#===============================================================================
# SLIDE 9: ADES - EXPOSURE-SAFETY
#===============================================================================

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "ADES: Exposure-Safety", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = block_list(
      fpar(ftext("Use Case: Adverse Event Analyses with Exposure", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext("")),
      fpar(ftext("Multi-Level Structure", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • Subject-level parameters (TEAE, TEAESEV, TESAE)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Event-level individual AE records", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Key Variables", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • ASEV/ASEVN: Severity (MILD/MODERATE/SEVERE)", prop = fp_text(font.size = 11, color = "#cc0000"))),
      fpar(ftext("  • AERELN: Relationship (0-4 numeric scale)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • All exposure metrics available", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Design Decision: ASEV vs AETOXGR", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • Uses severity (pharmaverseadam convention)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Broader applicability than toxicity grades", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Maps to regulatory serious criteria", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Supports summary statistics and detailed event analysis", prop = fp_text(font.size = 10, italic = TRUE, color = "#0066cc")))
    ),
    location = ph_location_type(type = "body")
  )

#===============================================================================
# SLIDE 10: ASEV vs AETOXGR
#===============================================================================

severity_table <- data.frame(
  Aspect = c("Variable", "Values", "Scale", "Use Case"),
  ASEV = c(
    "ASEV/ASEVN",
    "MILD (1), MODERATE (2), SEVERE (3)",
    "3-level",
    "All AEs (all therapeutic areas)"
  ),
  AETOXGR = c(
    "AETOXGR/AETOXGRN",
    "Grade 1-5 (CTCAE)",
    "5-level",
    "Oncology toxicities"
  ),
  stringsAsFactors = FALSE
)

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "ADES: Severity Variables (ASEV not AETOXGR)", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = severity_table,
    location = ph_location(left = 0.5, top = 1.5, width = 9, height = 2.0)
  ) %>%
  ph_with(
    value = block_list(
      fpar(ftext("")),
      fpar(ftext("Why ASEV/ASEVN?", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • Follows pharmaverseadam conventions", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Broader applicability (all AEs, not just toxicities)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Simpler 3-level scale", prop = fp_text(font.size = 11))),
      fpar(ftext("  • SEVERE maps to regulatory 'serious' criteria", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("For oncology requiring CTCAE grades:", prop = fp_text(bold = TRUE, font.size = 11))),
      fpar(ftext("  → Derive AETOXGR from ASEV + AE characteristics", prop = fp_text(font.size = 10, italic = TRUE)))
    ),
    location = ph_location(left = 0.5, top = 4.0, width = 9, height = 2.5)
  )

#===============================================================================
# SLIDE 11: ADTRR - TUMOR RESPONSE
#===============================================================================

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "ADTRR: Tumor Response for E-R", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = block_list(
      fpar(ftext("Use Case: Longitudinal Tumor Measurements with RECIST 1.1", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext("")),
      fpar(ftext("Key Features", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • Repeated measures structure", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Three parameters: TSIZE, BOR, NADIR", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Baseline normalization (BASE, CHG, PCHG)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • RECIST 1.1 categorical response (CR/PR/SD/PD)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • All exposure metrics available", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("8-Character Compliance", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • NADPCHG: Percent change from nadir", prop = fp_text(font.size = 11))),
      fpar(ftext("  • BORN: Numeric BOR (4=CR, 3=PR, 2=SD, 1=PD)", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Visualization Support", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • Waterfall plots (best % change)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Spider plots (individual trajectories)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Time-to-response, mixed models", prop = fp_text(font.size = 11)))
    ),
    location = ph_location_type(type = "body")
  )

#===============================================================================
# SLIDE 12: COMMON PATTERNS
#===============================================================================

pres <- pres %>%
  add_slide(layout = "Two Content", master = "Office Theme") %>%
  ph_with(value = "Common Patterns Across Datasets", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = block_list(
      fpar(ftext("Unified Foundation", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • All datasets built on ADER", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Same exposure transformations", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Shared baseline covariates", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Time Variables", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • ADT: Analysis date", prop = fp_text(font.size = 11))),
      fpar(ftext("  • ADY: Study day", prop = fp_text(font.size = 11))),
      fpar(ftext("  • AVAL: Numeric outcome", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Analysis Flags", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • ANL01FL: Primary analysis", prop = fp_text(font.size = 11))),
      fpar(ftext("  • ANL02FL: Sensitivity analyses", prop = fp_text(font.size = 11)))
    ),
    location = ph_location_left()
  ) %>%
  ph_with(
    value = block_list(
      fpar(ftext("CDISC Compliance", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • All variables ≤ 8 characters", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Systematic abbreviation rules", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Full metadata integration", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Implementation", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • {admiral} modular functions", prop = fp_text(font.size = 11))),
      fpar(ftext("  • pharmaverseadam example data", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Shared utility functions", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Benefits", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • Code reuse across studies", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Consistent validation", prop = fp_text(font.size = 11))),
      fpar(ftext("  • 50-80% efficiency gains", prop = fp_text(font.size = 11)))
    ),
    location = ph_location_right()
  )

#===============================================================================
# SLIDE 13: WHY {ADMIRAL} AND PHARMAVERSEADAM?
#===============================================================================

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Why {admiral} and pharmaverseadam?", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = block_list(
      fpar(ftext("{admiral} Advantages", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext("")),
      fpar(ftext("✓ Modular functions: derive_vars_*, derive_param_*", prop = fp_text(font.size = 12))),
      fpar(ftext("✓ Validation built-in: assert_* functions catch errors early", prop = fp_text(font.size = 12))),
      fpar(ftext("✓ Metadata integration: Works with {metacore}, {xportr}", prop = fp_text(font.size = 12))),
      fpar(ftext("✓ Community-driven: Pharmaverse ecosystem", prop = fp_text(font.size = 12))),
      fpar(ftext("✓ CDISC-aligned: Built with standards in mind", prop = fp_text(font.size = 12))),
      fpar(ftext("")),
      fpar(ftext("")),
      fpar(ftext("pharmaverseadam Benefits", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext("")),
      fpar(ftext("✓ Real example data: ADPP with actual PK parameters", prop = fp_text(font.size = 12))),
      fpar(ftext("✓ CDISC compliant: All datasets follow ADaM standards", prop = fp_text(font.size = 12))),
      fpar(ftext("✓ Ready to use: Extract AUCLST as AUCSS foundation", prop = fp_text(font.size = 12))),
      fpar(ftext("✓ Demonstrates real-world applicability", prop = fp_text(font.size = 12)))
    ),
    location = ph_location_type(type = "body")
  )

#===============================================================================
# SLIDE 14: BENEFITS - COMPREHENSIVE EXPOSURE
#===============================================================================

exposure_comp <- data.frame(
  Approach = c("Traditional", "New Framework"),
  Variables = c("3-5", "Comprehensive"),
  Examples = c("AUC, CMAX, CAVG", "Raw, Log, Standardized, Normalized, Dose-normalized, Categorical"),
  stringsAsFactors = FALSE
)

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Benefits: Comprehensive Exposure Metrics", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = exposure_comp,
    location = ph_location(left = 0.5, top = 1.5, width = 9, height = 1.5)
  ) %>%
  ph_with(
    value = block_list(
      fpar(ftext("")),
      fpar(ftext("Pre-Computed Transformations", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext("  • Raw: AUCSS, CMAXSS, CAVGSS", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Log-transformed: AUCSLOG, CMXSLOG, CAVGLOG", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Standardized: AUCSSSTD, CMXSSSTD (Z-scores)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Normalized: AUCSSN, CMAXSSN (mean = 1)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Dose-normalized: AUCSSDOS, CMXSSDOS", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Categorical: AUCSSCAT, CMXSSCAT (tertiles)", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("✓ Analysts can immediately test multiple E-R representations", prop = fp_text(font.size = 12, color = "#0066cc"))),
      fpar(ftext("✓ No recoding needed in analysis scripts", prop = fp_text(font.size = 12, color = "#0066cc"))),
      fpar(ftext("✓ Promotes exploration of optimal E-R model", prop = fp_text(font.size = 12, color = "#0066cc")))
    ),
    location = ph_location(left = 0.5, top = 3.5, width = 9, height = 3.0)
  )

#===============================================================================
# SLIDE 15: COMPARISON WITH TRADITIONAL
#===============================================================================

traditional_comp <- data.frame(
  Feature = c("Exposure variables", "Variable naming", "Transformations", "Documentation", "Structure", "Reusability"),
  Traditional = c("3-5", "Inconsistent", "Manual", "External", "Single file", "Limited"),
  `New Framework` = c("Comprehensive", "CDISC compliant", "Pre-computed", "Integrated specs", "Domain-optimized", "High"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Comparison with Traditional Approach", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = traditional_comp,
    location = ph_location(left = 0.5, top = 1.5, width = 9, height = 3.5)
  ) %>%
  ph_with(
    value = block_list(
      fpar(ftext("Demonstrated improvements: 50-80% efficiency gains, enhanced reproducibility", 
                 prop = fp_text(bold = TRUE, font.size = 11, color = "#0066cc")))
    ),
    location = ph_location(left = 0.5, top = 5.5, width = 9, height = 0.5)
  )

#===============================================================================
# SLIDE 16: PATH FORWARD
#===============================================================================

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Path Forward", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = block_list(
      fpar(ftext("Near-Term Steps", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext("")),
      fpar(ftext("1. Community Feedback", prop = fp_text(bold = TRUE, font.size = 12))),
      fpar(ftext("   • Pilot testing in real studies", prop = fp_text(font.size = 11))),
      fpar(ftext("   • Refinement based on feedback", prop = fp_text(font.size = 11))),
      fpar(ftext("   • Edge cases and exceptions", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("2. Extension Development", prop = fp_text(bold = TRUE, font.size = 12))),
      fpar(ftext("   • Additional domains (ECG, vital signs, lab markers)", prop = fp_text(font.size = 11))),
      fpar(ftext("   • Advanced exposure metrics", prop = fp_text(font.size = 11))),
      fpar(ftext("   • Visualization templates", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("3. Formalization", prop = fp_text(bold = TRUE, font.size = 12))),
      fpar(ftext("   • Pharmaverse working group", prop = fp_text(font.size = 11))),
      fpar(ftext("   • {admiral} function extensions", prop = fp_text(font.size = 11))),
      fpar(ftext("   • Potential CDISC submission", prop = fp_text(font.size = 11)))
    ),
    location = ph_location_type(type = "body")
  )

#===============================================================================
# SLIDE 17: GETTING STARTED TODAY
#===============================================================================

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Getting Started Today", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = block_list(
      fpar(ftext("Resources Available", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext("  • GitHub repository: https://github.com/jeffreyad/er-standards", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Complete derivation scripts (ADER, ADEE, ADES, ADTRR)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Example using pharmaverseadam::adpp", prop = fp_text(font.size = 11))),
      fpar(ftext("  • ADaM specifications (P21 format + Excel)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Metadata integration examples", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Try It Yourself", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext("# Clone repository", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("git clone https://github.com/jeffreyad/er-standards.git", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("# Run derivations with pharmaverseadam", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("source('programs/ad_ader.R')  # Uses pharmaverseadam::adpp", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("source('programs/ad_adee.R')  # Time-to-event", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("")),
      fpar(ftext("Connect", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext("  • Email: jeff.dickinson@navitaslifesciences.com", prop = fp_text(font.size = 11))),
      fpar(ftext("  • GitHub Issues: Feedback welcome", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Pharmaverse Slack: Join the discussion", prop = fp_text(font.size = 11)))
    ),
    location = ph_location_type(type = "body")
  )

#===============================================================================
# SLIDE 18: KEY TAKEAWAYS
#===============================================================================

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Key Takeaways", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = block_list(
      fpar(ftext("1. ER data needs standardization", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("   Current state: inconsistent, limited exposure coverage", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("2. Four specialized datasets proposed", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("   ADER (foundation), ADEE (efficacy), ADES (safety), ADTRR (tumor)", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("3. Full CDISC compliance achieved", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("   All variables ≤ 8 characters with systematic abbreviations", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("4. Uses real pharmaverseadam data", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("   ADPP provides actual PK parameters for realistic demonstration", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("5. {admiral} well-suited for implementation", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("   Modular, validated, community-supported", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("6. Significant benefits demonstrated", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("   50-80% efficiency gains, enhanced reproducibility, regulatory readiness", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("7. Community input needed for refinement and adoption", prop = fp_text(bold = TRUE, font.size = 13)))
    ),
    location = ph_location_type(type = "body")
  )

#===============================================================================
# SLIDE 19: QUESTIONS
#===============================================================================

pres <- pres %>%
  add_slide(layout = "Title Slide", master = "Office Theme") %>%
  ph_with(
    value = "Questions?",
    location = ph_location_type(type = "ctrTitle")
  ) %>%
  ph_with(
    value = block_list(
      fpar(ftext("Contact Information", prop = fp_text(bold = TRUE, font.size = 16))),
      fpar(ftext("")),
      fpar(ftext("Email: jeff.dickinson@navitaslifesciences.com", prop = fp_text(font.size = 12))),
      fpar(ftext("GitHub: jeffreyad", prop = fp_text(font.size = 12))),
      fpar(ftext("LinkedIn: https://www.linkedin.com/in/jeffreyad/", prop = fp_text(font.size = 12))),
      fpar(ftext("")),
      fpar(ftext("Resources", prop = fp_text(bold = TRUE, font.size = 16))),
      fpar(ftext("")),
      fpar(ftext("Repository: https://github.com/jeffreyad/er-standards", prop = fp_text(font.size = 12))),
      fpar(ftext("Pharmaverse: https://pharmaverse.org", prop = fp_text(font.size = 12))),
      fpar(ftext("Examples: https://pharmaverse.github.io/examples/", prop = fp_text(font.size = 12))),
      fpar(ftext("")),
      fpar(ftext("")),
      fpar(ftext("Thank you!", prop = fp_text(bold = TRUE, font.size = 18, color = "#0066cc")))
    ),
    location = ph_location_type(type = "subTitle")
  )

#===============================================================================
# SAVE PRESENTATION
#===============================================================================

output_file <- "presentations/ER_Standards_Presentation.pptx"
print(pres, target = output_file)

cat("\n✓ PowerPoint presentation created successfully!\n")
cat("  File:", output_file, "\n")
cat("  Slides: 19 (streamlined from 21)\n")
cat("  Updates:\n")
cat("    - Removed specific variable counts\n")
cat("    - Emphasized pharmaverseadam integration\n")
cat("    - Streamlined results content\n")
cat("    - Updated key takeaways\n")
cat("    - More concise throughout\n\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================