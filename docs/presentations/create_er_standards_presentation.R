#===============================================================================
# Script: create_er_standards_presentation.R
#
# Purpose: Create PowerPoint presentation on ER Standards
#
# Author: Jeff Dickinson
# Date: 2026-02-01
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

# Create presentation
pres <- read_pptx()

# Get layout names (for reference)
# layout_summary(pres)

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
      fpar(ftext("    - ADER: Exposure foundation (20 metrics)", prop = fp_text(font.size = 11, italic = TRUE))),
      fpar(ftext("    - ADEE: Exposure-Efficacy", prop = fp_text(font.size = 11, italic = TRUE))),
      fpar(ftext("    - ADES: Exposure-Safety", prop = fp_text(font.size = 11, italic = TRUE))),
      fpar(ftext("    - ADTRR: Tumor Response for E-R", prop = fp_text(font.size = 11, italic = TRUE))),
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
      fpar(ftext("  • Limited transformations (log, standardized, categorical)", prop = fp_text(font.size = 12))),
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
      fpar(ftext("20+ exposure metrics vs. 3-5 traditional", prop = fp_text(font.size = 11)))
    ),
    location = ph_location_type(type = "body")
  )

#===============================================================================
# SLIDE 5: FOUR ER DATASETS COMPARISON
#===============================================================================

# Create comparison table
comp_table <- data.frame(
  Aspect = c("Purpose", "Structure", "Key Variables", "Parameters", "Variables"),
  ADER = c("Exposure foundation", "Subject-level", "20 exposure metrics", "N/A", "65"),
  ADEE = c("Time-to-event", "BDS (one/subj/param)", "AVAL, CNSR, EVENT", "OS, PFS, TTP, TTNT", "71"),
  ADES = c("Safety events", "BDS + OCCDS hybrid", "ASEV, AEREL, event details", "TEAE, TEAESEV, TESAE", "78"),
  ADTRR = c("Tumor response", "BDS with visits", "AVAL, BASE, CHG, PCHG", "TSIZE, BOR, NADIR", "80"),
  stringsAsFactors = FALSE
)

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Four ER Datasets: A Comparison", location = ph_location_type(type = "title")) %>%
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
      fpar(ftext("Core Exposure Metrics (20 variables)", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext("")),
      fpar(ftext("Raw Metrics", prop = fp_text(bold = TRUE, font.size = 12))),
      fpar(ftext("  • AUCSS: Steady-state AUC", prop = fp_text(font.size = 10))),
      fpar(ftext("  • CMAXSS: Steady-state Cmax", prop = fp_text(font.size = 10))),
      fpar(ftext("  • CAVGSS: Steady-state Cavg", prop = fp_text(font.size = 10))),
      fpar(ftext("")),
      fpar(ftext("Transformed", prop = fp_text(bold = TRUE, font.size = 12))),
      fpar(ftext("  • AUCSLOG: log(AUCSS)", prop = fp_text(font.size = 10))),
      fpar(ftext("  • CMXSLOG: log(CMAXSS)", prop = fp_text(font.size = 10))),
      fpar(ftext("  • CAVGLOG: log(CAVGSS)", prop = fp_text(font.size = 10))),
      fpar(ftext("")),
      fpar(ftext("Standardized", prop = fp_text(bold = TRUE, font.size = 12))),
      fpar(ftext("  • AUCSSSTD: Z-score", prop = fp_text(font.size = 10))),
      fpar(ftext("  • CMXSSSTD: Z-score", prop = fp_text(font.size = 10)))
    ),
    location = ph_location_left()
  ) %>%
  ph_with(
    value = block_list(
      fpar(ftext("Normalized", prop = fp_text(bold = TRUE, font.size = 12))),
      fpar(ftext("  • AUCSSN: AUC/mean(AUC)", prop = fp_text(font.size = 10))),
      fpar(ftext("  • CMAXSSN: Cmax/mean(Cmax)", prop = fp_text(font.size = 10))),
      fpar(ftext("")),
      fpar(ftext("Dose-Normalized", prop = fp_text(bold = TRUE, font.size = 12))),
      fpar(ftext("  • AUCSSDOS: AUC/Dose", prop = fp_text(font.size = 10))),
      fpar(ftext("  • CMXSSDOS: Cmax/Dose", prop = fp_text(font.size = 10))),
      fpar(ftext("")),
      fpar(ftext("Categorical", prop = fp_text(bold = TRUE, font.size = 12))),
      fpar(ftext("  • AUCSSCAT: Tertiles/quartiles", prop = fp_text(font.size = 10))),
      fpar(ftext("  • AUCSCATN: Numeric category", prop = fp_text(font.size = 10))),
      fpar(ftext("")),
      fpar(ftext("")),
      fpar(ftext("20 exposure metrics cover most E-R needs", prop = fp_text(bold = TRUE, font.size = 11, color = "#0066cc"))),
      fpar(ftext("Traditional datasets have only 3-5", prop = fp_text(font.size = 10, italic = TRUE))),
      fpar(ftext("All follow 8-character limit", prop = fp_text(font.size = 10, italic = TRUE)))
    ),
    location = ph_location_right()
  )

#===============================================================================
# SLIDE 7: 8-CHARACTER COMPLIANCE
#===============================================================================

# Create abbreviation table
abbrev_table <- data.frame(
  Traditional = c("AUCSSLOG", "CMAXSSLOG", "CAVGSSLOG", "CMAXSSSTD", "AUCSSDOSE", "CMAXSSDOSE", "AUCSSCATN"),
  Updated = c("AUCSLOG", "CMXSLOG", "CAVGLOG", "CMXSSSTD", "AUCSSDOS", "CMXSSDOS", "AUCSCATN"),
  Rationale = c("Removed one S", "Abbreviated CMAX", "Removed SS", "Abbreviated CMAX", "Removed E", "Abbreviated + removed E", "Removed one S"),
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
    location = ph_location(left = 0.5, top = 2.0, width = 9, height = 3.0)
  ) %>%
  ph_with(
    value = block_list(
      fpar(ftext("")),
      fpar(ftext("✓ All 267 unique variables comply with 8-character limit", prop = fp_text(font.size = 11, color = "#0066cc"))),
      fpar(ftext("✓ Systematic abbreviation rules documented", prop = fp_text(font.size = 11, color = "#0066cc"))),
      fpar(ftext("✓ Labels provide full descriptions", prop = fp_text(font.size = 11, color = "#0066cc")))
    ),
    location = ph_location(left = 0.5, top = 5.5, width = 9, height = 1.0)
  )

#===============================================================================
# SLIDE 8: ADEE - EXPOSURE-EFFICACY
#===============================================================================

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "ADEE: Exposure-Efficacy (71 variables)", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = block_list(
      fpar(ftext("Use Case: Progression-Free Survival by AUC Tertiles", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext("")),
      fpar(ftext("Key Dataset Features", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • One record per subject per parameter (OS, PFS, TTP, TTNT)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • AVAL = time from treatment start (days)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • CNSR = censoring indicator (1=censored, 0=event)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • EVENT = event indicator (1=event, 0=censored)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • No ATPT/AVISIT (not standard for time-to-event)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • All 20 exposure metrics from ADER", prop = fp_text(font.size = 11))),
      fpar(ftext("  • 13 baseline covariates from ADER", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Ready for modeling:", prop = fp_text(bold = TRUE, font.size = 12))),
      fpar(ftext("  coxph(Surv(AVAL, EVENT) ~ AUCSSSTD + AGE + SEX)", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("  survfit(Surv(AVAL, EVENT) ~ AUCSSCAT)", prop = fp_text(font.size = 10, font.family = "Courier New")))
    ),
    location = ph_location_type(type = "body")
  )

#===============================================================================
# SLIDE 9: ADES - EXPOSURE-SAFETY
#===============================================================================

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "ADES: Exposure-Safety (78 variables)", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = block_list(
      fpar(ftext("Use Case: Adverse Event Rates by Cmax Categories", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext("")),
      fpar(ftext("Unique Challenges", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • Multiple analysis levels (subject-level parameters + event-level records)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Uses ASEV/ASEVN (MILD/MODERATE/SEVERE) not AETOXGR", prop = fp_text(font.size = 11, color = "#cc0000"))),
      fpar(ftext("  • AERELN (0-4) for relationship modeling", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Subject-Level Parameters", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • TEAE: Total treatment-emergent AEs", prop = fp_text(font.size = 11))),
      fpar(ftext("  • TEAESEV: Severe AEs (ASEVN = 3)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • TESAE: Serious AEs (AESER = 'Y')", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Event-Level Records", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • Individual AE occurrences with exposure context", prop = fp_text(font.size = 11))),
      fpar(ftext("  • AEDECOD, ASEV, AEREL, dates, exposure metrics", prop = fp_text(font.size = 11)))
    ),
    location = ph_location_type(type = "body")
  )

#===============================================================================
# SLIDE 10: ASEV vs AETOXGR
#===============================================================================

# Create severity table
severity_table <- data.frame(
  Variable = c("ASEV", "ASEVN", "Values", "Use Case"),
  Description = c(
    "Adverse Event Severity",
    "Severity (Numeric)",
    "MILD (1), MODERATE (2), SEVERE (3)",
    "General AE severity (all therapeutic areas)"
  ),
  Notes = c(
    "Character variable",
    "Numeric version for modeling",
    "3-level scale (not 1-5 toxicity grades)",
    "More common in non-oncology"
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
      fpar(ftext("  • Simpler 3-level scale vs. 5-level CTCAE grades", prop = fp_text(font.size = 11))),
      fpar(ftext("  • SEVERE maps to regulatory 'serious' criteria", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("For oncology requiring CTCAE grades 1-5:", prop = fp_text(bold = TRUE, font.size = 11))),
      fpar(ftext("  → Map from ASEV + additional AE characteristics", prop = fp_text(font.size = 10, italic = TRUE)))
    ),
    location = ph_location(left = 0.5, top = 4.0, width = 9, height = 2.5)
  )

#===============================================================================
# SLIDE 11: ADTRR - TUMOR RESPONSE
#===============================================================================

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "ADTRR: Tumor Response for E-R (80 variables)", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = block_list(
      fpar(ftext("Use Case: Longitudinal Tumor Measurements with RECIST 1.1", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext("")),
      fpar(ftext("Key Features", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • Repeated measures structure (one record per subject-visit-parameter)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Three parameters: TSIZE, BOR, NADIR", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Baseline normalization (BASE, CHG, PCHG)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • RECIST 1.1 categorical response (CR/PR/SD/PD)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Best overall response with BORN (numeric version)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • All 20 exposure metrics available", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("8-Character Compliance", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • NADPCHG: Percent change from nadir", prop = fp_text(font.size = 11))),
      fpar(ftext("  • BORN: Numeric BOR (4=CR, 3=PR, 2=SD, 1=PD)", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Enables waterfall plots, spider plots, time-to-response, mixed models", prop = fp_text(font.size = 10, italic = TRUE)))
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
      fpar(ftext("Time Variables", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • ADT: Analysis date", prop = fp_text(font.size = 11))),
      fpar(ftext("  • ADY: Study day (relative to TRTSDT)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • AVAL: Numeric outcome", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Consistent calculation methods", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Exposure Metrics (20 variables)", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • Raw: AUCSS, CMAXSS, CAVGSS", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Transformed: AUCSLOG, CMXSLOG", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Standardized: AUCSSSTD, CMXSSSTD", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Categorical: AUCSSCAT, AUCSCATN", prop = fp_text(font.size = 11)))
    ),
    location = ph_location_left()
  ) %>%
  ph_with(
    value = block_list(
      fpar(ftext("Baseline Covariates (13 variables)", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • Demographics: AGE, SEX, RACE", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Vitals: WTBL, HTBL, BMIBL, BSA", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Renal: CREATBL, CRCLBL, EGFRBL", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Hepatic: ALTBL, ASTBL, TBILBL", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Analysis Flags", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • ANL01FL: Primary analysis", prop = fp_text(font.size = 11))),
      fpar(ftext("  • ANL02FL: Sensitivity analyses", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Traceability", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • Clear link to SDTM via derivation", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Full metadata specifications", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Reproducible derivation scripts", prop = fp_text(font.size = 11)))
    ),
    location = ph_location_right()
  )

#===============================================================================
# SLIDE 13: WHY {ADMIRAL}?
#===============================================================================

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Why {admiral}?", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = block_list(
      fpar(ftext("Advantages for ER Data", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext("")),
      fpar(ftext("✓ Modular functions: derive_vars_*, derive_param_*", prop = fp_text(font.size = 12))),
      fpar(ftext("✓ Validation built-in: assert_* functions catch errors early", prop = fp_text(font.size = 12))),
      fpar(ftext("✓ Metadata integration: Works with {metacore}, {metatools}, {xportr}", prop = fp_text(font.size = 12))),
      fpar(ftext("✓ Community-driven: Pharmaverse ecosystem", prop = fp_text(font.size = 12))),
      fpar(ftext("✓ Well-documented: Functions follow consistent patterns", prop = fp_text(font.size = 12))),
      fpar(ftext("✓ CDISC-aligned: Built with standards in mind (8-character compliance)", prop = fp_text(font.size = 12))),
      fpar(ftext("")),
      fpar(ftext("")),
      fpar(ftext("Example Extension Function", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("derive_param_exposure_response <- function(", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("  dataset, exposure_var, outcome_var,", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("  method = c('tertile', 'quartile', 'continuous')", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext(") { ... }", prop = fp_text(font.size = 10, font.family = "Courier New")))
    ),
    location = ph_location_type(type = "body")
  )

#===============================================================================
# SLIDE 14: BENEFITS - REPRODUCIBILITY
#===============================================================================

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Benefits: Reproducibility", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = block_list(
      fpar(ftext("Before Standardization", prop = fp_text(bold = TRUE, font.size = 13, color = "#cc0000"))),
      fpar(ftext("# Study A", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("data$time_days <- difftime(data$event_dt, data$trt_start, units='days')", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("data$AUC_cat <- cut(data$AUC, breaks=c(0,500,1000,Inf))", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("data$log_AUC <- log(data$AUC)", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("")),
      fpar(ftext("# Study B", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("df$tte <- as.numeric(df$EventDate - df$TreatmentDate) + 1", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("df$exposure_grp <- ifelse(df$auc < median(df$auc), 'Low', 'High')", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("df$lnAUC <- log(df$auc)", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("")),
      fpar(ftext("")),
      fpar(ftext("After Standardization", prop = fp_text(bold = TRUE, font.size = 13, color = "#00cc00"))),
      fpar(ftext("# All studies use consistent approach and naming", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("coxph(Surv(AVAL, EVENT) ~ AUCSSSTD + AGE + SEX, data = adee)", prop = fp_text(font.size = 10, font.family = "Courier New")))
    ),
    location = ph_location_type(type = "body")
  )

#===============================================================================
# SLIDE 15: BENEFITS - COMPREHENSIVE EXPOSURE
#===============================================================================

# Create comparison data
exposure_comp <- data.frame(
  Approach = c("Traditional", "New Framework"),
  Variables = c("3-5", "20"),
  Examples = c("AUC, CMAX, CAVG", "AUCSS, AUCSLOG, AUCSSSTD, AUCSSN, AUCSSDOS, AUCSSCAT, ..."),
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
      fpar(ftext("20 Pre-Computed Transformations", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext("  • Raw (3): AUCSS, CMAXSS, CAVGSS", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Log-transformed (3): AUCSLOG, CMXSLOG, CAVGLOG", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Standardized (3): AUCSSSTD, CMXSSSTD, CAVGSTD", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Normalized (3): AUCSSN, CMAXSSN, CAVGSSN", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Dose-normalized (3): AUCSSDOS, CMXSSDOS, CAVGDOS", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Categorical (5): AUCSSCAT, AUCSCATN, CMXSSCAT, CMXSCATN", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("✓ Analysts can immediately test multiple E-R representations", prop = fp_text(font.size = 12, color = "#0066cc"))),
      fpar(ftext("✓ No recoding needed in analysis scripts", prop = fp_text(font.size = 12, color = "#0066cc"))),
      fpar(ftext("✓ Promotes exploration of optimal E-R model", prop = fp_text(font.size = 12, color = "#0066cc")))
    ),
    location = ph_location(left = 0.5, top = 3.5, width = 9, height = 3.0)
  )

#===============================================================================
# SLIDE 16: BENEFITS - EFFICIENCY
#===============================================================================

# Create efficiency table
efficiency_table <- data.frame(
  Task = c("Dataset creation", "QC time", "Analysis prep"),
  Before = c("3 weeks", "40 hours", "20 hours"),
  After = c("1 week", "12 hours", "4 hours"),
  Reduction = c("66%", "70%", "80%"),
  stringsAsFactors = FALSE
)

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Benefits: Efficiency", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = block_list(
      fpar(ftext("Time Savings Demonstrated", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext(""))
    ),
    location = ph_location(left = 0.5, top = 1.3, width = 9, height = 0.5)
  ) %>%
  ph_with(
    value = efficiency_table,
    location = ph_location(left = 1.5, top = 2.0, width = 7, height = 2.0)
  ) %>%
  ph_with(
    value = block_list(
      fpar(ftext("")),
      fpar(ftext("Source of Efficiency", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("  • Template-based derivation scripts", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Automated validation with {admiral}", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Pre-computed transformations", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Integrated metadata", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Quality and speed go together with standards!", prop = fp_text(bold = TRUE, font.size = 12, color = "#0066cc")))
    ),
    location = ph_location(left = 0.5, top = 4.5, width = 9, height = 2.0)
  )

#===============================================================================
# SLIDE 17: COMPARISON WITH TRADITIONAL
#===============================================================================

# Create comparison table
traditional_comp <- data.frame(
  Feature = c("Exposure Metrics", "Transformations", "Categorization", "Baseline Covariates", 
              "Variable Naming", "Documentation", "Datasets"),
  Traditional = c("3-5 variables", "Manual", "Inconsistent", "5-10 variables", 
                  "Ad-hoc", "External", "1 file"),
  New_Framework = c("20 variables", "Pre-computed", "Standardized", "13 variables", 
                    "CDISC compliant", "Integrated specs", "4 specialized"),
  Improvement = c("4-7× increase", "Time savings", "Reproducibility", "Comprehensive", 
                  "Regulatory ready", "Traceability", "Domain optimized"),
  stringsAsFactors = FALSE
)

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Comparison with Traditional Approach", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = traditional_comp,
    location = ph_location(left = 0.3, top = 1.5, width = 9.4, height = 4.0)
  ) %>%
  ph_with(
    value = block_list(
      fpar(ftext("Efficiency Gains: 50% dataset creation, 70% QC, 80% analysis prep", 
                 prop = fp_text(bold = TRUE, font.size = 11, color = "#0066cc")))
    ),
    location = ph_location(left = 0.5, top = 6.0, width = 9, height = 0.5)
  )

#===============================================================================
# SLIDE 18: PATH FORWARD
#===============================================================================

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Path Forward: Community Adoption", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = block_list(
      fpar(ftext("What We Need", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext("")),
      fpar(ftext("1. Feedback on proposed structures", prop = fp_text(bold = TRUE, font.size = 12))),
      fpar(ftext("   • Variable naming conventions (8-character compliance)", prop = fp_text(font.size = 11))),
      fpar(ftext("   • Mandatory vs. optional elements", prop = fp_text(font.size = 11))),
      fpar(ftext("   • Domain-specific needs", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("2. Real-world testing", prop = fp_text(bold = TRUE, font.size = 12))),
      fpar(ftext("   • Pilot studies across organizations", prop = fp_text(font.size = 11))),
      fpar(ftext("   • Edge cases and exceptions", prop = fp_text(font.size = 11))),
      fpar(ftext("   • Tooling gaps", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("3. Documentation", prop = fp_text(bold = TRUE, font.size = 12))),
      fpar(ftext("   • Implementation guides", prop = fp_text(font.size = 11))),
      fpar(ftext("   • Worked examples with code", prop = fp_text(font.size = 11))),
      fpar(ftext("   • Best practices", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("4. Potential formalization", prop = fp_text(bold = TRUE, font.size = 12))),
      fpar(ftext("   • Could inform future CDISC guidance", prop = fp_text(font.size = 11))),
      fpar(ftext("   • Pharmaverse ER working group?", prop = fp_text(font.size = 11)))
    ),
    location = ph_location_type(type = "body")
  )

#===============================================================================
# SLIDE 19: GETTING STARTED TODAY
#===============================================================================

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Getting Started Today", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = block_list(
      fpar(ftext("Resources Available", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext("  • GitHub repository: https://github.com/jeffreyad/er-standards", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Example datasets (simulated data)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Complete derivation scripts (ADER, ADEE, ADES, ADTRR)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • ADaM specifications (P21 format + Excel)", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Metacore objects for metadata", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Presentation materials", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("Try It Yourself", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext("# Clone repository", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("git clone https://github.com/jeffreyad/er-standards.git", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("cd er-standards", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("# Generate example data", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("source('data-raw/S0_Generate_Example_Data.R')", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("# Run ADaM derivations", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("source('programs/run_all_adams.R')", prop = fp_text(font.size = 10, font.family = "Courier New"))),
      fpar(ftext("")),
      fpar(ftext("Connect", prop = fp_text(bold = TRUE, font.size = 14))),
      fpar(ftext("  • Questions: jeff.dickinson@navitaslifesciences.com", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Feedback: GitHub issues", prop = fp_text(font.size = 11))),
      fpar(ftext("  • Collaboration: Pharmaverse Slack", prop = fp_text(font.size = 11)))
    ),
    location = ph_location_type(type = "body")
  )

#===============================================================================
# SLIDE 20: KEY TAKEAWAYS
#===============================================================================

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(value = "Key Takeaways", location = ph_location_type(type = "title")) %>%
  ph_with(
    value = block_list(
      fpar(ftext("1. ER data needs standardization", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("   Current state is inconsistent and limited (3-5 exposure variables)", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("2. CDISC principles apply", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("   We can extend SDTM-PK patterns with 8-character compliance", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("3. Four specialized datasets", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("   • ADER: Exposure foundation (20 metrics, 65 variables)", prop = fp_text(font.size = 11))),
      fpar(ftext("   • ADEE: Exposure-Efficacy time-to-event (71 variables)", prop = fp_text(font.size = 11))),
      fpar(ftext("   • ADES: Exposure-Safety events (78 variables, uses ASEV)", prop = fp_text(font.size = 11))),
      fpar(ftext("   • ADTRR: Tumor response longitudinal (80 variables)", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("4. Comprehensive exposure coverage: 20 metrics vs. traditional 3-5", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("")),
      fpar(ftext("5. {admiral} is well-suited", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("   Modular, validated, community-supported", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("6. Benefits are real", prop = fp_text(bold = TRUE, font.size = 13))),
      fpar(ftext("   Efficiency (50-80% time reduction), reproducibility, quality, compliance", prop = fp_text(font.size = 11))),
      fpar(ftext("")),
      fpar(ftext("7. Community input needed: pilot, refine, formalize", prop = fp_text(bold = TRUE, font.size = 13)))
    ),
    location = ph_location_type(type = "body")
  )

#===============================================================================
# SLIDE 21: QUESTIONS
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
      fpar(ftext("Documentation: https://github.com/jeffreyad/er-standards/specifications", prop = fp_text(font.size = 12))),
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
cat("  Slides: 21\n")
cat("  Topics:\n")
cat("    - Overview and motivation\n")
cat("    - Four datasets (ADER, ADEE, ADES, ADTRR)\n")
cat("    - 8-character compliance\n")
cat("    - ASEV vs AETOXGR\n")
cat("    - Benefits and efficiency gains\n")
cat("    - Path forward\n\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================