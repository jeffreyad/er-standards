# ER Standards Project - Complete Summary

---

## About

This repository supports the PHUSE US Connect 2026 paper and presentation:

**"Standardizing Exposure-Response Data for Modeling and Simulation Using CDISC Principles and `{admiral}`"**

Author: Jeffrey Dickinson, Navitas Data Sciences

The project proposes a CDISC-compliant framework for four Exposure-Response (ER) ADaM datasets, with working programming examples built using the `{admiral}` R package.

---

## Proposed ADaM Dataset Framework

| Dataset | Domain | Purpose |
|---------|--------|---------|
| **ADER** | Foundation | 20 exposure metrics (raw, log, standardized, dose-normalized, categorical) |
| **ADEE** | Exposure-Efficacy | Time-to-event endpoints (OS, PFS) using Weibull AFT models |
| **ADES** | Exposure-Safety | Adverse event rates, CTCAE grades, Poisson/logistic regression |
| **ADTRR** | Tumor Response | RECIST 1.1 longitudinal measurements, BOR, waterfall/spider plots |

---

## Repository Structure

```
er-standards/
├── README.md                        # Project overview and abstract
├── PROJECT_SUMMARY.md               # This file
│
├── programs/                        # ADaM dataset creation programs
│   ├── ad_adee.R                    # ADEE (Exposure-Efficacy)
│   ├── ad_ader.R                    # ADER (Exposure metrics foundation)
│   ├── ad_ades.R                    # ADES (Exposure-Safety)
│   ├── ad_adtrr.R                   # ADTRR (Tumor Response)
│   ├── ad_adee_long.R               # Long-format variant of ADEE
│   ├── ad_ades_long.R               # Long-format variant of ADES
│   ├── ad_adtrr_long.R              # Long-format variant of ADTRR
│   └── run_all_adams.R              # Master script to run all programs
│
├── R/                               # Supporting R functions and scripts
│   ├── 01_ee_exposure_efficacy.R    # Exposure-efficacy analysis examples
│   ├── 02_es_exposure_safety.R      # Exposure-safety analysis examples
│   ├── 03_tumor_response.R          # Tumor response analysis examples
│   ├── derive_exposure_metrics.R    # Reusable exposure derivation functions
│   ├── simulation_functions.R       # Data simulation utilities
│   └── S0_data.R / S1-S3 scripts   # Dataset-specific derivation scripts
│
├── specifications/                  # ADaM dataset specifications
│   ├── ADEE_P21_Specifications.csv  # ADEE variable-level specs
│   ├── ADES_P21_Specifications.csv  # ADES variable-level specs
│   ├── ADTR_P21_Specifications.csv  # ADTR variable-level specs
│   ├── ADTRR_P21_Specifications.csv # ADTRR variable-level specs
│   ├── ADER_P21_Specifications.csv  # ADER variable-level specs
│   ├── *_DefineXML.xml              # Define-XML files for each dataset
│   └── CHANGELOG.md                 # Specification change history
│
├── data/                            # Simulated input datasets (.rds, .csv)
├── data-raw/                        # Scripts to generate simulated data
│   ├── S0_Generate_Example_Data.R
│   ├── S1_Generate_ADSL.R
│   ├── S2_Generate_ADTTE.R
│   ├── S3_Generate_ADAE.R
│   ├── S4_Generate_ADTR.R
│   └── S5_Generate_ADRS.R
│
├── adam/                            # Final ADaM datasets — gitignored, local only
│   └── adee.xpt, ader.xpt, ades.xpt, adtrr.xpt (+ .csv, .rda, .rds)
│
├── docs/
│   ├── paper/
│   │   ├── paper_final.qmd          # Final manuscript (canonical version)
│   │   ├── paper_draft.qmd          # Working draft
│   │   ├── paper_draft_long.qmd     # Extended draft with full technical content
│   │   ├── er_framework_comparison.qmd  # Supplementary comparison document
│   │   ├── references.bib           # Citation database
│   │   └── _quarto.yml              # Quarto render configuration
│   └── presentations/
│       ├── presentation.qmd         # Conference slides (RevealJS, current version)
│       ├── presentation_v1.qmd      # Older slide version
│       ├── presentation.pptx        # Exported PowerPoint
│       ├── background.qmd           # Background slides
│       └── er_comparison.qmd        # ER comparison slides
│
├── config/
│   └── exposure_config.R            # Shared exposure parameter configuration
│
├── output/                          # Generated figures and tables — gitignored
│   ├── figures/                     # PDF figures (S1–S3 series)
│   └── tables/                      # CSV summary tables
│
└── renv/                            # R package environment management
    ├── renv.lock                    # Locked package versions (committed)
    └── activate.R                   # renv activation script (committed)
```

---

## Rendering the Paper

From the `docs/paper/` directory, run:

```bash
quarto render paper_final.qmd
```

The `_quarto.yml` is already configured to render `paper_final.qmd`.

---

## Running the ADaM Programs

```r
source("programs/run_all_adams.R")
```

Or run individual programs in `programs/` in any order. Input data is in `data/`.

---

## R Environment

This project uses `renv` for reproducible package management.

```r
renv::restore()  # Install all required packages from renv.lock
```


```         
er-standards-project/
├── README.md                          # Project overview and structure
├── QUICK_START.md                     # 5-minute getting started guide
├── scripts/
│   ├── 01_ee_exposure_efficacy.R      # Time-to-event ER examples (PFS/OS)
│   ├── 02_es_exposure_safety.R        # Adverse event rate examples
│   └── 03_tumor_response.R            # Longitudinal tumor measurements
├── functions/
│   └── derive_er_params.R             # Reusable helper functions
├── docs/
│   ├── paper_draft.qmd                # Full manuscript draft
│   └── presentation.qmd               # Conference presentation (35+ slides)
└── specifications/                     # (Placeholder for ADaM specs)
```

------------------------------------------------------------------------

## 📊 Three Complete Working Examples

### 1. Exposure-Efficacy (EE) - Time-to-Event

**File**: `scripts/01_ee_exposure_efficacy.R`

**What it demonstrates**: - Creating ADEE dataset for survival analysis - Time-to-event derivation (AVAL = days from treatment) - Exposure categorization (tertiles, quartiles, median splits) - Event vs. censoring handling (CNSR, EVENT variables) - Analysis flags (ANL01FL, ANL02FL) - Log-transformed and standardized exposure metrics

**Key output structure**:

```         
USUBJID | PARAMCD | AVAL | CNSR | EVENT | AUC0_24 | AUC_TERTILE | LOGAUC
```

**Ready for**: - Cox proportional hazards models - Kaplan-Meier curves by exposure category - Time-to-event plots

------------------------------------------------------------------------

### 2. Exposure-Safety (ES) - Adverse Events

**File**: `scripts/02_es_exposure_safety.R`

**What it demonstrates**: - Multi-level analysis structure: - Subject-level: Overall AE burden and rates - Event-level: Individual AE occurrences - Parameter-level: Specific AE types - Rate calculations (events per 100 patient-days) - Grade/severity tracking (CTCAE grades) - Serious AE flagging - Drug relationship assessment

**Key output structures**:

Subject-level:

```         
USUBJID | N_AES | N_SAE | RATE_AES | AUC_TERTILE | ANY_GRADE3
```

Event-level:

```         
USUBJID | AEDECOD | AETOXGR | ASTDY | CMAX | GRADE3FL | SERFL
```

**Ready for**: - Poisson regression for rates - Logistic regression for binary outcomes - Time-to-first-event analyses - Recurrent event models

------------------------------------------------------------------------

### 3. Tumor Response - Longitudinal Measurements

**File**: `scripts/03_tumor_response.R`

**What it demonstrates**: - Repeated measures structure - Baseline identification (ABLFL) - Change from baseline (CHG, PCHG) - RECIST 1.1 criteria implementation: - CR: Complete Response (tumor = 0) - PR: Partial Response (≥30% decrease) - PD: Progressive Disease (≥20% increase) - SD: Stable Disease - Best Overall Response (BOR) derivation - Nadir and best percent change parameters

**Key output structure**:

```         
USUBJID | VISIT | ADY | BASE | AVAL | CHG | PCHG | AVALC | BOR | AUC_TERTILE
```

**Ready for**: - Waterfall plots (best % change) - Spider plots (individual trajectories) - Response rate analysis - Mixed effects models

------------------------------------------------------------------------

## 🛠️ Reusable Functions

**File**: `functions/derive_er_params.R`

Created 15+ helper functions that you can use across projects:

**Exposure categorization**: - `derive_exposure_tertile()` - Split into Low/Medium/High - `derive_exposure_quartile()` - Split into Q1-Q4 - `derive_exposure_binary()` - Above/below median

**Exposure transformations**: - `derive_log_exposure()` - Natural log transformation - `derive_standardized_exposure()` - Z-score normalization

**Time calculations**: - `derive_study_day()` - Calculate ADY from dates - `derive_relative_time()` - Time in various units

**Clinical assessments**: - `derive_change_from_baseline()` - CHG and PCHG - `derive_recist_response()` - Apply RECIST 1.1 - `derive_best_response()` - Calculate BOR

**Quality control**: - `check_missing_exposure()` - Validate exposure data - `validate_time_variables()` - Check date logic - `summarize_er_by_exposure()` - Descriptive statistics

All functions follow {admiral} conventions with tidy evaluation!

------------------------------------------------------------------------

## 📝 Conference Presentation

**File**: `docs/presentation.qmd` (Quarto/RevealJS format)

**35+ slides including**:

**Introduction (5 slides)**: - Overview and challenge statement - The ER modeling landscape - Why standards matter - SDTM-PK foundation - Three ER domains comparison table

**Technical Content (20 slides)**: - Domain 1: Exposure-Efficacy - Use case and structure - Code examples with line highlighting - Analysis-ready output - Domain 2: Exposure-Safety - Multi-level structure - Subject-level derivations - Event-level granularity - Domain 3: Tumor Response - Longitudinal structure - RECIST implementation - Output examples

**Integration (5 slides)**: - Common patterns across domains - Why {admiral}? - Benefits: reproducibility & efficiency - Path forward for community adoption - Getting started resources

**Backup Slides (5 slides)**: - Variable specifications reference - Edge case handling - SDTM-PK mapping details

**Features**: - Professional RevealJS theme - Code highlighting - Incremental reveals - Speaker notes on every slide - Ready to render to HTML or PDF

------------------------------------------------------------------------

## 📄 Full Manuscript Draft

**File**: `docs/paper_draft.qmd`

**Complete sections**:

1.  **Abstract** (200 words)

    -   Clear problem statement
    -   Methods overview
    -   Key contributions

2.  **Introduction** (\~2000 words)

    -   Background on ER modeling
    -   CDISC SDTM-PK as foundation
    -   The {admiral} ecosystem
    -   Study objectives

3.  **Methods** (\~3000 words)

    -   Proposed framework overview
    -   ADEE specification
    -   ADES specification
    -   ADTR specification
    -   Implementation details
    -   Common derivation patterns
    -   Domain-specific derivations
    -   Validation approach

4.  **Results** (outline with placeholders)

    -   Dataset characteristics
    -   Domain-specific results
    -   Cross-domain patterns

5.  **Discussion** (outline)

    -   Advantages of standardization
    -   Limitations
    -   Path to formalization

6.  **Conclusions** (outline)

7.  **Appendices** (placeholders)

    -   Complete code examples
    -   Metadata specifications

**Additional notes**: - References framework set up (BibTeX) - Figure and table placeholders identified - Ready for your content addition - Can render to PDF or Word

------------------------------------------------------------------------

## 📚 Documentation

### README.md

-   Project overview
-   Directory structure
-   Domain descriptions
-   Key principles
-   Dependencies
-   Citation template

### QUICK_START.md

-   5-minute getting started guide
-   Run your first example
-   Key functions overview
-   Adaptation examples
-   Common workflows
-   Validation checklist
-   Troubleshooting tips

------------------------------------------------------------------------

## 🎯 What You Should Do Next

### Immediate (This Week):

1.  **Review the code examples** - Run all three scripts to see the outputs
2.  **Customize the presentation** - Add your organization details, adjust timing
3.  **Test with your data** - Replace simulated data with real examples from your work

### Short-term (Next 2 Weeks):

4.  **Expand the paper** - Fill in Results and Discussion sections with your insights
5.  **Create real figures** - Generate actual plots (waterfall, spider, K-M curves)
6.  **Get feedback** - Share with colleagues or admiral team for input

### Medium-term (Next Month):

7.  **Create ADaM specifications** - Use metatools to document variable specs
8.  **Add more domains** - Consider exposure-QT, exposure-immunogenicity if relevant
9.  **Write vignettes** - Document edge cases and advanced use cases
10. **Prepare for publication** - Select target journal, format accordingly

------------------------------------------------------------------------

## 💡 Key Decisions You'll Need to Make

1.  **Conference vs. Journal**:
    -   If conference-first: Focus on presentation, make paper an extended abstract
    -   If journal-first: Expand paper significantly, use presentation for dissemination
2.  **Example Data**:
    -   Use your real data (with appropriate blinding)?
    -   Stick with simulated examples?
    -   Mix of both?
3.  **Scope**:
    -   Keep focused on these three domains?
    -   Add more specialized cases?
    -   Include real-world edge cases?
4.  **Community Engagement**:
    -   Share on GitHub early for feedback?
    -   Present at pharmaverse meetup first?
    -   Coordinate with admiral team?

------------------------------------------------------------------------

## 🔧 Technical Notes

**All code is**: - Fully executable (runs as-is) - Well-commented - Follows {admiral} conventions - Uses tidy evaluation properly - Includes validation checks

**Presentation is**: - Quarto format (works with RStudio, Positron, VS Code) - Renders to HTML (RevealJS) - Can also render to PowerPoint or PDF - Has speaker notes for every slide

**Paper is**: - Quarto format - Renders to PDF or Word - BibTeX-ready for references - Includes cross-references for figures/tables

------------------------------------------------------------------------

## 📞 Next Steps & Support

Everything is in `/mnt/user-data/outputs/er-standards-project/`

You can: 1. Download the entire folder 2. Review each component 3. Start customizing immediately 4. Run the code examples 5. Begin rendering the presentation

If you need: - **More code examples** - Let me know what domains - **Presentation adjustments** - Different format, more/fewer slides - **Paper expansion** - Help writing specific sections - **Function improvements** - Additional helpers or validation - **Specification templates** - metatools-compatible specs

This is a solid foundation that you can build on. The hardest part (structure and initial code) is done!

------------------------------------------------------------------------

## ✨ Highlights

**What makes this special**:

1.  ✅ **Complete working code** - Not just pseudocode, actual runnable examples
2.  ✅ **Three distinct domains** - Comprehensive coverage of ER landscape
3.  ✅ **Reusable functions** - DRY principle, can use across projects
4.  ✅ **Professional presentation** - Conference-ready with minimal editing
5.  ✅ **Manuscript foundation** - Strong structure to build on
6.  ✅ **admiral-aligned** - Follows established pharmaverse patterns
7.  ✅ **Well-documented** - Comments, notes, and guides throughout
8.  ✅ **Practical focus** - Real-world applicable, not just theoretical

You're in great shape to move forward with this project!