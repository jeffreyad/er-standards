# ER Standards

Documents for PHUSE US Connect 2026

### Standardizing Exposure-Response Data for Modeling and Simulation Using CDISC Principles and `{admiral}`

# Abstract

Exposure-Response (ER) modeling is a key tool in assessing the safety and efficacy of new drugs, enabling evaluation of the relationship between drug exposure, toxicity, and clinical benefit. ER datasets often resemble those used in Population Pharmacokinetic (PopPK) modeling, sharing features such as numeric covariates, relative time variables, and dependent outcomes. While CDISC released standards for PopPK data in 2023, no equivalent standards currently exist for ER data. However, many of the same principles could be applied. This paper explores ER datasets across three domains: Exposure-Efficacy (EE), with endpoints like Overall Survival (OS) and Progression-Free Survival (PFS); Exposure-Safety (ES), which may include specific adverse event frequencies; and Tumor Response, which may include measures of tumor size over time. Using the {admiral} R package, we demonstrate programming examples that illustrate how ER data can be structured in alignment with emerging standards, supporting consistency and reproducibility in modeling workflows. We propose a framework for standardizing ER data that extends CDISC SDTM-PK principles while accommodating the unique requirements of each domain.

---

## Key Resources

### ADaM Dataset Programs

| Program | Dataset | Description |
|---------|---------|-------------|
| [programs/ad_ader.R](programs/ad_ader.R) | ADER | Exposure metrics (raw, log, standardized, dose-normalized, categorical) |
| [programs/ad_adee.R](programs/ad_adee.R) | ADEE | Exposure-Efficacy: time-to-event (OS, PFS) |
| [programs/ad_ades.R](programs/ad_ades.R) | ADES | Exposure-Safety: AE rates, CTCAE grades |
| [programs/ad_adtrr.R](programs/ad_adtrr.R) | ADTRR | Tumor Response: RECIST 1.1 longitudinal data |
| [programs/run_all_adams.R](programs/run_all_adams.R) | All | Master script to run all programs |

### Specifications

Define-XML and variable-level specifications for all four datasets are in [specifications/](specifications/).

### Paper & Presentation

- Paper: [docs/paper/paper_final.qmd](docs/paper/paper_final.qmd)
- Slides: [docs/presentations/presentation.qmd](docs/presentations/presentation.qmd)

---

## Getting Started

```r
# Restore package environment
renv::restore()

# Run all ADaM programs
source("programs/run_all_adams.R")
```
