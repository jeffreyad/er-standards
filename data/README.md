# Example Datasets for ER Standards Framework

## Overview
These datasets demonstrate the proposed standardized structure for
Exposure-Response (ER) data in clinical trials.

## Files

### adee_example.csv
- **Purpose**: Exposure-Efficacy analysis (time-to-event endpoints)
- **Structure**: One record per subject per parameter (PFS, OS)
- **Key Variables**: AVAL (time), EVENT/CNSR (status), EXPOSURE_VAR
- **Analysis**: Time-to-event modeling (Cox, Weibull)
- **N**: 400 records (200 subjects × 2 endpoints)

### ades_example.csv
- **Purpose**: Exposure-Safety analysis (adverse events)
- **Structure**: Multi-level (subject, event, parameter)
- **Key Variables**: N_AES, RATE_AES, AEDECOD, AETOXGR
- **Analysis**: Count/rate models (Poisson, negative binomial)
- **Levels**: Subject summaries, event details, parameter counts

### adtr_example.csv
- **Purpose**: Tumor Response analysis (RECIST 1.1)
- **Structure**: Longitudinal measurements per subject
- **Key Variables**: AVAL (tumor size), PCHG, BOR, NADIR
- **Analysis**: Longitudinal models, waterfall/spider plots
- **Visits**: Baseline + 8 post-baseline assessments

## Data Generation
These are synthetic datasets created using:
- Population PK exposure simulation
- Direct Weibull time-to-event simulation
- RECIST 1.1 response criteria
- Realistic clinical trial patterns

## Sample Size
- N = 200 subjects
- 3 dose levels (50, 100, 150 mg)
- Exposure-response relationships embedded

## Exposure-Response Relationships
All datasets include built-in ER relationships:
- **ADEE**: Higher exposure → longer PFS/OS (HR = 0.988 per μg·h/mL)
- **ADES**: Higher exposure → increased AE rates
- **ADTR**: Higher exposure → better tumor response

## Usage
Load data in R:
```r
adee <- read.csv('data/adee_example.csv')
ades <- read.csv('data/ades_example.csv')
adtr <- read.csv('data/adtr_example.csv')
```

## Contact
For questions about the ER Standards Framework, see the main manuscript.

