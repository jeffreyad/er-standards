# ===============================================================================
# Supplementary Material S0
# Generate Example Datasets for ER Standards Framework
#
# Purpose: Create realistic synthetic data for ADEE, ADES, and ADTR examples
#          These datasets follow the proposed standardized structure
#
# Author: [Your Name]
# Date: 2026-01-17
# ===============================================================================

library(dplyr)
library(tidyr)

set.seed(12345) # For reproducibility

# Create output directory if it doesn't exist
if (!dir.exists("data")) dir.create("data")

cat("=== ER Standards Framework - Data Generation ===\n\n")

# ===============================================================================
# HELPER FUNCTIONS
# ===============================================================================

# Generate subject IDs
generate_usubjid <- function(n, study = "PROTOCOL001", site_prefix = "001") {
  sprintf("%s-%s-%05d", study, site_prefix, 1:n)
}

# Simulate pharmacokinetic exposure
simulate_exposure <- function(dose, weight, age, crcl, cv = 0.3) {
  # Simple PopPK model: AUC depends on dose, weight, and clearance
  # CL = CL_typical * (CrCl/100)^0.75 * (WT/70)^-0.25
  cl_typical <- 5 # L/h
  cl <- cl_typical * (crcl / 100)^0.75 * (weight / 70)^-0.25

  # AUC = Dose / CL with log-normal variability
  auc_mean <- dose / cl
  auc <- auc_mean * exp(rnorm(length(dose), 0, cv))

  return(auc)
}

# Categorize exposure into tertiles/quartiles
categorize_exposure <- function(exposure, method = "tertile") {
  if (method == "tertile") {
    cut(exposure,
      breaks = quantile(exposure, probs = c(0, 1 / 3, 2 / 3, 1)),
      labels = c("Low", "Medium", "High"),
      include.lowest = TRUE
    )
  } else if (method == "quartile") {
    cut(exposure,
      breaks = quantile(exposure, probs = c(0, 0.25, 0.5, 0.75, 1)),
      labels = c("Q1", "Q2", "Q3", "Q4"),
      include.lowest = TRUE
    )
  } else if (method == "median") {
    cut(exposure,
      breaks = quantile(exposure, probs = c(0, 0.5, 1)),
      labels = c("Below Median", "Above Median"),
      include.lowest = TRUE
    )
  }
}

# Direct Weibull time-to-event simulation
simulate_tte_direct <- function(subjects_df, endpoint,
                                shape = 1.5,
                                scale0 = 180, # Baseline median days
                                beta_exposure = -0.015,
                                max_followup = 730) {
  # Center exposure
  exposure_c <- subjects_df$EXPOSURE_VAR - mean(subjects_df$EXPOSURE_VAR)

  # Scale parameter with exposure effect
  # scale = scale0 * exp(beta * exposure)
  # Higher exposure → lower hazard → longer survival
  scale <- scale0 * exp(beta_exposure * exposure_c)

  # Simulate Weibull event times
  # Using rweibull: T ~ Weibull(shape, scale)
  event_times <- rweibull(nrow(subjects_df), shape = shape, scale = scale)

  # Apply administrative censoring at max_followup
  observed_times <- pmin(event_times, max_followup)

  # Event indicator: 1 if event before censoring, 0 if censored
  event_indicator <- as.numeric(event_times <= max_followup)

  # Random censoring (some subjects drop out early)
  random_censor_time <- runif(
    nrow(subjects_df),
    max_followup * 0.5, # Min 50% of followup
    max_followup
  ) # Max full followup
  final_times <- pmin(observed_times, random_censor_time)
  final_event <- event_indicator * (observed_times <= random_censor_time)

  # Create result
  result <- subjects_df %>%
    mutate(
      PARAMCD = endpoint,
      PARAM = case_when(
        endpoint == "PFS" ~ "Progression-Free Survival",
        endpoint == "OS" ~ "Overall Survival",
        endpoint == "TTP" ~ "Time to Progression"
      ),
      AVAL = round(final_times),
      AVALU = "DAYS",
      EVENT = final_event,
      CNSR = 1 - final_event,
      ADT = TRTSDT + AVAL,
      ADY = AVAL + 1
    )

  return(result)
}

# ===============================================================================
# 1. GENERATE BASELINE SUBJECT CHARACTERISTICS
# ===============================================================================

n_subjects <- 200

cat("Generating baseline characteristics for", n_subjects, "subjects...\n")

baseline <- data.frame(
  USUBJID = generate_usubjid(n_subjects),
  STUDYID = "PROTOCOL001",
  SITEID = sample(sprintf("001-%02d", 1:10), n_subjects, replace = TRUE),

  # Demographics
  AGE = round(rnorm(n_subjects, mean = 58, sd = 12)),
  SEX = sample(c("M", "F"), n_subjects, replace = TRUE, prob = c(0.55, 0.45)),
  RACE = sample(c("WHITE", "BLACK OR AFRICAN AMERICAN", "ASIAN", "OTHER"),
    n_subjects,
    replace = TRUE,
    prob = c(0.70, 0.15, 0.10, 0.05)
  ),
  ETHNIC = sample(c("NOT HISPANIC OR LATINO", "HISPANIC OR LATINO"),
    n_subjects,
    replace = TRUE, prob = c(0.85, 0.15)
  ),

  # Baseline measurements
  WTBL = round(rnorm(n_subjects, mean = 75, sd = 15), 1),
  HTBL = round(rnorm(n_subjects, mean = 170, sd = 10), 0),

  # Treatment assignment
  ARM = sample(c("DRUG 50mg", "DRUG 100mg", "DRUG 150mg"),
    n_subjects,
    replace = TRUE
  ),
  ACTARM = sample(c("DRUG 50mg", "DRUG 100mg", "DRUG 150mg"),
    n_subjects,
    replace = TRUE
  ),

  # Study day 1 date
  TRTSDT = as.Date("2023-01-15") + sample(0:180, n_subjects, replace = TRUE)
) %>%
  mutate(
    # Ensure reasonable values
    AGE = pmax(18, pmin(AGE, 85)),
    WTBL = pmax(40, pmin(WTBL, 150)),
    HTBL = pmax(140, pmin(HTBL, 200)),

    # Derived variables
    BMIBL = round(WTBL / (HTBL / 100)^2, 1),
    BSABL = round(0.007184 * WTBL^0.425 * HTBL^0.725, 2),

    # Dose assignment
    DOSE = case_when(
      ARM == "DRUG 50mg" ~ 50,
      ARM == "DRUG 100mg" ~ 100,
      ARM == "DRUG 150mg" ~ 150
    ),

    # Baseline labs
    CREATBL = round(rnorm(n_subjects, mean = 1.0, sd = 0.3), 2),
    CRCLBL = round(
      ifelse(SEX == "M",
        ((140 - AGE) * WTBL) / (72 * CREATBL),
        ((140 - AGE) * WTBL) / (72 * CREATBL) * 0.85
      ),
      1
    ),
    EGFRBL = round(rnorm(n_subjects, mean = 85, sd = 20), 0),
    TBILBL = round(rnorm(n_subjects, mean = 0.8, sd = 0.3), 2),
    ASTBL = round(rnorm(n_subjects, mean = 28, sd = 8), 0),
    ALTBL = round(rnorm(n_subjects, mean = 25, sd = 7), 0),

    # Ensure labs are positive and reasonable
    CREATBL = pmax(0.5, CREATBL),
    CRCLBL = pmax(30, pmin(CRCLBL, 150)),
    EGFRBL = pmax(30, pmin(EGFRBL, 130)),
    TBILBL = pmax(0.2, TBILBL),
    ASTBL = pmax(10, ASTBL),
    ALTBL = pmax(10, ALTBL),

    # Analysis flags
    SAFFL = "Y",
    ITTFL = "Y",
    PPROTFL = sample(c("Y", "N"), n_subjects, replace = TRUE, prob = c(0.92, 0.08))
  )

# Simulate steady-state exposure
baseline <- baseline %>%
  mutate(
    EXPOSURE_VAR = simulate_exposure(DOSE, WTBL, AGE, CRCLBL),
    EXPOSURE_LOG = log(EXPOSURE_VAR),
    EXPOSURE_STD = scale(EXPOSURE_VAR)[, 1],
    EXPOSURE_TERTILE = categorize_exposure(EXPOSURE_VAR, "tertile"),
    EXPOSURE_QUARTILE = categorize_exposure(EXPOSURE_VAR, "quartile"),
    EXPOSURE_MEDIAN = categorize_exposure(EXPOSURE_VAR, "median")
  )

cat("✓ Baseline characteristics generated\n")
cat("  - Mean exposure:", round(mean(baseline$EXPOSURE_VAR), 1), "μg·h/mL\n")
cat(
  "  - Exposure range:", round(min(baseline$EXPOSURE_VAR), 1), "-",
  round(max(baseline$EXPOSURE_VAR), 1), "μg·h/mL\n\n"
)

# ===============================================================================
# 2. GENERATE ADEE DATASET (EXPOSURE-EFFICACY)
# ===============================================================================

cat("=== Generating ADEE Dataset ===\n")

# Generate PFS data
cat("  Simulating PFS events...\n")
adee_pfs <- simulate_tte_direct(
  baseline,
  endpoint = "PFS",
  shape = 1.3,
  scale0 = 150, # Median 150 days at mean exposure
  beta_exposure = -0.012, # HR = 0.988 per unit AUC (protective)
  max_followup = 730
)

cat("    - Events:", sum(adee_pfs$EVENT), "/", nrow(adee_pfs), "\n")
cat("    - Median time:", median(adee_pfs$AVAL), "days\n")

# Generate OS data (longer times, weaker exposure effect)
cat("  Simulating OS events...\n")
adee_os <- simulate_tte_direct(
  baseline,
  endpoint = "OS",
  shape = 1.5,
  scale0 = 300, # Median 300 days at mean exposure
  beta_exposure = -0.008, # Weaker effect than PFS
  max_followup = 730
) %>%
  mutate(
    # OS should be >= PFS (subjects can't die before progression)
    AVAL = pmax(AVAL, adee_pfs$AVAL + round(rexp(n(), 1 / 60))),
    # Recalculate dates
    ADT = TRTSDT + AVAL,
    ADY = AVAL + 1,
    # Adjust censoring if time extended
    EVENT = ifelse(AVAL >= 730, 0, EVENT),
    CNSR = 1 - EVENT
  )

cat("    - Events:", sum(adee_os$EVENT), "/", nrow(adee_os), "\n")
cat("    - Median time:", median(adee_os$AVAL), "days\n")

# Combine endpoints
adee <- bind_rows(adee_pfs, adee_os) %>%
  arrange(USUBJID, PARAMCD) %>%
  mutate(
    # Analysis timepoint reference
    ATPT = "BASELINE",
    ATPTN = 0,
    AVISIT = "DAY 1",
    AVISITN = 1,

    # Analysis flags
    ANL01FL = ifelse(PARAMCD == "PFS", "Y", "N"), # Primary analysis
    ANL02FL = ifelse(PARAMCD == "OS", "Y", "N"), # Secondary analysis

    # Exposure transformations
    EXPOSURE_LOG = log(EXPOSURE_VAR),
    EXPOSURE_STD = scale(EXPOSURE_VAR)[, 1],
    EXPOSURE_CAT = EXPOSURE_TERTILE
  )

# Save ADEE
write.csv(adee, "data/adee_example.csv", row.names = FALSE)
cat("✓ ADEE dataset created:", nrow(adee), "records\n")
cat("  - Subjects:", length(unique(adee$USUBJID)), "\n")
cat(
  "  - PFS events:", sum(adee$EVENT[adee$PARAMCD == "PFS"]),
  "(", round(100 * mean(adee$EVENT[adee$PARAMCD == "PFS"]), 1), "%)\n"
)
cat(
  "  - OS events:", sum(adee$EVENT[adee$PARAMCD == "OS"]),
  "(", round(100 * mean(adee$EVENT[adee$PARAMCD == "OS"]), 1), "%)\n\n"
)

# ===============================================================================
# 3. GENERATE ADES DATASET (EXPOSURE-SAFETY)
# ===============================================================================

cat("=== Generating ADES Dataset ===\n")

# ADES has multi-level structure:
# 1. Subject-level summary records
# 2. Event-level AE records
# 3. Parameter-level summaries (by AEDECOD)

## 3.1 Simulate Adverse Events ----

cat("  Simulating adverse events...\n")

# AE terms and their baseline rates
ae_terms <- data.frame(
  AEDECOD = c(
    "NAUSEA", "FATIGUE", "DIARRHEA", "HEADACHE",
    "DECREASED APPETITE", "RASH", "HYPERTENSION",
    "NEUTROPENIA", "THROMBOCYTOPENIA", "ANEMIA"
  ),
  AEBODSYS = c(
    "GASTROINTESTINAL DISORDERS",
    "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",
    "GASTROINTESTINAL DISORDERS",
    "NERVOUS SYSTEM DISORDERS",
    "METABOLISM AND NUTRITION DISORDERS",
    "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
    "VASCULAR DISORDERS",
    "BLOOD AND LYMPHATIC SYSTEM DISORDERS",
    "BLOOD AND LYMPHATIC SYSTEM DISORDERS",
    "BLOOD AND LYMPHATIC SYSTEM DISORDERS"
  ),
  baseline_rate = c(0.35, 0.42, 0.28, 0.22, 0.18, 0.15, 0.12, 0.25, 0.18, 0.30),
  exposure_effect = c(0.008, 0.005, 0.010, 0.003, 0.006, 0.007, 0.012, 0.015, 0.010, 0.008)
)

# Function to simulate AEs for one subject
simulate_subject_aes <- function(subject_data, ae_terms, max_duration = 365) {
  exposure <- subject_data$EXPOSURE_VAR
  trtdur <- min(max_duration, round(rexp(1, 1 / 180))) # Treatment duration

  aes_list <- list()

  for (i in 1:nrow(ae_terms)) {
    term <- ae_terms$AEDECOD[i]
    base_rate <- ae_terms$baseline_rate[i]
    exp_effect <- ae_terms$exposure_effect[i]

    # Probability increases with exposure
    prob_ae <- base_rate * exp(exp_effect * (exposure - mean(baseline$EXPOSURE_VAR)))
    prob_ae <- min(prob_ae, 0.8) # Cap at 80%

    # Does subject experience this AE?
    if (runif(1) < prob_ae) {
      # How many times?
      n_events <- rpois(1, lambda = 1.5) + 1

      for (j in 1:n_events) {
        ae_start <- round(runif(1, 1, trtdur))
        ae_duration <- round(rexp(1, 1 / 7)) # Mean 7 days

        aes_list[[length(aes_list) + 1]] <- data.frame(
          USUBJID = subject_data$USUBJID,
          AEDECOD = term,
          AEBODSYS = ae_terms$AEBODSYS[i],
          AESTDY = ae_start,
          AEENDY = min(ae_start + ae_duration, trtdur),
          AETOXGR = sample(1:4, 1, prob = c(0.5, 0.3, 0.15, 0.05)),
          AESER = sample(c("N", "Y"), 1, prob = c(0.85, 0.15)),
          AEREL = sample(
            c(
              "NOT RELATED", "POSSIBLY RELATED",
              "PROBABLY RELATED", "RELATED"
            ),
            1,
            prob = c(0.3, 0.35, 0.25, 0.10)
          )
        )
      }
    }
  }

  if (length(aes_list) > 0) {
    return(bind_rows(aes_list))
  } else {
    return(NULL)
  }
}

# Simulate AEs for all subjects
all_aes <- list()
for (i in 1:nrow(baseline)) {
  subject_aes <- simulate_subject_aes(baseline[i, ], ae_terms)
  if (!is.null(subject_aes)) {
    all_aes[[i]] <- subject_aes
  }
}

ae_events <- bind_rows(all_aes)

cat("    - Total AE records:", nrow(ae_events), "\n")
cat("    - Subjects with AEs:", length(unique(ae_events$USUBJID)), "/", n_subjects, "\n")
cat("    - Serious AEs:", sum(ae_events$AESER == "Y"), "\n")

## 3.2 Create ADES Multi-Level Structure ----

cat("  Creating multi-level structure...\n")

### Level 1: Subject-level summaries ----
ades_subject <- baseline %>%
  left_join(
    ae_events %>%
      group_by(USUBJID) %>%
      summarise(
        N_AES = n(),
        N_SAE = sum(AESER == "Y"),
        N_GRADE3 = sum(AETOXGR >= 3),
        N_GRADE4 = sum(AETOXGR == 4),
        TRTDURD = max(AEENDY, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "USUBJID"
  ) %>%
  mutate(
    # Replace NA with 0 for subjects with no AEs
    N_AES = ifelse(is.na(N_AES), 0, N_AES),
    N_SAE = ifelse(is.na(N_SAE), 0, N_SAE),
    N_GRADE3 = ifelse(is.na(N_GRADE3), 0, N_GRADE3),
    N_GRADE4 = ifelse(is.na(N_GRADE4), 0, N_GRADE4),
    TRTDURD = ifelse(is.na(TRTDURD), round(rexp(1, 1 / 180)), TRTDURD),

    # Rates per 100 patient-days
    RATE_AES = (N_AES / TRTDURD) * 100,
    RATE_SAE = (N_SAE / TRTDURD) * 100,

    # Binary indicators (CHARACTER for consistency)
    ANY_AE = ifelse(N_AES > 0, "Y", "N"),
    ANY_SAE = ifelse(N_SAE > 0, "Y", "N"),

    # ADES-specific variables
    PARAMCD = "SUBJSUM",
    PARAM = "Subject-Level Safety Summary",
    AVAL = N_AES,
    AVALU = "COUNT",
    ANL01FL = "Y"
  )

### Level 2: Event-level records ----
ades_event <- ae_events %>%
  left_join(
    baseline %>% select(
      USUBJID, STUDYID, TRTSDT, EXPOSURE_VAR,
      EXPOSURE_TERTILE, WTBL, AGE, SEX
    ),
    by = "USUBJID"
  ) %>%
  mutate(
    PARAMCD = "AEVENT",
    PARAM = "Adverse Event Occurrence",
    AVAL = 1,
    AVALU = "EVENT",
    ADT = TRTSDT + AESTDY,
    ADY = AESTDY + 1,
    ANL01FL = "Y",
    ANL02FL = ifelse(AESER == "Y", "Y", "N"), # SAEs only
    ANL03FL = ifelse(AETOXGR >= 3, "Y", "N"), # Grade 3+ only
    # Add placeholders for subject-level vars (NA for events)
    N_AES = NA_real_,
    N_SAE = NA_real_,
    N_GRADE3 = NA_real_,
    RATE_AES = NA_real_,
    RATE_SAE = NA_real_,
    ANY_AE = NA_character_,
    ANY_SAE = NA_character_
  )

### Level 3: Parameter-level summaries (by AEDECOD) ----
# Get treatment duration for each subject
subj_trtdur <- ae_events %>%
  group_by(USUBJID) %>%
  summarise(TRTDURD = max(AEENDY), .groups = "drop")

ades_param <- ae_events %>%
  left_join(baseline %>% select(USUBJID, EXPOSURE_VAR, EXPOSURE_TERTILE),
    by = "USUBJID"
  ) %>%
  left_join(subj_trtdur, by = "USUBJID") %>%
  group_by(USUBJID, AEDECOD, AEBODSYS, EXPOSURE_VAR, EXPOSURE_TERTILE, TRTDURD) %>%
  summarise(
    N_EVENTS = n(),
    MAX_GRADE = max(AETOXGR),
    ANY_SAE_NUM = as.numeric(any(AESER == "Y")), # Numeric first
    .groups = "drop"
  ) %>%
  mutate(
    # Convert to character for consistency
    ANY_SAE = ifelse(ANY_SAE_NUM == 1, "Y", "N"),
    PARAMCD = AEDECOD,
    PARAM = paste("Adverse Event:", AEDECOD),
    AVAL = N_EVENTS,
    AVALU = "COUNT",
    RATE = (N_EVENTS / TRTDURD) * 100,
    ANL01FL = "Y",

    # Add placeholders for subject-level vars
    N_AES = NA_real_,
    N_SAE = NA_real_,
    N_GRADE3 = NA_real_,
    RATE_AES = NA_real_,
    RATE_SAE = NA_real_,
    ANY_AE = NA_character_
  ) %>%
  select(-ANY_SAE_NUM) # Remove temporary numeric version

# Combine all levels (now all ANY_SAE are character)
ades <- bind_rows(
  ades_subject %>%
    select(
      USUBJID, STUDYID, PARAMCD, PARAM, AVAL, AVALU,
      EXPOSURE_VAR, EXPOSURE_TERTILE,
      N_AES, N_SAE, N_GRADE3, RATE_AES, RATE_SAE,
      ANY_AE, ANY_SAE, TRTDURD, ANL01FL,
      AGE, SEX, WTBL, BMIBL
    ),
  ades_event %>%
    select(
      USUBJID, STUDYID, PARAMCD, PARAM, AVAL, AVALU,
      AEDECOD, AEBODSYS, AESTDY, AEENDY, AETOXGR, AESER, AEREL,
      EXPOSURE_VAR, EXPOSURE_TERTILE, ADT, ADY,
      ANL01FL, ANL02FL, ANL03FL,
      AGE, SEX, WTBL,
      N_AES, N_SAE, N_GRADE3, RATE_AES, RATE_SAE, ANY_AE, ANY_SAE
    ),
  ades_param %>%
    select(
      USUBJID, PARAMCD, PARAM, AVAL, AVALU,
      AEDECOD, AEBODSYS, N_EVENTS, MAX_GRADE, ANY_SAE,
      EXPOSURE_VAR, EXPOSURE_TERTILE, RATE, TRTDURD, ANL01FL,
      N_AES, N_SAE, N_GRADE3, RATE_AES, RATE_SAE, ANY_AE
    )
) %>%
  arrange(USUBJID, PARAMCD)

# Save ADES
write.csv(ades, "data/ades_example.csv", row.names = FALSE)
cat("✓ ADES dataset created:", nrow(ades), "records\n")
cat("  - Subject-level:", sum(ades$PARAMCD == "SUBJSUM"), "\n")
cat("  - Event-level:", sum(ades$PARAMCD == "AEVENT"), "\n")
cat("  - Parameter-level:", sum(!ades$PARAMCD %in% c("SUBJSUM", "AEVENT")), "\n\n")

# ===============================================================================
# 4. GENERATE ADTR DATASET (TUMOR RESPONSE)
# ===============================================================================

cat("=== Generating ADTR Dataset ===\n")

# ADTR: Longitudinal tumor measurements with RECIST 1.1

cat("  Simulating tumor trajectories...\n")

# Visit schedule
visits <- data.frame(
  AVISITN = c(0, 1, 2, 3, 4, 5, 6, 7, 8),
  AVISIT = c(
    "BASELINE", "WEEK 6", "WEEK 12", "WEEK 18",
    "WEEK 24", "WEEK 30", "WEEK 36", "WEEK 42", "WEEK 48"
  ),
  STUDY_DAY = c(1, 42, 84, 126, 168, 210, 252, 294, 336)
)

# Function to simulate tumor trajectory for one subject
simulate_tumor_trajectory <- function(subject_data, visits) {
  exposure <- subject_data$EXPOSURE_VAR

  # Baseline tumor burden (sum of diameters)
  baseline_sum <- round(runif(1, 50, 150))

  # Response characteristics depend on exposure
  # Higher exposure → better response
  exposure_centered <- exposure - mean(baseline$EXPOSURE_VAR)

  # Maximum response (% reduction)
  # Base response + exposure effect + random variability
  max_response <- -30 + (-15 * exposure_centered / sd(baseline$EXPOSURE_VAR)) + rnorm(1, 0, 10)
  max_response <- max(max_response, -100) # Can't go below -100%

  # Time to maximum response
  time_to_max <- round(runif(1, 84, 168)) # 12-24 weeks

  # Eventually progresses?
  will_progress <- runif(1) < 0.4
  if (will_progress) {
    time_to_progression <- round(runif(1, 200, 400))
  } else {
    time_to_progression <- Inf
  }

  trajectory <- visits %>%
    mutate(
      USUBJID = subject_data$USUBJID,

      # Tumor size follows a biphasic model
      PCHG = ifelse(
        STUDY_DAY < time_to_max,
        # Initial response phase (exponential approach to max response)
        max_response * (1 - exp(-STUDY_DAY / (time_to_max / 3))),
        # Maintenance/progression phase
        ifelse(
          STUDY_DAY < time_to_progression,
          max_response, # Stable
          max_response + 0.3 * (STUDY_DAY - time_to_progression) # Progression
        )
      ),

      # Add measurement noise
      PCHG = PCHG + rnorm(n(), 0, 5),

      # Calculate absolute values
      BASE = ifelse(AVISITN == 0, baseline_sum, NA),
      AVAL = baseline_sum * (1 + PCHG / 100),
      AVAL = round(pmax(AVAL, 0)), # Can't be negative
      CHG = AVAL - baseline_sum,

      # Recalculate PCHG from actual values
      PCHG = round((AVAL / baseline_sum - 1) * 100, 1)
    )

  # Fill baseline value
  baseline_val <- trajectory$BASE[1]
  trajectory$BASE <- baseline_val

  # Derive RECIST categories
  trajectory <- trajectory %>%
    mutate(
      AVALC = case_when(
        AVAL == 0 ~ "CR",
        PCHG <= -30 ~ "PR",
        PCHG >= 20 ~ "PD",
        TRUE ~ "SD"
      ),
      AVALN = case_when(
        AVALC == "CR" ~ 4,
        AVALC == "PR" ~ 3,
        AVALC == "SD" ~ 2,
        AVALC == "PD" ~ 1
      )
    )

  return(trajectory)
}

# Generate trajectories for all subjects
all_trajectories <- list()
for (i in 1:nrow(baseline)) {
  all_trajectories[[i]] <- simulate_tumor_trajectory(baseline[i, ], visits)
}

adtr_long <- bind_rows(all_trajectories)

cat("    - Total measurements:", nrow(adtr_long), "\n")

# Derive best overall response (BOR)
cat("  Deriving best overall response...\n")
bor <- adtr_long %>%
  filter(AVISITN > 0) %>% # Exclude baseline
  group_by(USUBJID) %>%
  summarise(
    BOR = case_when(
      any(AVALC == "CR") ~ "CR",
      any(AVALC == "PR") ~ "PR",
      any(AVALC == "PD") ~ "PD",
      TRUE ~ "SD"
    ),
    BORN = case_when(
      BOR == "CR" ~ 4,
      BOR == "PR" ~ 3,
      BOR == "SD" ~ 2,
      BOR == "PD" ~ 1
    ),
    .groups = "drop"
  )

# Derive nadir (lowest value post-baseline)
nadir <- adtr_long %>%
  filter(AVISITN > 0) %>%
  group_by(USUBJID) %>%
  summarise(
    NADIR = min(AVAL),
    NADIR_PCHG = min(PCHG),
    NADIR_VISIT = AVISIT[which.min(AVAL)],
    .groups = "drop"
  )

# Create complete ADTR dataset
adtr <- adtr_long %>%
  left_join(
    baseline %>% select(
      USUBJID, STUDYID, TRTSDT,
      EXPOSURE_VAR, EXPOSURE_TERTILE,
      AGE, SEX, WTBL, BMIBL, DOSE
    ),
    by = "USUBJID"
  ) %>%
  left_join(bor, by = "USUBJID") %>%
  left_join(nadir, by = "USUBJID") %>%
  mutate(
    # Dates
    ADT = TRTSDT + STUDY_DAY - 1,
    ADY = STUDY_DAY,

    # Parameters
    PARAMCD = "TUMSIZE",
    PARAM = "Sum of Target Lesion Diameters",
    AVALU = "mm",

    # Baseline flag
    ABLFL = ifelse(AVISITN == 0, "Y", "N"),

    # Analysis flags
    ANL01FL = ifelse(AVISITN > 0, "Y", "N"), # Post-baseline
    ANL02FL = ifelse(AVISITN > 0 & AVALC %in% c("CR", "PR"), "Y", "N"), # Responders

    # Exposure transformations
    EXPOSURE_LOG = log(EXPOSURE_VAR),
    EXPOSURE_CAT = EXPOSURE_TERTILE
  ) %>%
  arrange(USUBJID, AVISITN)

# Add derived parameter records for BOR and NADIR
adtr_bor <- adtr %>%
  filter(AVISITN == 1) %>% # One record per subject
  mutate(
    PARAMCD = "BOR",
    PARAM = "Best Overall Response",
    AVAL = BORN,
    AVALC = BOR,
    AVALU = "",
    CHG = NA,
    PCHG = NA,
    ABLFL = "N",
    ANL01FL = "Y"
  )

adtr_nadir <- adtr %>%
  filter(AVISITN == 1) %>%
  mutate(
    PARAMCD = "NADIR",
    PARAM = "Nadir Tumor Size",
    AVAL = NADIR,
    AVALC = NA,
    CHG = NADIR - BASE,
    PCHG = NADIR_PCHG,
    ABLFL = "N",
    ANL01FL = "Y"
  )

# Combine all
adtr_final <- bind_rows(adtr, adtr_bor, adtr_nadir) %>%
  arrange(USUBJID, PARAMCD, AVISITN) %>%
  select(
    USUBJID, STUDYID, PARAMCD, PARAM, AVISITN, AVISIT,
    ADT, ADY, AVAL, AVALU, AVALC, AVALN,
    BASE, CHG, PCHG, BOR, BORN, NADIR, NADIR_PCHG,
    ABLFL, ANL01FL, ANL02FL,
    EXPOSURE_VAR, EXPOSURE_LOG, EXPOSURE_CAT,
    AGE, SEX, WTBL, BMIBL, DOSE
  )

# Save ADTR
write.csv(adtr_final, "data/adtr_example.csv", row.names = FALSE)
cat("✓ ADTR dataset created:", nrow(adtr_final), "records\n")
cat("  - Subjects:", length(unique(adtr_final$USUBJID)), "\n")
cat("  - Tumor measurements:", sum(adtr_final$PARAMCD == "TUMSIZE"), "\n")
cat("  - Response distribution:\n")
cat("    CR:", sum(adtr_final$BOR == "CR", na.rm = TRUE), "\n")
cat("    PR:", sum(adtr_final$BOR == "PR", na.rm = TRUE), "\n")
cat("    SD:", sum(adtr_final$BOR == "SD", na.rm = TRUE), "\n")
cat("    PD:", sum(adtr_final$BOR == "PD", na.rm = TRUE), "\n\n")

# ===============================================================================
# 5. CREATE DATA DOCUMENTATION
# ===============================================================================

cat("Creating documentation...\n")

# Create README for data directory
readme_text <- "# Example Datasets for ER Standards Framework

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
"

writeLines(readme_text, "data/README.md")
# ===============================================================================
# 6. SUMMARY
# ===============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("DATA GENERATION COMPLETE\n")
cat(strrep("=", 70), "\n\n")

cat("Files created in data/ directory:\n")
cat("  1. adee_example.csv -", nrow(adee), "records\n")
cat("  2. ades_example.csv -", nrow(ades), "records\n")
cat("  3. adtr_example.csv -", nrow(adtr_final), "records\n")
cat("  4. README.md - Data documentation\n\n")

cat("Summary statistics:\n")
cat("  Subjects:", n_subjects, "\n")
cat("  Dose levels: 50, 100, 150 mg\n")
cat(
  "  Exposure range:",
  round(min(baseline$EXPOSURE_VAR), 1), "-",
  round(max(baseline$EXPOSURE_VAR), 1), "μg·h/mL\n"
)
cat("  Mean exposure:", round(mean(baseline$EXPOSURE_VAR), 1), "μg·h/mL\n\n")

cat("Event rates:\n")
cat(
  "  PFS:", sum(adee$EVENT[adee$PARAMCD == "PFS"]), "/",
  sum(adee$PARAMCD == "PFS"),
  sprintf("(%.1f%%)", 100 * mean(adee$EVENT[adee$PARAMCD == "PFS"])), "\n"
)
cat(
  "  OS:", sum(adee$EVENT[adee$PARAMCD == "OS"]), "/",
  sum(adee$PARAMCD == "OS"),
  sprintf("(%.1f%%)", 100 * mean(adee$EVENT[adee$PARAMCD == "OS"])), "\n"
)
cat(
  "  AEs:", nrow(ae_events), "events in",
  length(unique(ae_events$USUBJID)), "subjects\n"
)
cat(
  "  ORR (CR+PR):",
  sum(adtr_final$BOR %in% c("CR", "PR"), na.rm = TRUE), "/", n_subjects,
  sprintf("(%.1f%%)", 100 * mean(adtr_final$BOR %in% c("CR", "PR"), na.rm = TRUE)),
  "\n\n"
)

cat("You can now run the analysis scripts:\n")
cat("  - S1_ADEE_Exposure_Efficacy_Analysis.R\n")
cat("  - S2_ADES_Exposure_Safety_Analysis.R\n")
cat("  - S3_ADTR_Tumor_Response_Analysis.R\n\n")

# Save session info
sink("data/session_info_data_generation.txt")
cat("=== R Session Information ===\n\n")
cat("Data Generated:", as.character(Sys.time()), "\n\n")
print(sessionInfo())
sink()

cat("Session info saved: data/session_info_data_generation.txt\n")

cat("\n")
cat(strrep("=", 70), "\n")
cat("END OF DATA GENERATION\n")
cat(strrep("=", 70), "\n")

# ===============================================================================
# END OF SCRIPT
# ===============================================================================
