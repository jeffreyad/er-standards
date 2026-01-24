# ===============================================================================
# Function: derive_exposure_metrics
#
# Purpose: Derive exposure metrics from dose and baseline covariates
#
# Description: Simulates or derives steady-state exposure metrics including
#              primary PK parameters, transformations, and categories.
#              In production, replace simulation with actual ADPC-derived values.
#
# Parameters:
#   adsl_data     - ADSL dataset with baseline covariates
#   source        - "simulated" or "adpc" (default: "simulated")
#   adpc_data     - Optional ADPC dataset (required if source = "adpc")
#   seed          - Random seed for simulation (default: 12345)
#   tertile_var   - Variable to use for tertile/quartile cutpoints (default: "AUCSS")
#
# Returns: Dataset with exposure metrics added
#
# Author: Jeff Dickinson
# Date: 2026-01-22
# Version: 1.0
#
# ===============================================================================

derive_exposure_metrics <- function(adsl_data,
                                    source = c("simulated", "adpc"),
                                    adpc_data = NULL,
                                    seed = 12345,
                                    tertile_var = "AUCSS") {
  source <- match.arg(source)

  # =============================================================================
  # DERIVE PRIMARY EXPOSURE METRICS
  # =============================================================================

  if (source == "simulated") {
    # Simulation-based approach (for testing/development)
    exposure_primary <- derive_simulated_exposure(adsl_data, seed)
  } else if (source == "adpc") {
    # Production approach: use actual ADPC data
    if (is.null(adpc_data)) {
      stop("adpc_data must be provided when source = 'adpc'")
    }
    exposure_primary <- derive_adpc_exposure(adsl_data, adpc_data)
  }

  # =============================================================================
  # DERIVE TRANSFORMATIONS
  # =============================================================================

  exposure_transformed <- derive_exposure_transformations(exposure_primary)

  # =============================================================================
  # DERIVE CATEGORIES
  # =============================================================================

  exposure_final <- derive_exposure_categories(
    exposure_transformed,
    tertile_var = tertile_var
  )

  return(exposure_final)
}

# ===============================================================================
# Helper: Simulated Exposure (for development/testing)
# ===============================================================================

derive_simulated_exposure <- function(adsl_data, seed = 12345) {
  set.seed(seed)

  exposure_data <- adsl_data %>%
    mutate(
      # Dose from treatment arm
      DOSE = case_when(
        ARM == "Placebo" ~ 0,
        ARM == "Xanomeline Low Dose" ~ 54,
        ARM == "Xanomeline High Dose" ~ 81,
        TRUE ~ NA_real_
      ),

      # Simulate individual clearance based on covariates
      # CL = CL_typical * (CrCL/100)^0.75 * (WT/70)^-0.25
      CL_EST = 5 * (CRCLBL / 100)^0.75 * (WTBL / 70)^(-0.25),

      # Steady-state AUC: AUC = Dose/CL with inter-individual variability (30% CV)
      AUCSS = if_else(
        DOSE > 0,
        (DOSE / CL_EST) * exp(rnorm(n(), 0, 0.3)),
        0
      ),

      # Cmax: Proportional to AUC with absorption-related variability (25% CV)
      CMAXSS = if_else(
        DOSE > 0,
        AUCSS * 0.18 * exp(rnorm(n(), 0, 0.25)),
        0
      ),

      # Average concentration (assuming QD dosing, tau = 24h)
      CAVGSS = if_else(DOSE > 0, AUCSS / 24, 0),

      # Trough concentration (simplified 2-compartment model)
      CMINSS = if_else(
        DOSE > 0,
        CAVGSS * 0.6 * exp(rnorm(n(), 0, 0.35)),
        0
      ),

      # Individual clearance
      CLSS = if_else(DOSE > 0, DOSE / AUCSS, NA_real_)
    ) %>%
    select(-CL_EST) # Remove intermediate calculation

  message("✓ Simulated exposure metrics derived (seed = ", seed, ")")

  return(exposure_data)
}

# ===============================================================================
# Helper: ADPC-derived Exposure (for production)
# ===============================================================================

derive_adpc_exposure <- function(adsl_data, adpc_data) {
  # Extract steady-state exposure metrics from ADPC
  # Assumes ADPC has individual PK parameters from population PK analysis

  exposure_from_adpc <- adpc_data %>%
    filter(PCTESTCD %in% c("AUCSS", "CMAXSS", "CAVGSS", "CMINSS", "CLSS")) %>%
    select(STUDYID, USUBJID, PCTESTCD, PCSTRESN) %>%
    pivot_wider(
      names_from = PCTESTCD,
      values_from = PCSTRESN
    )

  # Merge with ADSL
  exposure_data <- adsl_data %>%
    left_join(
      exposure_from_adpc,
      by = c("STUDYID", "USUBJID")
    ) %>%
    mutate(
      # Add DOSE if not already present
      DOSE = if (!"DOSE" %in% names(.)) {
        case_when(
          ARM == "Placebo" ~ 0,
          ARM == "Xanomeline Low Dose" ~ 54,
          ARM == "Xanomeline High Dose" ~ 81,
          TRUE ~ NA_real_
        )
      } else {
        DOSE
      },

      # Set placebo values to 0
      across(
        c(AUCSS, CMAXSS, CAVGSS, CMINSS, CLSS),
        ~ if_else(DOSE == 0, 0, .x)
      )
    )

  message("✓ Exposure metrics derived from ADPC")

  return(exposure_data)
}

# ===============================================================================
# Helper: Derive Transformations
# ===============================================================================

derive_exposure_transformations <- function(exposure_data) {
  # Calculate reference values from active treatment subjects only
  aucss_active <- exposure_data %>%
    filter(DOSE > 0) %>%
    pull(AUCSS)
  aucss_mean <- mean(aucss_active, na.rm = TRUE)
  aucss_sd <- sd(aucss_active, na.rm = TRUE)
  aucss_median <- median(aucss_active, na.rm = TRUE)

  cmaxss_active <- exposure_data %>%
    filter(DOSE > 0) %>%
    pull(CMAXSS)
  cmaxss_mean <- mean(cmaxss_active, na.rm = TRUE)
  cmaxss_sd <- sd(cmaxss_active, na.rm = TRUE)

  exposure_transformed <- exposure_data %>%
    mutate(
      # Log transformations (8-char names)
      AUCSLOG = log(AUCSS + 0.01),
      CMXSLOG = log(CMAXSS + 0.01),
      CAVGLOG = log(CAVGSS + 0.01),

      # Standardized (z-score) - only for active treatment
      AUCSSSTD = if_else(DOSE > 0, (AUCSS - aucss_mean) / aucss_sd, NA_real_),
      CMXSSSTD = if_else(DOSE > 0, (CMAXSS - cmaxss_mean) / cmaxss_sd, NA_real_),

      # Normalized (ratio to median)
      AUCSSN = if_else(DOSE > 0, AUCSS / aucss_median, NA_real_),

      # Dose-normalized (8-char names)
      AUCSSDOS = if_else(DOSE > 0, AUCSS / DOSE, NA_real_),
      CMXSSDOS = if_else(DOSE > 0, CMAXSS / DOSE, NA_real_)
    )

  message("✓ Exposure transformations derived")
  message("  Active subjects: ", length(aucss_active))
  message("  AUCSS mean: ", round(aucss_mean, 2))
  message("  AUCSS median: ", round(aucss_median, 2))

  return(exposure_transformed)
}

# ===============================================================================
# Helper: Derive Categories
# ===============================================================================

derive_exposure_categories <- function(exposure_data, tertile_var = "AUCSS") {
  # Calculate quantile cutpoints from active subjects only
  active_values <- exposure_data %>%
    filter(DOSE > 0) %>%
    pull(!!sym(tertile_var))

  tertile_breaks <- quantile(active_values,
    probs = c(0, 1 / 3, 2 / 3, 1),
    na.rm = TRUE
  )

  quartile_breaks <- quantile(active_values,
    probs = c(0, 0.25, 0.5, 0.75, 1),
    na.rm = TRUE
  )

  median_value <- median(active_values, na.rm = TRUE)

  exposure_categorized <- exposure_data %>%
    mutate(
      # Tertiles (8-char names)
      AUCSSCAT = if_else(
        DOSE > 0,
        as.character(cut(AUCSS,
          breaks = tertile_breaks,
          labels = c("Low", "Medium", "High"),
          include.lowest = TRUE
        )),
        NA_character_
      ),
      AUCSCATN = case_when(
        AUCSSCAT == "Low" ~ 1,
        AUCSSCAT == "Medium" ~ 2,
        AUCSSCAT == "High" ~ 3,
        TRUE ~ NA_real_
      ),

      # Quartiles
      AUCSSQ = if_else(
        DOSE > 0,
        as.character(cut(AUCSS,
          breaks = quartile_breaks,
          labels = c("Q1", "Q2", "Q3", "Q4"),
          include.lowest = TRUE
        )),
        NA_character_
      ),
      AUCSSQN = case_when(
        AUCSSQ == "Q1" ~ 1,
        AUCSSQ == "Q2" ~ 2,
        AUCSSQ == "Q3" ~ 3,
        AUCSSQ == "Q4" ~ 4,
        TRUE ~ NA_real_
      ),

      # Median split
      AUCSSMED = if_else(
        DOSE > 0,
        if_else(AUCSS >= median_value, "Above Median", "Below Median"),
        NA_character_
      )
    )

  message("✓ Exposure categories derived")
  message("  Tertile cutpoints: ", paste(round(tertile_breaks, 2), collapse = ", "))
  message("  Quartile cutpoints: ", paste(round(quartile_breaks, 2), collapse = ", "))

  return(exposure_categorized)
}
