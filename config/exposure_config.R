# ===============================================================================
# Exposure Metrics Configuration
#
# Purpose: Central configuration for exposure metric derivation
#          Allows easy switching between simulated and production data
#
# ===============================================================================

# Exposure source: "simulated" or "adpc"
EXPOSURE_SOURCE <- "simulated" # Change to "adpc" for production

# Random seed for simulation (reproducibility)
EXPOSURE_SEED <- 12345

# Variable to use for tertile/quartile cutpoints
TERTILE_VARIABLE <- "AUCSS"

# ADPC parameter codes (if using ADPC source)
ADPC_PARAMS <- c(
  AUCSS = "AUCSS", # Steady-state AUC
  CMAXSS = "CMAXSS", # Steady-state Cmax
  CAVGSS = "CAVGSS", # Steady-state Cavg
  CMINSS = "CMINSS", # Steady-state Cmin
  CLSS = "CLSS" # Steady-state Clearance
)

# Dose mapping by treatment arm
DOSE_MAPPING <- list(
  "Placebo" = 0,
  "Xanomeline Low Dose" = 54,
  "Xanomeline High Dose" = 81
)

# Transformation settings
LOG_OFFSET <- 0.01 # Small constant to add before log transformation

# Category settings
TERTILE_LABELS <- c("Low", "Medium", "High")
QUARTILE_LABELS <- c("Q1", "Q2", "Q3", "Q4")
MEDIAN_LABELS <- c("Below Median", "Above Median")
