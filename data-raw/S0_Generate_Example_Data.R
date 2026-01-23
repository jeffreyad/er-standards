## code to prepare `DATASET` dataset goes here

#===============================================================================
# Master Script: Generate All Example Data
#
# Purpose: Generate simulated datasets for ER Standards Framework examples
#
# Description: Creates synthetic clinical trial data with realistic structure
#              and relationships. Data is intentionally simplified for
#              demonstration purposes.
#
# Author: Jeff Dickinson
# Date: 2026-01-22
#
# Usage:
#   source("data-raw/S0_Generate_Example_Data.R")
#
# Outputs:
#   - data/adsl_simulated.rds
#   - data/adtte_simulated.rds
#   - data/adae_simulated.rds
#   - data/adtr_simulated.rds
#   - data/adrs_simulated.rds
#   - data/advs_simulated.rds
#   - data/adlb_simulated.rds
#
#===============================================================================

# Load required packages
library(dplyr)
library(tidyr)
library(lubridate)

# Source helper functions
source("R/simulation_functions.R")

# Set seed for reproducibility
set.seed(12345)

#===============================================================================
# CONFIGURATION
#===============================================================================

# Study parameters
N_SUBJECTS <- 300
N_SITES <- 10
STUDY_START <- as.Date("2024-01-01")
STUDY_END <- as.Date("2025-12-31")

# Treatment arms
ARMS <- c("Placebo", "Drug Low Dose", "Drug High Dose")
ARM_RATIO <- c(1, 1, 1)  # 1:1:1 randomization

# Doses (mg)
DOSE_MAP <- c(
  "Placebo" = 0,
  "Drug Low Dose" = 54,
  "Drug High Dose" = 81
)

cat("\n")
cat(strrep("=", 80), "\n")
cat("ER STANDARDS FRAMEWORK - DATA GENERATION\n")
cat(strrep("=", 80), "\n")
cat("Configuration:\n")
cat("  Subjects:", N_SUBJECTS, "\n")
cat("  Sites:", N_SITES, "\n")
cat("  Treatment arms:", paste(ARMS, collapse = ", "), "\n")
cat("  Study period:", STUDY_START, "to", STUDY_END, "\n")
cat(strrep("=", 80), "\n\n")

#===============================================================================
# CREATE OUTPUT DIRECTORY
#===============================================================================

if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

if (!dir.exists("logs")) {
  dir.create("logs", recursive = TRUE)
}

#===============================================================================
# GENERATE DATASETS
#===============================================================================

# Track generation status
generation_log <- list()

## Step 1: ADSL (Subject-Level) ----
cat("Step 1: Generating ADSL (Subject-Level)...\n")
start_time <- Sys.time()

tryCatch({
  source("data-raw/S1_Generate_ADSL.R")
  runtime <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat("  ✓ ADSL created:", nrow(adsl_simulated), "subjects\n")
  cat("  Runtime:", round(runtime, 2), "seconds\n\n")
  generation_log$adsl <- list(status = "SUCCESS", records = nrow(adsl_simulated))
}, error = function(e) {
  cat("  ✗ Error:", e$message, "\n\n")
  generation_log$adsl <- list(status = "ERROR", error = e$message)
})

## Step 2: ADTTE (Time-to-Event) ----
cat("Step 2: Generating ADTTE (Time-to-Event)...\n")
start_time <- Sys.time()

tryCatch({
  source("data-raw/S2_Generate_ADTTE.R")
  runtime <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat("  ✓ ADTTE created:", nrow(adtte_simulated), "records\n")
  cat("  Runtime:", round(runtime, 2), "seconds\n\n")
  generation_log$adtte <- list(status = "SUCCESS", records = nrow(adtte_simulated))
}, error = function(e) {
  cat("  ✗ Error:", e$message, "\n\n")
  generation_log$adtte <- list(status = "ERROR", error = e$message)
})

## Step 3: ADAE (Adverse Events) ----
cat("Step 3: Generating ADAE (Adverse Events)...\n")
start_time <- Sys.time()

tryCatch({
  source("data-raw/S3_Generate_ADAE.R")
  runtime <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat("  ✓ ADAE created:", nrow(adae_simulated), "records\n")
  cat("  Runtime:", round(runtime, 2), "seconds\n\n")
  generation_log$adae <- list(status = "SUCCESS", records = nrow(adae_simulated))
}, error = function(e) {
  cat("  ✗ Error:", e$message, "\n\n")
  generation_log$adae <- list(status = "ERROR", error = e$message)
})

## Step 4: ADTR (Tumor Measurements) ----
cat("Step 4: Generating ADTR (Tumor Measurements)...\n")
start_time <- Sys.time()

tryCatch({
  source("data-raw/S4_Generate_ADTR.R")
  runtime <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat("  ✓ ADTR created:", nrow(adtr_simulated), "records\n")
  cat("  Runtime:", round(runtime, 2), "seconds\n\n")
  generation_log$adtr <- list(status = "SUCCESS", records = nrow(adtr_simulated))
}, error = function(e) {
  cat("  ✗ Error:", e$message, "\n\n")
  generation_log$adtr <- list(status = "ERROR", error = e$message)
})

## Step 5: ADRS (Response Evaluations) ----
cat("Step 5: Generating ADRS (Response Evaluations)...\n")
start_time <- Sys.time()

tryCatch({
  source("data-raw/S5_Generate_ADRS.R")
  runtime <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat("  ✓ ADRS created:", nrow(adrs_simulated), "records\n")
  cat("  Runtime:", round(runtime, 2), "seconds\n\n")
  generation_log$adrs <- list(status = "SUCCESS", records = nrow(adrs_simulated))
}, error = function(e) {
  cat("  ✗ Error:", e$message, "\n\n")
  generation_log$adrs <- list(status = "ERROR", error = e$message)
})

#===============================================================================
# SUMMARY
#===============================================================================

cat(strrep("=", 80), "\n")
cat("DATA GENERATION SUMMARY\n")
cat(strrep("=", 80), "\n\n")

# Create summary table
summary_table <- tibble(
  Dataset = c("ADSL", "ADTTE", "ADAE", "ADTR", "ADRS"),
  Status = sapply(generation_log, function(x) x$status),
  Records = sapply(generation_log, function(x) 
    if (!is.null(x$records)) x$records else 0)
)

print(summary_table)

cat("\n")
cat("Generated datasets saved to: data/\n")
cat("  - adsl_simulated.rds\n")
cat("  - adtte_simulated.rds\n")
cat("  - adae_simulated.rds\n")
cat("  - adtr_simulated.rds\n")
cat("  - adrs_simulated.rds\n")
cat("\n")

# Count successes
n_success <- sum(summary_table$Status == "SUCCESS")
n_total <- nrow(summary_table)

if (n_success == n_total) {
  cat("✓ All datasets generated successfully!\n")
} else {
  cat("⚠ ", n_total - n_success, "/", n_total, "datasets failed\n")
}

cat(strrep("=", 80), "\n\n")

#===============================================================================
# SAVE GENERATION LOG
#===============================================================================

log_file <- file.path("logs", paste0("data_generation_", 
                                     format(Sys.time(), "%Y%m%d_%H%M%S"), 
                                     ".log"))

sink(log_file)
cat("ER Standards Framework - Data Generation Log\n")
cat(strrep("=", 80), "\n\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Seed: 12345\n\n")
print(summary_table)
sink()

cat("Log saved to:", log_file, "\n\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================


#usethis::use_data(DATASET, overwrite = TRUE)
