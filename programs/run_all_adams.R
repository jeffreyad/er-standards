# ===============================================================================
# Master Script: Run All ADaM Derivations
#
# Purpose: Execute all ER Standards ADaM derivations in sequence
#
# Author: Jeff Dickinson
# Date: 2026-01-22
#
# ===============================================================================

# Set working directory (adjust as needed)
# setwd("path/to/your/project")

# Clear environment (optional - use with caution)
# rm(list = ls())

# Load required packages
library(admiral)
library(admiralonco)
library(dplyr)
library(pharmaverseadam)

# ===============================================================================
# CONFIGURATION
# ===============================================================================

# Set exposure source
source("config/exposure_config.R")

# Datasets to create (in dependency order)
datasets <- c("ader", "adee", "ades", "adtrr")

# Track timing and status
results <- data.frame(
  Dataset = character(),
  Status = character(),
  Records = integer(),
  Runtime_Sec = numeric(),
  Error = character(),
  stringsAsFactors = FALSE
)

# ===============================================================================
# RUN DERIVATIONS
# ===============================================================================

cat("\n")
cat(strrep("=", 80), "\n")
cat("ER STANDARDS FRAMEWORK - BATCH EXECUTION\n")
cat(strrep("=", 80), "\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Exposure source:", EXPOSURE_SOURCE, "\n")
cat(strrep("=", 80), "\n\n")

for (ds in datasets) {
  cat(strrep("-", 80), "\n")
  cat("Processing:", ds, "\n")
  cat(strrep("-", 80), "\n")

  script_path <- file.path("programs", paste0("ad_", ds, ".R"))
  output_path <- file.path("adam", paste0(tolower(ds), ".rds"))

  # Check if script exists
  if (!file.exists(script_path)) {
    cat("✗ Script not found:", script_path, "\n\n")
    results <- rbind(results, data.frame(
      Dataset = ds,
      Status = "FAILED",
      Records = 0,
      Runtime_Sec = 0,
      Error = "Script not found"
    ))
    next
  }

  # Run script
  start_time <- Sys.time()

  tryCatch(
    {
      source(script_path, echo = FALSE)

      end_time <- Sys.time()
      runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))

      # Check if output was created
      if (file.exists(output_path)) {
        dataset <- readRDS(output_path)
        n_records <- nrow(dataset)

        cat("✓ SUCCESS:", ds, "created\n")
        cat("  Records:", n_records, "\n")
        cat("  Runtime:", round(runtime, 2), "seconds\n\n")

        results <- rbind(results, data.frame(
          Dataset = ds,
          Status = "SUCCESS",
          Records = n_records,
          Runtime_Sec = round(runtime, 2),
          Error = ""
        ))
      } else {
        cat("✗ FAILED: Output not created\n\n")
        results <- rbind(results, data.frame(
          Dataset = ds,
          Status = "FAILED",
          Records = 0,
          Runtime_Sec = round(runtime, 2),
          Error = "Output not created"
        ))
      }
    },
    error = function(e) {
      end_time <- Sys.time()
      runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))

      cat("✗ ERROR:", ds, "\n")
      cat("  Message:", e$message, "\n\n")

      results <<- rbind(results, data.frame(
        Dataset = ds,
        Status = "ERROR",
        Records = 0,
        Runtime_Sec = round(runtime, 2),
        Error = substr(e$message, 1, 100)
      ))
    }
  )
}

# ===============================================================================
# SUMMARY
# ===============================================================================

cat(strrep("=", 80), "\n")
cat("BATCH EXECUTION SUMMARY\n")
cat(strrep("=", 80), "\n")
cat("End time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Total runtime:", round(sum(results$Runtime_Sec), 2), "seconds\n\n")

print(results)

# Count successes/failures
n_success <- sum(results$Status == "SUCCESS")
n_failed <- sum(results$Status %in% c("FAILED", "ERROR"))

cat("\n")
cat("Results:", n_success, "succeeded,", n_failed, "failed\n")
cat(strrep("=", 80), "\n\n")

# Return results invisibly
invisible(results)
