#===============================================================================
# Supplementary Material S1
# Exposure-Efficacy (ADEE) Analysis Example
# Standardizing Exposure-Response Data for Modeling and Simulation
#
# Purpose: Demonstrate complete workflow from ADEE dataset to exposure-efficacy
#          modeling using time-to-event endpoints (PFS, OS)
#
# Author: [Your Name]
# Date: 2026-01-17
#===============================================================================

# Required packages
required_packages <- c("nlmixr2", "dplyr", "ggplot2", "survival", 
                       "survminer", "tidyr", "patchwork")

# Install if needed
new_packages <- required_packages[!(required_packages %in% 
                                    installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(nlmixr2)
library(dplyr)
library(ggplot2)
library(survival)
library(survminer)
library(tidyr)
library(patchwork)

#===============================================================================
# 1. DATA PREPARATION
#===============================================================================

# Load ADEE dataset
# This dataset follows the ADEE structure proposed in the manuscript:
# - One record per subject per parameter per timepoint
# - PARAMCD: "PFS" (Progression-Free Survival) or "OS" (Overall Survival)
# - AVAL: Days from treatment start to event/censoring
# - CNSR: Censoring indicator (1 = censored, 0 = event)
# - EVENT: Event indicator (1 = event, 0 = censored)
# - EXPOSURE_VAR: Continuous exposure (e.g., AUC_SS, Cavg)
# - EXPOSURE_CAT: Categorical exposure (tertiles, quartiles)
# - Multiple analysis flags (ANL01FL, ANL02FL)

adee <- read.csv("data/adee_example.csv")

# Inspect structure
cat("\n=== ADEE Dataset Structure ===\n")
str(adee)

cat("\n=== First 10 Records ===\n")
print(head(adee, 10))

cat("\n=== Summary Statistics ===\n")
summary(adee %>% select(AVAL, CNSR, EXPOSURE_VAR, EXPOSURE_CAT))

# Check for missing exposure values
cat("\n=== Missing Exposure Data ===\n")
cat("Records with missing EXPOSURE_VAR:", sum(is.na(adee$EXPOSURE_VAR)), "\n")

#===============================================================================
# 2. EXPLORATORY ANALYSIS
#===============================================================================

cat("\n=== EXPLORATORY ANALYSIS ===\n\n")

## 2.1 Kaplan-Meier Analysis by Exposure Category ----

# Filter to primary analysis population (PFS endpoint)
adee_pfs <- adee %>% 
  filter(PARAMCD == "PFS" & ANL01FL == "Y")

cat("Analysis population (PFS, ANL01FL='Y'):", nrow(adee_pfs), "subjects\n")

# Fit KM curves
km_fit <- survfit(Surv(AVAL, EVENT) ~ EXPOSURE_CAT, data = adee_pfs)

# Create KM plot
p1 <- ggsurvplot(
  km_fit, 
  data = adee_pfs,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  palette = c("#E41A1C", "#377EB8", "#4DAF4A"),
  title = "Progression-Free Survival by Exposure Tertile",
  xlab = "Time (days)",
  ylab = "Progression-Free Survival Probability",
  legend.title = "Exposure Tertile",
  legend.labs = c("Low", "Medium", "High"),
  risk.table.height = 0.25
)

# FIX: Save properly for ggsurvplot objects
pdf("output/figures/Figure_S1A_KM_by_exposure.pdf", width = 10, height = 8)
print(p1)
dev.off()

cat("\nKaplan-Meier plot saved: output/figures/Figure_S1A_KM_by_exposure.pdf\n")

## 2.2 Exposure Distribution ----

p2 <- ggplot(adee_pfs, aes(x = EXPOSURE_VAR, fill = EXPOSURE_CAT)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  facet_wrap(~EXPOSURE_CAT, ncol = 1) +
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
  labs(title = "Exposure Distribution by Tertile",
       x = "Exposure (AUC, μg·h/mL)",
       y = "Count") +
  theme_bw() +
  theme(legend.position = "none")

ggsave("output/figures/Figure_S1B_exposure_distribution.pdf", 
       p2, width = 8, height = 6)

## 2.3 Raw Exposure-Response Scatter ----

p3 <- ggplot(adee_pfs, aes(x = EXPOSURE_VAR, y = AVAL, color = factor(EVENT))) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  scale_color_manual(values = c("0" = "blue", "1" = "red"),
                     labels = c("Censored", "Event")) +
  labs(title = "Raw Exposure-Response Relationship",
       x = "Exposure (AUC, μg·h/mL)",
       y = "Time to Event/Censoring (days)",
       color = "Status") +
  theme_bw()

ggsave("output/figures/Figure_S1C_raw_er_scatter.pdf", 
       p3, width = 8, height = 6)

#===============================================================================
# 3. COX PROPORTIONAL HAZARDS MODEL
#===============================================================================

cat("\n=== COX PROPORTIONAL HAZARDS MODEL ===\n\n")

## 3.1 Univariate Cox Model ----

# Continuous exposure
cox_cont <- coxph(Surv(AVAL, EVENT) ~ EXPOSURE_VAR, data = adee_pfs)

cat("Univariate Cox Model (Continuous Exposure):\n")
print(summary(cox_cont))

# Categorical exposure
cox_cat <- coxph(Surv(AVAL, EVENT) ~ EXPOSURE_CAT, data = adee_pfs)

cat("\n\nUnivariate Cox Model (Categorical Exposure):\n")
print(summary(cox_cat))

# Test proportional hazards assumption
ph_test <- cox.zph(cox_cont)
cat("\n\nProportional Hazards Test:\n")
print(ph_test)

## 3.2 Multivariable Cox Model with Covariates ----

# Include baseline weight, age, sex as covariates
cox_multi <- coxph(Surv(AVAL, EVENT) ~ EXPOSURE_VAR + WTBL + AGE + SEX, 
                   data = adee_pfs)

cat("\n\nMultivariable Cox Model:\n")
print(summary(cox_multi))

# Forest plot of hazard ratios
hr_data <- broom::tidy(cox_multi, conf.int = TRUE, exponentiate = TRUE)

p4 <- ggplot(hr_data, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(size = 3) +
  scale_x_log10() +
  labs(title = "Hazard Ratios from Multivariable Cox Model",
       x = "Hazard Ratio (95% CI)",
       y = "") +
  theme_bw()

ggsave("output/figures/Figure_S1D_hazard_ratios.pdf", 
       p4, width = 8, height = 5)

#===============================================================================
# 4. PARAMETRIC TIME-TO-EVENT MODEL (NLMIXR2)
#===============================================================================

cat("\n=== PARAMETRIC TIME-TO-EVENT MODEL ===\n\n")

## 4.1 Weibull Model with Exposure Effect ----

# Prepare data for nlmixr2
# nlmixr2 expects specific column names for survival analysis
adee_nlmixr <- adee_pfs %>%
  mutate(
    ID = as.numeric(factor(USUBJID)),
    TIME = AVAL,
    DV = EVENT,
    EVID = 0,  # All observation records
    # Center covariates
    EXPOSURE_C = EXPOSURE_VAR - mean(EXPOSURE_VAR, na.rm = TRUE),
    WTBL_C = WTBL - mean(WTBL, na.rm = TRUE),
    AGE_C = AGE - mean(AGE, na.rm = TRUE)
  )

# Define Weibull time-to-event model
# Hazard function: h(t) = (shape/scale) * (t/scale)^(shape-1) * exp(beta*exposure)
# Scale parameter includes exposure effect

weibull_model <- function() {
  ini({
    # Log-transformed parameters
    log_shape <- log(1.5)      # Weibull shape parameter
    log_scale0 <- log(180)     # Baseline scale (median ~ 180 days)
    beta_exp <- 0.01           # Exposure coefficient
    
    # Between-subject variability
    eta_scale ~ 0.2
  })
  
  model({
    # Back-transform
    shape <- exp(log_shape)
    
    # Scale with exposure effect and random effect
    scale <- exp(log_scale0 + beta_exp * EXPOSURE_C + eta_scale)
    
    # Weibull hazard
    # For time-to-event, nlmixr2 uses special syntax
    hazard <- (shape / scale) * (TIME / scale)^(shape - 1)
    
    # Define survival endpoint
    TIME ~ weibull(shape, scale)
  })
}

cat("Fitting Weibull model with exposure effect...\n")
cat("This may take a few minutes...\n\n")

# Fit model using SAEM for faster estimation
fit_weibull <- nlmixr(
  weibull_model, 
  data = adee_nlmixr,
  est = "saem",
  control = saemControl(
    nBurn = 200,
    nEm = 300,
    print = 50
  )
)

# Model summary
cat("\n=== Weibull Model Results ===\n")
print(fit_weibull)

# Save parameter estimates
params_table <- as.data.frame(fit_weibull$parFixed)
write.csv(params_table, "output/tables/Table_S1_weibull_parameters.csv")

## 4.2 Model Diagnostics ----

# Standard GOF plots
pdf("output/figures/Figure_S1E_GOF_plots.pdf", width = 10, height = 10)
plot(fit_weibull)
dev.off()

cat("\nGoodness-of-fit plots saved: output/figures/Figure_S1E_GOF_plots.pdf\n")

## 4.3 Exposure-Response Curve ----

# Generate predictions across exposure range
exp_range <- seq(
  min(adee_pfs$EXPOSURE_VAR, na.rm = TRUE),
  max(adee_pfs$EXPOSURE_VAR, na.rm = TRUE),
  length.out = 100
)

# Extract parameter estimates
params <- fit_weibull$parFixed
shape_est <- exp(params["log_shape"])
scale0_est <- exp(params["log_scale0"])
beta_est <- params["beta_exp"]

# Calculate median TTE at each exposure level
pred_data <- data.frame(
  EXPOSURE_VAR = exp_range,
  EXPOSURE_C = exp_range - mean(adee_pfs$EXPOSURE_VAR, na.rm = TRUE)
) %>%
  mutate(
    scale = exp(scale0_est + beta_est * EXPOSURE_C),
    median_tte = scale * (log(2))^(1/shape_est),
    # 95% prediction interval (approximate)
    lower_tte = scale * (log(2/0.975))^(1/shape_est),
    upper_tte = scale * (log(2/0.025))^(1/shape_est)
  )

p5 <- ggplot() +
  # Prediction band
  geom_ribbon(data = pred_data, 
              aes(x = EXPOSURE_VAR, ymin = lower_tte, ymax = upper_tte),
              fill = "lightblue", alpha = 0.3) +
  # Median prediction line
  geom_line(data = pred_data, 
            aes(x = EXPOSURE_VAR, y = median_tte),
            color = "blue", size = 1.2) +
  # Observed data points (only events, sized by weight)
  geom_point(data = adee_pfs %>% filter(EVENT == 1),
             aes(x = EXPOSURE_VAR, y = AVAL, size = WTBL),
             alpha = 0.4, color = "red") +
  # Censored points
  geom_point(data = adee_pfs %>% filter(EVENT == 0),
             aes(x = EXPOSURE_VAR, y = AVAL),
             alpha = 0.4, color = "gray", shape = 3) +
  labs(
    title = "Exposure-Response Relationship for PFS",
    subtitle = "Parametric Weibull Model (nlmixr2 SAEM estimation)",
    x = "Steady-State AUC (μg·h/mL)",
    y = "Median Time to Progression (days)",
    size = "Baseline\nWeight (kg)"
  ) +
  theme_bw() +
  theme(legend.position = "right")

ggsave("output/figures/Figure_S1F_ER_curve.pdf", 
       p5, width = 10, height = 7)

#===============================================================================
# 5. COVARIATE ANALYSIS
#===============================================================================

cat("\n=== COVARIATE ANALYSIS ===\n\n")

## 5.1 Full Covariate Model ----

weibull_cov_model <- function() {
  ini({
    log_shape <- log(1.5)
    log_scale0 <- log(180)
    beta_exp <- 0.01
    beta_wt <- 0            # Weight effect
    beta_age <- 0           # Age effect
    beta_sex <- 0           # Sex effect (M vs F)
    
    eta_scale ~ 0.2
  })
  
  model({
    shape <- exp(log_shape)
    
    # Scale with all covariate effects
    scale <- exp(
      log_scale0 + 
      beta_exp * EXPOSURE_C + 
      beta_wt * WTBL_C +
      beta_age * AGE_C +
      beta_sex * (SEX == "M") +
      eta_scale
    )
    
    TIME ~ weibull(shape, scale)
  })
}

cat("Fitting full covariate model...\n")

fit_cov <- nlmixr(
  weibull_cov_model,
  data = adee_nlmixr,
  est = "saem",
  control = saemControl(nBurn = 200, nEm = 300, print = 50)
)

cat("\n=== Full Covariate Model Results ===\n")
print(fit_cov)

## 5.2 Model Comparison ----

# Compare models using AIC/BIC
comparison <- data.frame(
  Model = c("Base (Exposure only)", "Full Covariate"),
  AIC = c(AIC(fit_weibull), AIC(fit_cov)),
  BIC = c(BIC(fit_weibull), BIC(fit_cov)),
  LogLik = c(logLik(fit_weibull), logLik(fit_cov))
)

cat("\n=== Model Comparison ===\n")
print(comparison)

write.csv(comparison, "output/tables/Table_S2_model_comparison.csv", 
          row.names = FALSE)

#===============================================================================
# 6. DOSE SELECTION SIMULATION
#===============================================================================

cat("\n=== DOSE SELECTION SIMULATION ===\n\n")

# Simulate outcomes at different dose levels
# Assume linear dose-exposure relationship from population PK
# Dose (mg) → AUC (μg·h/mL) with slope from PopPK

dose_exp_slope <- 0.12  # Example: 100mg → 12 μg·h/mL

dose_scenarios <- data.frame(
  Dose_mg = c(50, 75, 100, 150, 200),
  Expected_AUC = c(50, 75, 100, 150, 200) * dose_exp_slope
)

# Function to simulate median PFS at given exposure
simulate_median_pfs <- function(exposure, params, n_sim = 1000) {
  shape <- exp(params["log_shape"])
  scale0 <- exp(params["log_scale0"])
  beta <- params["beta_exp"]
  
  # Center exposure
  exposure_c <- exposure - mean(adee_pfs$EXPOSURE_VAR, na.rm = TRUE)
  
  # Scale parameter
  scale <- exp(scale0 + beta * exposure_c)
  
  # Simulate TTE
  tte_sim <- rweibull(n_sim, shape = shape, scale = scale)
  
  # Return summary
  c(
    median = median(tte_sim),
    q25 = quantile(tte_sim, 0.25),
    q75 = quantile(tte_sim, 0.75),
    mean = mean(tte_sim)
  )
}

# Run simulations
params_est <- fit_weibull$parFixed

dose_scenarios <- dose_scenarios %>%
  rowwise() %>%
  mutate(
    Median_PFS_days = simulate_median_pfs(Expected_AUC, params_est)["median"],
    Q25_PFS_days = simulate_median_pfs(Expected_AUC, params_est)["q25"],
    Q75_PFS_days = simulate_median_pfs(Expected_AUC, params_est)["q75"],
    Mean_PFS_days = simulate_median_pfs(Expected_AUC, params_est)["mean"]
  ) %>%
  ungroup()

cat("\n=== Simulated PFS by Dose ===\n")
print(dose_scenarios)

write.csv(dose_scenarios, 
          "output/tables/Table_S3_dose_selection_simulation.csv",
          row.names = FALSE)

# Visualization
p6 <- ggplot(dose_scenarios, 
             aes(x = Dose_mg, y = Median_PFS_days)) +
  geom_ribbon(aes(ymin = Q25_PFS_days, ymax = Q75_PFS_days),
              fill = "lightblue", alpha = 0.3) +
  geom_line(size = 1.2, color = "blue") +
  geom_point(size = 3, color = "blue") +
  labs(
    title = "Simulated PFS by Dose Level",
    subtitle = "Based on parametric E-R model (median and IQR)",
    x = "Dose (mg)",
    y = "Median PFS (days)"
  ) +
  theme_bw()

ggsave("output/figures/Figure_S1G_dose_simulation.pdf",
       p6, width = 8, height = 6)

#===============================================================================
# 7. SUBGROUP ANALYSIS
#===============================================================================

cat("\n=== SUBGROUP ANALYSIS ===\n\n")

# Analyze exposure-response in clinically relevant subgroups

## 7.1 By Baseline Weight (< 70 kg vs >= 70 kg) ----

adee_pfs_subgroup <- adee_pfs %>%
  mutate(
    WT_GROUP = ifelse(WTBL < 70, "< 70 kg", "≥ 70 kg"),
    AGE_GROUP = ifelse(AGE < 65, "< 65 yrs", "≥ 65 yrs")
  )

# Fit Cox models by subgroup
cox_wt_low <- coxph(Surv(AVAL, EVENT) ~ EXPOSURE_VAR, 
                     data = adee_pfs_subgroup %>% filter(WT_GROUP == "< 70 kg"))
cox_wt_high <- coxph(Surv(AVAL, EVENT) ~ EXPOSURE_VAR,
                      data = adee_pfs_subgroup %>% filter(WT_GROUP == "≥ 70 kg"))

cat("Exposure effect (HR per unit AUC):\n")
cat("Weight < 70 kg: HR =", exp(coef(cox_wt_low)), 
    "95% CI:", exp(confint(cox_wt_low)), "\n")
cat("Weight ≥ 70 kg: HR =", exp(coef(cox_wt_high)),
    "95% CI:", exp(confint(cox_wt_high)), "\n")

# Test interaction
cox_interaction <- coxph(Surv(AVAL, EVENT) ~ EXPOSURE_VAR * WT_GROUP,
                         data = adee_pfs_subgroup)

cat("\nInteraction test (p-value):", 
    summary(cox_interaction)$coefficients["EXPOSURE_VAR:WT_GROUP≥ 70 kg", "Pr(>|z|)"], 
    "\n")

## 7.2 Visualization by Subgroups ----

# Create ER curves for each subgroup
subgroup_predictions <- expand.grid(
  EXPOSURE_VAR = exp_range,
  WT_GROUP = c("< 70 kg", "≥ 70 kg")
) %>%
  mutate(
    EXPOSURE_C = EXPOSURE_VAR - mean(adee_pfs$EXPOSURE_VAR, na.rm = TRUE),
    scale = exp(scale0_est + beta_est * EXPOSURE_C),
    median_tte = scale * (log(2))^(1/shape_est)
  )

p7 <- ggplot() +
  geom_line(data = subgroup_predictions,
            aes(x = EXPOSURE_VAR, y = median_tte, color = WT_GROUP),
            size = 1.2) +
  geom_point(data = adee_pfs_subgroup %>% filter(EVENT == 1),
             aes(x = EXPOSURE_VAR, y = AVAL, color = WT_GROUP),
             alpha = 0.4) +
  facet_wrap(~WT_GROUP) +
  labs(
    title = "Exposure-Response by Weight Subgroup",
    x = "Exposure (AUC, μg·h/mL)",
    y = "Median PFS (days)",
    color = "Weight Group"
  ) +
  theme_bw() +
  theme(legend.position = "none")

ggsave("output/figures/Figure_S1H_subgroup_analysis.pdf",
       p7, width = 10, height = 5)

#===============================================================================
# 8. SUMMARY AND EXPORT
#===============================================================================

cat("\n=== ANALYSIS COMPLETE ===\n\n")

# Create summary report
summary_stats <- list(
  dataset_info = list(
    n_subjects = length(unique(adee_pfs$USUBJID)),
    n_events = sum(adee_pfs$EVENT),
    n_censored = sum(1 - adee_pfs$EVENT),
    median_followup = median(adee_pfs$AVAL)
  ),
  
  exposure_summary = list(
    mean_auc = mean(adee_pfs$EXPOSURE_VAR, na.rm = TRUE),
    sd_auc = sd(adee_pfs$EXPOSURE_VAR, na.rm = TRUE),
    range_auc = range(adee_pfs$EXPOSURE_VAR, na.rm = TRUE)
  ),
  
  model_results = list(
    weibull_shape = exp(params_est["log_shape"]),
    baseline_scale = exp(params_est["log_scale0"]),
    exposure_coefficient = params_est["beta_exp"],
    median_pfs_at_median_exposure = median(pred_data$median_tte)
  ),
  
  key_findings = list(
    hr_per_unit_auc = exp(coef(cox_cont)),
    hr_95ci = exp(confint(cox_cont)),
    pvalue = summary(cox_cont)$coefficients[,"Pr(>|z|)"]
  )
)

# Save summary
saveRDS(summary_stats, "output/summary_stats_adee.rds")

cat("Summary statistics saved: output/summary_stats_adee.rds\n")

# Session info for reproducibility
sink("output/session_info_adee.txt")
cat("=== R Session Information ===\n\n")
print(sessionInfo())
sink()

cat("\n=== All outputs saved in output/ directory ===\n")
cat("\nFigures:\n")
cat("  - Figure_S1A_KM_by_exposure.pdf\n")
cat("  - Figure_S1B_exposure_distribution.pdf\n")
cat("  - Figure_S1C_raw_er_scatter.pdf\n")
cat("  - Figure_S1D_hazard_ratios.pdf\n")
cat("  - Figure_S1E_GOF_plots.pdf\n")
cat("  - Figure_S1F_ER_curve.pdf\n")
cat("  - Figure_S1G_dose_simulation.pdf\n")
cat("  - Figure_S1H_subgroup_analysis.pdf\n")

cat("\nTables:\n")
cat("  - Table_S1_weibull_parameters.csv\n")
cat("  - Table_S2_model_comparison.csv\n")
cat("  - Table_S3_dose_selection_simulation.csv\n")

cat("\n=== END OF ADEE ANALYSIS ===\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================