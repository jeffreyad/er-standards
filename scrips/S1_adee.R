#===============================================================================
# Supplementary Material S1
# Exposure-Efficacy (ADEE) Analysis Example
# Standardizing Exposure-Response Data for Modeling and Simulation
#
# Purpose: Demonstrate complete workflow from ADEE dataset to exposure-efficacy
#          modeling using time-to-event endpoints (PFS, OS)
#
# Author: [Your Name]
# Date: 2026-01-20
#===============================================================================

# Required packages
required_packages <- c("dplyr", "ggplot2", "survival", "survminer", 
                       "tidyr", "patchwork", "broom")

# Install if needed
new_packages <- required_packages[!(required_packages %in% 
                                    installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(dplyr)
library(ggplot2)
library(survival)
library(survminer)
library(tidyr)
library(patchwork)
library(broom)

# Prevent namespace conflicts
select <- dplyr::select
filter <- dplyr::filter

# Create output directories
if (!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)
if (!dir.exists("output/tables")) dir.create("output/tables", recursive = TRUE)

#===============================================================================
# 1. DATA PREPARATION
#===============================================================================

cat("\n=== ADEE EXPOSURE-EFFICACY ANALYSIS ===\n\n")

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

# Save plot (ggsurvplot requires special handling)
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
hr_data <- tidy(cox_multi, conf.int = TRUE, exponentiate = TRUE)

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
# 4. PARAMETRIC TIME-TO-EVENT MODEL
#===============================================================================

cat("\n=== PARAMETRIC TIME-TO-EVENT MODEL ===\n\n")

## 4.1 Weibull Model with Exposure Effect ----

# Prepare data
adee_survreg <- adee_pfs %>%
  mutate(
    # Center covariates for interpretability
    EXPOSURE_C = EXPOSURE_VAR - mean(EXPOSURE_VAR, na.rm = TRUE),
    EXPOSURE_C10 = EXPOSURE_C / 10,  # Scale: effect per 10-unit increase
    WTBL_C = WTBL - mean(WTBL, na.rm = TRUE),
    AGE_C = AGE - mean(AGE, na.rm = TRUE)
  )

# Fit Weibull model
cat("Fitting Weibull model with exposure effect...\n\n")

weibull_model <- survreg(
  Surv(AVAL, EVENT) ~ EXPOSURE_C10,
  data = adee_survreg,
  dist = "weibull"
)

# Model summary
cat("\n=== Weibull Model Results ===\n")
print(summary(weibull_model))

# Extract parameters
# survreg uses log(T) = intercept + beta*X + scale*error
# Convert to standard Weibull parameterization
shape_param <- 1 / weibull_model$scale
cat("\nWeibull shape parameter:", round(shape_param, 3), "\n")
cat("  (shape > 1 indicates increasing hazard over time)\n\n")

# Acceleration factor (how exposure changes survival time)
accel_factor <- exp(coef(weibull_model)["EXPOSURE_C10"])
accel_factor_ci <- exp(confint(weibull_model)["EXPOSURE_C10", ])

cat("Acceleration Factor per 10-unit exposure increase:\n")
cat(sprintf("  AF = %.3f (95%% CI: %.3f - %.3f)\n", 
            accel_factor, accel_factor_ci[1], accel_factor_ci[2]))
cat("  → AF > 1 means higher exposure → longer survival time\n\n")

# Hazard ratio (inverse of acceleration factor)
hr <- 1 / accel_factor
hr_ci <- 1 / rev(accel_factor_ci)  # Reverse for proper CI bounds

cat("Hazard Ratio per 10-unit exposure increase:\n")
cat(sprintf("  HR = %.3f (95%% CI: %.3f - %.3f)\n",
            hr, hr_ci[1], hr_ci[2]))
cat("  → HR < 1 means higher exposure → lower risk of progression\n\n")

# Save parameter estimates
params_table <- data.frame(
  Parameter = c("Shape", "Acceleration_Factor", "Hazard_Ratio"),
  Estimate = c(shape_param, accel_factor, hr),
  CI_Lower = c(NA, accel_factor_ci[1], hr_ci[1]),
  CI_Upper = c(NA, accel_factor_ci[2], hr_ci[2])
)

write.csv(params_table, "output/tables/Table_S1_weibull_parameters.csv",
          row.names = FALSE)

## 4.2 Model Diagnostics ----

# Extract predicted survival times
adee_survreg$predicted <- predict(weibull_model, type = "response")

# Residuals
adee_survreg$residuals <- residuals(weibull_model, type = "deviance")

# Create diagnostic plots
pdf("output/figures/Figure_S1E_GOF_plots.pdf", width = 10, height = 10)
par(mfrow = c(2, 2))

# 1. Predicted vs Observed
plot(adee_survreg$predicted, adee_survreg$AVAL,
     xlab = "Predicted Survival Time (days)",
     ylab = "Observed Time (days)",
     main = "Predicted vs Observed",
     pch = ifelse(adee_survreg$EVENT == 1, 16, 1),
     col = ifelse(adee_survreg$EVENT == 1, "red", "blue"))
abline(0, 1, lty = 2)
legend("topleft", c("Event", "Censored"), 
       pch = c(16, 1), col = c("red", "blue"))

# 2. Residuals vs Predicted
plot(adee_survreg$predicted, adee_survreg$residuals,
     xlab = "Predicted Survival Time (days)",
     ylab = "Deviance Residuals",
     main = "Residuals vs Predicted",
     pch = 16, col = "steelblue")
abline(h = 0, lty = 2, col = "red")

# 3. Q-Q plot of residuals
qqnorm(adee_survreg$residuals, main = "Normal Q-Q Plot of Residuals")
qqline(adee_survreg$residuals, col = "red")

# 4. Residuals vs Exposure
plot(adee_survreg$EXPOSURE_VAR, adee_survreg$residuals,
     xlab = "Exposure (AUC)",
     ylab = "Deviance Residuals",
     main = "Residuals vs Exposure",
     pch = 16, col = "steelblue")
abline(h = 0, lty = 2, col = "red")

par(mfrow = c(1, 1))
dev.off()

cat("Goodness-of-fit plots saved: output/figures/Figure_S1E_GOF_plots.pdf\n\n")

## 4.3 Exposure-Response Curve ----

# Generate predictions across exposure range
exp_range <- seq(
  min(adee_pfs$EXPOSURE_VAR, na.rm = TRUE),
  max(adee_pfs$EXPOSURE_VAR, na.rm = TRUE),
  length.out = 100
)

# Create prediction data
pred_data <- data.frame(
  EXPOSURE_VAR = exp_range,
  EXPOSURE_C = exp_range - mean(adee_pfs$EXPOSURE_VAR, na.rm = TRUE),
  EXPOSURE_C10 = (exp_range - mean(adee_pfs$EXPOSURE_VAR, na.rm = TRUE)) / 10
)

# Predict median survival times
pred_data$median_tte <- predict(weibull_model, newdata = pred_data, 
                                 type = "quantile", p = 0.5)

# Predict 25th and 75th percentiles for uncertainty bands
pred_data$q25_tte <- predict(weibull_model, newdata = pred_data,
                              type = "quantile", p = 0.25)
pred_data$q75_tte <- predict(weibull_model, newdata = pred_data,
                              type = "quantile", p = 0.75)

# Plot
p5 <- ggplot() +
  # Prediction band (IQR)
  geom_ribbon(data = pred_data,
              aes(x = EXPOSURE_VAR, ymin = q25_tte, ymax = q75_tte),
              fill = "lightblue", alpha = 0.3) +
  # Median prediction line
  geom_line(data = pred_data,
            aes(x = EXPOSURE_VAR, y = median_tte),
            color = "blue", linewidth = 1.2) +
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
    subtitle = "Parametric Weibull Model (survival::survreg)",
    x = "Steady-State AUC (μg·h/mL)",
    y = "Median Time to Progression (days)",
    size = "Baseline\nWeight (kg)"
  ) +
  theme_bw() +
  theme(legend.position = "right")

ggsave("output/figures/Figure_S1F_ER_curve.pdf", 
       p5, width = 10, height = 7)

cat("Exposure-response curve saved: output/figures/Figure_S1F_ER_curve.pdf\n\n")

#===============================================================================
# 5. COVARIATE ANALYSIS
#===============================================================================

cat("\n=== COVARIATE ANALYSIS ===\n\n")

## 5.1 Full Covariate Model ----

weibull_cov_model <- survreg(
  Surv(AVAL, EVENT) ~ EXPOSURE_C10 + WTBL_C + AGE_C + SEX,
  data = adee_survreg,
  dist = "weibull"
)

cat("=== Full Covariate Model Results ===\n")
print(summary(weibull_cov_model))
cat("\n")

## 5.2 Model Comparison ----

# Compare models using AIC/BIC
comparison <- data.frame(
  Model = c("Base (Exposure only)", "Full Covariate"),
  AIC = c(AIC(weibull_model), AIC(weibull_cov_model)),
  BIC = c(BIC(weibull_model), BIC(weibull_cov_model)),
  LogLik = c(logLik(weibull_model), logLik(weibull_cov_model)),
  df = c(
    length(coef(weibull_model)) + 1,  # +1 for scale parameter
    length(coef(weibull_cov_model)) + 1
  )
)

cat("\n=== Model Comparison ===\n")
print(comparison)

write.csv(comparison, "output/tables/Table_S2_model_comparison.csv", 
          row.names = FALSE)

# Likelihood ratio test
lrt <- -2 * (logLik(weibull_model) - logLik(weibull_cov_model))
lrt_df <- comparison$df[2] - comparison$df[1]
lrt_p <- pchisq(as.numeric(lrt), df = lrt_df, lower.tail = FALSE)

cat("\nLikelihood Ratio Test:\n")
cat(sprintf("  Chi-square = %.2f, df = %d, p-value = %.4f\n", 
            lrt, lrt_df, lrt_p))

if (lrt_p < 0.05) {
  cat("  → Covariates significantly improve model fit\n\n")
  final_model <- weibull_cov_model
} else {
  cat("  → Base model adequate\n\n")
  final_model <- weibull_model
}

## 5.3 Acceleration Factors for All Covariates ----

coef_table <- data.frame(
  Covariate = names(coef(weibull_cov_model))[-1],  # Exclude intercept
  Acceleration_Factor = exp(coef(weibull_cov_model)[-1]),
  row.names = NULL
)

# Add confidence intervals
ci_matrix <- confint(weibull_cov_model)
coef_table$CI_Lower <- exp(ci_matrix[-1, 1])
coef_table$CI_Upper <- exp(ci_matrix[-1, 2])

cat("Acceleration Factors (AF > 1 indicates longer survival):\n")
print(coef_table)
cat("\n")

# Forest plot
p_forest <- coef_table %>%
  mutate(
    Covariate = recode(Covariate,
                       "EXPOSURE_C10" = "Exposure (+10 units)",
                       "WTBL_C" = "Weight (+1 kg)",
                       "AGE_C" = "Age (+1 year)",
                       "SEXM" = "Sex (Male vs Female)")
  ) %>%
  ggplot(aes(x = Acceleration_Factor, 
             y = forcats::fct_reorder(Covariate, Acceleration_Factor))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2) +
  geom_point(size = 3) +
  scale_x_log10() +
  labs(
    title = "Acceleration Factors from Weibull Model",
    subtitle = "AF > 1 indicates longer survival time",
    x = "Acceleration Factor (95% CI, log scale)",
    y = ""
  ) +
  theme_bw()

ggsave("output/figures/Figure_S1D_acceleration_factors.pdf", 
       p_forest, width = 8, height = 5)

#===============================================================================
# 6. DOSE SELECTION SIMULATION
#===============================================================================

cat("\n=== DOSE SELECTION SIMULATION ===\n\n")

# Simulate outcomes at different dose levels
# Assume linear dose-exposure relationship from population PK
dose_exp_slope <- 0.12  # Example: 100mg → 12 μg·h/mL

dose_scenarios <- data.frame(
  Dose_mg = c(50, 75, 100, 150, 200),
  Expected_AUC = c(50, 75, 100, 150, 200) * dose_exp_slope
)

# Function to simulate median PFS at given exposure
simulate_median_pfs <- function(exposure, model, n_sim = 1000) {
  
  # Center exposure (same as in model fitting)
  exposure_c <- exposure - mean(adee_survreg$EXPOSURE_VAR, na.rm = TRUE)
  exposure_c10 <- exposure_c / 10
  
  # Create prediction data
  newdata <- data.frame(EXPOSURE_C10 = exposure_c10)
  
  # Get predicted median from model
  median_pred <- predict(model, newdata = newdata, type = "quantile", p = 0.5)
  
  # Get 25th and 75th percentiles for uncertainty
  q25_pred <- predict(model, newdata = newdata, type = "quantile", p = 0.25)
  q75_pred <- predict(model, newdata = newdata, type = "quantile", p = 0.75)
  
  # For simulation, extract Weibull parameters
  shape <- 1 / model$scale
  scale_log <- coef(model)["(Intercept)"] + coef(model)["EXPOSURE_C10"] * exposure_c10
  scale_param <- exp(scale_log)
  
  # Simulate from Weibull
  simulated_times <- rweibull(n_sim, shape = shape, scale = scale_param)
  
  return(c(
    median = median_pred,
    q25 = q25_pred,
    q75 = q75_pred,
    mean = mean(simulated_times),
    sd = sd(simulated_times)
  ))
}

# Run simulations
cat("Simulating PFS at different dose levels...\n")

dose_scenarios <- dose_scenarios %>%
  rowwise() %>%
  mutate(
    sim_results = list(simulate_median_pfs(Expected_AUC, weibull_model, n_sim = 1000))
  ) %>%
  ungroup() %>%
  mutate(
    Median_PFS_days = sapply(sim_results, function(x) x["median"]),
    Q25_PFS_days = sapply(sim_results, function(x) x["q25"]),
    Q75_PFS_days = sapply(sim_results, function(x) x["q75"]),
    Mean_PFS_days = sapply(sim_results, function(x) x["mean"]),
    SD_PFS_days = sapply(sim_results, function(x) x["sd"])
  ) %>%
  select(-sim_results)

cat("\n=== Simulated PFS by Dose ===\n")
print(dose_scenarios)
cat("\n")

write.csv(dose_scenarios, 
          "output/tables/Table_S3_dose_selection_simulation.csv",
          row.names = FALSE)

# Visualization
p6 <- ggplot(dose_scenarios, 
             aes(x = Dose_mg, y = Median_PFS_days)) +
  geom_ribbon(aes(ymin = Q25_PFS_days, ymax = Q75_PFS_days),
              fill = "lightblue", alpha = 0.3) +
  geom_line(linewidth = 1.2, color = "blue") +
  geom_point(size = 3, color = "blue") +
  geom_text(aes(label = sprintf("%.0f days", Median_PFS_days)),
            vjust = -1.5, size = 3) +
  scale_x_continuous(breaks = dose_scenarios$Dose_mg) +
  labs(
    title = "Simulated PFS by Dose Level",
    subtitle = "Based on parametric Weibull E-R model (median and IQR)",
    x = "Dose (mg)",
    y = "Median PFS (days)"
  ) +
  theme_bw()

ggsave("output/figures/Figure_S1G_dose_simulation.pdf",
       p6, width = 8, height = 6)

cat("Dose simulation plot saved: output/figures/Figure_S1G_dose_simulation.pdf\n\n")

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
cat("Weight < 70 kg: HR =", round(exp(coef(cox_wt_low)), 3), "\n")
cat("  95% CI:", round(exp(confint(cox_wt_low)), 3), "\n")
cat("Weight ≥ 70 kg: HR =", round(exp(coef(cox_wt_high)), 3), "\n")
cat("  95% CI:", round(exp(confint(cox_wt_high)), 3), "\n\n")

# Test interaction
cox_interaction <- coxph(Surv(AVAL, EVENT) ~ EXPOSURE_VAR * WT_GROUP,
                         data = adee_pfs_subgroup)

cat("Interaction test (p-value):", 
    round(summary(cox_interaction)$coefficients["EXPOSURE_VAR:WT_GROUP≥ 70 kg", "Pr(>|z|)"], 4), 
    "\n\n")

## 7.2 Visualization by Subgroups ----

# Fit Weibull models for each subgroup
subgroup_models <- list(
  "< 70 kg" = survreg(Surv(AVAL, EVENT) ~ EXPOSURE_VAR,
                       data = adee_pfs_subgroup %>% filter(WT_GROUP == "< 70 kg"),
                       dist = "weibull"),
  "≥ 70 kg" = survreg(Surv(AVAL, EVENT) ~ EXPOSURE_VAR,
                       data = adee_pfs_subgroup %>% filter(WT_GROUP == "≥ 70 kg"),
                       dist = "weibull")
)

# Generate predictions
exp_range <- seq(
  min(adee_pfs$EXPOSURE_VAR, na.rm = TRUE),
  max(adee_pfs$EXPOSURE_VAR, na.rm = TRUE),
  length.out = 100
)

subgroup_predictions <- expand.grid(
  EXPOSURE_VAR = exp_range,
  WT_GROUP = c("< 70 kg", "≥ 70 kg")
) %>%
  mutate(median_tte = NA_real_)

# Predict for each subgroup
for (grp in c("< 70 kg", "≥ 70 kg")) {
  pred_data <- data.frame(EXPOSURE_VAR = exp_range)
  predictions <- predict(subgroup_models[[grp]], 
                          newdata = pred_data, 
                          type = "quantile", 
                          p = 0.5)
  subgroup_predictions$median_tte[subgroup_predictions$WT_GROUP == grp] <- predictions
}

p7 <- ggplot() +
  geom_line(data = subgroup_predictions,
            aes(x = EXPOSURE_VAR, y = median_tte, color = WT_GROUP),
            linewidth = 1.2) +
  geom_point(data = adee_pfs_subgroup %>% filter(EVENT == 1),
             aes(x = EXPOSURE_VAR, y = AVAL, color = WT_GROUP),
             alpha = 0.4) +
  facet_wrap(~WT_GROUP) +
  scale_color_manual(values = c("< 70 kg" = "#E41A1C", "≥ 70 kg" = "#4DAF4A")) +
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

cat("Subgroup analysis plot saved: output/figures/Figure_S1H_subgroup_analysis.pdf\n\n")

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
    weibull_shape = shape_param,
    acceleration_factor = accel_factor,
    hazard_ratio = hr,
    median_pfs_at_median_exposure = median(pred_data$median_tte)
  ),
  
  key_findings = list(
    cox_hr_per_unit_auc = exp(coef(cox_cont)),
    cox_hr_95ci = exp(confint(cox_cont)),
    cox_pvalue = summary(cox_cont)$coefficients[,"Pr(>|z|)"]
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
cat("  - Figure_S1D_hazard_ratios.pdf (Cox model)\n")
cat("  - Figure_S1D_acceleration_factors.pdf (Weibull model)\n")
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