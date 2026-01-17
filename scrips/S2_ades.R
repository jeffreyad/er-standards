#===============================================================================
# Supplementary Material S2
# Exposure-Safety (ADES) Analysis Example
# Standardizing Exposure-Response Data for Modeling and Simulation
#
# Purpose: Demonstrate complete workflow from ADES dataset to exposure-safety
#          modeling using adverse event data with multi-level structure
#
# Author: [Your Name]
# Date: 2026-01-17
#===============================================================================

# Required packages
required_packages <- c("dplyr", "ggplot2", "tidyr", "MASS", "pscl",
                       "broom", "patchwork", "scales", "forcats")

# Install if needed
new_packages <- required_packages[!(required_packages %in% 
                                    installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(MASS)      # For glm.nb (negative binomial)
library(pscl)      # For zero-inflated models
library(broom)     # For tidy model output
library(patchwork) # For combining plots
library(scales)    # For formatting
library(forcats)   # For factor manipulation

# Prevent MASS::select from masking dplyr::select
select <- dplyr::select

# Create output directories
if (!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)
if (!dir.exists("output/tables")) dir.create("output/tables", recursive = TRUE)

#===============================================================================
# 1. DATA PREPARATION
#===============================================================================

cat("\n=== ADES EXPOSURE-SAFETY ANALYSIS ===\n\n")

# Load ADES dataset
ades <- read.csv("data/ades_example.csv")

cat("=== ADES Dataset Structure ===\n")
cat("Total records:", nrow(ades), "\n")
cat("Multi-level structure:\n")
cat("  - Subject-level (PARAMCD='SUBJSUM'):", 
    sum(ades$PARAMCD == "SUBJSUM"), "\n")
cat("  - Event-level (PARAMCD='AEVENT'):", 
    sum(ades$PARAMCD == "AEVENT"), "\n")
cat("  - Parameter-level (by AEDECOD):", 
    sum(!ades$PARAMCD %in% c("SUBJSUM", "AEVENT")), "\n\n")

# Extract different levels
ades_subject <- ades %>% filter(PARAMCD == "SUBJSUM")
ades_event <- ades %>% filter(PARAMCD == "AEVENT")
ades_param <- ades %>% filter(!PARAMCD %in% c("SUBJSUM", "AEVENT"))

cat("Analysis populations:\n")
cat("  - Subject-level:", nrow(ades_subject), "subjects\n")
cat("  - Event-level:", nrow(ades_event), "AE events\n")
cat("  - Unique AE terms:", length(unique(ades_param$AEDECOD)), "\n\n")

#===============================================================================
# 2. EXPLORATORY ANALYSIS
#===============================================================================

cat("=== EXPLORATORY ANALYSIS ===\n\n")

## 2.1 Overall AE Incidence by Exposure ----

# Summary by exposure tertile
ae_summary <- ades_subject %>%
  group_by(EXPOSURE_TERTILE = factor(EXPOSURE_TERTILE, 
                                      levels = c("Low", "Medium", "High"))) %>%
  summarise(
    N = n(),
    Mean_N_AES = mean(N_AES, na.rm = TRUE),
    SD_N_AES = sd(N_AES, na.rm = TRUE),
    Mean_Rate = mean(RATE_AES, na.rm = TRUE),
    SD_Rate = sd(RATE_AES, na.rm = TRUE),
    Prop_Any_AE = mean(ANY_AE == "Y", na.rm = TRUE),
    Prop_Any_SAE = mean(ANY_SAE == "Y", na.rm = TRUE),
    .groups = "drop"
  )

cat("AE Incidence by Exposure Tertile:\n")
print(ae_summary)
cat("\n")

# Visualization: AE counts by exposure tertile
p1 <- ggplot(ades_subject, 
             aes(x = EXPOSURE_TERTILE, y = N_AES, fill = EXPOSURE_TERTILE)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  scale_fill_manual(values = c("Low" = "#E41A1C", 
                                "Medium" = "#377EB8", 
                                "High" = "#4DAF4A")) +
  labs(title = "Total AE Count by Exposure Tertile",
       x = "Exposure Tertile",
       y = "Number of AEs per Subject") +
  theme_bw() +
  theme(legend.position = "none")

# AE rates by exposure
p2 <- ggplot(ades_subject, 
             aes(x = EXPOSURE_TERTILE, y = RATE_AES, fill = EXPOSURE_TERTILE)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  scale_fill_manual(values = c("Low" = "#E41A1C", 
                                "Medium" = "#377EB8", 
                                "High" = "#4DAF4A")) +
  labs(title = "AE Rate by Exposure Tertile",
       x = "Exposure Tertile",
       y = "AEs per 100 Patient-Days") +
  theme_bw() +
  theme(legend.position = "none")

# Combine plots
p_combined <- p1 | p2
ggsave("output/figures/Figure_S2A_AE_by_exposure_tertile.pdf",
       p_combined, width = 12, height = 5)

cat("Boxplots saved: output/figures/Figure_S2A_AE_by_exposure_tertile.pdf\n")

## 2.2 Continuous Exposure-Response ----

# Scatter plot: exposure vs AE count
p3 <- ggplot(ades_subject, aes(x = EXPOSURE_VAR, y = N_AES)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +
  labs(title = "Exposure-Response Relationship for AE Count",
       x = "Steady-State Exposure (AUC, μg·h/mL)",
       y = "Total Number of AEs") +
  theme_bw()

# Scatter plot: exposure vs AE rate
p4 <- ggplot(ades_subject, aes(x = EXPOSURE_VAR, y = RATE_AES)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "loess", se = TRUE, color = "red") +
  labs(title = "Exposure-Response Relationship for AE Rate",
       x = "Steady-State Exposure (AUC, μg·h/mL)",
       y = "AE Rate (per 100 Patient-Days)") +
  theme_bw()

p_scatter <- p3 | p4
ggsave("output/figures/Figure_S2B_continuous_ER.pdf",
       p_scatter, width = 12, height = 5)

#===============================================================================
# 3. POISSON REGRESSION MODEL
#===============================================================================

cat("\n=== POISSON REGRESSION MODEL ===\n\n")

## 3.1 Simple Poisson Model ----

# Account for varying treatment duration using offset
ades_subject_analysis <- ades_subject %>%
  mutate(
    log_trtdur = log(TRTDURD),
    EXPOSURE_C = EXPOSURE_VAR - mean(EXPOSURE_VAR, na.rm = TRUE),
    EXPOSURE_C10 = EXPOSURE_C / 10  # Scale for interpretability (per 10 units)
  )

# Fit Poisson model
poisson_model <- glm(N_AES ~ EXPOSURE_C10 + offset(log_trtdur),
                     data = ades_subject_analysis,
                     family = poisson(link = "log"))

cat("Poisson Model Summary:\n")
print(summary(poisson_model))
cat("\n")

# Rate ratio per 10-unit increase in exposure
rr_poisson <- exp(coef(poisson_model)["EXPOSURE_C10"])
rr_ci <- exp(confint(poisson_model)["EXPOSURE_C10", ])

cat("Rate Ratio (per 10 μg·h/mL increase in AUC):\n")
cat(sprintf("  RR = %.3f (95%% CI: %.3f - %.3f)\n", 
            rr_poisson, rr_ci[1], rr_ci[2]))

## 3.2 Check for Overdispersion ----

# Dispersion parameter
dispersion <- sum(residuals(poisson_model, type = "pearson")^2) / 
              poisson_model$df.residual

cat("\nDispersion parameter:", round(dispersion, 2), "\n")
if (dispersion > 1.5) {
  cat("  → Overdispersion detected (>1.5). Consider negative binomial model.\n\n")
} else {
  cat("  → No significant overdispersion.\n\n")
}

## 3.3 Negative Binomial Model (if overdispersed) ----

if (dispersion > 1.5) {
  cat("=== NEGATIVE BINOMIAL MODEL ===\n\n")
  
  nb_model <- glm.nb(N_AES ~ EXPOSURE_C10 + offset(log_trtdur),
                     data = ades_subject_analysis)
  
  cat("Negative Binomial Model Summary:\n")
  print(summary(nb_model))
  cat("\n")
  
  # Rate ratio
  rr_nb <- exp(coef(nb_model)["EXPOSURE_C10"])
  rr_nb_ci <- exp(confint(nb_model)["EXPOSURE_C10", ])
  
  cat("Rate Ratio (per 10 μg·h/mL increase):\n")
  cat(sprintf("  RR = %.3f (95%% CI: %.3f - %.3f)\n", 
              rr_nb, rr_nb_ci[1], rr_nb_ci[2]))
  cat("\n")
  
  # Model comparison
  cat("Model Comparison (AIC):\n")
  cat("  Poisson:", round(AIC(poisson_model), 1), "\n")
  cat("  Negative Binomial:", round(AIC(nb_model), 1), "\n")
  cat("  → Lower AIC indicates better fit\n\n")
  
  # Use NB model for predictions
  final_model <- nb_model
  model_type <- "Negative Binomial"
} else {
  final_model <- poisson_model
  model_type <- "Poisson"
}

#===============================================================================
# 4. COVARIATE ANALYSIS
#===============================================================================

cat("=== COVARIATE ANALYSIS ===\n\n")

## 4.1 Multivariable Model ----

if (model_type == "Negative Binomial") {
  mv_model <- glm.nb(N_AES ~ EXPOSURE_C10 + AGE + SEX + WTBL + 
                       offset(log_trtdur),
                     data = ades_subject_analysis)
} else {
  mv_model <- glm(N_AES ~ EXPOSURE_C10 + AGE + SEX + WTBL + 
                    offset(log_trtdur),
                  data = ades_subject_analysis,
                  family = poisson(link = "log"))
}

cat("Multivariable Model:\n")
print(summary(mv_model))
cat("\n")

# Extract coefficients
mv_coefs <- tidy(mv_model, conf.int = TRUE, exponentiate = TRUE)

cat("Rate Ratios from Multivariable Model:\n")
print(mv_coefs %>% dplyr::select(term, estimate, conf.low, conf.high, p.value))
cat("\n")

# Forest plot
p5 <- mv_coefs %>%
  filter(term != "(Intercept)") %>%
  mutate(term = recode(term,
                       "EXPOSURE_C10" = "Exposure (+10 units)",
                       "AGE" = "Age (+1 year)",
                       "SEXM" = "Sex (Male vs Female)",
                       "WTBL" = "Weight (+1 kg)")) %>%
  ggplot(aes(x = estimate, y = fct_reorder(term, estimate))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(size = 3) +
  scale_x_log10() +
  labs(title = "Rate Ratios for AE Count (Multivariable Model)",
       x = "Rate Ratio (95% CI)",
       y = "") +
  theme_bw()

ggsave("output/figures/Figure_S2C_forest_plot.pdf", p5, width = 8, height = 5)

#===============================================================================
# 5. SPECIFIC AE ANALYSIS
#===============================================================================

cat("=== SPECIFIC ADVERSE EVENT ANALYSIS ===\n\n")

## 5.1 Incidence by AE Term ----

ae_incidence <- ades_param %>%
  group_by(AEDECOD) %>%
  summarise(
    N_subjects = n(),
    Total_events = sum(AVAL, na.rm = TRUE),
    Mean_events = mean(AVAL, na.rm = TRUE),
    Prop_SAE = mean(ANY_SAE == "Y", na.rm = TRUE),
    Mean_max_grade = mean(MAX_GRADE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(N_subjects))

cat("AE Incidence by Term:\n")
print(ae_incidence)
cat("\n")

# Visualization
p6 <- ae_incidence %>%
  mutate(AEDECOD = fct_reorder(AEDECOD, N_subjects)) %>%
  ggplot(aes(x = N_subjects, y = AEDECOD)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = N_subjects), hjust = -0.2, size = 3) +
  labs(title = "Adverse Event Incidence",
       subtitle = "Number of subjects experiencing each AE",
       x = "Number of Subjects",
       y = "") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8))

ggsave("output/figures/Figure_S2D_AE_incidence.pdf", p6, width = 8, height = 6)

## 5.2 Exposure-Response for Specific AEs ----

# Analyze top 5 most common AEs
top_aes <- ae_incidence %>% 
  slice_max(N_subjects, n = 5) %>% 
  pull(AEDECOD)

# Logistic regression for each AE
ae_er_results <- list()

for (ae in top_aes) {
  # Create binary outcome (did subject experience this AE?)
  ae_data <- ades_subject_analysis %>%
    left_join(
      ades_param %>% 
        filter(AEDECOD == ae) %>% 
        dplyr::select(USUBJID, HAD_AE = AVAL),
      by = "USUBJID"
    ) %>%
    mutate(HAD_AE = ifelse(is.na(HAD_AE), 0, 1))
  
  # Logistic regression
  logit_model <- glm(HAD_AE ~ EXPOSURE_C10,
                     data = ae_data,
                     family = binomial(link = "logit"))
  
  # Extract OR
  or <- exp(coef(logit_model)["EXPOSURE_C10"])
  or_ci <- exp(confint(logit_model)["EXPOSURE_C10", ])
  pval <- summary(logit_model)$coefficients["EXPOSURE_C10", "Pr(>|z|)"]
  
  ae_er_results[[ae]] <- data.frame(
    AEDECOD = ae,
    OR = or,
    CI_low = or_ci[1],
    CI_high = or_ci[2],
    p_value = pval
  )
}

ae_er_table <- bind_rows(ae_er_results)

cat("Exposure-Response for Specific AEs (Logistic Regression):\n")
cat("Odds Ratio per 10-unit increase in exposure:\n")
print(ae_er_table)
cat("\n")

write.csv(ae_er_table, "output/tables/Table_S2A_specific_AE_ER.csv", 
          row.names = FALSE)

# Forest plot for specific AEs
p7 <- ae_er_table %>%
  mutate(AEDECOD = fct_reorder(AEDECOD, OR)) %>%
  ggplot(aes(x = OR, y = AEDECOD)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.2) +
  geom_point(size = 3, color = "steelblue") +
  scale_x_log10() +
  labs(title = "Exposure-Response for Common AEs",
       subtitle = "Odds ratio per 10-unit increase in exposure",
       x = "Odds Ratio (95% CI)",
       y = "Adverse Event") +
  theme_bw()

ggsave("output/figures/Figure_S2E_specific_AE_ER.pdf", p7, width = 8, height = 5)

#===============================================================================
# 6. GRADE-SPECIFIC ANALYSIS
#===============================================================================

cat("=== SEVERITY (GRADE) ANALYSIS ===\n\n")

## 6.1 Grade Distribution by Exposure ----

grade_summary <- ades_event %>%
  group_by(EXPOSURE_TERTILE, AETOXGR) %>%
  summarise(N = n(), .groups = "drop") %>%
  group_by(EXPOSURE_TERTILE) %>%
  mutate(Prop = N / sum(N))

cat("AE Grade Distribution by Exposure Tertile:\n")
print(grade_summary)
cat("\n")

# Stacked bar chart
p8 <- ggplot(grade_summary, 
             aes(x = EXPOSURE_TERTILE, y = Prop, fill = factor(AETOXGR))) +
  geom_col(position = "stack") +
  scale_fill_brewer(palette = "YlOrRd", name = "Grade") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "AE Grade Distribution by Exposure",
       x = "Exposure Tertile",
       y = "Proportion of AEs") +
  theme_bw()

ggsave("output/figures/Figure_S2F_grade_distribution.pdf", p8, 
       width = 8, height = 5)

## 6.2 Grade 3+ Analysis ----

# Proportion of subjects with Grade 3+ AEs by exposure
grade3_summary <- ades_subject_analysis %>%
  mutate(HAD_GRADE3 = N_GRADE3 > 0) %>%
  group_by(EXPOSURE_TERTILE) %>%
  summarise(
    N = n(),
    N_Grade3 = sum(HAD_GRADE3),
    Prop_Grade3 = mean(HAD_GRADE3),
    .groups = "drop"
  )

cat("Grade 3+ AE Incidence by Exposure:\n")
print(grade3_summary)
cat("\n")

# Logistic regression for Grade 3+ events
logit_grade3 <- glm(HAD_GRADE3 ~ EXPOSURE_C10,
                    data = ades_subject_analysis %>% 
                      mutate(HAD_GRADE3 = N_GRADE3 > 0),
                    family = binomial(link = "logit"))

or_grade3 <- exp(coef(logit_grade3)["EXPOSURE_C10"])
or_grade3_ci <- exp(confint(logit_grade3)["EXPOSURE_C10", ])

cat("Grade 3+ Exposure-Response:\n")
cat(sprintf("  OR per 10-unit increase: %.3f (95%% CI: %.3f - %.3f)\n",
            or_grade3, or_grade3_ci[1], or_grade3_ci[2]))
cat("\n")

#===============================================================================
# 7. TIME-TO-FIRST-AE ANALYSIS
#===============================================================================

cat("=== TIME TO FIRST AE ANALYSIS ===\n\n")

# Calculate time to first AE for each subject
time_to_first <- ades_event %>%
  group_by(USUBJID) %>%
  summarise(
    TIME_FIRST_AE = min(AESTDY, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  right_join(
    ades_subject_analysis %>% dplyr::select(USUBJID, EXPOSURE_VAR, EXPOSURE_TERTILE, 
                                       TRTDURD, ANY_AE),
    by = "USUBJID"
  ) %>%
  mutate(
    # Censoring: if no AE, censor at treatment duration
    TIME = ifelse(ANY_AE == "Y", TIME_FIRST_AE, TRTDURD),
    EVENT = ifelse(ANY_AE == "Y", 1, 0)
  )

cat("Time to First AE Summary:\n")
cat("  Subjects with AE:", sum(time_to_first$EVENT), "\n")
cat("  Median time to first AE:", 
    median(time_to_first$TIME[time_to_first$EVENT == 1]), "days\n\n")

# Kaplan-Meier by exposure
library(survival)
library(survminer)

km_ae <- survfit(Surv(TIME, EVENT) ~ EXPOSURE_TERTILE, data = time_to_first)

p9 <- ggsurvplot(
  km_ae,
  data = time_to_first,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  palette = c("#E41A1C", "#377EB8", "#4DAF4A"),
  title = "Time to First Adverse Event",
  xlab = "Days from Treatment Start",
  ylab = "Probability of Being AE-Free",
  legend.title = "Exposure Tertile",
  legend.labs = c("Low", "Medium", "High")
)

ggsave("output/figures/Figure_S2G_time_to_first_AE.pdf",
       print(p9), width = 10, height = 8)

# Cox model for time to first AE
cox_ae <- coxph(Surv(TIME, EVENT) ~ EXPOSURE_C10, 
                data = time_to_first %>% 
                  mutate(EXPOSURE_C10 = (EXPOSURE_VAR - mean(EXPOSURE_VAR)) / 10))

cat("Cox Model for Time to First AE:\n")
print(summary(cox_ae))
cat("\n")

hr_ae <- exp(coef(cox_ae)["EXPOSURE_C10"])
hr_ae_ci <- exp(confint(cox_ae)["EXPOSURE_C10", ])

cat(sprintf("Hazard Ratio per 10-unit exposure increase: %.3f (95%% CI: %.3f - %.3f)\n",
            hr_ae, hr_ae_ci[1], hr_ae_ci[2]))
cat("  → HR > 1 indicates higher exposure leads to earlier AEs\n\n")

#===============================================================================
# 8. PREDICTIVE MODELING
#===============================================================================

cat("=== PREDICTIVE MODELING ===\n\n")

# Predict AE rates at different exposure levels
exposure_range <- seq(
  min(ades_subject_analysis$EXPOSURE_VAR, na.rm = TRUE),
  max(ades_subject_analysis$EXPOSURE_VAR, na.rm = TRUE),
  length.out = 100
)

pred_data <- data.frame(
  EXPOSURE_VAR = exposure_range,
  EXPOSURE_C10 = (exposure_range - mean(ades_subject_analysis$EXPOSURE_VAR)) / 10,
  log_trtdur = log(median(ades_subject_analysis$TRTDURD)),
  AGE = median(ades_subject_analysis$AGE),
  SEX = "M",
  WTBL = median(ades_subject_analysis$WTBL)
)

# Predictions from model
pred_data$predicted_rate <- predict(final_model, newdata = pred_data, 
                                     type = "response") / 
                            median(ades_subject_analysis$TRTDURD) * 100

# Plot prediction
p10 <- ggplot() +
  geom_line(data = pred_data, 
            aes(x = EXPOSURE_VAR, y = predicted_rate),
            color = "blue", size = 1.2) +
  geom_point(data = ades_subject_analysis,
             aes(x = EXPOSURE_VAR, y = RATE_AES),
             alpha = 0.3) +
  labs(title = paste("Predicted AE Rate vs Exposure (", model_type, "Model)"),
       x = "Exposure (AUC, μg·h/mL)",
       y = "Predicted AE Rate (per 100 patient-days)") +
  theme_bw()

ggsave("output/figures/Figure_S2H_predicted_rate.pdf", p10, width = 8, height = 6)

#===============================================================================
# 9. SUMMARY AND EXPORT
#===============================================================================

cat("=== ANALYSIS COMPLETE ===\n\n")

# Create summary table
summary_results <- data.frame(
  Analysis = c("Overall AE Count", "Grade 3+ Events", "Time to First AE"),
  Effect_Metric = c("Rate Ratio", "Odds Ratio", "Hazard Ratio"),
  Estimate = c(
    ifelse(model_type == "Negative Binomial", rr_nb, rr_poisson),
    or_grade3,
    hr_ae
  ),
  CI_Lower = c(
    ifelse(model_type == "Negative Binomial", rr_nb_ci[1], rr_ci[1]),
    or_grade3_ci[1],
    hr_ae_ci[1]
  ),
  CI_Upper = c(
    ifelse(model_type == "Negative Binomial", rr_nb_ci[2], rr_ci[2]),
    or_grade3_ci[2],
    hr_ae_ci[2]
  ),
  Interpretation = c(
    "Per 10-unit AUC increase",
    "Per 10-unit AUC increase",
    "Per 10-unit AUC increase"
  )
)

cat("Summary of Exposure-Safety Relationships:\n")
print(summary_results)
cat("\n")

write.csv(summary_results, "output/tables/Table_S2B_summary_results.csv",
          row.names = FALSE)

# Session info
sink("output/session_info_ades.txt")
cat("=== R Session Information ===\n\n")
print(sessionInfo())
sink()

cat("All outputs saved in output/ directory\n\n")

cat("Figures:\n")
cat("  - Figure_S2A_AE_by_exposure_tertile.pdf\n")
cat("  - Figure_S2B_continuous_ER.pdf\n")
cat("  - Figure_S2C_forest_plot.pdf\n")
cat("  - Figure_S2D_AE_incidence.pdf\n")
cat("  - Figure_S2E_specific_AE_ER.pdf\n")
cat("  - Figure_S2F_grade_distribution.pdf\n")
cat("  - Figure_S2G_time_to_first_AE.pdf\n")
cat("  - Figure_S2H_predicted_rate.pdf\n\n")

cat("Tables:\n")
cat("  - Table_S2A_specific_AE_ER.csv\n")
cat("  - Table_S2B_summary_results.csv\n\n")

cat("=== END OF ADES ANALYSIS ===\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================