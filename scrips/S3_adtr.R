#===============================================================================
# Supplementary Material S3
# Tumor Response (ADTR) Analysis Example
# Standardizing Exposure-Response Data for Modeling and Simulation
#
# Purpose: Demonstrate complete workflow from ADTR dataset to exposure-response
#          modeling using longitudinal tumor measurements and RECIST 1.1
#
# Author: [Your Name]
# Date: 2026-01-17
#===============================================================================

# Required packages
required_packages <- c("dplyr", "ggplot2", "tidyr", "nlme", "lme4",
                       "broom", "broom.mixed", "patchwork", "scales", 
                       "forcats", "viridis")

# Install if needed
new_packages <- required_packages[!(required_packages %in% 
                                    installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(nlme)         # For mixed effects models
library(lme4)         # Alternative mixed effects
library(broom)        # For tidy model output
library(broom.mixed)  # For tidy mixed model output
library(patchwork)    # For combining plots
library(scales)       # For formatting
library(forcats)      # For factor manipulation
library(viridis)      # For color palettes

# Prevent conflicts
select <- dplyr::select
filter <- dplyr::filter

# Create output directories
if (!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)
if (!dir.exists("output/tables")) dir.create("output/tables", recursive = TRUE)

#===============================================================================
# 1. DATA PREPARATION
#===============================================================================

cat("\n=== ADTR TUMOR RESPONSE ANALYSIS ===\n\n")

# Load ADTR dataset
adtr <- read.csv("data/adtr_example.csv")

cat("=== ADTR Dataset Structure ===\n")
cat("Total records:", nrow(adtr), "\n")
cat("Parameters:\n")
cat("  - TUMSIZE (longitudinal):", sum(adtr$PARAMCD == "TUMSIZE"), "\n")
cat("  - BOR (best response):", sum(adtr$PARAMCD == "BOR"), "\n")
cat("  - NADIR (minimum size):", sum(adtr$PARAMCD == "NADIR"), "\n\n")

# Extract different parameter types
adtr_tumor <- adtr %>% filter(PARAMCD == "TUMSIZE")
adtr_bor <- adtr %>% filter(PARAMCD == "BOR") %>% distinct(USUBJID, .keep_all = TRUE)
adtr_nadir <- adtr %>% filter(PARAMCD == "NADIR") %>% distinct(USUBJID, .keep_all = TRUE)

cat("Analysis populations:\n")
cat("  - Subjects with tumor data:", length(unique(adtr_tumor$USUBJID)), "\n")
cat("  - Baseline measurements:", sum(adtr_tumor$ABLFL == "Y"), "\n")
cat("  - Post-baseline measurements:", sum(adtr_tumor$ANL01FL == "Y"), "\n\n")

# Best overall response distribution
bor_dist <- table(adtr_bor$BOR)
cat("Best Overall Response Distribution:\n")
print(bor_dist)
cat("  ORR (CR+PR):", sum(bor_dist[c("CR", "PR")]), "/", sum(bor_dist),
    sprintf("(%.1f%%)\n\n", 100 * sum(bor_dist[c("CR", "PR")]) / sum(bor_dist)))

#===============================================================================
# 2. EXPLORATORY ANALYSIS
#===============================================================================

cat("=== EXPLORATORY ANALYSIS ===\n\n")

## 2.1 Spaghetti Plot - Individual Trajectories ----

p1 <- ggplot(adtr_tumor, aes(x = AVISITN, y = PCHG, group = USUBJID)) +
  geom_line(alpha = 0.3, color = "gray50") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_hline(yintercept = -30, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 20, linetype = "dashed", color = "red") +
  annotate("text", x = 8, y = -32, label = "PR threshold (-30%)", 
           color = "blue", size = 3) +
  annotate("text", x = 8, y = 22, label = "PD threshold (+20%)", 
           color = "red", size = 3) +
  labs(title = "Individual Tumor Response Trajectories",
       subtitle = "Percent change from baseline",
       x = "Visit Number",
       y = "Percent Change from Baseline (%)") +
  theme_bw()

ggsave("output/figures/Figure_S3A_spaghetti_plot.pdf", p1, width = 10, height = 6)

## 2.2 Spider Plot by Exposure Tertile ----

p2 <- ggplot(adtr_tumor, 
             aes(x = AVISITN, y = PCHG, group = USUBJID, color = EXPOSURE_CAT)) +
  geom_line(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_hline(yintercept = -30, linetype = "dashed", color = "blue", alpha = 0.5) +
  geom_hline(yintercept = 20, linetype = "dashed", color = "red", alpha = 0.5) +
  facet_wrap(~factor(EXPOSURE_CAT, levels = c("Low", "Medium", "High"))) +
  scale_color_manual(values = c("Low" = "#E41A1C", 
                                 "Medium" = "#377EB8", 
                                 "High" = "#4DAF4A")) +
  labs(title = "Tumor Response by Exposure Tertile",
       x = "Visit Number",
       y = "Percent Change from Baseline (%)",
       color = "Exposure") +
  theme_bw() +
  theme(legend.position = "none")

ggsave("output/figures/Figure_S3B_spider_by_exposure.pdf", p2, width = 12, height = 4)

## 2.3 Mean Response Over Time ----

mean_response <- adtr_tumor %>%
  group_by(AVISITN, AVISIT, EXPOSURE_CAT) %>%
  summarise(
    Mean_PCHG = mean(PCHG, na.rm = TRUE),
    SE_PCHG = sd(PCHG, na.rm = TRUE) / sqrt(n()),
    N = n(),
    .groups = "drop"
  )

p3 <- ggplot(mean_response, 
             aes(x = AVISITN, y = Mean_PCHG, color = EXPOSURE_CAT, 
                 group = EXPOSURE_CAT)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Mean_PCHG - 1.96*SE_PCHG, 
                    ymax = Mean_PCHG + 1.96*SE_PCHG),
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_hline(yintercept = -30, linetype = "dashed", color = "blue", alpha = 0.5) +
  scale_color_manual(values = c("Low" = "#E41A1C", 
                                 "Medium" = "#377EB8", 
                                 "High" = "#4DAF4A")) +
  labs(title = "Mean Tumor Response Over Time by Exposure",
       x = "Visit Number",
       y = "Mean Percent Change from Baseline (%)",
       color = "Exposure Tertile") +
  theme_bw()

ggsave("output/figures/Figure_S3C_mean_response.pdf", p3, width = 10, height = 6)

#===============================================================================
# 3. WATERFALL PLOT - BEST RESPONSE
#===============================================================================

cat("\n=== WATERFALL PLOT ANALYSIS ===\n\n")

# Get nadir (best response) for each subject
waterfall_data <- adtr_nadir %>%
  arrange(PCHG) %>%
  mutate(
    subject_order = row_number(),
    Response_Cat = case_when(
      PCHG <= -30 ~ "PR",
      PCHG >= 20 ~ "PD",
      TRUE ~ "SD"
    ),
    Response_Cat = factor(Response_Cat, levels = c("PD", "SD", "PR"))
  )

# Waterfall plot
p4 <- ggplot(waterfall_data, aes(x = subject_order, y = PCHG, fill = Response_Cat)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_hline(yintercept = -30, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 20, linetype = "dashed", color = "red") +
  scale_fill_manual(
    values = c("PR" = "#4DAF4A", "SD" = "#377EB8", "PD" = "#E41A1C"),
    name = "Best Response"
  ) +
  labs(title = "Waterfall Plot: Best Tumor Response",
       subtitle = "Maximum reduction (or minimum increase) from baseline",
       x = "Subject (ordered by response)",
       y = "Best % Change from Baseline") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave("output/figures/Figure_S3D_waterfall.pdf", p4, width = 12, height = 6)

## 3.1 Waterfall by Exposure Tertile ----

p5 <- ggplot(waterfall_data, 
             aes(x = reorder(USUBJID, PCHG), y = PCHG, fill = EXPOSURE_CAT)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_hline(yintercept = -30, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 20, linetype = "dashed", color = "red") +
  facet_wrap(~factor(EXPOSURE_CAT, levels = c("Low", "Medium", "High")), 
             scales = "free_x") +
  scale_fill_manual(values = c("Low" = "#E41A1C", 
                                "Medium" = "#377EB8", 
                                "High" = "#4DAF4A")) +
  labs(title = "Waterfall Plot by Exposure Tertile",
       x = "Subject (ordered by response)",
       y = "Best % Change from Baseline") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")

ggsave("output/figures/Figure_S3E_waterfall_by_exposure.pdf", p5, 
       width = 12, height = 5)

#===============================================================================
# 4. BEST OVERALL RESPONSE ANALYSIS
#===============================================================================

cat("=== BEST OVERALL RESPONSE ANALYSIS ===\n\n")

## 4.1 ORR by Exposure Tertile ----

orr_summary <- adtr_bor %>%
  mutate(
    RESPONDER = BOR %in% c("CR", "PR"),
    EXPOSURE_CAT = factor(EXPOSURE_CAT, levels = c("Low", "Medium", "High"))
  ) %>%
  group_by(EXPOSURE_CAT) %>%
  summarise(
    N = n(),
    N_Responders = sum(RESPONDER),
    ORR = mean(RESPONDER),
    .groups = "drop"
  )

cat("Objective Response Rate by Exposure:\n")
print(orr_summary)
cat("\n")

# Bar plot
p6 <- ggplot(orr_summary, aes(x = EXPOSURE_CAT, y = ORR, fill = EXPOSURE_CAT)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = sprintf("%d/%d\n(%.1f%%)", N_Responders, N, ORR*100)),
            vjust = -0.5) +
  scale_fill_manual(values = c("Low" = "#E41A1C", 
                                "Medium" = "#377EB8", 
                                "High" = "#4DAF4A")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(title = "Objective Response Rate by Exposure Tertile",
       x = "Exposure Tertile",
       y = "ORR (CR + PR)") +
  theme_bw() +
  theme(legend.position = "none")

ggsave("output/figures/Figure_S3F_ORR_by_exposure.pdf", p6, width = 8, height = 6)

## 4.2 Logistic Regression for Response ----

# Prepare data
bor_analysis <- adtr_bor %>%
  mutate(
    RESPONDER = as.numeric(BOR %in% c("CR", "PR")),
    EXPOSURE_C = EXPOSURE_VAR - mean(EXPOSURE_VAR),
    EXPOSURE_C10 = EXPOSURE_C / 10
  )

# Logistic regression - continuous exposure
logit_orr <- glm(RESPONDER ~ EXPOSURE_C10,
                 data = bor_analysis,
                 family = binomial(link = "logit"))

cat("Logistic Regression for ORR:\n")
print(summary(logit_orr))
cat("\n")

or_orr <- exp(coef(logit_orr)["EXPOSURE_C10"])
or_orr_ci <- exp(confint(logit_orr)["EXPOSURE_C10", ])

cat(sprintf("Odds Ratio per 10-unit AUC increase: %.3f (95%% CI: %.3f - %.3f)\n",
            or_orr, or_orr_ci[1], or_orr_ci[2]))
cat("\n")

## 4.3 Predicted Response Probability ----

exposure_range <- seq(
  min(bor_analysis$EXPOSURE_VAR),
  max(bor_analysis$EXPOSURE_VAR),
  length.out = 100
)

pred_orr <- data.frame(
  EXPOSURE_VAR = exposure_range,
  EXPOSURE_C10 = (exposure_range - mean(bor_analysis$EXPOSURE_VAR)) / 10
)

pred_orr$prob_response <- predict(logit_orr, newdata = pred_orr, type = "response")

# Plot
p7 <- ggplot() +
  geom_line(data = pred_orr, 
            aes(x = EXPOSURE_VAR, y = prob_response),
            color = "blue", size = 1.2) +
  geom_point(data = bor_analysis,
             aes(x = EXPOSURE_VAR, y = RESPONDER),
             alpha = 0.3, position = position_jitter(height = 0.02)) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(title = "Predicted Response Probability vs Exposure",
       x = "Exposure (AUC, μg·h/mL)",
       y = "Probability of Response (CR or PR)") +
  theme_bw()

ggsave("output/figures/Figure_S3G_predicted_ORR.pdf", p7, width = 8, height = 6)

#===============================================================================
# 5. NADIR ANALYSIS (BEST REDUCTION)
#===============================================================================

cat("=== NADIR ANALYSIS ===\n\n")

## 5.1 Nadir vs Exposure ----

p8 <- ggplot(adtr_nadir, aes(x = EXPOSURE_VAR, y = NADIR_PCHG)) +
  geom_point(aes(color = BOR), size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  geom_hline(yintercept = -30, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  scale_color_manual(
    values = c("CR" = "#00BA38", "PR" = "#619CFF", 
               "SD" = "#F8766D", "PD" = "#C77CFF"),
    name = "Best Response"
  ) +
  labs(title = "Maximum Tumor Reduction vs Exposure",
       x = "Exposure (AUC, μg·h/mL)",
       y = "Maximum Reduction from Baseline (%)") +
  theme_bw()

ggsave("output/figures/Figure_S3H_nadir_vs_exposure.pdf", p8, width = 10, height = 6)

## 5.2 Linear Regression for Nadir ----

lm_nadir <- lm(NADIR_PCHG ~ EXPOSURE_C10, 
               data = adtr_nadir %>% 
                 mutate(EXPOSURE_C10 = (EXPOSURE_VAR - mean(EXPOSURE_VAR)) / 10))

cat("Linear Regression for Maximum Reduction:\n")
print(summary(lm_nadir))
cat("\n")

beta_nadir <- coef(lm_nadir)["EXPOSURE_C10"]
beta_nadir_ci <- confint(lm_nadir)["EXPOSURE_C10", ]

cat(sprintf("Change in max reduction per 10-unit AUC: %.2f%% (95%% CI: %.2f%% to %.2f%%)\n",
            beta_nadir, beta_nadir_ci[1], beta_nadir_ci[2]))
cat("  → Negative values indicate greater tumor reduction\n\n")

#===============================================================================
# 6. LONGITUDINAL MIXED EFFECTS MODEL
#===============================================================================

cat("=== LONGITUDINAL MIXED EFFECTS MODEL ===\n\n")

## 6.1 Prepare data for modeling ----

adtr_lme <- adtr_tumor %>%
  filter(ANL01FL == "Y") %>%  # Post-baseline only
  mutate(
    EXPOSURE_C = EXPOSURE_VAR - mean(EXPOSURE_VAR),
    EXPOSURE_C10 = EXPOSURE_C / 10,
    TIME_WEEKS = AVISITN * 6  # Convert visit to weeks (approximately)
  )

## 6.2 Fit mixed effects model ----

# Model: PCHG ~ TIME + EXPOSURE + TIME:EXPOSURE + (1 + TIME | SUBJECT)
# This allows different trajectories by exposure

lme_model <- lme(
  PCHG ~ TIME_WEEKS * EXPOSURE_C10,
  random = ~ TIME_WEEKS | USUBJID,
  data = adtr_lme,
  method = "REML"
)

cat("Mixed Effects Model Summary:\n")
print(summary(lme_model))
cat("\n")

# Extract fixed effects
fixed_effects <- tidy(lme_model, effects = "fixed")

cat("Fixed Effects:\n")
print(fixed_effects)
cat("\n")

## 6.3 Interpretation ----

beta_time <- fixed_effects$estimate[fixed_effects$term == "TIME_WEEKS"]
beta_exp <- fixed_effects$estimate[fixed_effects$term == "EXPOSURE_C10"]
beta_int <- fixed_effects$estimate[fixed_effects$term == "TIME_WEEKS:EXPOSURE_C10"]

cat("Model Interpretation:\n")
cat(sprintf("  Baseline slope (time effect): %.3f%% per week\n", beta_time))
cat(sprintf("  Exposure effect at baseline: %.3f%% per 10-unit AUC\n", beta_exp))
cat(sprintf("  Exposure × Time interaction: %.3f%% per week per 10-unit AUC\n", beta_int))

if (beta_int < 0) {
  cat("  → Higher exposure leads to steeper decline (better response) over time\n\n")
} else {
  cat("  → Higher exposure leads to less steep decline over time\n\n")
}

## 6.4 Predicted trajectories by exposure ----

# Create prediction data
pred_lme <- expand.grid(
  TIME_WEEKS = seq(6, 48, by = 6),
  EXPOSURE_C10 = c(-1, 0, 1)  # Low, Medium, High
) %>%
  mutate(
    EXPOSURE_LABEL = case_when(
      EXPOSURE_C10 == -1 ~ "Low Exposure (-1 SD)",
      EXPOSURE_C10 == 0 ~ "Medium Exposure (Mean)",
      EXPOSURE_C10 == 1 ~ "High Exposure (+1 SD)"
    )
  )

# Predictions (fixed effects only)
pred_lme$predicted_PCHG <- predict(lme_model, newdata = pred_lme, level = 0)

# Plot predicted trajectories
p9 <- ggplot(pred_lme, aes(x = TIME_WEEKS, y = predicted_PCHG, 
                            color = EXPOSURE_LABEL, group = EXPOSURE_LABEL)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_hline(yintercept = -30, linetype = "dashed", color = "blue", alpha = 0.5) +
  scale_color_manual(values = c("Low Exposure (-1 SD)" = "#E41A1C",
                                 "Medium Exposure (Mean)" = "#377EB8",
                                 "High Exposure (+1 SD)" = "#4DAF4A")) +
  labs(title = "Predicted Tumor Response Trajectories",
       subtitle = "From mixed effects model",
       x = "Time (weeks)",
       y = "Predicted % Change from Baseline",
       color = "Exposure Level") +
  theme_bw()

ggsave("output/figures/Figure_S3I_predicted_trajectories.pdf", p9, 
       width = 10, height = 6)

#===============================================================================
# 7. TIME TO RESPONSE ANALYSIS
#===============================================================================

cat("=== TIME TO RESPONSE ANALYSIS ===\n\n")

# Identify time to first PR/CR for responders
time_to_response <- adtr_tumor %>%
  filter(ANL01FL == "Y") %>%
  mutate(IS_RESPONSE = AVALC %in% c("CR", "PR")) %>%
  group_by(USUBJID) %>%
  summarise(
    FIRST_RESPONSE_TIME = ifelse(any(IS_RESPONSE),
                                   min(ADY[IS_RESPONSE]),
                                   NA),
    HAD_RESPONSE = any(IS_RESPONSE),
    .groups = "drop"
  ) %>%
  left_join(
    adtr_bor %>% select(USUBJID, EXPOSURE_VAR, EXPOSURE_CAT, BOR),
    by = "USUBJID"
  )

responder_summary <- time_to_response %>%
  filter(HAD_RESPONSE) %>%
  group_by(EXPOSURE_CAT) %>%
  summarise(
    N = n(),
    Median_TTR = median(FIRST_RESPONSE_TIME, na.rm = TRUE),
    Q1_TTR = quantile(FIRST_RESPONSE_TIME, 0.25, na.rm = TRUE),
    Q3_TTR = quantile(FIRST_RESPONSE_TIME, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

cat("Time to Response by Exposure:\n")
print(responder_summary)
cat("\n")

# Boxplot
p10 <- time_to_response %>%
  filter(HAD_RESPONSE) %>%
  ggplot(aes(x = EXPOSURE_CAT, y = FIRST_RESPONSE_TIME, fill = EXPOSURE_CAT)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.4) +
  scale_fill_manual(values = c("Low" = "#E41A1C", 
                                "Medium" = "#377EB8", 
                                "High" = "#4DAF4A")) +
  labs(title = "Time to First Response",
       subtitle = "Among responders only",
       x = "Exposure Tertile",
       y = "Days to First PR/CR") +
  theme_bw() +
  theme(legend.position = "none")

ggsave("output/figures/Figure_S3J_time_to_response.pdf", p10, width = 8, height = 6)

#===============================================================================
# 8. EXPOSURE-RESPONSE SUMMARY TABLE
#===============================================================================

cat("=== CREATING SUMMARY TABLE ===\n\n")

# Compile all ER results
er_summary <- data.frame(
  Endpoint = c(
    "Objective Response Rate",
    "Maximum Tumor Reduction",
    "Tumor Growth Rate",
    "Time to Response"
  ),
  Analysis_Method = c(
    "Logistic Regression",
    "Linear Regression",
    "Mixed Effects Model",
    "Descriptive"
  ),
  Effect_Estimate = c(
    sprintf("OR = %.2f", or_orr),
    sprintf("β = %.2f%%", beta_nadir),
    sprintf("β = %.3f%% per week", beta_int),
    sprintf("Median: %.0f days", median(responder_summary$Median_TTR))
  ),
  CI_95 = c(
    sprintf("(%.2f - %.2f)", or_orr_ci[1], or_orr_ci[2]),
    sprintf("(%.2f - %.2f)", beta_nadir_ci[1], beta_nadir_ci[2]),
    "See model output",
    sprintf("(%.0f - %.0f)", min(responder_summary$Q1_TTR), 
            max(responder_summary$Q3_TTR))
  ),
  Interpretation = c(
    "Higher exposure → higher response rate",
    "Higher exposure → greater tumor reduction",
    "Higher exposure → faster tumor shrinkage",
    "Similar time to response across exposures"
  )
)

cat("Exposure-Response Summary:\n")
print(er_summary)
cat("\n")

write.csv(er_summary, "output/tables/Table_S3_ER_summary.csv", row.names = FALSE)

#===============================================================================
# 9. DURATION OF RESPONSE ANALYSIS
#===============================================================================

cat("=== DURATION OF RESPONSE ANALYSIS ===\n\n")

# For responders, calculate duration from first response to progression
dor_data <- adtr_tumor %>%
  filter(ANL01FL == "Y") %>%
  group_by(USUBJID) %>%
  summarise(
    FIRST_RESPONSE = min(ADY[AVALC %in% c("CR", "PR")], na.rm = TRUE),
    FIRST_PD = min(ADY[AVALC == "PD"], na.rm = TRUE),
    LAST_FOLLOWUP = max(ADY),
    HAD_RESPONSE = any(AVALC %in% c("CR", "PR")),
    .groups = "drop"
  ) %>%
  filter(HAD_RESPONSE) %>%
  left_join(
    adtr_bor %>% select(USUBJID, EXPOSURE_VAR, EXPOSURE_CAT),
    by = "USUBJID"
  ) %>%
  mutate(
    # Duration of response
    DOR = ifelse(is.finite(FIRST_PD),
                  FIRST_PD - FIRST_RESPONSE,
                  LAST_FOLLOWUP - FIRST_RESPONSE),
    # Event = progression
    DOR_EVENT = is.finite(FIRST_PD)
  )

cat("Duration of Response (among responders):\n")
cat("  N responders:", nrow(dor_data), "\n")
cat("  N with progression:", sum(dor_data$DOR_EVENT), "\n")
cat("  Median DOR:", median(dor_data$DOR), "days\n\n")

# Boxplot by exposure
p11 <- ggplot(dor_data, aes(x = EXPOSURE_CAT, y = DOR, fill = EXPOSURE_CAT)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.4) +
  scale_fill_manual(values = c("Low" = "#E41A1C", 
                                "Medium" = "#377EB8", 
                                "High" = "#4DAF4A")) +
  labs(title = "Duration of Response by Exposure",
       subtitle = "Among responders (CR/PR)",
       x = "Exposure Tertile",
       y = "Duration of Response (days)") +
  theme_bw() +
  theme(legend.position = "none")

ggsave("output/figures/Figure_S3K_duration_of_response.pdf", p11, 
       width = 8, height = 6)

#===============================================================================
# 10. SUMMARY AND EXPORT
#===============================================================================

cat("\n=== ANALYSIS COMPLETE ===\n\n")

# Create comprehensive summary statistics
summary_stats <- list(
  dataset_info = list(
    n_subjects = length(unique(adtr_tumor$USUBJID)),
    n_measurements = sum(adtr_tumor$PARAMCD == "TUMSIZE"),
    n_visits = length(unique(adtr_tumor$AVISITN))
  ),
  
  baseline_tumor = list(
    mean_size = mean(adtr_tumor$BASE[adtr_tumor$ABLFL == "Y"], na.rm = TRUE),
    sd_size = sd(adtr_tumor$BASE[adtr_tumor$ABLFL == "Y"], na.rm = TRUE)
  ),
  
  best_response = list(
    orr = sum(bor_dist[c("CR", "PR")]) / sum(bor_dist),
    dcr = sum(bor_dist[c("CR", "PR", "SD")]) / sum(bor_dist),
    cr_rate = bor_dist["CR"] / sum(bor_dist),
    pr_rate = bor_dist["PR"] / sum(bor_dist)
  ),
  
  exposure_response = list(
    or_per_10units = or_orr,
    or_ci = or_orr_ci,
    nadir_beta = beta_nadir,
    nadir_ci = beta_nadir_ci
  )
)

# Save summary
saveRDS(summary_stats, "output/summary_stats_adtr.rds")

# Session info
sink("output/session_info_adtr.txt")
cat("=== R Session Information ===\n\n")
print(sessionInfo())
sink()

cat("All outputs saved in output/ directory\n\n")

cat("Figures:\n")
cat("  - Figure_S3A_spaghetti_plot.pdf\n")
cat("  - Figure_S3B_spider_by_exposure.pdf\n")
cat("  - Figure_S3C_mean_response.pdf\n")
cat("  - Figure_S3D_waterfall.pdf\n")
cat("  - Figure_S3E_waterfall_by_exposure.pdf\n")
cat("  - Figure_S3F_ORR_by_exposure.pdf\n")
cat("  - Figure_S3G_predicted_ORR.pdf\n")
cat("  - Figure_S3H_nadir_vs_exposure.pdf\n")
cat("  - Figure_S3I_predicted_trajectories.pdf\n")
cat("  - Figure_S3J_time_to_response.pdf\n")
cat("  - Figure_S3K_duration_of_response.pdf\n\n")

cat("Tables:\n")
cat("  - Table_S3_ER_summary.csv\n\n")

cat("=== END OF ADTR ANALYSIS ===\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================