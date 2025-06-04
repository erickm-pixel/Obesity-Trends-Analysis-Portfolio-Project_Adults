# OBESITY ANALYSIS 
# 1. SETUP -------------------------------------------------------------------
# Install missing packages only if needed
required_packages <- c("tidyverse", "viridis", "ggthemes")
new_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(tidyverse)
library(viridis)
library(ggthemes)

# 2. DATA IMPORT -------------------------------------------------------------
# Set correct working directory (replace with your actual path)
setwd("C:/Users/weree/Downloads/Adult_Obesity/")

# Check if file exists before loading
data_file <- "Adult_obesity_Data.csv"
if (!file.exists(data_file)) {
  stop("Data file not found. Please ensure:\n",
       "1. The file exists in: ", getwd(), "\n",
       "2. The filename is exactly: '", data_file, "'")
} else {
  message("Loading data from: ", file.path(getwd(), data_file))
  obesity <- read_csv(data_file)
}

# 3. DATA CLEANING -----------------------------------------------------------
obesity_clean <- obesity %>%
  mutate(
    YEAR = as.numeric(DIM_TIME),
    SEX = factor(DIM_SEX, 
                 levels = c("TOTAL", "MALE", "FEMALE"),
                 labels = c("All", "Male", "Female")),
    OBESITY_RATE = RATE_PER_100_N,
    LOWER_CI = RATE_PER_100_NL,
    UPPER_CI = RATE_PER_100_NU,
    CI_RANGE = UPPER_CI - LOWER_CI
  ) %>%
  select(YEAR, SEX, OBESITY_RATE, LOWER_CI, UPPER_CI, CI_RANGE)

# 4. VISUALIZATIONS ----------------------------------------------------------
# Create outputs directory if needed
if (!dir.exists("outputs")) dir.create("outputs")

# Plot 1: Time Trends
time_plot <- ggplot(obesity_clean, aes(x = YEAR, y = OBESITY_RATE, color = SEX)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = LOWER_CI, ymax = UPPER_CI, fill = SEX), alpha = 0.2) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  labs(title = "Adult Obesity Trends in Kenya (1990-2022)",
       x = "Year", y = "Obesity Rate (%)") +
  theme_minimal()

ggsave("outputs/obesity_trends.png", time_plot, width = 10, height = 6)

# Plot 2: Intervention Impact
intervention_results <- obesity_clean %>%
  filter(YEAR >= 2010) %>%  # Focus on recent years
  mutate(projected_rate = case_when(
    SEX == "Female" ~ OBESITY_RATE * 0.95,
    SEX == "Male" ~ OBESITY_RATE * 0.93,
    TRUE ~ OBESITY_RATE
  ))

intervention_plot <- ggplot(intervention_results, aes(x = YEAR)) +
  geom_line(aes(y = OBESITY_RATE, color = "Current")) +
  geom_line(aes(y = projected_rate, color = "Projected")) +
  scale_color_manual(values = c("Current" = "red", "Projected" = "blue")) +
  facet_wrap(~SEX) +
  labs(title = "Potential Impact of Exercise Interventions",
       y = "Obesity Rate (%)")

ggsave("outputs/intervention_impact.png", intervention_plot, width = 10, height = 6)

# 5. EXERCISE PRESCRIPTIONS --------------------------------------------------
calculate_metmins <- function(weight_kg, activity = "low") {
  kcal_needed <- (weight_kg * 0.05) * 7700  # 5% weight loss
  metmins <- (kcal_needed / (3.5 * weight_kg/200 * 26)) * 60  # 26 weeks
  return(round(metmins / ifelse(activity == "low", 1.2, 1)))
}

prescription_examples <- tibble(
  Weight = c(60, 75, 90),
  Activity = c("low", "medium", "high"),
  METmins = map2_dbl(Weight, Activity, calculate_metmins)
)

# 6. SAVE RESULTS ------------------------------------------------------------
write_csv(prescription_examples, "outputs/exercise_prescriptions.csv")

# Print session info
sink("outputs/session_info.txt")
print(sessionInfo())
sink()

# Display results
print(time_plot)
print(intervention_plot)
print(prescription_examples)

