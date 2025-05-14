# Load libraries
library(tidyverse)
library(zoo)
library(pheatmap)  # for heatmap


# Step 1: Load the data
delinquency_raw <- read_csv("Page 27 Data-Table 1.csv", skip = 2)

# Step 2: Clean the data
delinquency_data <- delinquency_raw %>%
  select(
    quarter = `...1`,
    age_18_29 = `18-29`,
    age_30_39 = `30-39`,
    age_40_49 = `40-49`,
    age_50_59 = `50-59`,
    age_60_69 = `60-69`,
    age_70_plus = `70+`,
    all_ages = all
  ) %>%
  filter(!is.na(quarter)) %>%
  mutate(
    quarter = str_replace(quarter, ":", " "),
    quarter = as.yearqtr(quarter, format = "%y Q%q")
  )

# Step 3: Reshape the data for easier plotting
delinquency_long <- delinquency_data %>%
  pivot_longer(
    cols = starts_with("age_"),
    names_to = "age_group",
    values_to = "transition_rate"
  )

# Step 4: Visualize the trends by age group
ggplot(delinquency_long, aes(x = quarter, y = transition_rate, color = age_group)) +
  geom_line(size = 1.2) +
  theme_minimal() +
  labs(
    title = "Transition into Serious Credit Card Delinquency (90+ Days) by Age Group",
    subtitle = "Source: NY Fed Household Credit Report (2024 Q4)",
    x = "Quarter",
    y = "Transition Rate (%)",
    color = "Age Group"
  ) +
  scale_x_yearqtr(format = "%Y Q%q", n = 8) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Step 5: Focus on post-pandemic data (2021 onward)
post_pandemic_data <- delinquency_long %>%
  filter(quarter >= as.yearqtr("2021 Q1"))

# Step 6: Summary Statistics for post-2021
summary_stats <- post_pandemic_data %>%
  group_by(age_group) %>%
  summarise(
    avg_transition_rate = mean(transition_rate, na.rm = TRUE),
    latest_transition_rate = last(transition_rate)
  )

# Print summary table
print(summary_stats) 






# Additional analysis


# 1. Q/Q Percent Change Post-2021
delinquency_qoq <- delinquency_data %>%
  arrange(quarter) %>%
  mutate(across(starts_with("age_"), ~ (.-lag(.))/lag(.) * 100, .names = "pct_change_{col}"))

qoq_long <- delinquency_qoq %>%
  select(quarter, starts_with("pct_change")) %>%
  pivot_longer(
    cols = -quarter,
    names_to = "age_group",
    values_to = "qoq_pct_change"
  ) %>%
  mutate(age_group = str_remove(age_group, "pct_change_"))

ggplot(qoq_long %>% filter(quarter >= as.yearqtr("2021 Q1")),
       aes(x = quarter, y = qoq_pct_change, color = age_group)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Quarter-over-Quarter Change in Delinquency Rates by Age",
    subtitle = "Post-Pandemic Volatility (2021–2024)",
    x = "Quarter", y = "Percent Change (%)"
  )

# 2. Heatmap of Delinquency Rates
library(pheatmap)

heatmap_df <- delinquency_data %>%
  filter(quarter >= as.yearqtr("2020 Q1")) %>%
  select(quarter, starts_with("age_")) %>%
  mutate(quarter = as.character(quarter)) %>%
  column_to_rownames("quarter")

heatmap_matrix <- as.matrix(t(heatmap_df))

pheatmap(heatmap_matrix,
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         color = colorRampPalette(c("white", "orange", "red"))(50),
         main = "Delinquency Rates by Age Group (2020–2024)",
         angle_col = 45)

# 3. Volatility (Standard Deviation)
volatility <- delinquency_data %>%
  filter(quarter >= as.yearqtr("2021 Q1")) %>%
  summarise(across(starts_with("age_"), sd, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "age_group", values_to = "std_dev") %>%
  mutate(age_group = str_remove(age_group, "age_"))

ggplot(volatility, aes(x = reorder(age_group, -std_dev), y = std_dev)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Volatility in Delinquency Rates (2021–2024)",
    x = "Age Group", y = "Standard Deviation"
  ) +
  theme_minimal()

# 4. Total Increase Since 2021
increase_summary <- delinquency_data %>%
  filter(quarter %in% c(as.yearqtr("2021 Q1"), as.yearqtr("2024 Q4"))) %>%
  pivot_longer(cols = starts_with("age_"), names_to = "age_group", values_to = "rate") %>%
  group_by(age_group) %>%
  arrange(quarter) %>%
  summarise(change = last(rate) - first(rate)) %>%
  mutate(age_group = str_remove(age_group, "age_"))

ggplot(increase_summary, aes(x = reorder(age_group, -change), y = change)) +
  geom_col(fill = "tomato") +
  labs(
    title = "Total Increase in Delinquency Rates (2021 Q1 to 2024 Q4)",
    x = "Age Group", y = "Increase in % Points"
  ) +
  theme_minimal()


