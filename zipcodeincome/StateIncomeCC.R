# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(fredr)
library(lubridate)

# 1. Set FRED API key
fredr_set_key("b1a5ad7c4a04ea912fc240e4bea05ba9")  # Replace with your actual key

# 2. Define target states
target_states <- c("AZ", "CA", "FL", "IL", "MI", "NJ", "NV", "NY", "OH", "PA", "TX")

# 3. Define correct FRED series IDs for total personal income (quarterly, millions of $)
state_series_ids <- c(
  AZ = "AZOTOT",
  CA = "CAOTOT",
  FL = "FLOTOT",
  IL = "ILOTOT",
  MI = "MIOTOT",
  NJ = "NJOTOT",
  NV = "NVOTOT",
  NY = "NYOTOT",
  OH = "OHOTOT",
  PA = "PAOTOT",
  TX = "TXOTOT"
)

# 4. Load credit card delinquency data from Excel
quarters <- c("2024Q1", "2024Q2", "2024Q3", "2024Q4")
files <- paste0("HHD_C_Report_", quarters, ".xlsx")
data_list <- list()

for (i in seq_along(files)) {
  df <- read_excel(files[i], sheet = "Page 33 Data", range = "A3:E100", col_names = TRUE)
  
  df_filtered <- df %>%
    filter(`...1` %in% target_states) %>%
    select(State = `...1`, CreditCard = `Credit Card`) %>%
    mutate(Quarter = quarters[i])
  
  data_list[[i]] <- df_filtered
}

delinquency_data <- bind_rows(data_list)

# 5. Load income data from FRED
income_data_list <- lapply(names(state_series_ids), function(state) {
  fredr(
    series_id = state_series_ids[[state]],
    observation_start = as.Date("2024-01-01"),
    observation_end = as.Date("2024-12-31"),
    frequency = "q"
  ) %>%
    mutate(State = state)
})

income_data <- bind_rows(income_data_list) %>%
  select(State, date, value) %>%
  rename(Income = value) %>%
  mutate(Quarter = paste0("2024Q", quarter(date)))

# 6. Average delinquency and income per state
avg_delinquency <- delinquency_data %>%
  group_by(State) %>%
  summarize(AvgCreditCard = mean(CreditCard, na.rm = TRUE))

avg_income <- income_data %>%
  group_by(State) %>%
  summarize(AvgIncome = mean(Income, na.rm = TRUE))  # in millions of $

# 7. Merge both datasets
merged_data <- avg_delinquency %>%
  inner_join(avg_income, by = "State")

# 8. Plot: Avg Income vs Avg Credit Card Delinquency
ggplot(merged_data, aes(x = AvgIncome, y = AvgCreditCard, label = State)) +
  geom_point(color = "darkblue", size = 3) +
  geom_text(nudge_y = 0.5) +
  labs(
    title = "2024 State Income vs Credit Card Delinquency",
    x = "Average Personal Income (Millions of $)",
    y = "Average Credit Card Delinquency (Thousands of $)"
  ) +
  theme_minimal()

# 9. Correlation
correlation <- cor(merged_data$AvgIncome, merged_data$AvgCreditCard, use = "complete.obs")
print(paste("Correlation between Income and Credit Card Delinquency:", round(correlation, 3)))


