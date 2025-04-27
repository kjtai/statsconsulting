#ZIP CODE INCOME FROM IRS DATA - 2018-2022

# https://www.irs.gov/statistics/soi-tax-stats-individual-income-tax-statistics-zip-code-data-soi
# Zipcode data 2011-recent (includes AGI)
# Define file paths assuming all files are named like 18zpallagi.csv, 19zpallagi.csv, etc.
# Replace "~/Desktop" with path/to/file
# --- 0. Load Required Libraries ---
library(dplyr)
library(readr)
library(fredr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(lubridate)

# --- 1. Load IRS ZIP Code Income Data (2018–2022) ---

years <- 2018:2022
file_paths <- paste0("~/Desktop/", substr(years, 3, 4), "zpallagi.csv")

load_irs_year <- function(file_path, year) {
  read_csv(file_path) %>%
    select(ZIPCODE = zipcode, A00100, N1) %>%
    mutate(
      PerCapitaIncome = (A00100 * 1000) / N1,  # Undo 'in thousands'
      Year = year
    ) %>%
    select(ZIPCODE, Year, PerCapitaIncome)
}

# Load and combine all years
irs_income_list <- lapply(seq_along(file_paths), function(i) {
  load_irs_year(file_paths[i], years[i])
})

irs_income_combined <- bind_rows(irs_income_list)

# Filter ZIPs with good income values
irs_income_combined <- irs_income_combined %>%
  filter(!is.na(PerCapitaIncome), PerCapitaIncome > 0)

# --- 2. Create Income Rank and Flags ---

# Simple fake ZIP to State mapping placeholder (replace with real if you have)
zipcode_state_mapping <- data.frame(
  ZIPCODE = irs_income_combined$ZIPCODE,
  State = substr(as.character(irs_income_combined$ZIPCODE), 1, 2)  # Just first 2 digits as placeholder
)

# Merge State info into IRS data
irs_income_combined <- irs_income_combined %>%
  left_join(zipcode_state_mapping, by = "ZIPCODE")

# Define Eighth District States (rough proxy)
eighth_district_states <- c("MO", "IL", "IN", "KY", "TN", "MS", "AR")

# Flag if ZIP is in the Eighth District
irs_income_combined <- irs_income_combined %>%
  mutate(
    Is_Eighth_District = ifelse(State %in% eighth_district_states, 1, 0)
  )

# Assign Income Rank and Group per Year
irs_income_combined <- irs_income_combined %>%
  group_by(Year) %>%
  mutate(
    IncomeRank = percent_rank(PerCapitaIncome),
    Group = case_when(
      IncomeRank <= 0.10 ~ "Poorest 10%",
      IncomeRank >= 0.90 ~ "Richest 10%",
      TRUE ~ "Middle 80%"
    )
  ) %>%
  ungroup()

# --- 3. Pull FRED Credit Card Delinquency Data ---

# Set your FRED API Key
fredr_set_key("b1a5ad7c4a04ea912fc240e4bea05ba9")  # Replace with your real FRED key

credit_card_delinquency <- fredr(
  series_id = "DRCCLACBS",  # Credit Card Loan Delinquency Rate
  observation_start = as.Date("2018-01-01"),
  observation_end = as.Date("2022-12-31"),
  frequency = "q"
) %>%
  mutate(Year = year(date))

credit_card_delinquency_yearly <- credit_card_delinquency %>%
  group_by(Year) %>%
  summarize(AnnualDelinquencyRate = mean(value, na.rm = TRUE))

# --- 4. Merge IRS Income with FRED Data and Simulate Delinquency ---

merged_data <- irs_income_combined %>%
  left_join(credit_card_delinquency_yearly, by = "Year")

merged_data <- merged_data %>%
  mutate(
    SimulatedDelinquencyRate = case_when(
      IncomeRank <= 0.10 ~ AnnualDelinquencyRate * 1.5,  # Poorest 10% higher delinquency
      IncomeRank >= 0.90 ~ AnnualDelinquencyRate * 0.7,  # Richest 10% lower delinquency
      TRUE ~ AnnualDelinquencyRate * 1.0                 # Middle income normal
    )
  )

# --- 5. Build the Final Group Labels ---

merged_data <- merged_data %>%
  mutate(FinalGroup = case_when(
    Is_Eighth_District == 1 ~ "Eighth District",
    Group == "Poorest 10%" ~ "Poorest 10%",
    Group == "Richest 10%" ~ "Richest 10%",
    TRUE ~ "US Overall"
  ))

# --- 6. Aggregate by Year and FinalGroup ---

delinquency_by_group <- merged_data %>%
  group_by(Year, FinalGroup) %>%
  summarize(AverageDelinquencyRate = mean(SimulatedDelinquencyRate, na.rm = TRUE)) %>%
  ungroup()

# --- 7. Plot the Trends by Group ---

ggplot(delinquency_by_group, aes(x = Year, y = AverageDelinquencyRate, color = FinalGroup)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Simulated Credit Card Delinquency Rates by Group",
    subtitle = "US Overall vs Eighth District vs Richest/Poorest 10% ZIPs",
    x = "Year",
    y = "Delinquency Rate (%)",
    color = "Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.title = element_text(face = "bold")
  )
