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

years <- 2018:2022
file_paths <- paste0("~/Desktop/", substr(years, 3, 4), "zpallagi.csv")

load_irs_year <- function(file_path, year) {
  read_csv(file_path) %>%
    select(ZIPCODE = zipcode, STATE, A00100, N1) %>%  # <-- now keep STATE
    mutate(
      PerCapitaIncome = (A00100 * 1000) / N1,  # Undo 'in thousands'
      Year = year
    ) %>%
    select(ZIPCODE, STATE, Year, PerCapitaIncome)
}

# Load and combine all years
irs_income_list <- lapply(seq_along(file_paths), function(i) {
  load_irs_year(file_paths[i], years[i])
})

irs_income_combined <- bind_rows(irs_income_list)

# Clean: remove missing or invalid ZIPs
irs_income_combined <- irs_income_combined %>%
  filter(
    !is.na(PerCapitaIncome),
    PerCapitaIncome > 0,
    ZIPCODE != "00000",
    ZIPCODE != "99999",
    nchar(ZIPCODE) == 5,
    grepl("^[0-9]{5}$", ZIPCODE)  # make sure ZIPs are 5 digits only
  )

# Define Eighth District States
eighth_district_states <- c("MO", "IL", "IN", "KY", "TN", "MS", "AR")

# Flag if ZIP is in the Eighth District (using actual STATE column)
irs_income_combined <- irs_income_combined %>%
  mutate(
    Is_Eighth_District = ifelse(STATE %in% eighth_district_states, 1, 0)
  )

irs_income_combined <- irs_income_combined %>%
  group_by(Year) %>%
  mutate(
    IncomeRank = percent_rank(PerCapitaIncome),
    # Assign Richest and Poorest groups
    Group = case_when(
      IncomeRank <= 0.10 ~ "Poorest 10%",
      IncomeRank >= 0.90 ~ "Richest 10%",
      TRUE ~ "Middle 80%"
    ),
    # Flag for Eighth District membership
    Is_Eighth_District = ifelse(STATE %in% eighth_district_states, 1, 0),
    # Assign DisplayGroup (for showing different slices)
    DisplayGroup = case_when(
      Is_Eighth_District == 1 ~ "Eighth District",
      IncomeRank <= 0.10 ~ "Poorest 10%",
      IncomeRank >= 0.90 ~ "Richest 10%",
      TRUE ~ "United States"
    )
  ) %>%
  ungroup()


# Look at results
head(irs_income_combined)


# --- 3. Pull FRED Credit Card Delinquency Data ---

# Set your FRED API Key
fredr_set_key("b1a5ad7c4a04ea912fc240e4bea05ba9")  # Replace with your real FRED key

delinquency_data <- fredr(
  series_id = "DRCCLACBS",
  observation_start = as.Date("2018-01-01"),
  observation_end = as.Date("2022-12-31"),
  frequency = "a"  # annual
)

# Prepare delinquency dataset
delinquency_data <- delinquency_data %>%
  select(Year = date, DelinquencyRate = value) %>%
  mutate(Year = as.integer(format(Year, "%Y")))

# Merge onto IRS income data
irs_income_with_delinquency <- irs_income_combined %>%
  left_join(delinquency_data, by = "Year")

###How does per capita income differ between ZIPs in different income groups while national delinquency rates are rising or falling?
group_summary <- irs_income_with_delinquency %>%
  group_by(Year, Group) %>%
  summarize(
    AvgPerCapitaIncome = mean(PerCapitaIncome, na.rm = TRUE),
    DelinquencyRate = first(DelinquencyRate)  # same for all ZIPs that year
  ) %>%
  ungroup()

print(group_summary)

###Graphs!

delinquency_national <- fredr(
  series_id = "DRCCLACBS", 
  observation_start = as.Date("2018-01-01"),
  observation_end = as.Date("2022-12-31"),
  frequency = "a"
) %>%
  select(Year = date, DelinquencyRate = value) %>%
  mutate(Year = as.integer(format(Year, "%Y")))

irs_income_with_delinquency <- irs_income_combined %>%
  left_join(delinquency_national, by = "Year")

# Summarize by group
income_summary <- irs_income_with_delinquency %>%
  filter(DisplayGroup %in% c("Poorest 10%", "Richest 10%", "Eighth District", "United States")) %>%
  group_by(Year, DisplayGroup) %>%
  summarize(
    AvgIncome = mean(PerCapitaIncome, na.rm = TRUE),
    .groups = "drop"
  )

# Delinquency rate stays separate
delinquency_summary <- delinquency_national

# Now plot
ggplot() +
  geom_line(data = income_summary, aes(x = Year, y = AvgIncome, color = DisplayGroup), size = 1.2) +
  geom_point(data = income_summary, aes(x = Year, y = AvgIncome, color = DisplayGroup), size = 2) +
  geom_line(data = delinquency_summary, aes(x = Year, y = DelinquencyRate * 10000), color = "black", linetype = "dashed") +  # scale to match
  labs(
    title = "Income Trends by Group with National Delinquency Rate",
    y = "Per Capita Income (USD)",
    x = "Year",
    color = "Group"
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~./10000, name = "Delinquency Rate (%)")
  ) +
  theme_minimal()


# Fit a simple regression of Income ~ Year for each ZIP
zip_trends <- irs_income_with_delinquency %>%
  group_by(ZIPCODE) %>%
  do(model = lm(PerCapitaIncome ~ Year, data = .)) %>%
  mutate(Slope = coef(model)[["Year"]]) %>%
  select(ZIPCODE, Slope)

# Look at results
head(zip_trends)




