#ZIP CODE INCOME FROM IRS DATA - 2018-2022
library(dplyr)
library(readr)

# Define years
years <- 2018:2022

# Define file paths assuming all files are named like 18zpallagi.csv, 19zpallagi.csv, etc.
file_paths <- paste0("~/Desktop/", substr(years, 3, 4), "zpallagi.csv")

# Function to load each year's IRS file, calculate per capita income, and tag Year
load_irs_year <- function(file_path, year) {
  read_csv(file_path) %>%
    select(ZIPCODE = zipcode, A00100, N1) %>%
    mutate(
      PerCapitaIncome = (A00100 * 1000) / N1,  # undo 'in thousands'
      Year = year
    ) %>%
    select(ZIPCODE, Year, PerCapitaIncome)
}


# Load and process all years
irs_income_list <- lapply(seq_along(file_paths), function(i) {
  load_irs_year(file_paths[i], years[i])
})

# Combine all years together
irs_income_combined <- bind_rows(irs_income_list)

##Income Deciles for Zip Code 
#every ZIPCODE in each Year will be assigned an IncomeDecile from 1 (lowest) to 10 (highest)
irs_income_combined <- irs_income_combined %>%
  group_by(Year) %>%
  mutate(IncomeDecile = ntile(PerCapitaIncome, 10)) %>%
  ungroup()

head(irs_income_combined)



