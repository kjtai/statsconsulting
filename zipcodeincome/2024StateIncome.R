install.packages("fredr")
library(fredr)

fredr_set_key("b1a5ad7c4a04ea912fc240e4bea05ba9")

# Define target states and their corresponding series IDs
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

# Initialize an empty list to store data
income_data_list <- list()

# Loop through each state and retrieve data
for (state_abbr in names(state_series_ids)) {
  series_id <- state_series_ids[[state_abbr]]
  
  # Retrieve data for 2024
  data <- fredr(
    series_id = series_id,
    observation_start = as.Date("2024-01-01"),
    observation_end = as.Date("2024-12-31"),
    frequency = "q"
  )
  
  # Add state abbreviation to the data
  data$state <- state_abbr
  
  # Append to the list
  income_data_list[[state_abbr]] <- data
}

# Combine all data into a single data frame
income_data <- do.call(rbind, income_data_list)

# View the first few rows
head(income_data)

library(ggplot2)

ggplot(income_data, aes(x = date, y = value, color = state)) +
  geom_line() +
  labs(
    title = "Quarterly Personal Income by State (2024)",
    x = "Quarter",
    y = "Personal Income (Millions of Dollars)"
  ) +
  theme_minimal()




