library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Define file names and quarters
quarters <- c("2024Q1", "2024Q2", "2024Q3", "2024Q4")
files <- paste0("HHD_C_Report_", quarters, ".xlsx")

# Initialize empty list to store data
data_list <- list()

# Loop through each file and extract relevant data
for (i in seq_along(files)) {
  df <- read_excel(files[i], sheet = "Page 33 Data", range = "A3:E100", col_names = TRUE)
  
  # Filter only target states
  states <- c("AZ", "CA", "FL", "IL", "MI", "NJ", "NV", "NY", "OH", "PA", "TX")
  df_filtered <- df %>%
    filter(`...1` %in% states) %>%  # column A might be unnamed (e.g. `...1`)
    select(State = `...1`, CreditCard = `Credit Card`) %>%
    mutate(Quarter = quarters[i])
  
  data_list[[i]] <- df_filtered
}

# Combine all quarters into one dataframe
all_data <- bind_rows(data_list)

ggplot(all_data, aes(x = Quarter, y = CreditCard, color = State, group = State)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Credit Card Delinquency by State (2024)",
       x = "Quarter",
       y = "Credit Card Delinquency (Thousands of $)") +
  theme_minimal()

