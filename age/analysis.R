library(readxl)
library(tidyverse)

data <- read_excel("HHD_C_Report_2024Q4.xlsx", sheet = "Page 27 Data", range = "A3:H103")
colnames(data)[1] <- "Quarter"

data_filtered <- data %>%
  filter(Quarter >= "18:Q1")

data_long <- data_filtered %>%
  pivot_longer(cols = -Quarter, names_to = "AgeGroup", values_to = "DelinquencyRate")

data_long$Quarter <- factor(data_long$Quarter, levels = unique(data_long$Quarter), ordered = TRUE)

data_long$Year <- substr(as.character(data_long$Quarter), 1, 2)

ggplot(data_long, aes(x = Quarter, y = DelinquencyRate, color = AgeGroup, group = AgeGroup)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_discrete(
    breaks = data_long$Quarter[grep("Q1", data_long$Quarter)],
    labels = function(x) paste0("20", substr(x, 1, 2))
  ) +
  labs(
    title = "Delinquency Rates by Age Group (2018–2024)",
    x = "Year",
    y = "Delinquency Rate (%)",
    color = "Age Group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

df_long <- data_filtered %>%
  pivot_longer(cols = -Quarter, names_to = "Age_Group", values_to = "Value")

anova_result <- aov(Value ~ Age_Group, data = df_long)
summary(anova_result)
