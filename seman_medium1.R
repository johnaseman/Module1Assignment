# John Seman
# Medium Post 1 Code
# 13 February 2025

library(tidyverse)
library(ggplot2)

file <- read_csv(file.choose())

# 1. Clean the GPA column by converting categorical values to numeric scores
file$GPA_numeric <- recode(file$`Grade Average Point (GAP) (in the last year)`,
                           "repeat" = 0,
                           "Excellent" = 4,
                           "Very good" = 3,
                           "Good" = 2,
                           "Fair" = 1)

# 2. Aggregate the PSQI component scores to create an overall sleep quality score
file$sleep_score <- rowSums(file[, grep("PSQI component", colnames(file))], na.rm = TRUE)

# 3. Remove unnecessary columns
file_clean <- file %>%
  select(Gender, Age, GPA_numeric, sleep_score)  

# 4. Check for missing values in the important columns (GPA and sleep_score)
file_clean <- file_clean %>%
  filter(!is.na(GPA_numeric) & !is.na(sleep_score))

# Create a bar chart to compare average GPA across different levels of sleep quality
file_clean %>%
  mutate(sleep_quality_level = case_when(
    sleep_score <= 5 ~ "Low Sleep Quality",
    sleep_score <= 10 ~ "Medium Sleep Quality",
    TRUE ~ "High Sleep Quality",
  )) %>%
  ggplot(aes(x = sleep_quality_level, y = GPA_numeric, fill = sleep_quality_level)) +
  geom_bar(stat = "summary", fun = "mean", width = 0.6, show.legend = FALSE) +
  labs(
    title = "Average GPA Across Sleep Quality Levels",
    x = "Sleep Quality Level",
    y = "Average GPA",
  ) +
  scale_fill_manual(values = c("Low Sleep Quality" = "#FF6F61", 
                               "Medium Sleep Quality" = "#FFB03B", 
                               "High Sleep Quality" = "#6B8E23"))  
