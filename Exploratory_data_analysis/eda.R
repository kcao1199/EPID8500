# Load necessary libraries
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(summarytools)
library(naniar)
library(here)

# Load the cleaned dataset
data <- read.csv(here("data", "cleaned_cdc_data.csv")

# Inspect the Dataset
str(data)
summary(data)

# Check for Missing Values
missing_vals <- colSums(is.na(data))
print(missing_vals)

# Visualize missing data
gg_miss_var(data) +
  labs(title = "Missing Values by Column")

# Descriptive Statistics
summary_output <- dfSummary(data)  # Summary statistics
stview(summary_output, file = "results/tables/eda_summary.html")
webshot("results/tables/eda_summary.html", "results/tables/eda_summary.png")

# Frequency table of selected categorical variables
table(data$Locationdesc) 
table(data$strat_value) 

# Visualizations
# Distribution of prevalence
ggplot(data, aes(x = prevalence)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Data Values", x = "Data Value", y = "Frequency") +
  theme_minimal()

# Boxplot of prevalence by stratification group
ggplot(data, aes(x = strat_value, y = prevalence)) +
  geom_boxplot(fill = "orange", alpha = 0.6) +
  labs(title = "Boxplot of Data Values by Stratification Group", x = "Stratification Group", y = "Data Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatterplot: Confidence limits
ggplot(data, aes(x = conf_low, y = conf_high, color = strat_value)) +
  geom_point(alpha = 0.6) +
  labs(title = "Confidence Limits Scatterplot", x = "Low Confidence Limit", y = "High Confidence Limit") +
  theme_minimal()
