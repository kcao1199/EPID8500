# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(gt)
library(here)
library(webshot)

# Load and prepare the data
data <- read.csv(here("data", "cleaned_cdc_data.csv"))

# Filter for 2022 data
HIV_2022 <- data %>% 
  filter(year == "2022")

# Ensure strat_value is treated as a factor
HIV_2022 <- HIV_2022 %>%
  mutate(strat_value = as.factor(strat_value))

# Check if strat_value has more than one level for each strat_group
valid_groups <- HIV_2022 %>%
  group_by(strat_group) %>%
  filter(n_distinct(strat_value) > 1) %>%
  pull(strat_group) %>%
  unique()

# Filter out groups where strat_value has only one level
HIV_2022_valid <- HIV_2022 %>%
  filter(strat_group %in% valid_groups)

# Split the data by strat_group and fit a linear regression model for each group
regression_results <- HIV_2022_valid %>%
  group_by(strat_group) %>%
  do({
    # Fit the linear model for the current strat_group
    model <- lm(prevalence ~ strat_value, data = .)
    
    # Get model summary and extract coefficients and p-values
    summary_model <- summary(model)
    coef_table <- summary_model$coefficients
    
    # Create a data frame with coefficients and p-values
    result_df <- data.frame(
      StratGroup = unique(.$strat_group),
      StratValue = gsub("strat_value", "", rownames(coef_table)),
      Coefficient = round(coef_table[, 1], 4),
      StdError = round(coef_table[, 2], 4),
      tValue = round(coef_table[, 3], 4),
      pValue = round(coef_table[, 4], 4)
    )
    result_df
  })

# Combine all results into one data frame
regression_results_combined <- bind_rows(regression_results)

# Remove the intercept for clarity
regression_results_combined <- regression_results_combined[regression_results_combined$StratValue != "(Intercept)", ]

# Sort by p-value for clarity
regression_results_combined <- regression_results_combined[order(regression_results_combined$pValue), ]

# Create a table using gt
gt_table <- regression_results_combined %>%
  gt() %>%
  tab_header(
    title = "Coefficients and p-values for Stratification Determinants (Linear Regression)"
  ) %>%
  cols_label(
    StratValue = "Stratification Variable",
    Coefficient = "Coefficient",
    StdError = "Standard Error",
    tValue = "t-Value",
    pValue = "p-Value"
  ) %>%
  tab_spanner(
    label = "Regression Results",
    columns = vars(Coefficient, StdError, tValue, pValue)
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "#f2f2f2")
    ),
    locations = cells_title()
  ) %>%
  tab_style(
    style = cell_text(color = "blue"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_fill(color = "#ffe6e6", alpha = 0.5),
    locations = cells_body(
      columns = pValue,
      rows = pValue < 0.05
    )
  ) %>%
  tab_options(
    table.width = pct(90),
    table.font.size = 14
  )

gt_table

# Save the table as HTML first
html_path <- here("results", "tables", "regression_by_strat_group_valid.html")
gtsave(gt_table, html_path)

# Use webshot to save the table as PNG
png_path <- here("results", "tables", "regression_by_strat_group_valid.png")
webshot(html_path, png_path)
