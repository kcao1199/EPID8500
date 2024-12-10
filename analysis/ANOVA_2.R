# Load necessary libraries
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(here)
library(gt)

# Load and prepare the data
data <- read.csv(here("data", "cleaned_cdc_data.csv"))

# Fit an ANOVA model
anova_model <- aov(prevalence ~ strat_value, data = data)

# Extract ANOVA table
anova_table <- summary(anova_model)[[1]]

# Create a data frame with ANOVA results
anova_df <- data.frame(
  Determinant = rownames(anova_table),  # Determinants (Factor Levels)
  SumSq = round(anova_table[, "Sum Sq"], 4),  # Sum of Squares
  Df = anova_table[, "Df"],  # Degrees of Freedom
  MeanSq = round(anova_table[, "Mean Sq"], 4),  # Mean Squares
  FValue = round(anova_table[, "F value"], 4),  # F-statistic
  pValue = round(anova_table[, "Pr(>F)"], 4)  # p-value
)

# Remove residual row from the table for clarity 
anova_df <- anova_df[rownames(anova_df) != "Residuals", ]

# Rank by p-value in ascending order 
anova_df <- anova_df[order(anova_df$pValue), ]

# Create a table using gt
gt_table <- anova_df %>%
  gt() %>%
  tab_header(
    title = "ANOVA Results for Stratification Determinants"
  ) %>%
  cols_label(
    Determinant = "Factor Level",
    SumSq = "Sum of Squares",
    Df = "Degrees of Freedom",
    MeanSq = "Mean Square",
    FValue = "F-Statistic",
    pValue = "p-Value"
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

# Display the table
gt_table

# Save the table as HTML
html_path <- here("results", "tables", "anova_results.html")
gtsave(gt_table, html_path)

# Use webshot to save the table as PNG
png_path <- here("results", "tables", "anova_results.png")
webshot(html_path, png_path)
