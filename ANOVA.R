# Install required packages if not already installed
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("knitr")) install.packages("knitr")
if (!require("kableExtra")) install.packages("kableExtra")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("grid")) install.packages("grid")

# Load necessary libraries
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(gridExtra)
library(grid)

# Load and prepare the data
data <- read.csv("data/Georgia_df.csv", stringsAsFactors = FALSE)

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

# Save the table as a PNG file
png("anova_results.png", width = 800, height = 600, res = 120)  # Set file name and dimensions

# Create a table using gridExtra and display it
grid.table(anova_df)

dev.off()  # Close the graphics device
