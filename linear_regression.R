# Install required packages if not already installed
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("knitr")) install.packages("knitr")
if (!require("kableExtra")) install.packages("kableExtra")

# Load necessary libraries
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# Load and prepare the data
data <- read.csv("data/Georgia_df.csv", stringsAsFactors = FALSE)

# Fit a linear regression model
fitall <- lm(prevalence ~ strat_value, data = data)

# Extract coefficients and p-values from the model
coef_table <- summary(fitall)$coefficients

# Create a data frame with coefficients and p-values, rounding values
coef_df <- data.frame(
  StratValue = gsub("strat_value", "", rownames(coef_table)),  # Remove 'strat_value' prefix
  Coefficient = round(coef_table[, 1], 4),  # Coefficients
  StdError = round(coef_table[, 2], 4),  # Standard Errors
  tValue = round(coef_table[, 3], 4),  # t-Values
  pValue = round(coef_table[, 4], 4)  # p-Values
)

# Remove the intercept for clarity
coef_df <- coef_df[coef_df$StratValue != "(Intercept)", ]

# Rank by p-value in ascending order
coef_df <- coef_df[order(coef_df$pValue), ]

# Create a formatted HTML table using kable
table <-kable(coef_df, 
      format = "html",
      caption = "Table: Coefficients and p-values for Stratification Determinants (Linear Regression)",
      digits = 4,
      col.names = c("Determinant", "Coefficient", "Standard Error", "t-Value", "p-Value"),
      row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE)
# Save it
save_kable(table, file = "linear_regression.html")
