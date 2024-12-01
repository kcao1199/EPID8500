# Load necessary libraries
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(webshot)
library(here)

# Load and prepare the data
data <- read.csv(here("data", "Georgia_df.csv"))

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
table <- kable(coef_df, 
               format = "html",
               caption = "Table: Coefficients and p-values for Stratification Determinants (Linear Regression)",
               digits = 4,
               col.names = c("Determinant", "Coefficient", "Standard Error", "t-Value", "p-Value"),
               row.names = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE)

path = here("results", "tables", "linear_regression.html")
# Save the table as an HTML file
save_kable(table, file = path)
webshot(path, "results/tables/linear_regression.png")
