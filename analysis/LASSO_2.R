# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(gt)
library(here)
library(webshot)
library(tidymodels)
library(glmnet)

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

# Define a recipe for preprocessing
rec <- recipe(prevalence ~ strat_value, data = HIV_2022_valid) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%  # Create dummy variables for strat_value
  step_normalize(all_numeric_predictors())       # Normalize numeric predictors

# Define LASSO model specification
lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>% set_engine("glmnet")

# Create a workflow
lasso_workflow <- workflow() %>%
  add_recipe(rec) %>%
  add_model(lasso_spec)

# Cross-validation for tuning the penalty
cv_splits <- vfold_cv(HIV_2022_valid, v = 5)
grid <- tibble(penalty = seq(0.0001, 0.01, length.out = 10))  # Smaller penalty grid

# Tune the model using grid search
lasso_tune <- tune_grid(
  lasso_workflow,
  resamples = cv_splits,
  grid = grid,
  control = control_grid(save_pred = TRUE)
)

# Select the best penalty based on RMSE
best_penalty <- select_best(lasso_tune, "rmse")

# Finalize the workflow and fit the model with the best penalty
final_lasso_workflow <- lasso_workflow %>% finalize_workflow(best_penalty)
final_lasso_model <- fit(final_lasso_workflow, data = HIV_2022_valid)

# Extract model coefficients and p-values
lasso_fit <- pull_workflow_fit(final_lasso_model)

# Convert to a data frame for coefficients and p-values
coef_table <- tidy(lasso_fit$fit)

# Clean up the 'term' names to remove "strat_value"
coef_table <- coef_table %>%
  mutate(term = gsub("strat_value_", "", term))  # Remove 'strat_value' from the variable names

# Filter out coefficients that are zero
non_zero_coefs <- coef_table %>%
  filter(estimate != 0) %>%
  arrange(desc(abs(estimate))) %>%
  select(term, estimate)

# Filter to include only the top 5 coefficients by absolute value
top_five_coefs <- non_zero_coefs %>%
  distinct(term, .keep_all = TRUE)  # Remove duplicates based on 'term'

# Ensure 'term' is treated as a factor for better display
top_five_coefs <- top_five_coefs %>%
  mutate(term = factor(term))

# Create a table for the top 5 coefficients using gt
gt_top_five_table <- top_five_coefs %>%
  gt() %>%
  tab_header(
    title = "Top 5 Non-Zero Coefficients for LASSO Model (2022)"
  ) %>%
  cols_label(
    term = "Stratification Variable",
    estimate = "Coefficient"
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
      rows = abs(estimate) > 0.5  # Example highlight for large coefficients
    )
  ) %>%
  tab_options(
    table.width = pct(90),
    table.font.size = 14,
    table.height = px(200)  # Further reduce table height
  )

# Display the table
gt_top_five_table

# Save the top 5 coefficients table as HTML
html_path_top_five <- here("results", "tables", "lasso_top_five_table.html")
gtsave(gt_top_five_table, html_path_top_five)

# Adjust webshot settings to reduce the extra space further
png_path_top_five <- here("results", "tables", "lasso_top_five_table.png")
webshot(html_path_top_five, png_path_top_five, vwidth = 1000, vheight = 300)  
