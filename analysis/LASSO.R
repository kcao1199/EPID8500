# Install required packages if not already installed
if (!require("tidymodels")) install.packages("tidymodels")
if (!require("glmnet")) install.packages("glmnet")
if (!require("car")) install.packages("car")

# Load necessary libraries
library(tidymodels)
library(glmnet)
library(car)

# Load your dataset
data <- read.csv("data/Georgia_df.csv", stringsAsFactors = FALSE)

# Split data into training and testing sets
set.seed(123)
data_split <- initial_split(data, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

# Define a recipe for preprocessing
rec <- recipe(prevalence ~ strat_value, data = train_data) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%  # Create dummy variables
  step_normalize(all_numeric_predictors())       # Normalize all numeric predictors

# Prep and inspect preprocessed data (debugging step)
prepped_data <- prep(rec) %>% juice()
print(head(prepped_data))  # Inspect preprocessed training data

# Define the LASSO model specification
lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>%  # LASSO (mixture = 1)
  set_engine("glmnet")

# Create a workflow for model and preprocessing
lasso_workflow <- workflow() %>%
  add_recipe(rec) %>%
  add_model(lasso_spec)

# Perform cross-validation to tune the penalty (lambda) parameter
set.seed(123)
cv_splits <- vfold_cv(train_data, v = 5)

# Use a fine grid of penalties for better tuning
grid <- tibble(penalty = seq(0.001, 0.1, length.out = 10))  # Smaller penalty range

lasso_tune <- tune_grid(
  lasso_workflow,
  resamples = cv_splits,
  grid = grid,
  control = control_grid(save_pred = TRUE)
)

# View tuning results
tuning_results <- lasso_tune %>% collect_metrics()
print(tuning_results)

# Select the best penalty value based on RMSE
best_penalty <- lasso_tune %>% select_best("rmse")
print(best_penalty)

# Finalize the workflow with the best penalty
final_lasso_workflow <- lasso_workflow %>%
  finalize_workflow(best_penalty)

# Fit the final model
final_lasso_model <- fit(final_lasso_workflow, data = train_data)

# Predict on the test data
predictions <- predict(final_lasso_model, new_data = test_data)

# Evaluate predictions
test_results <- test_data %>%
  mutate(predicted_prevalence = predictions$.pred)

metrics <- test_results %>%
  metrics(truth = prevalence, estimate = predicted_prevalence)
print(metrics)  # Print performance metrics

# Check for multicollinearity 
print(vif(lm(prevalence ~ ., data = train_data)))

# Extract non-zero coefficients from the final model
lasso_fit <- pull_workflow_fit(final_lasso_model)
ranked_determinants <- tidy(lasso_fit$fit) %>%
  filter(estimate != 0) %>%  # Keep only non-zero coefficients
  arrange(desc(abs(estimate))) %>%  # Rank by absolute value of coefficients
  select(term, estimate)  # Select term (determinant) and coefficient

# Print the ranked determinants
print(ranked_determinants)

# Save non-zero coefficients as a CSV 
#write.csv(ranked_determinants, "ranked_determinants.csv", row.names = FALSE)
