# Load necessary libraries
library(dplyr)
library(stringr)

# Load the dataset
data <- read.csv("data/BRFSS__Table_of_HIV-AIDS_20241116.csv", stringsAsFactors = FALSE)

# View initial structure of the dataset
str(data)

# Selecting Columns to use
cleaned_data <- data %>%
  select(
    Year,
    Locationdesc,         # State Name
    Class,                # Class Description
    Topic,                # Topic Description
    Question,             # Question Text
    Response,             # Evaluated Response
    Break_Out,            # Stratification Value
    Break_Out_Category,   # Stratification Grouping
    Sample_Size,          # Sample size used
    Data_value,           # Main data value
    Confidence_limit_Low, # Low confidence limit
    Confidence_limit_High,# High confidence limit
    Data_value_type       # Type of data value (e.g., prevalence)
  )

# Removing rows with missing or NA values in critical columns
cleaned_data <- cleaned_data %>%
  filter(
    !is.na(Year) & 
      !is.na(Topic) & 
      !is.na(Question) & 
      !is.na(Data_value) &
      !is.na(Confidence_limit_High) &
      !is.na(Confidence_limit_Low)
  )

# Standardize column names for easier usage
cleaned_data <- cleaned_data %>%
  rename_with(~ str_to_lower(.), everything()) %>% # Convert column names to lowercase
  rename(
    year = year,
    state_name = locationdesc,
    class_desc = class,
    topic_desc = topic,
    question_text = question,
    response_text = response,
    strat_value = break_out,
    strat_group = break_out_category,
    sample_size = sample_size,
    prevalence = data_value,
    conf_low = confidence_limit_low,
    conf_high = confidence_limit_high,
    value_type = data_value_type
  )

# Remove duplicate rows if any
cleaned_data <- cleaned_data %>%
  distinct()

# Filter for Georgia-specific data
Georgia_df <- cleaned_data %>% 
  filter(state_name == "Georgia")

# Filtered for latest data
HIV_2023 <-cleaned_data %>% 
  filter(year == "2023")

# Export cleaned data to a new CSV file
write.csv(cleaned_data, "data/cleaned_cdc_data.csv", row.names = FALSE)
write.csv(Georgia_df, "data/Georgia_df.csv", row.names = FALSE)
write.csv(HIV_2023, "data/HIV_2023.csv", row.names = FALSE)

# Summary of the cleaned dataset
summary(cleaned_data)
