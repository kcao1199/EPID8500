# Load necessary libraries
library(dplyr)
library(stringr)

# Load the dataset
data <- read.csv("BRFSS__Table_of_HIV-AIDS_20241116.csv", stringsAsFactors = FALSE)

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
    Data_value_unit,      # Unit of the data value
    Data_value_type       # Type of data value (e.g., prevalence)
  )

# Removing rows with missing or NA values in critical columns
cleaned_data <- cleaned_data %>%
  filter(
    !is.na(Year) & 
      !is.na(Locationabbr) & 
      !is.na(Topic) & 
      !is.na(Question) & 
      !is.na(Data_value)
  )

# Standardize column names for easier usage
cleaned_data <- cleaned_data %>%
  rename_with(~ str_to_lower(.), everything()) %>% # Convert column names to lowercase
  rename(
    year = year,
    state_abbreviation = locationabbr,
    state_name = locationdesc,
    class_desc = class,
    topic_desc = topic,
    question_text = question,
    response_text = response,
    strat_value = break_out,
    strat_group = break_out_category,
    sample_size = sample_size,
    data_value = data_value,
    conf_low = confidence_limit_low,
    conf_high = confidence_limit_high,
    value_unit = data_value_unit,
    value_type = data_value_type
  )

# Remove duplicate rows if any
cleaned_data <- cleaned_data %>%
  distinct()

# Export cleaned data to a new CSV file
write.csv(cleaned_data, "cleaned_cdc_data.csv", row.names = FALSE)

# Summary of the cleaned dataset
summary(cleaned_data)
