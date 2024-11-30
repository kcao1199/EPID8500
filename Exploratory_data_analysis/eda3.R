# Load necessary libraries
library(dplyr)
library(ggplot2)
library(maps)
library(RColorBrewer)
library(patchwork)

# Load the dataset
data <- read.csv("cleaned_cdc_data.csv", stringsAsFactors = FALSE)

#############################################################################
#############################################################################
# Summarize prevalence for Georgia
Georgia_df <- data %>%
  filter(state_name == "Georgia") %>%
  group_by(year) %>%
  summarize(prevalence = mean(prevalence, na.rm = TRUE)) %>%
  mutate(region = "Georgia") # Add a region column

# Summarize prevalence for the entire US
US_df <- data %>%
  group_by(year) %>%
  summarize(prevalence = mean(prevalence, na.rm = TRUE)) %>%
  mutate(region = "US") # Add a region column

# Combine the datasets
combined_df <- bind_rows(Georgia_df, US_df)

# Plot the combined time series
ggplot(combined_df, aes(x = year, y = prevalence, color = region, group = region)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(min(combined_df$year), max(combined_df$year), by = 1)) + # Set x-axis to whole numbers
  labs(title = "Time Series of HIV Prevalence in Georgia vs US",
       x = "Year", y = "Prevalence (%)", color = "Region") +
  theme_minimal()
#############################################################################
#############################################################################
# Distribution of prevalence rates in the US
ggplot(data, aes(x = prevalence)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "white", alpha = 0.7) +
  labs(title = "Distribution of Prevalence Rates", x = "Prevalence (%)", y = "Count") +
  theme_minimal()

# Age group stratification 
age_group_df <- data %>%
  filter(strat_group=='Age Group')%>%
  group_by(strat_value) %>%
  summarize(avg_prevalence = mean(prevalence, na.rm = TRUE))

colnames(age_group_df)[1] <- "Age Group"

#############################################################################
#############################################################################

# Load map data for the US states
us_map <- map_data("state")

# Summarize the prevalence data by state
state_df <- data %>%
  group_by(state_name) %>%
  summarize(avg_prevalence = mean(prevalence, na.rm = TRUE))

# Ensure the state names are in lowercase to match map data format
state_df$state_name <- tolower(state_df$state_name)

# Merge the map data with the state prevalence data
us_map_data <- left_join(us_map, state_df, by = c("region" = "state_name"))

# Plot the map with average prevalence
ggplot(us_map_data, aes(x = long, y = lat, group = group, fill = avg_prevalence)) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  labs(title = "Heatmap of HIV Prevalence Across US States", fill = "Average Prevalence (%)") +
  theme_minimal()

#############################################################################
#############################################################################

# Add a new column for 'source' to distinguish between datasets
data$source <- "USA"
Georgia_df <- data %>%
  filter(state_name == "Georgia") %>%
  mutate(source = "Georgia")

# Combine the datasets
combined_data <- bind_rows(data, Georgia_df)

# Side-by-side box plot with stratification groups and source
ggplot(combined_data, aes(x = strat_group, y = prevalence, fill = strat_group)) +
  geom_boxplot(position = position_dodge(width = 0.8), 
               outlier.size = 2, outlier.colour = "red", outlier.shape = 16) +  # Highlight outliers
  facet_wrap(~source, scales = "free_y") +  # Make sure each facet has its own y-axis range
  labs(title = "Prevalence by Stratification Group", 
       x = "Stratification Group", 
       y = "Prevalence (%)") +
  scale_fill_brewer(palette = "Set3") +  # Use a nice color palette
  theme_minimal(base_size = 14) +  # Clean minimal theme with larger text
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis text
    axis.title = element_text(size = 14, face = "bold"),  # Bold axis titles
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Centered plot title
    strip.text = element_text(size = 14, face = "bold"),  # Bold facet labels
    legend.position = "none"  # Remove the legend as it's unnecessary
  )

#############################################################################
#############################################################################

# Summarize the data by year and stratification group (mean prevalence)
summary_data <- data %>%
  group_by(year, strat_group) %>%
  summarize(mean_prevalence = mean(prevalence, na.rm = TRUE))

ggplot(summary_data, aes(x = year, y = mean_prevalence, color = strat_group)) +
  geom_line() +
  facet_wrap(~ strat_group, scales = "free_y") +
  scale_x_continuous(
    breaks = seq(min(summary_data$year), max(summary_data$year), by = 2)  # Skip every other year
  ) + 
  labs(title = "Time Trends by Stratification Group", x = "Year", y = "Mean Prevalence (%)") +
  theme_minimal()

#############################################################################
#############################################################################

var_within <- data %>%
  group_by(strat_group) %>%
  summarise(variance = var(prevalence, na.rm = TRUE))
var_within
