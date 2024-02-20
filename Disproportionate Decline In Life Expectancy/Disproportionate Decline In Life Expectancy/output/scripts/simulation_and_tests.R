################################################################################
## SIMULATION
################################################################################

# Load necessary libraries
library(dplyr)

# Set the seed for reproducibility
set.seed(123)

# Simulate years
years <- 2010:2021

# Simulate data
data <- expand.grid(year_id = years, racial_ethnic_group = c("Non-Hispanic White", "Non-White"))

# Simulate life expectancy for each group by year
data$life_exp <- ifelse(data$racial_ethnic_group == "Non-Hispanic White",
                        rnorm(nrow(data), mean = 78, sd = 2),  # Parameters for Non-Hispanic White
                        rnorm(nrow(data), mean = 75, sd = 3))  # Parameters for Non-White

# Simulate expected and observed death rates per 1000 people
data$expected_death_rate <- rnorm(nrow(data), mean = 8, sd = 1.5)
data$observed_death_rate <- data$expected_death_rate + rnorm(nrow(data), mean = 0, sd = 0.5)

# Print the first few rows of the simulated data
head(data)

# Write the data to a CSV file for tests
write.csv(data, "simulated_life_and_death_data.csv", row.names = FALSE)

################################################################################
## TESTS
################################################################################

# Test 1: Check for Missing Values
print("Test 1: Check for Missing Values")

# Check if there are any missing values in the key variables
missing_life_exp <- sum(is.na(data$life_exp))
missing_expected_death_rate <- sum(is.na(data$expected_death_rate))
missing_observed_death_rate <- sum(is.na(data$observed_death_rate))

cat("Missing Life Expectancy Values:", missing_life_exp, "\n")
cat("Missing Expected Death Rate Values:", missing_expected_death_rate, "\n")
cat("Missing Observed Death Rate Values:", missing_observed_death_rate, "\n")

################################################################################

# Test 2: Summary Statistics
print("Test 2: Summary Statistics")

# Calculate and print summary statistics for life expectancy
summary_stats_life_exp <- summary(data$life_exp)
cat("Life Expectancy Summary Statistics:\n")
print(summary_stats_life_exp)

# Calculate and print summary statistics for death rates
summary_stats_death_rate <- summary(data$expected_death_rate)
cat("Expected Death Rate Summary Statistics:\n")
print(summary_stats_death_rate)

summary_stats_observed_death <- summary(data$observed_death_rate)
cat("Observed Death Rate Summary Statistics:\n")
print(summary_stats_observed_death)

################################################################################

# Test 3: Count of Records per Group
print("Test 3: Count of Records per Group")

# Count and print the number of records for each racial-ethnic group
records_per_group <- table(data$`racial_ethnic_group`)
cat("Number of Records per Racial-Ethnic Group:\n")
print(records_per_group)




