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

# Optionally, write the data to a CSV file for later use
write.csv(data, "simulated_life_and_death_data.csv", row.names = FALSE)