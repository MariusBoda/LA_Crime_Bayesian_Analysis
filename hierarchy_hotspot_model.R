# Install necessary packages if not already installed
install.packages(c("brms", "rstan", "tidyverse", "sf"))

# Load the packages
library(brms)
library(rstan)
library(tidyverse)
library(sf)

# Read in the crime data
crime_data <- read.csv("Data - Sheet1.csv")

# Convert relevant variables into proper formats
crime_data <- crime_data %>%
  mutate(
    # Convert DATE_OCC to POSIXct format
    DATE_OCC = as.POSIXct(DATE.OCC, format="%m/%d/%Y %I:%M:%S %p"),  # Adjusted format for datetime
    
    # Create 'day_of_week' as a factor with specific order
    day_of_week = factor(weekdays(DATE_OCC), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  )

# Check the cleaned data
head(crime_data)

# Define a list of predefined hotspot areas (ensure lowercase for consistency)
hotspot_areas <- tolower(c("central"))  

# Clean the AREA.NAME column (remove extra spaces and convert to lowercase)
crime_data$AREA.NAME <- trimws(tolower(crime_data$AREA.NAME))

# Create a binary hotspot column based on AREA_NAME matching predefined hotspots
crime_data$hotspot <- ifelse(crime_data$AREA.NAME %in% hotspot_areas, 1, 0)

# Check the result
head(crime_data)

# Add a column for Area (as a random effect), assuming this is available in your dataset
crime_data$AREA <- as.factor(crime_data$AREA)  # Assuming 'AREA' exists in your dataset

# Build the hierarchical model (with random effect for AREA)
crime_model_hierarchical <- brm(
  formula = hotspot ~ Vict.Age + day_of_week + Premis.Cd + Crm.Cd + (1 | AREA),
  data = crime_data,
  family = bernoulli(),  # Logistic regression for binary outcome
  prior = c(
    prior(normal(0, 1), class = "b"),  # Priors for the regression coefficients
    prior(normal(0, 1), class = "Intercept"),  # Prior for intercept
    prior(normal(0, 1), class = "sd")  # Prior for the standard deviation of the random effect
  ),
  iter = 2000,  # Number of iterations
  chains = 4,   # Number of Markov Chains
  warmup = 1000,  # Number of warmup iterations
  control = list(adapt_delta = 0.99)  # Control for better convergence
)

# Check the model summary
summary(crime_model_hierarchical)