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
    # Convert DATE_OCC to POSIXct format, but we won't use it in the model
    DATE_OCC = as.POSIXct(DATE.OCC, format="%m/%d/%Y %I:%M:%S %p"),  # Adjusted format for datetime
    
    # Create 'day_of_week' as a factor with specific order
    day_of_week = factor(weekdays(DATE_OCC), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
    
    # Create binary outcome for 'assault with weapon' (Crm.Cd = 230)
    assault_with_weapon = ifelse(Crm.Cd == 230, 1, 0)  # 1 if Crm.Cd is 230, otherwise 0
  )

# Check the cleaned data
head(crime_data)

# Build the model (predicting whether the crime code is 230)
crime_model <- brm(
  formula = assault_with_weapon ~ Vict.Age + day_of_week + Premis.Cd + Crm.Cd,
  data = crime_data,
  family = bernoulli(),  # Logistic regression for binary outcome
  prior = c(
    prior(normal(0, 1), class = "b"),  # Priors for the regression coefficients
    prior(normal(0, 1), class = "Intercept")  # Prior for intercept
  ),
  iter = 2000,  # Number of iterations
  chains = 4,   # Number of Markov Chains
  warmup = 1000,  # Number of warmup iterations
  control = list(adapt_delta = 0.99)  # Control for better convergence
)

# Check the model summary
summary(crime_model)
