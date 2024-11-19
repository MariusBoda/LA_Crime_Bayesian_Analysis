# Install necessary packages if not already installed
install.packages(c("brms", "rstan", "tidyverse", "sf"))

# Load the packages
library(brms)
library(rstan)
library(tidyverse)
library(sf)

crime_data <- read.csv("Data - Sheet1.csv")

# Convert relevant variables into proper formats
crime_data <- crime_data %>%
  mutate(
    DATE_OCC = as.Date(DATE_OCC),
    time_of_day = case_when(
      as.integer(format(TIME_OCC, "%H")) < 6 ~ "Night",
      as.integer(format(TIME_OCC, "%H")) < 12 ~ "Morning",
      as.integer(format(TIME_OCC, "%H")) < 18 ~ "Afternoon",
      TRUE ~ "Evening"
    ),
    day_of_week = factor(weekdays(DATE_OCC), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
    Vict_Sex = as.factor(Vict_Sex),
    Vict_Age = as.numeric(Vict_Age), # Ensure age is numeric
    Premis_Desc = as.factor(Premis_Desc),
    Crm_Cd_Desc = as.factor(Crm_Cd_Desc),
    AREA_NAME = as.factor(AREA_NAME)
  )

# Check the cleaned data
head(crime_data)

# Define a list of predefined hotspot areas
hotspot_areas <- tolower(c("hollywood", "central", "west la"))  # Ensure it's lowercase

# Clean the AREA.NAME column (remove extra spaces and convert to lowercase)
crime_data$AREA.NAME <- trimws(tolower(crime_data$AREA.NAME))

# Create a binary hotspot column based on area names
crime_data$hotspot <- ifelse(crime_data$AREA.NAME %in% hotspot_areas, 1, 0)

# Check the result
head(crime_data)

# Build the model
# Build the model
crime_model <- brm(
  formula = hotspot ~ Vict.Sex + Vict.Age + DATE.OCC + TIME.OCC + Premis.Cd + Crm.Cd + AREA.NAME,
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