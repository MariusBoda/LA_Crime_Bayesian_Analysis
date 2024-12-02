# Load necessary libraries
library(dplyr)
library(lubridate)
library(ggplot2)

# Read the data
df <- read.csv("data/Data - Sheet2.csv")

# Convert TIME OCC to a proper time format (assuming the column is numeric or a string)
df$TIME.OCC <- sprintf("%04d", df$TIME.OCC)  # Ensure 4 digits for consistency

# Convert to POSIXct for easier manipulation
df$TIME.OCC <- as.POSIXct(df$TIME.OCC, format="%H%M", tz="UTC")

# Create 1-hour intervals by extracting hour and rounding to the start of each hour
df$Interval <- format(df$TIME.OCC, "%H:%M")
df$Interval <- sub(":(00|30|15|45)$", ":00", df$Interval)  # Round to the start of the hour

# Add a new column for grouping intervals into 1-hour blocks
df$Interval_Group <- format(df$TIME.OCC, "%H:00")

# Count total crimes in each interval group
total_crimes <- df %>%
  group_by(Interval_Group) %>%
  summarise(total = n())

# Count crimes with weapon used (non-empty Weapon Desc)
weapon_crimes <- df %>%
  filter(!is.na(Weapon.Desc) & Weapon.Desc != "") %>%
  group_by(Interval_Group) %>%
  summarise(weapon_used = n())

# Merge both total crimes and weapon crimes by interval group
merged_data <- merge(total_crimes, weapon_crimes, by = "Interval_Group", all.x = TRUE)

# Calculate fraction of crimes with a weapon used
merged_data$weapon_fraction <- merged_data$weapon_used / merged_data$total

# Handle any NA values (no weapon crimes for an interval)
merged_data$weapon_fraction[is.na(merged_data$weapon_fraction)] <- 0

# Print the results
print(merged_data)

# Plotting the fraction of crimes with a weapon used for each 1-hour block as a dot plot
ggplot(merged_data, aes(x = Interval_Group, y = weapon_fraction)) +
  geom_point(color = "blue", size = 4) +  # Dot plot
  labs(title = "Fraction of Crimes with a Weapon Used by 1-Hour Block",
       x = "1-Hour Block",
       y = "Fraction of Crimes with Weapon Used") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

