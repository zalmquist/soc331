# Load necessary libraries
library(tidyverse)
library(forecast)
library(scales)  # For formatting the y-axis labels

# Create the dataset with population in millions converted to full numbers
data <- tibble(
  Year = c(-10000, -5000, -3000, -2000, -1000, -500, 1, 500, 1000, 1200, 1300, 1350, 1400, 1500, 1600, 1700, 1800, 1850, 1900, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020, 2024),
  Population = c(5, 20, 60, 100, 120, 140, 185, 190, 280, 360, 400, 350, 370, 450, 550, 600, 1000, 1260, 1650, 2500, 3000, 3700, 4400, 5300, 6100, 6900, 7800, 8100) * 1e6,
  Era = c("End of last Ice Age", "Early civilizations forming", "Mesopotamia, Egypt rising", "Bronze Age empires", "Iron Age begins", "Rise of Rome, classical China", 
          "Roman Empire, Han Dynasty", "Fall of Western Rome, early Middle Ages", "Medieval period, rise of Islamic Golden Age", "Mongol Empire expansion", 
          "Pre-Black Death peak", "Black Death kills tens of millions", "Recovery after plague", "European Age of Exploration begins", "Colonial empires grow", 
          "Agricultural revolution spreads", "Start of Industrial Revolution", "Railroads, industrialization", "Before World Wars", "Post-WWII baby boom", 
          "Green Revolution begins", "Rapid development in Asia, Africa", "Global urbanization increases", "Fall of USSR, globalization", "Internet era begins", 
          "Rise of China, mobile tech", "COVID-19 pandemic", "Global population stabilizing in some regions"),
  EraStart = c(-10000, -5000, -3000, -2000, -1000, -500, 1, 500, 1000, 1200, 1300, 1350, 1400, 1500, 1600, 1700, 1800, 1850, 1900, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020, 2024),
  EraEnd = c(-5000, -3000, -2000, -1000, -500, 1, 500, 1000, 1200, 1300, 1350, 1400, 1500, 1600, 1700, 1800, 1850, 1900, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020, 2024, 2124)
)

# Use Exponential Smoothing (ETS model) for population forecasting
# Fit ETS model to the historical population data
ets_model <- ets(data$Population)

# Forecast the next 100 years (from 2024 to 2124)
forecast_result <- forecast(ets_model, h = 50)

# Generate a data frame with forecasted values
future_years <- tibble(Year = seq(2024, 2123-50, by = 1),
                       Population = forecast_result$mean)

# Combine historical and forecasted data
forecast_data <- bind_rows(data, future_years)

# Set the y-axis drop (250k per label)
label_positions <- seq(max(data$Population) - 250000 * (nrow(data) - 1), 
                       max(data$Population), 
                       by = 250000)

# Ensure that the length of `label_positions` matches the number of eras (28)
label_positions <- label_positions[1:nrow(data)]

# Plot the data with era labels as colored regions
label_positions <- label_positions[1:nrow(data)]

# Shift all label positions up by 10 billion
label_positions <- label_positions + 10e9

# Plot the data with era labels as colored regions
ggplot(forecast_data, aes(x = Year, y = Population)) +
  # Historical data points
  geom_point(data = data, color = "blue") + 
  # Forecasted line
  geom_line(color = "red") + 
  labs(title = "Population Growth and 100-Year Forecast (Exponential Model)", 
       x = "Year", 
       y = "Population (individuals)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  # Adding colored regions for eras
  geom_rect(data = data, aes(xmin = EraStart, xmax = EraEnd, ymin = 0, ymax = Inf, fill = Era), alpha = 0.3) +
  # Adding black, bold, era labels (without jitter) in the middle of each colored region
  geom_text(data = data, aes(x = (EraStart + EraEnd) / 2, 
                             y = label_positions, 
                             label = Era),
            size = 4, 
            angle = 90, 
            color = "black", 
            fontface = "bold") +
  scale_fill_viridis_d() +  # Choose a color palette for the eras
  scale_y_continuous(labels = label_comma()) +  # Format the y-axis without scientific notation
  theme(legend.position = "none")