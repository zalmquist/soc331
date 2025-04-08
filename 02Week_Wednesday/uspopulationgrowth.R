# List of required packages
required_packages <- c("tidyverse", "readxl", "janitor", "viridis", "stringr")

# Check if each package is installed, and install if not
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  # Load the package
  library(pkg, character.only = TRUE)
}


# "population-change-data-table.xlsx"
# Step 2: Read and clean
pop_data <- read_excel("population-change-data-table.xlsx", sheet = "Population Change", skip = 3) %>%
  clean_names()  # Makes column names syntactically safe

# Rename the first column (which becomes "state" after clean_names)
colnames(pop_data)[1] <- "state"

# Remove first row
pop_data<-pop_data%>%slice(-1)

# Select rows
pop_data<-pop_data%>%select(state, matches("^x\\d{4}_census$"))

# Rename
pop_data <- pop_data %>%
  select(state, matches("^x\\d{4}_census$")) %>%
  rename_with(
    ~ str_extract(., "\\d{4}"),  # Extract just the year
    matches("^x\\d{4}_census$")
  )%>%
  select(state, sort(names(.))[2:length(names(.))]) 

## Remove last two rows
pop_data<-pop_data%>%slice_head(n = NROW(pop_data) - 2)
pop_data<-pop_data%>%mutate(state = ifelse(state == "United States1", "United States", state))

# Pivot Long
pop_long <- pop_data  %>%
  pivot_longer(
    cols = -state,               # Exclude 'state' column from pivot
    names_to = "year",           # Create a new column 'year' from the column names (1910, 1920, etc.)
    values_to = "population"     # Create a new column 'population' for the corresponding values
  ) %>%
  mutate(
    year = as.integer(year),     # Ensure 'year' is an integer
    population = as.numeric(population),  # Ensure 'population' is numeric
    state = str_to_title(state)  # Capitalize the state names
  ) %>%
  filter(!is.na(population))  


# Plot
library(ggplot2)
library(viridis)

# Plot all states with different colors and US total population as red line
ggplot(pop_long, aes(x = year, y = population, group = state, color = state)) +
  geom_line(alpha = 0.5) +  # Line for each state with transparency
  geom_line(data = filter(pop_long, state == "United States"),
            aes(x = year, y = population),
            color = "red", size = 1.5) +  # Red line for the US total
  scale_y_continuous(labels = scales::comma_format()) +  # Disable scientific notation
  labs(
    title = "Population Growth of U.S. States and National Total (1910–2020)",
    x = "Year",
    y = "Population",
    caption = "Source: U.S. Census Bureau"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Move legend to the bottom
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  ) +
  scale_color_manual(values = c("United States" = "red", 
                                setNames(viridis::viridis(length(unique(pop_long$state)) - 1), 
                                         unique(pop_long$state)[unique(pop_long$state) != "United States"])))  # Custom color for states


## Remove regions
# Remove rows where state contains the word "region" (case-insensitive)
pop_long_cleaned <- pop_long %>%
  filter(!str_detect(state, fixed("region", ignore_case = TRUE)))  # Correct use of case-insensitive filtering

# Plot all states with different colors and US total population as red line
ggplot(pop_long_cleaned, aes(x = year, y = population, group = state, color = state)) +
  geom_line(alpha = 0.5) +  # Line for each state with transparency
  geom_line(data = filter(pop_long_cleaned, state == "United States"),
            aes(x = year, y = population),
            color = "red", size = 1.5) +  # Red line for the US total
  scale_y_continuous(labels = scales::comma_format()) +  # Disable scientific notation
  labs(
    title = "Population Growth of U.S. States and National Total (1910–2020)",
    x = "Year",
    y = "Population",
    caption = "Source: U.S. Census Bureau"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Move legend to the bottom
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    legend.title = element_blank()  # Remove the legend title
  ) +
  scale_color_manual(values = c("United States" = "red", 
                                setNames(viridis::viridis(length(unique(pop_long_cleaned$state)) - 1), 
                                         unique(pop_long_cleaned$state)[unique(pop_long_cleaned$state) != "United States"])))  # Custom color for states


## Add Label for California and Washington State
# Define the positions for arrows: selecting a point on the trend line for California and Washington (e.g., mid-1950s)
california_arrow_point <- pop_long_cleaned %>%
  filter(state == "California" & year == 2010) %>%  # Selecting a point (e.g., 1950 for California)
  select(year,state, population)

washington_arrow_point <- pop_long_cleaned %>%
  filter(state == "Washington" & year == 2000) %>%  # Selecting a point (e.g., 1950 for Washington)
  select(year, state, population)

# Plot all states and Label California and Washington
ggplot(pop_long_cleaned, aes(x = year, y = population, group = state, color = state)) +
  geom_line(alpha = 0.5) +  # Line for each state with transparency
  geom_line(data = filter(pop_long_cleaned, state == "United States"),
            aes(x = year, y = population),
            color = "red", size = 1.5) +  # Red line for the US total
  scale_y_continuous(labels = scales::comma_format()) +  # Disable scientific notation
  labs(
    title = "Population Growth of U.S. States and National Total (1910–2020)",
    x = "Year",
    y = "Population",
    caption = "Source: U.S. Census Bureau"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Move legend to the bottom
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    legend.title = element_blank()  # Remove the legend title
  ) +
  scale_color_manual(values = c("United States" = "red", 
                                setNames(viridis::viridis(length(unique(pop_long_cleaned$state)) - 1), 
                                         unique(pop_long_cleaned$state)[unique(pop_long_cleaned$state) != "United States"]))) +  # Custom color for states
  # Add arrows pointing from the center of the plot to the trend line of California at a selected point
  geom_segment(data = california_arrow_point,
               aes(x = 1990, xend = year, y = 250000000, yend = population), 
               arrow = arrow(type = "closed", length = unit(0.2, "inches")), color = "orange") +  # Arrow pointing to California (use same color)
  # Add arrows pointing from the center of the plot to the trend line of Washington at a selected point
  geom_segment(data = washington_arrow_point,
               aes(x = 1990, xend = year, y = 101000000, yend = population), 
               arrow = arrow(type = "closed", length = unit(0.2, "inches")), color = "purple") +  # Arrow pointing to Washington (use same color)
  # Add labels for California and Washington in the center of the plot, around 200 million for California and 100 million for Washington
  ## Fiddle with location of California and Washignton to be in a better place!!
  geom_text(data = data.frame(state = "California", year = 1990, population = 200000000),
            aes(label = "California"), color = "orange", fontface = "bold", size = 4, vjust = 1, hjust = 0.5) +  # Label for California
  geom_text(data = data.frame(state = "Washington", year = 1985, population = 100000000),
            aes(label = "Washington"), color = "purple", fontface = "bold", size = 4, vjust = 1, hjust = 0.5)  # Label for Washington



