######################
## Soc 331 EC #1
######################

# Load Library
library(tidyverse)


kc<-read_csv("KingCountyDemPivot.csv")


# Create the population pyramid
age_order <- c("under 5", "5-9", "10-14", "15-17", "18-19", "20-21", "22-24", "25-29", 
               "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-61", "62-64", 
               "65-66", "67-69", "70-74", "75-79", "80-84", "85+")

kc$Age <- factor(kc$Age, levels = age_order)

# Plot the population pyramid
ggplot(kc, aes(x = Male, y = Age)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  geom_bar(aes(x = -Female), stat = "identity", fill = "pink") + 
  labs(
    title = "Population Pyramid for King County",
    x = "Population",
    y = "Age Groups"
  ) + 
  theme_minimal() + 
  scale_x_continuous(labels = scales::comma_format()) +  # Prevent scientific notation
  theme(axis.text.y = element_text(size = 8))  # Adjust the size of the axis text for readability