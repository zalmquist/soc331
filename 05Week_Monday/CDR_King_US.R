library(tidyverse)


###########
# King County
###########
king<-read_csv("King County Deaths - Sheet1.csv")
king<-king%>%mutate(CDR=`Death Count`/`Population`,CDR=100000*CDR,Area="King County, WA")

############
## USA
############
# Read the file
data <- read_csv("Deaths_1x1.csv")
total<-read_csv("Population.csv")


data_clean <- data %>%
  mutate(across(c(Female, Male, Total), as.numeric)) %>%  # ensure numeric
  group_by(Year, Age) %>%
  summarise(
    Female = mean(Female, na.rm = TRUE),
    Male = mean(Male, na.rm = TRUE),
    Total = mean(Total, na.rm = TRUE),
    .groups = "drop"
  ) %>% filter(Year >= 2000)%>%group_by(Year)%>%summarise(Death=sum(Total))


pop_total<-total%>%group_by(Year)%>%summarise(Population=sum(Total))
pop_total<-pop_total%>%filter(Year>=2000)%>%mutate(Year=as.numeric(Year))

tab<-left_join(data_clean,pop_total,by="Year")
tab<-tab%>%mutate(CDR=Death/Population,CDR=CDR*100000,Area="USA")


df<-bind_rows(king%>%select(Year,CDR,Area),tab%>%select(Year,CDR,Area))
df<-df%>%mutate(Area=as.factor(Area))

df%>%ggplot(aes(x = Year, y = CDR, color = Area)) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    title = "Crude Death Rate Over Time",
    x = "Year",
    y = "CDR (Deaths per 100,000 Population)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



