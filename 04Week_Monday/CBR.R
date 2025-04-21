library(tidyverse)


wpp_cbr<-read_csv("data/WPP2022_Demographic_Indicators_Medium 4.csv")
cbr<-wpp_cbr%>% 
  rename(region = Location,year=Time,cbr=CBR,cdr=CDR,pop=TPopulationFemale1July)%>%
  select(year,region,cbr,cdr,pop)

cbr%>%filter(region%in%c("Japan",
                         "United States of America",
                         "China",
                         "Mexico",
                         "France",year<2023))%>%
  ggplot(aes(x=year,y=cbr,group=region,color=region))+
  geom_line()+
  theme_bw()

## Save Plot and submit on canvas