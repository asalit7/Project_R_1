#Drug Overdose Stats

library(tidyverse)
library(lubridate)
library(ggplot2)


df <- read.csv('C:\\Users\\alecs\\Project_R\\Drug_Deaths.csv')
summary(df)

df %>% select(ResidenceState, ResidenceCity, ResidenceCounty, AnyOpioid)%>%
  summarise(total = sum(AnyOpioid))%>%
  arrange(ResidenceCity)

city <- df %>% group_by(ResidenceCity) %>%
  summarise(total = n()) %>% arrange(desc(total))