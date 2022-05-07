#Drug Overdose Stats

library(tidyverse)
library(lubridate)
library(ggplot2)


df <- read.csv('C:\\Users\\alecs\\Project_R\\Drug_Deaths.csv')
summary(df)

city <- df %>% filter(!ResidenceCity == '') %>% group_by(ResidenceCity) %>%
  summarise(total = n()) %>% arrange(desc(total))
ggplot(head(city, 15), aes(x = reorder(ResidenceCity, total), y = total)) + 
  geom_bar(stat='identity',fill='orange') + coord_flip() + labs(x = "Cities", y = "Total Deaths") + ggtitle('Death Count Top 15 Cities')

df %>% select(ResidenceState, ResidenceCity, ResidenceCounty, AnyOpioid)%>%
  summarise(total = sum(AnyOpioid))%>%
  arrange(ResidenceCity)