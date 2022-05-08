#Drug Overdose Stats

library(tidyverse)
library(lubridate)
library(ggplot2)


df <- read.csv('C:\\Users\\alecs\\Project_R\\Drug_Deaths.csv')
summary(df)

city <- df %>% filter(!ResidenceCity == '') %>% group_by(ResidenceCity) %>%
  summarise(total = n()) %>% arrange(desc(total))

ggplot(head(city, 15), aes(x = reorder(ResidenceCity, total), y = total)) + 
  geom_bar(stat='identity',fill='orange') + coord_flip() + labs(x = "Cities", y = "Total Deaths") + ggtitle('Death Count By Top 15 Cities')

age <- df  %>% group_by(Age) %>% summarise(total = n())
ggplot(na.omit(age), aes(x = Age, y=total)) + geom_count(fill='red') + labs(y= 'Total Deaths') + ggtitle('Death Count By Age')

gender <- df %>% filter(Sex == "Male" | Sex == "Female") %>% group_by(Sex) %>% summarise(total = n())
ggplot(gender, aes(x = reorder(Sex, total), y=total)) + geom_bar(stat="identity",fill ="blue") + labs(x="Gender", Y="Total Deaths")+
  ggtitle("Death Count by Gender")

race <- df %>% filter(Race != "") %>% group_by(Race) %>% summarise(total= n())

df %>% filter(AnyOpioid == "Y",!Race == "" & !Race == "Unknown") %>% group_by(Race) %>% summarise(total = n()) %>%
  ggplot(aes(x = reorder(Race,total), y = total)) + geom_bar(stat='identity',fill='red') + coord_flip()+
  labs(x = "Race", y = "Count of Opioid Present") + ggtitle("Opioids Present Within Race")






# Trying things out

df %>% pivot_longer(df, col = (Heroin:Other) , names_to = "Drug", values_to = "Total")

df %>% select(Heroin:Other)%>% filter(df[Heroin:Other == "Y"] ) %>% pivot_longer(df, col = (Heroin:Other), names_to = "Drug", values_to = "Total")

drugs <- df %>% filter(str_detect(c(Heroin:Other):"Y")) %>% pivot_longer(df, col = (Heroin:Other), names_to = "Drug", values_to = "Total")

drugs <- drugs %>% select(Drug,Total) %>% group_by(Drug) %>% mutate(sum_by_group = str_count(Total, "Y"))
# %>% pivot_longer(df, col = (Heroin:Other), names_to = "Drug", values_to = "Total")

drug <- df %>% select(Heroin:Other) %>% filter(Heroin == "Y")%>% group_by(df$ID) %>% summarise(total = n())