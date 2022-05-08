#Drug Overdose Stats

library(tidyverse)
library(lubridate)
library(ggplot2)


df <- read.csv('C:\\Users\\alecs\\Project_R\\Drug_Deaths.csv')
summary(df)

# creating a city variable with highest > lowest Residence City's by death count
city <- df %>% filter(!ResidenceCity == '') %>% group_by(ResidenceCity) %>%
  summarise(total = n()) %>% arrange(desc(total))
# graphing city death count
ggplot(head(city, 15), aes(x = reorder(ResidenceCity, total), y = total)) + 
  geom_bar(stat='identity',fill='orange') + coord_flip() + labs(x = "Cities", y = "Total Deaths") + ggtitle('Death Count By Top 15 Cities')

# creating variable of summed total deaths by age and graphing it
age <- df  %>% group_by(Age) %>% summarise(total = n())
ggplot(na.omit(age), aes(x = Age, y=total)) + geom_point() + labs(y= 'Total Deaths') + ggtitle('Death Count By Age')

age <- df  %>% group_by(Age) %>% summarise(total = n())
ggplot(na.omit(age), aes(x = Age)) + geom_density(aes(y=total)) + labs(y= 'Total Deaths') + ggtitle('Death Count By Age')

# creating variable of deaths by gender and graphing it into a bar chart
gender <- df %>% filter(Sex == "Male" | Sex == "Female") %>% group_by(Sex) %>% summarise(total = n())
ggplot(gender, aes(x = reorder(Sex, total), y=total)) + geom_bar(stat="identity",fill ="blue") + labs(x="Gender", Y="Total Deaths")+
  ggtitle("Death Count by Gender")

# creating new variable of counting deaths by race
race <- df %>% filter(Race != "") %>% group_by(Race) %>% summarise(total= n())
# filtering the data of race and graphing into a bar graph
df %>% filter(AnyOpioid == "Y",!Race == "" & !Race == "Unknown") %>% group_by(Race) %>% summarise(total = n()) %>%
  ggplot(aes(x = reorder(Race,total), y = total)) + geom_bar(stat='identity',fill='red') + coord_flip()+
  labs(x = "Race", y = "Count of Opioid Present") + ggtitle("Opioids Present Within Race")

# Setting the individual drug columns from "Y" to 1 if present and 0 if not
drugs = df %>% select(Heroin:Other) %>% colnames()
for (i in drugs){
  df[[i]] = ifelse(df[[i]] == "Y",1,0)
}

# Setting date column to date format
df$Date <- as.Date(df$Date,format="%m/%d/%Y")


