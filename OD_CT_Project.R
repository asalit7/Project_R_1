#Drug Overdose Stats
#Alec Salit

library(tidyverse)
library(lubridate)
library(ggplot2)

# loading in the dataset
df <- read.csv('C:\\Users\\alecs\\Project_R\\Drug_Deaths.csv')
summary(df)

# Setting the individual drug columns from "Y" to 1 if present and 0 if not
drugs = df %>% select(Heroin:Hydromorphone) %>% colnames()
for (i in drugs){
  df[[i]] = ifelse(df[[i]] == "Y",1,0)
}

# Setting date column to date format
df$Date <- as.Date(df$Date,format="%m/%d/%Y")

# Set up Year as a column
df[, "Year"] = format(df$Date, '%Y')

# creating a city variable with highest > lowest Residence City's by death count
city <- df %>% filter(!ResidenceCity == '') %>% group_by(ResidenceCity) %>%
  summarise(total = n()) %>% arrange(desc(total))

# graphing city death count
ggplot(head(city, 15), aes(x = reorder(ResidenceCity, total), y = total)) + 
  geom_bar(stat='identity',fill='orange') + coord_flip() + labs(x = "Cities", y = "Total Deaths") + ggtitle('Death Count By Top 15 Cities')

# creating variable of summed total deaths by age and graphing it
age <- df  %>% group_by(Age) %>% summarise(total = n())
ggplot(na.omit(age), aes(x = Age, y=total)) + geom_point() + labs(y= 'Total Deaths') + ggtitle('Death Count By Age')

# creating variable of deaths by gender and graphing it into a bar chart
gender <- df %>% filter(Sex == "Male" | Sex == "Female") %>% group_by(Sex) %>% summarise(total = n())
ggplot(gender, aes(x = reorder(Sex, total), y=total)) + geom_bar(stat="identity",fill ="blue") + labs(x="Gender", y="Total Deaths")+
  ggtitle("Death Count by Gender")

# filtering the data of race and graphing into a bar graph
df %>% filter(!Race == "" & !Race == "Unknown") %>% group_by(Race) %>% summarise(total = n()) %>%
  ggplot(aes(x = reorder(Race,total), y = total)) + geom_bar(stat='identity',fill='red') + coord_flip()+
  labs(x = "Race", y = "Deaths") + ggtitle("Accidental Drug Deaths by Race")

# Created a line graph of deaths over the years 
df %>% select(Heroin:Hydromorphone, Year) %>% filter(!is.na(Year))%>% group_by(Year) %>% summarise(total = n()) %>%
  ggplot(aes(x=Year, y=total, group=1)) + geom_line() + geom_point() + labs(y = "Total Deaths") + ggtitle("Total Deaths by Year")

# Create a density graph comparing deaths of Sex against Age
df %>% select(Age, Sex) %>% filter(Sex == 'Male'|Sex == 'Female') %>% group_by(Sex) %>%
  ggplot(aes(x=Age,color=Sex)) + geom_density() + labs(y= 'Density') + ggtitle("Comparing Deaths of Age and Gender")

# Create a graph viewing the total deaths by drug
df %>% select(Heroin:Hydromorphone) %>% summarise_all(sum) %>% 
  pivot_longer(cols = Heroin:Hydromorphone, names_to = "Drug", values_to = "Total")%>%
  ggplot(aes(x=reorder(Drug,Total), y = Total)) + geom_bar(stat='identity',fill='red') + coord_flip() + 
  labs(x="Drug Name", y="Total Deaths") + ggtitle("Total Deaths by Drug")

# Created a scatterplot of ages and genders for deaths by heroin
df %>% select(Heroin, Sex, Age) %>% filter(!is.na(Heroin), Heroin == 1,!is.na(Sex), Sex == 'Male'|Sex == 'Female', !is.na(Age)) %>% 
  group_by(Age, Sex) %>% summarise(total = n())%>%
  ggplot(aes(x = Age, y = total)) + geom_point(aes(color = Sex)) + labs(y="Death Count") + ggtitle("Deaths of Heroin by Age and Gender")

# Created a scatterplot of ages and genders for death by Fentanyl
df %>% select(Fentanyl, Sex, Age) %>% filter(!is.na(Fentanyl), Fentanyl == 1,!is.na(Sex), Sex == 'Male'|Sex == 'Female', !is.na(Age)) %>% 
  group_by(Age, Sex) %>% summarise(total = n())%>%
  ggplot(aes(x = Age, y = total)) + geom_point(aes(color = Sex)) + labs(y="Death Count") + ggtitle("Deaths of Fentanyl by Age and Gender")
