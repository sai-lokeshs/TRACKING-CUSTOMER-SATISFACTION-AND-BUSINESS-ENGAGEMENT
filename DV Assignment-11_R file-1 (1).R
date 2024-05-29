library(dplyr)
library(tidyverse)
library(ggplot2)
data <- read.csv("/Users/sailokeshsiddanathi/Documents/dvp.csv",header= TRUE, sep = ",")

head(data,5)


#encoding conversions as 0 and 1 (instead of "yes" and "no")
data <- data %>%
  mutate(Response=ifelse(Response=="No", 0, 1))
data$Response <- as.integer(data$Response)

#rename column Response to Engaged
data <- data %>%
  rename(Engaged=Response)



#calculating engagement and conversion rate
EngagementRate <- data %>%
  group_by(Engaged) %>%
  summarize(Count=n()) %>%
  mutate(Percentage=Count/nrow(data)*100)

#calculate engagement/conversion rate
EngagementRate <- data %>%
  group_by(Engaged) %>%
  summarize(Count=n()) %>%
  mutate(Percentage=Count/nrow(data)*100)

#transposing the Column
transposed <- t(EngagementRate)
colnames(transposed) <- EngagementRate$Engaged
transposed <- transposed[-1,]
transposed


#To Analyze the engagement rates for different channels
SalesChannel <- data %>%
  group_by(Engaged, Sales.Channel) %>%
  summarize(Count=n()) %>%
  arrange(Sales.Channel)
SalesChannel

options(repr.plot.width = 15, repr.plot.height = 8)
ggplot(SalesChannel, aes(x="", y=Count, fill=Sales.Channel)) +
  geom_bar(stat = "identity", position = position_fill()) +
  geom_text(aes(x=1.15, label=Count), position = position_fill(vjust=0.5)) +
  coord_polar("y") + facet_wrap(~Engaged) +
  ggtitle("Sales Channel_SiddanaEDA (0: Not Engaged, 1: Engaged)") + 
  theme(legend.position = "bottom", legend.text=element_text(size=18), plot.title = element_text(size=20))


#As we can see on the pie charts, more than a half engaged customers came from Agents, 
#while non-engaged customers distributed more evenly across all 4 channels.

conversionsState <- data %>%
  group_by(State) %>%
  summarize(TotalCount=n(), NumConversions=sum(Engaged)) %>%
  mutate(ConversionRate=NumConversions/TotalCount*100)
conversionsState






conversionsEdu <- data %>%
  group_by(Education) %>%
  summarize(TotalCount=n(), NumConversions=sum(Engaged)) %>%
  mutate(ConversionRate=NumConversions/TotalCount*100)
conversionsEdu

ggplot(conversionsEdu, aes(x=Education, y=ConversionRate)) +
  geom_bar(width=0.5, stat = "identity", fill="Blue") +
  labs(title="Conversion Rates by Education_SiddanaEDA") + 
  theme(axis.text.x = element_text(size=14),axis.text.y = element_text(size=12), 
        axis.title = element_text(size = 15), plot.title = element_text(size=15))

#Result : Customers with Doctoral and Master's degrees tend to engage more.

#employment status
conversionsEmp <- data %>%
  group_by(EmploymentStatus) %>%
  summarize(TotalCount=n(), NumConversions=sum(Engaged)) %>%
  mutate(ConversionRate=NumConversions/TotalCount*100)
conversionsEmp

ggplot(conversionsEmp, aes(x=EmploymentStatus, y=ConversionRate)) +
  geom_bar(width=0.5, stat = "identity", fill="Red") +
  labs(title="Conversion Rates by Employment Status") +
  theme(axis.text.x = element_text(size=14),axis.text.y = element_text(size=14), 
        axis.title = element_text(size = 18), plot.title = element_text(size=18))
#Result : The engagement rate among retired customers is much higher compared to other groups.



#gender
conversionsGender <- data %>%
  group_by(Gender) %>%
  summarize(TotalCount=n(), NumConversions=sum(Engaged)) %>%
  mutate(ConversionRate=NumConversions/TotalCount*100)
conversionsGender

ggplot(conversionsGender, aes(x=Gender, y=ConversionRate)) +
  geom_bar(width=0.2, stat = "identity", fill="darkgreen") +
  labs(title="Conversion Rates by Gender") +
  theme(axis.text.x = element_text(size=14),axis.text.y = element_text(size=14), 
        axis.title = element_text(size = 18), plot.title = element_text(size=18))
#Result :I don't see any significant difference in the engagement rate between male and female customers.

#location code
conversionsLocation <- data %>%
  group_by(Location.Code) %>%
  summarize(TotalCount=n(), NumConversions=sum(Engaged)) %>%
  mutate(ConversionRate=NumConversions/TotalCount*100)

ggplot(conversionsLocation, aes(x=Location.Code, y=ConversionRate)) +
  geom_bar(width=0.5, stat = "identity", fill="Yellow") +
  labs(title="Conversion Rates by Location Code_Siddanas") +
  theme(axis.text.x = element_text(size=14),axis.text.y = element_text(size=14), 
        axis.title = element_text(size = 18), plot.title = element_text(size=18))
#Result : Customers with the location code "suburban" have a 2 times higher engagement rate.


#marital status
conversionsMarital <- data %>%
  group_by(Marital.Status) %>%
  summarize(TotalCount=n(), NumConversions=sum(Engaged)) %>%
  mutate(ConversionRate=NumConversions/TotalCount*100)
conversionsMarital

ggplot2(conversionsMarital, aes(x=Marital.Status, y=ConversionRate)) +
  geom_bar(width=0.5, stat = "identity", fill="magenta") +
  labs(title="Conversion Rates by Marital Status") +
  theme(axis.text.x = element_text(size=18),axis.text.y = element_text(size=18), 
        axis.title = element_text(size = 20), plot.title = element_text(size=22))

#Result : Divorced customers are more likely to engage.



#Regression Analysis (continuous variables)
#get numeric columns
continuous <-select_if(data, is.numeric)

#fit regression model with continuous variables
logit.fit <- glm(Engaged ~ ., data=continuous, family=binomial)
summary(logit.fit)

logit.fit <-glm(Engaged ~ factor(Education) + factor(Gender)+ factor(State) +
                  factor(Coverage) + factor(EmploymentStatus) +
                  factor(Location.Code) + factor(Marital.Status)+
                  factor(Policy.Type) + factor(Policy) + factor(Renew.Offer.Type) +
                  factor(Sales.Channel) + factor(Vehicle.Class) + 
                  factor(Vehicle.Size), data = data, family=binomial)
summary(logit.fit)


#Regression Analysis (continuous + categorical variables)

continuous$Education <- factor(data$Education)
continuous$Gender <- factor(data$Gender)
continuous$State <- factor(data$State)
continuous$Coverage <- factor(data$Coverage)
continuous$EmploymentStatus <- factor(data$EmploymentStatus)
continuous$Location.Code <- factor(data$Location.Code)
continuous$Marital.Status <- factor(data$Marital.Status)
continuous$Policy.Type <- factor(data$Policy.Type)
continuous$Policy <- factor(data$Policy)
continuous$Renew.Offer.Type <- factor(data$Renew.Offer.Type)
continuous$Sales.Channel <- factor(data$Sales.Channel)
continuous$Vehicle.Class <- factor(data$Vehicle.Class)
continuous$Vehicle.Size <- factor(data$Vehicle.Size)

#fit regression model with continuous and categorical variables
logit.fit <- glm(Engaged ~ ., data=continuous, family=binomial)
summary(logit.fit)






