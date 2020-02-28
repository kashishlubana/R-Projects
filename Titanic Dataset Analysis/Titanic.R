getwd()
setwd("C:/Users/Kashish/Desktop/Semester 1/First Steps into Case Studies")
titanic<-read.csv('Dataset_2_Titanic.txt' , header = TRUE  , sep = "," , na.strings=c("","NA"))
library(ggplot2)

table(is.na(titanic$Age))
split_name <- data.frame(do.call('rbind', strsplit(as.character(titanic$Name),',',fixed=TRUE)))
titanic$LastName<-split_name$X1
names(titanic)
titanic$FirstName<-split_title$X2
titanic$Title<-split_title$X1
names(titanic)
titanic2 <- titanic[,c(1,2,3,15,14,13,5,6,7,8,9,10,11,12)]
duplicated(titanic2$Ticket)

titanic2$Pclass<-as.factor(titanic2$Pclass)
titanic2$Survived<-as.factor(titanic2$Survived)
titanic2$Sex<-as.factor(titanic2$Sex)
titanic2$Embarked<-as.factor(titanic2$Embarked)

Survivedsubset<- subset(titanic2, Survived == 0 | Survived == 1)

ggplot(Survivedsubset , aes(x = Survived)) + 
  theme_bw()+
  geom_bar()+
  labs (y="Passenger Count")
prop.table(table(Survivedsubset$Survived))


#Female to male Survival ratio
GenderPlot<-ggplot(Survivedsubset, aes(x = Sex , fill = Survived)) + 
  theme_bw()+
  geom_bar()+
  labs (y="Passenger Count" , title="Female to Male Survival Stats")

#passenger class survival ratio
PclassPlot<-ggplot(Survivedsubset, aes(x = Pclass , fill = Survived)) + 
  theme_bw()+
  
  geom_bar()+
  labs (y="Passenger Count" , title="PClass Survival Stats")

#Relationship between passenger class, gender and Survival
Pclassgender<-ggplot(Survivedsubset, aes(x = Sex , fill = Survived)) + 
  theme_bw()+
  facet_wrap(~Pclass)+
  geom_bar()+
  labs (y="Passenger Count" , title="Passenger class and Gender Survival Stats")

# Did age play a role in survival of a passenger
AgeSurvival<-ggplot(Survivedsubset, aes(x = Age , fill = Survived)) + 
  theme_bw()+
  geom_histogram(binwidth = 5)+
  labs (y="Passenger Count" ,x = "Age (binwidth=5)" , title="Age Survival Stats")


#Did boarding from a certain port play a role in passenger's survival?


Embarked_New <- subset(Survivedsubset, Embarked=="Q" |Embarked=="C"|Embarked=="S")

ggplot(Embarked_New, aes(x = Embarked , fill = Survived)) + 
  theme_bw()+
  geom_bar()+
  labs (y="Number of Passengers Survived" , x = "Port" , title="Passenger Embarked From")




#What is the relation between passenger's Age,sex and Embarkment with his/her Survial ?

ggplot(Embarked_New, aes(x = Age , fill = Survived)) + 
  theme_bw()+
  facet_wrap(Sex~Embarked)+
  geom_density(alpha = 0.5)+
  labs (y="Survived" , x = "Age" , title="Passenger Age, Embarkment and Gender Survival Stats")


# Family Size vs survival plot

Survivedsubset$FamilySize <- Survivedsubset$SibSp + Survivedsubset$ParCh +1

ggplot(Survivedsubset, aes(x = FamilySize , fill = Survived)) + 
  theme_bw()+
  geom_histogram(binwidth = 1)+
  labs (y="Number of Passengers Survived" , x = "Family Members" , title="Family Survival Ratio")


