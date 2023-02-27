
#importing data
Crime <- read.csv("~/Crime_Data_from_2010_to_2019.csv")
View(Crime)

#learning more about the data
str(Crime)
summary(Crime)

#loading necessary packages
library(tidyverse)
library(lubridate)
library(plyr)
library(dplyr)


#converting the date occured column to string
Crime$DATE.OCC<-as.character(Crime$DATE.OCC)
str(Crime)
#getting rid of the time to remain with just the date
Crime$DateOcc<-substr(Crime$DATE.OCC,1,nchar(Crime$DATE.OCC)-12)
head(Crime$DateOcc,10)
#converting the time variable to date
Crime$DateOcc<-as.Date(Crime$DateOcc, "%m/%d/%Y")
str(Crime)

#creating new variables for day, month and year when crime occured
#Year
trial<-as.character(Crime$DateOcc)
Crime$Year<-substr(trial,1,nchar(trial)-6)
Crime$Year<-as.factor(Crime$Year)
str(Crime)

#Month
Crime$MonthOfCrime<-month(Crime$DateOcc)
Crime$MonthOfCrime<-as.factor(Crime$MonthOfCrime)

Crime$MonthOfCrime2<-month(Crime$DateOcc,label=TRUE)
?month
Crime$MonthOfCrime2<-as.factor(Crime$MonthOfCrime2)
str(Crime)

#Day of the week
Crime$DayofWeek<-weekdays(Crime$DateOcc, abbreviate = T)
Crime$DayofWeek<-as.factor(Crime$DayofWeek)

#Day of the month
Crime$day<-substr(x = Crime$DateOcc, start = 9, stop = 10)
Crime$day<-as.numeric(Crime$day)
str(Crime)

#variable for whether a weapon was used or not 
#replacing empty values with no weapon
#Crime$Weapon.Desc<-dplyr::recode(Crime$Weapon.Desc, from = "", to = "NO WEAPON")
Crime$Weapon.Desc<-mapvalues(Crime$Weapon.Desc, from = "", to = "NO WEAPON")
#creating a variable that indicates 0 for no weapon used and 1 for a weapon used
Crime$weapon<-Crime$Weapon.Desc
levels(Crime$weapon)=c("0",
                       "1","1","1","1","1","1","1","1","1","1",
                       "1","1","1","1","1","1","1","1","1","1",
                       "1","1","1","1","1","1","1","1","1","1",
                       "1","1","1","1","1","1","1","1","1","1",
                       "1","1","1","1","1","1","1","1","1","1",
                       "1","1","1","1","1","1","1","1","1","1",
                       "1","1","1","1","1","1","1","1","1","1",
                       "1","1","1","1","1","1","1","1","1")
levels(Crime$weapon)=c("0","1")
Crime$weapon<-as.factor(Crime$weapon)
class(Crime$weapon)
#removing missing values from different variables
#descent
levels(Crime$Vict.Descent)
summary(Crime$Vict.Descent)
str(Crime$Vict.Descent)
Crime<-Crime[Crime$Vict.Descent!="",]
Crime<-Crime[Crime$Vict.Descent!="-",]


#gender
levels(Crime$Vict.Sex)
summary(Crime$Vict.Sex)
Crime<-Crime[Crime$Vict.Sex!="",]
Crime<-Crime[Crime$Vict.Sex!="-",]

#Premise
levels(Crime$Premis.Desc)
summary(Crime$Premis.Desc)
Crime<-Crime[Crime$Premis.Desc!="",]

#Status
levels(Crime$Status)
summary(Crime$Status)
summary(Crime$Status.Desc)
Crime<-Crime[Crime$Status!="",]


Crime<-droplevels(Crime)
str(Crime)

incase<-Crime

#plotting the data
#categorical variables
#Year
plot(Crime$Year,xlab="Year",main="Year of Crime",col="#FFA07A")

#Month
plot(Crime$MonthOfCrime2,xlab="Month",main="Month of Crime",col="#FFA07A")

#Days of the week
plot(Crime$DayofWeek,xlab="Day",main="Day of the Week of Crimes",col="#FFA07A")

#Days of the month
plot(as.factor(Crime$day),xlab="Day",main="Day of the Month of Crimes",col="#FFA07A")

#Time of crime occurence
hist(Crime$TIME.OCC,xlab="Time",main="Time of Occurence of Crimes",col="#FFA07A")


#Victim sex
plot(Crime$Vict.Sex,xlab="Gender",main="Gender of the victims",col="#FFA07A")

#Victim descent
Crime$Vict.Descent2<-Crime$Vict.Descent
levels(Crime$Vict.Descent2)
levels(Crime$Vict.Descent2) = c("Other Asian","Black",
                                "Chinese","Cambodian","Filipino",
                                "Guamanian","Hispanic/Latin/Mexican",
                                "American Indian/Alaskan Native",
                                "Japanese","Korean","Laotian ",
                                "Other","Pacific Islander",
                                "Samoan","Hawaiian","Vietnamese",
                                "White","Unknown","AsianIndian")

Crime %>%
  filter(!is.na(Vict.Descent2)) %>%
  group_by(Vict.Descent2) %>%
  tally() %>%
  ungroup() %>%
  mutate(Vict.Descent2 = reorder(Vict.Descent2,n)) %>%
  arrange(desc(n)) %>%
  head(20) %>%
  
  ggplot(aes(x = Vict.Descent2,y = n)) +
  geom_bar(stat='identity',colour="white", fill ="#FFA07A") +
  geom_text(aes(x = Vict.Descent2, y = 1, label = paste("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Victim Descent', y = 'Count of Incidents', 
       title = 'Count of Incidents and Victim Descent') +
  coord_flip() + 
  theme_bw()

#Victim age
hist(df2,xlab="Age",main="Victims Age",col="#FFA07A")
df1=(Crime$Vict.Age > 0)
df2=Crime$Vict.Age[df1]
table(df2)

#Type of crime
Crime %>%
  filter(!is.na(Crm.Cd.Desc)) %>%
  group_by(Crm.Cd.Desc) %>%
  tally() %>%
  ungroup() %>%
  mutate(Crm.Cd.Desc = reorder(Crm.Cd.Desc,n)) %>%
  arrange(desc(n)) %>%
  head(20) %>%
  
  ggplot(aes(x = Crm.Cd.Desc,y = n)) +
  geom_bar(stat='identity',colour="white", fill ="#FFA07A") +
  geom_text(aes(x = Crm.Cd.Desc, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Crime Description', y = 'Count of Incidents', 
       title = 'Count of Incidents and Type of Crime') +
  coord_flip() + 
  theme_bw()

#the weapon used
Crime %>%
  filter(!is.na(Weapon.Desc)) %>%
  group_by(Weapon.Desc) %>%
  tally() %>%
  ungroup() %>%
  mutate(Weapon.Desc = reorder(Weapon.Desc,n)) %>%
  arrange(desc(n)) %>%
  head(20) %>%
  
  ggplot(aes(x = Weapon.Desc,y = n)) +
  geom_bar(stat='identity',colour="white", fill ="#FFA07A") +
  geom_text(aes(x = Weapon.Desc, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Type of Weapon', y = 'Count of Incidents', 
       title = 'Count of Incidents and Type of Weapon used') +
  coord_flip() + 
  theme_bw()

#Status Description
plot(Crime$Status.Desc,xlab="Status Description",main="Status Description of the Crime",col="#FFA07A")


#Premise Description
Crime %>%
  filter(!is.na(Premis.Desc)) %>%
  group_by(Premis.Desc) %>%
  tally() %>%
  ungroup() %>%
  mutate(Premis.Desc = reorder(Premis.Desc,n)) %>%
  arrange(desc(n)) %>%
  head(20) %>%
  
  ggplot(aes(x = Premis.Desc,y = n)) +
  geom_bar(stat='identity',colour="white", fill ="#FFA07A") +
  geom_text(aes(x = Premis.Desc, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'PremiseDescription', y = 'Count of Incidents', 
       title = 'Count of Incidents and PremiseDescription') +
  coord_flip() + 
  theme_bw()


#Area name
Crime %>%
  filter(!is.na(AREA.NAME)) %>%
  group_by(AREA.NAME) %>%
  tally() %>%
  ungroup() %>%
  mutate(AREA.NAME = reorder(AREA.NAME,n)) %>%
  arrange(desc(n)) %>%
  head(20) %>%
  
  ggplot(aes(x = AREA.NAME,y = n)) +
  geom_bar(stat='identity',colour="white", fill ="#FFA07A") +
  geom_text(aes(x = AREA.NAME, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Area Name', y = 'Count of Incidents', 
       title = 'Count of Incidents and Area Name') +
  coord_flip() + 
  theme_bw()



#MODELING
#creating a dataframe that excludes victims with negative age
Crime2<-Crime

Crime3<-Crime2[Crime2$Vict.Age!=-9,]
Crime3<-Crime3[Crime3$Vict.Age!=-8,]
Crime3<-Crime3[Crime3$Vict.Age!=-7,]
Crime3<-Crime3[Crime3$Vict.Age!=-6,]
Crime3<-Crime3[Crime3$Vict.Age!=-5,]
Crime3<-Crime3[Crime3$Vict.Age!=-4,]
Crime3<-Crime3[Crime3$Vict.Age!=-3,]
Crime3<-Crime3[Crime3$Vict.Age!=-2,]
Crime3<-Crime3[Crime3$Vict.Age!=-1,]
table(Crime3$Vict.Age)

#new variable for gender of victims as male, female and other
rimmme<-Crime3
Crime3$Vict.Sex2<-Crime3$Vict.Sex
levels(Crime3$Vict.Sex2)<-c("F","Other","M","Other","Other")

#new variable for descent
Crime3$Vict.Descent2.1<-Crime3$Vict.Descent2
levels(Crime3$Vict.Descent2.1)
levels(Crime3$Vict.Descent2.1)<-c("Others","Black","Others","Others","Others",
                                  "Others","Hispanic/Latin/Mexican","Others","Others","Others",
                                  "Others","Others","Others","Others","Others",
                                  "Others","White","Others","Others")

#type of crime
Crime3$TypeCrime<-Crime3$Crm.Cd.Desc
levels(Crime3$TypeCrime)<-c("Others","Others","Others","ASSAULT","Others","BATTERY - SIMPLE ASSAULT","Others","Others","Others","Others",
                            "Others","Others","Others","Others","Others","Others","Others","Others","Others","Others",
                            "Others","BURGLARY","BURGLARY FROM VEHICLE","Others","Others","Others","Others","Others","Others","Others",
                            "Others","Others","Others","Others","Others","Others","Others","Others","Others","Others",
                            "Others","Others","Others","Others","Others","Others","Others","Others","Others","Others",
                            "Others","Others","Others","Others","Others","Others","Others","Others","Others","Others",
                            "Others","Others","Others","Others","Others","Others","Others","Others","Others","Others",
                            "Others","Others","Others","Others","Others","INTIM.PARTNER-SIMPLE.ASSAULT","Others","Others","Others","Others",
                            "Others","Others","Others","Others","Others","Others","Others","Others","Others","Others",
                            "Others","Others","Others","Others","Others","Others","Others","Others","Others","Others",
                            "Others","Others","Others","Others","Others","Others","Others","Others","Others","Others",
                            "Others","Others","Others","Others","Others","Others","Others","Others","THEFT.OF.IDENTITY","Others",
                            "THEFT PLAIN-PETTY ","Others","Others","Others","Others","Others","Others","Others","Others","Others",
                            "Others","Others","Others","VANDALISM-FELONY","Others","Others","Others","Others","Others","Others",
                            "Others","Others")
summary(Crime3$TypeCrime)


#Modeling

#predicting the age of a victim using various variables

regmodel<-lm(Vict.Age~TIME.OCC + day + DayofWeek + Year
             + MonthOfCrime2 + Vict.Descent2.1 +
               Vict.Sex2 + weapon + TypeCrime,data=Crime3)
summary(regmodel)

 
