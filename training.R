#Reading Data 1979 and 2014
setwd("C:/Users/C_v/Desktop/UK accidents/R-Directory")  # note / instead of \ in windows
library(readr)
Accidents <- read_csv("C:/Users/C_v/Desktop/UK accidents/R-Directory/Accidents19792014AllAttributes.csv")
names(df1)
library(dplyr)
glimpse(Accidents_Vehicles_Casualties)
######## Cleaning data #################
sum(is.na(df1$Longitude))
sum(is.na(df1$Latitude))
sum(is.na(df1$Location_Easting_OSGR))
sum(is.na(df1$Location_Northing_OSGR))
df1$Longitude[which(is.na(df1$Longitude))]<- 0
df1$Latitude[which(is.na(df1$Latitude))]<- 0

df1$Location_Easting_OSGR[which(is.na(df1$Location_Easting_OSGR))]<- 0
df1$Location_Northing_OSGR[which(is.na(df1$Location_Northing_OSGR))]<- 0
#AccidentsData7904[AccidentsData7904=="-1"] <- 0
df1$Longitude<-as.numeric(df1$Longitude)
df1$Latitude<-as.numeric(df1$Latitude)

###

tail(df1,n=15)
## Coverting date and time
df1$Time <- as.integer(substr(sprintf("%04d", df1$Time), 1, 2))

#df <- data.frame(x = c("a", "a b", "a b c", NA))
install.packages("tidyr")
library("tidyr")
#d <- AccidentsData7904
df1$Month <- format(as.Date(df1$Date), "%m")
df1$Date_of_Month<- format(as.Date(df1$Date), "%y")
df1$Year<- format(as.Date(df1$Date,format="%d/%m/%Y"),"%Y")

df1$Month<-as.numeric(df1$Month)
df1$Date_of_Month<-as.numeric(df1$Date_of_Month)
df1$Year<-as.numeric(df1$Year)

#df1 <- subset( df1, select = -Date )

###
df2 <- read_csv("C:/Users/C_v/Desktop/UK accidents/UK dataset/Stats19_Data_2005-2014/Accidents0514.csv")
names(df2)
library(dplyr)
glimpse(df2)
######## Cleaning data #################
sum(is.na(df2$Longitude))
sum(is.na(df2$Latitude))
sum(is.na(df2$Location_Easting_OSGR))
sum(is.na(df2$Location_Northing_OSGR))
df2$Longitude[which(is.na(df2$Longitude))]<- 0
df2$Latitude[which(is.na(df2$Latitude))]<- 0

Accidents0514$Location_Easting_OSGR[which(is.na(Accidents0514$Location_Easting_OSGR))]<- 0
Accidents0514$Location_Northing_OSGR[which(is.na(Accidents0514$Location_Northing_OSGR))]<- 0
#AccidentsData7904[AccidentsData7904=="-1"] <- 0
#df2$Longitude<-as.numeric(df2$Longitude)
#df2$Latitude<-as.numeric(df2$Latitude)

###

tail(df2,n=15)
## Coverting date and time
df2$Time <- as.integer(substr(sprintf("%04d", df2$Time), 1, 2))

#df <- data.frame(x = c("a", "a b", "a b c", NA))
install.packages("tidyr")
library("tidyr")
#d <- AccidentsData7904
df2$Month <- format(as.Date(df2$Date), "%m")
df2$Date_of_Month<- format(as.Date(df2$Date), "%y")
df2$Year<- format(as.Date(df2$Date,format="%d/%m/%Y"),"%Y")

df2$Month<-as.numeric(df2$Month)
df2$Date_of_Month<-as.numeric(df2$Date_of_Month)
df2$Year<-as.numeric(df2$Year)

#df1 <- subset( df1, select = -Date )

#merge by row
Accidents <- rbind(Accidents7904,Accidents0514)
glimpse(df3)
write.csv(df3, file = "Accidents19792014AllAttributes.csv")
### All attributes
#### Select Attributes
df3<- subset(df3, select = -Accident_Index )
glimpse(Accidents)
#AccidentsData7904 <- subset( AccidentsData7904, select = -("Location_Easting_OSGR","Location_Northing_OSGR","Local_Authority_(Highway)"))
# exclude variables v1, v2, v3
myvars <- names(df3) %in% c("Location_Easting_OSGR","Location_Northing_OSGR","Local_Authority_(Highway)") 
df3<- df3[!myvars]
myvars <- names(df3) %in% c("1st_Road_Class","1st_Road_Number","2nd_Road_Class","2nd_Road_Number","LSOA_of_Accident_Location") 
df3<- df3[!myvars]
Accidents <- df3
Accidents <- subset(Accidents, select = -X1 )

########## Vehicles
Vehicles <- read_csv("C:/Users/C_v/Desktop/UK accidents/R-Directory/Vehicles1979to2014AllAttributes.csv")
df2 <- read_csv("C:/Users/C_v/Desktop/UK accidents/UK dataset/Stats19_Data_2005-2014/Vehicles0514.csv")
#####
names(Accidents)
Vehicles <- subset(Vehicles, select = -X1 )
Accidents <- subset(Accidents, select = -Vehicle_Reference) 
Accidents <- subset(Accidents, select = -Driver_IMD_Decile)
Accidents <- subset(Accidents, select = -Driver_Home_Area_Type)

Accidents <- subset(Accidents, select = -Driver_Home_Area_Type)
myvars <- names(Accidents) %in% c("Driver_Home_Area_Type","Driver_IMD_Decile","Journey_Purpose_of_Driver","Was_Vehicle_Left_Hand_Drive?","Hit_Object_off_Carriageway","Vehicle_Leaving_Carriageway","Hit_Object_in_Carriageway","Skidding_and_Overturning","Junction_Location","Vehicle_Location-Restricted_Lane","Towing_and_Articulation","Vehicle_Reference") 
Accidents<- Accidents[!myvars]
#### Casualties
df1 <- read_csv("C:/Users/C_v/Desktop/UK accidents/UK dataset/Stats19-Data1979-2004/Casualty7904.csv")
df2 <- read_csv("C:/Users/C_v/Desktop/UK accidents/UK dataset/Stats19_Data_2005-2014/Casualties0514.csv")
names(df1)
names(df2)
#### Data set 1 accident + casulties
Accidents$Vehicle_Reference<- Casualties$Vehicle_Reference[ match(Accidents$Accident_Index, Casualties$Accident_Index)]
Accidents$Casualty_Reference<- Casualties$Casualty_Reference[ match(Accidents$Accident_Index, Casualties$Accident_Index)]
Accidents$Casualty_Class<- Casualties$Casualty_Class[ match(Accidents$Accident_Index, Casualties$Accident_Index)]
Accidents$Sex_of_Casualty<- Casualties$Sex_of_Casualty[ match(Accidents$Accident_Index, Casualties$Accident_Index)]
Accidents$Age_of_Casualty<- Casualties$Age_of_Casualty[ match(Accidents$Accident_Index, Casualties$Accident_Index)]
Accidents$Age_Band_of_Casualty<- Casualties$Age_Band_of_Casualty[ match(Accidents$Accident_Index, Casualties$Accident_Index)]
Accidents$Casualty_Severity<- Casualties$Casualty_Severity[ match(Accidents$Accident_Index, Casualties$Accident_Index)]
Accidents$Pedestrian_Location<- Casualties$Pedestrian_Location[ match(Accidents$Accident_Index, Casualties$Accident_Index)]
Accidents$Pedestrian_Movement<- Casualties$Pedestrian_Movement[ match(Accidents$Accident_Index, Casualties$Accident_Index)]
Accidents$Car_Passenger<- Casualties$Car_Passenger[ match(Accidents$Accident_Index, Casualties$Accident_Index)]
Accidents$Bus_or_Coach_Passenger<- Casualties$Bus_or_Coach_Passenger[ match(Accidents$Accident_Index, Casualties$Accident_Index)]
Accidents$Pedestrian_Road_Maintenance_Worker<- Casualties$Pedestrian_Road_Maintenance_Worker[ match(Accidents$Accident_Index, Casualties$Accident_Index)]
Accidents$Casualty_Type<- Casualties$Casualty_Type[ match(Accidents$Accident_Index, Casualties$Accident_Index)]
Accidents$Casualty_Home_Area_Type<- Casualties$Casualty_Home_Area_Type[ match(Accidents$Accident_Index, Casualties$Accident_Index)]
######
Accidents$Vehicle_Reference<- Vehicles$Vehicle_Reference[ match(Accidents$Accident_Index, Vehicles$Accident_Index)]
Accidents$Vehicle_Type<- Vehicles$Vehicle_Type[ match(Accidents$Accident_Index, Vehicles$Accident_Index)]
Accidents$Towing_and_Articulation<- Vehicles$Towing_and_Articulation[ match(Accidents$Accident_Index, Vehicles$Accident_Index)]
Accidents$Vehicle_Manoeuvre<- Vehicles$Vehicle_Manoeuvre[ match(Accidents$Accident_Index, Vehicles$Accident_Index)]
Accidents$`Vehicle_Location-Restricted_Lane`<- Vehicles$`Vehicle_Location-Restricted_Lane`[ match(Accidents$Accident_Index, Vehicles$Accident_Index)]
Accidents$Junction_Location<- Vehicles$Junction_Location[ match(Accidents$Accident_Index, Vehicles$Accident_Index)]
Accidents$Skidding_and_Overturning<- Vehicles$Skidding_and_Overturning[ match(Accidents$Accident_Index, Vehicles$Accident_Index)]
Accidents$Hit_Object_in_Carriageway<- Vehicles$Hit_Object_in_Carriageway[ match(Accidents$Accident_Index, Vehicles$Accident_Index)]
Accidents$Vehicle_Leaving_Carriageway<- Vehicles$Vehicle_Leaving_Carriageway[ match(Accidents$Accident_Index, Vehicles$Accident_Index)]
Accidents$Hit_Object_off_Carriageway<- Vehicles$Hit_Object_off_Carriageway[ match(Accidents$Accident_Index, Vehicles$Accident_Index)]
Accidents$`1st_Point_of_Impact`<- Vehicles$`1st_Point_of_Impact`[ match(Accidents$Accident_Index, Vehicles$Accident_Index)]
Accidents$`Was_Vehicle_Left_Hand_Drive?`<- Vehicles$`Was_Vehicle_Left_Hand_Drive?`[ match(Accidents$Accident_Index, Vehicles$Accident_Index)]
Accidents$Journey_Purpose_of_Driver<- Vehicles$Journey_Purpose_of_Driver[ match(Accidents$Accident_Index, Vehicles$Accident_Index)]
Accidents$Sex_of_Driver<- Vehicles$Sex_of_Driver[ match(Accidents$Accident_Index, Vehicles$Accident_Index)]
Accidents$Age_of_Driver<- Vehicles$Age_of_Driver[ match(Accidents$Accident_Index, Vehicles$Accident_Index)]
Accidents$Age_Band_of_Driver<- Vehicles$Age_Band_of_Driver[ match(Accidents$Accident_Index, Vehicles$Accident_Index)]
Accidents$`Engine_Capacity_(CC)`<- Vehicles$`Engine_Capacity_(CC)`[ match(Accidents$Accident_Index, Vehicles$Accident_Index)]
Accidents$Propulsion_Code<- Vehicles$Propulsion_Code[ match(Accidents$Accident_Index, Vehicles$Accident_Index)]
Accidents$Age_of_Vehicle<- Vehicles$Age_of_Vehicle[ match(Accidents$Accident_Index, Vehicles$Accident_Index)]
Accidents$Driver_IMD_Decile<- Vehicles$Driver_IMD_Decile[ match(Accidents$Accident_Index, Vehicles$Accident_Index)]
Accidents$Driver_Home_Area_Type<- Vehicles$Driver_Home_Area_Type[ match(Accidents$Accident_Index, Vehicles$Accident_Index)]
#Accidents$Vehicle_Type<- Vehicles$Vehicle_Type[ match(Accidents$Accident_Index, Vehicles$Accident_Index)]
#### Data set 1 accident + casulties
#Accidents$Vehicle_Reference<- Casualties$Vehicle_Reference[ match(Accidents$Accident_Index, Casualties$Accident_Index)]
Vehicles$Casualty_Reference <- Casualties$Casualty_Reference[ match(Vehicles$Accident_Index, Casualties$Accident_Index) & match(Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
Vehicles$Casualty_Class<- Casualties$Casualty_Class[ match(Vehicles$Accident_Index, Casualties$Accident_Index)& match(Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
Vehicles$Sex_of_Casualty<- Casualties$Sex_of_Casualty[ match(Vehicles$Accident_Index, Casualties$Accident_Index)& match(Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
Vehicles$Age_of_Casualty<- Casualties$Age_of_Casualty[ match(Vehicles$Accident_Index, Casualties$Accident_Index)& match(Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
Vehicles$Age_Band_of_Casualty<- Casualties$Age_Band_of_Casualty[ match(Vehicles$Accident_Index, Casualties$Accident_Index)& match(Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
Vehicles$Casualty_Severity<- Casualties$Casualty_Severity[ match(Vehicles$Accident_Index, Casualties$Accident_Index)& match(Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
Vehicles$Pedestrian_Location<- Casualties$Pedestrian_Location[ match(Vehicles$Accident_Index, Casualties$Accident_Index)& match(Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
Vehicles$Pedestrian_Movement<- Casualties$Pedestrian_Movement[ match(Vehicles$Accident_Index, Casualties$Accident_Index)& match(Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
Vehicles$Car_Passenger<- Casualties$Car_Passenger[ match(Vehicles$Accident_Index, Casualties$Accident_Index)& match(Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
Vehicles$Bus_or_Coach_Passenger<- Casualties$Bus_or_Coach_Passenger[ match(Vehicles$Accident_Index, Casualties$Accident_Index)& match(Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
Vehicles$Pedestrian_Road_Maintenance_Worker<- Casualties$Pedestrian_Road_Maintenance_Worker[ match(Vehicles$Accident_Index, Casualties$Accident_Index)& match(Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
Vehicles$Casualty_Type<- Casualties$Casualty_Type[ match(Vehicles$Accident_Index, Casualties$Accident_Index)& match(Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
Vehicles$Casualty_Home_Area_Type<- Casualties$Casualty_Home_Area_Type[ match(Vehicles$Accident_Index, Casualties$Accident_Index)& match(Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
######
names(Accidents_Vehicles)
glimpse(Accidents_Vehicles)
Accidents_Vehicles$Casualty_Reference<- Casualties$Casualty_Reference [match(Accidents_Vehicles$Accident_Index,Casualties$Accident_Index)]# & match(Accidents_Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
Accidents_Vehicles$Casualty_Class<- Casualties$Casualty_Class[match(Accidents_Vehicles$Accident_Index, Casualties$Accident_Index) ]#& match(Accidents_Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
Accidents_Vehicles$Sex_of_Casualty<- Casualties$Sex_of_Casualty[ match(Accidents_Vehicles$Accident_Index, Casualties$Accident_Index) ]#& match(Accidents_Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
Accidents_Vehicles$Age_of_Casualty<- Casualties$Age_of_Casualty[ match(Accidents_Vehicles$Accident_Index, Casualties$Accident_Index) ]#& match(Accidents_Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
Accidents_Vehicles$Age_Band_of_Casualty<- Casualties$Age_Band_of_Casualty[ match(Accidents_Vehicles$Accident_Index, Casualties$Accident_Index)]# & match(Accidents_Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
Accidents_Vehicles$Casualty_Severity<- Casualties$Casualty_Severity[ match(Accidents_Vehicles$Accident_Index, Casualties$Accident_Index)]# & match(Accidents_Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
Accidents_Vehicles$Pedestrian_Location<- Casualties$Pedestrian_Location[ match(Accidents_Vehicles$Accident_Index, Casualties$Accident_Index)]# & match(Accidents_Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
Accidents_Vehicles$Pedestrian_Movement<- Casualties$Pedestrian_Movement[ match(Accidents_Vehicles$Accident_Index, Casualties$Accident_Index)]# & match(Accidents_Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
Accidents_Vehicles$Car_Passenger<- Casualties$Car_Passenger[ match(Accidents_Vehicles$Accident_Index, Casualties$Accident_Index)]# & match(Accidents_Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
Accidents_Vehicles$Bus_or_Coach_Passenger<- Casualties$Bus_or_Coach_Passenger[ match(Accidents_Vehicles$Accident_Index, Casualties$Accident_Index)]# & match(Accidents_Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
Accidents_Vehicles$Pedestrian_Road_Maintenance_Worker<- Casualties$Pedestrian_Road_Maintenance_Worker[ match(Accidents_Vehicles$Accident_Index, Casualties$Accident_Index)]# & match(Accidents_Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
Accidents_Vehicles$Casualty_Type<- Casualties$Casualty_Type[ match(Accidents_Vehicles$Accident_Index, Casualties$Accident_Index)]# & match(Accidents_Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]
Accidents_Vehicles$Casualty_Home_Area_Type<- Casualties$Casualty_Home_Area_Type[ match(Accidents_Vehicles$Accident_Index, Casualties$Accident_Index)]# & match(Accidents_Vehicles$Vehicle_Reference, Casualties$Vehicle_Reference)]

######
glimpse(Vehicles)
Accidents_Vehicles_Casualties <- Accidents_Vehicles
glimpse(Accidents)
tail(Accidents_Vehicles)
head(Vehicles)
#Accidents[Accidents$Accident_Index="197901A11AD14"]'#2014984137714 (9), 2014984138414 (19)
Casualties[which(Casualties$Accident_Index == "197901A11AD14"), ]
Vehicles[which(Vehicles$Accident_Index == "2014984137714"), ]
Accidents$Casualty_Type[Accidents$Accident_Index == "197901A11AD14"  ]

Accidents[which(Accidents$Accident_Index == "2014984137514"), ]
tail(Casualties)
#014984136214
Accidents_Casualties <- Accidents
