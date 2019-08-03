#upto 2014 data all
AVC14 <- Accidents_Vehicles_Casualties
library(sm)
tail(AVC14$Speed_limit)
# speed limit is 30
AVC14s30 <- subset(AVC14,Speed_limit == 30)
# create value labels 
accident.f <- factor(AVC14s30$Accident_Severity, levels= c(1,2,3),
                     labels = c("1.Fatal", "2.Serious", "3.Slight")) 
roadtype.f <- factor(AVC14s30$Road_Type, levels= c(1,2,3,6,7,9,12,-1),
                     labels = c("1	Roundabout",
                                "2	One way street",
                                "3	Dual carriageway",
                                "6	Single carriageway",
                                "7	Slip road",
                                "9	Unknown",
                                "12	One way street/Slip road",
                                "-1	Data missing or out of range"
                     )) 
light.f <- factor(AVC14s30$Light_Conditions, levels= c(1,4,5,6,7,-1),
                  labels = c("1.Daylight",
                             "4.	Darkness - lights lit",
                             "5.	Darkness - lights unlit",
                             "6.	Darkness - no lighting",
                             "7.	Darkness - lighting unknown",
                             "-1.	Data missing or out of range"
                  ))
weather.f <- factor(AVC14s30$Weather_Conditions, levels= c(1,2,3,4,5,6,7,8,9,-1),
                    labels = c("1	Fine no high winds",
                               "2	Raining no high winds",
                               "3	Snowing no high winds",
                               "4	Fine + high winds",
                               "5	Raining + high winds",
                               "6	Snowing + high winds",
                               "7	Fog or mist",
                               "8	Other",
                               "9	Unknown",
                               "-1.	Data missing or out of range"
                    ))
road.f <- factor(AVC14s30$Road_Surface_Conditions, levels= c(1,2,3,4,5,6,7,-1),
                 labels = c("1.Dry",
                            "2.Wet or damp",
                            "3.Snow",
                            "4.Frost or ice",
                            "5.Flood over 3cm. deep",
                            "6.Oil or diesel",
                            "7.Mud",
                            "-1.Data missing"
                 ))
area.f <- factor(AVC14s30$Urban_or_Rural_Area, levels= c(1,2,3),
                 labels = c("1	Urban","2	Rural",
                            "3	Unallocated"
                 ))
day.f <- factor(AVC14s30$Day_of_Week, levels= c(1,2,3,4,5,6,7),
                labels = c("1	Sunday",
                           "2	Monday",
                           "3	Tuesday",
                           "4	Wednesday",
                           "5	Thursday",
                           "6	Friday",
                           "7	Saturday"
                ))



tail(AVC14s30)
sum(is.na(AVC14s30$Month))
AVC14s30$Month[which(is.na(AVC14s30$Month))]<- 0
plot(density(AVC14s30$Speed_limit))
# plot densities 
sm.density.compare(AVC14s30$Vehicle_Type,accident.f, xlab="Vehicle_Type")#,xlim=c(0,20))

#title(main="MPG Distribution by Car Cylinders")

# add legend via mouse click
colfill<-c(2:(2+length(levels(accident.f)))) 
legend(locator(1), levels(accident.f), fill=colfill)
colfill<-c(2:(2+length(levels(roadtype.f)))) 
legend(locator(1), levels(roadtype.f), fill=colfill)
colfill<-c(2:(2+length(levels(light.f)))) 
legend(locator(1), levels(light.f), fill=colfill)
colfill<-c(2:(2+length(levels(weather.f)))) 
legend(locator(1), levels(weather.f), fill=colfill)
colfill<-c(2:(2+length(levels(road.f)))) 
legend(locator(1), levels(road.f), fill=colfill)
colfill<-c(2:(2+length(levels(day.f)))) 
legend(locator(1), levels(day.f), fill=colfill)
########### Vehicles #############33
#upto 2014 data all
#AVC14 <- Accidents_Vehicles_Casualties
library(sm)
library(ggplot2)
tail(AVC14day6$Time)
# speed limit is 30
AVC14day6 <- subset(AVC14,Day_of_Week == 6)
ggplot(AVC14day6, aes(x = Time)) + geom_density() 
# create value labels 
accident.f <- factor(AVC14day6$Accident_Severity, levels= c(1,2,3),
                     labels = c("1.Fatal", "2.Serious", "3.Slight")) 
roadtype.f <- factor(AVC14day6$Road_Type, levels= c(1,2,3,6,7,9,12,-1),
                     labels = c("1	Roundabout",
                                "2	One way street",
                                "3	Dual carriageway",
                                "6	Single carriageway",
                                "7	Slip road",
                                "9	Unknown",
                                "12	One way street/Slip road",
                                "-1	Data missing or out of range"
                     )) 
light.f <- factor(AVC14day6$Light_Conditions, levels= c(1,4,5,6,7,-1),
                  labels = c("1.Daylight",
                             "4.	Darkness - lights lit",
                             "5.	Darkness - lights unlit",
                             "6.	Darkness - no lighting",
                             "7.	Darkness - lighting unknown",
                             "-1.	Data missing or out of range"
                  ))
weather.f <- factor(AVC14day6$Weather_Conditions, levels= c(1,2,3,4,5,6,7,8,9,-1),
                    labels = c("1	Fine no high winds",
                               "2	Raining no high winds",
                               "3	Snowing no high winds",
                               "4	Fine + high winds",
                               "5	Raining + high winds",
                               "6	Snowing + high winds",
                               "7	Fog or mist",
                               "8	Other",
                               "9	Unknown",
                               "-1.	Data missing or out of range"
                    ))
road.f <- factor(AVC14day6$Road_Surface_Conditions, levels= c(1,2,3,4,5,6,7,-1),
                 labels = c("1.Dry",
                            "2.Wet or damp",
                            "3.Snow",
                            "4.Frost or ice",
                            "5.Flood over 3cm. deep",
                            "6.Oil or diesel",
                            "7.Mud",
                            "-1.Data missing"
                 ))
area.f <- factor(AVC14day6$Urban_or_Rural_Area, levels= c(1,2,3),
                 labels = c("1	Urban","2	Rural",
                            "3	Unallocated"
                 ))
day.f <- factor(AVC14day6$Day_of_Week, levels= c(1,2,3,4,5,6,7),
                labels = c("1	Sunday",
                           "2	Monday",
                           "3	Tuesday",
                           "4	Wednesday",
                           "5	Thursday",
                           "6	Friday",
                           "7	Saturday"
                ))



tail(AVC14day6)
sum(is.na(AVC14day6$Time_In_Ms))
AVC14day6$Time_In_Ms[which(is.na(AVC14day6$Time_In_Ms))]<- 0
plot(density(AVC14day6$Time_In_Ms))
# plot densities 
sm.density.compare(AVC14day6$Sex_of_Driver,accident.f, xlab="Sex_of_Driver")#,xlim=c(0,90))

#title(main="MPG Distribution by Car Cylinders")

# add legend via mouse click
colfill<-c(2:(2+length(levels(accident.f)))) 
legend(locator(1), levels(accident.f), fill=colfill)
colfill<-c(2:(2+length(levels(roadtype.f)))) 
legend(locator(1), levels(roadtype.f), fill=colfill)
colfill<-c(2:(2+length(levels(light.f)))) 
legend(locator(1), levels(light.f), fill=colfill)
colfill<-c(2:(2+length(levels(weather.f)))) 
legend(locator(1), levels(weather.f), fill=colfill)
colfill<-c(2:(2+length(levels(road.f)))) 
legend(locator(1), levels(road.f), fill=colfill)
colfill<-c(2:(2+length(levels(day.f)))) 
legend(locator(1), levels(day.f), fill=colfill)
########### Vehicles #############33
