setwd("C:/Users/C_v/Desktop/UK accidents/R-Directory")  # note / instead of \ in windows
library(readr)
AccidentsData7904 <- read_csv("C:/Users/C_v/Desktop/UK accidents/R-Directory/Accidents7904preprocessed.csv")
AccidentsData0514 <- read_csv("C:/Users/C_v/Desktop/UK accidents/R-Directory/Accidents0514preprocessed.csv")

# trying to merge years 79 to 14
df1 <- AccidentsData7904
df2 <- AccidentsData0514
tail(df2)
names(df1)
tail(df3)
names(df2)
library(dplyr)
glimpse(df1)
sum(is.na(df1$Longitude))
sum(is.na(df1$Latitude))
sum(is.na(df2$Longitude))
sum(is.na(df2$Latitude))
sum(is.na(df3$Month))
sum(is.na(df3$Date_of_Month))
df3$Month[which(is.na(df3$Month))]<- -1
df3$Time[which(is.na(df3$Time))]<- -1
df3$Year[which(is.na(df3$Year))]<- -1
df3$Date_of_Month[which(is.na(df3$Date_of_Month))]<- -1
df1$Longitude[which(is.na(df1$Longitude))]<- 0
df1$Latitude[which(is.na(df1$Latitude))]<- 0
#identical(names(df1new[[11]]), names(df1new[[9]]) )
#merge by row
df3 <- rbind(df1,df2)
#update IDs
df3$X1 <- 1:nrow(df3)
colnames(df3)[1] <- "ID"
df3 <- subset(df3, select = -X1)
#output
df3
glimpse(df3)
library(ggplot2)
#ggplot(aes(x = df3$Time,), data = df3) + geom_line()
plot(density(Accidents$`1st_Road_Number`))

#Number_of_ <- df3$
#layout(matrix(1:1, ncol = 1))
hist(x,probability = TRUE, main = "Gaussian kernel",border = "gray")
lines(density(x, width = 12), lwd = 2)
#rug(x)
# Filled Density Plot
#d <- density(df3$)
plot(d, main="Kernel Density")
polygon(d, col="red", border="blue")
######## Compasrisons ###########
install.packages("sm")
library(sm)
tail(df3$Police_Force)
# create value labels 
accident.f <- factor(Accidents$Accident_Severity, levels= c(1,2,3),
                labels = c("1.Fatal", "2.Serious", "3.Slight")) 
roadtype.f <- factor(df3$Road_Type, levels= c(1,2,3,6,7,9,12,-1),
                     labels = c("1	Roundabout",
                                "2	One way street",
                                "3	Dual carriageway",
                                "6	Single carriageway",
                                "7	Slip road",
                                "9	Unknown",
                                "12	One way street/Slip road",
                                "-1	Data missing or out of range"
                     )) 
light.f <- factor(df3$Light_Conditions, levels= c(1,4,5,6,7,-1),
                     labels = c("1.Daylight",
"4.	Darkness - lights lit",
"5.	Darkness - lights unlit",
"6.	Darkness - no lighting",
"7.	Darkness - lighting unknown",
"-1.	Data missing or out of range"
))
weather.f <- factor(df3$Weather_Conditions, levels= c(1,2,3,4,5,6,7,8,9,-1),
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
road.f <- factor(df3$Road_Surface_Conditions, levels= c(1,2,3,4,5,6,7,-1),
                    labels = c("1.Dry",
                               "2.Wet or damp",
                               "3.Snow",
                               "4.Frost or ice",
                               "5.Flood over 3cm. deep",
                               "6.Oil or diesel",
                               "7.Mud",
                               "-1.Data missing"
                    ))
area.f <- factor(df3$Urban_or_Rural_Area, levels= c(1,2,3),
                 labels = c("1	Urban","2	Rural",
                            "3	Unallocated"
                 ))
day.f <- factor(df3$Day_of_Week, levels= c(1,2,3,4,5,6,7),
                 labels = c("1	Sunday",
"2	Monday",
"3	Tuesday",
"4	Wednesday",
"5	Thursday",
"6	Friday",
"7	Saturday"
))



tail(accident.f)
# plot densities 
sm.density.compare(Accidents$`Engine_Capacity_(CC)`,accident.f, xlab="Engine Capacity",xlim=c(0,2000))
sm.density.compare(df3$Police_Force,df3$Accident_Severity, xlab="Police Force")
sm.density.compare(df3$Month,df3$Accident_Severity, xlab="Month",xlim=c(1,12))
sm.density.compare(df3$Latitude,accident.f, xlab="Latitude",xlim=c(40,60))

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
library(readr)
Vehicles1979to2014preprocessed <- read_csv("C:/Users/C_v/Desktop/UK accidents/R-Directory/Vehicles1979to2014preprocessed.csv")
class(Vehicles1979to2014preprocessed)
dim(Vehicles1979to2014preprocessed)
names(Vehicles1979to2014preprocessed)
str(Vehicles1979to2014preprocessed)
#install.packages("dplyr")
library(dplyr)
glimpse(Vehicles1979to2014preprocessed)
summary(Vehicles1979to2014preprocessed)
df <- subset(Vehicles1979to2014preprocessed, select = -X1)
df <- subset(df, select = -X1)
glimpse(df)
tail(df)
names(df)
tail(df)
names(df)
library(dplyr)
glimpse(df)
summary(df)
sum(is.na(df))
plot(density(df$Age_of_Vehicle))
###########
age_of_driver <- df$Age_of_Driver
  #layout(matrix(1:1, ncol = 1))
hist(age_of_driver,probability = TRUE, main = "Gaussian kernel",border = "gray")
lines(density(age_of_driver, width = 12), lwd = 2)
#rug(x)
# Filled Density Plot
#d <- density(df3$)
#plot(d, main="Kernel Density")
polygon(density(age_of_driver, width = 12), col="red", border="blue")
# Subset the  data
#dat = df3[,c(1:2)]
########comparisons
install.packages("sm")
library(sm)
tail(df$Vehicle_Type)
# create value labels 
sex.f <- factor(df$Sex_of_Driver, levels= c(1,2,3,-1),
                     labels = c("1.Male",
                                "2.Female",
                                "3.Not known",
                                "-1.Data missing")) 

tail(df$Vehicle_Type)
# plot densities 
hist(df$Vehicle_Type,probability = TRUE, border = "gray",xlim=c(1,23))
sm.density.compare(df$Age_of_Vehicle,sex.f, xlab="Age of Vehicle",xlim=c(0,10))

#title(main="MPG Distribution by Car Cylinders")

# add legend via mouse click
colfill<-c(2:(2+length(levels(sex.f)))) 
legend(locator(1), levels(sex.f), fill=colfill)

############ Casualty ################
Casualty1979to2014preprocessed <- read_csv("C:/Users/C_v/Desktop/UK accidents/R-Directory/Casualty1979to2014preprocessed.csv")
class(Casualty1979to2014preprocessed)
dim(Casualty1979to2014preprocessed)
names(Casualty1979to2014preprocessed)
str(Casualty1979to2014preprocessed)
#install.packages("dplyr")
library(dplyr)
glimpse(Casualty1979to2014preprocessed)
summary(Casualty1979to2014preprocessed)
dfc <- subset(Casualty1979to2014preprocessed, select = -X1)
#df <- subset(df, select = -X1)
glimpse(dfc)

names(dfc)
tail(dfc)
names(dfc)
library(dplyr)
glimpse(dfc)
sum(is.na(dfc))
plot(density(dfc$Casualty_Home_Area_Type))#,xlim=c(90,99),ylim=c(0,0.001))
age_band_of_casualty <- dfc$Age_Band_of_Casualty
#layout(matrix(1:1, ncol = 1))
hist(age_band_of_casualty,probability = TRUE, main = "Gaussian kernel",border = "gray")
lines(density(age_band_of_casualty, width = 12), lwd = 2)
#rug(x)
# Filled Density Plot
#d <- density(df3$)
#plot(d, main="Kernel Density")
polygon(density(age_of_driver, width = 12), col="white", border="blue")
# Subset the  data
# require(graphics)
# 
# plot(density(c(-20, rep(0,98), 20)), xlim = c(-4, 4))  # IQR = 0
# 
# # The Old Faithful geyser data
# d <- density(df3$Longitude)
# d
# plot(d)
# 
# plot(d, type = "n")
# polygon(d, col = "wheat")
# 
# ## Missing values:
# x <- xx <- df3$Longitude
# x[i.out <- sample(length(x), 10)] <- NA
#dat = df3[,c(1:2)]
########comparisons
install.packages("sm")
library(sm)
tail(dfc$Casualty_Class)
# create value labels 
casualty.f <- factor(dfc$Casualty_Severity, levels= c(1,2,3),
                labels = c("1.Fatal",
                           "2.Serious",
                           "3.Slight"
                          )) 
class.f <- factor(dfc$Casualty_Class, levels= c(1,2,3),
                     labels = c("1.Driver or rider",
                                "2.Passenger",
                                "3.Pedestrian"
                     )) 


sex.f <- factor(dfc$Sex_of_Casualty, levels= c(1,2,-1),
                labels = c("1.Male",
                           "2.Female",
                           "-1.Data missing")) 
tail(class.f)
# plot densities 
#hist(df$Vehicle_Type,probability = TRUE, border = "gray",xlim=c(1,23))
sm.density.compare(dfc$Casualty_Home_Area_Type,casualty.f, xlab="Casualty_Home_Area_Type")#,xlim=c(90,99))
sm.density.compare(dfc$Age_Band_of_Casualty,class.f, xlab="Age Band_of_Casualty")#,xlim=c(10,70))#,xlim
sm.density.compare(dfc$Age_of_Casualty,sex.f, xlab="Age_of_Casualty",xlim=c(10,70))#,xlim

#title(main="MPG Distribution by Car Cylinders")

# add legend via mouse click
colfill<-c(2:(2+length(levels(sex.f)))) 
legend(locator(1), levels(sex.f), fill=colfill)

colfill<-c(2:(2+length(levels(class.f)))) 
legend(locator(1), levels(class.f), fill=colfill)

colfill<-c(2:(2+length(levels(casualty.f)))) 
legend(locator(1), levels(casualty.f), fill=colfill)
# doR <- density(x, bw = 0.15, na.rm = TRUE)
# lines(doR, col = "blue")
# points(xx[], rep(0.01, 10))
# 
# ## Weighted observations:
# fe <- sort(faithful$eruptions) # has quite a few non-unique values
# ## use 'counts / n' as weights:
# dw <- density(unique(fe), weights = table(fe)/length(fe), bw = d$bw)
# utils::str(dw) ## smaller n: only 126, but identical estimate:
# stopifnot(all.equal(d[1:3], dw[1:3]))
# 
# ## simulation from a density() fit:
# # a kernel density fit is an equally-weighted mixture.
# fit <- density(xx)
# N <- 1e6
# x.new <- rnorm(N, sample(xx, size = N, replace = TRUE), fit$bw)
# plot(fit)
# lines(density(x.new), col = "blue")
# 
# 
# (kernels <- eval(formals(density.default)$kernel))
# 
# ## show the kernels in the R parametrization
# plot (density(0, bw = 1), xlab = "",
#       main = "R's density() kernels with bw = 1")
# for(i in 2:length(kernels))
#   lines(density(0, bw = 1, kernel =  kernels[i]), col = i)
# legend(1.5,.4, legend = kernels, col = seq(kernels),
#        lty = 1, cex = .8, y.intersp = 1)
# 
# ## show the kernels in the S parametrization
# plot(density(0, from = -1.2, to = 1.2, width = 2, kernel = "gaussian"),
#      type = "l", ylim = c(0, 1), xlab = "",
#      main = "R's density() kernels with width = 1")
# for(i in 2:length(kernels))
#   lines(density(0, width = 2, kernel =  kernels[i]), col = i)
# legend(0.6, 1.0, legend = kernels, col = seq(kernels), lty = 1)
# 
# ##-------- Semi-advanced theoretic from here on -------------
# 
# 
# (RKs <- cbind(sapply(kernels,
#                      function(k) density(kernel = k, give.Rkern = TRUE))))
# 100*round(RKs["epanechnikov",]/RKs, 4) ## Efficiencies
# 
# bw <- bw.SJ(precip) ## sensible automatic choice
# plot(density(precip, bw = bw),
#      main = "same sd bandwidths, 7 different kernels")
# for(i in 2:length(kernels))
#   lines(density(precip, bw = bw, kernel = kernels[i]), col = i)
# 
# ## Bandwidth Adjustment for "Exactly Equivalent Kernels"
# h.f <- sapply(kernels, function(k)density(kernel = k, give.Rkern = TRUE))
# (h.f <- (h.f["gaussian"] / h.f)^ .2)
# ## -> 1, 1.01, .995, 1.007,... close to 1 => adjustment barely visible..
# 
# plot(density(precip, bw = bw),
#      main = "equivalent bandwidths, 7 different kernels")
# for(i in 2:length(kernels))
#   lines(density(precip, bw = bw, adjust = h.f[i], kernel = kernels[i]),
#         col = i)
# legend(55, 0.035, legend = kernels, col = seq(kernels), lty = 1)
