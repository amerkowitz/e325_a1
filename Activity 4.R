install.packages(c("lubridate", "dplyr","ggplot2"))
library(lubridate)
library(dplyr)
library(ggplot2)

weather <- read.csv("/cloud/project/activity04/campus_weather.csv",
                    na.strings="#N/A")
weather$dateF <- mdy_hm(weather$Date)

interval <- weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1]
interval
#set up time intervals in a vector of dates
timeInterval <- function(x){
  x[-length(x)] %--% x[-1]
}  

timeInterval(weather$dateF)

for(i in 1:6){
  print(paste("example", i))
}

seqEx <- c(1,4,6)
for (i in seqEx){
  print(paste("example", i))
}

chEx <- character()
for(i in 1:6){
  chEx[i] <- paste("example", i)
}

numEx <- numeric()
for(i in 1:6){
  numEx[i] <- 6*i
}


numEx2 <- numeric()
for(i in 2:6){
  numEx2[i] <- 6*i
}

#prompt 1

weather$month <- month(weather$dateF)
weather$year <- year(weather$dateF)

Jan22 <- weather %>%
  filter(month == 1 & year == 2022)

mean(Jan22$AirTemp[1:8])

rollAveTemp <- numeric()
for(i in 8:nrow(Jan22)){
  rollAveTemp[i] <- mean(Jan22$AirTemp[(i-7):i])
}
Jan22$rollAveTemp <- rollAveTemp

ggplot(data = Jan22, 
       aes(x = dateF, y=AirTemp))+
  geom_line()+ 
  labs(x="Date", y="Air Temperature") 

ggplot(data = Jan22, 
       aes(x = dateF, y=rollAveTemp))+
  geom_line()+ 
  labs(x="Date", y="Rolling Temperature Average") 

#prompt 2
MayJune <- weather %>%
  filter(month == 5 | month == 6 & year == 2021)
ggplot(data = MayJune, 
       aes(x = dateF, y=SolRad))+
  geom_line()+ 
  labs(x="Date", y="Solar Radiation") 
#prompt 3
weather$dateF[1] %--% weather$dateF[2]
int_length(weather$dateF[1] %--% weather$dateF[2])
intervals <- weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1]
interval_times <- int_length(intervals)
intervals[interval_times != 900]
timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
}
  timeCheck900(weather$dateF)
  
  
#homework

#question 1
weather$precip.QC <- ifelse(weather$AirTemp <= 0 | weather$XLevel >= 2  | weather$YLevel >= 2, 
                              
                              NA, 
                              weather$Precip) 
  which(is.na(weather$precip.QC))

#question 2
weather$voltagelow <- ifelse(weather$BatVolt < 8500, 
                             1,
                             0) 

#question 3
realistic <- function(x){
  conditions <- ifelse(weather$AirTemp < -50 | weather$AirTemp > 60 | weather$SolRad < 0 | weather$SolRad > 1750,
                       1, 
                       0)
  conditions == 1
}
realistic (weather$dateF)

#question 4
JanMarch <- weather %>%
  filter(month >= 1 & month <= 3  & year == 2021) 
ggplot(data = JanMarch, 
       aes(x = dateF, y=AirTemp))+
  geom_line()+ 
  labs(x="Date", y="Air Temperature") 

#question 5

MarchApril <- weather %>%
  filter(month >= 3 & month <= 4  & year == 2021) 
MarchApril$day <- yday(MarchApril$dateF)


MarchApril$fairenheit<-32+9*MarchApril$AirTemp/5

for (i in 2:nrow(MarchApril)){
  if(MarchApril$fairenheit[i] < 35){
    exclude_rows[i] <- TRUE
    exclude_rows[i-1] <- TRUE
  }
}

final_table <- MarchApril[!exclude_rows,]

