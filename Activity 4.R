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
for(i in 8:nrow(Jan22))
  rollAveTemp[i] <- mean(Jan22$AirTemp[(i-7):i])
}
Jan22$rollAveTemp <- rollAveTemp


