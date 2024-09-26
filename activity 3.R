install.packages(c("dplyr","lubridate","ggplot2"))
library(dplyr)
library(lubridate)
library(ggplot2)

datCO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")


colnames(datCO2)[4] <- "CO2"
colnames(datCO2)

plot(datCO2$Year, datCO2$CO2, type="b",
     xlab = "Year", ylab= "CO2 emissions (tons)")


NA_CO <- datCO2 %>%
  filter(Entity== "United States"|
           Entity== "Mexico"| Entity== "Canada")
ggplot(NA_CO, aes(x=Year, y=CO2, color=Entity))+
  geom_line()+
  labs(x="Year", y="CO2 emissions")+
  theme_classic()

# In class activity
#prompt 1 
anomalies <- read.csv("/cloud/project/activity03/climate-change.csv")
Date <- c("1880-01-15")
ymd(exampleDate)
anomalies$Date <- ymd(anomalies$Day)
NH <- anomalies[anomalies$Entity == "Northern Hemisphere",]
SH <- anomalies[anomalies$Entity == "Southern Hemisphere",]
plot(NH$Date,
     NH$temperature_anomaly, 
     type = "b", 
     pch = 19, 
     ylab = "Temperature Anomaly", 
     xlab = "Date") 
points(SH$Date, 
       SH$temperature_anomaly, 
       type = "b", 
       pch = 19,
       col= "darkgoldenrod3")


