install.packages(c("dplyr","lubridate","ggplot2"))
library(dplyr)
library(lubridate)
library(ggplot2)

datCO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")


colnames(datCO2)[4] <- "CO2"
colnames(datCO2)

plot(datCO2$Year, datCO2$CO2, type="b",
     xlab = "Year", ylab= "CO2 emissions (tons)")



# In class activity
#prompt 1 
anomalies <- read.csv("/cloud/project/activity03/climate-change.csv")
Date <- c("1880-01-15")
ymd(Date)
anomalies$Date <- ymd(anomalies$Day)
NH <- anomalies[anomalies$Entity == "Northern Hemisphere",]
SH <- anomalies[anomalies$Entity == "Southern Hemisphere",]
plot(NH$Date,
     NH$temperature_anomaly, 
     type = "b", 
     pch = 19, 
     ylab = "Temperature Anomaly (degrees Celsius)", 
     xlab = "Date") 
points(SH$Date, 
       SH$temperature_anomaly, 
       type = "b", 
       pch = 19,
       col= "darkgoldenrod3")

hemispheres <- anomalies %>%
  filter(Entity== "Northern Hemisphere"|
           Entity== "Southern Hemisphere")
ggplot(hemispheres, aes(x=Date, y=temperature_anomaly, color=Entity))+
  geom_line()+
  labs(x="Date", y="Temperature Anomaly (degrees CelsiusP")+
  theme_classic()

#prompt 2

NA_CO <- datCO2 %>%
  filter(Entity== "United States"|
           Entity== "Mexico"| Entity== "Canada")
ggplot(NA_CO, aes(x=Year, y=CO2, color=Entity))+
   geom_point()+
   geom_line()+
  labs(x="Year", y="CO2 emissions in tons")+
  theme_classic()


#homework

#Question 1
KOR_CO <- datCO2 %>%
  filter(Entity== "North Korea"|
           Entity== "South Korea")%>%
filter(Year>1900)

ggplot(data = KOR_CO, 
       aes(x = Year, y=CO2, color=Entity ) )+ 
  geom_point()+ 
  geom_line()+
  labs(x="Year", y="Fossil fuel emissions (tons CO2)")+ # make axis labels
  theme_classic()

#Question 2
worldCO2 <- datCO2 %>%
  filter(Entity== "World")
ggplot(data = worldCO2, 
       aes(x = Year, y=CO2 ) )+
  geom_line()+ 
  labs(x="Year", y="US fossil fuel emissions (tons CO2)")
worldanomalies <- anomalies %>%
  filter(Entity== "World")
ggplot(data = worldanomalies, 
       aes(x = Date, y = temperature_anomaly) )+ 
  geom_line()+ 
  labs(x="Year", y="Global Temperature Anomalies (degrees Celsius)") 

#question 3
wind <- read.csv("/cloud/project/activity03/cumulative-installed-wind-energy-capacity-gigawatts.csv")
newgraph <- wind %>%
  filter(Entity== "United States"|
           Entity== "China"| Entity== "India"|Entity== "Germany")
ggplot(newgraph, aes(x=Year, y=Wind.energy.capacity...GW, color=Entity))+
  geom_point()+
  geom_line()+
  labs(x="Year", y="Wind Energy Capacity - GW")+
  theme_classic()
