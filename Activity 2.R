# in class activity
install.packages(c("dplyr","lubridate"))
library(dplyr)
library(lubridate)

streamH <- read.csv("/cloud/project/activtiy02/stream_gauge.csv")
siteinfo <- read.csv("/cloud/project/activtiy02/site_info.csv")

# parse our date
streamH$dateF <- ymd_hm(streamH$datetime,
                        tz="America/New_York")
year(streamH$dateF)

# Prompt 1, join site info to stream gauge height
floods <- full_join(streamH, siteinfo, by = "siteID")


peace <- floods %>%
  filter(siteID == 2295637)
# Prompt 2
streamH$dateF <- ymd_hm(streamH$datetime,
                        tz="America/New_York")
year(streamH$dateF)

example <- floods %>%
  filter(gheight.ft >= 10)
plot(peace$dateF, peace$gheight.ft, type="l")


max.ht <- floods  %>%
  group_by(names) %>%
  summarise(max_ht_ft=max(gheight.ft, na.rm=TRUE),
            mean_ft = mean(gheight.ft, na.rm=TRUE))


# Prompt 3

flood_date <- floods %>%
  filter(gheight.ft >= flood.ft) %>%
  group_by(names) %>%
  summarise(min_date=min(dateF))


# Homework

# Question 1
library(dplyr)
library(lubridate)
peaceH <- streamH[streamH$siteID == 2295637, ]
plot(peaceH$dateF, peaceH$gheight.ft, type="b", pch=19, xlab="Date",
     ylab = "Stage height (ft)")
fishH <- streamH[streamH$siteID == 2256500, ]
plot(fishH$dateF, fishH$gheight.ft, type="b", pch=19, xlab="Date",
     ylab = "Stage height (ft)")
santafeH <- streamH[streamH$siteID == 2322500, ]
plot(santafeH$dateF, santafeH$gheight.ft, type="b", pch=19, xlab="Date",
     ylab = "Stage height (ft)")
withH <- streamH[streamH$siteID == 2312000, ]
plot(withH$dateF, withH$gheight.ft, type="b", pch=19, xlab="Date",
     ylab = "Stage height (ft)")
# Question 2
action <- floods %>% 
  group_by(names) %>% 
  filter(gheight.ft >= action.ft, gheight.ft <= flood.ft) 
min(action$datetime)
action.date <- action %>% # 
  group_by(names) %>% 
  summarise(min.date = min(dateF))

floodstage <- floods %>% 
  group_by(names) %>% 
  filter(gheight.ft >= flood.ft, gheight.ft <= moderate.ft) 
min(action$datetime)
flood.date <- floodstage %>% # 
  group_by(names) %>% 
  summarise(min.date = min(dateF))

moderate <- floods %>% 
  group_by(names) %>% 
  filter(gheight.ft >= moderate.ft, gheight.ft <= major.ft) 
min(moderate$datetime)
moderate.date <- moderate %>% # 
  group_by(names) %>% 
  summarise(min.date = min(dateF))

major <- floods %>% 
  group_by(names) %>% 
  filter(gheight.ft >= major.ft) 
min(major$datetime)
major.date <- major %>% # 
  group_by(names) %>% 
  summarise(min.date = min(dateF))

#Question 3
floods$diff <- floods$gheight.ft-floods$major.ft
floods$gheight.ft-floods$major.ft
max(floods$diff)
which(floods$diff > 7.84)

