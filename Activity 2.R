library(dplyr)
library(lubridate)

streamH <- read.csv('/cloud/project/activtiy02/stream_gauge.csv')
siteInfo <- read.csv('/cloud/project/activtiy02/site_info.csv')

#ymd_hm --> year month date 24 hour time with hours and minutes
streamH$dateF <- ymd_hm(streamH$datetime)
streamH$year <- year(streamH$dateF)

peaceH <- streamH %>%
  filter(siteID == 2295637)

plot(peaceH$dateF, peaceH$gheight.ft, type="b", pch=19, xlab="Date", ylab="Stage Height (ft)")

# PART 1 CW (left table, right table, joining by)
floods <- full_join(streamH, siteInfo, by="siteID")

height.ave <- floods %>%
  group_by(names) %>%
  summarize(mean.height = mean(gheight.ft))

# PART 2 CW

floods$doy <- yday(floods$dateF)

height.day <- floods %>%
  group_by(names, doy) %>%
  summarize(mean.height = mean(gheight.ft))

max.cat <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft) %>%
  summarize(n.major = n())

max.flo <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= flood.ft) %>%
  summarise(first.date = min(dateF))

# HOMEWORK 1
#A

plot(floods$dateF[floods$siteID=="2256500"], floods$gheight.ft[floods$siteID=="2256500"], type="b", pch=19, xlab="Date", ylab="Stage Height (ft)", main="Fisheating Creek Stream Gauge")

#B

plot(floods$dateF[floods$siteID=="2295637"], floods$gheight.ft[floods$siteID=="2295637"], type="b", pch=19, xlab="Date", ylab="Stage Height (ft)", main="Peace River Stream Gauge")

#C

plot(floods$dateF[floods$siteID=="2322500"], floods$gheight.ft[floods$siteID=="2322500"], type="b", pch=19, xlab="Date", ylab="Stage Height (ft)", main="Santa Fe River Stream Gauge")

#D

plot(floods$dateF[floods$siteID=="2312000"], floods$gheight.ft[floods$siteID=="2312000"], type="b", pch=19, xlab="Date", ylab="Stage Height (ft)", main="Withlacoochee River Stream Gauge")


#HOMEWORK 2

action.first <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= action.ft) %>%
  summarise(first = min(dateF))

flood.first <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= flood.ft) %>%
  summarise(first = min(dateF))

moderate.first <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= moderate.ft) %>%
  summarise(first = min(dateF))

major.first <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft) %>%
  summarise(first = min(dateF))

# HOMEWORK 3

major.bigdiff <- floods %>%
  group_by(names,major.ft) %>%
  summarise(maxH = max(gheight.ft)) %>%
  summarise(diff = maxH-major.ft)
            