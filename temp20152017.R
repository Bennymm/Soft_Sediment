library("tidyverse", lib.loc="~/R/win-library/3.4")
library("dplyr", lib.loc="~/R/win-library/3.4")
library("vegan", lib.loc="~/R/win-library/3.4")
library("BiodiversityR", lib.loc="~/R/win-library/3.4")
library("knitr", lib.loc="~/R/win-library/3.4")
library("yaml", lib.loc="~/R/win-library/3.4")
library("markdown", lib.loc="~/R/win-library/3.4")
#library("MASS", lib.loc="~/R/win-library/3.4")
library("magrittr", lib.loc="~/R/win-library/3.4")
library("lubridate", lib.loc="~/R/win-library/3.4")
library("TropFishR", lib.loc="~/R/win-library/3.4")
library("data.table", lib.loc="~/R/win-library/3.4")

setwd("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature")

#import Dudas temperature data
a1 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/1510_1601hcg.csv")
a2 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/1510_1601piles.csv")
a3 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/1510_1601marten.csv")

b1 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/1601_1604hcg.csv")
b2 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/1601_1604piles.csv")
b3 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/1601_1604marten.csv")

c1 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/1604_1605hcg.csv")
c2 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/1604_1605piles.csv")
c3 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/1604_1605marten.csv")

d1 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/1605_1606hcg.csv")
d2 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/1605_1606piles.csv")
d3 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/1605_1606marten.csv")

e1 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/1606_1607hcg.csv")
e2 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/1606_1607piles.csv")
e3 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/1606_1607marten.csv")

f1 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/1607_1608hcg.csv")
f2 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/1607_1608piles.csv")
f3 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/1607_1608marten.csv")

#concatenate above Dudas temp
tempall <- do.call("rbind", list(a1, a2, a3, b1, b2, b3, c1, c2, c3, d1, d2, d3, e1, e2, e3, f1, f2, f3))

#import tide data; remove extras and add column names
adams2014 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/adams2014.csv")
adams2014 <- adams2014[-c(1:23),]
adams2014 <- adams2014[1:3]
colnames(adams2014)[1:3] <- c("date", "time", "tide")
adams2015 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/adams2015.csv")
adams2015 <- adams2015[-c(1:23),]
adams2015 <- adams2015[1:3]
colnames(adams2015)[1:3] <- c("date", "time", "tide")
adams2016 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/adams2016.csv")
adams2016 <- adams2016[-c(1:23),]
adams2016 <- adams2016[1:3]
colnames(adams2016)[1:3] <- c("date", "time", "tide")
adams2017 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/adams2017.csv")
adams2017 <- adams2017[-c(1:23),]
adams2017 <- adams2017[1:3]
colnames(adams2017)[1:3] <- c("date", "time", "tide")

#concatenate tides
adamstides <- do.call("rbind", list(adams2014, adams2015, adams2016, adams2017))
adamstides$date.time <- with(adamstides, ymd(date) + hm(time))
adamstides$date.time <- as.character.Date(adamstides$date.time)

#dope
tempall$date.time <- mdy_hms(tempall$Date.Hour)
tempall$date.time <- round.POSIXt(tempall$date.time, "hour")
tempall$date.time <- as.character.Date(tempall$date.time)

#merge tides and Dudas temp
merged <- merge(adamstides, tempall,  
                by = "date.time")

#remove variables; change column name(temp)
merged$date <- NULL
merged$time <- NULL
merged$Date.Hour <- NULL
merged$Date.Hour.Windows.format <- NULL
colnames(merged)[3] <- "temp"


#import Hakai temperature data
g1 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/g1.csv")
g2 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/g2.csv")
g3f <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/g3f.csv")
g4 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/g4.csv")
g5 <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/g5.csv")
g6f <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/g6f.csv")

#convert farenheight to celcius
g3f$temp = (g3f$tempf-32)*5/9
g3f$tempf <- NULL
g3 <- g3f

#convert farenheight to celcius
g6f$temp = (g6f$tempf-32)*5/9
g6f$tempf <- NULL
g6 <- g6f

#concatenate Hakai temp
tempss <- do.call("rbind", list(g1, g2, g3, g4, g5, g6))

tempss$date.time <- mdy_hm(tempss$date.time)
#coerce to date/time
tempss$date.time <- as.character.Date(tempss$date.time)

#working
mergedhak <- merge(tempss, adamstides,  
                   by = "date.time")

mergedhak$date <- NULL
mergedhak$time <- NULL

#concatenate Dudas and Hakai data
temptideSS <- rbind(merged, mergedhak)

#coerce tide height to numeric
temptideSS$tide <- as.numeric(as.character(temptideSS$tide))
temptideSS$year <- year(temptideSS$date.time)
temptideSS$month <- month(temptideSS$date.time)
temptideSS$day <- mday(temptideSS$date.time)
temptideSS$time <- hour(temptideSS$date.time)
temptideSS$month <- factor(temptideSS$month, 
                           levels = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                           labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))


#filter out drying times
temp <- filter(temptideSS, tide > 2.6)

#daily temperature temperature for observational data
temp.daily <-
  group_by(temp, year, month, day, site) %>%
  summarise(temp_daily = mean(temp))

#monthly temperature for graphics
temp.monthly <-
  group_by(temp, year, month, site) %>%
  summarise(se = sd(temp, na.rm = TRUE)/sqrt((length(month))), tempm = mean(temp))

#monthly mean temperature

temp_monthly <-
temp.monthly %>%  
  ggplot() +
  aes(x = month, y = tempm, colour = site, group = site) +
  geom_point(size = 2.5, position = position_dodge(width = 0.25)) +
  geom_line( position = position_dodge(width = 0.25)) +
  facet_wrap(~year)+
  theme_bw() +
  geom_errorbar(aes(ymin = tempm - se,
                    ymax = tempm + se), 
                width = 0,
                size = 1,
                position = position_dodge(width = 0.25)) +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Month", y = expression(paste("Mean Temperature (",degree,"C)")))
temp_monthly

#categorize tide depth
temp$depth <- cut(temp$tide, 
                  c(2.4, 3, 4, 6), 
                  c("0.5m", "1.5m", "2.5m"))

temp.monthly.depth <-
  group_by(temp, year, month, site, depth) %>%
  summarise(se = sd(temp, na.rm = TRUE)/sqrt((length(month))), tempm = mean(temp))

#temperature at depth: Marten
temp_Marten <-
temp.monthly.depth %>%  
  filter(site == "marten") %>%
  ggplot() +
  aes(x = month, y = tempm, colour = depth, group = depth) +
  geom_point(size = 2.5, position = position_dodge(width = 0.25)) +
  geom_line( position = position_dodge(width = 0.25)) +
  facet_wrap(~year)+
  theme_bw() +
  geom_errorbar(aes(ymin = tempm - se,
                    ymax = tempm + se), 
                width = 0,
                size = 1,
                position = position_dodge(width = 0.25)) +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Month", y = expression(paste("Mean Temperature (",degree,"C)")))
temp_Marten

#temperature at depth: Pruth Bay
temp_Pruth <-
temp.monthly.depth %>%  
  filter(site == "hcg") %>%
  ggplot() +
  aes(x = month, y = tempm, colour = depth, group = depth) +
  geom_point(size = 2.5, position = position_dodge(width = 0.25)) +
  geom_line( position = position_dodge(width = 0.25)) +
  facet_wrap(~year)+
  theme_bw() +
  geom_errorbar(aes(ymin = tempm - se,
                    ymax = tempm + se), 
                width = 0,
                size = 1,
                position = position_dodge(width = 0.25)) +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Month", y = expression(paste("Mean Temperature (",degree,"C)")))
temp_Pruth

#temperature at depth: Piles
temp_Piles <-
temp.monthly.depth %>%  
  filter(site == "piles") %>%
  ggplot() +
  aes(x = month, y = tempm, colour = depth, group = depth) +
  geom_point(size = 2.5, position = position_dodge(width = 0.25)) +
  geom_line( position = position_dodge(width = 0.25)) +
  facet_wrap(~year)+
  theme_bw() +
  geom_errorbar(aes(ymin = tempm - se,
                    ymax = tempm + se), 
                width = 0,
                size = 1,
                position = position_dodge(width = 0.25)) +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Month", y = expression(paste("Mean Temperature (",degree,"C)")))
temp_Piles