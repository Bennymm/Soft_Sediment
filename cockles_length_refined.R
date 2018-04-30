#This block of code contains import, cleaning and summaries of cockle 
#annuli measurements from 2017 on Calvert Island. In the 2017 field season
#each cockle annuli was measured. This block of code describes growth of cockles
#over the past 4-5 years by looking at cohort growth across years. Data contains length 
#measurements(l1-l10) of each annuli and derived growth measurements(g1-g10). The numeric
#values within l1-l10 and g1-g10 refer to the age of the annuli.

library("tidyverse", lib.loc="~/R/win-library/3.4")
library("dplyr", lib.loc="~/R/win-library/3.4")
library("vegan", lib.loc="~/R/win-library/3.4")
library("BiodiversityR", lib.loc="~/R/win-library/3.4")
library("knitr", lib.loc="~/R/win-library/3.4")
library("yaml", lib.loc="~/R/win-library/3.4")
library("markdown", lib.loc="~/R/win-library/3.4")
library("MASS", lib.loc="~/R/win-library/3.4")
library("magrittr", lib.loc="~/R/win-library/3.4")
library("lubridate", lib.loc="~/R/win-library/3.4")
library("TropFishR", lib.loc="~/R/win-library/3.4")

# make fabsdivlength name of and create working file 
setwd("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files")

cockles      <- read.csv ("cockles_outliers.csv")
# coerce some variables to type 'character'
cockles$elevation <-
  as.character(cockles$elevation)
cockles$species <-
  as.character(cockles$species)
cockles$age <-
  as.numeric(cockles$age)
#numeric month to text
cockles$month <- factor(cockles$month, 
                        levels = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                        labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
cockles$age2 <- (cockles$age - 1)
cockles$age3 <- (cockles$age2 - 1)
cockles$age4 <- (cockles$age3 - 1)
cockles$age5 <- (cockles$age4 - 1)
cockles$age6 <- (cockles$age5 - 1)

cockles$age <- factor(cockles$age, 
                       levels = c(1,2,3,4,5,6,7,8,9,10), 
                       labels = c(2017,2016,2015,2014,2013,2012,2011,2010,2009,2008))
cockles$age2 <- factor(cockles$age2, 
                        levels = c(1,2,3,4,5,6,7,8,9,10), 
                        labels = c(2017,2016,2015,2014,2013,2012,2011,2010,2009,2008))
cockles$age3 <- factor(cockles$age3, 
                       levels = c(1,2,3,4,5,6,7,8,9,10), 
                       labels = c(2017,2016,2015,2014,2013,2012,2011,2010,2009,2008))
cockles$age4 <- factor(cockles$age4, 
                       levels = c(1,2,3,4,5,6,7,8,9,10), 
                       labels = c(2017,2016,2015,2014,2013,2012,2011,2010,2009,2008))
cockles$age5 <- factor(cockles$age5, 
                       levels = c(1,2,3,4,5,6,7,8,9,10), 
                       labels = c(2017,2016,2015,2014,2013,2012,2011,2010,2009,2008))

# na's <- 0
cockles[is.na(cockles)] <- 0

#function for n values in ggplot
give.n <- function(x){
  return(c(y = median(x)*1.04, label = length(x))) 
}

#cockle growth at different sites, re-run with other cohorts
  filter(cockles ,g3 != 0) %>%
  ggplot() +
  aes(x = site, y = g2) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1))+
  labs(x = "site", y = expression("growth"), title = "1st year growth")

  
#cockle growth at different ages, re-run with different cohorts
#1st year growth
group_by(cockles, site)%>%
  ggplot() +
  aes(x = age, y = g1) +
  geom_boxplot() +
  stat_summary(fun.data = give.n, geom = "text", position = position_dodge(width = 0.75)) +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1))+
  labs(x = "year", y = expression("growth"), title = "1st year growth")

#2nd year growth
group_by(cockles, site)%>%
  filter(g3 != 0) %>%
  ggplot() +
  aes(x = age2, y = g2, fill = site) +
  geom_boxplot() +
  stat_summary(fun.data = give.n, geom = "text", position = position_dodge(width = 0.75)) +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1))+
  labs(x = "year", y = expression("growth"), title = "2nd year growth")

#3rd year growth
group_by(cockles, site)%>%
  filter(g4 != 0) %>%
  ggplot() +
  aes(x = age3, y = g3, fill = site) +
  geom_boxplot() +
  stat_summary(fun.data = give.n, geom = "text") +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1))+
  labs(x = "year", y = expression("growth"), title = "3rd year growth")

#4th year growth
group_by(cockles, site)%>%
  filter(g5 != 0) %>%
  ggplot() +
  aes(x = age4, y = g4, fill = site) +
  geom_boxplot() +
  stat_summary(fun.data = give.n, geom = "text") +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1))+
  labs(x = "year", y = expression("growth"), title = "4th year growth")


#1st year growth
cockles1styear <-cockles %>%
  group_by(age) %>%
  filter(g2 != 0) %>%
  summarise(se = sd(g1, na.rm = TRUE)/sqrt((length(age))), 
            growth = mean(g1, na.rm = TRUE), n = length(age))

cockles1styear$growthyear <- 1
#2nd year growth
cockles2ndyear <-cockles %>%
  group_by(age) %>%
  filter(g3 != 0) %>%
  summarise(se = sd(g2, na.rm = TRUE)/sqrt((length(age))), 
            growth = mean(g2, na.rm = TRUE), n = length(age))

cockles2ndyear$growthyear <- 2
#3rd year growth
cockles3rdyear <-cockles %>%
  group_by(age) %>%
  filter(g4 != 0) %>%
  summarise(se = sd(g3, na.rm = TRUE)/sqrt((length(age))), 
            growth = mean(g3, na.rm = TRUE), n = length(age))

cockles3rdyear$growthyear <- 3
#4th year growth
cockles4thyear <-cockles %>%
  group_by(age) %>%
  filter(g5 != 0) %>%
  summarise(se = sd(g4, na.rm = TRUE)/sqrt((length(age))), 
            growth = mean(g4, na.rm = TRUE), n = length(age))

cockles4thyear$growthyear <- 4


growthsummary <- rbind(cockles1styear,cockles2ndyear,cockles3rdyear,cockles4thyear)

growthsummary$growthyear <- factor(growthsummary$growthyear, 
                     levels = c(1,2,3,4), 
                     labels = c("first", "second", "third", "fourth"))

growthsummary%>%
  filter(growth != 0) %>%
ggplot() +
  aes(x = age, y = growth, colour = growthyear, group = growthyear) +
  geom_point(position = position_dodge(width = 0.25)) +
  geom_text(aes(label = n),hjust=2, vjust=0) +
  geom_errorbar(aes(ymin = growth - se,
                    ymax = growth + se), 
                width = 0,
                size = 1,
                position = position_dodge(width = 0.25)) +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "year", y = expression("growth(mm)"), title = "cockle growth", colour = "Growth Year") 

str(fit)

#t-test and anova by year
firstyear2014 <-
  filter(cockles, age == 2014)
firstyearother <-
  filter(cockles, age == 2016 | age == 2015 | age == 2013 | age == 2012 | age == 2010)

t.test(firstyear2014$g1, firstyearother$g1)

fit <- 
cockles %>%
  filter(g2 != 0)

fit<-
  aov(fit$g1 ~ fit$age)
summary(fit)
TukeyHSD(fit)
str(fit)
plot(fit)

#t-test and anova by site
fit <- 
  cockles %>%
  filter(g2 != 0)
fit$site <- as.character(fit$site)
fit<-
  aov(fit$g1 ~ fit$site)
summary(fit)
plot(fit)
TukeyHSD(fit)

#Blocking
fit <- 
  cockles %>%
  filter(g2 != 0)

fit<-
  aov(fit$g1 ~ fit$site * fit$age)
summary(fit)
TukeyHSD(fit)
plot(fit)


#above analysis with indices rather than growth

r1 <- cockles$g1/((with(cockles, mean(g1[g1 > 0])))*2)
r2 <- cockles$g2/((with(cockles, mean(g2[g2 > 0])))*2)
r3 <- cockles$g3/((with(cockles, mean(g3[g3 > 0])))*2)
r4 <- cockles$g4/((with(cockles, mean(g4[g4 > 0])))*2)
r5 <- cockles$g5/((with(cockles, mean(g5[g5 > 0])))*2)


cockles_relgrowth <- 
  gather(cockles, agex, growthx, r1:r5)

cockles_relgrowth <- filter(cockles_relgrowth, growthx != 0 & growthx < 1)

ggplot(cockles_relgrowth) +
  aes(x = site, y = growthx) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1))+
  labs(x = "site", y = expression("growth"), title = "relative growth by site")


fit <- 
  cockles_relgrowth %>%
  filter(growthx != 0)

fit<-
  aov(fit$growthx ~ fit$site)
summary(fit)
TukeyHSD(fit)
str(fit)
plot(fit)


shapiro.test(cockles_relgrowth$growthx)
hist(cockles_relgrowth$growthx)

ks.test(cockles_relgrowth$growthx, pnorm)
