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

cockles      <- read.csv ("cockles.csv")
# coerce some variables to type 'character'
cockles$elevation <-
  as.character(cockles$elevation)
cockles$species <-
  as.character(cockles$species)
cockles$age <-
  as.character(cockles$age)
#numeric month to text
cockles$month <- factor(cockles$month, 
                        levels = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                        labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
cockles$age <- factor(cockles$age, 
                        levels = c(1,2,3,4,5,6,7,8,9,10), 
                        labels = c(2017,2016,2015,2014,2013,2012,2011,2010,2009,2008))

# na's <- 0
cockles[is.na(cockles)] <- 0

#cockle growth at different sites, re-run with other cohorts
  filter(cockles ,g2 != 0) %>%
  ggplot() +
  aes(x = site, y = g2) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1))+
  labs(x = "site", y = expression("growth"), title = "2nd year growth")

#cockle growth at different ages, re-run with different cohorts
group_by(cockles, site)%>%
  filter(g2 != 0) %>%
  ggplot() +
  aes(x = age, y = g2) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1))+
  labs(x = "year", y = expression("growth"), title = "2nd year growth")
