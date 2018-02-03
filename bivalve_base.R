#This block of code contains initial data imports, manipulations and reformatting. 
#These processes may include but are not limited to: merging, transposing, 
#summarising, new variable derivation, renaming and variable 'type' 
#coersion. This script must be run before other subsequent scripts.

#load packages
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
library("gridGraphics", lib.loc="~/R/win-library/3.4")
library("grid", lib.loc="~/R/win-library/3.4")

# set working directory
setwd("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files")

# read in data
bivalves      <- read.csv ("bivalves.csv")
# coerce some variables to type 'character'
bivalves$elevation <-
  as.character(bivalves$elevation)
bivalves$species <-
  as.character(bivalves$species)

#create y.m.d date variable for TropFishR (length frequency data)
bivalves$date <- ymd(paste(bivalves$year, bivalves$month, bivalves$day))

### count of species by quadtrat
bi.wide.1 <- bivalves %>%
  group_by(year, month, day, site, elevation, quad, species) %>%
  summarise(abundance = length(species)) %>%
  spread("species", "abundance")

# replace n/a with 0's
bi.wide.1[is.na(bi.wide.1)] <- 0
# create long data with null observations
bi.long <- gather(bi.wide.1, species, abundance, bama:umac)

#create character for month
bi.wide.1$month <- factor(bi.wide.1$month, 
                        levels = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                        labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
bi.long$month <- factor(bi.long$month, 
                        levels = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                        labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))


#pseudo-standardize effort: reduce 2016 values by a factor of 4; equal area 
bi.long$abundance_modified <- if_else(bi.long$year == 2016,
                                      bi.long$abundance / 4,
                                      if_else(bi.long$year == 2017 & bi.long$month == "Feb", 
                                              bi.long$abundance / 4, 
                                              bi.long$abundance),
                                      bi.long$abundance)

#wide data with effort correction; was bi.wde.core if other scripts aren't working
bi.wide <- bi.long %>%
  group_by(year, month, day, site, elevation, quad, species) %>%
  summarise(abundance = mean(abundance_modified)) %>%
  spread("species", "abundance")

#for mean seasonal length of individual species
bi.length.y.m.s <-
  group_by(bivalves, year, month, site, species)  %>%
  summarise(se = sd(length, na.rm = TRUE)/sqrt((length(length))), length = mean(length, na.rm = TRUE))
bi.length.y.m.s$month <- factor(bi.length.y.m.s$month, 
                        levels = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                        labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

#for seasonal abundance of individual species
bi.long.y.m.s.s <-bi.long %>%
  group_by(year, month, site, species) %>%
  summarise(se = sd(abundance_modified, na.rm = TRUE)/sqrt((length(species))), abundance = mean(abundance_modified, na.rm = TRUE))


                         