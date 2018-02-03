#This block of code contains initial data imports, manipulations and reformatting. 
#These processes may include but are not limited to: merging, transposing, 
#summarising, new variable derivation, renaming and variable 'type' 
#coersion. This script must be run before other subsequent scripts.

#library("tidyverse", lib.loc="~/R/win-library/3.4")
#library("dplyr", lib.loc="~/R/win-library/3.4")
#library("vegan", lib.loc="~/R/win-library/3.4")
#library("BiodiversityR", lib.loc="~/R/win-library/3.4")
#library("knitr", lib.loc="~/R/win-library/3.4")
#library("yaml", lib.loc="~/R/win-library/3.4")
#library("markdown", lib.loc="~/R/win-library/3.4")
#library("MASS", lib.loc="~/R/win-library/3.4")
#library("magrittr", lib.loc="~/R/win-library/3.4")
#library("lubridate", lib.loc="~/R/win-library/3.4")
#library("TropFishR", lib.loc="~/R/win-library/3.4")
#library("gridGraphics", lib.loc="~/R/win-library/3.4")
#library("grid", lib.loc="~/R/win-library/3.4")

inverts      <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/inverts.csv")

#create y.m.d date variable for TropFishR (length frequency data)
inverts$date <- ymd(paste(inverts$year, inverts$month, inverts$day))

#create character for month
inverts$month <- factor(inverts$month, 
                            levels = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                            labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

# coerce some variables to type 'character'
inverts$elevation <-
  as.character(inverts$elevation)
inverts$species <-
  as.character(inverts$species)


### count of species by quadtrat
in.wide <- inverts %>%
  group_by(year, month, day, site, elevation, quad, species) %>%
  summarise(count = mean(count)) %>%
  spread("species", "count")

# replace n/a with 0's
in.wide[is.na(in.wide)] <- 0
# create long data with null observations
in.long <- gather(in.wide, species, count, amph:unwo)


#for mean seasonal abundance of individual species
in.count.y.m.s <-
  group_by(in.long, year, month, site, elevation, species)  %>%
  summarise(se = sd(count, na.rm = TRUE)/sqrt(length(count)), 
            count = mean(count, na.rm = TRUE))


#for seasonal abundance and diversity of individual species
in.long.y.m.s.s <-in.long %>%
  group_by(year, month, site, species) %>%
  summarise(se = sd(count, na.rm = TRUE)/sqrt((length(species))), 
            abundance = mean(count, na.rm = TRUE))

