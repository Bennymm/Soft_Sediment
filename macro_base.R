#This block of code contains initial data imports, manipulations and reformatting. 
#These processes may include but are not limited to: merging, transposing, 
#summarising, new variable derivation, renaming and variable 'type' 
#conversions. This script must be run before other subsequent scripts.


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
library("TropFishR", lib.loc="~/R/win-liibrary/3.4")

macrophytes      <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/macrophytes.csv")

#create y.m.d date variable for TropFishR (length frequency data)
macrophytes$date <- ymd(paste(macrophytes$year, macrophytes$month, macrophytes$day))

#create character for month
macrophytes$month <- factor(macrophytes$month, 
                                levels = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                                labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

### count of species by quadtrat
ma.wide <- macrophytes %>%
  group_by(year, month, day, site, elevation, quad, species) %>%
  summarise(cover = mean(cover)) %>%
  spread("species", "cover")

# replace n/a with 0's
ma.wide[is.na(ma.wide)] <- 0
# create long data with null observations
ma.long <- gather(ma.wide, species, cover, acro:zost)


#for mean seasonal %cover of individual species
ma.cover.y.m.s <-
  group_by(ma.long, year, month, site, elevation, species)  %>%
  summarise(se = sd(cover, na.rm = TRUE)/sqrt(length(cover)), 
                    cover = mean(cover, na.rm = TRUE))


#for seasonal abundance and diversity of individual species
ma.long.y.m.s.s <-bi.long %>%
  group_by(year, month, site, species) %>%
  summarise(se = sd(abundance_modified, na.rm = TRUE)/sqrt((length(species))), 
                    abundance = mean(abundance_modified, na.rm = TRUE))

#sum %cover for all species
ma.cover.absolute <- macrophytes %>%
  group_by(year, month, site, elevation, quad) %>%
  summarise(total.cover = sum(cover))
# reduce cover > %100 to %100 
ma.cover.absolute$total.cover <-
if_else(ma.cover.absolute$total.cover > 100, 
        100, 
        ma.cover.absolute$total.cover)
#create mean total macrophyte coverage
ma.cover.total <- ma.cover.absolute %>%
group_by(year, month, site, elevation) %>%
  summarise(se = sd(total.cover, na.rm = TRUE)/sqrt((length(elevation))), 
            total.cover = mean(total.cover, na.rm = TRUE))




