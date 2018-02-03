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
#library("data.table", lib.loc="~/R/win-library/3.4")

setwd("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files")

chl <- read.csv ("chlorophyll.csv")

chl$month <- factor(chl$month, 
                        levels = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                        labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

chla <-
chl %>%
  group_by(year, month) %>%
  ggplot() +
    aes(x = month, y = chla.nmol.g) +
    geom_boxplot() +
    theme_bw() +
    facet_wrap(~year) +
    theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
          legend.position  = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text             = element_text(size = 13, vjust = 0.1))+
    labs(x = "month", y = expression("Chlorophyll A" ~ "(nmol/gram)"), title = "Chlorophyll A concentrations")

chla

#pdf("chla", height = 6)
#chla
#dev.off()

chl.site <-
  group_by(chl, year, month, site)  %>%
summarise(se = sd(chla.nmol.g, na.rm = TRUE)/sqrt((length(chla.nmol.g))), chla = mean(chla.nmol.g, na.rm = TRUE))

chla_bysite <- 
chl.site %>% 
    ggplot() +
    aes(x = month, y =chla, colour = site, group = site) +
    geom_point(size = 2.5, position = position_dodge(width = 0.25)) +
    geom_line( position = position_dodge(width = 0.25)) +
    facet_wrap(~year)+
    theme_bw() +
    geom_errorbar(aes(ymin = chla - se,
                      ymax = chla + se), 
                  width = 0,
                  size = 1,
                  position = position_dodge(width = 0.25)) +
    theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
          #legend.position  = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text             = element_text(size = 13, vjust = 0.1)) +
    labs(x = "Month", y = expression("Chlorophyll A" ~ "(nmol/gram)"))

chla_bysite

    #pdf("chla_bysite", width = 12, height = 6)
    #chla_bysite
    #dev.off()

chla_site_box <-  
  chl %>%
  group_by(year, month) %>%
  ggplot() +
  aes(x = month, y = chla.nmol.g, fill = site) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~year) +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1))+
  labs(x = "month", y = expression("Chlorophyll A" ~ "(nmol/gram)"), title = "Chlorophyll A concentrations")

chla_site_box

#pdf("chla_site_box", width = 11, height = 6)
#chla_site_box
#dev.off()
