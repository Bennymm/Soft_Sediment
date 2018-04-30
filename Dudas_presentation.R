#Run "bivalve_base.R" first
#new column; shannon H
bi.wide$shan <- diversity(
  bi.wide[(which(colnames(bi.wide) == "bama")) : 
            (which(colnames(bi.wide) == "umac"))],
  index = "shannon")

#new column; species abundance
bi.wide$abundance<- specnumber(
  bi.wide[(which(colnames(bi.wide) == "bama")) :
            (which(colnames(bi.wide) == "umac"))])

#new column; shannon H
in.wide$shan <- diversity(
  in.wide[(which(colnames(in.wide) == "amph")) : 
            (which(colnames(in.wide) == "unwo"))],
  index = "shannon")

#new column; species abundance
in.wide$abundance<- specnumber(
  in.wide[(which(colnames(in.wide) == "amph")) :
            (which(colnames(in.wide) == "unwo"))])



#seasonal mean length function for any species with standard error
mean_length_seasonal <- function(Species, Year) {
  filter(bi.length.y.m.s, species == Species, year == Year, site != "hcg") %>%
    ggplot() +
    aes(x = month, y = length, colour = factor(site), group = site) +
    geom_point(size = 2.5, position = position_dodge(width = 0.25)) +
    geom_line(position = position_dodge(width = 0.25), size = 1) +
    geom_errorbar(aes(ymin = length - se,
                      ymax = length + se), 
                  width = 0.25,
                  size = 1,
                  position = position_dodge(width = 0.25)) +
    theme_bw() +
    theme(axis.text.x      = element_text(size = 14, angle = 45, hjust = 1),
          #legend.position  = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text             = element_text(size = 16, vjust = 0.1)) +
    labs(x = "Month", y = expression("Mean Length(mm)"), colour = "site", title =
           "Juvenile Macoma Length by Month (n = 2482)")
}
png(filename="C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/dudas/juv_macoma_growth.png")
mean_length_seasonal("umac",2017)
dev.off()


#richness; requires bivalve biodiversity

bivalve_seasonal_species_richness <- 
  bi.wide %>% 
  group_by(year, month, site) %>%
  summarise_at(vars(shan:abundance), mean) %>%
  ggplot() +
  aes(x = month, y = abundance, colour = site, group = site) +
  geom_point() +
  ylim(0, 8) +
  geom_line(size = 1) +
  theme_bw() +
  facet_wrap(~year) +
  theme(axis.text.x      = element_text(size = 14, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 16, vjust = 0.1))+
  labs(x = "Month", y = expression("Richness" ~ "(average species count/site visit)"), title = 
         "Bivalve Species Richness")

png(filename="C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/dudas/bivalve_richness.png", width = 800, height = 480)
bivalve_seasonal_species_richness
dev.off()

#bivalve diversity(shannons and spp.richness) by site and month; all sites
#richness
invert_seasonal_species_richness <-
  in.wide %>% 
  group_by(year, month, site) %>%
  summarise_at(vars(shan:abundance), mean) %>%
  ggplot() +
  aes(x = month, y = abundance, colour = site, group = site) +
  geom_point() +
  geom_line(size = 1) +
  theme_bw() +
  facet_wrap(~year) +
  theme(axis.text.x      = element_text(size = 14, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 16, vjust = 0.1))+
  labs(x = "Month", y = expression("Richness" ~ "(species count/site visit)"), title = 
         "Invertebrate Richness")

png(filename="C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/dudas/invert_richness.png", width = 800, height = 480)
invert_seasonal_species_richness
dev.off()


#total cover
#seasonal %cover; all species combined; standard error
ma_total_cover <- 
  filter(ma.cover.total) %>%
  ggplot() +
  aes(x = month, y = total.cover, colour = factor(site), group = site) +
  geom_point(size = 2.5, position = position_dodge(width = 0.25)) +
  geom_line(position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(ymin = total.cover - se,
                    ymax = total.cover + se), 
                width = 0.25,
                size = 1,
                position = position_dodge(width = 0.25)) +
  theme_bw() +
  facet_wrap(~year) +
  theme(axis.text.x      = element_text(size = 14, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 16, vjust = 0.1)) +
  labs(x = "Month", y = expression("Mean Total Cover(%)"), colour = "Site", title = 
         "Seasonal Macrophyte Cover")

png(filename="C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/dudas/macrophyte_cover.png", width = 800, height = 480)
ma_total_cover
dev.off()

