#Run "invert_base.R" first

#Summaries of species richness and diversity.

#new column; shannon H
in.wide$shan <- diversity(
  in.wide[(which(colnames(in.wide) == "amph")) : 
            (which(colnames(in.wide) == "unwo"))],
  index = "shannon")

#new column; species abundance
in.wide$abundance<- specnumber(
  in.wide[(which(colnames(in.wide) == "amph")) :
            (which(colnames(in.wide) == "unwo"))])

#species accumulation curve; requires GridGraphics and grid packages
specaccum(in.wide[
  (which(colnames(in.wide) == "amph")) : 
    (which(colnames(in.wide) == "unwo"))]) %>%
  plot(ci.type="polygon", ci.col="lightyellow", xlab = "quadrats", ylab = "species number")
invert_species_accumulation <- recordPlot()

#bivalve diversity(shannons and spp.richness) by site and month; all sites
#richness
invert_seasonal_species_richness <-
in.wide %>% 
  group_by(year, month, site) %>%
  summarise_at(vars(shan:abundance), mean) %>%
  ggplot() +
  aes(x = month, y = abundance, colour = site, group = site) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~year) +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1))+
  labs(x = "Month", y = expression("Richness" ~ "(species count/site visit)"))

#shannons H
invert_seasonal_shannons <-
in.wide %>% 
  group_by(year, month, site) %>%
  summarise_at(vars(shan:abundance), mean) %>%
  ggplot() +
  aes(x = month, y = shan, colour = site, group = site) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~year) +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1))+
  labs(x = "Month", y = expression("Shannons H"))

#bivalve species abundance by tide height and year
invert_tidal_richness <-
in.wide %>%
  group_by(year, elevation) %>%
  ggplot() +
  aes(x = elevation, y = abundance, group= elevation) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~year) +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1))+
  labs(x = "tide height (m)", y = expression("Benthic Invertebrate Richness"))

invert_tidal_richness

#bivalve abundance by tide height and year
invert_tidal_abundance <-
in.long %>% 
  group_by(year, month, day, site, elevation, quad) %>%
  summarise(abundance = sum(count, na.rm = TRUE)) %>%
  group_by(year, elevation) %>%
  ggplot() +
  aes(x = elevation, y = abundance, group= elevation) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~year) +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1))+
  labs(x = "tide height (m)", y = expression("Benthic Invertebrate Abundance" ~ "(mean count/quadrat)"))

invert_tidal_abundance
#seasonal mean count function for any species with standard error
invert_abundance_seasonal <- function(Species, Title) {
  filter(in.count.y.m.s, species == Species, year == 2017, count > 0) %>%
    ggplot() +
    aes(x = month, y = count, colour = factor(site), group = site) +
    geom_point(size = 2.5, position = position_dodge(width = 0.25)) +
    geom_line(position = position_dodge(width = 0.25)) +
    geom_errorbar(aes(ymin = count - se,
                      ymax = count + se), 
                  width = 0,
                  size = 1,
                  position = position_dodge(width = 0.25)) +
    theme_bw() +
    facet_wrap(~elevation) +
    theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
          #legend.position  = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text             = element_text(size = 13, vjust = 0.1)) +
    labs(x = "Month", y = expression("Mean count/quadrat"), colour = "site", title = Title)
}

#execute above function
invert_abundance_seasonal("amph", "Amphipoda")
invert_abundance_seasonal("flat", "Phylum: Platyhelminthes")
invert_abundance_seasonal("bagl", expression(italic(Balanus~glandula)))
invert_abundance_seasonal("limp", "Limpets")
invert_abundance_seasonal("myti", expression(italic(Mytilus~spp.)))
invert_abundance_seasonal("poly", "Polychaeta")
invert_abundance_seasonal("puve", "Nutricola~tantilla")
invert_abundance_seasonal("rowo", "Nematoda")
invert_abundance_seasonal("scut", expression(italic(Littorina~scutulata)))
invert_abundance_seasonal("thbi", expression(italic(Bittium~eschrichtii)))
invert_abundance_seasonal("lacu", expression(italic(Lacuna~spp)))

