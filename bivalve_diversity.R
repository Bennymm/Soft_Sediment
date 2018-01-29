#Run "bivalve_base.R" first

#Summaries of species richness and diversity.

#new column; shannon H
bi.wide$shan <- diversity(
  bi.wide[(which(colnames(bi.wide) == "bama")) : 
          (which(colnames(bi.wide) == "umac"))],
  index = "shannon")

#new column; species abundance
bi.wide$abundance<- specnumber(
  bi.wide[(which(colnames(bi.wide) == "bama")) :
          (which(colnames(bi.wide) == "umac"))])

specaccum(bi.wide[
  (which(colnames(bi.wide) == "bama")) : 
  (which(colnames(bi.wide) == "umac"))]) %>%
  plot(ci.type="polygon", ci.col="lightyellow", xlab = "site visits", ylab = "species number")

#bivalve diversity(shannons and spp.richness) by site and month; all sites
#abundance
bi.wide %>% 
  group_by(year, month, site) %>%
  summarise_at(vars(shan:abundance), mean) %>%
ggplot() +
  aes(x = month, y = abundance, colour = site, group = site) +
  geom_point() +
  ylim(0, 8) +
  geom_line() +
  theme_bw() +
  facet_wrap(~year) +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1))+
  labs(x = "Month", y = expression("Richness" ~ "(average species count/site visit)"))

#shannons H
bi.wide %>% 
  group_by(year, month, site) %>%
  summarise_at(vars(shan:abundance), mean) %>%
  ggplot() +
  aes(x = month, y = shan, colour = site, group = site) +
  geom_point() +
  ylim(0, 3) +
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
bi.wide %>%
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
  labs(x = "tide height (m)", y = expression("Richness" ~ "(average species count/site visit)"))

#bivalve abundance by tide height and year: effort corrected
bi.long %>% 
  group_by(year, month, day, site, elevation, quad) %>%
  summarise(abundance = sum(abundance_modified, na.rm = TRUE)) %>%
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
  labs(x = "tide height (m)", y = expression("Bivalve abundance" ~ "(mean count/quadrat)"))

