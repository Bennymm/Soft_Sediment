#Run "macro_base.R" first

#Summaries of macrophyte cover.

#seasonal mean cover function for any species with standard error
mean_cover_seasonal <- function(Species, Title) {
  filter(ma.cover.y.m.s, species == Species, cover > 0) %>%
    ggplot() +
    aes(x = month, y = cover, colour = factor(site), group = site) +
    geom_point(size = 2.5, position = position_dodge(width = 0.25)) +
    geom_line(position = position_dodge(width = 0.25)) +
    geom_errorbar(aes(ymin = cover - se,
                      ymax = cover + se), 
                  width = 0,
                  size = 1,
                  position = position_dodge(width = 0.25)) +
    theme_bw() +
    facet_wrap(~year) +
    theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
          #legend.position  = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text             = element_text(size = 13, vjust = 0.1)) +
    labs(x = "Month", y = expression("Mean Cover(%)"), colour = "site", title = Title)
}
#execute above function
mean_cover_seasonal("ulva", expression(italic(Ulva~lactuca)))
mean_cover_seasonal("ulin", expression(italic(Ulva~intestinalis)))
mean_cover_seasonal("gras", expression(italic(Gracilaria~spp.)))
mean_cover_seasonal("zost", expression(italic(Zostera~marina)))

# %cover boxplot; discard?
cover_seasonal_box <- function(Species) {
filter(ma.cover.y.m.s, species == Species) %>%
  ggplot() +
  aes(x = month, y = cover) +
  geom_boxplot() +
  theme_bw() +
    facet_wrap(~year) +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1))+
  labs(x = "month", y = expression("% cover"))
}
cover_seasonal_box("zost")

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
                width = 0,
                size = 1,
                position = position_dodge(width = 0.25)) +
  theme_bw() +
  facet_wrap(~year) +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Month", y = expression("Mean Total Cover(%)"), colour = "site", title = "All Species Combined")
ma_total_cover

#cover all species combined
filter(ma.cover.absolute) %>%
  ggplot() +
  aes(x = month, y = total.cover) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~year) +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1))+
  labs(x = "month", y = expression("% cover"))

