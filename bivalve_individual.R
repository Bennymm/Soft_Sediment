#Run "bivale_base.R" first

#function summarising individual bivalve densities.

abundance_seasonal <- function(Species){
  filter(bi.long.y.m.s.s, species == Species) %>%
    filter(year != "2018") %>%
    filter(abundance > 0) %>%
    ggplot() +
    aes(x = month, y = abundance, colour = site, group = site) +
    geom_point(size = 2.5, position = position_dodge(width = 0.25)) +
    geom_line( position = position_dodge(width = 0.25)) +
    facet_wrap(~year)+
    theme_bw() +
    geom_errorbar(aes(ymin = abundance - se,
                      ymax = abundance + se), 
                  width = 0,
                  size = 1,
                  position = position_dodge(width = 0.25)) +
    theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
          #legend.position  = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text             = element_text(size = 13, vjust = 0.1)) +
    labs(x = "Month", y = expression(Density ~ "(mean count/0.065m"^"2)"))
}
#execute above function
abundance_seasonal("plne")
