#Run "bivale_base.R" first

#template for bivalve length graphics
#change species and length filter

#length frequency graphics function by year and month
lfq_year_month <- function(Species1, Species2) {
  plotlfq <-dl.temp <- filter(bivalves, species == Species1 | species == Species2)
  dl.lfq  <- lfqCreate(dl.temp, Lname = "length", Dname = "date", Fname = NA, bin_size = 3,
                       length_unit = "cm", plus_group = FALSE, aggregate_dates = TRUE,
                       plot = FALSE)
  plot(dl.lfq, Fname = "catch", ylab = "Length (mm)")
}
#execute above function
#function of species and length filter(outliers)
lfq_year_month("umac", "pmac")

#seasonal mean length function for any species with standard error
mean_length_seasonal <- function(Species, Year) {
  filter(bi.length.y.m.s, species == Species, year == Year) %>%
    ggplot() +
    aes(x = month, y = length, colour = factor(site), group = site) +
    geom_point(size = 2.5, position = position_dodge(width = 0.25)) +
    geom_line(position = position_dodge(width = 0.25)) +
    geom_errorbar(aes(ymin = length - se,
                      ymax = length + se), 
                  width = 0,
                  size = 1,
                  position = position_dodge(width = 0.25)) +
    theme_bw() +
    theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
          #legend.position  = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text             = element_text(size = 13, vjust = 0.1)) +
    labs(x = "Month", y = expression("Mean Length(mm)"), colour = "site")
}
#execute above function
mean_length_seasonal("umac",2017)
