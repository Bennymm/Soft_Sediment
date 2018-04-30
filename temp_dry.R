#run temp 20152017.R first

#filter out drying times
tempdry <- filter(temptideSS, tide < 1.9)

#extract time from timestamp
tempdry$time <- hour(tempdry$date.time)

#filter daytime temps
temp.dry.day <- tempdry %>%
  filter(time < 16 & time > 8) %>%
  group_by(year, month, day, site) %>%
  summarise(temp_daily = mean(temp))

#minimum
temp.dry.min <- tempdry %>%
  group_by(year, month, day, site) %>%
  summarise(temp_daily = min(temp))

#filter daytime temps
temp.dry.max <- tempdry %>%
  group_by(year, month, day, site) %>%
  summarise(temp_daily = max(temp))

#temp_dry_daily <-
  #temp.dry.day %>%  
  ggplot(temp.dry.day) +
  aes(x = month, y = temp_daily, colour = site, group = site) +
  geom_point(size = 2.5, position = position_dodge(width = 0.7)) +
  facet_wrap(~year)+
  theme_bw() +
   theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Month", y = expression(paste("Mean Temperature (",degree,"C)")))
#temp_dry_monthly

  
#boxplot 
drydaytimetemp <-
ggplot(temp.dry.day) +
  aes(x = month, y = temp_daily, fill = site) +
  geom_boxplot() +
  facet_wrap(~year)+
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Month", y = expression(paste("Mean Temperature (",degree,"C)"))) +
  ggtitle("Dry daytime average temperature")

drydaytimetemp

#boxplot 
drymin <-
  ggplot(temp.dry.min) +
  aes(x = month, y = temp_daily, fill = site) +
  geom_boxplot() +
  facet_wrap(~year)+
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Month", y = expression(paste("Mean Temperature (",degree,"C)"))) +
  ggtitle("Minimum average temperature")

drymin

#boxplot 
drymax <-
  ggplot(temp.dry.max) +
  aes(x = month, y = temp_daily, fill = site) +
  geom_boxplot() +
  facet_wrap(~year)+
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Month", y = expression(paste("Mean Temperature (",degree,"C)"))) +
  ggtitle("Maximum average temperature")

drymax
