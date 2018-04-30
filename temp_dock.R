

pruthtemp <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/Soft_sed/working_files/temperature/pruth_dock_temp_2015_2018.csv")

#format dat/time
pruthtemp$date.time <- mdy_hm(pruthtemp$date.time)

#coerce date format
pruthtemp$year <- year(pruthtemp$date.time)
pruthtemp$month <- month(pruthtemp$date.time)
pruthtemp$day <- mday(pruthtemp$date.time)
pruthtemp$time <- hour(pruthtemp$date.time)
pruthtemp$month <- factor(pruthtemp$month, 
                          levels = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                          labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

pruthtemp.ch <- pruthtemp 
pruthtemp.ch$year <- as.factor(pruthtemp$year)

#monthly temperature boxplot
docktemp <-
ggplot(pruthtemp.ch) +
  aes(x = month, y = temp, fill = year) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Month", y = expression(paste("Mean Temperature (",degree,"C)"))) +
  ggtitle("Pruth Dock average temperature")

docktemp

#monthly daytime temperature boxplot 
dockdaytimetemp <-
pruthtemp.ch %>%
  filter(time < 16 & time > 8) %>%
  group_by(year, month, day, site) %>%
  summarise(temp_daily = mean(temp)) %>%
ggplot() +
  aes(x = month, y = temp_daily, fill = year) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Month", y = expression(paste("Mean Temperature (",degree,"C)"))) +
  ggtitle("Pruth Dock average daytime temperature")

dockdaytimetemp

#monthly max temperature boxplot
dockmaxtemp <-
pruthtemp.ch %>%
  ggplot() +
  aes(x = month, y = AirTemp_Max, fill = year) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Month", y = expression(paste("Mean Temperature (",degree,"C)"))) +
  ggtitle("Pruth Dock maximum daily temperature")

dockmaxtemp

#monthly max temperature boxplot
dockmintemp <-
  pruthtemp.ch %>%
  ggplot() +
  aes(x = month, y = AirTemp_Min, fill = year) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Month", y = expression(paste("Mean Temperature (",degree,"C)"))) +
  ggtitle("Pruth Dock maximum daily temperature")

dockmintemp

