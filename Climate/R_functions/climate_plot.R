
#overview plot of temperature and precipitation in period 2014-2016
ggplot(Temperature_Grid, aes(Month, value, col=Year))+
  geom_smooth()

ggplot(Precipitation_Grid, aes(Month, value, fill=factor(Year)))+
  geom_bar(stat = "identity", position= "dodge")

#Temperature and Precipitation accros grid split by years
ggplot(Temperature_Grid, aes(Month, value, col=Site)) + 
    geom_smooth()+
    facet_grid(~Year)
      
ggplot(Precipitation_Grid, aes(Month, value, fill=Site)) + 
  geom_bar(stat = "identity", position= "dodge")+
  facet_grid(~Year)

# combination of overall Precipitation and Temperature across years 
ggplot(Precipitation_Grid, aes(Month, value, fill=Site)) + 
  geom_bar(stat = "identity", position= "dodge") +
  geom_smooth(data=Temperature_Grid, aes(Month, value), col= "black")+
  scale_fill_grey()+
  theme_classic()

#Temperature and Precipitation accros grid split by years
ggplot(T_mean, aes(Site, mn_T, col=Site)) + 
  geom_bar(stat = "identity", position= "dodge")+
  facet_grid(~Year)

ggplot(Prec_total, aes(Site, total_P, col=Site)) + 
  geom_bar(stat = "identity", position= "dodge")+
  facet_grid(~Year)



#Making Figures from daily climate data
# Temperature
ggplot(climate, aes(x = Date, y = Temperature, color = Site)) +
  geom_line() +
  scale_colour_brewer(palette="Paired") +
  facet_wrap(~ Site) + 
  theme(legend.position="none") +
  ggtitle("Temperature")