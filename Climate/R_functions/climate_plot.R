
#overview plot of temperature and precipitation in period 2014-2016
ggplot(Temperature_Grid, aes(Month, value, col=Year))+
  geom_smooth()

ggplot(Precipitation_Grid, aes(Month, value, fill=factor(Year)))+
  geom_bar(stat = "identity", position= "dodge")


ggplot(Precipitation_Grid, aes(Month, value, fill=Year)) + 
  geom_bar(stat = "identity", position= "dodge") +
  geom_smooth(data=Temperature_Grid, aes(Month, value), col= "black")+
  scale_fill_grey()+
  theme_classic()


#Making Figures from daily climate data
# Temperature
ggplot(climate, aes(x = Date, y = Temperature, color = Site)) +
  geom_line() +
  scale_colour_brewer(palette="Paired") +
  facet_wrap(~ Site) + 
  theme(legend.position="none") +
  ggtitle("Temperature")