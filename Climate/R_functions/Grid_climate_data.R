# Load Gridded climate data 
load("O:/FunCab/Data/FunCaB/Climate/Data/GriddedDailyClimateData2009-2016.RData")
load("O:/FunCab/Data/FunCaB/Climate/Data/GriddedMonth_AnnualClimate2009-2016.Rdata")

#description Climate data
#Mean daily temperature (°C, Temperature)
#Relative air moisture (%, RelAirMoisture)
#Mean wind (meter / second, Wind)
#Mean cloud cover (in 8 parts, no dimension, CloudCover)
#Precipitation (mm, Precipitation)

# load packages
library(ggplot2)
library(dplyr)
library(tidyr)


#create new column for year and month in monthlyClimate
monthlyClimate$Year<- format(monthlyClimate$dateMonth,format= "%Y") 
monthlyClimate$Month<- format(monthlyClimate$dateMonth,format= "%b") 
numMonth<-function(x) 
  c(jan=1,feb=2,mar=3,apr=4,mai=5,jun=6,jul=7,aug=8,sep=9,okt=10,nov=11,des=12)[tolower(x)]
monthlyClimate$Month<-numMonth(monthlyClimate$Month)


#select data for different loggers for period 2014-2016
Temperature_Grid<- filter(monthlyClimate, Logger=="Temperature", Year>2013)
Temperature_Grid<- subset(Temperature_Grid, Month>=5 & Month<11)
Precipitation_Grid<- filter(monthlyClimate, Logger=="Precipitation", Year>2013)
Precipitation_Grid<- subset(Precipitation_Grid, Month>=5 & Month<11)
Airmoisture_Grid<- filter(monthlyClimate, Logger=="RelAirMoisture", Year>2013)
Airmoisture_Grid<- subset(Airmoisture_Grid, Month>=5 & Month<11)

#calculate means over summer period
T_mean<- Temperature_Grid %>%
         group_by(Site, Year)%>%
         summarise(mn_T = mean(value))

#calculate means over summer period
Prec_total<- Precipitation_Grid %>%
             group_by(Site, Year)%>%
             summarise(total_P = sum(value))  

#calculate means over summer period
airM_mean<- Temperature_Grid %>%
            group_by(Site, Year)%>%
            summarise(mn_T = mean(value))

#average_climate<- right_join(T_mean, Prec_total, by = "Site" & "Year")


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