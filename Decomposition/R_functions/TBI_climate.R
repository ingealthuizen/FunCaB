library(ggplot2)
library(plyr)
library(dplyr)
library(reshape)

# load Monthly.Temp_GriddedData_2010-2015.RData from files
load("Monthly.Temp_GriddedData_2010-2015.RData", verbose = TRUE)

# make new column for year, month.
y<- format(monthly.temp$date, format = "%Y")
monthly.temp$year <- y
y<- format(monthly.temp$date, format = "%m" )
monthly.temp$month<- y 
monthly.temp$month<- as.numeric(monthly.temp$month)
monthly.temp$year<- as.numeric(monthly.temp$year)

#rename sites from gridded data and make it a factor
monthly.temp$site<- revalue(monthly.temp$site, c("Skjellingahaugen"="Skj","Gudmedalen"="Gud","Lavisdalen"="Lav", "Ulvhaugen"="Ulv", "Veskre"="Ves", "Rambera"="Ram", "Hogsete"="Hog", "Alrust"="Alr", "Ovstedal"="Ovs", "Arhelleren"="Arh", "Vikesland"="Vik", "Fauske"="Fau")) 


# select climate observations from all sites in summer periods of year 2014, 2015 and 2016
TBI.gridclimate<-subset(monthly.temp, year>2013 & month>4 & month<11 )

#calculate mean temperature over whole summer period per year
TBI.meanTemp<-ddply(TBI.gridclimate,  .(site, year),  summarize,  mean.temp = round(mean(temperature), 2))
cast(TBI.meanTemp, site~year)
# VIK and HOG same climate in gridded data

#plot temperature of sites per year
ggplot(TBI.gridclimate, aes(month, temperature, col=factor(site)))+
  geom_line(size=1)+
  facet_grid(.~ year)


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////

# load Monthly.Temperature_2008-2016.RData from files
load("Monthly.Temperature_2008-2016.RData", verbose = TRUE)

# make new column for year, month.
y<- format(monthlyTemperature$date, format = "%Y")
monthlyTemperature$year <- y
y<- format(monthlyTemperature$date, format = "%m" )
monthlyTemperature$month<- y 
monthlyTemperature$month<- as.numeric(monthlyTemperature$month)
monthlyTemperature$year<- as.numeric(monthlyTemperature$year)


# select climate observations from all sites and loggers in summer periods of year 2014, 2015 and 2016
TBI.climatestation<-subset(monthlyTemperature, year > 2013 & month>4 & month<11)
climatestation2014<-subset(monthlyTemperature, year ==  2014 & month>4 & month<11)
climatestation2015<-subset(monthlyTemperature, year ==  2015 & month>4 & month<11)

#plot temperature from all loggers of sites per year
#ggplot(climatestation2014, aes(month, value, col=factor(logger)))+
#  geom_line(size=1)+
#  facet_wrap(~ site)

#ggplot(climatestation2015, aes(month, value, col=factor(logger)))+
#  geom_line(size=1)+
#  facet_wrap(~ site)

# create plot for each site 
#site_list <- unique(TBI.climatestation$site)
#for (i in seq_along(site_list)) { 
    # create plot for each site 
#  q <- ggplot(subset(TBI.climatestation, TBI.climatestation$site==site_list[i]),
#           aes(month, value, colour = factor(logger))) + 
#           geom_line(size=1)+
#           facet_grid(.~ year)+
#           ggtitle(paste(site_list[i]))
#  print(q)
#}
#q

# temp200cm missing values for gudmedalen in 2014
TBI.climatestation.200<-subset(monthlyTemperature, year>2013 & month>4 & month<10 & logger =="temp200cm")
TBI.climatestation.30<-subset(monthlyTemperature, year>2013 & month>4 & month<10 & logger =="temp30cm")
TBI.climatestation.a<-subset(monthlyTemperature, year>2013 & month>4 & month<10 & logger =="tempabove")
TBI.climatestation.s<-subset(monthlyTemperature, year>2013 & month>4 & month<10 & logger =="tempsoil")


#plot temperature of sites per year
ggplot(TBI.climatestation, aes(month, value, col=factor(site)))+
  geom_line(size=1)+
  facet_grid(.~ year)

#calculate mean temperature over whole summer period per year
TBI.meanTempstation.200<-ddply(TBI.climatestation.200, .(site,year), summarize, mean.temp =round(mean(value), 2))
TBI.meanTempstation.30<-ddply(TBI.climatestation.30, .(site, year), summarize, mean.temp = round(mean(value), 2))
TBI.meanTempstation.a<-ddply(TBI.climatestation.a, .(site, year), summarize,  mean.temp = round(mean(value), 2))
TBI.meanTempstation.s<-ddply(TBI.climatestation.s, .(site, year), summarize,  mean.temp = round(mean(value), 2))


# temp200cm missing values for gudmedalen in 2014, fill gap with data climate grid, Gud 2014 = 5.72 
x <- data.frame(site="Gud",year=2014, mean.temp=5.72)
TBI.meanTempstation.200<- rbind(TBI.meanTempstation.200, x)

x<-cast(TBI.meanTempstation.200, site~year)
x$dT1415<- (x[,2] - x[,3])
x$dT1416<- (x[,2] - x[,4])
x$dT1516<- (x[,3] - x[,4])

#compare gridded climate data to climate station data!
compare.climate<- merge(TBI.meanTempstation.200, TBI.meanTemp, by = c("site", "year"))
compare.climate$dT<- compare.climate$mean.temp.x - compare.climate$mean.temp.y
#compare.climate<- merge(TBI.climatestation, TBI.gridclimate)

#compare mean Temp from different logger of climatestations
x<- merge(TBI.meanTempstation.200, TBI.meanTempstation.30, by = c("site", "year"))
y<- merge(TBI.meanTempstation.s, TBI.meanTempstation.a, by = c("site", "year"))
compare.logger<- merge(x, y, by = c("site", "year"))
colnames(compare.logger) <- c("site", "year", "200cm", "30cm", "above", "soil")
#missing out on lines of data which had NA for sites
# soil temperature higher then airtemperature?!

#//////////////////////////////////////////////////////////////////////////////////////////////////////////////
# load GriddedMonthlyClimateData2009-2015.RData from files
load("GriddedMonthlyClimateData2009-2015.RData", verbose = TRUE)

#plot temperature data of all sites across years
monthlyClimate %>% 
  filter(Logger == "Temperature") %>% 
  ggplot(aes(x = dateMonth, y = value)) + 
  geom_line() + 
  facet_wrap(~Site)

#plot precipitation data of all sites accross years
monthlyClimate %>% 
  filter(Logger == "Precipitation") %>%
  ggplot(aes(x = dateMonth, y = value)) + 
  geom_line() + 
  facet_wrap(~Site)

y<- format(monthlyClimate$dateMonth, format = "%Y")
monthlyClimate$year <- y
y<- format(monthlyClimate$dateMonth, format = "%m" )
monthlyClimate$month<- y 
monthlyClimate$month<- as.numeric(monthlyClimate$month)
monthlyClimate$year<- as.numeric(monthlyClimate$year)

#subset summer period, may to sept, form years 2014-2015
climatestation.prec<-  subset(monthlyClimate, year>2013 & month>4 & month<10 & Logger == "Precipitation")

#average precipitation per site per year
ggplot(climatestation.prec, aes(month, value, color=factor(year)))+
  geom_line(size=1)+
  facet_wrap(~Site)
  
#calculate sum and mean precipitation at site per year
#prec.total<-ddply(climatestation.prec,  .(Site, year),  summarize,  prec = sum(value))
#prec.total<-ddply(climatestation.prec,  .(Site, year),  summarize,  total.prec = mean(value))
#x<-cast(prec.total, Site~year)
#x$dT1415<- (x[,2] - x[,3])
# VIK and HOG same climate in gridded data, ALR wetter than VIK and HOG!


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////

# load GriddedDailyClimateData2009-2015.RData
load("GriddedDailyClimateData2009-2015.RData", verbose = TRUE)
climate0915$Month<-as.numeric(climate0915$Month)


# not correct code! 
monthlyPrecipitation <- climate0915 %>%
  group_by(Month, Year, Site) %>%
  summarise(n = n(), mean.prec = mean(Precipitation), sum.prec = sum(Precipitation)) %>%
  #filter(n > threshold) %>%
  select(-n)



climate0915 %>% 
  filter(Year == "2015" | Year == "2014") %>%
  filter( Month>4 & Month<10) %>%
  ggplot(aes(x = Month, y = Precipitation, color=factor(Year) )) + 
  geom_col() + 
  facet_wrap(~Site)

#///////////////////////////////////////////////////////////////////////////////////////////////////////////////

# MONTHLY AND DAILY precipitation
library("lubridate")
library("tidyr")
library("dplyr")
library("ggplot2")

load("Soilmoisture.RData", verbose = TRUE)
#### Calculate monthly means ####
threshold <- 7 * 24 #~ one week. Minimum accepted

monthlySoilmoisture <- soilmoisture %>%
  filter(!is.na(value)) %>%
  mutate(date = dmy(paste0("15-",format(date, "%b.%Y")))) %>%
  group_by(date, logger, site) %>%
  summarise(n = n(), value = mean(value), sum = sum(value)) %>%
  #filter(n > threshold) %>%
  select(-n, -sum)

save(monthlySoilmoisture, file = "Monthly.Soilmoisture_2008-2016.RData")

filler <- expand.grid(
  site = unique(monthlySoilmoisture$site),
  logger = unique(monthlySoilmoisture$logger),
  date = seq(
    min(monthlySoilmoisture$date),
    max(monthlySoilmoisture$date),
    by = "month"
  )
)
monthlySoilmoisture <- merge(monthlySoilmoisture, filler, all = TRUE)


ggplot(monthlySoilmoisture, aes(date, value, color=factor(logger)))+
  geom_line()+
  facet_wrap(~site)



load("Precipitation.RData", verbose = TRUE)
#### Calculate monthly means ####
threshold <- 7 * 24 #~ one week. Minimum accepted

monthlyPrecipitation <- precipitation %>%
  filter(!is.na(value)) %>%
  mutate(date = dmy(paste0("15-",format(date, "%b.%Y")))) %>%
  group_by(date, logger, site) %>%
  summarise(n = n(), value = mean(value), sum = sum(value)) %>%
  #filter(n > threshold) %>%
  select(-n)

save(monthlyPrecipitation, file = "Monthly.Precipitation_2008-2016.RData")

filler <- expand.grid(
  site = unique(monthlyPrecipitation$site),
  logger = unique(monthlyPrecipitation$logger),
  date = seq(
    min(monthlyPrecipitation$date),
    max(monthlyPrecipitation$date),
    by = "month"
  )
)
monthlyPrecipitation <- merge(monthlyPrecipitation, filler, all = TRUE)


ggplot(monthlyPrecipitation, aes(date, sum))+
  geom_line()+
  facet_wrap(~site)
