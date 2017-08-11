# Climate data ibutton TBI
library(readxl) #require packages
library(dplyr) 
library(tidyr) 
library(ggplot2)
library(reshape2)
library(stringr)
library("lubridate")


# load data ibutton TBI2014
TBI_ibut2014<-read_excel("O:/FunCab/Data/FunCaB/Climate/Data/ibutton/TBI_ibut2014.xlsx")
TBI_ibut2014$day<-format(TBI_ibut2014$datetime, "%d") 
TBI_ibut2014$month<-format(TBI_ibut2014$datetime, "%m") 
TBI_ibut2014$year<-format(TBI_ibut2014$datetime, "%Y") 

# Calculate average soilTemp per day for each logger
TBI_day_sT<-TBI_ibut2014 %>%
            group_by(day, month, year) %>%
            summarise_each(funs(mean(., na.rm =TRUE))) %>%
            arrange(month)

# rounding of temperature values to 2 dec and removing datetime column
is.num <- sapply(TBI_day_sT, is.numeric)
  TBI_day_sT[is.num] <- lapply(TBI_day_sT[is.num], round, 2)
  TBI_day_sT<- TBI_day_sT[ -c(4)]
  
# removing 2 extreme loggers, Ovs-3 and Ovs-4
TBI_day_sT<-   TBI_day_sT[ -c(6, 26, 27)]

# Transforming data into long format and renaming variables > logger and splitting them in site and block
Day_sT<- melt(TBI_day_sT, id.vars = c("month", "day", "year"))
  Day_sT$date<- as.Date(with(Day_sT, paste(day, month, year, sep = "-")), "%d-%m-%Y")

Day_sT<-rename(Day_sT, logger = variable)
  Day_sT$logger<- as.character(Day_sT$logger)

site<-as.data.frame(do.call(rbind, strsplit(Day_sT$logger, "-")))
  Day_sT<- bind_cols(Day_sT, site)
  Day_sT<-rename(Day_sT, Site = V1, Block = V2)

# calculate average soilTemp acros site based on the different ibutton loggers
grid_sT<-Day_sT %>%
          group_by(day, month, year, date, Site) %>%
          summarise(mn_sT = mean(value, na.rm =TRUE)) %>%
          arrange(Site, month)

grid_sT<- grid_sT[complete.cases(grid_sT),]

# retrieve start and end time of TBI incubation time 
TBI_time<-grid_sT %>%
          group_by(Site) %>%
          summarise(BurialDate = first(date), RecoveryDate = last(date))
  

# load climate daily gridded climate data 
load("O:/FunCab/Data/FunCaB/Climate/Data/GriddedDailyClimateData2009-2016.RData")

# Function to lookup climate date from start and end dates in the TBI_ibutton data 2014
TBI.climateLookup <- function(TBI_time, climate) {
  # Function to retrieve a data frame of the climate at the sites between the burial and recovery dates
  climRetrieval <- function(sampInfo, climate, colName) {
    # Boolean denoting the rows of the climate data that we want
    climBool <- as.Date(climate$Date) >= as.Date(sampInfo[1]) & as.Date(climate$Date) <= as.Date(sampInfo[2]) & climate$Site == sampInfo[3]
    # Retieve the climate from the relevant rows of the climate data
    curClim <- climate[climBool, colName]
    climDates <- as.Date(climate[climBool, "Date"])
    # Initialise an output matrix
    outMat <- matrix(NA, ncol = 3, nrow = length(curClim))
    colnames(outMat) <- c(colName, "Site", "Date")
    # Convert matrix to data.frame
    outMat <- as.data.frame(outMat)
    outMat[, colName] <- curClim
    outMat$Site <- rep(sampInfo[3], length(curClim))
    outMat$Date <- climDates
    outMat
  }
  # Apply the climate retrieval function to each site and compress into one giant data frame
  tempData <- do.call(rbind, apply(X = as.matrix(cbind(as.character(TBI_time$BurialDate), as.character(TBI_time$RecoveryDate), as.character(TBI_time$Site))), FUN = climRetrieval, MARGIN = 1, climate = climate, colName = "Temperature"))
  
  tempData
}

Grid.Temp<-TBI.climateLookup(TBI_time, climate)


#load Daily Temperature data from climate stations and TBI data
load("O:/SeedClim-Climate-Data/Daily.Temperature_2008-2016.RData")

# select only loggers recording soil temperature
climStation_sT<- filter(dailyTemperature, logger == "tempsoil")
climStation_sT<- rename(climStation_sT, Date = date, Temperature = value, Site = site)
climStation_sT$Site<- as.factor(climStation_sT$Site)

# load TBI data and select unique burialtime periods 
TBI<-read_excel("O:\\FunCab\\Data\\Decomposition\\TBI\\TBI_141516new07082017.xlsx")
TBI$BurialDate<-as.Date(TBI$BurialDate)
TBI<-rename(TBI, Site = site)


TBI_times<- TBI %>%
            group_by(Site, RecoveryDate) %>%
            distinct(BurialDate)

TBI_times <- TBI_times[c(2, 1,3)]
TBI_times<- TBI_times[complete.cases(TBI_times),]
TBI_times$RecoveryDate<- as.Date(TBI_times$RecoveryDate)

# create dataframe with soilTemperatures of all sites measured by climate stations
Station.Temp<-TBI.climateLookup(TBI_times, as.data.frame(climStation_sT))
Station.Temp$day<-format(Station.Temp$Date, "%d") 
Station.Temp$month<-format(Station.Temp$Date, "%m") 
Station.Temp$year<-format(Station.Temp$Date, "%Y")

# create dataframe with soilTemperatures of all sites from gridded data
Gridded.Temp <-TBI.climateLookup(TBI_times, climate)

Gridded.Temp$day<-format(Gridded.Temp$Date, "%d") 
Gridded.Temp$month<-format(Gridded.Temp$Date, "%m") 
Gridded.Temp$year<-format(Gridded.Temp$Date, "%Y")
Gridded.Temp$yday<- strptime(Gridded.Temp$Date, "%Y-%m-%d")$yday+1



# model soiltemp from gridded data and climate station soiltempsensor
# maybe only use summer months 4-10?
all_TEMP <- left_join(climate, climStation_sT, by= c("Site" = "Site", "Date" = "Date"))
all_TEMP <- left_join(all_TEMP, grid_sT, by= c("Site" = "Site", "Date" = "date") )


modelclimate <- all_TEMP %>%
                group_by(Site) %>%
                mutate( new_T = (Temperature.x - mean(Temperature.x)) * sd(Temperature.y, na.rm =TRUE)/ sd(Temperature.x)+ mean                        (Temperature.x)+(mean(Temperature.y, na.rm =TRUE)-mean(Temperature.x)))%>%
                filter( Year >2013)
                

modelclimate$yday<- strptime(modelclimate$Date, "%Y-%m-%d")$yday+1



# Function to lookup climate date from start and end dates
TBI.climateLookup <- function(TBI_time, climate) {
  # Function to retrieve a data frame of the climate at the sites between the burial and recovery dates
  climRetrieval <- function(sampInfo, climate, colName) {
    # Boolean denoting the rows of the climate data that we want
    climBool <- as.Date(climate$Date) >= as.Date(sampInfo[1]) & as.Date(climate$Date) <= as.Date(sampInfo[2]) & climate$Site == sampInfo[3]
    # Retieve the climate from the relevant rows of the climate data
    curClim <- climate[climBool, colName]
    climDates <- as.Date(climate[climBool, "Date"])
    # Initialise an output matrix
    outMat <- matrix(NA, ncol = 3, nrow = length(curClim))
    colnames(outMat) <- c(colName, "Site", "Date")
    # Convert matrix to data.frame
    outMat <- as.data.frame(outMat)
    outMat[, colName] <- curClim
    outMat$Site <- rep(sampInfo[3], length(curClim))
    outMat$Date <- climDates
    outMat
  }
  # Apply the climate retrieval function to each site and compress into one giant data frame
  gridTemp <- do.call(rbind, apply(X = as.matrix(cbind(as.character(TBI_time$BurialDate), as.character(TBI_time$RecoveryDate), as.character(TBI_time$Site))), FUN = climRetrieval, MARGIN = 1, climate = climate, colName = "Temperature.x"))
stationTemp <- do.call(rbind, apply(X = as.matrix(cbind(as.character(TBI_time$BurialDate), as.character(TBI_time$RecoveryDate), as.character(TBI_time$Site))), FUN = climRetrieval, MARGIN = 1, climate = climate, colName = "Temperature.y"))
modelTemp<- do.call(rbind, apply(X = as.matrix(cbind(as.character(TBI_time$BurialDate), as.character(TBI_time$RecoveryDate), as.character(TBI_time$Site))), FUN = climRetrieval, MARGIN = 1, climate = climate, colName = "new_T"))
ibutTemp <- do.call(rbind, apply(X = as.matrix(cbind(as.character(TBI_time$BurialDate), as.character(TBI_time$RecoveryDate), as.character(TBI_time$Site))), FUN = climRetrieval, MARGIN = 1, climate = climate, colName = "mn_sT"))

  list(
    gridTemp = gridTemp,
    stationTemp = stationTemp,
    modelTemp = modelTemp,
    ibutTemp = ibutTemp
  )
}

tempList <- TBI.climateLookup(TBI_times, as.data.frame(modelclimate))

#function to merge Lists of temperatures (grid, station, model, ibut) together in a dataframe
merge.all<- function (dfs, by){
              return(Reduce(function(x,y){
                merge(x,y, by = by, all =TRUE)}, dfs))
}
  
AllTemp<-merge.all(dfs = tempList, by = c("Site",  "Date"))
AllTemp$Site<-as.factor(AllTemp$Site)
AllTemp<- cbind (AllTemp, Year = year(AllTemp$Date))

#AllTemp %>%
#  group_by(Year, Site)%>%
#  summarise_each(funs(mean(., na.rm =TRUE)))


#calculate means for different Temperature measurements Gridded, climate station, ibutton, modeled
#mean_Temp<- AllTemp %>%
#  group_by(Site, Year)%>%
#  summarise_each(funs(mean(., na.rm =TRUE)))


mean_Temp<-AllTemp %>%
            group_by(Year, Site)%>%
            summarise(model_T = mean(Temperature.x, na.rm =TRUE))

#mean modelTemp for Skj is systematically higher than other alpine sites, replace it with gridded Temp value
mean_Temp$model_T[9]= 10.08 # Skj 2014
mean_Temp$model_T[21]= 7.60 # Skj 2015
mean_Temp$model_T[33]= 8.01 # SKj 2016


mean_Temp$ID<- paste(mean_Temp$Site, mean_Temp$Year)

# select modelled  temperature from year 2014 
modelclimate2014<-modelclimate %>%
                  filter(Year == 2014)

#correlation between modeled temperature Ibutton and climatestation in year 2014
plyr::ddply(modelclimate2014, ~Site, summarize,
               climstation = cor(Temperature.y, mn_sT, use= "pairwise.complete.obs", method = "pearson"),
                ibut = cor(Temperature.x, mn_sT, use= "pairwise.complete.obs", method = "pearson"))

#correlation between modelled soilTemp and measured soilTemp from climate station and ibuttons per site, excluding NA
plyr::ddply(modelclimate, ~Site, summarize,
      met.no = cor(Temperature.x, Temperature.y, use= "complete.obs", method = "pearson"),
      climsTation = cor(Temperature.y, new_T, use= "complete.obs", method = "pearson"),
      ibutton = cor(mn_sT, new_T, use= "complete.obs", method = "pearson"))


#=================================================Precipitation======================================================================


# Function to lookup Precipitation date from start and end dates in the TBI_ibutton data 2014
TBI.climateLookup <- function(TBI_time, climate) {
  # Function to retrieve a data frame of the climate at the sites between the burial and recovery dates
  climRetrieval <- function(sampInfo, climate, colName) {
    # Boolean denoting the rows of the climate data that we want
    climBool <- as.Date(climate$Date) >= as.Date(sampInfo[1]) & as.Date(climate$Date) <= as.Date(sampInfo[2]) & climate$Site == sampInfo[3]
    # Retieve the climate from the relevant rows of the climate data
    curClim <- climate[climBool, colName]
    climDates <- as.Date(climate[climBool, "Date"])
    # Initialise an output matrix
    outMat <- matrix(NA, ncol = 3, nrow = length(curClim))
    colnames(outMat) <- c(colName, "Site", "Date")
    # Convert matrix to data.frame
    outMat <- as.data.frame(outMat)
    outMat[, colName] <- curClim
    outMat$Site <- rep(sampInfo[3], length(curClim))
    outMat$Date <- climDates
    outMat
  }
  # Apply the climate retrieval function to each site and compress into one giant data frame
  PrecData <- do.call(rbind, apply(X = as.matrix(cbind(as.character(TBI_time$BurialDate), as.character(TBI_time$RecoveryDate), as.character(TBI_time$Site))), FUN = climRetrieval, MARGIN = 1, climate = climate, colName = "Precipitation"))
  
  PrecData
  
}

Gridded.Prec <-TBI.climateLookup(TBI_times, climate) # change colName to Precipitation

Gridded.Prec$day<-format(Gridded.Prec$Date, "%d") 
Gridded.Prec$month<-format(Gridded.Prec$Date, "%m") 
Gridded.Prec$year<-format(Gridded.Prec$Date, "%Y")
Gridded.Prec$yday<- strptime(Gridded.Prec$Date, "%Y-%m-%d")$yday+1

Total.Prec<- Gridded.Prec %>%
              group_by(year, Site) %>%
              summarise( total.Prec = sum(Precipitation))
Total.Prec$Site <- factor(Total.Prec$Site, levels = c("Fau","Vik","Arh","Ovs","Alr","Hog","Ram","Ves", "Ulv","Lav","Gud", "Skj"))

ggplot(Total.Prec, aes(Site, total.Prec, col = year))+
  geom_bar(stat = "identity", position = "dodge", fill = "white")+
  #scale_x_discrete(labels= c("Fau", "Alr", "Ulv", "Vik", "Hog", "Lav", "Arh", "Ram", "Gud", "Ovs", "Ves", "Skj"))+
  theme_classic()

ggplot(Gridded.Prec, aes(yday, Precipitation, color = year))+
  geom_smooth()+
  theme_bw()

#==========Soil Moisture=============================================================================================================
load("O:/SeedClim-Climate-Data/Monthly.Soilmoisture_2008-2016.Rdata")

monthlySoilmoisture$day<-format(monthlySoilmoisture$date, "%d")
monthlySoilmoisture$month<-format(monthlySoilmoisture$date, "%m")
monthlySoilmoisture$year<-format(monthlySoilmoisture$date, "%Y")


climStation_sM<- monthlySoilmoisture %>%
                  group_by(site, date, year) %>%
                  summarise(mn_sM = mean(value, na.rm = TRUE)) %>%
                  filter(year> 2013)


ggplot(climStation_sM, aes(date, mn_sM, col = year))+
    geom_line()+
    facet_wrap(~site)+
    labs(x = "Date ", y= "soilmoisture")+
    theme_bw()




#calculate mean soil Temperature from ibuttons and mean gridded Temperature over incubation period
TBI_temperature %>%
  group_by(Site)%>%
  summarise(sT= mean(mn_sT), gridT = mean(Temperature.x))



#plot soilTemp from all loggers over incubation time
ggplot(Day_sT, aes(date, value, col= logger))+
  geom_line()

#plot mean soilTemp at site over incubation time
ggplot(grid_sT, aes(date, mn_sT, col= Site))+
  geom_line()
