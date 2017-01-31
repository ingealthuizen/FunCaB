# Climate data ibutton TBI
library(readxl) #require packages
library(dplyr) 
library(tidyr) 
library(ggplot2)
library(reshape2)
library(stringr)

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
          summarise(start_date = first(date), end_date = last(date))
  

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
  tempData <- do.call(rbind, apply(X = as.matrix(cbind(as.character(TBI_time$start_date), as.character(TBI_time$end_date), as.character(TBI_time$Site))), FUN = climRetrieval, MARGIN = 1, climate = climate, colName = "Temperature"))
  
  tempData
}

Grid.Temp<-TBI.climateLookup(TBI_time, climate)


#load Daily Temperature data from climate stations and TBI data
load("O:/SeedClim-Climate-Data/Daily.Temperature_2008-2016.RData")

climStation_sT<- filter(dailyTemperature, logger == "tempsoil")
climStation_sT<- rename(climStation_sT, Date = date)

TBI<-read_excel("O:\\FunCab\\Data\\Decomposition\\TBI\\TBI_141516.xlsx")
TBI$BurialDate<-as.Date(TBI$`BurialDate `)
TBI$RecoveryDate<-as.Date(TBI$RecoveryDate)

TBI_times<- TBI %>%
            group_by(site, RecoveryDate) %>%
            distinct(BurialDate)

TBI_times <- TBI_times[c(2, 1,3)]
TBI_times<- TBI_times[complete.cases(TBI_times),]


# Function to lookup climate date from start and end dates in the TBI_ibutton data 2014
TBI.climateLookup <- function(TBI_time, climate) {
  # Function to retrieve a data frame of the climate at the sites between the burial and recovery dates
  climRetrieval <- function(sampInfo, climate, colName) {
    # Boolean denoting the rows of the climate data that we want
    climBool <- as.Date(climate$Date) >= as.Date(sampInfo[1]) & as.Date(climate$Date) <= as.Date(sampInfo[2]) & climate$site == sampInfo[3]
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
  tempData <- do.call(rbind, apply(X = as.matrix(cbind(as.character(TBI_time$BurialDate), as.character(TBI_time$RecoveryDate), as.character(TBI_time$site))), FUN = climRetrieval, MARGIN = 1, climate = climate, colName = "value"))
  
  tempData
}

# create dataframe with soilTemperatures of all sites measured by climate stations
Station.Temp<-TBI.climateLookup(TBI_times, as.data.frame(climStation_sT))
Station.Temp$day<-format(Station.Temp$Date, "%d") 
Station.Temp$month<-format(Station.Temp$Date, "%m") 
Station.Temp$year<-format(Station.Temp$Date, "%Y")

#subset only data from particular year
Station_2015<- Station.Temp %>%
              filter(year == 2015)

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
  tempData <- do.call(rbind, apply(X = as.matrix(cbind(as.character(TBI_time$BurialDate), as.character(TBI_time$RecoveryDate), as.character(TBI_time$site))), FUN = climRetrieval, MARGIN = 1, climate = climate, colName = "Temperature"))
  
  tempData
}

# create dataframe with soilTemperatures of all sites from gridded data
Gridded.Temp <-TBI.climateLookup(TBI_times, climate)
Gridded.Temp$day<-format(Gridded.Temp$Date, "%d") 
Gridded.Temp$month<-format(Gridded.Temp$Date, "%m") 
Gridded.Temp$year<-format(Gridded.Temp$Date, "%Y")

#subset only data from particular year
Gridded_2015 <-Gridded.Temp %>%
                filter(year == 2015)

#join temperature data of ibuttons and gridded climate data together by Site and Date
TBI_temperature<-left_join(Grid.Temp, grid_sT, by= c("Site" = "Site", "Date" = "date" ))
TBI_temperature<-left_join(TBI_temperature, Station_2014, by= c ("Site" = "Site", "Date" = "Date"))

TBI_temp2015 <-right_join(Station_2015, Gridded_2015, by= c("Site" = "Site", "Date" = "Date" ))


ggplot(TBI_temperature, aes(Date))+
  geom_line(aes(y= Temperature, col= "Temperature" ))+
  geom_line(aes(y= mn_sT, col= "mn_sT"))+
  geom_line(aes(y= value, col= "value"))+
  facet_wrap(~Site)+
  theme_bw()

ggplot(TBI_temp2015, aes(Date))+
  geom_line(aes(y= Temperature, col= "Temperature" ))+
  geom_line(aes(y= value, col= "value"))+
  facet_wrap(~Site)+
  theme_bw()


#calculate mean soil Temperature from ibuttons and mean gridded Temperature over incubation period
TBI_temperature %>%
  group_by(Site)%>%
  summarise(sT= mean(mn_sT), gridT = mean(Temperature))



#plot soilTemp from all loggers over incubation time
ggplot(Day_sT, aes(date, value, col= logger))+
  geom_line()

#plot mean soilTemp at site over incubation time
ggplot(grid_sT, aes(date, mn_sT, col= Site))+
  geom_line()