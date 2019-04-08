# TBI data
# First run TBI_ibutton for getting Modelled Temperature 

library(readxl) #require packages
library(ggplot2)
library(graphics)
library(stats)
#library(plyr)
library(lattice)
library(dplyr)
library(reshape2)

TBI<-read_excel("O:\\FunCab\\Data\\Decomposition\\TBI\\TBI_141516_07082017.xlsx")


#change particular columns to factors 
TBI$site<- as.factor(TBI$site)
TBI$year<- as.factor(TBI$year)
TBI$Temp<- as.factor(TBI$Temp)
TBI$Prec<- as.factor(TBI$Prec)
#TBI$BurialDate<-TBI$`BurialDate `
#rounding of numeric data on 2 decimals
is.num <- sapply(TBI, is.numeric)
TBI[is.num] <- lapply(TBI[is.num], round, 3)

#Ag = fraction decomposed green tea, Ar = Predicted labile fraction red tea, Wt = Fraction remaining red tea
# new column for fraction decomposed red tea
TBI$decomp.R<- 1-TBI$Wt


# load climate daily climate data! 
load("O:/FunCab/Data/FunCaB/Climate/Data/GriddedDailyClimateData2009-2016.RData")

# function for calculation coefficient of variance
CV<- function (mean, sd){
  (sd/mean)*100  }

# Function to lookup climate date from recovery and burial dates in the TBI data
TBI.climateLookup <- function(TBI, climate) {
  # Function to find the 'colName' climate between the burial and recovery dates in the climate data frame
  # and apply the 'apFunc' to them
  climLookup <- function(sampInfo, climate, colName, apFunc, ...) {
    apFunc(climate[as.Date(climate$Date) > as.Date(sampInfo[1]) & as.Date(climate$Date) < as.Date(sampInfo[2]) & climate$Site == sampInfo[3], colName], ...)
  }
  
  # Find the temperature data and apply the mean and variance function to it
  tempMean <- apply(X = as.matrix(cbind(as.character(TBI$BurialDate), as.character(TBI$RecoveryDate), as.character(TBI$site))), FUN = climLookup, MARGIN = 1, climate = climate, colName = "Temperature", apFunc = mean, na.rm = TRUE)
  tempVar <- apply(X = as.matrix(cbind(as.character(TBI$BurialDate), as.character(TBI$RecoveryDate), as.character(TBI$site))), FUN = climLookup, MARGIN = 1, climate = climate, colName = "Temperature", apFunc = var, na.rm = TRUE)
  
  # Find the precipitation data and apply the sum and variance function to it
 precipTotal <- apply(X = as.matrix(cbind(as.character(TBI$BurialDate), as.character(TBI$RecoveryDate), as.character(TBI$site))), FUN = climLookup, MARGIN = 1, climate = climate, colName = "Precipitation", apFunc = sum, na.rm = TRUE)
  precipMean <- apply(X = as.matrix(cbind(as.character(TBI$BurialDate), as.character(TBI$RecoveryDate), as.character(TBI$site))), FUN = climLookup, MARGIN = 1, climate = climate, colName = "Precipitation", apFunc = mean , na.rm = TRUE)
  precipSd <- apply(X = as.matrix(cbind(as.character(TBI$BurialDate), as.character(TBI$RecoveryDate), as.character(TBI$site))), FUN = climLookup, MARGIN = 1, climate = climate, colName = "Precipitation", apFunc = sd , na.rm = TRUE)
  
  
  
  # Add the temperature and precipitation data to the TBI data frame
  cbind(
    TBI,
    data.frame(
      gridTemp = tempMean,
      Temp.Var = tempVar,
      gridPrec = precipTotal,
      Prec.CV = precipMean/precipSd*100
    )
  )
}

TBI<-TBI.climateLookup(TBI, climate)


# bind modeled temperature to TBI_variables
TBI$ID <- paste(TBI$site, TBI$year)
TBI$modelTemp<- mean_Temp$model_T[match(TBI$ID,mean_Temp$ID)]


# add data from Serge 2010-2012 to dataframe
site_variables<-read.table("O:/FunCab/Data/FunCaB/Other/Data_Serge/Variable_soildata.txt", header= TRUE, dec= ",")
#first exclude two extreme values for GUD, 1059.67 and 487.78
site_variables$AvailN[site_variables$AvailN > 480] <- NA
#x<-site_variables%>%
 # group_by(site)%>%
  #summarise_each(funs(sd(., na.rm =TRUE))) 

#calculate mean values for all variables per site excluding NA's
site_variables<-site_variables %>%
                          group_by(site) %>%
                          summarise_all(funs(mean(., na.rm =TRUE))) 

# combine data mean.site.variables with TBI based on site
TBI_variables<-right_join(TBI, site_variables, by= "site")

# load and read litter CN data
Litter_data<- read_excel("O:/FunCab/Data/FunCaB/Decomposition/Data/Litter/Litter_CN2016.xlsx")
Litter_mean<- Litter_data %>%
              group_by(site) %>%
              summarise( Litter.C = mean(C), Litter.N = mean(N), Litter.CN = mean(CNratio))

# combine data mean.site.variables with TBI based on site
TBI_variables<-left_join(TBI_variables, Litter_mean, by= "site")


# load soil moisture data 2014-2016
soilmoisture<-read_excel("O:/FunCab/Data/FunCaB/Climate/Data/soil_moisture_141516.xlsx")
soilmoisture$year<- as.factor(soilmoisture$year)
soilmoisture$site<- as.factor(soilmoisture$site)
soilmoisture<- soilmoisture %>%
                      group_by(site, year) %>%
                      summarise(soil_moist = mean(mean_moist, na.rm =TRUE)) %>%
                      ungroup()

tidyr::spread(soilmoisture, year, soil_moist)

# combine data mean.site.moisture with TBI based on site
TBI_variables<-full_join(TBI_variables, soilmoisture, by= c("site" = "site", "year" = "year" ))


# load vegetation biomass data 2014-2016 and add as variables to TBI_variables
biomass_data<- read_excel("O:/FunCab/Data/FunCaB/Other/Vegetation/biomass_1415.xlsx")
biomass_data$Year<- as.factor(biomass_data$Year)
biomass_data$Site<- as.factor(biomass_data$Site)

biomass_data<-biomass_data %>%
                    group_by(Site, Year) %>%
                    summarise_all(funs(mean(., na.rm =TRUE))) 
                    

TBI_variables<-left_join(TBI_variables, biomass_data, by= c("site" = "Site", "year" = "Year" ))


# load vegetation diversity data and subset years 2015-2016 
P.diversity_data<- read.table ("O:/FunCab/Data/FunCaB/Other/Vegetation/diversity.txt", header= TRUE)

#rename siteID to match TBI_variables
#newnames<-c("Alr","Arh", "Fau", "Gud", "Hog","Lav", "Ovs", "Ram", "Skj", "Ulv", "Ves", "Vik")
#names(newnames)<-c("Alrust","Arhelleren","Fauske","Gudmedelen","Hogsete","Lavisdalen", "Ovstedal","Rambera","Skjellingahaugen","Ulvhaugen","Veskre","Vikesland")
#P.diversity_data$site<-newnames[P.diversity_data$siteID]

#Select diversity data from 2015 and 2016
P.diversity_data<- subset(P.diversity_data, Year>=2015)
P.diversity_data$Year<- as.factor(P.diversity_data$Year)

#P.diversity_data %>%
 # group_by(site) %>%
 #summarise(sd=sd(diversity,na.rm=TRUE))

# calculate mean richness and diversity and add as variables to TBI_variables
P.diversity_mean<-P.diversity_data %>%
                      group_by(site) %>%
                      summarise(P_div = mean(diversity, na.rm =TRUE), P_even = mean(evenness, na.rm =TRUE)) 


TBI_variables<-left_join(TBI_variables, P.diversity_mean, by= c("site" = "site"))

# read in microbial data
microbial_data<- read_excel("O:/FunCab/Data/FunCaB/Decomposition/Data/TBI/Microbialdiversity_turfs.xlsx")

#Select diversity data from 2015 and 2016
microbial_data<- subset(microbial_data, Treat == "Control")

# calculate mean richness and diversity and add as variables to TBI_variables
microbial_data<-microbial_data %>%
                      group_by(Site) %>%
                      summarise(M_Richnes = mean(Rarefied_richness), M_Shannon.H = mean(Shannon_H), M_P.even = mean(Pielou_evenness),                        M_Rar.even = mean(Rarefied_evenness), M_Simpson.I = mean(Simpson_index)) 

TBI_variables<-left_join(TBI_variables, microbial_data, by= c("site" = "Site"))


##rounding of numeric data on 2 decimals
is.num <- sapply(TBI_variables, is.numeric)
TBI_variables[is.num] <- lapply(TBI_variables[is.num], round, 3)

# Remove unimportant and duplicate columns
TBI_variables<- TBI_variables[ -c(1, 28:32, 48)]
TBI_variables$year<- as.numeric(TBI_variables$year)

#create new variable rain factor (RF), Lang et al 1976 Water and Plant Life. Springer: Berlin, Heidelberg, New York 
#the ratio between mean precipitation and mean temperature, or RF

#change order of sites in dataframe
TBI_variables$site <- factor(TBI_variables$site, levels = c("Fau","Vik","Arh","Ovs","Alr","Hog","Ram","Ves", "Ulv","Lav","Gud",                                                              "Skj"))

#write.table(TBI_variables, file = "O:\\FunCab\\Data\\Decomposition\\TBI\\TBI_variables.csv")

#==============================================================================================================================


