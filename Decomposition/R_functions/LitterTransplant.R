# Litter transplant 

library(readxl) #require packages
library(ggplot2)
library(graphics)
library(stats)
#library(plyr)
library(lattice)
library(dplyr)
library(reshape2)

#load litter transplant data
Litter<- read.table("\\\\eir.uib.no\\home6\\ial008\\FunCab\\Data\\Decomposition\\LitterTransplant.txt", header= TRUE, dec= ",")

Litter$Time<- as.numeric(Litter$Time)
Litter$BurialDate<- as.Date(Litter$BurialDate, format= "%d.%m.%Y")
Litter$RecoveryDate<- as.Date(Litter$RecoveryDate, format= "%d.%m.%Y")             

#rename DonorSite and BurialSite in Litter to match climate
newnames<-c("Alr","Arh", "Fau", "Gud", "Hog","Lav", "Ovs", "Ram", "Skj", "Ulv", "Ves", "Vik")
names(newnames)<-c("ALR","ARH","FAU","GUD","HOG","LAV", "OVS","RAM","SKJ","ULV","VES","VIK")
Litter$DonorSite<-newnames[Litter$DonorSite]
Litter$BurialSite<-newnames[Litter$BurialSite]


# load gridded climate data
load("O:/FunCab/Data/FunCaB/Climate/Data/GriddedDailyClimateData2009-2016.RData")

# function for calculation coefficient of variance
CV<- function (mean, sd){
  (sd/mean)*100  }

# calculate average temperature and precipitation during incubation period
# Function to lookup climate date from recovery and burial dates in the TBI data
Litter.climateLookup <- function(Litter, climate) {
  # Function to find the 'colName' climate between the burial and recovery dates in the climate data frame
  # and apply the 'apFunc' to them
  climLookup <- function(sampInfo, climate, colName, apFunc, ...) {
    apFunc(climate[as.Date(climate$Date) > as.Date(sampInfo[1]) & as.Date(climate$Date) < as.Date(sampInfo[2]) & climate$Site == sampInfo[3], colName], ...)
  }
  
  # Find the temperature data and apply the mean and variance function to it
  tempMean <- apply(X = as.matrix(cbind(as.character(Litter$BurialDate), as.character(Litter$RecoveryDate), as.character(Litter$BurialSite))), FUN = climLookup, MARGIN = 1, climate = climate, colName = "Temperature", apFunc = mean, na.rm = TRUE)
  tempVar <- apply(X = as.matrix(cbind(as.character(Litter$BurialDate), as.character(Litter$RecoveryDate), as.character(Litter$BurialSite))), FUN = climLookup, MARGIN = 1, climate = climate, colName = "Temperature", apFunc = var, na.rm = TRUE)
  
  # Find the precipitation data and apply the sum and variance function to it
  precipTotal <- apply(X = as.matrix(cbind(as.character(Litter$BurialDate), as.character(Litter$RecoveryDate), as.character(Litter$BurialSite))), FUN = climLookup, MARGIN = 1, climate = climate, colName = "Precipitation", apFunc = sum, na.rm = TRUE)
  precipMean <- apply(X = as.matrix(cbind(as.character(Litter$BurialDate), as.character(Litter$RecoveryDate), as.character(Litter$BurialSite))), FUN = climLookup, MARGIN = 1, climate = climate, colName = "Precipitation", apFunc = mean , na.rm = TRUE)
  precipSd <- apply(X = as.matrix(cbind(as.character(Litter$BurialDate), as.character(Litter$RecoveryDate), as.character(Litter$BurialSite))), FUN = climLookup, MARGIN = 1, climate = climate, colName = "Precipitation", apFunc = sd , na.rm = TRUE)
  
# Add the temperature and precipitation data to the TBI data frame
  cbind(
    Litter,
    data.frame(
      gridTemp = tempMean,
      Temp.Var = tempVar,
      gridPrec = precipTotal,
      Prec.CV = precipMean/precipSd*100
    )
  )
}

Litter<-Litter.climateLookup(Litter, climate)



#remove empty or bad data points for analysis
Litter_clean<- subset(Litter,is.na(Comment))


#calculate mean litter loss of same litter from each Donorsite and treatment and timestep
DonorLitter<- Litter_clean%>%
  group_by(DonorSite, Treatment, Timestep, Time) %>%
  summarise(N.sample =length(W_loss), m.Loss = mean(W_loss), sd.Loss = sd(W_loss), se.Loss   = sd.Loss / sqrt(N.sample), m.remain = mean(W_remain), sd.remain = sd(W_remain), se.remain  = sd.remain / sqrt(N.sample))

#change order of sites in dataframe
DonorLitter$DonorSite <- factor(DonorLitter$DonorSite, levels = c("FAU","VIK","ARH","OVS","ALR","HOG","RAM","VES", "ULV","LAV","GUD",                                                                    "SKJ"))

#calculate mean litter loss of different litter at Burial site and treatment and timestep
BurialLitter<- Litter_clean%>%
  group_by(BurialSite, Treatment, Timestep, Time) %>%
  summarise(N.sample =length(W_loss), m.Loss = mean(W_loss), sd.Loss = sd(W_loss), se.Loss   = sd.Loss / sqrt(N.sample), m.remain = mean(W_remain), sd.remain = sd(W_remain), se.remain  = sd.remain / sqrt(N.sample))

#change order of sites in dataframe
BurialLitter$BurialSite <- factor(BurialLitter$BurialSite, levels=c("FAU","VIK","ARH","OVS","ALR","HOG","RAM","VES", "ULV","LAV","GUD",                                                                     "SKJ"))

# plots of %litter loss through time
#comparing same litter at different sites so treatment
ggplot(DonorLitter, aes(Time, col = factor(Treatment)))+
  geom_point(aes(y= m.Loss), size= 5, position = position_dodge(0.5))+
  geom_line(aes(y= m.Loss),size=1, position = position_dodge(0.5))+
  facet_wrap(~DonorSite)+
  ggtitle("%lost of DONORlitter (same litter/different site)")

# comparing different litter at same site
ggplot(BurialLitter, aes(Timestep, col = factor(Treatment)))+
  geom_point(aes(y= m.Loss), size= 5, position = position_dodge(0.5))+
  geom_line(aes(y= m.Loss),size=1, position = position_dodge(0.5))+
  facet_wrap(~BurialSite)+
  ggtitle("%lost at Burialsite (different litter/same site)")

# plots of %litter remain through time
ggplot(DonorLitter, aes(Time, col = factor(Treatment)))+
  geom_point(aes(y= m.remain), size= 5, position = position_dodge(0.5))+
  geom_line(aes(y= m.remain),size=1, position = position_dodge(0.5))+
  facet_wrap(~DonorSite)+
  ggtitle("%remain of DONORlitter (same litter/different site)")

ggplot(BurialLitter, aes(Time, col = factor(Treatment)))+
  geom_point(aes(y= m.remain), size= 5, position = position_dodge(0.5))+
  geom_line(aes(y= m.remain),size=1, position = position_dodge(0.5))+
  facet_wrap(~BurialSite)+
  ggtitle("%remain of DONORlitter (different litter/same site)")

 
