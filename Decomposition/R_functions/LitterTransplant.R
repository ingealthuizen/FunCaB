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
Litter<- read.table("\\\\eir.uib.no\\home6\\ial008\\FunCab\\Data\\Decomposition\\LitterTransplant_14082017.txt", header=TRUE, dec= ",")

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

# add column "site" to add litter quality values to Litter data
Litter$site<- Litter$BurialSite

# Add litter quality data to Litter dataframe
L_quality <- read_excel("O:\\FunCab\\Data\\FunCaB\\Decomposition\\Data\\Litter\\Litter_CN2016.xlsx")


L_mean.quality<- L_quality%>%
  group_by(site) %>%
  summarise(C = mean(C), N = mean(N), CNratio = mean(CNratio))

Litter<- full_join(Litter, L_mean.quality, by ="site")


#remove empty or bad data points for analysis #17 observations excluded
Litter_clean<- subset(Litter,is.na(DataCheck))


#calculate mean litter loss of same litter from each Donorsite and treatment and timestep
DonorLitter<- Litter_clean%>%
  group_by(DonorSite, T_level, P_level, Treatment, Timestep, Time) %>%
  summarise(N.sample =length(W_loss), m.Loss = mean(W_loss), sd.Loss = sd(W_loss), se.Loss   = sd.Loss / sqrt(N.sample), m.remain = mean(W_remain), sd.remain = sd(W_remain), se.remain  = sd.remain / sqrt(N.sample))

#calculate mean litter loss of different litter at Burial site and treatment and timestep
BurialLitter<- Litter_clean%>%
  group_by(BurialSite, T_level, P_level, Treatment, Timestep, Time) %>%
  summarise(N.sample =length(W_loss), m.Loss = mean(W_loss), sd.Loss = sd(W_loss), se.Loss   = sd.Loss / sqrt(N.sample), m.remain = mean(W_remain), sd.remain = sd(W_remain), se.remain  = sd.remain / sqrt(N.sample))


#change order of sites in dataframe
DonorLitter$DonorSite <- factor(DonorLitter$DonorSite, levels = c("Fau","Vik","Arh","Ovs","Alr","Hog","Ram","Ves", "Ulv","Lav","Gud","Skj"))
#change order of sites in dataframe
BurialLitter$BurialSite<- factor(BurialLitter$BurialSite, levels=c("Fau","Vik","Arh","Ovs","Alr","Hog","Ram","Ves", "Ulv","Lav","Gud","Skj"))


####### Exploratory plots ################
# plots of %litter loss through time
#comparing same litter at different sites so treatment
ggplot(DonorLitter, aes(Time, col = factor(Treatment)))+
  geom_point(aes(y= m.Loss), size= 5, position = position_dodge(0.5))+
  geom_line(aes(y= m.Loss),size=1, position = position_dodge(0.5))+
  facet_wrap(~DonorSite)+
  ggtitle("%lost of DONORlitter (same litter/different site)")

# comparing different litter at same site
ggplot(BurialLitter, aes(Time, col = factor(Treatment)))+
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


ggplot(Litter_clean, aes(x= factor(Timestep), y= W_remain, col = factor(Treatment)))+
  geom_boxplot()+
  facet_wrap(~DonorSite)+
  ggtitle("%remain of DONORlitter (same litter/different site)")

ggplot(Litter_clean, aes(x= factor(Timestep), y= W_remain, col = factor(Treatment)))+
  geom_boxplot()+
  facet_wrap(~BurialSite)+
  ggtitle("%remain of DONORlitter (different litter/same site)")


library(ggthemes)
ggplot(BurialLitter, aes(Time, group = factor(Treatment)))+
  geom_errorbar(aes(ymin=m.remain-sd.remain, ymax=m.remain-sd.remain), width=.3, position = position_dodge(0.5)) +
  geom_point(aes(y= m.remain, fill= factor(Treatment) , shape = factor(Treatment)), size= 5, position = position_dodge(0.5))+
  geom_line(aes(y= m.remain, group= factor(Treatment), linetype= factor(Treatment)), size=1, position = position_dodge(0.5))+
  facet_wrap(~BurialSite)+
  labs(y= "  Litter remaining (%)", x = " Time (days) ")+
  scale_fill_manual(values =c("green", "red","blue", "purple"),
                    name="Treatment", 
                    breaks=c("1", "2", "3", "4"), 
                    labels = c("C", "WA", "WE", "WA+WE"),
                    guide= TRUE)+
  scale_colour_manual(values =c("#000000", "#000000", "#000000","#000000"),
                      name="Treatment", 
                      breaks=c("1", "2", "3", "4"), 
                      labels = c("C", "WA", "WE", "WA+WE"),
                      guide= TRUE)+
  scale_shape_manual(values = c(24, 21, 22, 25),
                      name="Treatment", 
                      breaks=c("1", "2", "3", "4"), 
                      labels = c("C", "WA", "WE", "WA+WE"),
guide= TRUE)+
  theme_few()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = "none", strip.text.x = element_blank())



  