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

TBI<-read_excel("O:\\FunCab\\Data\\Decomposition\\TBI\\TBI_141516new.xlsx")


#change particular columns to factors 
TBI$site<- as.factor(TBI$site)
TBI$year<- as.factor(TBI$year)
TBI$Temp<- as.factor(TBI$Temp)
TBI$Prec<- as.factor(TBI$Prec)
TBI$BurialDate<-TBI$`BurialDate `
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

#calculate mean values for all variables per site excluding NA's
site_variables<-site_variables %>%
                          group_by(site) %>%
                          summarise_each(funs(mean(., na.rm =TRUE))) 

# combine data mean.site.variables with TBI based on site
TBI_variables<-right_join(TBI, site_variables, by= "site")

# load and read litter CN data
Litter_data<- read_excel("O:/FunCab/Data/FunCaB/Decomposition/Data/Litter/Litter_CN2016.xlsx")
Litter_data<- Litter_data %>%
              group_by(site) %>%
              summarise( Litter.C = mean(C), Litter.N = mean(N), Litter.CN = mean(CNratio))

# combine data mean.site.variables with TBI based on site
TBI_variables<-left_join(TBI_variables, Litter_data, by= "site")


# load soil moisture data 2014-2016
soilmoisture<-read_excel("O:/FunCab/Data/FunCaB/Climate/Data/soil_moisture_141516.xlsx")
soilmoisture$year<- as.factor(soilmoisture$year)
soilmoisture$site<- as.factor(soilmoisture$site)
soilmoisture<- soilmoisture %>%
                      group_by(site, year) %>%
                      summarise(soil_moist = mean(mean_moist, na.rm =TRUE))

# combine data mean.site.moisture with TBI based on site
TBI_variables<-full_join(TBI_variables, soilmoisture, by= c("site" = "site", "year" = "year" ))


# load vegetation biomass data 2014-2016 and add as variables to TBI_variables
biomass_data<- read_excel("O:/FunCab/Data/FunCaB/Other/Vegetation/biomass_1415.xlsx")
biomass_data$Year<- as.factor(biomass_data$Year)
biomass_data$Site<- as.factor(biomass_data$Site)

biomass_data<-biomass_data %>%
                    group_by(Site, Year) %>%
                    summarise_each(funs(mean(., na.rm =TRUE))) 
                    

TBI_variables<-left_join(TBI_variables, biomass_data, by= c("site" = "Site", "year" = "Year" ))


# load vegetation diversity data and subset years 2015-2016 
P.diversity_data<- read.table ("O:/FunCab/Data/FunCaB/Other/Vegetation/diversity.txt", header= TRUE)

#rename siteID to match TBI_variables
newnames<-c("Alr","Arh", "Fau", "Gud", "Hog","Lav", "Ovs", "Ram", "Skj", "Ulv", "Ves", "Vik")
names(newnames)<-c("Alrust","Arhelleren","Fauske","Gudmedelen","Hogsete","Lavisdalen", "Ovstedal","Rambera","Skjellingahaugen","Ulvhaugen","Veskre","Vikesland")
P.diversity_data$site<-newnames[P.diversity_data$siteID]

#Select diversity data from 2015 and 2016
P.diversity_data<- subset(P.diversity_data, Year>=2014)
P.diversity_data$Year<- as.factor(P.diversity_data$Year)

# calculate mean richness and diversity and add as variables to TBI_variables
P.diversity_data<-P.diversity_data %>%
                      group_by(site) %>%
                      summarise(P_div = mean(diversity, na.rm =TRUE), P_even = mean(evenness, na.rm =TRUE)) 


TBI_variables<-left_join(TBI_variables, P.diversity_data, by= c("site" = "site"))

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
TBI_variables<- TBI_variables[ -c(1, 7:10, 12:15, 22, 28, 30:34, 50)]
TBI_variables$year<- as.numeric(TBI_variables$year)

#create new variable rain factor (RF), Lang et al 1976 Water and Plant Life. Springer: Berlin, Heidelberg, New York 
#the ratio between mean precipitation and mean temperature, or RF = P · T−1

#change order of sites in dataframe
TBI_variables$site <- factor(TBI_variables$site, levels = c("Fau","Vik","Arh","Ovs","Alr","Hog","Ram","Ves", "Ulv","Lav","Gud",                                                              "Skj"))

#write.table(TBI_variables, file = "O:\\FunCab\\Data\\Decomposition\\TBI\\TBI_variables.csv")

#==============================================================================================================================



ggplot(plotDF, aes(x=value, y=Resid, col=Temp.x)) + 
  geom_point(shape= 1)+      
  geom_hline(yintercept = 0)+
  facet_wrap(~ variable, scales = "free_x")+
  scale_color_discrete(name= "elevation", labels = c("alp", "sub", "bor"))+
    theme_bw()+
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 15), legend.title=element_text(size=14),             legend.text=element_text(size=12))
  







## first run TBI_climate.R to get TBI.meanTemp!!!
#add temperature data to TBI data
TBI3<-merge(TBI2, TBI.meanTempstation.200, by = c("site","year"))

MyVar <- c("S", "k", "Temp", "Prec", "year", "mean.temp")
pairs(TBI3[, MyVar], lower.panel = panel.cor)
#correlation between year and temp!

#calculate mean and sd per Year at each site
x<-aggregate(TBI2[, 21], list(TBI2$site, TBI2$year), mean)
y<-aggregate(TBI2[, 21], list(TBI2$site, TBI2$year), sd)
TBI.overview <-merge(x, y, by=c("Group.1", "Group.2"))
TBI.overview <- setNames(TBI.overview, c("site","year","mean.k", "sd.k"))
TBI.overview

TBI.overview<-  merge(TBI.overview, TBI.meanTempstation, by = c("site","year"))

##/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
ggplot(TBI.overview, aes(mean.temp, mean.k, col=factor(site)))+
  geom_point()+
  #geom_text(label= factor(site))+
  facet_grid(.~year)
#ULV is missing from climate data!!

#check whether temperature difference between sites is consistent
ggplot(TBI3, aes(mean.temp, k, col=factor(site)))+
  geom_jitter()+
  facet_grid(.~year)
#Hogsete not in right place of climate grid!!!

#Collinearity
pairs(TBI3[,MyVar], 
      lower.panel = panel.cor) # K and S but k is dirived from S

# Relationships 
ggplot(TBI2, aes(S, k, col=factor(Year)))+
  geom_point(size= 3)

# Relationships 
ggplot(TBI2, aes(S, k, col=factor(Location)))+
  geom_point(size= 3)
  
#relation k with Temp&Prec for all years together
ggplot(TBI2, aes(factor(Temp), k, col=factor(Temp)))+
  geom_boxplot()+
  facet_grid(. ~ Year)

ggplot(TBI2, aes(factor(Prec), k, col=factor(Prec)))+
  geom_boxplot()+
  facet_grid(. ~ Year)


#////////////////////////////////////////////////////////////////////////////////////////////////////////

# anova 
fit <- aov(k ~ year, data=TBI3)
plot(fit)
summary(fit)
TukeyHSD(fit)
# significant different k for years

fit <- aov(S ~ year, data=TBI3)
plot(fit)
summary(fit)
TukeyHSD(fit)
#S not different between 2014 and 2015, different 2014-2016, 2015-2016

fit <- aov(k ~ year + Temp * Prec, data=TBI3)
plot(fit)
summary(fit)
summary.lm(fit)
# ? difference between years bigger than difference between sites when prec & temp as.factor


fit <- aov(k ~ year + mean.temp + Prec, data=TBI2014)
plot(fit)
summary(fit)
# interaction between Temp and Prec
summary.lm(fit)

#effect of temp and prec for different years
#subset data for different years
TBI2014<-subset(TBI3,  year== "2014" )
TBI2015<-subset(TBI3,  year== "2015" )
TBI2016<-subset(TBI3,  year== "2016" )

fit <- aov(k ~ Temp*Prec, data=TBI2014)
summary(fit)
#No effect of Temp and Prec 

fit <- aov(k ~ Temp*Prec, data=TBI2015)
summary(fit)
summary.lm(fit)

fit <- aov(k ~ Temp*Prec, data=TBI2016)
summary(fit)
#Precipitation effect and interaction between Temp & Prec

#relation k with Temp&Prec against per year 
ggplot(TBI2016, aes(factor(Temp), k, col=factor(Temp)))+
  geom_boxplot()+
  facet_grid(. ~ Prec)

#relation k with Temp/Prec against per year 
ggplot(TBI2, aes(factor(Temp), k, col=Temp))+
  geom_boxplot()+
  facet_grid( ~ Year)

ggplot(TBI2, aes(factor(Prec), k, col=Prec))+
  geom_boxplot()+
  facet_grid( ~ Year)

ggplot(TBI2, aes(factor(Temp), k, col=factor(Prec)))+
  geom_boxplot()+
  labs(x= "Temperature[?C]", y = "Decomposition rate [k]", col="Prec")+
  scale_x_discrete(labels=c("7.5C","9.5C","11.5C"))+
  theme(axis.title.y = element_text(size = rel(2), angle = 90))+
  theme(axis.title.x = element_text(size = rel(2)))+
  theme(axis.text = element_text(size= rel(1.5), colour = "black"))+
  theme(legend.title= element_text(size= rel(1.5), colour = "black"))+
  theme(legend.text= element_text(size= rel(1.5), colour = "black"))+
  theme(legend.position = c(.9, .8))

ggplot(TBI2, aes(factor(Prec), k, col=factor(Year)))+
  geom_boxplot()+
  labs(x= "Prec[mm/year]", y = "Decomposition rate [k]", col="Year")+
  scale_x_discrete(labels=c("600", "1200", "2000", "2700"))+
  theme(axis.title.y = element_text(size = rel(2), angle = 90))+
  theme(axis.title.x = element_text(size = rel(2)))+
  theme(axis.text = element_text(size= rel(1.5), colour = "black"))+
  theme(legend.title= element_text(size= rel(1.5), colour = "black"))+
  theme(legend.text= element_text(size= rel(1.5), colour = "black"))+
  theme(legend.position = c(.9, .8))


List = by(TBI2014, TBI2014$Location,  function(x) length(unique(x$k)))
DataFrame = do.call(rbind, lapply(names(List), function(x) 
  data.frame(Location=x, k=List[[x]])))
DataFrame



ggplot(TBI2014, aes(factor(Temp), k, col=factor(Prec)))+
  geom_boxplot()+
  labs(title= "2014", x= "Temperature[?C]", y = "Decomposition rate [k]", col="Precipitation")

ggplot(TBI2014, aes(factor(Prec), k, col=factor(Temp)))+
  geom_boxplot()+
labs(title= "2014", x= "Precipitation", y = "Decomposition rate [k]", col="Temperature[?C]")


#linear model
M1<-lm(k~Temp, data = TBI3)
  summary(M1)
M2<-lm(k~Temp, data= TBI2)
  summary(M2)
M3<- lm(k~ Prec + Temp, data = TBI2)
summary (M3)
M4<- lm(k~Prec*Temp, data = TBI2)
drop1(M4, test = "F") 

#linear mixed models
library(lme4)  # load library

M5<-lmer(k~Temp + (1|Site), data=TBI2)
summary(M5)

M6<-lmer(k~Prec + (1|Site), data=TBI2)
summary(M6)

M0<-lmer(k~1 + (1|Site), data=TBI2)
summary(M0)

#
anova(M0, M5)
anova(M0, M6)

M7<-lmer(k~ Temp+ Prec + (1|Site), data=TBI2)
summary(M7)

# compare the two models
anova(M7, M5)

#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

TBI1415<-read_excel("\\\\eir.uib.no\\home6\\ial008\\FunCab\\Data\\TBI\\TBIcalc14-15.xlsx")
names(TBI1415)
str(TBI1415)

Temp<-c(1,1,1,1,2,2,2,2,3,3,3,3)
names(Temp)<-c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS")
TBI1415$Temp<-Temp[TBI1415$Site]

Prec<-c(1,2,3,4,1,2,3,4,1,2,3,4)
names(Prec)<-c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS")
TBI1415$Prec<-Prec[TBI1415$Site]  
head(TBI1415)

#check for outliers
MyVar <- c("S", "k", "Temp", "Prec")
Mydotplot(TBI1415[, MyVar])
# outlier S>0.6 and S<0

I<- TBI1415$S>0.6 | TBI1415$S<0 | TBI1415$k<0 # locate outliers 
I

TBI1415 <- TBI1415[c(-34,-45, -48, -66, -89), ] # taking out ouliers S>0.6 and S<0
Mydotplot(TBI1415[, MyVar])

colSums(is.na(TBI1415)) # check for NA's 
TBI1415<- na.exclude(TBI1415)

#Collinearity
pairs(TBI1415[,MyVar], 
      lower.panel = panel.cor) # K and S but k is dirived from S

# Relationships 
plot(x = TBI1415$S, 
     y = TBI1415$k,
     pch = 16)

ggplot(TBI1415, aes(S, k, col=factor(Year)))+
  geom_point()

ggplot(TBI1415, aes(factor(Temp), k, col=factor(Year)))+
  geom_boxplot()+
  labs(x= "Temperature[?C]", y = "Decomposition rate [k]", col="Year")+
  scale_x_discrete(labels=c("7.5?C","9.5?C","11.5?C"))+
  theme(axis.title.y = element_text(size = rel(2), angle = 90))+
  theme(axis.title.x = element_text(size = rel(2)))+
  theme(axis.text = element_text(size= rel(1.5), colour = "black"))+
  theme(legend.title= element_text(size= rel(1.5), colour = "black"))+
  theme(legend.text= element_text(size= rel(1.5), colour = "black"))+
  theme(legend.position = c(.9, .8))



