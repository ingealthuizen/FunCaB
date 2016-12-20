#setwd("O:\\FunCab\\Data\\Rscript")
source("O:\\FunCab\\Data\\FunCaB Rscript\\HighstatLibV10.R")
require(readxl) #require packages
require(ggplot2)
require(graphics)
require(stats)
library(plyr)
library(lattice)
require(dplyr)

TBI<-read_excel("O:\\FunCab\\Data\\Decomposition\\TBI\\TBI_141516.xlsx")
names(TBI)
str(TBI)

#change particular columns to factors 
TBI$site<- as.factor(TBI$site)
TBI$year<- as.factor(TBI$year)
TBI$Temp<- as.factor(TBI$Temp)
TBI$Prec<- as.factor(TBI$Prec)
#rounding of numeric data on 2 decimals
is.num <- sapply(TBI, is.numeric)
TBI[is.num] <- lapply(TBI[is.num], round, 3)

#Ag = fraction decomposed green tea, Ar = Predicted labile fraction red tea, Wt = Fraction remaining red tea
# new column for fraction decomposed red tea
TBI$decomp.R<- 1-TBI$Wt

#exclode rows with NA values
TBI<- na.exclude(TBI)
TBI

#check for outliers
MyVar <- c("S", "k", "Temp", "Prec", "year")
Mydotplot(TBI[, MyVar])

# outlier S>0.6 and S<0
pairs(TBI[,MyVar], lower.panel = panel.cor)
# outlier S>0.6 and S<0

I<- TBI$S>0.6 | TBI$S<0 | TBI$k<0 | TBI$k>0.05 # locate outliers k>0.05
I

TBI2 <- TBI[c(-140, -151, -154, -194, -328), ] # taking out ouliers S>0.6 and S<0
Mydotplot(TBI2[, MyVar])

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
##//////////////////////////////////////////////////////////////////////////////////////////////////////////////
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
ggplot(TBI2, aes(factor(Temp), k, col=factor(Temp)))+
  geom_boxplot()+
  facet_grid(. ~ Year)

ggplot(TBI2, aes(factor(Prec), k, col=factor(Prec)))+
  geom_boxplot()+
  facet_grid(. ~ Year)

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

ggplot(TBI1415, aes(factor(Prec), k, col=factor(Year)))+
  geom_boxplot()

TBI2014<- subset(TBI1415, Year== "2014")
TBI2015<- subset(TBI1415, Year== "2015")

 
ggplot(TBI2014, aes(factor(Temp), k, col=factor(Prec)))+
  geom_boxplot()+
  labs(title= "2014", x= "Temperature[?C]", y = "Decomposition rate [k]", col="Precipitation")
  
ggplot(TBI2015, aes(factor(Temp), k, col=factor(Prec)))+
  geom_boxplot()
  labs(title= "2015", x= "Temperature[?C]", y = "Decomposition rate [k]", col="Precipitation")

ggplot(TBI2014, aes(factor(Prec), k, col=factor(Temp)))+
  geom_boxplot()
  labs(title= "2014", x= "Precipitation", y = "Decomposition rate [k]", col="Temperature[?C]")

ggplot(TBI2015, aes(factor(Prec), k, col=factor(Temp)))+
  geom_boxplot()
  labs(title= "2015", x= "Precipitation", y = "Decomposition rate [k]", col="Temperature[?C]")


