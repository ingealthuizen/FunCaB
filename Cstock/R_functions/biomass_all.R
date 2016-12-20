require(ggplot2)
require(plyr)
library(reshape)

Biomass<- read.table("O:\\FunCab\\Data\\Vegetation data\\biomass 2013-2015.txt", header = TRUE, dec =  ".")
names(Biomass)
str(Biomass)

Biomass.Mean<-ddply(Biomass, c("Year", "Site"), summarise,
                    N= sum(!is.na(Bryo)), Bryo.M= mean(Bryo, na.rm=TRUE), Bryo.sd= sd(Bryo, na.rm=TRUE),
                    Gram.M= mean(Gram*16, na.rm=TRUE), Gram.sd= sd(Gram, na.rm=TRUE),
                    Forb.M= mean(Forbs, na.rm=TRUE), Forb.sd= sd(Forbs, na.rm=TRUE),
                    Litter.M= mean(Litter, na.rm=TRUE), Litter.sd= sd(Litter, na.rm=TRUE),
                    Live.M= mean(Live, na.rm=TRUE), Live.sd= sd(Live, na.rm=TRUE),
                    Total.M= mean(Total, na.rm=TRUE), Total.sd= sd(Total, na.rm=TRUE))

Temp<-c(1,1,1,1,2,2,2,2,3,3,3,3)
names(Temp)<-c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS")
Biomass.Mean$Temp<-Temp[Biomass.Mean$Site]
Prec<-c(1,2,3,4,1,2,3,4,1,2,3,4)
names(Prec)<-c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS")
Biomass.Mean$Prec<-Prec[Biomass.Mean$Site]
# sometime wrong with assigning temp and prec level

write.table(Biomass.Mean, "O:\\FunCab\\Data\\Vegetation data\\biomassMean.txt", sep="\t")

p<- ggplot(Biomass.Mean, aes(factor(Year), Total.M, fill= factor(Site))) 
p+geom_boxplot()