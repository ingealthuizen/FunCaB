setwd("C:/Users/Tabea/Dropbox/BIO 299") #set working directiory, decides where files are stored or loaded from
library(readxl)
teabag<-read_excel('TeabagIndexcalc.xlsx')
head(teabag)
teabag<-teabag[, 1:24]
summary(teabag)
teabag
teabag<-teabag[!is.na(teabag$Site), ]
teabag$Reject[is.na(teabag$Reject)]<-""
teabag$Site<-factor(teabag$Site, levels = c("Fauske", "Alrust", "Ulvhaugen", "Vikesland", "Hogsete", "Lavisdalen", "Arhelleren", "Rambera", "Gudmedalen", "Ovstedalen", "Veskre", "Skjellingahaugen"))
boxplot(k~Site, data=teabag, las=2,par(mar=c(7,5,3,1)), subset = Reject!="x", ylab="Decomposition rate k" , cex.axis=0.9, col = c("green4", "darkorange2", "deepskyblue1"))
 #to make a large bottom margin. Alternatively, in the plot command, use the argument 

#produces boxplot for the data of teabag, output is k and input(x-axes) is set to sites, the 'las' turns the labeling
teabag$Site
which.max(teabag$k) 
rank(teabag$k)
order(teabag$k)
?subset
teabag$Reject
names(teabag)


setwd("C:/Users/Tabea/Desktop/ClimateData SeedClim")
Siteinfo<-read_excel('Site GPS reading.xlsx')  #import climat data to set up seedclim grid from 2009

Siteinfo$Humidity<-factor(Siteinfo$Humidity) # make the precipitation into a factor of 4 levels
Siteinfo$Treatment<-factor(Siteinfo$Treatment)

head(Siteinfo) # show first bit ut content

# merge two data frames by "Site" horizontally
alldata <- merge(teabag, Siteinfo,by="Site")

head(alldata)

#plot for alldata temperature against k minus the outliers
plot( k~`Temperature °C 1 Tetra Therm`, data=alldata, subset= Reject!="x",
      xlab= "Mean summer temperature [°C]",
      ylab="Decomposition rate k",
      bg=colours()[c(122,92,258)][Treatment.y],
      pch=c(24,21,22,25)[Humidity])

plot( k~`Precipitation 1 Mean Annual`, data=alldata, subset= Reject!="x",
      xlab= "Mean annual Precipitation [mm]",
      ylab="Decomposition rate k",
      bg=colours()[c(122,92,258)][Treatment.y],
      pch=c(24,21,22,25)[Humidity])

plot( S~`Temperature °C 1 Tetra Therm`, data=alldata, subset= Reject!="x",
      xlab= "Mean summer temperature [°C]",
      ylab="Stabilisation factor S",
      bg=colours()[c(122,92,258)][Treatment.y],
      pch=c(24,21,22,25)[Humidity])
colours()
plot( S~`Precipitation 1 Mean Annual`, data=alldata, subset= Reject!="x",
      xlab= "Mean annual Precipitation [mm]",
      ylab="Stabilisation factor S",
      bg=colours()[c(122,92,258)][Treatment.y],
      pch=c(24,21,22,25)[Humidity])

names(alldata)

#linear model for precipitation
# we see that with -8.12e-07 if we go up precipitation k value goes down, so it tells the wetter the less decomposition is going on
modprek<-lm( k~`Precipitation 1 Mean Annual`, data=alldata, subset= Reject!="x")
summary(modprek)

modtemp<-lm( k~`Temperature °C 1 Tetra Therm`, data=alldata, subset= Reject!="x")
summary(modtemp)

# linear model without ineraction of two predictors, significance shown
modtpk<-lm(k~`Temperature °C 1 Tetra Therm` + `Precipitation 1 Mean Annual`,data=alldata, subset= Reject!="x" )
summary(modtpk)


# linear model with interaction of two predictors, no significance shown
modtpik<-lm(k~`Temperature °C 1 Tetra Therm` * `Precipitation 1 Mean Annual`,data=alldata, subset= Reject!="x" )
summary(modtpik)

anova(modtpk,modtpik)

#run linear mixed models because I have clustered data
# mixmodt significant but wrong direction, the codlder the more k, and mixmodp was not significant but going in the right dirction.
library(lme4)  # load library

mixmodt<-lmer(k~`Temperature °C 1 Tetra Therm` + (1|Site), data=alldata, subset= Reject!="x")
summary(mixmodt)

mixmodp<-lmer(k~`Precipitation 1 Mean Annual` + (1|Site), data=alldata, subset= Reject!="x")
summary(mixmodp)

mixmod0<-lmer(k~1 + (1|Site), data=alldata, subset= Reject!="x")
summary(mixmod0)

#
anova(mixmod0, mixmodt)

#
anova(mixmod0, mixmodp)

mixmodtp<-lmer(k~`Temperature °C 1 Tetra Therm` + `Precipitation 1 Mean Annual` + (1|Site), data=alldata, subset= Reject!="x")
summary(mixmodtp)

# compare the two models
anova(mixmodtp, mixmodt)

plot(`Fraction decomposed green tea (ag)`~`Temperature °C 1 Tetra Therm`, data=alldata, subset= Reject!="x" & Reject!="h",
      xlab= "Mean summer temperature [°C]",
      ylab="Decay of green tea ",
      bg=colours()[c(122,92,258)][Treatment.y],
      pch=c(24,21,22,25)[Humidity])

plot( k~`Precipitation 1 Mean Annual`, data=alldata, subset= Reject!="x" & Reject!="h",
      xlab= "Mean annual Precipitation [mm]",
      ylab="Decomposition rate k",
      bg=colours()[c(122,92,258)][Treatment.y],
      pch=c(24,21,22,25)[Humidity])

