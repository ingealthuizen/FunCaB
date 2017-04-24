#source functions for import/proces and calculation of CO2 flux
source("CO2/R_functions/import_process_CO2data.R") 
source("CO2/R_functions/CO2flux_calculation.R")
source("CO2/R_functions/CO2_plot.R")
#give exact location of R functions.R file
#source("HighstatLibV10.R")

library(readxl) #require packages
library(ggplot2)
library(lattice)
library(dplyr)

# get specific functions from plyr by using plyr::ldply
#==============================================================================================================================
# importing single datafiles, process and set new start and end times. 
temp.data<-read.ibutton("CO2/Data/Temperature_files_2016/20160622_FAU_CH1_TEMP.txt")
log.data<-read.logger("CO2/Data/Fluxdata2016_Li1400/20160622_FAU_LI1400_CH1_1.txt")
meta.data<-read.metadata("CO2/Data/metadata_2016/22062016_FAU_ch1_1.txt")
combine.data<-process.data(meta=meta.data, logger=log.data, temp=temp.data)
setStartEndTimes(combine.data)


#x$dat$keep = TRUE for newly set start and endtimes FALSE for ommited measurement points

# fluxcalc should use data for $dat$keep ==TRUE




data2016 <- list(meta = meta.data, Tstart = tstart, Tfinish = tfinish, Temp=meantemp, PAR=  )

cbind(input$meta, PAR=PAR, temp=temp, nee=nee, rsqd=rsqd)

data2016[[1]] <- process.data(xlfile, 1)


#import all pre removal datafiles from 2015 and all datafiles of 2016
sites.data.2015<-read.sitefiles("CO2/Data/data_files_2015_pre.xlsx")
sites.data.2016<-read.sitefiles("CO2/Data/data_files_2016new.xlsx") #!Only Li1400 data, not SQ files

# Run fluxcalculation on all datafiles of 2015 
#fluxcalc(sites.data.2015[[1]]) #calculate flux 1 plot
overviewsitesdata_2015<-do.call(rbind, lapply(sites.data.2015, fluxcalc)) #calculate flux for all pre-removal data 2015

overviewsitesdata_2016<-do.call(rbind, lapply(sites.data.2016, fluxcalc)) #calculate flux for all data 2016

# add column were cover S1 and S2 are renamed L
overviewsitesdata_2016$lightlevel<-overviewsitesdata_2016$cover
overviewsitesdata_2016$cover<- as.factor(overviewsitesdata_2016$cover)
levels(overviewsitesdata_2016$cover)<- c("D", "L", "L", "L")


#explore datapoints graphs
#allNEEtemp<-ggplot(overviewsitesdata, aes(temp, nee, color=site))+
#geom_point(aes(fill=factor(site)), shape=1)+
#geom_smooth(aes(fill=factor(site)), method=lm)+
#scale_colour_hue(l=50)
#allNEEtemp  

#import and process datafiles 2016 !Note that these are only Li1400 data, not SQ files, those need to be ran seperately!
sites.data.2016<-read.sitefiles("CO2/Data/data_files_2016new.xlsx")


# GPP calculation
# create extra column with date only for calculating GPP= L(NEE)-D(Reco)
y<-format(overviewsitesdata_2015$starttime, format="%y-%m-%d")
  overviewsitesdata_2015$date<-y #make new date column

y<-format(overviewsitesdata_2015$starttime, format="%H")
  overviewsitesdata_2015$time<-y #make new date column  

#seperate L and D measurements and merge them in new file with new column GPP, selecting data with r2>=.9
  CO2_NEE_2015<-subset(overviewsitesdata_2015, cover== "L" & rsqd>=.9 )   
  CO2_RECO_2015<-subset(overviewsitesdata_2015, cover== "D" & rsqd>=.9 )   
  CO2_RECO_2015$Reco<-CO2_RECO_2015$nee*-1
  CO2_RECO_2015$tempK<-CO2_RECO_2015$temp+273.15
  CO2_GPP_2015<- merge(CO2_NEE_2015, CO2_RECO_2015, by=c("site", "block", "treatment", "date", "time"))
  CO2_GPP_2015$GPP<-CO2_GPP_2015$nee.x- CO2_GPP_2015$nee.y #NEE-Reco
  
#save CO2 flux data to csv file 
  #write.table(CO2_NEE_2015, file = "O:\\FunCab\\Data\\FunCaB\\CO2\\CO2_NEE_2015.csv")
  #write.table(CO2_RECO_2015, file = "O:\\FunCab\\Data\\FunCaB\\CO2\\CO2_RECO_2015.csv")
  
#make a data file of 2015 data including:
  #date, time, site, block, treatment, PAR(mean), Temp(mean), Moisture(average), Re, NEE, NEP, traits?, cover/biomass?
  CO2_GPP_2015<- CO2_GPP_2015[, c("site", "block", "treatment", "date", "time", "nee.x", "Reco", "GPP")]  
  #write.table(CO2_GPP_2015, file = "O:\\FunCab\\Data\\FunCaB\\CO2\\CO2_GPP_2015.csv")

# create extra column with date only for calculating GPP= L(NEE)-D(Reco)
  y<-format(overviewsitesdata_2016$starttime, format="%y-%m-%d")
  overviewsitesdata_2016$date<-y #make new date column
  
  y<-format(overviewsitesdata_2016$starttime, format="%H")
  overviewsitesdata_2016$time<-y #make new date column  
  
  
#seperate L and D measurements and merge them in new file with new column GPP, selecting data with r2>=.9
  CO2_NEE_2016<-subset(overviewsitesdata_2016, cover== "L" & rsqd>=.8 )   
  CO2_RECO_2016<-subset(overviewsitesdata_2016, cover== "D" & rsqd>=.8 )   
  CO2_RECO_2016$Reco<-CO2_RECO_2016$nee*-1
  CO2_RECO_2016$tempK<-CO2_RECO_2016$temp+273.15
  CO2_GPP_2016<- merge(CO2_NEE_2016, CO2_RECO_2016, by=c("site", "block", "treatment", "date", "time"))
  CO2_GPP_2016$GPP<-CO2_GPP_2016$nee.x- CO2_GPP_2016$nee.y #NEE-Reco
  
   
  
#add columns with precipitation and temperature level
  tempV<-c(1,1,1,1,2,2,2,2,3,3,3,3)
  names(tempV)<-c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS")
  #tempV[overviewsitesdata$site]
  overviewsitesdata_2015$templevel<-tempV[overviewsitesdata_2015$site]
  
  precL<-c(1,2,3,4,1,2,3,4,1,2,3,4)
  names(precL)<-c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS")
  overviewsitesdata_2015$preclevel<-precL[overviewsitesdata_2015$site]

#make a data file of 2015 data including:
#date, time, site, block, treatment, PAR(mean), Temp(mean), Moisture(average), Re, NEE, NEP, traits?, cover/biomass?
keep.columns <- c("y", "a")
  DF[keeps]
  
#plot overview of dark and light measurement data per site
  #fluxboxplot<- ggplot(overviewsitesdata, aes(site,nee))
  #fluxboxplot+geom_boxplot(aes(fill=factor(cover))) #plot of Reco and NEE per site
    
MyVar <- c("preclevel", "templevel" ,"temp", "PAR", "Reco")
  Mydotplot(SubsetD[,MyVar])
pairs(SubsetD[,MyVar], 
        lower.panel = panel.cor)

MyVar <- c("preclevel.x", "templevel.x" ,"tempK", "PAR.x", "Reco15", "GPPnew")
Mydotplot(MergeLD[,MyVar])
pairs(MergeLD[,MyVar], 
      lower.panel = panel.cor)

# Data spread per site 
xyplot(Reco ~ block | factor(site), data = MergeLD,
         xlab = "Temp",ylab = "Reco" )

xyplot(GPP ~ temp.x | factor(site), data = MergeLD,
       xlab = "Temp", ylab = "GPP")

xyplot(GPP ~ PAR.x  | factor(site), data = MergeLD,
        xlab = "PAR", ylab = "GPP")

q<- ggplot(MergeLD, aes(tempK, Reco, col=factor(templevel.x)))+
  geom_point(shape= 16, size= 3.5)+
  geom_smooth(method = "glm", se = FALSE)+
  labs(x= "Temperature (K)", y = "Reco (?mol/m^2/s)", col="Temperature")+
  scale_colour_manual(labels=c("7.5?C","9.5?C","11.5?C"), values=c("#56B4E9","#009E73","#FF6633"))+
  theme(axis.title.y = element_text(size = rel(2), angle = 90))+
  theme(axis.title.x = element_text(size = rel(2)))+
  theme(axis.text = element_text(size= rel(1.5), colour = "black"))+
  theme(legend.title= element_text(size= rel(1.5), colour = "black"))+
  theme(legend.text= element_text(size= rel(1.5), colour = "black"))+
  theme(legend.position = c(.9, .15))
q


q<- ggplot(MergeLD, aes(PAR.x, GPP, col=factor(templevel.x)))+
    geom_point(shape= 16, size= 3.5)+
    labs(x= "PAR (?mol/m^2/s)", y = "GPP (?mol/m^2/s)", col="Temperature")+
    scale_colour_manual(labels=c("7.5?C","9.5?C","11.5?C"), values=c("#56B4E9","#009E73","#FF6633"))+
    theme(axis.title.y = element_text(size = rel(2), angle = 90))+
    theme(axis.title.x = element_text(size = rel(2)))+
    theme(axis.text = element_text(size= rel(1.5), colour = "black"))+
    theme(legend.title= element_text(size= rel(1.5), colour = "black"))+
    theme(legend.text= element_text(size= rel(1.5), colour = "black"))+
    theme(legend.position = c(.9, .15))
q

xyplot(Reco ~ starttime.y | factor(site), data = MergeLD, na.rm=TRUE,
       xlab = "date",ylab = "Reco", scales = list(x   = list(relation = "free"), y   = list(relation = "same")))

xyplot(temp.x ~ starttime.y | factor(site), data = MergeLD, 
       xlab = "date",ylab = "TEMP", scales = list(x   = list(relation = "free"), y   = list(relation = "same")))

xyplot(GPP ~ starttime.x | factor(site), data = MergeLD,
       xlab = "date",ylab = "GPP", scales = list(x   = list(relation = "free"), y   = list(relation = "same")))


#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////# NORMALIZING FLUXDATA
#fit Arheniusequation from Loyd&Taylor 1994<- A*exp(E0/T-T0)
require(graphics)

fit.nls<-nls((Reco~A*exp(-308.56/I(tempK-227.13))), start=c(A=0 ), data=MergeLD)
  fit.nls #A=412.1  residual sum-of-squares: 5727
  summary(fit.nls)

E1 <- resid(fit.nls)   #better: E0 <- rstandard(M0)
F1 <- fitted(fit.nls)
  plot(x = F1,
       y = E1,
       xlab = "Fitted values",
       ylab = "Residuals",
       cex.lab = 1.5,
       col = as.numeric(MergeLD$preclevel.x),
       abline(h = 0, lty = 2))  

boxplot(E1 ~ site, data = MergeLD) # heterogeneity
#Is the ratio of the largest and smalled variance > 4
tapply(E1, INDEX = MergeLD$site, FUN = var)

Recofit<-plot(Reco~tempK, data = MergeLD)
  #lines(sort(tempK), fitted(fit.nls)[order(tempK)], col="red") #rough fit
  newx<-seq(min(MergeLD$tempK), max(MergeLD$tempK), length=100) #create new x values for fitting line
  Reco.pred<-predict(fit.nls, newdata=data.frame(tempK=newx)) #predict line from newx values 
  lines(newx, Reco.pred, col="blue")

  
#Recalculating flux values taking in account heteroscedasticity 
recalcflux<-MergeLD$Reco/fitted(fit.nls)*(coef(fit.nls)*exp(-308.56/I(283.15-227.13)))
MergeLD$Reco15<-recalcflux
plot(MergeLD$tempK, MergeLD$Reco15)
 
#equation from longdoz et al 2000 based on lloyd and taylor 1994  
#Ea<-12970*xtempK.9/(xtempK.9-227.13), Fs =Fs10 * exp {Ea *(Ts - 283.2)/(283.2 * R * Ts)}

fit.Reco.pre<-nls((Reco~A * exp (12970*tempK/(tempK-227.13) *(tempK - 283.2)/(283.2 * 8.314 * tempK))), start=c(A=0 ), data=SubsetD)
fit.Reco.pre

plot(Reco~tempK, data = SubsetD)
newx.new<-seq(min(SubsetD$tempK), max(SubsetD$tempK), length=100) #create new x values for fitting line
Reco.pred.new<-predict(fit.Reco.pre, newdata=data.frame(tempK=newx.new)) #predict line from newx values 
lines(newx.new, Reco.pred.new, col="red") 

# normalize Reco by dividing measured values by fitted values (measurement fitted all together/not site by site)
SubsetD$Reconorm<-SubsetD$Reco/fitted(fit.Reco.pre)
plot(SubsetD$Reconorm~SubsetD$tempK)

#splitting dataframe to per temperaturelevel and fitting equation to each Tlevel (alp, sub, low), plotting the fits
library(plyr)
site.norm.pre<-ddply(SubsetD, ~templevel, function(x){
  site.nls<-nls(Reco ~ A * exp (12970*tempK/(tempK-227.13) *(tempK - 283.2)/(283.2 * 8.314 * tempK)), start=c(A=0 ), data=x)
  x11() #plot fitted equation per site
  plot(Reco~tempK, data=x, main=x$templevel[1])
  newx<-seq(min(x$tempK), max(x$tempK), length=100) 
  Reco.pred.site<-predict(site.nls, newdata=data.frame(tempK=newx)) 
  lines(newx, Reco.pred.site, col="blue")
  x$RecoTlevel<-x$Reco/fitted(site.nls)*(coef(fit.nls)*exp(-308.56/I(283.15-227.13)))
  x
})
plot(site.norm.pre$RecoTlevel~site.norm.pre$tempK)

# multiple linear regressiion
M1Reco <- lm(Reco15 ~ factor(templevel.x) + factor(preclevel.x) + block + treatment,
           data = MergeLD)

summary(M1Reco)
step(M1Reco)

M2Reco<-lm(formula = Reco15 ~ factor(templevel.x) + factor(preclevel.x), data = MergeLD)

drop1(M2Reco, test = "F")

#Graphs
p<- ggplot(MergeLD, aes(templevel.x, Reco15, colour= factor(preclevel.x)))
  p+geom_jitter()
  
p<- ggplot(SubsetD, aes(preclevel, Reco15, colour = factor(templevel)))
  p+geom_jitter()
  
p<- ggplot(MergeLD, aes(factor(preclevel.x), Reco15, fill= factor(templevel.x)))+
  geom_boxplot()+
  scale_fill_manual(name="Temperature",labels=c("7.5?C","9.5?C","11.5?C"), values=c("#56B4E9","#009E73","#E69F00"))+
  scale_x_discrete(labels=c("600", "1200", "2000", "2700"))+
  labs(list(x="precipation (mm/y)", y="Reco at 15?C"))+
  theme(axis.title.y = element_text(size = rel(2.5), angle = 90))+
  theme(axis.title.x = element_text(size = rel(2.5)))+
  theme(axis.text = element_text(size= rel(1.5), colour = "black"))+
  theme(legend.title= element_text(size= rel(1.5), colour = "black"))+
  theme(legend.text= element_text(size= rel(1.5), colour = "black"))+
  theme(legend.position = c(.9, .8))
p  
 
p<- ggplot(MergeLD, aes(factor(templevel.x), Reco15)) 
  p+geom_boxplot()
  

  
# ====GPP===========================================================================================================  
  
# fit exponential curve to GPP~temp
# rectangular hyperbola Thornley and Johnson (1990)  GPP = (aGPP?GPPmax ?PAR)/( aGPP?PAR + GPPmax)
#aGPP is the initial slope of the rectangular hyperbola, also called the 'apparent quantum yield of GPP', and GPPmax is the asymptotic approach to a maximum GPP at high light intensity. 

p<- ggplot(MergeLD, aes(PAR.x, GPP, col=factor(templevel.x)))
  p+geom_point()
  # seems to be a difference in response to PAR between temp levels, or at least for ALP sites

fit.GPP<-nls((GPP~ (A*B*PAR.x)/(A*PAR.x+B)), start=c(A=0.001, B=2), data=MergeLD)
fit.GPP  #A 0.03755901  B 12.72741077

GPPfit<-plot(GPP~PAR.x, data = MergeLD)
  newPAR<-seq(min(MergeLD$PAR.x), max(MergeLD$PAR.x), length=100) #create new x values for fitting line
  GPP.pred<-predict(fit.GPP, newdata=data.frame(PAR.x=newPAR)) #predict line from newx values 
  lines(newPAR, GPP.pred, col="blue")

#Recalculating flux values taking in account heteroscedasticity 
recalcGPP<-MergeLD$GPP/fitted(fit.GPP)*(((coef(fit.GPP)[1])*(coef(fit.GPP)[2])*1500)/((coef(fit.GPP)[1])*1500+(coef(fit.GPP)[2])))
  MergeLD$GPPnew<-recalcGPP
  plot(MergeLD$temp.x, MergeLD$GPPnew)

q<- ggplot(MergeLD, aes(tempK, GPPnew, col=factor(templevel.x)))+
    geom_point(shape= 16, size= 3.5)+
    labs(x= "Temperature (K)", y = "GPP (?mol/m^2/s)", col="Temperature")+
    scale_colour_manual(labels=c("7.5?C","9.5?C","11.5?C"), values=c("#56B4E9","#009E73","#E69F00"))+
    theme(axis.title.y = element_text(size = rel(2), angle = 90))+
    theme(axis.title.x = element_text(size = rel(2)))+
    theme(axis.text = element_text(size= rel(1.5), colour = "black"))+
    theme(legend.title= element_text(size= rel(1.5), colour = "black"))+
    theme(legend.text= element_text(size= rel(1.5), colour = "black"))+
    theme(legend.position = c(.9, .15))
  q  
  
q<- ggplot(MergeLD, aes(factor(preclevel.x), GPPnew, fill= factor(templevel.x)))+ 
    geom_boxplot()+
    scale_fill_manual(name="Temperature",labels=c("7.5?C","9.5?C","11.5?C"), values=c("#56B4E9","#009E73","#E69F00"))+
    scale_x_discrete(labels=c("600", "1200", "2000", "2700"))+
    labs(list(x="precipation (mm/y)", y="GPP at PAR 1500 (?mol/m^2/s)"))+
    theme(axis.title.y = element_text(size = rel(2), angle = 90))+
    theme(axis.title.x = element_text(size = rel(2.5)))+
    theme(axis.text = element_text(size= rel(1.5), colour = "black"))+
    theme(legend.title= element_text(size= rel(1.5), colour = "black"))+
    theme(legend.text= element_text(size= rel(1.5), colour = "black"))+
    theme(legend.position = c(.9, .8))
q  
  
  
#splitting dataframe to per temperaturelevel and fitting equation to each Tlevel (alp, sub, low), plotting the fits
# probably not good to correct for Temperature on a categorical basis.
GPP.split<-ddply(MergeLD, ~templevel.x, function(x){
    GPP.nls<-nls((GPP~ (A*B*PAR.x)/(A*PAR.x+B)), start=c(A=0.001, B=2), data=x)
    print(coef(GPP.nls))
    A<-coef(fit.GPP)[1]
    B<-coef(fit.GPP)[2]
    x11() #plot fitted equation per site
    plot(GPP~PAR.x, data=x, main=x$templevel.x[1])
    newx<-seq(min(x$PAR.x), max(x$PAR.x), length=100) 
    GPP.pred.new<-predict(GPP.nls, newdata=data.frame(PAR.x=newx)) 
    lines(newx, GPP.pred.new, col="blue")
    x$GPPTlevel<-x$GPP/fitted(GPP.nls)*((A*B*1500)/(A*1500+B)) 
    x
  })
plot(GPP.split$GPPTlevel~GPP.split$temp.x)


q<- ggplot(GPP.split, aes(factor(preclevel.x), GPPTlevel, fill= factor(templevel.x)))+ 
  geom_boxplot()+
  scale_fill_manual(name="Temperature",labels=c("7.5?C","9.5?C","11.5?C"), values=c("#56B4E9","#009E73","#E69F00"))+
  scale_x_discrete(labels=c("600", "1200", "2000", "2700"))+
  labs(list(x="precipation (mm/y)", y="GPP at PAR 1500 (?mol/m^2/s)"))+
  theme(axis.title.y = element_text(size = rel(2), angle = 90))+
  theme(axis.title.x = element_text(size = rel(2.5)))+
  theme(axis.text = element_text(size= rel(1.5), colour = "black"))+
  theme(legend.title= element_text(size= rel(1.5), colour = "black"))+
  theme(legend.text= element_text(size= rel(1.5), colour = "black"))+
  theme(legend.position = c(.9, .8))
q  


q<- ggplot(GPP.split, aes(tempK, GPPTlevel, col=factor(templevel.x)))+
  geom_point(shape= 16, size= 3.5)+
  labs(x= "Temperature (K)", y = "GPP (?mol/m^2/s)", col="Temperature")+
  scale_colour_manual(labels=c("7.5?C","9.5?C","11.5?C"), values=c("#56B4E9","#009E73","#E69F00"))+
  theme(axis.title.y = element_text(size = rel(2), angle = 90))+
  theme(axis.title.x = element_text(size = rel(2)))+
  theme(axis.text = element_text(size= rel(1.5), colour = "black"))+
  theme(legend.title= element_text(size= rel(1.5), colour = "black"))+
  theme(legend.text= element_text(size= rel(1.5), colour = "black"))+
  theme(legend.position = c(.9, .15))
q  

p<- ggplot(GPP.split, aes(factor(preclevel.x), GPPTlevel, fill= factor(templevel.x))) 
  p+geom_boxplot()  


    
    
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    #ANOVA AND REGRESSION & MULTIVARIATE ANALYSIS
    
  #Regression, ANOVA and Multivariate analysis on temp and prec effect on Reco and GPP, NON-NORMALIZED DATA
    #linear regression
    
    tapply(MergeLD$Reco, list(MergeLD$templevel.x, MergeLD$preclevel.x), mean)
    tapply(SubsetD$Reco15, list(SubsetD$templevel, SubsetD$preclevel), mean)
    
    ANOVA.Reco1<-aov(Reco15 ~ factor(templevel),data = SubsetD)
    summary(ANOVA.Reco1)
    plot(ANOVA.Reco1)
    TukeyHSD(ANOVA.Reco1)
    
    ANOVA.Reco2<-aov(Reco15 ~ factor(preclevel),data = SubsetD)
    summary(ANOVA.Reco2)
    plot(ANOVA.Reco2)
    TukeyHSD(ANOVA.Reco2)
    
    ANOVA.Reco3<-aov(Reco15 ~ factor(templevel)* factor(preclevel),data = SubsetD)
    summary(ANOVA.Reco3)
    plot(ANOVA.Reco3)
    summary.lm(ANOVA.Reco3)
    TukeyHSD(ANOVA.Reco3)
    
    #linear Regression
    lm1<-lm(Reco15~templevel, data=SubsetD)
    anova(lm1)  
    plot(lm1)
    
    lm2<-lm(Reco15~preclevel, data=SubsetD)
    anova(lm2)  
    plot(lm2)
    
    # multiple regression
    lm3<- lm(Reco15~templevel*preclevel, data=SubsetD)
    anova(lm3)
    
    
    
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////#AFTER REMOVAL
#import datafiles after removal
read.sitefiles2<-function(file){
  require(readxl) #install package
  sites<-read_excel(file, sheet=1, col_names=TRUE, col_type= NULL) #read excel file
  sites$dates<- as.Date(sites$dates, format="%d.%m.%y") 
  sites<-sites[!is.na(sites$site), ] # remove rows with no data
  #import data from files of site.files
  sites.data<-lapply(1:nrow(sites), function(i){
  r<-sites[i, ]
  #print(r)
  import.everything(metaFile = r$meta, loggerFile = r$logger, tempFile = r$temp)
  }) #process data from all files
  unlist(sites.data, recursive = FALSE) # make on big list of data from all sites, without sublists
}
    
after.data2015<-read.sitefiles2("\\\\eir.uib.no\\home6\\ial008\\FunCab\\Data\\CO2 flux\\RcodeCO2\\sitesfiles after2015.xlsx")
    
  fluxcalc(after.data2015[[1]]) #calculate flux 1 plot
  after.2015<-do.call(rbind, lapply(after.data2015,fluxcalc)) #calculate flux all plots in all sites.
  
  

#add columns with precipitation and temperature level
  tempV<-c(1,1,1,1,2,2,2,2,3,3,3,3)
  names(tempV)<-c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS")
  #tempV[overviewsitesdata$site]
  after.2015$templevel<-tempV[after.2015$site]
  
  precL<-c(1,2,3,4,1,2,3,4,1,2,3,4)
  names(precL)<-c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS")
  after.2015$preclevel<-precL[after.2015$site]
  
#calculate GPP     
y<-format(after.2015$starttime, format="%y-%m-%d")
  after.2015$date<-y #make new date column
  
#seperate L and D measurements and merge them in new file with new column GPP
  subsetLafter<-subset(after.2015, cover== "L")    #& rsqd>=.8
  subsetDafter<-subset(after.2015, cover== "D" )   #& rsqd>=.8
  subsetDafter$Reco<-subsetDafter$nee*-1
  subsetDafter$tempK<-subsetDafter$temp+273.15
  mergeLDafter<- merge(subsetLafter, subsetDafter, by=c("site", "block", "treatment", "date"))
  mergeLDafter$GPP<-mergeLDafter$nee.x- mergeLDafter$nee.y #NEE-Reco

# boxplots for exploring spread of flux values within sites 
  fluxboxplot<- ggplot(mergeLD, aes(site,Reco))
  fluxboxplot+geom_boxplot()+
  scale_x_discrete(limits=c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS"))
  
  fluxboxplot<- ggplot(mergeLD, aes(site,GPP))
  fluxboxplot+geom_boxplot()+ 
    scale_x_discrete(limits=c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS"))  

#explore datapoints graphs to localize outliers 
#Recoafter<-ggplot(subsetDafter, aes(temp, Reco, label=site))+
#  geom_point(aes(fill=factor(site)), shape=1)+
#  geom_text(size=4)+
 # scale_colour_hue(l=50)
 # Recoafter    
    
  
# NORMALIZING FLUXDATA
fit.Reco.after<-nls((Reco~A * exp (12970*tempK/(tempK-227.13) *(tempK - 283.2)/(283.2 * 8.314 * tempK))), start=c(A=0 ), data=subsetDafter)
  fit.Reco.after
  
  plot(Reco~tempK, data = subsetDafter)
  newx.new<-seq(min(subsetDafter$tempK), max(subsetDafter$tempK), length=100) #create new x values for fitting line
  Reco.pred.after<-predict(fit.Reco.after, newdata=data.frame(tempK=newx.new)) #predict line from newx values 
  lines(newx.new, Reco.pred.after, col="red") 
  
# normalize Reco by dividing measured values by fitted values (measurement fitted all together/not site by site)
  subsetDafter$Reconorm<-subsetDafter$Reco/fitted(fit.Reco.after)
  plot(subsetDafter$Reconorm~subsetDafter$tempK)
  
  grid.prec.Reconorm<-ggplot(subsetDafter, aes(factor(preclevel), Reconorm))
  grid.prec.Reconorm+geom_boxplot(aes(fill=factor(templevel)))
  
  grid.temp.Reco<-ggplot(subsetDafter, aes(factor(templevel), Reconorm))
  grid.temp.Reco+geom_boxplot(aes(fill=factor(preclevel))) 

#splitting dataframe to per site and fitting equation to each site, plotting the fits
library(plyr)
after.Reconorm<-ddply(mergeLDafter, ~site, function(x){
    Reco.nls<-nls(Reco ~ A * exp (12970*tempK/(tempK-227.13) *(tempK - 283.2)/(283.2 * 8.314 * tempK)), start=c(A=0 ), data=x)
    x11() #plot fitted equation per site
    plot(Reco~tempK, data=x, main=x$site[1])
    newx<-seq(min(x$tempK), max(x$tempK), length=100) 
    Reco.pred.site<-predict(Reco.nls, newdata=data.frame(tempK=newx)) 
    lines(newx, Reco.pred.site, col="blue")
    x$Recosite<-x$Reco/fitted(Reco.nls)
    x
  })
plot(after.Reconorm$Recosite~after.Reconorm$tempK)
  
Reco.grid<-ggplot(after.Reconorm, aes(factor(site), Recosite))
Reco.grid+geom_boxplot(aes(fill=factor(treatment)))

 
    
  

  
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ 
#import all sitefiles of 2015 pre&after removal
  
#import all datafiles
read.allsitefiles<-function(file){
    require(readxl) #install package
    sites<-read_excel(file, sheet=1, col_names=TRUE, col_type= NULL) #read excel file
    sites$dates<- as.Date(sites$dates, format="%d.%m.%y") 
    sites<-sites[!is.na(sites$site), ] # remove rows with no data
    #import data from files of site.files
    sites.data<-lapply(1:nrow(sites), function(i){
      r<-sites[i, ]
      #   print(r)
      import.everything(metaFile = r$meta, loggerFile = r$logger, tempFile = r$temp)
    }) #process data from all files
    unlist(sites.data, recursive = FALSE) # make on big list of data from all sites, without sublists
}
  
data2015<-read.allsitefiles("\\\\eir.uib.no\\home6\\ial008\\FunCab\\Data\\CO2 flux\\RcodeCO2\\sitefiles2015.xlsx")
  
 fluxcalc(data2015[[1]]) #calculate flux 1 plot
  fluxdata2015<-do.call(rbind, lapply(data2015,fluxcalc)) #calculate flux all plots in all sites.
  
#add columns with precipitation and temperature level
  tempV<-c(1,1,1,1,2,2,2,2,3,3,3,3)
  names(tempV)<-c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS")
  #tempV[sitesdata2015$site]
  fluxdata2015$templevel<-tempV[fluxdata2015$site]
  
  precL<-c(1,2,3,4,1,2,3,4,1,2,3,4)
  names(precL)<-c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS")
  fluxdata2015$preclevel<-precL[fluxdata2015$site]
  
#GPP calc  
y<-format(fluxdata2015$starttime, format="%y-%m-%d")
  fluxdata2015$date<-y #make new date column  
  
  subsetL2015<-subset(fluxdata2015, cover== "L")   
  subsetD2015<-subset(fluxdata2015, cover== "D") 
  mergeLD2015<- merge(subsetL2015, subsetD2015, by=c("site", "block", "treatment", "date"))
  mergeLD2015$GPP<-mergeLD2015$nee.x- mergeLD2015$nee.y #NEE-Reco
  
