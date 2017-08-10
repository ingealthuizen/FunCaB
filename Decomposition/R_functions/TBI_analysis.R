#TBI analysis

# TBI exploring data 
source("O:\\FunCab\\Data\\FunCaB\\Other\\R_functions\\Highstat_library.R")

#check for outliers Y (k and S)
#par(mfrow = c(1, 2))
#boxplot(TBI_variables$k, 
#        main = "decomposition")
#dotchart(TBI_variables$lk, 
#         xlab = "Range of data", 
#         ylab = "Order of the data")

# possible outlier Vik k= 0.26 ,| TBI_variables$k>0.025



# calculate k based on mean S per site per year, first need to recalculate ar =predicted labile fraction
TBI_variables<-TBI_variables %>% 
                    group_by(year, site) %>% 
                    mutate(mean.S = mean(na.omit(S)))

TBI_variables<-TBI_variables %>% 
                  mutate(new.Ar = 0.552*(1-mean.S))
TBI_variables<-TBI_variables %>% 
                          mutate(new.k = log(new.Ar/(Wt-(1-new.Ar)))/Time)

# remove outliers (2 measurements) bigger than 0.20 + large difference with other k at same site same year
#TBI_variables<-TBI_variables[!(TBI_variables$S>0.6 |TBI_variables$S<0 |TBI_variables$k<=0 | TBI_variables$k>0.020 ),] 

#| TBI_variables$Time<61 to leave out 8 pairs of tea bags from LAV which were only incubated 60 days, however SKJ 2014 only 65 days

#change year to factor
TBI_variables$year<- as.factor(TBI_variables$year)

#remove rows with NA for k
TBI_variables<-TBI_variables[!is.na(TBI_variables$k),]

#change names of Temperature and precipitation levels
levels(TBI_variables$Temp.x) <- c("ALP", "SUB", "BOR")
levels(TBI_variables$Prec.x) <- c("Prec1", "Prec2", "Prec3", "Prec4")

TBI_variables2<- TBI_variables
levels(TBI_variables2$Prec.x) <- c("All","All","All","All" )
levels(TBI_variables2$Temp.x) <- c("All","All","All")


TBI_variables2x<-bind_rows(TBI_variables2, TBI_variables)
TBI_variables2x$Temp.x_f<- factor(TBI_variables2x$Temp.x, levels= c("All","ALP", "SUB", "BOR"))
TBI_variables2x$Prec.x_f<- factor(TBI_variables2x$Prec.x, levels= c("All","Prec1", "Prec2", "Prec3", "Prec4"))

levels(TBI_variables2x$Prec.x) <- c("Prec1", "Prec2", "Prec3", "Prec4")

#add 
TBI_variables2<-TBI_variables%>%
  mutate(allprec.x == "All")


# create subsets for different Precipitation levels of decompostion data
TBI_P1<- TBI_variables[(TBI_variables$Prec.x == "1"),]
TBI_P2<- TBI_variables[(TBI_variables$Prec.x == "2"),]
TBI_P3<- TBI_variables[(TBI_variables$Prec.x == "3"),]
TBI_P4<- TBI_variables[(TBI_variables$Prec.x == "4"),]


# create myVar for plotting with all variables
MyVar <- c("modelTemp", "gridPrec",  "pH", "AvailN", "Plant_CN", "Root", "soil_C.N", "Litter.CN", "soil_moist", "Bryo", "Gram", "Forbs", "Litter", "Live", "Total", "P_div", "M_Shannon.H")

# Check for outliers
Mydotplot(TBI_variables[, MyVar]) 

## Collinearity
pairs(TBI_variables[, MyVar], lower.panel = panel.cor)
# collinearity for biomass terms, pH and Shannon H

#cor.test(TBI_means$pH, TBI_means$soil_CN)


#conditional boxplot to check for collinearity between a continuous covariate and a categorical 
boxplot(modelTemp ~ factor(Temp.x), 
        data = TBI_variables)

boxplot(gridPrec ~ factor(Prec.x), 
        data = TBI_variables)

boxplot(modelTemp ~ factor(year), 
        data = TBI_variables)

boxplot(Temp.Var ~ factor(year), 
        data = TBI_variables)


# collinearity between year and modeltemp en logPrec and Prec.x

## Relationships Y vs X

# check for relationships
MyVar <- c("k", "S", "modelTemp", "gridPrec", "Temp.x", "Prec.x", "year", "pH", "AvailN", "soil_C.", "Litter.C", "Plant_CN", "Root", "soil_C.N", "Litter.CN", "soil_moist", "Total", "P_div", "M_Shannon.H")

pairs(TBI_variables[, MyVar], lower.panel = panel.cor)

# conditional boxplot to explore relation between categorical data 
boxplot(k ~ factor(Prec.x), 
        data = TBI_variables)
boxplot(k ~ factor(Temp.x), 
        data = TBI_variables)
boxplot(k ~ factor(year), 
        data = TBI_variables)


#Plot every continuous covariate versus Y
MyX  <- c("modelTemp", "gridPrec", "Temp.Var","Prec.CV", "pH", "AvailN", "Plant_CN", "soil_CN", "Litter.CN", "Total", "P_div", "M_Shannon.H")
Myxyplot(TBI_variables, MyX, "k", MyYlab = "Decomposition (k)")



#==============================================================================================================================

x<-TBI_variables%>%
  group_by( year)%>%
  summarize(mean.k= mean(k),mean.S= mean(S), mean.T = mean(modelTemp), mean.P = mean(gridPrec))

tempV<-c(3,3,3,3,2,2,2,2,1,1,1,1)
names(tempV)<-c("Ulv","Lav","Gud","Skj","Alr","Hog","Ram","Ves","Fau","Vik","Arh","Ovs")

x$templevel<-tempV[x$site]
precL<-c(1,2,3,4,1,2,3,4,1,2,3,4)
names(precL)<-c("Ulv","Lav","Gud","Skj","Alr","Hog","Ram","Ves","Fau","Vik","Arh","Ovs")
x$preclevel<-precL[x$site]

#calculate mean +Sd for temp and prec per year
TBI_climate_M<-TBI_variables%>%
  group_by(site,year) %>%
  summarise(m.T = mean(gridTemp), m.P = mean(gridPrec))

tempV<-c(3,3,3,3,2,2,2,2,1,1,1,1)
names(tempV)<-c("Ulv","Lav","Gud","Skj","Alr","Hog","Ram","Ves","Fau","Vik","Arh","Ovs")
#tempV[overviewsitesdata$site]
TBI_climate_M$templevel<-as.factor(tempV[TBI_climate_M$site])

precL<-c(1,2,3,4,1,2,3,4,1,2,3,4)
names(precL)<-c("Ulv","Lav","Gud","Skj","Alr","Hog","Ram","Ves","Fau","Vik","Arh","Ovs")
TBI_climate_M$preclevel<-as.factor(precL[TBI_climate_M$site])


####### Two-way ANOVA tests for difference in climate #################################
aov.T<- aov(m.T ~ factor(year)*templevel, data=TBI_grid) #
summary(aov.T)

# test the pairwise comparison between years for temperature
pairwise.t.test(TBI_grid$m.T, TBI_grid$year, p.adj = "none")
# test the pairwise comparison between templevel for temperature
pairwise.t.test(TBI_grid$m.T, TBI_grid$templevel, p.adj = "none")

aov.P<- aov(m.P ~ factor(year)*preclevel, data=TBI_grid)
summary(aov.P)

# test the pairwise comparison between years for temperature
pairwise.t.test(TBI_grid$m.P, TBI_grid$year, p.adj = "none")
# test the pairwise comparison between templevel for temperature
pairwise.t.test(TBI_grid$m.P, TBI_grid$preclevel, p.adj = "none")

####### Two-way ANOVA tests for difference in environment #################################
site_variables<-read.table("O:/FunCab/Data/FunCaB/Other/Data_Serge/Variable_soildata.txt", header= TRUE, dec= ",")
site_variables$Temp<- as.factor(site_variables$Temp)
site_variables$Prec<- as.factor(site_variables$Prec)
#boxplot(pH~ factor(Temp), data = site_variables)
#boxplot(pH~ factor(Prec), data = site_variables)
#boxplot(AvailN~ factor(Temp), data = site_variables)
#boxplot(AvailN~ factor(Prec), data = site_variables)
#boxplot(Plant_CN~ factor(Temp), data = site_variables)
#boxplot(Plant_CN~ factor(Prec), data = site_variables)
#boxplot(soil_C.N~ factor(Temp), data = site_variables)
#boxplot(soil_C.N~ factor(Prec), data = site_variables)

# pH
aov.PH <- aov(pH ~ Temp*Prec, data=site_variables)
summary(aov.PH)
#interaction**, Temp ***, Prec **
# test the pairwise comparison 
pairwise.t.test(site_variables$pH, site_variables$Temp, p.adj = "none")
pairwise.t.test(TBI_grid$m.P, TBI_grid$preclevel, p.adj = "none")

# Available N
#first exclude two extreme values for GUD, 1059.67 and 487.78
site_variables$AvailN[site_variables$AvailN > 480] <- NA

aov.N<-aov(AvailN ~ Temp*Prec, data=site_variables)
summary(aov.N)
#no interaction, Temp 0.07, Prec not sign

# Plant C:N ratio
aov.P_CN <- aov(Plant_CN ~ Temp*Prec, data=site_variables)
summary(aov.P_CN)
#interaction, Temp not sign, Prec ***
pairwise.t.test(site_variables$Plant_CN, site_variables$Prec, p.adj = "none")

# Soil C:N ratio
# omit rows with NA for C:N
site_variables<-read.table("O:/FunCab/Data/FunCaB/Other/Data_Serge/Variable_soildata.txt", header= TRUE, dec= ",")
site_variables_CN<-na.omit(site_variables)
aov.S_CN <- aov(soil_C.N ~ Temp*Prec, data=site_variables_CN)
summary(aov.S_CN)
#no interaction, Temp *, Prec ***
pairwise.t.test(site_variables_CN$soil_C.N, site_variables_CN$Temp, p.adj = "none")
pairwise.t.test(site_variables_CN$soil_C.N, site_variables_CN$Prec, p.adj = "none")


# Plant diversity
P.diversity_data<- read.table ("O:/FunCab/Data/FunCaB/Other/Vegetation/diversity.txt", header= TRUE)
P.diversity_data$site<- as.character(P.diversity_data$site)

tempV<-c(1,1,1,1,2,2,2,2,3,3,3,3)
names(tempV)<-c("Ulv","Lav","Gud","Skj","Alr","Hog","Ram","Ves","Fau","Vik","Arh","Ovs")
precL<-c(1,2,3,4,1,2,3,4,1,2,3,4)
names(precL)<-c("Ulv","Lav","Gud","Skj","Alr","Hog","Ram","Ves","Fau","Vik","Arh","Ovs")

P.diversity_data$Temp<-as.factor(tempV[P.diversity_data$site])
P.diversity_data$Prec<-as.factor(precL[P.diversity_data$site])

P.diversity_data<-P.diversity_data %>%
  filter(Year>2013) %>%
  group_by(site, turfID, Temp, Prec) %>%
  summarise(P_div = mean(diversity, na.rm =TRUE), P_even = mean(evenness, na.rm =TRUE)) 

aov.P_div <- aov(P_div ~ Temp*Prec, data=P.diversity_data)
summary(aov.P_div)
#interaction***, templevel***, preclevel*
pairwise.t.test(P.diversity_data$P_div, P.diversity_data$Temp, p.adj = "none")


# Litter C:N
Litter_data<- read_excel("O:/FunCab/Data/FunCaB/Decomposition/Data/Litter/Litter_CN2016.xlsx")

Litter_data<- Litter_data %>%
  group_by(site, sample)%>%
  summarise( Litter.C = mean(C), Litter.N = mean(N), Litter.CN = mean(CNratio))
Litter_data$templevel<-as.factor(tempV[Litter_data$site])
Litter_data$preclevel<-as.factor(precL[Litter_data$site])

#boxplot(Litter.CN~ factor(preclevel), data = Litter_data)
aov.L_CN <- aov(Litter.CN ~ templevel*preclevel, data=Litter_data)
summary(aov.L_CN )
#interaction Temp*** Prec***
pairwise.t.test(Litter_data$Litter.CN, Litter_data$templevel, p.adj = "none")
pairwise.t.test(Litter_data$Litter.CN, Litter_data$preclevel, p.adj = "none")

plot(site_variables$soil_C.N, site_variables$Plant_CN)


###### One-way ANOVA for testing difference in k and S across years###########
aov.k <- aov(new.k ~ factor(year), data=TBI_variables)
#plot(aov.k)
summary(aov.k)
TukeyHSD(aov.k)
# significant different k for years

aov.S <- aov(S ~ factor(year), data=TBI_variables)
#plot(aov.S)
summary(aov.S)
TukeyHSD(aov.S)
# significant different S for years

TBI_variables %>%
  group_by(year) %>%
  summarise(new.k = mean(new.k, na.rm =TRUE), S = mean(S, na.rm =TRUE))

###### Two-way ANOVA for testing differences in k and S across temp and prec levels for each year###########
# create subsets for different years of decompostion data
TBI_2014<- TBI_variables[(TBI_variables$year == 2014),]
TBI_2015<- TBI_variables[(TBI_variables$year == 2015),]
TBI_2016<- TBI_variables[(TBI_variables$year == 2016),]

aov.k <- aov(new.k ~ Prec.x*Temp.x, data=TBI_variables)
summary(aov.k)

##### linear regression for Green and Rooibos tea for different years

year_lm<-lm( k~ modelTemp, data= TBI_2015)
summary(year_lm) 

# k_T_2014 =  r2= 0.003, p=0.56
# k_T_2015 =  r2= 0.04, p<0.05
# k_T_2016 =  r2= 0.06, p=0.2
# k_P_2014 = r2= 0.008, p=0.34
# k_P_2015 = r2= 0.04, p<0.05
# k_P_2016 = r2= 0.05, p<0.05


# create subsets for different altitudes of decompostion data
TBI_ALP<- TBI_variables[(TBI_variables$Temp.x == "1"),]
TBI_SUB<- TBI_variables[(TBI_variables$Temp.x == "2"),]
TBI_BOR<- TBI_variables[(TBI_variables$Temp.x == "3"),]

# linear regression decomp rate k with temp
k_lm<-lm( new.k~ gridPrec+, data= TBI_BOR)
summary(k_lm)

#TBI_ALL T r2= 0.002 p=0.43
#TBI_ALP T r2= 0.003 p=0.07
#TBI_SUB T r2= 0.05 p<0.05
#TBI_BOR T r2= 0.08 p<0.01

#TBI_ALL P r2= 0.07 p<0.001
#TBI_ALP P r2 =0.12 p<0.001
#TBI_SUB P r2=0.13 p<0.001
#TBI_BOR P r2= 0.07 p<0.05

# Test for effect of Temperature level on relation with k
summary(lm(new.k~ gridTemp+Temp.x, data= TBI_variables))
summary(lm(new.k~ gridPrec+Temp.x, data= TBI_variables))

# linear regression Stabilisation with temp
S_lm<-lm( S~ gridPrec, data= TBI_variables)
summary(S_lm)

#TBI_ALL T r2= 0.03 p<0.01
#TBI_ALP T r2= 0.001 p=0.6
#TBI_SUB T r2= 0.07 p<0.01
#TBI_BOR T r2= 0.015 p<0.001

#TBI_ALL P r2= 0.15 p<0.001
#TBI_ALP P r2 =0.44 p<0.001
#TBI_SUB P r2=0.08  p<0.01
#TBI_BOR P r2= 0.02 p=0.13

# Test for effect of Temperature level on relation with k
summary(lm(S ~ gridTemp+Temp.x, data= TBI_variables))
summary(lm(S ~ gridPrec+Temp.x, data= TBI_variables))

summary(lm(k ~ gridTemp+gridPrec, data= TBI_variables))
summary(lm(S ~ gridTemp+gridPrec, data= TBI_variables))

ggplot(TBI_variables, aes(modelTemp, k, col= factor(Prec.x)))+
  geom_point()+
  geom_smooth(method = "lm")

########### Spatial climate effect - linear mixed-effects model ###################################################
library(nlme)
library(lme4)
hist(TBI_BOR$k) # check normal distribution k and S for total and different temp levels
TBI_variables$year<-as.factor(TBI_variables$year)

#year and site random, fixed temp and prec
M0<- lme(new.k~ gridTemp*gridPrec+factor(Temp.x)*factor(Prec.x), random= ~+1|year/site, data= TBI_variables, na.action=na.omit)
M1<- lme(new.k~ gridTemp+gridPrec+factor(Temp.x)*factor(Prec.x), random= ~+1|year/site, data= TBI_variables, na.action=na.omit)
anova(M0)
anova(M1)# best model

# residuals check
plot(fitted(M1), resid(M1))
abline (0,0)

# response variable linear function of fitted value
plot(fitted(M1), na.omit(TBI_variables$new.k))

# residuals normally distributed
qqnorm(resid(M1))
qqline(resid(M1))

af<-anova(M1)
afss <- af$"Sum Sq"
print(cbind(af,PctExp=afss/sum(afss)*100))

plot(fitted(M4b), resid(M4b)) # residuals M2 off
abline(0,0)

plot(fitted(M2), TBI_BOR$k)
plot(fitted(M3b), TBI_SUB$k)
plot(fitted(M4b), TBI_ALP$k)

qqnorm(resid(M2))
qqline(resid(M2))

summary(T3)
af<-anova(T3)
afss <- af$"Sum Sq"
print(cbind(af,PctExp=afss/sum(afss)*100))

#S
S0<- lme(S~ gridTemp*gridPrec+factor(Temp.x)*factor(Prec.x), random= ~+1|year/site, data= TBI_variables, na.action=na.omit)
S1<- lme(S~ gridTemp*gridPrec+factor(Temp.x)+factor(Prec.x), random= ~+1|year/site, data= TBI_variables, na.action=na.omit)
anova(S0) #modeltemp p<0.001, gridprec p<0.001 + interaction p<0.001, best model
anova(S1) #modeltemp p<0.01, gridprec p<0.001 

# residuals check
plot(fitted(M1), resid(M1))
abline (0,0)

# response variable linear function of fitted value
plot(fitted(M1), na.omit(TBI_variables$S))

# residuals normally distributed
qqnorm(resid(M1))
qqline(resid(M1))


##### Mixid linear model including environmental variables for means
#year and site random, fixed temp and prec
MMfull<- lme(new.k~ factor(Temp.x)*factor(Prec.x), 
             random= ~+1|year/site, data= TBI_means, method = "REML")
anova(MMfull)
summary(MMfull)
drop1(MMfull, test = "F")
step(MMfull)
#+ Plant_CN + AvailN + soil_C.N + Litter.CN +factor(Temp.x)




#############################Multilinear Model########################################################################################

### Mean decomposition rates#######
TBI_means<- TBI_variables%>%
  group_by(year, site)%>%
  summarise_each(funs(mean(., na.rm =TRUE)))

#add columns with precipitation and temperature level
tempV<-c(3,3,3,3,2,2,2,2,1,1,1,1)
names(tempV)<-c("Ulv","Lav","Gud","Skj","Alr","Hog","Ram","Ves","Fau","Vik","Arh","Ovs")
#tempV[overviewsitesdata$site]
TBI_means$Temp.x<-tempV[TBI_means$site]

precL<-c(1,2,3,4,1,2,3,4,1,2,3,4)
names(precL)<-c("Ulv","Lav","Gud","Skj","Alr","Hog","Ram","Ves","Fau","Vik","Arh","Ovs")
TBI_means$Prec.x<-precL[TBI_means$site]


K_lm<-lm( k ~ pH + P_div  + soil_C.N  , data= TBI_means) #+ Litter.CN + factor(Temp.x) + factor(Prec.x)
summary(K_lm)
step(K_lm)

K_lm2<-lm( k ~  gridPrec + factor(year)  , data= TBI_means) #modelTemp 
summary(K_lm2)
step(K_lm2)

#Variance decomposition between years and sites
T1<- lm(new.k~ gridTemp + gridPrec, data = TBI_means)
summary(T1)
step(T1)

T2<- lm(new.k~ factor(Temp.x)*factor(Prec.x), data = TBI_means)
summary(T2)

T3<- lm(new.k~ factor(Temp.x)+ gridTemp + gridPrec, data = TBI_means)
summary(T3)

T4<- lm(new.k~ factor(Temp.x)*factor(Prec.x) + gridTemp + gridPrec, data = TBI_means)
summary(T4)

T5<- lm(new.k~ factor(Temp.x) + gridTemp + gridPrec + pH + P_div , data = TBI_means)
summary(T5)

T6<- lm(new.k~  gridTemp + gridPrec + pH + P_div , data = TBI_means)
summary(T6)
step(T2)

Tfull<- lm(new.k~ factor(Temp.x) + gridTemp + gridPrec + pH + P_div , data = TBI_means)
summary(Tfull)
step(Tfull)

Tred<- lm(new.k~ gridPrec + pH + P_div , data = TBI_means)
summary(Tred)
anova(Tred)


step(T5)
af<-anova(T1)
afss <- af$"Sum Sq"
print(cbind(af,PctExp=afss/sum(afss)*100))

T1<- lm(S~ gridTemp * gridPrec, data = TBI_means)
summary(T1)
step(T1)

T2<- lm(S~ factor(Temp.x)*factor(Prec.x), data = TBI_means)
summary(T2)

T3<- lm(S~ factor(Temp.x)+ gridTemp + gridPrec, data = TBI_means)
summary(T3)

T4<- lm(S~ factor(Temp.x)*factor(Prec.x) + gridTemp + gridPrec, data = TBI_means)
summary(T4)

T5<- lm(S~ factor(Temp.x) + factor(Prec.x) + gridTemp * gridPrec, data = TBI_means)
summary(T5)

T6<- lm(S~ factor(Temp.x) + gridTemp + gridPrec + pH + P_div + AvailN + soil_C. + Litter.CN, data = TBI_means)
summary(T6)
step(T6)

Tfull<- lm(S~ factor(Temp.x)*factor(Prec.x) + gridTemp + gridPrec  , data = TBI_means)
summary(Tfull)
step(Tfull)

af<-anova(T1)
afss <- af$"Sum Sq"
print(cbind(af,PctExp=afss/sum(afss)*100))

Tred<- lm(S~  factor(Temp.x)+ gridPrec + Litter.C , data = TBI_means)
summary(Tred)



#Full model with all temperature and Precipitation terms
M1 <- lm( k ~  factor(Temp.x) factor(Prec.x) , data = TBI_means) #+ soil_moist + pH  modelTemp + gridPrec +
summary(M1) 
drop1(M1, test = "F")
step(M1)

af<-anova(M1)
afss <- af$"Sum Sq"
print(cbind(af,PctExp=afss/sum(afss)*100))


###Model validation
# Check for homogeneity
E1 <- resid(M1)
F1 <- fitted(M1)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, v = 0, lty = 2)

#Influential observations
par(mfrow = c(1, 1))
plot(cooks.distance(M1), type = "h", ylim = c(0, 1))
abline(h = 1)

#Normality
E1 <- resid(M1)
hist(E1, breaks = 15)
#right skew of residuals 

#"Independence"
#Plot residuals vs each covariate in the model
#Plot residuals vs each covariate not in the model

TBI_means$E1 <- E1   #Put E2 inside the Birds object (which will give trouble if there are NAs)
MySel <- c("Prec.CV",  "AvailN_mg.g", "Plant_CN", "soil_CN", "Litter.CN", "Root", "Bryo", "Forbs", "Gram","Total", "P_div", "M_Shannon.H")

Myxyplot(TBI_variables, MySel, "E1", MyYlab = "Residuals")
# relation with plant diversity?

boxplot(E1~ factor(Temp.x), data= TBI_variables)
boxplot(E1~ factor(Prec.x), data= TBI_variables)
boxplot(E1~ factor(year), data= TBI_variables)
# no further relations with categorical variables

#Dataset with means per site per year
M2 <- lm( k ~  gridPrec +pH + P_div , data = TBI_means) #+ soil_moist + pH
summary(M2) 
drop1(M2, test = "F")
step(M2)


###Model validation
# Check for homogeneity
E2 <- resid(M2)
F2 <- fitted(M2)
plot(x = F2, 
     y = E2,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, v = 0, lty = 2)

#Influential observations
par(mfrow = c(1, 1))
plot(cooks.distance(M2), type = "h", ylim = c(0, 1))
abline(h = 1)

#Normality
E2 <- resid(M2)
hist(E2, breaks = 15)
#right skew of residuals 

#"Independence"
#Plot residuals vs each covariate in the model
#Plot residuals vs each covariate not in the model

TBI_2015$E2 <- E2   #Put E2 inside the Birds object (which will give trouble if there are NAs)
MySel <- c("Prec.CV",  "AvailN", "Plant_CN", "soil_CN", "Litter.CN", "Root", "Bryo", "Forbs", "Gram","Total", "P_div", "M_Shannon.H")

Myxyplot(TBI_2015, MySel, "E2", MyYlab = "Residuals")
# relation with plant diversity?

boxplot(E2~ factor(Temp.x), data= TBI_2015)
boxplot(E2~ factor(Prec.x), data= TBI_2015)
boxplot(E2~ factor(year), data= TBI_2015)
# no further relations with categorical variables


# create myVar for plotting with all variables
MyVar <- c("modelTemp", "gridPrec", "Temp.Var", "Prec.CV", "pH", "AvailN", "Plant_CN", "Root", "soil_CN", "Litter.CN", "soil_moist", "Bryo", "Gram", "Forbs", "Litter", "Live", "Total", "P_div", "M_Shannon.H")

# Check for outliers
Mydotplot(TBI_means[, MyVar]) 

## Collinearity
pairs(TBI_means[, MyVar], lower.panel = panel.cor)
# collinearity for biomass terms, pH and Microbial_Shannon H /soil_CN

boxplot(pH ~ factor(Temp), 
        data = site_variables)
cor.test(site_variables$pH, site_variables$Temp)
#correlation between pH and Temp.x r=-0.85 p<0.001

TBI_mean<-TBI_means %>%
  filter(year == 2014)
boxplot(P_div ~ factor(Temp.x), 
        data = TBI_mean)
cor.test(TBI_mean$P_div, TBI_mean$Temp.x)
#correlation between P_div and Temp.x r=0.62 p<0.05


## Relationships Y vs X
# check for relationships
MyVar <- c("k", "S", "modelTemp", "gridPrec", "Temp.Var", "Prec.CV", "pH", "AvailN", "Plant_CN", "Root", "soil_CN", "Litter.CN", "soil_moist", "Total", "P_div", "M_Shannon.H")

pairs(TBI_means[, MyVar], lower.panel = panel.cor)

####Full model on mean decomposition rates with all climatic variables
M_m1<-lm( k ~  gridPrec +  pH + P_div, data = TBI_means)
summary(M_m1) 
drop1(M_m1, test = "F")
step(M_m1) #adj R2 = 0.42, AIC=-483.12, p=0.012
af<-anova(M_m1)
afss <- af$"Sum Sq"
print(cbind(af,PctExp=afss/sum(afss)*100))



M_m2<- lm( k ~  gridTemp + gridPrec  + factor(Temp.x) + factor(Prec.x) + P_div +pH   , data = TBI_means) 
summary(M_m2) # you cannot make up which parameters should be kept in the model
drop1(M_m2, test = "F")
step(M_m2)
af<-anova(M_m2)
afss <- af$"Sum Sq"
print(cbind(af,PctExp=afss/sum(afss)*100))


anova(M_m2, M_m1)
# model M_m1 is better

#calculate how much variance each parameter explains
af <- anova(M_m1)
afss <- af$"Sum Sq"
print(cbind(af,PctExp=afss/sum(afss)*100))

#calculate how much variance each parameter explains
Var_lm<- lm( k ~ factor(Temp.x)* factor(Prec.x), data= TBI_means) #factor(Temp.x), factor(Prec.x), factor(year)
af <- anova(Var_lm)
afss <- af$"Sum Sq"
print(cbind(af,PctExp=afss/sum(afss)*100))

#22% by year, 3% by elevation, 8% by prec level, 37% by site


## Mixed effects model
library(lme4)
library(nlme)

MeM1<- gls(k ~ 1 + modelTemp + gridPrec + factor(Prec.x)* factor(Temp.x) ,
           method= "REML", data= TBI_variables)
MeM2<- lme(k ~ 1 + modelTemp + gridPrec + factor(Prec.x)* factor(Temp.x) , data= TBI_variables,
           random = ~1 | year, method= "REML")
MeM3<- lme(k ~ 1 + modelTemp + gridPrec + factor(Prec.x)* factor(Temp.x) , data= TBI_variables,
           random = ~1 | site, method= "REML")
MeM4<- lme(k ~ 1 + modelTemp + gridPrec + factor(Prec.x)* factor(Temp.x) , data= TBI_variables,
           random = ~1 + year | site, method= "REML")

MeM1<- gls(k ~ 1 + modelTemp + gridPrec + pH + P_div ,
           method= "REML", data= TBI_variables)
MeM2<- lme(k ~ 1 + modelTemp + gridPrec + pH + P_div  , data= TBI_variables,
           random = ~1 | year, method= "REML")
MeM3<- lme(k ~ 1 + modelTemp + gridPrec + pH + P_div  , data= TBI_variables,
           random = ~1 | site, method= "REML")
MeM4<- lme(k ~ 1 + modelTemp + gridPrec + pH + P_div  , data= TBI_variables,
           random = ~1 + year | site, method= "REML")


AIC(MeM1, MeM2, MeM3, MeM4)
anova( MeM1, MeM2, MeM3, MeM4)
summary(MeM2)


# calculate CV for GridTemp and GridPrec per site per year
CV<- function (mean, sd){
  (sd/mean)*100  }

# summary of TBI variables
TBI_variables %>%
  group_by(year)%>%
  summarise(mean.k= mean(k), sd.k = sd(k))

TBI_variables %>%
  group_by(year)%>%
  summarise(mean.T= mean(modelTemp, na.rm =TRUE),  sd.T= sd(modelTemp, na.rm =TRUE))

x<-TBI_variables %>%
  group_by(year, site)%>%
  summarise(mean.T= mean(modelTemp, na.rm =TRUE),  total.P= max(gridPrec, na.rm =TRUE))

TBI.oav <- aov(k ~ factor(year), data=TBI_variables)
plot(TBI.oav)
summary(TBI.oav)
TukeyHSD(TBI.oav)
hist(TBI_variables$k)

TBI.oav <- aov(k ~ modelTemp*Temp.x, data=TBI_variables)
TBI.oav <- aov(k ~ gridPrec*Prec.x, data=TBI_variables)


#======================================================================================================================================
# look at differences between elevation levels. 
Mybwplot(TBI_variables, MyVar, "Temp.x" )

site_variables<- TBI_variables %>%
                  group_by(Temp.x, Prec.x, site) %>%
                  summarise_each(funs(mean(., na.rm =TRUE)))

# lowest elevation has no data for Microbial diversity so change to NA
site_variables[c(8:12), c(43:47)] = NA

MyVar  <- c("pH", "NO3N", "NH4N", "AvailN", "Plant_CN", "Root", "SoilC", "SoilN", "soil_CN", "SoilD", "Litter.CN", "Bryo", "Gram", "Forbs", "Live", "P_div","P_even", "M_Shannon.H")


## Collinearity
pairs(TBI_variables[, MyVar], lower.panel = panel.cor)
# NH4N with available N, soilC with soilN, Bryo with Live, P_div with P_even, Mshannon with pH

# look at relations between elevation levels, after taking out variables wiht correlation
MyVar  <- c("Temp.x", "pH", "NO3N", "NH4N", "Plant_CN", "Root", "soil_CN", "SoilD", "Litter.CN", "Bryo", "Gram", "Forbs", "P_div", "M_Shannon.H")
pairs(TBI_variables[, MyVar], lower.panel = panel.cor)

Mybwplot(TBI_variables, MyVar, "Temp.x" )



## Multiple ANOVA for site variables between elevations
lapply(site_variables[,c("pH", "NO3N", "NH4N", "AvailN", "Plant_CN", "Root", "SoilC", "SoilN", "soil_CN", "SoilD", "Litter.CN", "Bryo", "Gram", "Forbs", "Total", "P_div","P_even", "M_Shannon.H")], function(x) anova(lm(x ~ site_variables$Temp.x)))

multi.tests <- function(fun = t.test, df, vars, group.var, ...) {
  sapply(simplify = FALSE,                                    # sapply(simplify=T) better, elements named
         vars,                                                # loop on vector of outcome variable names
         function(var) {
           formula <- as.formula(paste(var, "~", group.var))# create a formula with outcome and grouping var.
           fun(data = df, formula, ...)                     # perform test with a given fun, default t.test
         }
  )
}

par(mfrow=c(3,3))
multi.tests(fun = plot,
            df = site_variables,
            vars = c("pH", "NO3N", "NH4N", "AvailN", "Plant_CN", "Root", "SoilC", "SoilN", "soil_CN", "SoilD", "Litter.CN", "Bryo",                         "Gram", "Forbs", "Live", "P_div","P_even", "M_Shannon.H"),
            group.var = "Temp.x")

res.multi.t.tests<-multi.tests(fun = oneway.test,
              df = site_variables,
              vars = c("pH", "NO3N", "NH4N", "AvailN", "Plant_CN", "Root", "SoilC", "SoilN", "soil_CN", "SoilD", "Litter.CN", "Bryo",                         "Gram", "Forbs", "Live", "P_div","P_even", "M_Shannon.H"),
              group.var = "Temp.x",
              var.equal = TRUE)


## p-values can be extracted from the result object
data.frame(p.value = sapply(res.multi.t.tests, getElement, name = "p.value"))

# if take out extreme diversity of Ves then P_div also sign. 
site_variables$P_div[8]= NA 
`#somewhere microbe_diversity got set to NA