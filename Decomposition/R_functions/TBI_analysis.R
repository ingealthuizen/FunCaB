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

# remove outliers (10 measurements) / cut off at 0.20 (10 measurements)
TBI_variables<-TBI_variables[!(TBI_variables$S>0.6 |TBI_variables$S<0 |TBI_variables$k<=0 | TBI_variables$k>0.020),]


#remove rows with NA for k
TBI_variables<-TBI_variables[!is.na(TBI_variables$k),]


# create subsets for different years of decompostion data
TBI_2014<- TBI_variables[(TBI_variables$year == 2014),]
TBI_2015<- TBI_variables[(TBI_variables$year == 2015),]
TBI_2016<- TBI_variables[(TBI_variables$year == 2016),]

# create subsets for different altitudes of decompostion data
TBI_ALP<- TBI_variables[(TBI_variables$Temp.x == "1"),]
TBI_SUB<- TBI_variables[(TBI_variables$Temp.x == "2"),]
TBI_BOR<- TBI_variables[(TBI_variables$Temp.x == "3"),]

# create subsets for different Precipitation levels of decompostion data
TBI_P1<- TBI_variables[(TBI_variables$Prec.x == "1"),]
TBI_P2<- TBI_variables[(TBI_variables$Prec.x == "2"),]
TBI_P3<- TBI_variables[(TBI_variables$Prec.x == "3"),]
TBI_P4<- TBI_variables[(TBI_variables$Prec.x == "4"),]


# create myVar for plotting with all variables
MyVar <- c("modelTemp", "gridPrec", "Temp.Var", "Prec.CV", "pH", "AvailN", "Plant_CN", "Root", "soil_CN", "Litter.CN", "soil_moist", "Bryo", "Gram", "Forbs", "Litter", "Live", "Total", "P_div", "M_Shannon.H")

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
MyVar <- c("k", "S", "modelTemp", "gridPrec", "Temp.Var", "Prec.CV","Temp.x", "Prec.x", "year", "pH", "AvailN", "Plant_CN", "Root", "soil_CN", "Litter.CN", "soil_moist", "Total", "P_div", "M_Shannon.H")

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
  group_by(site, year)%>%
  summarize(mean.k= mean(k), mean.T = mean(modelTemp), mean.P = mean(gridPrec))

tempV<-c(3,3,3,3,2,2,2,2,1,1,1,1)
names(tempV)<-c("Ulv","Lav","Gud","Skj","Alr","Hog","Ram","Ves","Fau","Vik","Arh","Ovs")

x$templevel<-tempV[x$site]
precL<-c(1,2,3,4,1,2,3,4,1,2,3,4)
names(precL)<-c("Ulv","Lav","Gud","Skj","Alr","Hog","Ram","Ves","Fau","Vik","Arh","Ovs")
x$preclevel<-precL[x$site]

ggplot(x, aes(mean.T, mean.k, col = factor(templevel)))+
  geom_point()+
  geom_smooth( method = "lm")

ggplot(x, aes(mean.P, mean.k, col = factor(templevel)))+
  geom_point()+
  geom_smooth( method = "lm")

TBI_grid<- TBI_variables%>%
            group_by(site, year) %>%
            summarise(m.T = mean(modelTemp), m.P = mean(gridPrec))

# ANOVA 
aov.T<- aov(m.T ~ factor(year), data=TBI_grid)
plot(aov.T)
summary(aov.T)
TukeyHSD(aov.T)

aov.P<- aov(m.P ~ factor(year), data=TBI_grid)
plot(aov.P)
summary(aov.P)
TukeyHSD(aov.P)

ggplot(TBI_grid, aes(factor(year), m.P))+
  geom_boxplot()

aov.k <- aov(k ~ factor(year), data=TBI_variables)
plot(aov.k)
summary(aov.k)
TukeyHSD(aov.k)
# significant different k for years

aov.2014T<- aov(k ~ factor(Temp.x), data=TBI_2016)
plot(aov.2014T)
summary(aov.2014T)
TukeyHSD(aov.2014T)

aov.2014P<- aov(k ~ factor(Prec.x), data=TBI_2016)
plot(aov.2014P)
summary(aov.2014P)
TukeyHSD(aov.2014P)

aov.Tea <- aov(k ~ Ag + decomp.R, data=TBI_variables)
plot(aov.k)
summary(aov.k)
TukeyHSD(aov.k)

##### linear regression for Green and Rooibos tea for different years
Ag_lm<-lm( decomp.R~ gridPrec, data= TBI_2016)
summary(Ag_lm) 
# Ag_T_2014 = 0.467 + 0.013 T, r2= 0.11, p<0.001
# Ag_T_2015 = 0.302 + 0.035 T, r2= 0.26, p<0.001
# Ag_T_2016 = 0.702 + -0.005 T, r2= 0.06, p=0.2
# Rb_T_2014 = 0.215 + 0.003 T, r2= 0.01, p=0.19
# Rb_T_2015 = 0.097 + 0.015 T, r2= 0.16, p<0.001
# Rb_T_2016 = 0.301 + -0.005 T, r2= 0.03, p=0.08

# Ag_T_2014 = r2= 0.02, p=0.12
# Ag_T_2015 = r2= 0.25, p<0.001
# Ag_T_2016 = r2= 0.13, p<0.001
# Rb_T_2014 = r2= 0.03, p=0.07
# Rb_T_2015 = r2= 0.06, p<0.01
# Rb_T_2016 = r2= 0.001, p=0.71

# linear regression decomp rate k with temp
k_lm<-lm( k~ modelTemp, data= TBI_BOR)
summary(k_lm)

#TBI_ALP T r2= 0.06 p<0.05
#TBI_SUB T r2= 0.04 p<0.05
#TBI_BOR T r2= 0.05 p<0.01

#TBI_ALL P r2= 0.06 p<0.001
#TBI_ALP P r2 =0.14 p<0.001
#TBI_SUB P r2=0.10 p<0.001
#TBI_BOR P r2= 0.04 p<0.05

ggplot(TBI_variables, aes(modelTemp, k, col= factor(Prec.x)))+
  geom_point()+
  geom_smooth(method = "lm")

#############################Multilinear Model########################################################################################
Ag_lm<-lm( decomp.R~ gridPrec, data= TBI_2016)
summary(Ag_lm)

#Complete data set
#Full model with all temperature and Precipitation terms
M1 <- lm( k ~ modelTemp + gridPrec + factor(Temp.x)* factor(Prec.x) , data = TBI_variables) #+ soil_moist + pH
summary(M1) 
drop1(M1, test = "F")
step(M1)

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

TBI_variables$E1 <- E1   #Put E2 inside the Birds object (which will give trouble if there are NAs)
MySel <- c("Prec.CV",  "AvailN", "Plant_CN", "soil_CN", "Litter.CN", "Root", "Bryo", "Forbs", "Gram","Total", "P_div", "M_Shannon.H")

Myxyplot(TBI_variables, MySel, "E1", MyYlab = "Residuals")
# relation with plant diversity?

boxplot(E1~ factor(Temp.x), data= TBI_variables)
boxplot(E1~ factor(Prec.x), data= TBI_variables)
boxplot(E1~ factor(year), data= TBI_variables)
# no further relations with categorical variables

#Dataset with means per site per year
M2 <- lm( k ~  gridPrec , data = TBI_means) #+ soil_moist + pH
summary(M2) 
drop1(M2, test = "F")
step(M2)
+pH + P_div

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


### Mean decomposition rates#######
TBI_means<- TBI_variables%>%
   group_by(year, site)%>%
   summarise_each(funs(mean(., na.rm =TRUE)))

#add columns with precipitation and temperature level
tempV<-c(1,1,1,1,2,2,2,2,3,3,3,3)
names(tempV)<-c("Ulv","Lav","Gud","Skj","Alr","Hog","Ram","Ves","Fau","Vik","Arh","Ovs")
#tempV[overviewsitesdata$site]
TBI_means$Temp.x<-tempV[TBI_means$site]

precL<-c(1,2,3,4,1,2,3,4,1,2,3,4)
names(precL)<-c("Ulv","Lav","Gud","Skj","Alr","Hog","Ram","Ves","Fau","Vik","Arh","Ovs")
TBI_means$Prec.x<-precL[TBI_means$site]

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
M_m1<-lm( k ~  gridPrec +  P_div +pH , data = TBI_means)
summary(M_m1) 
drop1(M_m1, test = "F")
step(M_m1) #adj R2 = 0.42, AIC=-472.65, p=0.012
anova(M_m1)

modelTemp +
M_m2<- lm( k ~  gridPrec + modelTemp + factor(Temp.x) + P_div +pH   , data = TBI_means) 
summary(M_m2) # you cannot make up which parameters should be kept in the model
drop1(M_m2, test = "F")
step(M_m2)

anova(M_m2, M_m1)
# model M_m1 is better

#Reduced dataframe 2015
#model including factors that possibly explain decomposition rate
M_m3<- lm(k ~ modelTemp + gridPrec +  pH + P_div , data = TBI_means2016)
summary(M_m3) 
drop1(M_m3, test = "F")
step(M_m3) #adj R2 = 0.49, AIC=-215.43, p=0.07

+ factor(Temp.x) + pH + P_div
anova(M_m1)

# relationship between k and reduced dataset with only 2015 and 2016
TBI_variables1516<- filter(TBI_variables, year>2014)

MyX  <- c("modelTemp", "gridPrec", "Prec2", "Temp.Var","Prec.CV", "AvailN", "Plant_CN", "soil_CN", "Litter.CN", "Total", "P_div", "M_Shannon.H")
Myxyplot(TBI_variables1516, MyX, "k", MyYlab = "Decomposition (k)")

M4<- lm( k ~ modelTemp + gridPrec + factor(Temp.x) , data = TBI_variables1516)
summary(M4)
drop1(M4, test = "F")
step(M4)

M5<- lm( k ~ modelTemp + gridPrec + factor(Temp.x) + , data = TBI_variables1516)
summary(M5)
drop1(M5, test = "F")
step(M5)

anova(M4, M5, test = "Chisq")

# Cannot compare models because M4 with P_div has NA for 2014, so less data then M3, so stick with M3

Mgt1 <- lm( Ag ~ modelTemp + gridPrec + factor(Temp.x) + pH + soil_moist, data = TBI_variables)
summary(Mgt1) # you cannot make up which parameters should be kept in the model
drop1(Mgt1, test = "F")
step(Mgt1)

Mrt1 <- lm( decomp.R ~ modelTemp + gridPrec + soil_moist, data = TBI_variables)
summary(Mrt1) # you cannot make up which parameters should be kept in the model
drop1(Mrt1, test = "F")
step(Mrt1)

Mrt2 <- lm( decomp.R ~ modelTemp + gridPrec , data = TBI_variables)
summary(Mrt2) # you cannot make up which parameters should be kept in the model
drop1(Mrt2, test = "F")
step(Mrt2)

anova(Mrt1, Mrt2)

#plot fitted values vs observed values
plot(x = F3, 
     y = TBI_variables$k,
     xlab = "Fitted values",
     ylab = "Observed data")
abline(coef = c(0, 1), lty = 2)


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
#somewhere microbe_diversity got set to NA