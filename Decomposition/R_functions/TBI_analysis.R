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

# remove outliers (8 measurements) / cut of at 0.20 (10 measurements)
TBI_variables<-TBI_variables[!(TBI_variables$S>0.6 |TBI_variables$S<0 |TBI_variables$k<=0 | TBI_variables$k>0.025),]



#remove rows with NA for k
TBI_variables<-TBI_variables[!is.na(TBI_variables$k),]


# create myVar for plotting with all variables
MyVar <- c("modelTemp", "gridPrec", "Temp.Var", "Prec.CV", "pH", "AvailN", "Plant_CN", "Root", "soil_CN", "Litter.CN", "soil_moist", "Bryo", "Gram", "Forbs", "Litter", "Live", "Total", "P_div", "M_Shannon.H")

# Check for outliers
Mydotplot(TBI_variables[, MyVar]) 

## Collinearity
pairs(TBI_variables[, MyVar], lower.panel = panel.cor)
# collinearity for biomass terms, pH and Shannon H


#conditional boxplot to check for collinearity between a continuous covariate and a categorical 
boxplot(modelTemp ~ factor(Temp.x), 
        data = TBI_variables)

boxplot(logPrec ~ factor(Prec.x), 
        data = TBI_variables)

boxplot(modelTemp ~ factor(year), 
        data = TBI_variables)

boxplot(Temp.Var ~ factor(year), 
        data = TBI_variables)

# collinearity between year and modeltemp en logPrec and Prec.x

## Relationships Y vs X

# check for relationships
MyVar <- c("k", "S", "modelTemp", "gridPrec", "Temp.Var", "Prec.CV","Temp.x", "Prec.x", "year",  "AvailN", "Plant_CN", "Root", "soil_CN", "Litter.CN", "soil_moist", "Total", "P_div", "M_Shannon.H")

pairs(TBI_variables[, MyVar], lower.panel = panel.cor)

# high pearson value for abund vs graze, use conditional boxplot to explore relation between categorical data (grazing and abundance)
boxplot(k ~ factor(Prec.x), 
        data = TBI_variables)
boxplot(k ~ factor(Temp.x), 
        data = TBI_variables)
boxplot(k ~ factor(year), 
        data = TBI_variables)


#Plot every continuous covariate versus Y
MyX  <- c("modelTemp", "gridPrec", "Temp.Var","Prec.CV", "AvailN", "Plant_CN", "soil_CN", "Litter.CN", "Total", "P_div", "M_Shannon.H")
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


# ANOVA 
aov.T<- aov(modelTemp ~ factor(year), data=TBI_variables)
plot(aov.T)
summary(aov.T)
TukeyHSD(aov.T)

aov.P<- aov(logPrec ~ factor(year), data=TBI_variables)
plot(aov.P)
summary(aov.P)
TukeyHSD(aov.P)


aov.k <- aov(k ~ factor(year), data=TBI_variables)
plot(aov.k)
summary(aov.k)
TukeyHSD(aov.k)
# significant different k for years

aov.Tea <- aov(k ~ Ag + decomp.R, data=TBI_variables)
plot(aov.k)
summary(aov.k)
TukeyHSD(aov.k)



#Multilinear Model
#Full model with all temperature and Precipitation terms
M1 <- lm( k ~ modelTemp +gridPrec + Temp.Var + Prec.CV + factor(Temp.x) + factor(Prec.x)+ factor(year), data = TBI_variables)

summary(M1) # you cannot make up which parameters should be kept in the model
drop1(M1, test = "F")
step(M1)

# Reduced model, dropping terms with collinearity
M1A<- lm( k ~ modelTemp + gridPrec + Temp.Var + Prec.CV + factor(year),  data = TBI_variables)
summary(M1A)
drop1(M1A, test = "F")
step(M1A)


M1B<- lm( k ~ modelTemp + gridPrec + Temp.Var + Prec.CV + factor(Temp.x), data = TBI_variables)
summary(M1B)
drop1(M1B, test = "F")
step(M1B)

#interaction between year and temp.level

M3 <- lm( k ~ modelTemp + gridPrec + factor(Temp.x), data = TBI_variables)
summary(M3)
drop1(M3, test = "F")
step(M3)

M4<- lm( k ~ modelTemp + gridPrec + factor(Temp.x) + Litter.CN + soil_CN , data = TBI_variables)
summary(M4)
drop1(M4, test = "F")
step(M4)



###Model validation
# Check for homogeneity
E3 <- resid(M3)
F3 <- fitted(M3)
plot(x = F3, 
     y = E3,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, v = 0, lty = 2)

#Influential observations
par(mfrow = c(1, 1))
plot(cooks.distance(M3), type = "h", ylim = c(0, 1))
abline(h = 1)

#Normality
E3 <- resid(M3)
hist(E3, breaks = 15)

#"Independence"
#Plot residuals vs each covariate in the model
#Plot residuals vs each covariate not in the model

TBI_variables$E3 <- E3   #Put E2 inside the Birds object (which will give trouble if there are NAs)
MySel <- c("Prec.CV",  "AvailN", "Plant_CN", "soil_CN", "Litter.CN", "Root", "Bryo", "Forbs", "Gram","Total", "P_div", "M_Shannon.H")

Myxyplot(TBI_variables, MySel, "E3", MyYlab = "Residuals")
# relation with plant diversity?

boxplot(E3~ factor(Temp.x), data= TBI_variables)
boxplot(E3~ factor(Prec.x), data= TBI_variables)
boxplot(E3~ factor(year), data= TBI_variables)
# no further relations with categorical variables

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

x<-TBI_variables %>%
  group_by(year, site)%>%
  summarise(mean.T= mean(gridTemp, na.rm =TRUE),  total.P= max(gridPrec, na.rm =TRUE))

TBI.oav <- aov(k ~ factor(year), data=TBI_variables)
plot(TBI.oav)
summary(TBI.oav)
TukeyHSD(TBI.oav)

TBI.oav <- aov(k ~ modelTemp*Temp.x, data=TBI_variables)
TBI.oav <- aov(k ~ gridPrec*Prec.x, data=TBI_variables)




ggplot(TBI_variables, aes(x=value, y=Resid, col=Temp.x)) + 
  geom_point(shape= 1)+      
  geom_hline(yintercept = 0)+
  facet_wrap(~ variable, scales = "free_x")+
  scale_color_discrete(name= "elevation", labels = c("alp", "sub", "bor"))+
  theme_bw()+
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 15), legend.title=element_text(size=14),             legend.text=element_text(size=12))

