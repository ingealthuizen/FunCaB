source("O:\\FunCab\\Data\\FunCaB\\Other\\R_functions\\Highstat_library.R")
library(lme4)
library(lmerTest)
## Making subsets for comparison ##

com_CO2 <- CO2_mass_traits %>%
  select(Wmean_N, Wmean_CN, Wmean_LDMC, Wmean_Lth, Wmean_LA, Wmean_SLA, Wmean_logSLA, Wmean_Height,Wmean_logHeight, Reco, GPP, 
         G_c.biomass, F_c.biomass, B_c.biomass)

#### Checking for correlations ###
GGally::ggpairs(com_CO2)


### Biomass relations across grid
ggplot(data=biomass_XC_long)+
  geom_boxplot(aes(y=total.biomass, x=P_level))

ggplot(data=biomass_XC_long)+
  geom_boxplot(aes(y=total.biomass, x=T_level))
#total biomass increases with T_level

ggplot(data=biomass_XC_long)+
  geom_boxplot(aes(y=dry.weight, x=P_level, col= T_level))+
  facet_wrap(~functional.group)

ggplot(data=CO2_mass_traits, aes(y=G_c.biomass, x=P.mm, col= T_level))+
    geom_point()+
    geom_smooth(method= "lm")

ggplot(data=CO2_mass_traits, aes(y=F_c.biomass, x=P.mm, col= T_level))+
  geom_point()+
  geom_smooth(method= "lm")

ggplot(data=CO2_mass_traits, aes(y=B_c.biomass, x=P.mm, col= T_level))+
  geom_point()+
  geom_smooth(method= "lm")

ggplot(data=CO2_mass_traits, aes(y=Total_c.biomass, x=P.mm, col= T_level))+
  geom_point()+
  geom_smooth(method= "lm")

ggplot(data=CO2_mass_traits, aes(y=G_c.biomass, x=Temp.C, col= P_level))+
  geom_point()+
  geom_smooth(method= "lm")

ggplot(data=CO2_mass_traits, aes(y=F_c.biomass, x=Temp.C, col= P_level))+
  geom_point()+
  geom_smooth(method= "lm")

ggplot(data=CO2_mass_traits, aes(y=B_c.biomass, x=Temp.C, col= P_level))+
  geom_point()+
  geom_smooth(method= "lm")

ggplot(data=CO2_mass_traits, aes(y=Total_c.biomass, x=Temp.C, col= P_level))+
  geom_point()+
  geom_smooth(method= "lm")


### explore trait variation across grid
ggplot(data = CO2_mass_traits, aes(x=P_level, y = Wmean_CN))+
  geom_boxplot()+
  facet_wrap(~T_level)

ggplot(data = CO2_mass_traits, aes(x=P_level, y = Wmean_N))+
  geom_boxplot()+
  facet_wrap(~T_level)

ggplot(data = CO2_mass_traits, aes(x=P_level, y = Wmean_LDMC))+
  geom_boxplot()+
  facet_wrap(~T_level)

ggplot(data = CO2_mass_traits, aes(x=P_level, y = Wmean_Lth))+
  geom_boxplot()+
  facet_wrap(~T_level)

ggplot(data = CO2_mass_traits, aes(x=P_level, y = Wmean_LA))+
  geom_boxplot()+
  facet_wrap(~T_level)

ggplot(data = CO2_mass_traits, aes(x=P_level, y = Wmean_SLA))+
  geom_boxplot()+
  facet_wrap(~T_level)

ggplot(data = CO2_mass_traits, aes(x=P_level, y = Wmean_logHeight))+
  geom_boxplot()+
  facet_wrap(~T_level)

### GPP and RECO across grid
ggplot(data = CO2_mass_traits, aes(x=P_level, y = GPP, col=Year))+
  geom_boxplot()+
  facet_wrap(~T_level)

ggplot(data = CO2_mass_traits, aes(x=P_level, y = Reco, col=Year))+
  geom_boxplot()+
  facet_wrap(~T_level)

#### RECO vs T and GPP vs PAR relation across grid
ggplot(CO2_mass_traits, aes(x=tempK, y=Reco, col=factor(Site)))+
  geom_point(na.rm= TRUE)+
  geom_smooth(method = "nls", formula= y~A*exp(-308.56/I(x-227.13)), method.args = list(start=c(A=0)), se=FALSE, na.rm= TRUE)+
  #facet_grid(~T_level, scales= "free")+
  theme_bw()

ggplot(CO2_mass_traits, aes(x=PAR, y=GPP, col= factor(T_level)))+
  geom_point(na.rm= TRUE)+
  geom_smooth(method = "nls", formula= y~(A*B*x)/(A*x+B), method.args = list(start=c(A=0.01, B=2)), se=FALSE, na.rm= TRUE)+
  #facet_wrap(~P_level)+
  theme_bw()



#### Making models ####


CO2_mass_traits$Site<-as.character(CO2_mass_traits$Site)
CO2_mass_traits$turfID<-as.character(CO2_mass_traits$turfID)
# 64 rows missing Moisture data
# 837 rows missing soilT data

##### GPP ######
#### 1. abiotic factors
model_a0<- lmer(GPP ~ PAR +  T_level + P_level +(1|Site/turfID), data=CO2_mass_traits)
model_a1<- lmer(GPP ~ tempK + T_level + P_level +(1|Site/turfID), data=CO2_mass_traits)
model_a2<- lmer(GPP ~ Moisture + T_level + P_level +(1|Site/turfID), data=CO2_mass_traits)
model_a3<- lmer(GPP ~ soilT +T_level + P_level + (1|Site/turfID), data=CO2_mass_traits)
model_a4<- lmer(GPP ~ PAR +  T_level + (1|Site/turfID), data=CO2_mass_traits)
model_aFull<- lmer(GPP ~ PAR + T_level +  (1|Site/turfID), data=CO2_mass_traits)
AIC(model_a0, model_a1, model_a2, model_a3,  model_aFull)
summary(model_a0)
summary(model_aFull)
#anova(model_0)

#### 2. Biomass
model_b0<- lmer(GPP ~ Total_c.biomass +  T_level +  (1|Site/turfID), data=CO2_mass_traits)
model_b1<- lmer(GPP ~ G_c.biomass +(1|Site/turfID), data=CO2_mass_traits)
model_b2<- lmer(GPP ~ F_c.biomass +(1|Site/turfID), data=CO2_mass_traits)
model_b3<- lmer(GPP ~ VegetationHeight +(1|Site/turfID), data=CO2_mass_traits)
model_bFull<- lmer(GPP ~ G_c.biomass + F_c.biomass + B_c.biomass + VegetationHeight + (1|Site/turfID), data=CO2_mass_traits)
summary(model_b0)
summary(model_b1)
summary(model_b2)
summary(model_b3)
summary(model_bFull)

model_abFull<- lmer(GPP ~ PAR +  T_level + F_c.biomass + (1|Site/turfID), data=CO2_mass_traits)

#### 3. Plant traits
model_c0<- lmer(GPP ~ G_c.biomass + F_c.biomass + Wmean_Height + Wmean_SLA + Wmean_LA + Wmean_LDMC +(1|Site/turfID), data=CO2_mass_traits)
summary(model_c0)

model_c0<- lmer(GPP1200 ~ G_c.biomass + Wmean_Height + (1|Site/turfID), data=CO2_mass_traits)
summary(model_c0)

##### Reco ######
### 1. abiotic factors
model_a1<- lmer(Reco ~ tempK + T_level + P_level +(1|Site/turfID), data=CO2_mass_traits)
model_a2<- lmer(Reco ~ Moisture + T_level + P_level +(1|Site/turfID), data=CO2_mass_traits)
model_a3<- lmer(Reco ~ soilT +T_level + P_level + (1|Site/turfID), data=CO2_mass_traits) #missing 56 observations with soilT
model_a4<- lmer(Reco ~   T_level + (1|Site/turfID), data=CO2_mass_traits)
model_aFull<- lmer(Reco ~ tempK + Moisture +  T_level + P_level + (1|Site/turfID), data=CO2_mass_traits) 
summary(model_aFull)
levels(CO2_mass_traits$turfID)

#### 2. Biomass
model_abFull<- lmer(Reco ~ tempK + Moisture + VegetationHeight + (1|Site/turfID), data=CO2_mass_traits) 
summary(model_abFull)

#### 3. Plant traits
model_cFull<- lmer(Reco ~ tempK + Moisture + VegetationHeight + Wmean_LA +  (1|Site/turfID), data=CO2_mass_traits) 
summary(model_cFull)

########### model with only traits for non-standardized fluxes 
model_GPP_Full <- lmer(GPP ~  Wmean_N + Wmean_logHeight + (1|Site/turfID), data=CO2_mass_traits)
summary(model_GPP_Full)
drop1(model_GPP_Full)
# same traits left as with standardized fluxes

model_Reco_Full <- lmer(Reco ~ Wmean_logHeight + (1|Site/turfID), data=CO2_mass_traits)
summary(model_Reco_Full)
drop1(model_Reco_Full)
# different Trait remain than for standardized flux

########### model with only traits for recalculated fluxes GPP1200 and Reco15
model_GPP_Full <- lmer(GPP1200 ~  Wmean_N + Wmean_logHeight + (1|Site/turfID), data=CO2_mass_traits)
summary(model_GPP_Full)
drop1(model_GPP_Full)
#! These two traits remain after dropping least significant Wmean_trait one by one

model_Reco_Full <- lmer(Reco15 ~  Wmean_N + Wmean_C + (1|Site/turfID), data=CO2_mass_traits)
summary(model_Reco_Full)
drop1(model_Reco_Full)
#! These two traits remain after dropping least significant Wmean_trait one by one


### model for specific site



model_0 <- lmer(GPP1200 ~ Total_c.biomass + T_level + P_level + (1|Site/turfID), data=CO2_mass_traits)
#summary(model_0)
#anova(model_0)

model_CN <- lmer(GPP1200 ~ Total_c.biomass + T_level + P_level + Wmean_CN+ (1|Site/turfID), data=CO2_mass_traits)
#summary(model_CN)
#anova(model_CN)

model_LDMC <- lmer(GPP1200 ~ Total_c.biomass + T_level + P_level + Wmean_LDMC + (1|Site/turfID), data=CO2_mass_traits)
#summary(model_LDMC)
#anova(model_LDMC)

model_Lth <- lmer(GPP1200 ~ Total_c.biomass + T_level + P_level + Wmean_Lth + (1|Site/turfID), data=CO2_mass_traits)
#summary(model_Lth)
#anova(model_Lth)

model_LA <- lmer(GPP1200 ~ Total_c.biomass + T_level + P_level + Wmean_LA + (1|Site/turfID), data=CO2_mass_traits)
#summary(model_LA)
#anova(model_LA)

model_SLA <- lmer(GPP1200 ~ Total_c.biomass + T_level + P_level + Wmean_SLA + (1|Site/turfID), data=CO2_mass_traits)
#summary(model_SLA)
#anova(model_SLA)

model_Height <- lmer(GPP1200 ~ Total_c.biomass + T_level + P_level + Wmean_Height + (1|Site/turfID), data=CO2_mass_traits)
#summary(model_Height)
#anova(model_Height)

model_all_traits <- lmer(GPP1200 ~ Total_c.biomass + T_level + P_level + Wmean_CN + scale(Wmean_LDMC) + scale(Wmean_Lth) + Wmean_LA + Wmean_SLA + Wmean_Height + (1|Site/turfID), data=CO2_mass_traits)
#summary(model_all_traits)
#anova(model_all_traits)

AIC(model_0, model_CN, model_LDMC, model_Lth, model_LA, model_SLA, model_Height, model_all_traits)

#From the AIC it is the model with the CN that is the better model. INvestigating that further with the summery output:

summary(model_CN)

#According to Richard it would be interesting to change the temperature and precipitation levels to numerical instead of factors. It should be in the cleaning file so it is just to change the code so that you choose this collumn instead of the one with the categorical T_level and P_level.

anova(model_0, model_CN)
anova(model_0, model_LDMC)
anova(model_0, model_Lth)
anova(model_0, model_LA)
anova(model_0, model_SLA)
anova(model_0, model_Height)

# According to the anova when comparing the null model with the other models, it is only CN ratio that makes it a better model.

## Plotting the model to check for things ##

a <- plot(model_0)
b <-plot(model_CN)

gridExtra::grid.arrange(a,b)

qqnorm(residuals(model_0))
qqline(resid(model_0))

qqnorm(residuals(model_CN))
qqline(resid(model_CN))


#### Making graphs to visualize ####


ggplot(CO2_mass_traits, aes(x=PAR, y=GPP, col=T_level))+
  geom_point()+
  geom_smooth(method = "nls", formula= y~(A*B*x)/(A*x+B), method.args = list(start=c(A=0.01, B=2)), se=FALSE, na.rm= TRUE)

ggplot(CO2_mass_traits, aes(x=tempK, y=Reco, col=T_level))+
  geom_point()+
  geom_smooth(method = "nls", formula= y~A*exp(-308.56/I(x-227.13)), method.args = list(start=c(A=0)), se=FALSE, na.rm= TRUE)


ggplot(CO2_mass_traits, aes(x=total.biomass, y=GPP1200, col=T_level))+
  geom_point()


ggplot(CO2_traits_community, aes(y=GPP1200, x=site, fill=treatment))+
  geom_boxplot()+
  facet_wrap(~ T_level, scales="free_x")


ggplot(CO2_traits_community, aes(y=Wmean_SLA, x=site, fill=T_level))+
  geom_boxplot()+
  facet_wrap(~ T_level, scales="free_x")

ggplot(CO2_traits_community, aes(y=Wmean_CN, x=site, fill=T_level))+
  geom_boxplot()+
  facet_wrap(~ T_level, scales="free_x")

ggplot(CO2_traits_community, aes(y=Wmean_N, x=site, fill=T_level))+
  geom_boxplot()+
  facet_wrap(~ T_level, scales="free_x")

ggplot(CO2_traits_community, aes(y=Reco15, x=site, fill=treatment))+
  geom_boxplot()+
  facet_wrap(~ T_level, scales="free_x")



# Traits across grid
CO2_mass_traits%>%
  ggplot(aes(x= P_level, y=Wmean_CN, col= T_level))+
  geom_boxplot()+
  facet_grid(~ T_level)

CO2_mass_traits%>%
  ggplot(aes(x= P_level, y=Wmean_Lth, col= T_level))+
  geom_boxplot()+
  facet_grid(~ T_level)

CO2_mass_traits%>%
  ggplot(aes(x= P_level, y=Wmean_LA, col= T_level))+
  geom_boxplot()+
  facet_grid(~ T_level)

CO2_mass_traits%>%
  ggplot(aes(x= P_level, y=Wmean_SLA, col= T_level))+
  geom_boxplot()+
  facet_grid(~ T_level)

CO2_mass_traits%>%
  ggplot(aes(x= P_level, y=Wmean_Height, col= T_level))+
  geom_boxplot()+
  facet_grid(~ T_level)

CO2_mass_traits%>%
  ggplot(aes(x= P_level, y=Wmean_LDMC, col= T_level))+
  geom_boxplot()+
  facet_grid(~ T_level)

CO2_mass_traits%>%
  ggplot(aes(x= P_level, y=Wmean_SLN, col= T_level))+
  geom_boxplot()+
  facet_grid(~ T_level)


###
CO2_mass_traits%>%
  ggplot(aes(x= Gram_biomass, y=GPP, col= T_level))+
  geom_point()



CO2_mass_traits%>%
  ggplot(aes(x= site, y=GPP1200, col= T_level))+
  geom_point()

CO2_mass_traits%>%
  ggplot(aes(x= Wmean_CN, y=GPP1200, col= T_level))+
  geom_point()+
  geom_smooth(aes(group=1), method="lm")

