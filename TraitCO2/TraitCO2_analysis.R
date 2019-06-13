source("O:\\FunCab\\Data\\FunCaB\\Other\\R_functions\\HighstatLibV10.R")
#source("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\Read_alldataTraitCO2.R")
library(lme4)
library(lmerTest)
library(ade4)
library(stringr)
library(nlme)
library(r2glmm)
library(broom)
library(tidyverse)
######################################################################################################################

###### Calculate median GPP1200 and Reco15 per plot per year
CO2_mass_traits_median <- CO2_mass_traits%>%
  dplyr::rename(Site =site)%>%
  dplyr::group_by(turfID, Site, year, P_level, T_level, treatment) %>%
  dplyr::summarise_all(median, na.rm=TRUE)%>%
  dplyr::rename(Year = year)%>%
  ungroup()

apply(CO2_mass_traits_median, 2, function(x) length(which(!is.na(x))))

#### Transformation of variables####
#### Distribution of variables
cols <- c("VegetationHeight", "gram_cover", "forb_cover", "Wmean_N", "Wmean_C", "Wmean_CN", "Wmean_LDMC", "Wmean_Lth", "Wmean_LA", "Wmean_Height", "Wmean_SLA")
hist(CO2_mass_traits_median[cols])

#hist(CO2_mass_traits_median$GPP1200)
#hist(CO2_mass_traits_median$Reco15)

#### Check for correlation between plant traits and vegetation composition####
com_traits <- CO2_mass_traits_median %>%
  ungroup()%>%
  select(VegetationHeight, richness, evenness, diversity, Wmean_N, Wmean_C, Wmean_CN, Wmean_LDMC, Wmean_Lth, Wmean_LA, Wmean_SLA, Wmean_Height)%>%
  rename(CWM_LCN = Wmean_CN , CWM_LN =Wmean_N , CWM_LC = Wmean_C,  CWM_H = Wmean_Height,  CWM_SLA = Wmean_SLA, CWM_LDMC = Wmean_LDMC, CWM_LT = Wmean_Lth,  CWM_LA = Wmean_LA , Richness= richness, Evenness = evenness, Diversity= diversity, Veg.Height = VegetationHeight)

#### Checking for correlations ###
GGally::ggpairs(com_traits)


#### Climate Effect on Vegetation Structure and Traits variables
# Biomass
summary(lmer(forb_cover~ Temp.C:scale(P.mm) + (1|Year/Site), CO2_mass_traits_median)) # sign P.mm p= 0.05
summary(lmer(gram_cover~ Temp.C+   (1|Year/Site), CO2_mass_traits_median)) # non sign
summary(lmer(VegetationHeight~ Temp.C+ (1|Year/Site), CO2_mass_traits_median)) # sign T p <0.001
summary(lmer(richness~ Temp.C+  (1|Year/Site), CO2_mass_traits_median))
summary(lmer(evenness~ Temp.C+  (1|Year/Site), CO2_mass_traits_median))
summary(lmer(diversity ~ Temp.C+  (1|Year/Site), CO2_mass_traits_median))

#Traits CWM
summary(lmer(Wmean_LDMC ~ scale(P.mm) + (1|Year/Site), CO2_mass_traits_median)) # sign P.mm and interaction
summary(lmer(Wmean_Lth ~  Temp.C + (1|Year/Site), CO2_mass_traits_median)) 
summary(lmer(Wmean_LA ~  Temp.C+  (1|Year/Site), CO2_mass_traits_median)) #sign Temp.C
summary(lmer(Wmean_SLA ~ Temp.C+  Temp.C:scale(P.mm) +  (1|Year/Site), CO2_mass_traits_median)) # all sign
summary(lmer(Wmean_Height ~ Temp.C+Temp.C:scale(P.mm) +  (1|Year/Site), CO2_mass_traits_median)) # T sign
summary(lmer(Wmean_CN ~ Temp.C+ scale(P.mm) +  (1|Year/Site), CO2_mass_traits_median)) # non sign
summary(lmer(Wmean_C ~ Temp.C+ (1|Year/Site), CO2_mass_traits_median)) #sign Temp.C
summary(lmer(Wmean_N ~ scale(P.mm) + (1|Year/Site), CO2_mass_traits_median)) # non sign

#Traits Fvar
summary(lmer(Wvar_LDMC ~ Temp.C+ scale(P.mm) +  (1|Year/Site), CO2_mass_traits_median)) # sign P.mm and interaction
summary(lmer(Wvar_Lth ~ Temp.C +   (1|Year/Site), CO2_mass_traits_median)) 
summary(lmer(Wvar_LA ~   Temp.C:scale(P.mm) + (1|Year/Site), CO2_mass_traits_median)) #sign Temp.C
summary(lmer(Wvar_SLA ~ Temp.C+ (1|Year/Site), CO2_mass_traits_median)) # all sign
summary(lmer(Wvar_Height ~ Temp.C+ (1|Year/Site), CO2_mass_traits_median)) # T sign
summary(lmer(Wvar_CN ~ Temp.C+ (1|Year/Site), CO2_mass_traits_median)) # non sign
summary(lmer(Wvar_C ~ Temp.C+  (1|Year/Site), CO2_mass_traits_median)) #sign Temp.C
summary(lmer(Wvar_N ~ Temp.C+ scale(P.mm) + (1|Year/Site), CO2_mass_traits_median)) # non sign



######## Variance decomposition ###########################################################################################################
library(MASS)
library(lmerTest)
library(car)






# Between site variance
# Climate model
lm_A<- lm(GPP700 ~ Temp.C, data=CO2_mass_traits_median)
summary(lm_A) #adj R2 0.1352
AIC(lm_A)
step(lm_A)

#Veg structure model
lm_B<- lm(GPP700 ~ forb_cover + VegetationHeight , data=CO2_mass_traits_median)
summary(lm_B) # adj R2 0.1947
step(lm_B)

#Traits
### Model selection with regularisation ; lasso method
library(glmnet)

CO2_traits_GPPmodelC<-CO2_mass_traits_median%>%
  select(GPP700, Wmean_Height , Wmean_Lth , Wmean_N , Wmean_CN , Wmean_C, Wmean_SLA , Wmean_LDMC , Wmean_Lth , Wvar_Height , Wvar_Lth , Wvar_CN , Wvar_SLA , Wvar_LDMC , Wvar_C, Wvar_LA , Wvar_N)

x=model.matrix(GPP700~.-1,data=CO2_traits_GPPmodelC) 
y=CO2_traits_GPPmodelC$GPP700
fit.lasso=glmnet(x,y,alpha=1)
plot(fit.lasso,xvar="lambda",label=TRUE)
plot(fit.lasso,xvar="dev",label=TRUE)
cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)
coef(cv.lasso)

#regularisation model
GPP_C1<- (lm(GPP700 ~  Wmean_Height +  Wmean_N + Wmean_SLA + Wvar_SLA + Wvar_C, data=CO2_mass_traits_median))
summary(GPP_C1)
GPP_C2<- (lm(GPP700 ~  Wmean_Height + Wmean_Lth + Wmean_N + Wmean_SLA + Wvar_LDMC + Wvar_LA, data=CO2_mass_traits_median))
summary(GPP_C2)

anova(GPP_C1, GPP_C2)
step(GPP_C2)

lm_AB<- lm(GPP700 ~ Temp.C + forb_cover + VegetationHeight, data=CO2_mass_traits_median)
summary(lm_AB)

lm_AC<- lm(GPP700 ~ Temp.C+ Wmean_Height + Wmean_N + Wmean_Lth + Wmean_SLA + Wvar_LA + Wvar_LDMC +Wvar_C , data=CO2_mass_traits_median)
summary(lm_AC)

lm_BC <- lm(GPP700 ~ forb_cover + VegetationHeight + Wmean_Height + Wmean_N + Wmean_SLA + Wmean_Lth + Wvar_C + Wvar_LDMC + Wvar_LA, data=CO2_mass_traits_median)
summary(lm_BC)


lm_ABC <- lm(GPP700 ~ Temp.C + forb_cover + VegetationHeight + Wmean_Height + Wmean_N + Wmean_SLA + Wmean_Lth + Wvar_C + Wvar_LDMC + Wvar_LA, data=CO2_mass_traits_median)
summary(lm_ABC)




#Within site
lm_0Full<- lm(GPP700 ~ Site, data=CO2_mass_traits_median)
summary(lm_0Full)

lm_a<- lm(GPP700 ~ Site + Temp.C, data=CO2_mass_traits_median)
summary(lm_a)

lm_b<- lm(GPP700 ~ Site + forb_cover + VegetationHeight , data=CO2_mass_traits_median)
summary(lm_b)

lm_c<- lm(GPP700 ~ Site +  Wmean_Height + Wmean_N + Wmean_SLA + Wmean_Lth +  Wvar_C + Wvar_SLA + Wvar_LA, data=CO2_mass_traits_median)
summary(lm_c)

lm_ab<- lm(GPP700 ~ Site + Temp.C + forb_cover + VegetationHeight, data=CO2_mass_traits_median)
summary(lm_ab)

lm_ac<- lm(GPP700 ~ Site + Temp.C + Wmean_Height + Wmean_N + Wmean_SLA + Wmean_Lth +  Wvar_C + Wvar_SLA + Wvar_LA, 
           data=CO2_mass_traits_median)
summary(lm_ac)
anova(lm_0Full, lm_ac)

lm_bc<- lm(GPP700 ~ Site + forb_cover + VegetationHeight + Wmean_Height + Wmean_N + Wmean_SLA + Wmean_Lth +  Wvar_C + Wvar_SLA + Wvar_LA,
           data=CO2_mass_traits_median)
summary(lm_bc)

lm_abc<- lm(GPP700 ~ Site + Temp.C + forb_cover + VegetationHeight + Wmean_Height + Wmean_N + Wmean_SLA + Wmean_Lth +  Wvar_C + Wvar_SLA + Wvar_LA, data=CO2_mass_traits_median)
summary(lm_abc)



#RECO BETWEEN SITE VARIATION
lm_0Full<- lm(Reco15 ~ Site, data=CO2_mass_traits_median)
summary(lm_0Full)
# R2a 0.2578

lm_A<- lm(Reco15 ~ Temp.C , data=CO2_mass_traits_median)
summary(lm_A)
# R2a 0.1784

# Veg structure model 
lm_B <- lm(Reco15 ~ VegetationHeight , data=CO2_mass_traits_median)
summary(lm_B)
# R2a 0.1784

# Trait model
CO2_traits_RecomodelC<-CO2_mass_traits_median%>%
  select(Reco15, Wmean_Height , Wmean_Lth , Wmean_N , Wmean_CN , Wmean_C, Wmean_SLA , Wmean_LDMC , Wmean_Lth , Wvar_Height , Wvar_Lth , Wvar_CN , Wvar_SLA , Wvar_LDMC , Wvar_C, Wvar_LA , Wvar_N)

x=model.matrix(Reco15~.-1,data=CO2_traits_RecomodelC) 
y=CO2_traits_RecomodelC$Reco15
fit.lasso=glmnet(x,y,alpha=1)
plot(fit.lasso,xvar="lambda",label=TRUE)
plot(fit.lasso,xvar="dev",label=TRUE)
cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)
coef(cv.lasso)

Reco_C1<- lm(Reco15 ~ Wmean_Height + Wmean_SLA + Wvar_C , data=CO2_mass_traits_median)
summary(Reco_C1)
Reco_C2<- lm(Reco15 ~ Wmean_Height + Wmean_N + Wvar_C + Wvar_LA, data=CO2_mass_traits_median)
summary(Reco_C2)
anova(Reco_C1, Reco_C2)
# R2a 0.2011

lm_AB <- lm(Reco15 ~ Temp.C + VegetationHeight , data=CO2_mass_traits_median)
summary(lm_AB)
# R2a 0.217

lm_AC <- lm(Reco15 ~ Temp.C + Wmean_Height + Wmean_N + Wvar_C + Wvar_LA, data=CO2_mass_traits_median)
summary(lm_AC)
# R2a 0.226

lm_BC <- lm(Reco15 ~ VegetationHeight + Wmean_Height + Wmean_N + Wvar_C + Wvar_LA, data=CO2_mass_traits_median)
summary(lm_BC)
# R2a 0.2369

lm_ABC <- lm(Reco15 ~ Temp.C +VegetationHeight + Wmean_Height + Wmean_N + Wvar_C + Wvar_LA, data=CO2_mass_traits_median)
summary(lm_ABC)
# R2a 0.2481


#RECO WITHIN SITE VARIATION
lm_a<- lm(Reco15 ~ Site + Temp.C , data=CO2_mass_traits_median)
summary(lm_a)

lm_b<- lm(Reco15 ~ Site + VegetationHeight , data=CO2_mass_traits_median)
summary(lm_b)

lm_c<- lm(Reco15 ~ Site +  Wmean_Height + Wmean_N + Wvar_C + Wvar_LA , data=CO2_mass_traits_median)
summary(lm_c)

lm_ab<- lm(Reco15 ~ Site + Temp.C + VegetationHeight, data=CO2_mass_traits_median)
summary(lm_ab)

lm_ac<- lm(Reco15 ~ Site + Temp.C + Wmean_Height + Wmean_N + Wvar_C + Wvar_LA, data=CO2_mass_traits_median)
summary(lm_ac)

lm_bc <- lm(Reco15 ~ Site + VegetationHeight + Wmean_Height + Wmean_N + Wvar_C + Wvar_LA, data=CO2_mass_traits_median)
summary(lm_bc)

lm_abc<- lm(Reco15 ~ Site + Temp.C + VegetationHeight + Wmean_Height + Wmean_N + Wvar_C + Wvar_LA, data=CO2_mass_traits_median)
summary(lm_abc)


######################################################################################################################
# Structural Equation Model
######################################################################################################################
library(piecewiseSEM)
library(nlme)
# Fit piecewise model with random effect
# Create component models and store in list
model <- psem(lm(y1 ~ x1, dat), lm(y1 ~ y2, dat), lm(y2 ~ x1, dat), lm(y3 ~ y1, dat))
summary(model, .progressBar = F)


GPP_PSM_Climate_Biomass <- psem(
  #indirect pathway of climate through Biomass
  lme(VegetationHeight~ Temp.C + P.mm , random = ~ 1 | Year/Site, data = CO2_mass_traits_median),
  lme(forb_cover~ Temp.C + P.mm, random = ~ 1 | Year/Site, data = CO2_mass_traits_median),
  lme(GPP700~ Temp.C + P.mm + VegetationHeight + forb_cover, random = ~ 1 | Year/Site, data = CO2_mass_traits_median)
)

summary(GPP_PSM_Climate_Biomass, .progressBar = TRUE)
coefs(GPP_PSM_Climate_Biomass, standardize = "scale")


GPP_PSM_Climate_Traits <- psem(
  #indirect pathway of climate through Biomass
  lme(Wmean_Height~ Temp.C + P.mm , random = ~ 1 | Year/Site, data = CO2_mass_traits_median),
  lme(Wmean_SLA~ Temp.C + P.mm , random = ~ 1 | Year/Site, data = CO2_mass_traits_median),
  lme(Wmean_Lth~ Temp.C + P.mm + Wmean_SLA, random = ~ 1 | Year/Site, data = CO2_mass_traits_median),
  lme(Wmean_N~ Temp.C + P.mm + Wmean_SLA + Wmean_Lth + Wmean_Height, random = ~ 1 | Year/Site, data = CO2_mass_traits_median),
  lme(GPP700~ Temp.C + P.mm + Wmean_Height + Wmean_N + Wmean_Lth + Wmean_SLA, random = ~ 1 | Year/Site, data = CO2_mass_traits_median)
)

summary(GPP_PSM_Climate_Traits, .progressBar = TRUE)
coefs(GPP_PSM_Climate_Traits, standardize = "scale")


GPP_PSM_Climate_BiomassTraits <- psem(
  #indirect pathway of climate through Biomass
  lme(VegetationHeight~ Temp.C + P.mm , random = ~ 1 | Year/Site, data = CO2_mass_traits_median),
  lme(forb_cover~ Temp.C + P.mm, random = ~ 1 | Year/Site, data = CO2_mass_traits_median),
  lme(Wmean_Height~ Temp.C + P.mm , random = ~ 1 | Year/Site, data = CO2_mass_traits_median),
  lme(Wmean_SLA~ Temp.C + P.mm , random = ~ 1 | Year/Site, data = CO2_mass_traits_median),
  lme(Wmean_Lth~ Temp.C + P.mm + Wmean_SLA, random = ~ 1 | Year/Site, data = CO2_mass_traits_median),
  lme(Wmean_N~ Temp.C + P.mm +  Wmean_SLA + Wmean_Lth + Wmean_Height, random = ~ 1 | Year/Site, data = CO2_mass_traits_median),
  lme(GPP700~ Temp.C + P.mm + Wmean_Height + VegetationHeight+ forb_cover + Wmean_N + Wmean_Lth + Wmean_SLA, random = ~ 1 | Year/Site, data = CO2_mass_traits_median)
)

summary(GPP_PSM_Climate_BiomassTraits, .progressBar = TRUE)
coefs(GPP_PSM_Climate_BiomassTraits, standardize = "scale")



######################################################################################################################
#Mixed effect linear models 
########### Forward selection of models using median values per plot per year for GPP1200 and Reco ###################

####### GPP1200 ########

# NUL-model only random effect of median dataset
model_0Full<- lmer(GPP700 ~ (1|Year/Site), data=CO2_mass_traits_median)
summary(model_0Full)
r.squaredGLMM(model_0Full)
AIC(model_0Full)
#R2c = 0.30

# model A, best model Climate
model_aFull<- lmer(GPP700 ~ Temp.C*P.mm + (1|Year/Site), data=CO2_mass_traits_median)
summary(model_aFull)
AIC(model_aFull)
r.squaredGLMM(model_aFull)
CO2_mass_traits_median$GPPA<- fitted(model_aFull)
#R2m 0.239 R2c 0.29

# model B best model Biomass
model_bFull<- lmer(GPP700 ~ forb_cover + vegetationHeight + (1|Site), data=CO2_mass_traits_median)
summary(model_bFull)
AIC(model_bFull)
r.squaredGLMM(model_bFull)
CO2_mass_traits_median$GPPB<- fitted(model_bFull)
#R2m 0.099  R2c  0.28

# model C best model traits
# Wmean_CN + Wmean_C + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight +

model_cred<- lmer(GPP700 ~ Wmean_CN + Wmean_LA + (1|Site), data=CO2_mass_traits_median) 
summary(model_cred)
AIC(model_cred)
r.squaredGLMM(model_cred)
CO2_mass_traits_median$GPPC<- fitted(model_cred)
#R2m 0.058  R2c  0.35

# model A+B
model_bFull<- lmer(GPP700 ~ Temp.C*P.mm + forb_cover + VegetationHeight + (1|Site), data=CO2_mass_traits_median)
summary(model_bFull)
r.squaredGLMM(model_bFull)
#R2m 0.22  R2c  0.34  # adding interaction does not have significant increase of R2 F_c.biomass + G_c.biomass +

# model A+C
model_bFull<- lmer(GPP700 ~ Temp.C*P.mm + Wmean_CN +  Wmean_LA +  (1|Site), data=CO2_mass_traits_median)
summary(model_bFull)
r.squaredGLMM(model_bFull)
#R2m 0.265  R2c  0.33

# model B+C
model_cred<- lmer(GPP700 ~ forb_cover + VegetationHeight + Wmean_CN + Wmean_LA + (1|Site), data=CO2_mass_traits_median) 
summary(model_cred)
r.squaredGLMM(model_cred)
#R2m 0.18 R2c  0.44

# model ABC 
model_abc<- lmer(GPP700 ~ Temp.C*P.mm + forb_cover + VegetationHeight + Wmean_CN + Wmean_LA + (1|Site), data=CO2_mass_traits_median)
summary(model_abc)
AIC(model_abc)
r.squaredGLMM(model_abc)
CO2_mass_traits_median$GPPABC<- fitted(model_abc)
#R2m 0.32 R2c  0.48

###### Best model
model_abc2<- lmer(GPP700 ~ forb_cover + VegetationHeight + Wmean_CN + Wmean_LA + (1|Site), data=CO2_mass_traits_median)
summary(model_abc2)
r.squaredGLMM(model_abc2)
AIC(model_abc2)
#R2m 0.18 R2c  0.41  
CO2_mass_traits_median$GPPABC2<- fitted(model_abc2)


# regression between observed and modeled GPP
summary(lm(GPP700~GPPA, data= CO2_mass_traits_median)) #r2=0.30
summary(lm(GPP700~GPPB, data= CO2_mass_traits_median)) #r2=0.35
summary(lm(GPP700~GPPC, data= CO2_mass_traits_median)) #r2=0.32
summary(lm(GPP700~GPPABC, data= CO2_mass_traits_median)) #r2=0.37
summary(lm(GPP700~GPPABC2, data= CO2_mass_traits_median)) #r2=0.37

# Variance decomposition 
summary(lmer(VegetationHeight ~ T_level + (1|Site), data=CO2_mass_traits_median))
summary(lmer(Wmean_LA ~ T_level + (1|Site), data=CO2_mass_traits_median))

r.squaredGLMM(lmer(GPP700 ~ T_level+ Wmean_LA + (1|Site), data=CO2_mass_traits_median))
r.squaredGLMM(lmer(GPP700 ~ T_level+ VegetationHeight + (1|Site), data=CO2_mass_traits_median))



########### Reco ################################################################################################################################### Forward selection of models

model_0Full<- lmer(Reco15 ~ (1|Year/Site), data=CO2_mass_traits_median)
summary(model_0Full)
AIC(model_0Full)
r.squaredGLMM(model_0Full)


# How much does grid explain
# Model A
model_aFull<- lmer(Reco15 ~ P.mm + (1|Year/Site), data=CO2_mass_traits_median)
summary(model_aFull)
AIC(model_aFull)
r.squaredGLMM(model_aFull)
CO2_mass_traits_median$RecoA<- fitted(model_aFull)
#R2m 0.04 R2c  0.16 

# How much does Total biomass/total _cover explain (Functional group biomass goes towards functional traits explanation)
# Model B
model_bFull<- lmer(Reco15 ~ VegetationHeight + (1|Site), data=CO2_mass_traits_median)
summary(model_bFull)
AIC(model_bFull)
r.squaredGLMM(model_bFull)
CO2_mass_traits_median$RecoB<- fitted(model_bFull)
#R2m 0.022  R2c  0.088

# How much do traits explain
model_cFull<- lmer(Reco15 ~ Wmean_CN + Wmean_N +Wmean_C + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_SLA + Wmean_Height +  (1|Site), data=CO2_mass_traits_median)
summary(model_cFull)
step(model_cFull)
AIC(model_cFull)
r.squaredGLMM(model_cFull)
#R2m 0.02 R2c  0.12

#model C
model_cred<- lmer(Reco15 ~ Wmean_N + (1|Site), data=CO2_mass_traits_median) 
summary(model_cred)
AIC(model_cred)
r.squaredGLMM(model_cred)
CO2_mass_traits_median$RecoC<- fitted(model_cred)
#R2m 0.04 R2c  0.23
#Traits explain very little variation in RECO  


# Model AB
model_abFull<- lmer(Reco15 ~ Temp.C*P.mm + VegetationHeight + (1|Site), data=CO2_mass_traits_median)
summary(model_abFull)
r.squaredGLMM(model_abFull)
#R2m 0.07  R2c  0.20

# Model AC
model_acFull<- lmer(Reco15 ~ Temp.C*P.mm + Wmean_N + (1|Site), data=CO2_mass_traits_median)
summary(model_acFull)
r.squaredGLMM(model_acFull)
#R2m 0.07 R2c  0.19

# Model BC
model_bcFull<- lmer(Reco15 ~  VegetationHeight + Wmean_N + (1|Site), data=CO2_mass_traits_median)
summary(model_bcFull)
r.squaredGLMM(model_bcFull)
#R2m 0.06  R2c  0.24

# Model ABC
model_abcFull<- lmer(Reco15 ~ Temp.C*P.mm + VegetationHeight + Wmean_N + (1|Site), data=CO2_mass_traits_median)
summary(model_abcFull)
AIC(model_abcFull)
r.squaredGLMM(model_abcFull)
CO2_mass_traits_median$RecoABC<- fitted(model_abcFull)
#R2m 0.10  R2c  0.21

# Model ABC2
model_abc2Full<- lmer(Reco15 ~ VegetationHeight + Wmean_N + (1|Site), data=CO2_mass_traits_median)
summary(model_abc2Full)
AIC(model_abc2Full)
r.squaredGLMM(model_abc2Full)
CO2_mass_traits_median$RecoABC2<- fitted(model_abc2Full)
#R2m 0.10  R2c  0.21

summary(lm(Reco15~RecoA, data= CO2_mass_traits_median)) #r2=0.15
summary(lm(Reco15~RecoB, data= CO2_mass_traits_median)) #r2=0.17
summary(lm(Reco15~RecoC, data= CO2_mass_traits_median)) #r2=0.16
summary(lm(Reco15~RecoABC, data= CO2_mass_traits_median)) #r2=0.18
summary(lm(Reco15~RecoABC2, data= CO2_mass_traits_median)) #r2=0.18


summary(lmer(VegetationHeight ~ Temp.C*P.mm + (1|Site), data=CO2_mass_traits_median)) # sign. correlation with T
summary(lmer(Wmean_N ~ Temp.C*P.mm  + (1|Site), data=CO2_mass_traits_median))
summary(lmer(Wmean_N ~ VegetationHeight  + (1|Site), data=CO2_mass_traits_median))

r.squaredGLMM(lmer(Reco15 ~ Temp.C + Wmean_N + (1|Site), data=CO2_mass_traits_median))
r.squaredGLMM(lmer(Reco15 ~ Temp.C + VegetationHeight + (1|Site), data=CO2_mass_traits_median))

######## ANPP Grasses and Forbs

model_0Full<- lmer(Gram_biomass+Forb_biomass ~  + (1|Site), na.action=na.omit, data=CO2_mass_traits_median)
summary(model_0Full)
AIC(model_0Full)
r.squaredGLMM(model_0Full)

# climate model
model_aFull<- lmer(Gram_biomass+Forb_biomass ~ Temp.C*P.mm + (1|Site), na.action=na.omit, data=CO2_mass_traits_median)
summary(model_aFull)
AIC(model_aFull)
r.squaredGLMM(model_aFull)

#ggplot(CO2_mass_traits_median, aes(y=total.biomass, x = P_level ))+
  #geom_boxplot()

model_bFull<- lmer(Gram_biomass+Forb_biomass ~ gram_cover + VegetationHeight + (1|Site), na.action=na.omit, data=CO2_mass_traits_median)
summary(model_bFull)
AIC(model_bFull)
r.squaredGLMM(model_bFull)
#R2m 0.29 R2c 0.64

model_b2Full<- lmer(Gram_biomass+Forb_biomass ~ gram_cover + forb_cover + VegetationHeight + (1|Site), na.action=na.omit, data=CO2_mass_traits_median)
summary(model_b2Full)
AIC(model_b2Full)
r.squaredGLMM(model_b2Full)
#R2m 0.29 R2c 0.64
anova(model_bFull, model_b2Full)

model_cFull<- lmer(Gram_biomass+Forb_biomass ~ Wmean_Height + (1|Site), na.action=na.omit, data=CO2_mass_traits_median)
summary(model_cFull)
AIC(model_cFull)
r.squaredGLMM(model_cFull)

model_abcFull<- lmer(Gram_biomass+Forb_biomass ~ Temp.C*P.mm + gram_cover + forb_cover + VegetationHeight + (1|Site), na.action=na.omit, data=CO2_mass_traits_median)
summary(model_abcFull)
AIC(model_abcFull)
r.squaredGLMM(model_abcFull)


summary(lmer(VegetationHeight ~ Temp.C*P.mm + (1|Site), data=CO2_mass_traits_median)) # sign. correlation with T
summary(lmer(Wmean_N ~ Temp.C*P.mm  + (1|Site), data=CO2_mass_traits_median))
summary(lmer(Wmean_N ~ VegetationHeight  + (1|Site), data=CO2_mass_traits_median))

r.squaredGLMM(lmer(Gram_biomass+Forb_biomass ~ Temp.C*P.mm + gram_cover + forb_cover + VegetationHeight + (1|Site), data=CO2_mass_traits_median))
r.squaredGLMM(lmer(Gram_biomass+Forb_biomass ~ Temp.C*P.mm + Wmean_Height + (1|Site), data=CO2_mass_traits_median))
r.squaredGLMM(lmer(Gram_biomass+Forb_biomass ~ gram_cover + forb_cover + VegetationHeight + Wmean_Height + (1|Site), data=CO2_mass_traits_median))












########### GPP ######
# Backward selection of models using all data

###### Model with combination of abiotic and biotic factors
model_aFull<- lmer(GPP1200 ~ scale(PAR) + tempK + scale(Total_c.biomass) + scale(B_c.biomass) + scale(G_c.biomass) + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight + T_level*P_level + (1|Site/turfID), data=CO2_mass_traits)
summary(model_aFull)
r.squaredGLMM(model_aFull)

model_aFull<- lmer(GPP1200 ~ tempK + scale(B_c.biomass) + Wmean_CN +  Wmean_logHeight + (1|Site/turfID), data=CO2_mass_traits)
summary(model_aFull)
r.squaredGLMM(model_aFinal)

model_aFinal<- lmer(GPP1200 ~ scale(PAR) + tempK +  scale(F_c.biomass) + Wmean_CN + Wmean_logHeight + (1|Site/turfID), data=CO2_mass_traits)
summary(model_aFinal)
r.squaredGLMM(model_aFinal)
# R2m 0.16 R2c 0.46

model_a2Full<- lmer(GPP ~ scale(PAR) + tempK + scale(F_c.biomass) + scale(G_c.biomass) + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight + Temp.C + scale(P.mm) + (1|Site/turfID), data=CO2_mass_traits)
summary(model_a2Full)
r.squaredGLMM(model_a2Full)
# R2m 0.36 R2c 0.57 ; PAR explaines ~0.14

model_a2Final<- lmer(GPP ~ scale(PAR) + tempK + scale(F_c.biomass) + Wmean_CN + Wmean_logHeight + (1|Site/turfID)
                     , data=CO2_mass_traits)
summary(model_a2Final)
r.squaredGLMM(model_a2Final)
# R2m 0.33 R2c 0.56

########### model with only CWM traits Wmean_
model_GPP1200_Full<- lmer(GPP1200 ~  Wmean_N + Wmean_C + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight
                          +(1|Site/turfID), data=CO2_mass_traits)
summary(model_GPP1200_Full)
step(model_GPP1200_Full)

model_GPP1200_Final <- lmer(GPP1200 ~  Wmean_CN + Wmean_logHeight + (1|Site/turfID), data=CO2_mass_traits)
summary(model_GPP1200_Final)
r.squaredGLMM(model_GPP1200_Final)
#R2m 0.11 R2c 0.42

####### model with only Trait variation ; Wvar_
model_a3Full<- lmer(GPP1200 ~ scale(Wvar_LDMC) + scale(Wvar_Lth) + Wvar_LA + Wvar_logSLA + Wvar_logHeight + Wvar_CN + Wvar_C + Wvar_N + (1|Site/turfID), data=CO2_mass_traits)
summary(model_a3Full)
r.squaredGLMM(model_a3Full)
# R2m 0.03 R2c 0.39 ; Fixed effects of trait variation only explain 3% of  the varaiation in GPP standardized to PAR=1200

model_a3Final<- lmer(GPP1200 ~ Wvar_logHeight + Wvar_CN + Wvar_C + Wvar_N + (1|Site/turfID), data=CO2_mass_traits)
summary(model_a3Final)
r.squaredGLMM(model_a3Final)
# R2m 0.03 R2c 0.38 ; Fixed effects of trait variation only explain 3% of  the varaiation in GPP standardized to PAR=1200


######### Reco #######

###### Model with combination of abiotic and biotic factors
model_bFull<- lmer(Reco15 ~ tempK + scale(F_c.biomass) + scale(B_c.biomass) + scale(G_c.biomass) + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight + Temp.C + scale(P.mm) + (1|Site/turfID), data=CO2_mass_traits)
summary(model_bFull)
r.squaredGLMM(model_bFull)
# R2m 0.16 R2C 0.49

model_bFinal<- lmer(Reco15 ~ tempK + scale(B_c.biomass) + Wmean_CN + Wmean_logHeight + (1|Site/turfID), data=CO2_mass_traits)
summary(model_bFinal)
r.squaredGLMM(model_bFinal)
# R2m 0.14 R2c 0.44

model_b2Full<- lmer(Reco ~ tempK + scale(F_c.biomass) + scale(B_c.biomass) + scale(G_c.biomass) + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight + Temp.C + scale(P.mm) + (1|Site/turfID), data=CO2_mass_traits)
summary(model_b2Full)
r.squaredGLMM(model_b2Full)
# R2m 0.26 R2C 0.53

model_b2Final<- lmer(Reco ~ tempK + Wmean_LA + Wmean_logHeight + (1|Site/turfID), data=CO2_mass_traits)
summary(model_b2Final)
r.squaredGLMM(model_b2Final)
# R2m 0.24 R2c 0.50






########## CHECK MODELS ###################################
#### residuals check
plot(fitted(model_mean_aFinal), resid(model_mean_aFinal))
abline (0,0)

#### response variable linear function of fitted value
plot(fitted(model_mean_aFinal), na.omit(TBI_variables$S))

#### residuals normally distributed
qqnorm(resid(model_mean_aFinal))
qqline(resid(model_mean_aFinal))

af<-anova(model_mean_aFinal)
afss <- af$"Sum Sq"
print(cbind(af,PctExp=afss/sum(afss)*100))

plot(fitted(model_mean_bFinal), resid(model_mean_bFinal)) # residuals M2 off
abline(0,0)

#### residuals normally distributed
qqnorm(resid(model_mean_bFinal))
qqline(resid(model_mean_bFinal))

af<-anova(model_mean_bFinal)
afss <- af$"Sum Sq"
print(cbind(af,PctExp=afss/sum(afss)*100))

plot(fitted(M2), TBI_BOR$k)
plot(fitted(M3b), TBI_SUB$k)
plot(fitted(M4b), TBI_ALP$k)

qqnorm(resid(M2))
qqline(resid(M2))

TBI_means$modelK<- fitted(Kred)





model_Reco15_Full <- lmer(Reco15 ~  Wmean_N + Wmean_C + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight +
                       (1|Site/turfID), data=CO2_mass_traits)
summary(model_Reco15_Full)
step(model_Reco15_Full)

model_Reco15_Final <- lmer(Reco15~  Wmean_CN + (1|Site/turfID), data=CO2_mass_traits)
summary(model_Reco15_Final)
r.squaredGLMM(model_Reco15_Final)
#! Wmean_CN remains after dropping least significant Wmean_trait one by one


########### model with only traits for non-standardized fluxes 
model_GPP_Full <- lmer(GPP ~  Wmean_N + Wmean_C + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight +
                         (1|Site/turfID), data=CO2_mass_traits)
summary(model_GPP_Full)
step(model_GPP_Full)
# same traits Wmean_logHeight and Wmean_CN remain as with standardized fluxes

model_Reco_Full <- lmer(Reco ~ Wmean_N + Wmean_C + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight +
                        (1|Site/turfID), data=CO2_mass_traits)
summary(model_Reco_Full)
step(model_Reco_Full)
# different Trait logHeight remain than for standardized flux

### model for specific T_level and P_level
### Subset the different climate levels
CO2_mass_traits_ALP <- subset(CO2_mass_traits_median, T_level == "Alpine")
CO2_mass_traits_SUB <- subset(CO2_mass_traits_median, T_level == "Sub-alpine")
CO2_mass_traits_BOR <- subset(CO2_mass_traits_median, T_level == "Boreal")

CO2_mass_traits_P1 <- subset(CO2_mass_traits_median, P_level == "1")
CO2_mass_traits_P2 <- subset(CO2_mass_traits_median, P_level == "2")
CO2_mass_traits_P3 <- subset(CO2_mass_traits_median, P_level == "3")
CO2_mass_traits_P4 <- subset(CO2_mass_traits_median, P_level == "4")

######## model standardized GPP for separate climate levels
model_GPP1200_ALP<- lmer(GPP1200 ~ Wmean_CN + Wmean_Lth + Wmean_LA + 
                           (1|Site), data=CO2_mass_traits_ALP)
summary(model_GPP1200_ALP)
r.squaredGLMM(model_GPP1200_ALP)
# Wmean_N + Wmean_Lth + Wmean_LA + Wmean_logHeight remain

model_GPP1200_SUB<- lmer(GPP1200 ~  Wmean_N + Wmean_C + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight +
                           (1|Site/turfID), data=CO2_mass_traits_SUB)
summary(model_GPP1200_SUB)
step(model_GPP1200_SUB)
# Wmean_CN + Wmean_logHeight remain

model_GPP1200_BOR<- lmer(GPP1200 ~  Wmean_N + Wmean_C + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight +
                           (1|Site/turfID), data=CO2_mass_traits_BOR)
summary(model_GPP1200_BOR)
step(model_GPP1200_BOR)
#Wmean_N remains

model_GPP1200_P1<- lmer(GPP1200 ~  Wmean_N + Wmean_C + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight +
                           (1|Site/turfID), data=CO2_mass_traits_P1)
summary(model_GPP1200_P1)
step(model_GPP1200_P1)
# Wmean_N + Wmean_logHeight remains

model_GPP1200_P2<- lmer(GPP1200 ~  Wmean_N + Wmean_C + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight +
                          (1|Site/turfID), data=CO2_mass_traits_P2)
summary(model_GPP1200_P2)
step(model_GPP1200_P2)
# Wmean_CN + Wmean_logHeight remains

model_GPP1200_P3<- lmer(GPP1200 ~  Wmean_N + Wmean_C + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight +
                          (1|Site/turfID), data=CO2_mass_traits_P3)
summary(model_GPP1200_P3)
step(model_GPP1200_P3)
# Wmean_LDMC + Wmean_logHeight remains

model_GPP1200_P4<- lmer(GPP1200 ~  Wmean_N + Wmean_C + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight +
                          (1|Site/turfID), data=CO2_mass_traits_P4)
summary(model_GPP1200_P4)
step(model_GPP1200_P4)
# Wmean_LDMC + Wmean_logSLA remains


##### model standardized RECO for separate climate levels
model_RECO15_ALP<- lmer(Reco15 ~  Wmean_N + Wmean_C + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight +
                           (1|Site/turfID), data=CO2_mass_traits_ALP)
summary(model_RECO15_ALP)
step(model_RECO15_ALP)
# Wmean_N + Wmean_C + Wmean_Lth + Wmean_logHeight remain

model_RECO15_SUB<- lmer(Reco15 ~  Wmean_N + Wmean_C + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight +
                           (1|Site/turfID), data=CO2_mass_traits_SUB)
summary(model_RECO15_SUB)
step(model_RECO15_SUB)
# Wmean_CN + Wmean_LA  remain

model_RECO15_BOR<- lmer(Reco15 ~  Wmean_N + Wmean_C + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight +
                           (1|Site/turfID), data=CO2_mass_traits_BOR)
summary(model_RECO15_BOR)
step(model_RECO15_BOR)
# Wmean_C remains

model_RECO15_P1<- lmer(Reco15 ~  Wmean_N + Wmean_C + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight +
                          (1|Site/turfID), data=CO2_mass_traits_P1)
summary(model_RECO15_P1)
step(model_RECO15_P1)
# Error in stepprocedure

model_RECO15_P2<- lmer(Reco15 ~  Wmean_N + Wmean_C + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight +
                          (1|Site/turfID), data=CO2_mass_traits_P2)
summary(model_RECO15_P2)
step(model_RECO15_P2)
# Wmean_LA remains

model_RECO15_P3<- lmer(Reco15 ~  Wmean_N + Wmean_C + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight +
                          (1|Site/turfID), data=CO2_mass_traits_P3)
summary(model_RECO15_P3)
step(model_RECO15_P3)
# Error in stepprocedure

model_RECO15_P4<- lmer(Reco15 ~  Wmean_N + Wmean_C + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight +
                          (1|Site/turfID), data=CO2_mass_traits_P4)
summary(model_RECO15_P4)
step(model_RECO15_P4)
#  Error in stepprocedure


###### model for specific site
CO2_mass_traits_Hog <- subset(CO2_mass_traits, Site == "Hog")
model_GPP1200_Hog<- lmer(GPP1200 ~ scale(B_c.biomass) + Wmean_CN + Wmean_LDMC + (1|block/turfID), data=CO2_mass_traits_Hog)
summary(model_GPP1200_Hog)
r.squaredGLMM(model_GPP1200_Hog)
# R2m 0.31 R2c 0.33

model_GPP1200_Hog<- lmer(GPP1200 ~  Wmean_CN + Wmean_LDMC + Wmean_logSLA + Wmean_logHeight +
                         (1|block/turfID), data=CO2_mass_traits_Hog)
summary(model_GPP1200_Hog)
r.squaredGLMM(model_GPP1200_Hog)
# R2m 0.27 R2c 0.32


model_RECO15_Hog<- lmer(Reco15 ~  Wmean_N + Wmean_C + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight +
                         (1|block/turfID), data=CO2_mass_traits_Hog)
summary(model_RECO15_Hog)
r.squaredGLMM(model_RECO15_Hog)
# R2m 0.11 R2c 0.15

model_RECO15_Hog<- lmer(Reco15 ~  Wmean_CN + Wmean_LA + (1|block/turfID), data=CO2_mass_traits_Hog)
summary(model_RECO15_Hog)
r.squaredGLMM(model_RECO15_Hog)
# R2m 0.11 R2c 0.15

########### model with CWM +  Wvar traits for recalculated fluxes GPP1200 and Reco15
model_GPP1200_Full<- lmer(GPP1200 ~  Wmean_N + Wmean_C + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight
                          + Wvar_logSLA + Wvar_logHeight+ Wvar_CN + Wvar_C + Wvar_N + (1|Site/turfID), data=CO2_mass_traits)
summary(model_GPP1200_Full)
step(model_GPP1200_Full)

model_RECO15_Full<- lmer(Reco15 ~  Wmean_N + Wmean_C + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight
                          + Wvar_logSLA + Wvar_logHeight+ Wvar_CN + Wvar_C + Wvar_N + (1|Site/turfID), data=CO2_mass_traits)
summary(model_RECO15_Full)
step(model_RECO15_Full)

#! These two traits remain after dropping least significant Wmean_trait one by one

model_Reco15_Full <- lmer(Reco15 ~  Wmean_N + Wmean_C + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_logSLA + Wmean_logHeight +
                            (1|Site/turfID), data=CO2_mass_traits)
summary(model_Reco15_Full)
step(model_Reco15_Full)

model_Reco15_Final <- lmer(Reco15~  Wmean_CN + (1|Site/turfID), data=CO2_mass_traits)
summary(model_Reco15_Final)
#! Wmean_CN remains after dropping least significant Wmean_trait one by one




######### ATTEMPT to RLQ analysis ####
library(ade4)
#creating dataframe plot x species of specific years
("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\funcabCompDataWithTTCs.RData")
Species_RLQ<- comp2
Species_RLQ <- Species_RLQ %>%
  filter(Year > 2014) %>%
  mutate(turfID_Year = paste(turfID, Year))%>%
  select(turfID_Year, siteID, turfID, Year, species, cover)
#remove duplicate rows
Species_RLQ <- Species_RLQ<-Species_RLQ[!duplicated(Species_RLQ[,c( 'turfID_Year','species')]),] %>%
  spread(species, cover, fill=NA)

#write.table(Species_RLQ, "O:\\FunCab\\Data\\FunCaB\\TraitCO2\\Species_RLQ.txt")

#creating dataframe species x traits for specific sites
("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\funcabCompDataWithTTCs.RData")

Trait_RLQ <- traitdata %>%
  select(Site, Species, SLA_mean, logSLA_mean, Lth_mean, Height_mean, logHeight_mean, LDMC_mean, LA_mean, N_mean, C_mean, CN_ratio_mean, logCN_ratio_mean)

#write.table(Trait_RLQ, "O:\\FunCab\\Data\\FunCaB\\TraitCO2\\Trait_RLQ.txt")

Env_RLQ<- read.csv("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\Env_RLQ.csv", header=TRUE, sep= ";", dec=",")
species <- Species_RLQ[,-c(1:4)]
env <- Env_RLQ[,-c(1)]
trait<- Trait_RLQ[,-c(1)]


