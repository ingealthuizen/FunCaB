## Making subsets for comparison ##

com_CO2 <- CO2_mass_traits %>%
  select(Wmean_CN, Wmean_LDMC, Wmean_Lth, Wmean_LA, Wmean_SLA, Wmean_Height, cover, vegHeight, Reco15, GPP1200, total.biomass)


#### Checking for correlations ####

GGally::ggpairs(com_CO2)

#### Making linear models ####

library(lme4)
library(lmerTest)

model_0 <- lmer(GPP1200 ~ total.biomass + T_level + P_level + (1|site/turfID), data=CO2_mass_traits)

#summary(model_0)
#anova(model_0)

model_CN <- lmer(GPP1200 ~ total.biomass + T_level + P_level + Wmean_CN+ (1|site/turfID), data=CO2_mass_traits)

#summary(model_CN)
#anova(model_CN)

model_LDMC <- lmer(GPP1200 ~ total.biomass + T_level + P_level + Wmean_LDMC + (1|site/turfID), data=CO2_mass_traits)

#summary(model_LDMC)
#anova(model_LDMC)

model_Lth <- lmer(GPP1200 ~ total.biomass + T_level + P_level + Wmean_Lth + (1|site/turfID), data=CO2_mass_traits)

#summary(model_Lth)
#anova(model_Lth)

model_LA <- lmer(GPP1200 ~ total.biomass + T_level + P_level + Wmean_LA + (1|site/turfID), data=CO2_mass_traits)

#summary(model_LA)
#anova(model_LA)

model_SLA <- lmer(GPP1200 ~ total.biomass + T_level + P_level + Wmean_SLA + (1|site/turfID), data=CO2_mass_traits)

#summary(model_SLA)
#anova(model_SLA)

model_Height <- lmer(GPP1200 ~ total.biomass + T_level + P_level + Wmean_Height + (1|site/turfID), data=CO2_mass_traits)

#summary(model_Height)
#anova(model_Height)

model_all_traits <- lmer(GPP1200 ~ total.biomass + T_level + P_level + Wmean_CN + scale(Wmean_LDMC) + scale(Wmean_Lth) + Wmean_LA + Wmean_SLA + Wmean_Height + (1|site/turfID), data=CO2_mass_traits)

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

ggplot(CO2_mass_traits, aes(x=total.biomass, y=GPP1200, col=T_level))+
  geom_point()


ggplot(CO2_traits_community, aes(y=GPP1200, x=site, fill=T_level))+
  geom_boxplot()+
  facet_wrap(~ T_level, scales="free_x")

biomass%>%
  ggplot(aes(x=plotID, y=dry.weight, fill=functional.group))+
  geom_col()+
  facet_wrap(~site)

CO2_mass_traits%>%
  ggplot(aes(x= site, y=Wmean_CN, col= T_level))+
  geom_point()

CO2_mass_traits%>%
  ggplot(aes(x= site, y=GPP1200, col= T_level))+
  geom_point()

CO2_mass_traits%>%
  ggplot(aes(x= Wmean_CN, y=GPP1200, col= T_level))+
  geom_point()+
  geom_smooth(aes(group=1), method="lm")

