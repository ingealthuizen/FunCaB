#####

ggplot(data=CO2_mass_traits_median)+
  geom_point(aes(y=gram_cover, x=P.mm, col=T_level), size= 4, na.rm= TRUE)+
  geom_smooth(aes(y=gram_cover, x=P.mm, col=T_level), method= "lm")

ggplot(data=CO2_mass_traits_median)+
  geom_point(aes(y=gram_cover, x=Temp.C, col=P_level), size= 4, na.rm= TRUE)+
  geom_smooth(aes(y=gram_cover, x=Temp.C, col=P_level), method= "lm")
  
ggplot(data=CO2_mass_traits_median)+
  geom_point(aes(y=forb_cover, x=P.mm, col=T_level), size= 4, na.rm= TRUE)+
  geom_smooth(aes(y=forb_cover, x=P.mm, col=T_level), method= "lm")

ggplot(data=CO2_mass_traits_median)+
  geom_point(aes(y=forb_cover, x=Temp.C, col=P_level), size= 4, na.rm= TRUE)+
  geom_smooth(aes(y=forb_cover, x=Temp.C, col=P_level), method= "lm")

ggplot(data=CO2_mass_traits_median)+
  geom_point(aes(y=Vasc_cover, x=P.mm, col=T_level), size= 4, na.rm= TRUE)+
  geom_smooth(aes(y=Vasc_cover, x=P.mm, col=T_level), method= "lm")

ggplot(data=CO2_mass_traits_median)+
  geom_point(aes(y=Vasc_cover, x=Temp.C, col=P_level), size= 4, na.rm= TRUE)+
  geom_smooth(aes(y=Vasc_cover, x=Temp.C, col=P_level), method= "lm")

ggplot(data=CO2_mass_traits_median)+
  geom_point(aes(y=VegetationHeight, x=Temp.C, col=P_level), size= 4, na.rm= TRUE)+
  geom_smooth(aes(y=VegetationHeight, x=Temp.C, col=P_level), method= "lm")

ggplot(data=CO2_mass_traits_median)+
  geom_point(aes(y=VegetationHeight, x=P.mm, col=T_level), size= 4, na.rm= TRUE)+
  geom_smooth(aes(y=VegetationHeight, x=P.mm, col=T_level), method= "lm")


###### Functional group cover- biomass relationships
ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=Gram_biomass+Forb_biomass, x=t_coverFG_vegH, col=Temp.C, shape = P_level), size= 4, na.rm= TRUE)+
  geom_smooth(aes(y=Gram_biomass+Forb_biomass, x=t_coverFG_vegH), method= "lm", formula= y~ 0 + x)+
  scale_color_gradient2(low = "blue", high = "red", mid="yellow", midpoint = 8.5) +
  scale_x_continuous(limits = c(0, 2500))+
  scale_y_continuous(limits = c(0, 600))

ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=Gram_biomass+Forb_biomass, x=total_FGcover, col=Temp.C, shape = P_level), size= 4, na.rm= TRUE)+
  geom_smooth(aes(y=Gram_biomass+Forb_biomass, x=total_FGcover), method= "lm", formula= y~ 0 + x)+
  scale_color_gradient2(low = "blue", high = "red", mid="yellow", midpoint = 8.5) +
  scale_x_continuous(limits = c(0, 200))+
  scale_y_continuous(limits = c(0, 400))



ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=Gram_biomass, x=gram_cover, col=Temp.C, shape = P_level), size= 4, na.rm= TRUE)+
  geom_smooth(aes(y=Gram_biomass, x=gram_cover), method= "lm")+
  scale_color_gradient2(low = "blue", high = "red", mid="yellow", midpoint = 8.5) 

ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=Forb_biomass, x=forb_cover, col=Temp.C, shape = P_level), size= 4, na.rm= TRUE)+
  geom_smooth(aes(y=Forb_biomass, x=forb_cover), method= "lm")+
  scale_color_gradient2(low = "blue", high = "red", mid="yellow", midpoint = 8.5) 

ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=Bryo_biomass, x=Bryo_cover, col=Temp.C, shape = P_level), size= , na.rm= TRUE)+
  geom_smooth(aes(y=Bryo_biomass, x=Bryo_cover), method= "lm")+
  scale_color_gradient2(low = "blue", high = "red", mid="yellow", midpoint = 8.5) 


ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=Gram_biomass, x=G_CoverxVegH, col=Temp.C, shape = P_level), size= 4, na.rm= TRUE)+
  geom_smooth(aes(y=Gram_biomass, x=G_CoverxVegH), method= "lm")+
  scale_color_gradient2(low = "blue", high = "red", mid="yellow", midpoint = 8.5) 

ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=Forb_biomass, x=F_CoverxVegH, col=Temp.C, shape = P_level), size= 4, na.rm= TRUE)+
  geom_smooth(aes(y=Forb_biomass, x=F_CoverxVegH), method= "lm")+
  scale_color_gradient2(low = "blue", high = "red", mid="yellow", midpoint = 8.5) 


####### BOXPLOT harvested above-ground biomass XC plots across grid

ggplot(data=CO2_mass_traits)+
  geom_boxplot(aes(y=total.biomass, x= P_level, col= T_level))

# Vascular biomass across grid  
ggplot(data=CO2_mass_traits)+
  geom_boxplot(aes(y=Gram_biomass+Forb_biomass, x= P_level, col= T_level))

# non-vascular BRYO biomass across grid  
ggplot(data=CO2_mass_traits)+
  geom_boxplot(aes(y=Bryo_biomass, x= P_level, col= T_level))

ggplot(data=CO2_mass_traits)+
  geom_boxplot(aes(y=Gram_biomass, x= P_level, col= T_level))

ggplot(data=CO2_mass_traits)+
  geom_boxplot(aes(y=Forb_biomass, x= P_level, col= T_level))

ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=Gram_biomass, x= P.mm, col= T_level))+
  geom_smooth(aes(y=Gram_biomass, x= P.mm, col= T_level), method="lm")

ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=Forb_biomass, x= P.mm, col= T_level))+
  geom_smooth(aes(y=Forb_biomass, x= P.mm, col= T_level), method="lm")

#calculate percentage of biomass from each functional group
CO2_mass_traits$Forb_perc.biomass <- CO2_mass_traits$Forb_biomass/CO2_mass_traits$total.biomass
CO2_mass_traits$Gram_perc.biomass <- CO2_mass_traits$Gram_biomass/CO2_mass_traits$total.biomass
CO2_mass_traits$Bryo_perc.biomass<- CO2_mass_traits$Bryo_biomass/CO2_mass_traits$total.biomass


ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=Gram_perc.biomass, x= P.mm, col= T_level))+
  geom_smooth(aes(y=Gram_perc.biomass, x= P.mm, col= T_level), method="lm")

ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=Forb_perc.biomass, x= P.mm, col= T_level))+
  geom_smooth(aes(y=Forb_perc.biomass, x= P.mm, col= T_level), method="lm")


### Biomass relations across grid
ggplot(data=biomass_XC_long)+
  geom_boxplot(aes(y=dry.weight, x=P_level, col=T_level))+
  facet_grid(~functional.group)

ggplot(data=biomass_XC_long)+
  geom_boxplot(aes(y=total.biomass, x=T_level))
#total biomass increases with T_level


###### Estimated biomass based on cover-biomass relation 
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

ggplot(data=CO2_mass_traits, aes(y=G_c.biomass, x=Temp.C, col= P_level ))+ #x=P.mm, col= T_level
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


##########
########## explore trait variation across grid
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

ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=Wmean_logHeight, x=P.mm, col=T_level, shape = P_level), size= 2, na.rm= TRUE)+
  geom_smooth(aes(y=Wmean_logHeight, x=P.mm, col=T_level), method= "lm")

ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=Wmean_logSLA, x=P.mm, col=T_level, shape = P_level), size= 2, na.rm= TRUE)+
  geom_smooth(aes(y=Wmean_logSLA, x=P.mm, col=T_level), method= "lm")

ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=Wmean_LDMC, x=P.mm, col=T_level, shape = P_level), size= 2, na.rm= TRUE)+
  geom_smooth(aes(y=Wmean_LDMC, x=P.mm, col=T_level), method= "lm")

ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=Wmean_CN, x=P.mm, col=T_level, shape = P_level), size= 2, na.rm= TRUE)+
  geom_smooth(aes(y=Wmean_CN, x=P.mm, col=T_level), method= "lm")

ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=Wmean_Lth, x=P.mm, col=T_level, shape = P_level), size= 2, na.rm= TRUE)+
  geom_smooth(aes(y=Wmean_Lth, x=P.mm, col=T_level), method= "lm")

ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=Wmean_LA, x=P.mm, col=T_level, shape = P_level), size= 2, na.rm= TRUE)+
  geom_smooth(aes(y=Wmean_LA, x=P.mm, col=T_level), method= "lm")

ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=Wmean_logHeight, x=Temp.C, col=P_level, shape = P_level), size= 2, na.rm= TRUE)+
  geom_smooth(aes(y=Wmean_logHeight, x=Temp.C, col=P_level), method= "lm")

ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=Wmean_logSLA, x=Temp.C, col=P_level, shape = P_level), size= 2, na.rm= TRUE)+
  geom_smooth(aes(y=Wmean_logSLA, x=Temp.C, col=P_level), method= "lm")

ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=Wmean_LDMC, x=Temp.C, col=P_level, shape = P_level), size= 2, na.rm= TRUE)+
  geom_smooth(aes(y=Wmean_LDMC, x=Temp.C, col=P_level), method= "lm")

ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=Wmean_CN, x=Temp.C, col=P_level, shape = P_level), size= 2, na.rm= TRUE)+
  geom_smooth(aes(y=Wmean_CN, x=Temp.C, col=P_level), method= "lm")

ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=Wmean_Lth, x=Temp.C, col=P_level, shape = P_level), size= 2, na.rm= TRUE)+
  geom_smooth(aes(y=Wmean_Lth, x=Temp.C, col=P_level), method= "lm")

ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=Wmean_LA, x=Temp.C, col=P_level, shape = P_level), size= 2, na.rm= TRUE)+
  geom_smooth(aes(y=Wmean_LA, x=Temp.C, col=P_level), method= "lm")


####################################################################################################################################
### GPP and RECO across grid
ggplot(data = CO2_mass_traits, aes(x=P_level, y = GPP, col=Year))+
  geom_boxplot()+
  facet_wrap(~T_level)

ggplot(data = CO2_mass_traits, aes(x=P_level, y = Reco, col=Year))+
  geom_boxplot()+
  facet_wrap(~T_level)

ggplot(data = CO2_mass_traits, aes(x=P_level, y = GPP1200, col=Year))+
  geom_boxplot()+
  facet_wrap(~T_level)

ggplot(data = CO2_mass_traits, aes(x=P_level, y = Reco15, col=Year))+
  geom_boxplot()+
  facet_wrap(~T_level)

#### RECO vs T and GPP vs PAR relation across grid
ggplot(CO2_mass_traits, aes(x=tempK, y=Reco, col=factor(P_level)))+
  geom_point(na.rm= TRUE)+
  geom_smooth(method = "nls", formula= y~A*exp(-308.56/I(x-227.13)), method.args = list(start=c(A=0)), size=2, se=FALSE, na.rm= TRUE)+
  #facet_wrap(~T_level)+   # , scales= "free"
  theme_bw()

ggplot(CO2_mass_traits, aes(x=PAR, y=GPP, col= factor(T_level)))+
  geom_point(na.rm= TRUE)+
  geom_smooth(method = "nls", formula= y~(A*B*x)/(A*x+B), method.args = list(start=c(A=0.01, B=2)), size=2, se=FALSE, na.rm= TRUE)+
  #facet_wrap(~T_level)+
  theme_bw()

ggplot(CO2_mass_traits, aes(x=P_level, y=GPP, col= T_level))+
  geom_boxplot()

ggplot(CO2_mass_traits, aes(x=P_level, y=GPP1200, col= T_level))+
  geom_boxplot()

ggplot(CO2_mass_traits, aes(x=P_level, y=Reco, col= T_level))+
  geom_boxplot()

ggplot(CO2_mass_traits, aes(x=P_level, y=Reco15, col= T_level))+
  geom_boxplot()

####################################################################################################################################
ggplot(CO2_mass_traits_median, aes(x=P.mm, y=Reco15, col= T_level))+
  geom_point()

#### OBSERVED VS PREDICTED 
ggplot(CO2_mass_traits_median, aes(x= GPPA, y= GPP1200))+
  geom_point(aes(fill= factor(T_level) , shape = factor(P_level)), size= 5, width = 0.1)+
  geom_abline(intercept = 0, slope = +1, color="black", size=1.5)+
  labs(x= expression(paste("Predicted ", (italic("GPP1200")))), y = expression(paste("Observed ", (italic("GPP1200")))))+
  #facet_wrap(~year)+
  scale_fill_manual(values =c("dodgerblue","lightgreen", "red" ),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"))+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  scale_x_continuous(limits = c(0,16))+
  scale_y_continuous(limits = c(0,16))+
  #annotate("text", x=0.41, y=0.37, label= "b)", size =7)+
  guides(fill=guide_legend(override.aes=list(size=6)))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = "none", strip.text.x = element_text(size = 18))

ggplot(CO2_mass_traits_median, aes(x= GPPC, y= GPP1200))+
  geom_point(aes(fill= factor(T_level) , shape = factor(P_level)), size= 5, width = 0.1)+
  geom_abline(intercept = 0, slope = +1, color="black", size=1.5)+
  labs(x= expression(paste("Predicted ", (italic("GPP1200")))), y = expression(paste("Observed ", (italic("GPP1200")))))+
  #facet_wrap(~year)+
  scale_fill_manual(values =c("dodgerblue","lightgreen", "red" ),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"))+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  scale_x_continuous(limits = c(0,16))+
  scale_y_continuous(limits = c(0,16))+
  #annotate("text", x=0.41, y=0.37, label= "b)", size =7)+
  guides(fill=guide_legend(override.aes=list(size=6)))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = "none", strip.text.x = element_text(size = 18))


####################################################################################################################################
####### NEE, GPP and RECO in relation to BIOMASS and traits


ggplot(CO2_mass_traits_median, aes(x=Wmean_CN , y=GPP1200, col= P_level))+
  geom_point(na.rm= TRUE)+
  geom_smooth(method = "lm")

ggplot(CO2_mass_traits_median, aes(x=Wmean_logHeight, y=GPP1200, col= T_level))+
  geom_point(na.rm= TRUE)+
  geom_smooth(method = "lm")

ggplot(CO2_mass_traits_median, aes(x=Wmean_CN, y=Reco15, col= T_level))+
  geom_point(na.rm= TRUE)+
  geom_smooth(method = "lm")
