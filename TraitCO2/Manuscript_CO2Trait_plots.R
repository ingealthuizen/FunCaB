library(ggthemes)
library(plotly)
library(readxl)
#install.packages("gtable")
library(gtable)


# Forb and Gram cover and Vegetation Height along Temp and Prec gradients
# first calculate mean and sd per Site

CO2_mass_traits_SiteMean<- CO2_mass_traits%>%
  select(Site, P_level, T_level, Temp.C, P.mm, GPP1200, Reco15, gram_cover, forb_cover, Vasc_cover, VegetationHeight, Bryo_biomass, Gram_biomass, Forb_biomass, Wmean_LDMC, Wmean_Lth, Wmean_LA, Wmean_logSLA, Wmean_logHeight, Wmean_CN)%>%
  group_by(Site, P_level, T_level, Temp.C, P.mm) %>%
  summarise_all(c("mean", "sd"), na.rm= TRUE)

ggplot(data=CO2_mass_traits_median, aes(y=GPP700, x=Site, col =Year))+
  geom_boxplot()

#### functional group cover biomass relation ############################
ggplot(data=CO2_mass_traits, aes(y=Gram_biomass, x=Gram_cover, col=Temp.C))+
  geom_point(aes(shape = P_level), size= 4, na.rm= TRUE)+
  geom_smooth(method= "lm", formula = y ~ 0 + x )+
  scale_color_gradient2(low = "blue", high = "red", mid="yellow", midpoint = 8.5)+
  scale_x_continuous(limits = c(0,100))+
  scale_y_continuous(limits = c(0,300))+
  #annotate("text", x=15, y=250, label= "R2 = 0.90, p<0.001", size =5)+
  theme_classic()

ggplot(data=CO2_mass_traits, aes(y=Forb_biomass, x=Forb_cover, col=Temp.C))+
  geom_point(aes(shape = P_level), size= 4, na.rm= TRUE)+
  geom_smooth(method= "lm", formula = y ~ 0 + x )+
  scale_color_gradient2(low = "blue", high = "red", mid="yellow", midpoint = 8.5)+
  scale_x_continuous(limits = c(0,100))+
  scale_y_continuous(limits = c(0,300))+
  #annotate("text", x=15, y=250, label= "R2 = 0.83, p<0.001", size =5)+
  theme_classic()

ggplot(data=CO2_mass_traits, aes(y=Bryo_biomass, x=Bryo_cover, col=Temp.C))+
  geom_point(aes(shape = P_level), size= 4, na.rm= TRUE)+
  geom_smooth(method= "lm", formula = y ~ 0 + x )+
  scale_color_gradient2(low = "blue", high = "red", mid="yellow", midpoint = 8.5)+
  scale_x_continuous(limits = c(0,100))+
  scale_y_continuous(limits = c(0,300))+
  #annotate("text", x=15, y=250, label= "R2 = 0.83, p<0.001", size =5)+
  theme_classic()

gplot(data=CO2_mass_traits)+
  geom_point(aes(y=Forb_biomass+Gram_biomass, x=Vasc_cover, col=Temp.C, shape = P_level), size= 4, na.rm= TRUE)+
  geom_smooth(aes(y=Forb_biomass+Gram_biomass, x=Vasc_cover), method= "lm")+
  scale_color_gradient2(low = "blue", high = "red", mid="yellow", midpoint = 8.5)

sm<-scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#525252"),
                        name="Precipitation level", 
                        breaks=c("1", "2", "3", "4"), 
                        labels = c("1", "2", "3", "4")) +
    scale_shape_manual(values = c(24, 21, 22, 25),
                       name="Precipitation level", 
                       breaks=c("1", "2", "3", "4"), 
                       labels = c("1", "2", "3", "4")) +
    scale_linetype_manual(values = c("b", "f", "g", "a"),
                       name="Precipitation level", 
                       breaks=c("1", "2", "3", "4"), 
                      labels = c("1", "2", "3", "4"))

install.packages("ggfortify")
df<- CO2_mass_traits_median%>%
   ungroup()%>%
    dplyr::select(VegetationHeight, forb_cover:Wvar_CN)
autoplot(prcomp(df))



#### Variance within and between site; Traits and Biomass predictors
load("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\funcabComposition.RData") 
## correct height data of specific plots
subVals <- !is.na(comp2$vegetationHeight) <= 10
comp2$vegetationHeight[subVals] <- comp2$vegetationHeight[subVals] * 10
comp2$vegetationHeight[subVals] <- comp2$vegetationHeight[subVals] * 10
comp2<-comp2%>%
  mutate(species=gsub("\\.", "_", species))%>%
  mutate(Site= substr(siteID, 1,3))%>%
  mutate(species =paste0(Site,"_", species))
  
community_traits <- left_join(comp2, Species_traits, by= c("Site" = "Site" , "species" = "Species"))%>%
  mutate(T_level = recode(Site, Ulv = "1", Lav = "1",  Gud = "1", Skj = "1", Alr = "2", Hog = "2", Ram = "2", Ves = "2", Fau = "3", Vik = "3", Arh = "3", Ovs = "3")) %>%
  select(Site, T_level, turfID, Year,  species, cover, C, N, CN, SLA, Lth, LDMC, LA, Height, vegetationHeight, forbCov, graminoidCov, richness, diversity, evenness)%>%
  ungroup()


library("ape")
library("nlme")
community_traits%>%
  gather(key = pred, value = value, -c(functionalGroup:cover))%>%
  group_by(pred)%>%
  do(tidy(anova(lm(value ~ Site, data=. )))) 


CWM_traits<- CWM_traits%>%
  mutate(Site= substr(turfID, 1,3))%>%
  mutate(T_level = recode(Site, Ulv = "1", Lav = "1",  Gud = "1", Skj = "1", Alr = "2", Hog = "2", Ram = "2", Ves = "2", Fau = "3", Vik = "3", Arh = "3", Ovs = "3")) 

varSLA_spa <- lme(Wmean_SLA~T_level, random= ~1|Site, data=CO2_mass_traits_median, na.action = na.omit)
summary(varSLA_spa)

SLA_Fixed_var <- var(predict(varSLA_spa, level = 0))
SLA_spa <- varcomp(varSLA_spa)
SLA_total_var <- (sum(as.numeric(SLA_spa)))+SLA_Fixed_var

# Variation within site
SLA_var_1<-as.numeric(SLA_spa["Within"])/SLA_total_var
#Variation between sites
SLA_var_2<-as.numeric(SLA_spa["Site"])/SLA_total_var
#Variation between temperature level
SLA_var_3<-SLA_Fixed_var/SLA_total_var

#### LDMC ####
varLDMC_spa <- lme(Wmean_LDMC~T_level, random= ~1|Site, data=CO2_mass_traits_median, na.action = na.omit)
summary(varLDMC_spa)


LDMC_Fixed_var <- var(predict(varLDMC_spa, level = 0))
LDMC_spa <- varcomp(varLDMC_spa)

LDMC_total_var<-(sum(as.numeric(LDMC_spa)))+LDMC_Fixed_var

# Variation within site
LDMC_var_1<-as.numeric(LDMC_spa["Within"])/LDMC_total_var
#Variation between sites
LDMC_var_2<-as.numeric(LDMC_spa["Site"])/LDMC_total_var
#Variation between temperature level
LDMC_var_3<-SLA_Fixed_var/SLA_total_var

### Leaf thickness ###
varLth_spa <- lme(Wmean_Lth~T_level, random= ~1|Site, data=CO2_mass_traits_median, na.action = na.omit)
summary(varLth_spa)

Lth_Fixed_var<-var(predict(varLth_spa, level = 0))
#sem.model.fits(varLth_spa)

Lth_spa <- varcomp(varLth_spa)

Lth_total_var<-(sum(as.numeric(Lth_spa)))+Lth_Fixed_var

# Variation within site
Lth_var_1<-as.numeric(Lth_spa["Within"])/Lth_total_var
#Variation between sites
Lth_var_2<-as.numeric(Lth_spa["Site"])/Lth_total_var
#Variation between temperature level
Lth_var_3<-SLA_Fixed_var/SLA_total_var

### C/N ratio ###
varCN_spa <- lme(Wmean_CN~T_level, random= ~1|Site, data=CO2_mass_traits_median, na.action = na.omit)
summary(varCN_spa)

CN_Fixed_var<-var(predict(varCN_spa, level = 0))
#sem.model.fits(varCN_spa)

CN_spa <- varcomp(varCN_spa)

CN_total_var<-(sum(as.numeric(CN_spa)))+CN_Fixed_var

# Variation within site
CN_var_1<-as.numeric(CN_spa["Within"])/CN_total_var
#Variation between sites
CN_var_2<-as.numeric(CN_spa["Site"])/CN_total_var
#Variation between temperature level
CN_var_3<-SLA_Fixed_var/SLA_total_var

### Height graminoid ###
varHeight_spa <- lme(Wmean_Height~T_level, random= ~1|Site, data=CO2_mass_traits_median, na.action = na.omit)
summary(varHeight_spa)

H_Fixed_var<-var(predict(varHeight_spa, level = 0))

H_spa <- varcomp(varHeight_spa)

H_total_var<-(sum(as.numeric(H_spa)))+H_Fixed_var

# Variation within site
H_var_1<-as.numeric(H_spa["Within"]/H_total_var)
#Variation between sites
H_var_2<-as.numeric(H_spa["Site"]/H_total_var)
#Variation between temperature level
H_var_3<-SLA_Fixed_var/SLA_total_var

Variance2<-c(SLA_var_3, SLA_var_2, SLA_var_1, LDMC_var_3, LDMC_var_2, LDMC_var_1, Lth_var_3, Lth_var_2, Lth_var_1, CN_var_3, CN_var_2, CN_var_1, H_var_3, H_var_2, H_var_1)

Traits2<-c("SLA","SLA","SLA", "LDMC","LDMC","LDMC", "Lth","Lth","Lth", "CN","CN","CN", "VH","VH","VH")
Spacial<-c("aTemp_level", "bSite", "cWithin", "aTemp_level", "bSite", "cWithin", "aTemp_level", "bSite", "cWithin", "aTemp_level", "bSite", "cWithin","aTemp_level", "bSite", "cWithin")

Col_vec<-(c("Traits", "Variance", "Spacial scale"))

spatial<- data.frame(matrix(vector(), 15, 3,
                            dimnames=list(c(), Col_vec)),stringsAsFactors=F)


spatial[,2]<-Variance2
spatial[,1]<-Traits2
spatial[,3]<-Spacial

ggplot(data=spatial, aes(x=Traits, y=Variance, fill=Spacial))+
  geom_bar(stat="identity")+
  theme_minimal()+
  scale_fill_manual(labels = c("Temperature level", "Site", "Within"), values=rev(c("#FF6666","#FFCC33","#99CCFF")))+
  scale_x_discrete(labels=c("C/N ratio", "VH", "LDMC", "Leaf thickness", "SLA"))+
  theme(axis.text.x= element_text(angle=90))+
  labs(x="", y="Proportion of variance", fill="")



#### Effect size plots ####
library(lme4)

labels <- c(GPP700 = "GPP", Reco15 = "Reco")


p1<-CO2_mass_traits_median%>%
  ungroup()%>%
  select(Reco15, GPP700, Site, Year, VegetationHeight, forb_cover:Wvar_CN)%>%
  rename(GPP = GPP700, Reco = Reco15)%>%
  gather(key = pred, value = value, -c(GPP, Reco, Site, Year))%>%
  gather(key = response, value = Cflux, -c(Site, Year, pred, value))%>%
  group_by(response, pred)%>%
  mutate(value = scale(value))%>%
    do(tidy(lmer(Cflux ~ value + (1|Year/Site), data=. ))) %>%
    filter(!term == "(Intercept)") %>% 
    filter(!grepl("^sd_", term)) %>% 
    mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>%
    as.data.frame()

postscript(file = "O:\\FunCab\\Manuscripts\\CO2trait\\figures\\Figure1.eps", width = 8.3, height = 5.8)
        ggplot(p1, aes(x =pred, y = estimate, shape=response, fill= response , ymin = lower, ymax = upper)) +
        geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
        geom_hline(yintercept = 0, linetype = "solid", color="grey") +
        geom_vline(xintercept = c(8.5, 16.5, 22.5), linetype = "dotted", size =1) +
        geom_point(position = position_dodge(width = 0.5), size = 3) +
        scale_shape_manual(labels = c("GPP", "Reco"), values = c(24,21)) + 
        scale_fill_manual(labels = c("GPP", "Reco"), values = c("grey70", "black")) + 
        scale_x_discrete(limits=c( "Wvar_CN", "Wvar_N", "Wvar_C", "Wvar_Height", "Wvar_SLA", "Wvar_LDMC", "Wvar_Lth", "Wvar_LA", "Wmean_CN", "Wmean_N", "Wmean_C", "Wmean_Height", "Wmean_SLA", "Wmean_LDMC", "Wmean_Lth", "Wmean_LA", "richness", "evenness", "diversity","forb_cover", "gram_cover", "VegetationHeight", "Temp.C", "P.mm"), labels=c( "Wvar_CN" = "LCNvar", "Wvar_N" = "LNvar", "Wvar_C"= "LCvar", "Wvar_Height"= "Hvar", "Wvar_SLA" = "SLAvar", "Wvar_LDMC" = "LDMCvar", "Wvar_Lth" = "LTvar", "Wvar_LA" = "LAvar", "Wmean_CN" = "LCNmean", "Wmean_N" = "LNmean", "Wmean_C"= "LCmean", "Wmean_Height"= "Hmean", "Wmean_SLA" = "SLAmean", "Wmean_LDMC" = "LDMCmean", "Wmean_Lth" = "LTmean", "Wmean_LA" = "LAmean", "richness"= "Richness", "evenness" = "Evenness", "diversity"= "Diversity","forb_cover" = "Forb cover", "gram_cover" = "Graminoid cover", "VegetationHeight" = "Vegetation Height", "Temp.C" = "Temperature", "P.mm" = "Precipitation"))+
          labs(x= "             CWV traits                CWM traits             Veg structure      Climate")+
  facet_grid(~response)+ #, labeller=labeller(response = labels)
       theme(axis.title.x=element_text(size = 12), axis.text.x=element_text(size = 12), axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.y=element_text(size = 12), strip.background = element_rect(colour="black", fill="white"), panel.background = element_rect(fill= "white"), panel.border = element_rect(colour = "black", fill=NA), strip.text.x = element_text(size=12, face="bold"),  axis.line = element_line(colour = "black"), legend.position = "none" )+
coord_flip()
dev.off()


GPP.effect<- tidy(lmer(GPP700 ~ VegetationHeight + gram_cover + forb_cover + Wmean_N + Wmean_C + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_Height+ Wmean_SLA + (1|Year/Site), data=CO2_mass_traits_median)) # p<0.05

Reco.effect<- tidy(lmer(Reco15 ~ VegetationHeight + gram_cover + forb_cover + Wmean_N + Wmean_C + Wmean_CN + Wmean_LDMC + Wmean_Lth + Wmean_LA + Wmean_Height+ Wmean_SLA + (1|Year/Site), data=CO2_mass_traits_median)) 

GPP.effect %>% 
  filter(!term == "(Intercept)") %>% 
  filter(!grepl("^sd_", term)) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>%
  as.data.frame() %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  #scale_shape_manual(legend.TOD, values = c(21,24)) + #, labels = c("0.6","2.7","6.5","10.5")
  #scale_fill_manual(legend.TOD, values = c("grey90", "black")) + #guide = guide_legend(reverse=TRUE)
  coord_flip() 

##### variance decomposition ####
Variance_D<- read_excel("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\VarianceDecomp.xlsx")

# Total model(F+R) = T_perc,Fixed effect =F_perc, Random effect = R_effect, Partial perc = P_perc 
Variance_D<- Variance_D %>%
  mutate(F_perc = R2m*100) %>%
  mutate(T_perc = R2c*100) %>%
  mutate(R_perc = T_perc- F_perc) %>%
  select(-R2m, -R2c, -Variance_expl, -T_perc)%>%
  group_by(Response, Model)%>%
  gather( "Percentage",  F_perc:R_perc, key = Modelpart)%>%
  ungroup()

Variance_D<- Variance_D[(which(nchar(Variance_D$Model) != 2)),]

# Plot fixed vs Random effect part of models 
Variance_D%>%
  filter(Response == "GPP")%>%
  ggplot(aes(x=factor(Model, levels = c("C", "B", "A", "ABC", "O")), y =Percentage, fill= factor(Modelpart, levels= c("R_perc", "F_perc")))) +
  geom_bar( stat = "identity", width = 0.80) +
  scale_fill_grey(start = 0.8, end = 0)+
  coord_flip() +
  ylab("% Variance explained")+ 
  scale_x_discrete("", labels = c("O"= "Null", "ABC"= "Combined" ,"A" = "Climate", "B"= "Biomass", "C" = "Traits" ))+
  scale_y_continuous(expand = c(0, 0),limits = c(0,52))+
  theme_bw()+
  theme(axis.title.x=element_text(size = 35), axis.text.x=element_text(size = 30), axis.title = element_text(size = 35), axis.text.y = element_text(size = 30), legend.position = "none", strip.text.x = element_text(size = 18))

Variance_D%>%
  filter(Response == "RECO")%>%
  ggplot(aes(x=factor(Model, levels = c("C", "B", "A", "ABC", "O")), y =Percentage, fill= factor(Modelpart, levels= c("R_perc", "F_perc")))) +
  geom_bar( stat = "identity", width = 0.80) +
  scale_fill_grey(start = 0.8, end = 0)+
  coord_flip() +
  ylab("% Variance explained")+ 
  scale_x_discrete("", labels = c("O"= "Null", "ABC"= "Combined" ,"A" = "Climate", "B"= "Biomass", "C" = "Traits" ))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,52))+
  theme_bw()+
  theme(axis.title.x=element_text(size = 35), axis.text.x=element_text(size = 30), axis.title = element_text(size = 35), axis.text.y = element_text(size = 30), legend.position = "none", strip.text.x = element_text(size = 18))

Variance_D%>%
  filter(Response == "ANPP")%>%
  ggplot(aes(x=factor(Model, levels = c("C", "B", "A", "ABC", "O")), y =Percentage, fill= factor(Modelpart, levels= c("R_perc", "F_perc")))) +
  geom_bar( stat = "identity", width = 0.80) +
  scale_fill_grey(start = 0.8, end = 0)+
  coord_flip() +
  ylab("% Variance explained")+ 
  scale_x_discrete("", labels = c("O"= "Null", "ABC"= "Combined" ,"A" = "Climate", "B"= "Biomass", "C" = "Traits" ))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,70))+
  theme_bw()+
  theme(axis.title.x=element_text(size = 35), axis.text.x=element_text(size = 30), axis.title = element_text(size = 35), axis.text.y = element_text(size = 30), legend.position = "none", strip.text.x = element_text(size = 18))

####### make Partial Variance decomposition #############################################################################################################
Variance_P<- read_excel("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\VarianceDecomp2.xlsx")

# Total model(F+R) = T_perc,Fixed effect =F_perc, Random effect = R_effect, Partial perc = P_perc 
postscript(file = "O:\\FunCab\\Manuscripts\\CO2trait\\figures\\Figure2a.eps", width = 10.2, height = 3.2)
ggplot(Variance_P, aes(x=factor(Model, levels = c("C", "B", "A")), col= factor(ID)))+
  geom_errorbar(data= subset(Variance_P, Response == "GPP" & ID == "abc"), aes(ymin=start, ymax=end), size = 20, width=0, position = position_dodge(0.5))+
  geom_errorbar(data= subset(Variance_P, Response == "GPP" & ID == "ac"), aes(ymin=start, ymax=end), size = 20, width=0, position = position_dodge(0.5))+
  geom_errorbar(data= subset(Variance_P, Response == "GPP" & ID == "bc"), aes(ymin=start, ymax=end), size = 20, width=0, position = position_dodge(0.5))+
  geom_errorbar(data= subset(Variance_P, Response == "GPP" & ID == "uniqueB"), aes(ymin=start, ymax=end), size = 20, width=0, position = position_dodge(0.5))+
  geom_errorbar(data= subset(Variance_P, Response == "GPP" & ID == "uniqueC"), aes(ymin=start, ymax=end), size = 20, width=0, position = position_dodge(0.5))+
  geom_hline(yintercept = 28.3, size = 1.5, linetype = "dashed")+
  coord_flip() +
  #scale_color_brewer(palette="RdYlGn")+
  scale_color_manual(name="", 
                     breaks=c("abc", "ac", "bc", "uniqueB","uniqueC" ), 
                     labels = c("shared C-V-T", "shared C-T", "shared V-T", "distinct V", "distinct T"),
                    values =c("#336699","#99CCCC", "#99CC00", "#339933", "#FFCC00"))+
  labs(x = "GPP", y = "% Variance explained")+ #99CC66"
  ggtitle("          Between-site                                                      Within-site")+
  scale_x_discrete( labels = c("A" = "  C", "B"= "  V", "C" = "  T"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,52))+
  annotate("text", x=3.4, y=2, label= "a)", size =6)+
  theme_classic()+
  theme(panel.border = element_rect(colour = "black", fill=NA), plot.title = element_text(size=18), axis.title.x=element_text(size = 16), axis.text.x=element_text(size = 14), axis.title = element_text(size = 16), axis.title.y=element_text(size = 16), axis.text.y = element_text(size = 14), legend.position = "none", strip.text.x = element_text(size = 14))
dev.off()

postscript(file = "O:\\FunCab\\Manuscripts\\CO2trait\\figures\\Figure2b.eps", width = 10.1, height = 3.5)
ggplot(Variance_P, aes(x=factor(Model, levels = c("C", "B", "A")), col= factor(ID)))+
  geom_errorbar(data= subset(Variance_P, Response == "RECO" & ID == "unique"), aes(ymin=start, ymax=end), size = 20, width=0, position = position_dodge(0.5))+
  geom_errorbar(data= subset(Variance_P, Response == "RECO" & ID == "abc"), aes(ymin=start, ymax=end), size = 20, width=0, position = position_dodge(0.5))+
  geom_errorbar(data= subset(Variance_P, Response == "RECO" & ID == "ac"), aes(ymin=start, ymax=end), size = 20, width=0, position = position_dodge(0.5))+
  geom_errorbar(data= subset(Variance_P, Response == "RECO" & ID == "bc"), aes(ymin=start, ymax=end), size = 20, width=0, position = position_dodge(0.5))+
  geom_errorbar(data= subset(Variance_P, Response == "RECO" & ID == "uniqueA"), aes(ymin=start, ymax=end), size = 20, width=0, position = position_dodge(0.5))+
  geom_errorbar(data= subset(Variance_P, Response == "RECO" & ID == "uniqueB"), aes(ymin=start, ymax=end), size = 20, width=0, position = position_dodge(0.5))+
  geom_errorbar(data= subset(Variance_P, Response == "RECO" & ID == "uniqueC"), aes(ymin=start, ymax=end), size = 20, width=0, position = position_dodge(0.5))+
  geom_hline(yintercept = 25.8, size = 1.5, linetype = "dashed")+
  coord_flip() +
  scale_color_manual(name="", 
                     breaks=c("abc", "ac", "bc","uniqueA", "uniqueB","uniqueC" ), 
                     labels = c("shared C-V-T", "shared C-T", "shared V-T", "distinct C", "distinct V", "distinct T"),
                     values =c("#336699","#99CCCC", "#99CC00","#3399CC", "#339933", "#FFCC00"))+
  labs(x = "Reco", y = "% Variance explained")+
  ggtitle("          Between-site                                                         Within-site")+
  scale_x_discrete(labels = c("A" = "  C", "B"= "  V", "C" = "T"), expand=c(0.8, 0))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,52))+
  annotate("text", x=3.4, y=2, label= "b)", size =6)+
  theme_classic()+
  theme(panel.border = element_rect(colour = "black", fill=NA), plot.title = element_text(size=18),axis.title.x=element_text(size = 16), axis.text.x=element_text(size = 14), axis.title = element_text(size = 16), axis.text.y = element_text(size = 14), legend.position = c(0.9, 0.5), legend.background = element_blank(), legend.box.background = element_rect(colour = "black"), legend.text=element_text(size=14),  strip.text.x = element_text(size = 14))
dev.off()

Variance_P%>%
  filter(Response == "ANPP" )%>%
  ggplot(aes(x=factor(Model, levels = c("C", "B", "A", "ABC"))))+
  geom_errorbar(aes(ymin=start, ymax=end), size= 21, width=0, position = position_dodge(0.5))+
  coord_flip() +
  ylab("% Variance explained")+ 
  scale_x_discrete("", labels = c( "ABC"= "Combined" ,"A" = "Climate", "B"= "Biomass", "C" = "Traits" ))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,70))+
  theme_classic()+
  theme(panel.border = element_rect(colour = "black", fill=NA), axis.title.x=element_text(size = 35), axis.text.x=element_text(size = 30), axis.title = element_text(size = 35), axis.text.y = element_text(size = 30), legend.position = "none", strip.text.x = element_text(size = 18))



#### Predictor vs Climate gradients ####
c1<-ggplot(CO2_mass_traits_median, aes(x= Temp.C, y=Wmean_CN, shape= P_level, linetype=P_level, fill= P_level))+ 
  geom_point(size = 3, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#525252"),
                    name="Precipitation level", 
                    breaks=c("1", "2", "3", "4"), 
                    labels = c("1", "2", "3", "4")) +
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  scale_linetype_manual(values = c(3, 6, 2, 1),
                        name="Precipitation level", 
                        breaks=c("1", "2", "3", "4"), 
                        labels = c("1", "2", "3", "4"))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 20), axis.text.x=element_text(size = 16), axis.title = element_text(size = 20), axis.text.y = element_text(size = 16), legend.position = c(0.15, 0.8), legend.title = element_text(size=12), legend.text = element_text(size=12), strip.text.x = element_text(size = 15))

c2 <- ggplot(CO2_mass_traits_median, aes(x= Temp.C, y=Wmean_C, shape= P_level, linetype=P_level, fill= P_level))+ 
  geom_point(size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#525252"),
                    name="Precipitation level", 
                    breaks=c("1", "2", "3", "4"), 
                    labels = c("1", "2", "3", "4")) +
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  scale_linetype_manual(values = c(3, 6, 2, 1),
                        name="Precipitation level", 
                        breaks=c("1", "2", "3", "4"), 
                        labels = c("1", "2", "3", "4"))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 20), axis.text.x=element_text(size = 16), axis.title = element_text(size = 20), axis.text.y = element_text(size = 16), legend.position = "none", strip.text.x = element_text(size = 15))

c3 <- ggplot(CO2_mass_traits_median, aes(x= Temp.C, y=Wmean_SLA, shape= P_level, linetype=P_level, col= P_level))+ 
  geom_point(fill = "grey", size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), axis.text.y = element_text(size = 20), legend.position = "none", strip.text.x = element_text(size = 15))

c4 <- ggplot(CO2_mass_traits_median, aes(x= Temp.C, y=Wmean_LA, shape= P_level, linetype=P_level, col= P_level))+ 
  geom_point(fill = "grey", size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), axis.text.y = element_text(size = 20), legend.position = "none", strip.text.x = element_text(size = 15))

c5 <- ggplot(CO2_mass_traits_median, aes(x= Temp.C, y=Wmean_LDMC, shape= P_level, linetype=P_level, col= P_level))+ 
  geom_point(fill = "grey", size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), axis.text.y = element_text(size = 20), legend.position = "none", strip.text.x = element_text(size = 15))


c6 <- ggplot(CO2_mass_traits_median, aes(x= Temp.C, y=Wmean_CN, shape= P_level, linetype=P_level, col= P_level))+ 
  geom_point(fill = "grey", size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), axis.text.y = element_text(size = 20), legend.position = "none", strip.text.x = element_text(size = 15))

ggplot(CO2_mass_traits_median, aes(x= P.mm, y=Wmean_LDMC, col = T_level))+ 
  geom_point()+
  geom_smooth(method = "lm" )

ggplot(CO2_mass_traits_median, aes(x= P.mm, y=Wmean_Lth, col=T_level))+ 
  geom_point()+
  geom_smooth(method = "lm")



##### GPP vs predictors ####
p1 <- ggplot(data=CO2_mass_traits_median, aes(y=GPP700, x=Temp.C, col=T_level))+
  geom_point(fill = "grey",  shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  ylab(bquote('GPP ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')'))+
  xlab(bquote("Summer Temperature (°C)"))+
  #annotate("text", x=7.5, y=20, label= "R2 = 0.108, p<0.01")+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), axis.text.y = element_text(size = 20), legend.position = "none", strip.text.x = element_text(size = 15))

p2<- ggplot(data=CO2_mass_traits_median, aes(y=GPP700, x=VegetationHeight, col=T_level))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  #annotate("text", x=60, y=20, label= "R2 = 0.092, p<0.001")+
  theme_classic()+
  xlab(bquote("Vegetation height (mm)"))+
  ylab(NULL) +
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), legend.position = "none", axis.text.y = element_blank())
p2

p3<- ggplot(data=CO2_mass_traits_median, aes(y=GPP700, x=forb_cover, col=T_level))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  #annotate("text", x=40, y=20, label= "R2 = 0.22, p<0.01")+
  theme_classic()+
  xlab(bquote("Forb cover (%)"))+
  ylab(NULL) +
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), legend.position = "none", axis.text.y = element_blank())
p3

p4 <- ggplot(data=CO2_mass_traits_median, aes(y=GPP700, x=gram_cover, col=T_level))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  #annotate("text", x=15, y=250, label= "R2 = 0.83, p<0.001")+
  theme_classic()+
  xlab(bquote("Graminoid cover (%)"))+
  ylab(NULL) +
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), legend.position = "none", axis.text.y = element_blank())
p4

p5 <- ggplot(data=CO2_mass_traits_median, aes(y=GPP700, x=Wmean_LA, col=T_level))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  #annotate("text", x=2, y=20, label= "R2 = 0.023, p<0.01")+
  theme_classic()+
  ylab(bquote('GPP ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')'))+
  xlab(bquote(" Leaf area ("*~cm^-2*")"))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), axis.text.y = element_text(size = 20), legend.position = "none", strip.text.x = element_text(size = 15))
p5

p6<- ggplot(data=CO2_mass_traits_median, aes(y=GPP700, x=Wmean_Height, col=T_level))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  #annotate("text", x=5.6, y=20, label= "R2 = 0.0.046, p<0.01")+
  theme_classic()+
  xlab(bquote(" Vegetative Height (mm)"))+
  ylab(NULL) +
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), legend.position = "none", axis.text.y = element_blank())
p6

p7 <- ggplot(data=CO2_mass_traits_median, aes(y=GPP700, x=Wmean_SLA, col=T_level))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  #annotate("text", x=180, y=20, label= "R2 = 0.018, p<0.05")+
  theme_classic()+
  xlab(bquote(" Specific Leaf Area ("*~cm^-2/g*")"))+
  ylab(NULL) +
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), legend.position = "none", axis.text.y = element_blank())
p7

p8<- ggplot(data=CO2_mass_traits_median, aes(y=GPP700, x=Wmean_Lth, col=T_level))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  #annotate("text", x=15, y=20, label= "R2 = 0.83, p<0.001")+
  theme_classic()+
  xlab(bquote(" Leaf Thickness (mm)"))+
  ylab(NULL) +
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), legend.position = "none", axis.text.y = element_blank())
p8

p9 <- ggplot(data=CO2_mass_traits_median, aes(y=GPP700, x=Wmean_LDMC, col=T_level))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  #annotate("text", x=0.4, y=20, label= "R2 = 0.023, p<0.01")+
  theme_classic()+
  ylab(bquote('GPP ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')'))+
  xlab(bquote("Leaf dry matter content ("*mg~g^-1*")"))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), axis.text.y = element_text(size = 20), legend.position = "none", strip.text.x = element_text(size = 15))
p9

p10 <- ggplot(data=CO2_mass_traits_median, aes(y=GPP700, x=Wmean_C, col=T_level))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  #annotate("text", x=15, y=20, label= "R2 = 0.83, p<0.001")+
  theme_classic()+
  xlab(bquote(" Leaf C content  ("*g~kg^-1*")"))+
  ylab(NULL) +
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), legend.position = "none", axis.text.y = element_blank())
p10

p11 <- ggplot(data=CO2_mass_traits_median, aes(y=GPP700, x=Wmean_N, col=T_level))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  #annotate("text", x=2.7, y=20, label= "R2 = 0.023, p<0.05")+
  theme_classic()+
  xlab(bquote(" Leaf N content  ("*g~kg^-1*")"))+
  ylab(NULL) +
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), legend.position = "none", axis.text.y = element_blank())
p11

p12 <- ggplot(data=CO2_mass_traits_median, aes(y=GPP700, x=Wmean_CN, col=T_level))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  #annotate("text", x=34, y=20, label= "R2 = 0.029, p<0.01")+
  theme_classic()+
  xlab(bquote(" Leaf CN ratio "))+
  ylab(NULL) +
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), legend.position = "none", axis.text.y = element_blank())
p12

p13 <- ggplot(data=CO2_mass_traits_median, aes(y=GPP700, x=Wvar_LA, col=T_level))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  #annotate("text", x=34, y=20, label= "R2 = 0.029, p<0.01")+
  theme_classic()+
  xlab(bquote(" Fvar LA "))+
  ylab(NULL) +
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), legend.position = "none", axis.text.y = element_blank())

p14 <- ggplot(data=CO2_mass_traits_median, aes(y=GPP700, x=Wvar_LDMC, col=T_level))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  #annotate("text", x=34, y=20, label= "R2 = 0.029, p<0.01")+
  theme_classic()+
  xlab(bquote(" Fvar LDMC "))+
  ylab(NULL) +
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), legend.position = "none", axis.text.y = element_blank())

p15 <- ggplot(data=CO2_mass_traits_median, aes(y=GPP700, x=Wvar_CN, col=T_level))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  #annotate("text", x=34, y=20, label= "R2 = 0.029, p<0.01")+
  theme_classic()+
  xlab(bquote(" Fvar CN "))+
  ylab(NULL) +
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), legend.position = "none", axis.text.y = element_blank())


th1<- theme(axis.title.x=element_text(size = 14), axis.text.x=element_text(size = 12), axis.title = element_text(size = 14), legend.position = "none", axis.text.y = element_text(size =12))
th2<- theme(axis.title.x=element_text(size = 14), axis.text.x=element_text(size = 12), axis.title = element_text(size = 14), legend.position = "none", axis.text.y = element_blank())

x11(height = 450/25.4, width = 400/25.4)
cowplot::plot_grid(p1 + th1, p2+ th2, p3+ th2, p4+ th2, p5+ th1, p6+ th2, p7+ th2, p8+ th2, p9+ th1, p10+ th2, p11+ th2, p12+ th2,p13+ th2, p14+ th2, p15+ th2, nrow = 4,  ncol=4, align = "h")
cowplot::save_plot("O:\\FunCab\\Manuscripts\\CO2Trait\\figures\\FigureX.eps", Figure3 , base_height = 10, base_width = 12)


##### GPP vs predictors ####
r1 <- ggplot(data=CO2_mass_traits_median, aes(y=Reco15, x=Temp.C))+
  geom_point(fill = "grey",  shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  #geom_smooth(method= "lm", color= "black", se = FALSE)+
  ylab(bquote('Reco ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')'))+
  xlab(bquote("Summer Temperature (°C)"))+
  #annotate("text", x=7.5, y=10, label= "R2 = 0.108, p<0.01")+
  theme_classic()

r2<- ggplot(data=CO2_mass_traits_median, aes(y=Reco15, x=VegetationHeight))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  annotate("text", x=60, y=10, label= "R2 = 0.031, p<0.01")+
  theme_classic()+
  xlab(bquote("Vegetation height (mm)"))+
  ylab(NULL) 

r3<- ggplot(data=CO2_mass_traits_median, aes(y=Reco15, x=forb_cover))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  annotate("text", x=40, y=10, label= "R2 = 0.011, p<0.05")+
  theme_classic()+
  xlab(bquote("Forb cover (%)"))+
  ylab(NULL) 

r4 <- ggplot(data=CO2_mass_traits_median, aes(y=Reco15, x=gram_cover))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  annotate("text", x=25, y=10, label= "R2 = 0.007, p<0.05")+
  theme_classic()+
  xlab(bquote("Graminoid cover (%)"))+
  ylab(NULL) 

r5 <- ggplot(data=CO2_mass_traits_median, aes(y=Reco15, x=Wmean_LA))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  #geom_smooth(method= "lm", color= "black", se = FALSE)+
  #annotate("text", x=2, y=10, label= "R2 = 0.023, p<0.01")+
  theme_classic()+
  ylab(bquote('Reco ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')'))+
  xlab(bquote(" Leaf area ("*~cm^-2*")"))+
  theme_classic()

r6<- ggplot(data=CO2_mass_traits_median, aes(y=Reco15, x=Wmean_Height))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  annotate("text", x=5.6, y=10, label= "R2 = 0.008, p<0.05")+
  theme_classic()+
  xlab(bquote(" Vegetative Height (mm)"))+
  ylab(NULL) 

r7 <- ggplot(data=CO2_mass_traits_median, aes(y=Reco15, x=Wmean_SLA))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  #geom_smooth(method= "lm", color= "black", se = FALSE)+
  #annotate("text", x=180, y=10, label= "R2 = 0.018, p<0.05")+
  theme_classic()+
  xlab(bquote(" Specific Leaf Area ("*~cm^-2/g*")"))+
  ylab(NULL) 

r8<- ggplot(data=CO2_mass_traits_median, aes(y=Reco15, x=Wmean_Lth))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  #geom_smooth(method= "lm", color= "black", se = FALSE)+
  #annotate("text", x=15, y=10, label= "R2 = 0.83, p<0.001")+
  theme_classic()+
  xlab(bquote(" Leaf Thickness (mm)"))+
  ylab(NULL) 

r9 <- ggplot(data=CO2_mass_traits_median, aes(y=Reco15, x=Wmean_LDMC))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  #geom_smooth(method= "lm", color= "black", se = FALSE)+
  #annotate("text", x=0.4, y=10, label= "R2 = 0.023, p<0.01")+
  theme_classic()+
  ylab(bquote('Reco ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')'))+
  xlab(bquote("Leaf dry matter content ("*mg~g^-1*")"))

r10 <- ggplot(data=CO2_mass_traits_median, aes(y=Reco15, x=Wmean_C))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  annotate("text", x=45, y=10, label= "R2 = 0.027, p<0.05")+
  theme_classic()+
  xlab(bquote(" Leaf C content  ("*g~kg^-1*")"))+
  ylab(NULL) 

r11 <- ggplot(data=CO2_mass_traits_median, aes(y=Reco15, x=Wmean_N))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  annotate("text", x=2.7, y=10, label= "R2 = 0.020, p<0.05")+
  theme_classic()+
  xlab(bquote(" Leaf N content  ("*g~kg^-1*")"))+
  ylab(NULL) 

r12 <- ggplot(data=CO2_mass_traits_median, aes(y=Reco15, x=Wmean_CN))+
  geom_point(fill = "grey", shape = 21, size = 2, alpha = 0.5, na.rm= TRUE)+
  geom_smooth(method= "lm", color= "black", se = FALSE)+
  annotate("text", x=34, y=10, label= "R2 = 0.022, p<0.05")+
  theme_classic()+
  xlab(bquote(" Leaf CN ratio "))+
  ylab(NULL)

th1<- theme(axis.title.x=element_text(size = 14), axis.text.x=element_text(size = 12), axis.title = element_text(size = 14), legend.position = "none", axis.text.y = element_text(size =12))
th2<- theme(axis.title.x=element_text(size = 14), axis.text.x=element_text(size = 12), axis.title = element_text(size = 14), legend.position = "none", axis.text.y = element_blank())

x11(height = 450/25.4, width = 400/25.4)
cowplot::plot_grid(r1 + th1, r2+ th2, r3+ th2, r4+ th2, r5+ th1, r6+ th2, r7+ th2, r8+ th2, r9+ th1, r10+ th2, r11+ th2, r12+ th2, nrow = 3,  ncol=4, align = "h")
cowplot::save_plot("O:\\FunCab\\Manuscripts\\TBI\\figures\\ProofFigures\\Figure3.eps", Figure3 , base_height = 10, base_width = 12)


#### OBSERVED VS PREDICTED 
ggplot(CO2_mass_traits_median, aes(x= GPPABC, y= GPP700))+
  geom_point(aes(fill= factor(T_level) , shape = factor(P_level)), size= 5, width = 0.1)+
  geom_abline(intercept = 0, slope = +1, color="grey", size=1.5)+
  geom_smooth(method="lm", linetype= "dashed", color = "black", size= 1.5, se=FALSE)+
  labs(x= expression(paste("Predicted GPP")), y = expression(paste("Observed GPP")))+
  #facet_wrap(~year)+
  scale_fill_manual(values =c("dodgerblue","lightgreen", "red" ),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"))+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  scale_x_continuous(breaks = seq(0,25, by=5), limits = c(0, 26))+
  scale_y_continuous(breaks = seq(0,25, by=5), limits = c(0, 26))+
  annotate("text", x=2, y=25, label= "R2 = 0.37", size =6)+
  annotate("text", x=26, y=25, label= "d)", size =6)+
  guides(fill=guide_legend(override.aes=list(size=6)))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = "none", strip.text.x = element_text(size = 18))

ggplot(CO2_mass_traits_median, aes(x= RecoABC2, y= Reco15))+
  geom_point(aes(fill= factor(T_level) , shape = factor(P_level)), size= 5, width = 0.1)+
  geom_abline(intercept = 0, slope = +1, color="grey", size=1.5)+
  geom_smooth(method="lm", linetype= "dashed", color = "black", size= 1.5, se=FALSE)+
  labs(x= expression(paste("Predicted Reco")), y = expression(paste("Observed Reco")))+
  #facet_wrap(~year)+
  scale_fill_manual(values =c("dodgerblue","lightgreen", "red" ),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"))+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  scale_x_continuous(breaks = seq(0,12, by=2), limits = c(0, 12))+
  scale_y_continuous(breaks = seq(0,12, by=2), limits = c(0, 12))+
  annotate("text", x=1, y=12, label= "R2 = 0.18", size =5)+
  annotate("text", x=12, y=11.5, label= "e)", size =6)+
  guides(fill=guide_legend(override.aes=list(size=6)))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = "none", strip.text.x = element_text(size = 18))

####
ggplot(CO2_mass_traits_median, aes(Wmean_Height, Gram_biomass+Forb_biomass, col=T_level, shape =P_level))+
  geom_point()
ggplot(CO2_mass_traits_median, aes(gram_cover, Gram_biomass+Forb_biomass, col=T_level, shape =P_level))+
  geom_point()
ggplot(CO2_mass_traits_median, aes(forb_cover, Gram_biomass+Forb_biomass, col=T_level, shape =P_level))+
  geom_point()

ggplot(CO2_mass_traits_median, aes(forb_cover, GPP700, col=T_level, shape =P_level))+
  geom_point()
ggplot(CO2_mass_traits_median, aes(VegetationHeight, GPP700, col=T_level, shape =P_level))+
  geom_point()
ggplot(CO2_mass_traits_median, aes(Wmean_LA, GPP700, col=T_level, shape =P_level))+
  geom_point()
ggplot(CO2_mass_traits_median, aes(Wmean_N, GPP700, col=T_level, shape =P_level))+
  geom_point()
ggplot(CO2_mass_traits_median, aes(Wmean_Height, GPP700, col=T_level, shape =P_level))+
  geom_point()

ggplot(CO2_mass_traits_median, aes(VegetationHeight, Reco15, col=T_level, shape =P_level))+
  geom_point()
ggplot(CO2_mass_traits_median, aes(Wmean_CN, Reco15, col=T_level, shape =P_level))+
  geom_point()

ggplot(CO2_mass_traits_median, aes(T_level, Forb_biomass))+
  geom_boxplot()

ggplot(CO2_mass_traits_median, aes( forb_cover, GPP700, col=T_level))+
  geom_point()

ggplot(CO2_mass_traits_median, aes(T_level, Reco15, col=P_level))+
  geom_boxplot()

# VegetationHeight
ggplot(CO2_mass_traits_SiteMean, aes(VegetationHeight_mean))+
  geom_errorbar(aes(ymin=GPP1200_mean-GPP1200_sd, ymax=GPP1200_mean+GPP1200_sd), width=.002, position = position_dodge(0.5)) +
  geom_point(aes(y= GPP1200_mean, fill= factor(T_level) , shape = factor(P_level)), size= 5, position = position_dodge(0.5))+
  geom_smooth(aes(y= GPP1200_mean, col= "black"), method = "lm", size=1, se=FALSE)+
  #geom_text(data = ann_textD ,aes(x= 3.3, y= 0.43, label =lab), size =7, inherit.aes = FALSE)+
  #labs(y= "Gram_cover (%) ", x = " Precipitation (mm) ")+
  scale_fill_manual(values =c("dodgerblue", "lightgreen","red"),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"),
                    guide= FALSE)+
  scale_colour_manual(values =c("#000000", "#000000", "#000000","#000000"),
                      name="Temperature level", 
                      breaks=c("1", "2", "3", "4"), 
                      labels = c("ALP", "SUB", "BOR", "ALL"),
                      guide = FALSE)+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4"))+
  #theme_few()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), 
        axis.text.y = element_text(size = 22), legend.position = "none", strip.text.x = element_blank())

# VegetationHeight
ggplot(CO2_mass_traits_SiteMean, aes(forb_cover_mean))+
  geom_errorbar(aes(ymin=GPP1200_mean-GPP1200_sd, ymax=GPP1200_mean+GPP1200_sd), width=.002, position = position_dodge(0.5)) +
  geom_point(aes(y= GPP1200_mean, fill= factor(T_level) , shape = factor(P_level)), size= 5, position = position_dodge(0.5))+
  geom_smooth(aes(y= GPP1200_mean, col= "black"), method = "lm", size=1, se=FALSE)+
  #geom_text(data = ann_textD ,aes(x= 3.3, y= 0.43, label =lab), size =7, inherit.aes = FALSE)+
  #labs(y= "Gram_cover (%) ", x = " Precipitation (mm) ")+
  scale_fill_manual(values =c("dodgerblue", "lightgreen","red"),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"),
                    guide= FALSE)+
  scale_colour_manual(values =c("#000000", "#000000", "#000000","#000000"),
                      name="Temperature level", 
                      breaks=c("1", "2", "3", "4"), 
                      labels = c("ALP", "SUB", "BOR", "ALL"),
                      guide = FALSE)+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4"))+
  #theme_few()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), 
        axis.text.y = element_text(size = 22), legend.position = "none", strip.text.x = element_blank())


###########Suplementary Figures
## GPP- PAR relation (Thornley & Johnson, 1990), Reco- T relation (loyd 1994)
ggplot(CO2_GPP_1516Trait, aes(x=PAR.x, y=GPP))+
  geom_point(na.rm= TRUE, shape = 1)+
  geom_smooth(method = "nls", formula= y~(A*B*x)/(A*x+B), method.args = list(start=c(A=0.01, B=2)), se=FALSE, na.rm= TRUE)+
  labs(x= expression(paste("PAR")), y = expression(paste("GPP")))+
  theme_classic()

ggplot(CO2_GPP_1516Trait, aes(x=tempK, y=Reco))+
  geom_point(na.rm= TRUE, shape = 1)+
  geom_smooth(method = "nls", formula= y~A*exp(-308.56/I(x-227.13)), method.args = list(start=c(A=0)), se=FALSE, na.rm= TRUE)+
  labs(x= expression(paste("Temperature (K)")), y = expression(paste("Reco")))+
  theme_classic()




####### CWM Traits - GPP relation### 

ggplot(CO2_mass_traits_SiteMean, aes(Wmean_CN_mean))+
  geom_errorbar(aes(ymin=GPP1200_mean-GPP1200_sd, ymax=GPP1200_mean+GPP1200_sd), width=.2, position = position_dodge(0.5)) +
  geom_point(aes(y= GPP1200_mean, fill= factor(T_level) , shape = factor(P_level)), size= 8, position = position_dodge(0.5))+
  geom_smooth(aes(y= GPP1200_mean, col= "black"), method = "lm", size=1, se=FALSE)+
  #geom_text(data = ann_textD ,aes(x= 3.3, y= 0.43, label =lab), size =7, inherit.aes = FALSE)+
  #labs(y= "Gram_cover (%) ", x = " Precipitation (mm) ")+
  scale_fill_manual(values =c("dodgerblue", "lightgreen","red"),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"),
                    guide= FALSE)+
  scale_colour_manual(values =c("#000000", "#000000", "#000000","#000000"),
                      name="Temperature level", 
                      breaks=c("1", "2", "3", "4"), 
                      labels = c("ALP", "SUB", "BOR", "ALL"),
                      guide = FALSE)+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4"))+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = "none", strip.text.x = element_blank())

####### Climate - Reco relation

ggplot(CO2_mass_traits_SiteMean, aes(Temp.C))+
  geom_errorbar(aes(ymin=Reco15_mean-Reco15_sd, ymax=Reco15_mean+Reco15_sd), width=.2, position = position_dodge(0.5)) +
  geom_point(aes(y= Reco15_mean, fill= factor(T_level) , shape = factor(P_level)), size= 8, position = position_dodge(0.5))+
  geom_smooth(aes(y= Reco15_mean, col= "black"), method = "lm", size=1, se=FALSE)+
  #geom_text(data = ann_textD ,aes(x= 3.3, y= 0.43, label =lab), size =7, inherit.aes = FALSE)+
  #labs(y= "Gram_cover (%) ", x = " Precipitation (mm) ")+
  scale_fill_manual(values =c("dodgerblue", "lightgreen","red"),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"),
                    guide= FALSE)+
  scale_colour_manual(values =c("#000000", "#000000", "#000000","#000000"),
                      name="Temperature level", 
                      breaks=c("1", "2", "3", "4"), 
                      labels = c("ALP", "SUB", "BOR", "ALL"),
                      guide = FALSE)+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4"))+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = "none", strip.text.x = element_blank())

  
####### CWM Traits - Reco relation

ggplot(CO2_mass_traits_SiteMean, aes(Wmean_Lth_mean))+
  geom_errorbar(aes(ymin=Reco15_mean-Reco15_sd, ymax=Reco15_mean+Reco15_sd), width=.005, position = position_dodge(0.5)) +
  geom_point(aes(y= Reco15_mean, fill= factor(T_level) , shape = factor(P_level)), size= 8, position = position_dodge(0.5))+
  geom_smooth(aes(y= Reco15_mean, col= "black"), method = "lm", size=1, se=FALSE)+
  #geom_text(data = ann_textD ,aes(x= 3.3, y= 0.43, label =lab), size =7, inherit.aes = FALSE)+
  #labs(y= "Gram_cover (%) ", x = " Precipitation (mm) ")+
  scale_fill_manual(values =c("dodgerblue", "lightgreen","red"),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"),
                    guide= FALSE)+
  scale_colour_manual(values =c("#000000", "#000000", "#000000","#000000"),
                      name="Temperature level", 
                      breaks=c("1", "2", "3", "4"), 
                      labels = c("ALP", "SUB", "BOR", "ALL"),
                      guide = FALSE)+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4"))+
  scale_x_continuous(limits = c(0.15, 0.3))+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = "none", strip.text.x = element_blank())



