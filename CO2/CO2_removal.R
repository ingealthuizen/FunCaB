setwd("O:\\FunCab\\Data\\FunCaB")
#source functions for import/proces and calculation of CO2 flux
source("CO2/R_functions/import_process_CO2data_2017.R") 
source("CO2/R_functions/CO2flux_calculation.R")
source("CO2/R_functions/CO2_plot.R")
source("CO2/R_functions/import_process_SQlogger_2017.R")
#give exact location of R functions.R file
source("O:\\FunCab\\Data\\FunCaB\\Other\\R_functions\\Highstat_library.R")

library(readxl) #require packages
library(ggplot2)
library(ggthemes)
library(lattice)
library(dplyr)
library(broom)
library(tidyr)

#import all pre removal datafiles from 2017
sites.data.2017Li1400<-read.sitefiles("CO2/Data/Li1400files2017new.xlsx")#!Li1400 dataflagged outliers+new times
sites.data.2017SQ<- read.sitefiles.SQ("CO2/Data/SQfiles2017new.xlsx") #!SQ dataflagged outliers+new time

# check number of columns of metadata 
#sapply(sites.data.2017Li1400, function(x) ncol(x$meta))
#sapply(sites.data.2017Li1400, function(x) (x$meta$NA == NULL))

# Run fluxcalculation on all datafiles of 2017
#fluxcalc(sites.data.2017Li1400[[1]]) #calculate flux 1 plot
CO2_Li1400_2017<-do.call(rbind, lapply(sites.data.2017Li1400, fluxcalc)) #calculate flux for all pre-removal data 2015
CO2_SQ_2017<-do.call(rbind, lapply(sites.data.2017SQ, fluxcalc)) #calculate flux for all data 2016
CO2_Li1400_2017$PAR_est<- as.numeric(CO2_Li1400_2017$PAR)
CO2_SQ_2017$PAR_est<- as.numeric(CO2_SQ_2017$PAR)
CO2_Li1400_2017$PAR<- NULL
CO2_SQ_2017$PAR<- NULL
CO2_Li1400_2017$soilT<- as.numeric(gsub(",", ".", CO2_Li1400_2017$soilT))
CO2_SQ_2017$soilT<- as.numeric(gsub(",", ".", CO2_SQ_2017$soilT))
CO2_Li1400_2017$ToD<-format(CO2_Li1400_2017$starttime, format="%H") #create new Time of Day column 
CO2_SQ_2017$ToD<-format(CO2_SQ_2017$starttime, format="%H") #create new Time of Day column 

# bind together 2015 and 2016 data
CO2_2017<- rbind(CO2_Li1400_2017, CO2_SQ_2017)
#CO2_2017$ToD<-format(CO2_2017$starttime, format="%H") #create new Time of Day column 

CO2_2017<- CO2_2017%>%
  mutate(ToD = format(CO2_2017$starttime, format="%H"))%>%
  mutate(site = recode(site, ULV = "Ulv", ALR = "Alr", FAU = "Fau", LAV = "Lav", HOG = "Hog", VIK = "Vik", GUD = "Gud", RAM = "Ram", ARH = "Arh", 
                       SKJ = "Skj", VES = "Ves", OVS = "Ovs")) %>%
  rename("Date"= "date", "Site"= "site", "Block"= "block", "Treatment" = "treatment")
CO2_2017$TurfID = paste0(CO2_2017$Site, CO2_2017$Block, CO2_2017$Treatment) 

# add columns with precipitation and temperature level for 2015 data
tempV<-c("ALP","ALP","ALP","ALP","SUB","SUB","SUB","SUB","BOR","BOR","BOR","BOR")
names(tempV)<-c("Ulv","Lav","Gud","Skj","Alr","Hog","Ram","Ves","Fau","Vik","Arh","Ovs")
precL<-c("PL1","PL2","PL3","PL4","PL1","PL2","PL3","PL4","PL1","PL2","PL3","PL4")
names(precL)<-c("Ulv","Lav","Gud","Skj","Alr","Hog","Ram","Ves","Fau","Vik","Arh","Ovs")

#tempV[overviewsitesdata$site]
CO2_2017$Tlevel<-tempV[CO2_2017$Site]
CO2_2017$Tlevel = factor(CO2_2017$Tlevel, levels = c("ALP", "SUB", "BOR"))
CO2_2017$Plevel<-precL[CO2_2017$Site]
CO2_2017$Plevel = factor(CO2_2017$Plevel, levels = c("PL1", "PL2", "PL3", "PL4"))
CO2_2017$Treatment <- factor(CO2_2017$Treatment, levels=c("C","B", "G", "F", "GB", "FB", "GF", "FGB"))


# Correct PAR data for measurements when PAR sensor was broken with estimated value
# if cover = "L" & PAR > 200 then use PAR_est value, if cover D and high PAR, correct to estimated value
CO2_2017 <- CO2_2017 %>%
  select(-comment, -flag, -weather)%>%
  mutate(PAR.correct = ifelse(PAR < 200 & cover == "L", PAR_est, ifelse(PAR > 100 & cover == "D", PAR_est,PAR)))%>%
  mutate(Block = as.character(Block))


#### load soilmoisture data 2017 ####
Soil_moisture<- read.table("\\\\eir.uib.no\\home6\\ial008\\FunCab\\Data\\soil moisture\\Soilmoisture2017.txt", header = TRUE, dec = ",")

Soil_moisture<- Soil_moisture %>% 
  mutate(Site = recode(Site, ULV = "Ulv", ALR = "Alr", FAU = "Fau", LAV = "Lav", HOG = "Hog", VIK = "Vik", GUD = "Gud", RAM = "Ram", ARH =           "Arh", SKJ = "Skj", VES = "Ves", OVS = "Ovs"))%>%
  mutate(Moisture_mean =  rowMeans(subset(Soil_moisture, select = c(6, 7, 8, 9)), na.rm = TRUE)) %>%
  mutate(TurfID=paste0(Site, SCBlock, Treatment)) %>%
  select(Date, TurfID, Moisture_mean)%>%
  group_by(Date, TurfID)%>%
  summarise(Moisture_mean = mean(Moisture_mean, na.rm=TRUE))

CO2_2017<- left_join(CO2_2017, Soil_moisture, by = c("Date", "TurfID"))


##### Load vegetation data ####
load("O:\\FunCab\\Data\\FunCaB\\CO2\\Data\\funcabComp12052018.RData") 
#Add missing height data 2017
composition <- within(composition, bryophyteCov[turfID == 'Alr1F' & Year == "2017"] <- 0)
composition <- within(composition, mossHeight[turfID == 'Alr1F' & Year == "2017"] <- 0)
composition <- within(composition, mossHeight[turfID == 'Alr3G' & Year == "2017"] <- 8.6) # NA > average block
composition <- within(composition, mossHeight[turfID == 'Alr5F' & Year == "2017"] <- 1.75) # NA > average block
composition <- within(composition, mossHeight[turfID == 'Alr5G' & Year == "2017"] <- 1.75) # NA > average block
composition <- within(composition, bryophyteCov[turfID == 'Fau5F' & Year == "2017"] <- 0) 
composition <- within(composition, bryophyteCov[turfID == 'Fau2G' & Year == "2017"] <- 28) # NA > average block
composition <- within(composition, mossHeight[turfID == 'Fau2F' & Year == "2017"] <- 4)
composition <- within(composition, mossHeight[turfID == 'Fau2G' & Year == "2017"] <- 4) # NA > average block
composition <- within(composition, mossHeight[turfID == 'Fau5F' & Year == "2017"] <- 0)
composition <- within(composition, mossHeight[turfID == 'Skj2F' & Year == "2017"] <- 7)
composition <- within(composition, mossHeight[turfID == 'Ulv3F' & Year == "2017"] <- 3)
composition <- within(composition, mossHeight[turfID == 'Ulv4C' & Year == "2017"] <- 0)
composition <- within(composition, mossHeight[turfID == 'Alr2GF' & Year == "2017"] <- 3)
composition <- within(composition, mossHeight[turfID == 'Hog4GF' & Year == "2017"] <- 18)
composition <- within(composition, mossHeight[turfID == 'Alr1C' & Year == "2017"] <- 1.5) # NA > average block
composition <- within(composition, vegetationHeight[turfID == 'Alr1C' & Year == "2017"] <- 135)
composition <- within(composition, vegetationHeight[turfID == 'Alr3GB' & Year == "2017"] <- 65)
composition <- within(composition, vegetationHeight[turfID == 'Fau4F' & Year == "2017"] <- 70)
composition <- within(composition, vegetationHeight[turfID == 'Ulv3B' & Year == "2017"] <- 44.5) # NA > average block
composition <- within(composition, vegetationHeight[turfID == 'Ulv3GB' & Year == "2017"] <- 30)
composition <- within(composition, vegetationHeight[turfID == 'Ves1FB' & Year == "2017"] <- 65)
composition <- within(composition, vegetationHeight[turfID == 'Ves2GB' & Year == "2017"] <- 40)
composition <- within(composition, vegetationHeight[turfID == 'Ves3FB' & Year == "2017"] <- 50)

#check<- composition %>%
#  filter(Treatment %in% c("F", "GF", "G", "C"))%>%
#  filter(is.na(mossHeight))%>%
#  filter(Year == "2017")

#check<- composition %>%
#  filter(!Treatment %in% c("FGB", "GF"))%>%
#  filter(is.na(vegetationHeight))%>%
#  filter(Year == "2017")

### FG group cover in control plots 2017
FG_comp_grid<- composition%>%
  mutate(siteID= substr(siteID, 1,3))%>%
  filter(Year == 2017 & Treatment == "C") %>% group_by(siteID)%>%
  summarise(Gram = mean(graminoidCov, na.rm = TRUE),
            Forb = mean(forbCov, na.rm = TRUE),
            Bryo = mean(bryophyteCov, na.rm = TRUE))%>%
  mutate(total.cover = Gram + Forb + Bryo,
         Gram.perc = Gram/total.cover*100,
         Forb.perc = Forb/total.cover*100,
         Bryo.perc = Bryo/total.cover*100)%>%
  select(-Gram, -Forb, -Bryo)%>%
  gather(FG, cover, Gram.perc:Bryo.perc, factor_key=TRUE)

FG_comp_grid$siteID<- factor(FG_comp_grid$siteID, levels=c("Fau", "Vik", "Arh", "Ovs","Alr", "Hog","Ram", "Ves","Ulv", "Lav","Gud", "Skj"))
ggplot(FG_comp_grid, aes(siteID, cover, fill = FG))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("#009E73", "#F0E442", "#0072B2")) + 
  theme(axis.title.x=element_text(size = 14), axis.text.x=element_text(size = 14), axis.title = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.y=element_text(size = 14), strip.background = element_rect(colour="black", fill="white"), panel.background = element_rect(fill= "white"), panel.border = element_rect(colour = "black", fill=NA), strip.text.x = element_text(size=14, face="bold"),  axis.line = element_line(colour = "black"), legend.position = "none" )
  

veg_comp<- composition %>%
  mutate(Site = recode(siteID, Ulvhaugen = "Ulv",  Skjellingahaugen = "Skj", Alrust = "Alr", Hogsete = "Hog",  Veskre = "Ves", Fauske = "Fau", Vikesland = "Vik", Lavisdalen = "Lav", Ovstedal = "Ovs", Gudmedalen = "Gud", Rambera = "Ram", Arhelleren = "Arh"))%>% 
  rowwise() %>%
  filter(Year == 2017)%>% 
  filter(!Site %in% c("Ram", "Arh", "Gud"))%>%
  group_by(Site, blockID)%>%
  select(Site, blockID, Treatment, turfID, bryophyteCov, forbCov, graminoidCov, vegetationHeight, mossHeight)%>%
  distinct()%>%
  ungroup()
  
#### Biomass regression
load("O:\\FunCab\\Data\\FunCaB\\CO2\\Data\\funcabComp2015.RData") 
composition2015 <- within(composition, vegetationHeight[turfID == 'Ram4F'& Year == "2015"] <- 80)
subVals <- !is.na(composition2015$vegetationHeight) <= 10
composition2015$vegetationHeight[subVals] <- composition2015$vegetationHeight[subVals] * 10
composition2015 <- within(composition2015, vegetationHeight[turfID == 'Fau2C' & Year == "2015"] <- 155)
composition2015 <- within(composition2015, vegetationHeight[turfID == 'Alr1C' & Year == "2015"] <- 130)
composition2015 <- within(composition2015, vegetationHeight[turfID == 'Alr2C' & Year == "2015"] <- 197.5)
composition2015 <- within(composition2015, turfID[siteID == 'Lavisdalen' & Year == "2015" & blockID == "1" & Treatment == "G"] <- "Lav1G")


#### Biomass regression based on 2015 removal data
#### load biomass data ####
Biomass_2015<- read_excel("\\\\eir.uib.no\\home6\\ial008\\FunCab\\Data\\Vegetation data\\biomass_removals_2015.xlsx")
Biomass_2015$Biomass_g<- as.numeric(Biomass_2015$Biomass_g)
Biomass_2015<-Biomass_2015%>%
  filter(!grepl("RTC", Treatment))%>%
  spread(key = Func_group, value = Biomass_g)%>%
  select(-Block, -Round)%>%
  mutate(B_g.m2 = B/0.0625,
         G_g.m2 = G/0.0625,
         F_g.m2 = F/0.0625)


# mean mossHeight at site
mossheight.site<-composition2015%>%
  mutate(Site = recode(siteID, Ulvhaugen = "Ulv",  Skjellingahaugen = "Skj", Alrust = "Alr", Hogsete = "Hog",  Veskre = "Ves", Fauske = "Fau", Vikesland = "Vik", Lavisdalen = "Lav", Ovstedal = "Ovs", Gudmedalen = "Gud", Rambera = "Ram", Arhelleren = "Arh"))%>% 
  group_by(Site)%>%
  summarise(mossHeight = mean(mossHeight, na.rm=TRUE))

biomass_cover <- left_join(Biomass_2015, composition2015,  by= c("Year", "Treatment", "TurfID"="turfID"))%>%
  select(Site, Treatment, TurfID, B, F, G, B_g.m2, G_g.m2, F_g.m2, totalGraminoids, totalForbs, totalBryophytes, vegetationHeight)%>%
  #filter(!Site %in% c("Ram", "Gud", "Arh"))%>%
  left_join(mossheight.site, by = "Site")%>%
  mutate(T_level = recode(Site, Ulv = "1", Lav = "1",  Gud = "1", Skj = "1", Alr = "2", Hog = "2", Ram = "2", Ves = "2", Fau = "3", Vik = "3", Arh = "3", Ovs = "3")) %>%
  mutate(P_level = recode(Site, Ulv = "1", Alr = "1", Fau = "1", Lav = "2", Hog = "2", Vik = "2", Gud = "3", Ram = "3", Arh = "3", Skj = "4", Ves = "4", Ovs = "4"))


#### Biomass Regressions Removals; cover + height biomass estimates 2015 ####
# number of observations per Functional group: 192
B_biomass.fit <-lm(B_g.m2 ~ 0 + totalBryophytes + mossHeight , data=biomass_cover) 
summary(B_biomass.fit) #estimate tB=0.0060149 , mH = 0.0121319 R2 = 0.80
G_biomass.fit <-lm(G_g.m2 ~ 0 + totalGraminoids + vegetationHeight, data=biomass_cover)  
summary(G_biomass.fit) #estimate tG=0.0045586 , vH = 0.0020865 R2 = 0.91
F_biomass.fit <-lm(F_g.m2 ~ 0 + totalForbs + vegetationHeight , data=biomass_cover) 
summary(F_biomass.fit) #estimate tF=0.0060959 , vH = 0.0008366 R2 = 0.79

# save predictions of the biomass regression models new data frame together with predictor
predicted_B <- data.frame(pred_B = predict(B_biomass.fit, biomass_cover), mossHeight=biomass_cover$mossHeight)
predicted_G <- data.frame(pred_G = predict(G_biomass.fit, biomass_cover), vegetationHeight=biomass_cover$vegetationHeight)
predicted_F <- data.frame(pred_F = predict(F_biomass.fit, biomass_cover), vegetationHeight=biomass_cover$vegetationHeight)

# this is the predicted line of multiple linear regression
#ggplot(data = biomass_cover, aes(x = mossHeight, y = B_g.m2)) + 
#  geom_point(color='blue') +
#  geom_line(color='red',data = predicted_B, aes(x=mossHeight, y=pred_B))


#### calculate FG biomass based on biomass regressions
veg_comp<- veg_comp%>%
  mutate(G.Height = ifelse(Treatment == "B" | Treatment == "F"| Treatment == "FB", vegetationHeight,0),
         F.Height = ifelse(Treatment == "B" | Treatment == "G"| Treatment == "GB", vegetationHeight,0))%>%
  mutate(B.biomass = 0 + 1.3231*bryophyteCov + 3.1864*mossHeight,
         G.biomass = 0 + 1.04437*graminoidCov + 0.55246*G.Height,
         F.biomass = 0 + 1.37491*forbCov + 0.22363*F.Height)

##################### CO2 flux processing ##########################################################################

#seperate L and D measurements and merge them in new file with new column GPP, selecting data with r2>=.9
CO2_2017_NEE<-subset(CO2_2017, cover== "L" & rsqd>=.8 | cover== "L" & rsqd<=.2 )   
CO2_2017_RECO<-subset(CO2_2017, cover== "D" & rsqd>=.8 | cover== "D" & rsqd<=.2 )  
CO2_2017_RECO$Reco<-CO2_2017_RECO$nee*-1
CO2_2017_RECO$tempK<-CO2_2017_RECO$temp+273.15
CO2_2017_GPP<- inner_join(CO2_2017_NEE, CO2_2017_RECO, by=c("Date", "chamber", "Site", "Block", "Treatment", "ToD", "Tlevel", "Plevel" ))
CO2_2017_GPP$GPP<-CO2_2017_GPP$nee.x- CO2_2017_GPP$nee.y #NEE-Reco
CO2_2017_GPP<-subset(CO2_2017_GPP, GPP>0 & PAR.x >200)

#write.csv(CO2_2017_GPP, file = "O:\\FunCab\\Data\\FunCaB\\CO2\\CO2_GPP_Removal2017.csv")
# Measurements per plot
x<-CO2_2017_GPP %>%
  group_by(Site, Treatment)%>%
  summarize(min.par =min(PAR.x),
            max.par = max(PAR.x))

#### Explore data ####
CO2_2017_GPP%>%
  ggplot(aes(x=Site, y=PAR.correct.x, col= Treatment))+
  geom_boxplot()

CO2_2017_GPP%>%
  ggplot(aes(x=Site, y=tempK, col= Treatment))+
  geom_boxplot()

CO2_2017_GPP%>%
  ggplot(aes(x=Reco, y=tempK, col= Treatment))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  facet_wrap(~Site)

library(lme4)
library(lmerTest)
summary(lmer(GPP~ PAR.correct.x*Treatment + (1|Site), CO2_2017_GPP)) 
summary(lmer(Reco~ tempK*Treatment + (1|Site), CO2_2017_GPP)) 
summary(lmer(nee.x~ tempK*Treatment + PAR.correct.x*Treatment + (1|Site), CO2_2017_GPP)) 


#### calculated median flux values per plot 
CO2_2017_data<- CO2_2017_GPP%>%
  select(Site, Block, Treatment, nee.x, Moisture_mean.x, tempK, PAR.correct.x, Reco, GPP)%>% #-(starttime.y:PAR.correct.y)
  group_by(Site, Block, Treatment)%>%
  summarise(NEE = median(nee.x, na.rm= TRUE),
            GPP = median(GPP, na.rm = TRUE),
            Reco = median(Reco, na.rm = TRUE))
  

#### Combine flux and vegetation data ####"
CO2veg_2017<- left_join(CO2_2017_data, veg_comp, by = c("Site", "Treatment", "Block" = "blockID", "Treatment"))%>%
  filter(!is.na(forbCov))%>%    # removing plots that got damaged and have no vegetation data
  mutate(T_level = recode(Site, Ulv = "Alpine", Lav = "Alpine",  Gud = "Alpine", Skj = "Alpine", Alr = "Sub-alpine", Hog = "Sub-alpine", Ram = "Sub-alpine", Ves = "Sub-alpine", Fau = "Boreal", Vik = "Boreal", Arh = "Boreal", Ovs = "Boreal")) %>%
  mutate(T_level = ordered(T_level, levels = c("Alpine", "Sub-alpine", "Boreal")))%>%
  mutate(P_level = recode(Site, Ulv = "1", Alr = "1", Fau = "1", Lav = "2", Hog = "2", Vik = "2", Gud = "3", Ram = "3", Arh = "3", Skj = "4", Ves = "4", Ovs = "4"))%>%
  mutate(P_level = ordered(P_level, levels = c("1", "2", "3", "4")))%>%
  mutate(Temp.C = recode(Site, Ulv = 6.17, Lav = 6.45,  Gud = 5.87, Skj = 6.58, Alr = 9.14, Hog = 9.17, Ram = 8.77, 
                         Ves = 8.67, Fau = 10.3, Vik = 10.55, Arh = 10.6, Ovs = 10.78))%>%
  mutate(P.mm = recode(Site, Ulv = 596, Alr = 789, Fau = 600, Lav = 1321, Hog = 1356, Vik = 1161, Gud = 1925, 
                       Ram = 1848, Arh = 2044, Skj = 2725, Ves = 3029, Ovs = 2923))%>%
  ungroup()

#write.csv(CO2veg_2017, file = "O:\\FunCab\\Data\\FunCaB\\CO2\\CO2veg_Removal2017.csv")

CO2veg_2017_all<- left_join(CO2_2017_GPP, veg_comp, by = c("Site", "Treatment", "Block" = "blockID", "Treatment"))%>%
  filter(!is.na(forbCov)) # removing plots that got damaged and have no vegetation data

### soil vs aboveground plant respiration
CO2veg_2017%>%
  filter(Treatment %in% c("C", "FGB"))%>%
  group_by(T_level, Treatment)%>%
  summarise(m.Reco = mean(GPP))%>%
  spread(Treatment, m.Reco)%>%
  mutate(diff = (C-FGB)/C*100)


###### ANALYSIS ###############
##### ANALYSIS
library(lme4)
library(lmerTest)
hist(CO2veg_2017$GPP)
hist(CO2veg_2017$Reco)

#### ANOVA with Tuckey HSD 
TukeyHSD(aov(GPP~ Treatment+T_level, data=CO2veg_2017))
TukeyHSD(aov(Reco~ Treatment+T_level, data=CO2veg_2017))

#### Effect of FG removal on Cflux
Removal_T1<- CO2veg_2017%>% filter(T_level == "Alpine")
Removal_T2<- CO2veg_2017%>% filter(T_level == "Sub-alpine")
Removal_T3<- CO2veg_2017%>% filter(T_level == "Boreal")

# test the pairwise comparison between treatments for different T_levels
TukeyHSD(aov(GPP~ Treatment, data=Removal_T1)) 
TukeyHSD(aov(GPP~ Treatment, data=Removal_T2))
TukeyHSD(aov(GPP~ Treatment, data=Removal_T3))      
TukeyHSD(aov(Reco~ Treatment, data=Removal_T1)) 
TukeyHSD(aov(Reco~ Treatment, data=Removal_T2)) 
TukeyHSD(aov(Reco~ Treatment, data=Removal_T3))         
         
         
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(aov(GPP ~ factor(Treatment)+T_level*P_level, data=CO2veg_2017))
plot(aov(Reco ~ factor(Treatment)+T_level*P_level, data=CO2veg_2017))

####
ggplot(CO2veg_2017, aes(Treatment, Reco, fill = T_level ))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(size =1.5,  alpha = 0.6, position = position_jitterdodge(jitter.width = 0.1)) +
  #geom_jitter(width = 0.1,  size =1.5,  alpha = 0.6, dodge)+
  #facet_grid(~T_level)+
  scale_x_discrete(limits=c( "C", "B", "G", "F", "GB", "FB", "GF", "FGB"))+
  labs(y = "Reco µmol/mol/s")+
  theme_classic()+
  scale_fill_manual(values =c("white", "grey70", "grey40"),
                    name="Temperature level", 
                    breaks=c("Alpine", "Sub-alpine", "Boreal"), 
                    labels = c("ALP", "SUB", "BOR"),
                    guide= FALSE)+
  scale_y_continuous(limits = c(0,16))+
  annotate("text", x=8.2, y=15, label= "b)", size =5)+
  theme(axis.title.x=element_text(size = 14), axis.text.x=element_text(size = 12), axis.title = element_text(size = 14), axis.text.y = element_text(size = 12), axis.title.y=element_text(size = 14), strip.background = element_rect(colour="black", fill="white"), panel.background = element_rect(fill= "white"), panel.border = element_rect(colour = "black", fill=NA), strip.text.x = element_text(size=12, face="bold"),  axis.line = element_line(colour = "black"), legend.position = "none" )
  
summary(lmer(GPP~ Treatment*T_level + (1|Site/Block), data=CO2veg_2017)) #


#### Effect of grid and treatment on GPP and Reco
ann_textA <- data.frame(estimate = -6, term = "B",lab=c("a)","b)"),
                        response = factor(c("GPP","Reco")),levels = c("GPP","Reco"))
CO2veg_2017%>%  ungroup()%>%
  mutate(Treatment=replace(Treatment, Treatment == "B", "Moss")) %>%
  #filter(!Treatment %in% c("FGB", "C"))%>%
  select(GPP, Reco, graminoidCov, forbCov, bryophyteCov, Site, Block, Treatment, Temp.C, P.mm, T_level, P_level) %>% 
  gather(key = response, value = Cflux, -c(graminoidCov:P_level))%>%
  na.omit() %>% 
group_by(response)%>%
  do(tidy(lmer(Cflux ~  Treatment + (1|Site/Block), data=. )))%>%
  filter(!term == "(Intercept)") %>% 
  filter(!grepl("^sd_", term)) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>%
  as.data.frame()%>%
  ggplot(aes(x =term, y = estimate, shape = response, fill = response, ymin = lower, ymax = upper)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey", size =1) +
  geom_vline(xintercept = c(1.5, 4.5), linetype = "dotted", size =1) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_shape_manual(labels = c("GPP", "Reco"), values = c(24,21)) + 
  scale_fill_manual(labels = c("GPP", "Reco"), values = c("grey80", "black")) + 
  scale_x_discrete(limits=c( "TreatmentFGB", "TreatmentGF", "TreatmentFB", "TreatmentGB", "TreatmentF", "TreatmentG", "TreatmentMoss"), labels=c( "TreatmentFGB" = "FGB", "TreatmentGF" = "GF", "TreatmentFB"= "FB", "TreatmentGB"= "GB", "TreatmentF"= "F", "TreatmentG"= "G", "TreatmentMoss"= "B"))+
  facet_grid(~response)+ #, labeller=labeller(response = labels)
  geom_text(data = ann_textA ,aes(x= 1, y= 0.4, label =lab), size= 5, inherit.aes = FALSE)+
  theme(axis.title.x=element_text(size = 18), axis.text.x=element_text(size = 14), axis.title = element_text(size = 18), axis.text.y = element_text(size = 14), axis.title.y=element_blank(), strip.background = element_rect(colour="black", fill="white"), panel.background = element_rect(fill= "white"), panel.border = element_rect(colour = "black", fill=NA), strip.text.x = element_text(size=12, face="bold"),  axis.line = element_line(colour = "black"), legend.position = "none" )+
  coord_flip()



### Does effect of FGcover on Cflux change acros grid
# Model: -1, no intercept as GPP will be 0 with no FG cover 
GPP_FG<-CO2veg_2017%>%  ungroup()%>%
  mutate(Treatment=replace(Treatment, Treatment == "B", "Moss")) %>%
  #filter(!Treatment %in% c("FGB", "C"))%>%
  select(GPP, Reco, graminoidCov, forbCov, bryophyteCov, Block, B.biomass, G.biomass, F.biomass, Site, Treatment, Temp.C, P.mm, T_level, P_level) %>% 
  gather(key = response, value = Cflux, -c(graminoidCov:P_level))%>%
  na.omit() %>% 
  filter(response =="GPP")%>%
  group_by(response)%>%
  do(tidy(lmer(Cflux ~ -1 + Temp.C + scale(P.mm) + B.biomass + G.biomass + F.biomass + (1|Site/Block) , data=. )))%>%
  filter(!term == "(Intercept)") %>% 
  filter(!grepl("^sd_", term)) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))%>%
  as.data.frame()

GPP_FG%>%
  ggplot(aes(x =term, y = estimate, shape=response, fill= response, ymin = lower, ymax = upper)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey", size =1) +
  geom_vline(xintercept = c(3.5), linetype = "dotted", size =1) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_shape_manual(labels = c("GPP", "Cflux"), values = c(24,24)) + 
  scale_fill_manual(labels = c("GPP", "GPP"), values = c("grey80", "grey80")) + 
  scale_x_discrete(limits=c("B.biomass",   "G.biomass",  "F.biomass",  "scale(P.mm)", "Temp.C"), labels=c("B.biomass"= "Bryophyte", "G.biomass"= "Graminoid",  "F.biomass"= "Forb", "scale(P.mm)" = "Precipitation",  "Temp.C" = "Temperature"))+
  facet_grid(~response)+ #, labeller=labeller(response = labels)
  scale_y_continuous(limits = c(-1.2,1.2))+
  annotate("text", x=1, y=1.2, label= "a)", size =5)+
  theme(axis.title.x=element_text(size = 14), axis.text.x=element_text(size = 12), axis.title = element_text(size = 14), axis.text.y = element_text(size = 12), axis.title.y=element_blank(), strip.background = element_rect(colour="black", fill="white"), panel.background = element_rect(fill= "white"), panel.border = element_rect(colour = "black", fill=NA), strip.text.x = element_text(size=12, face="bold"),  axis.line = element_line(colour = "black"), legend.position = "none" )+
  coord_flip()


#### If I take out main effects of climate than there is effects of FG cover
Reco_FG<-CO2veg_2017%>%  ungroup()%>%
  mutate(Treatment=replace(Treatment, Treatment == "B", "Moss")) %>%
  #filter(!Treatment %in% c("FGB", "C"))%>%
  select(GPP, Reco, graminoidCov, forbCov, bryophyteCov, Block, B.biomass, G.biomass, F.biomass, Site, Treatment, Temp.C, P.mm, T_level, P_level) %>% 
  gather(key = response, value = Cflux, -c(graminoidCov:P_level))%>%
  na.omit() %>% 
  filter(response =="Reco")%>%
  group_by(response)%>%
  do(tidy(lmer(Cflux ~  Temp.C + scale(P.mm) + B.biomass + G.biomass+ F.biomass + (1|Site/Block) , data=. ))) %>% 
  filter(!term == "(Intercept)") %>% 
  filter(!grepl("^sd_", term)) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>%
  as.data.frame()
  #filter(!term == "Temp.C")%>%

Reco_FG%>%  
ggplot(aes(x =term, y = estimate, shape=response, fill=response, ymin = lower, ymax = upper)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey", size =1) +
  geom_vline(xintercept = c(3.5), linetype = "dotted", size =1) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_shape_manual(labels = c("Cflux", "Reco"), values = c(21,21)) + 
  scale_fill_manual(labels = c("GPP", "Reco"), values = c("black", "black")) + 
  scale_x_discrete(limits=c("B.biomass",   "G.biomass",  "F.biomass",  "scale(P.mm)", "Temp.C"), labels=c("B.biomass"= "Bryophyte", "G.biomass"= "Graminoid",  "F.biomass"= "Forb", "scale(P.mm)" = "Precipitation",  "Temp.C" = "Temperature"))+
  facet_grid(~response)+ #, labeller=labeller(response = labels)
  scale_y_continuous(limits = c(-1.2,1.2))+
  annotate("text", x=1, y=1.2, label= "b)", size =5)+
  theme(axis.title.x=element_text(size = 14), axis.text.x=element_text(size = 12), axis.title = element_text(size = 14), axis.text.y =element_text(size = 14), axis.title.y=element_blank(), strip.background = element_rect(colour="black", fill="white"), panel.background = element_rect(fill= "white"), panel.border = element_rect(colour = "black", fill=NA), strip.text.x = element_text(size=12, face="bold"),  axis.line = element_line(colour = "black"), legend.position = "none" )+
  coord_flip()


### check distribution of Reco with P.mm
ggplot(CO2veg_2017, aes(x = Reco, fill = factor(P_level))) +
  geom_density(alpha = 0.3)+
  facet_grid(~T_level)

#####################################################################################################################
#### CI calculation with biomass
#ED is % contribution by removed FG in intact community
#assumption that per-unit function is same in intact community as when FG is alone
# CI contributions
# 1. changes in biomass of remaining PFGs compared to control situation (gap-recruitment)
# 2. change in per unit function of remaining PFG
# 3. differences in per-unit functioning between removed and remaining PFG (difference in fluxrate)


CO2veg_2017controls<- CO2veg_2017%>%
  filter(Treatment == "C")%>%
  filter(graminoidCov > 0 , forbCov > 0, bryophyteCov > 0 )
  
CO2veg_2017<- CO2veg_2017%>%
  filter(Treatment != "C")%>%
  bind_rows(CO2veg_2017controls)

B.specificF <- CO2veg_2017%>%
  gather(key= Cflux , value = flux, NEE, Reco, GPP)%>%
  group_by(Site, Block, Cflux)%>%
  left_join((.) %>% filter(Treatment == "C")%>% select(Site, Block, Cflux, control= flux))%>%
  group_by(Site, Treatment)%>%
  mutate(presentFG = recode(Treatment, FGB = "zero", GF = "b", FB= "g", GB = "f", B= "gf", G = "fb", F = "gb"))%>%
  mutate(totalBiomass = B.biomass+G.biomass+F.biomass)%>%# calculate cover sum per plot 
  filter(Treatment %in% c("GB" , "FB" , "GF"))%>% # filter for plots with single FG
  gather(key= FG, value= biomass, B.biomass, G.biomass, F.biomass )%>%
  filter((Treatment == "GB" & FG == "F.biomass") |(Treatment == "FB" & FG == "G.biomass") |(Treatment =="GF" & FG == "B.biomass"))%>%
  mutate(SpecificFlux = flux/ biomass)%>%
  ungroup()%>%
  select(Site, Block, FG, Cflux, SpecificFlux)

ggplot(B.specificF, aes(Site, SpecificFlux, col = FG))+
  geom_boxplot()+
  geom_point(pch = 21, position = position_jitterdodge())+
  facet_grid(~Cflux)

B.ED_calc <- CO2veg_2017%>%
  gather(key= Cflux , value = flux, NEE, Reco, GPP)%>%
  gather(key= FG, value= biomass, B.biomass, G.biomass, F.biomass )%>%
  left_join(., B.specificF, by = c("Site", "Block", "FG", "Cflux"))%>%
  group_by(Site, Block, Treatment, turfID, Cflux)%>%
  summarise(treatmentED = sum(biomass*SpecificFlux, na.rm = TRUE))%>%
  ungroup()%>%
  left_join((.) %>% filter(Treatment == "C") %>% select(Site, Block, Cflux, controlED = treatmentED))%>%
  mutate(ED = treatmentED / controlED *100)

B.OD_calc <- CO2veg_2017%>%
  gather(key= Cflux , value = flux, NEE, Reco, GPP)%>%
  left_join((.) %>% filter(Treatment == "C")%>% select(Site, Block, Cflux, controlflux= flux))%>%
  group_by(Site, Block, turfID, Cflux)%>%
  mutate( OD = (controlflux - flux)/controlflux *100)%>%
  ungroup()

B.flux_CI <- left_join(B.OD_calc, B.ED_calc, by = c("Site", "Block", "Treatment", "turfID", "Cflux"))%>%
  mutate(presentFG = recode(Treatment, FGB = "zero", GF = "b", FB= "g", GB = "f", B= "gf", G = "fb", F = "gb", C = "all"))%>%
  group_by(Site, Block, turfID, Cflux)%>%
  mutate(CI = (ED-OD)/ED)%>%
  mutate(T_level = recode(Site, Ulv = "Alpine", Lav = "Alpine",  Gud = "Alpine", Skj = "Alpine", Alr = "Sub-alpine", Hog = "Sub-alpine", Ram = "Sub-alpine", Ves = "Sub-alpine", Fau = "Boreal", Vik = "Boreal", Arh = "Boreal", Ovs = "Boreal")) %>%
  mutate(P_level = recode(Site, Ulv = "600mm", Alr = "600mm", Fau = "600mm", Lav = "1200mm", Hog = "1200mm", Vik = "1200mm", Gud = "2700mm", Ram = "2700mm", Arh = "2000mm", Skj = "2700mm", Ves = "2700mm", Ovs = "2700mm"))

save(B.flux_CI, file = "O:\\FunCab\\Data\\FunCaB2\\CO2\\CI_biomass.RData")

CImean <- B.flux_CI%>%
  group_by(Cflux, presentFG, T_level)%>%
  summarize(CI = mean(CI))%>%
  spread(Cflux, CI)

test.CI.GPP<- B.flux_CI%>% filter(Cflux == "GPP")%>%
  filter(Treatment %in% c("FB" , "GB" , "GF"))
aov.CI.GPP<- aov(CI~ presentFG+T_level, data=test.CI.GPP) #
summary(aov.CI.GPP)

test.CI.Reco<- B.flux_CI%>% filter(Cflux == "Reco")%>%
  filter(Treatment %in% c("FB" , "GB" , "GF"))
aov.CI.Reco<- aov(CI ~ T_level, data=test.CI.Reco) #
summary(aov.CI.Reco)
TukeyHSD(aov.CI.Reco)

### MANUSCRIPT PLOTS
ann_textB<- data.frame(estimate = -6, term = "B",lab=c("a)","b)"),
                        Cflux = factor(c("GPP","Reco"),levels = c("GPP","Reco")))
B.flux_CI%>% filter(!Cflux == "NEE")%>%
  filter(Treatment %in% c("FB" , "GB" , "GF"))%>%
  ggplot( aes(presentFG, CI, fill = T_level))+
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_boxplot(outlier.shape = NA)+
  geom_point(size =1.5,  alpha = 0.6, position = position_jitterdodge(jitter.width = 0.1)  ) +
  scale_fill_manual(values =c("white", "grey70", "grey40"),
                    name="Temperature level", 
                    breaks=c("Alpine", "Sub-alpine", "Boreal"), 
                    labels = c("ALP", "SUB", "BOR"),
                    guide= FALSE)+
  scale_x_discrete(limits=c("g","f", "b"), labels = c("g" ="G", "f" ="F", "b"= "B"   ))+
  scale_y_discrete(limits = c(-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3))+
    theme_classic()+
  facet_grid(~Cflux)+
  geom_text(data = ann_textB ,aes(x= 3.4, y= -7.1, label =lab), size= 5, inherit.aes = FALSE)+
    theme(axis.title.x=element_blank(), axis.text.x=element_text(size = 12), axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.y=element_text(size = 14), strip.background = element_rect(colour="black", fill="white"), panel.background = element_rect(fill= "white"), panel.border = element_rect(colour = "black", fill=NA), strip.text.x = element_text(size=12, face="bold"),  axis.line = element_line(colour = "black"), legend.position = "none" )

ann_textC<- data.frame(estimate = -6, term = "B",lab=c("c)","d)"),
                       Cflux = factor(c("GPP","Reco"),levels = c("GPP","Reco")))
B.flux_CI%>% filter(!Cflux == "NEE")%>%
  filter(Treatment %in% c("G" , "F" , "B"))%>%
  ggplot( aes(presentFG, CI, fill = T_level))+
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_boxplot(outlier.shape = NA)+
  geom_point(size =1.5,  alpha = 0.6, position = position_jitterdodge(jitter.width = 0.1)  ) +
  scale_fill_manual(values =c("white", "grey70", "grey40"),
                    name="Temperature level", 
                    breaks=c("Alpine", "Sub-alpine", "Boreal"), 
                    labels = c("ALP", "SUB", "BOR"),
                    guide= FALSE)+
  scale_x_discrete(limits=c("gb","fb", "gf"), labels = c("gb" = "GB", "fb" ="FB", "gf"= "GF"   ))+
  theme_classic()+
  scale_y_discrete(limits = c(-2,-1,0,1,2,3))+
  facet_grid(~Cflux)+
  geom_text(data = ann_textC ,aes(x= 3.4, y= -2, label =lab), size= 5, inherit.aes = FALSE)+
  theme(axis.title.x=element_blank(), axis.text.x=element_text(size = 12), axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.y=element_text(size = 14), strip.background = element_rect(colour="black", fill="white"), panel.background = element_rect(fill= "white"), panel.border = element_rect(colour = "black", fill=NA), strip.text.x = element_text(size=12, face="bold"),  axis.line = element_line(colour = "black"), legend.position = "none" )


##### Analysis of Compensation Index ####
#### compenstation of single remaining FG ###!!!!!!!!!
testGPP<-B.flux_CI%>% filter(Cflux== "GPP" )%>%
  filter(Treatment %in% c("GB" , "FB" , "GF"))%>%
  mutate(deltaCI.C = CI -1)
summary(glm(deltaCI.C~ -1 + Treatment + Treatment:T_level , data =testGPP))
# B present different from the rest, no interactions

testReco<-B.flux_CI%>% filter(Cflux== "Reco" )%>%
  filter(Treatment %in% c("GB" , "FB" , "GF"))%>%
  mutate(deltaCI.C = CI -1)
summary(glm(deltaCI.C~ -1 + Treatment + Treatment:T_level, data =testReco))


### compensation of two remaining FG !!!!!!!
testGPP2<-flux_CI%>% filter(Cflux== "GPP" )%>%
  filter(Treatment %in% c("G" , "F" , "B"))%>%
  filter(!is.na(bryophyteCov))%>%
  mutate(deltaCI.C = CI -1)
summary(glm(deltaCI.C~ -1 + Treatment + Treatment:T_level, data =testGPP2))
# G and F removal different from control (CI =1), B removal not different from CI= 1 so full compensation, forb removal sign interaction with T_level

testReco2<-flux_CI%>% filter(Cflux== "Reco" )%>%
  filter(Treatment %in% c( "G" , "F" , "B"))%>%
  filter(!is.na(bryophyteCov))%>%
  mutate(deltaCI.C = CI -1)
summary(glm(deltaCI.C~ -1 + Treatment + Treatment:T_level, data =testReco2))
# B and F removal different from control (CI =1), G removal not sign different (p = 0.08) from CI= 1 so full compensation, no interactions


#### Separating CI contributions
# 1. changes in biomass of remaining PFGs compared to control situation (gap-recruitment)
# R = removal community, I = intact community, i = removed species, j = remaining species
# MjR - MjI = delta Mj, when >0 biomass compensation. 
# full compensation MiI = delta M, amount of gap recruitment biomass equals biomass lost through removal
# delta Mj/MiI * 100 % biomass compensation

### HAVE RICHARD CHECK THIS!!!
#### FG Cover compensation ####
### Now for all biomass, when only accounting for Vasc biomass use Vasc.biomass instead of Total.biomass!!

#Biomass.compensation<- CO2veg_2017%>%
#  select(Site, Block, Treatment, turfID, bryophyteCov, graminoidCov, forbCov, vegetationHeight, mossHeight, 
#        B.biomass, G.biomass, F.biomass)%>%
#  group_by(Site, Block, Treatment, turfID)%>%
#  group_by(Site, Block, Treatment)%>%
#  mutate(Total.biomass = B.biomass + G.biomass + F.biomass,
#         Vasc.biomass = G.biomass + F.biomass)%>%
  #gather(key= FGcover , value = cover, bryophyteCov, graminoidCov, forbCov)%>%
#  gather(key= FGbiomass, value =biomass, B.biomass, G.biomass, F.biomass)%>%
#  group_by(Site, Block, FGbiomass)%>%
#  left_join((.) %>% filter(Treatment == "C")%>% select(Site, Block, FGbiomass, control.biomass= Total.biomass))%>%
#  mutate(comp.biomass = (Total.biomass-control.biomass)/control.biomass)%>%
#  filter(!Treatment %in% c("C", "FGB"))%>%
#  mutate(T_level = recode(Site, Ulv = "1", Lav = "1",  Gud = "1", Skj = "1", Alr = "2", Hog = "2", Ram = "2", Ves = #"2", Fau = "3", Vik = "3", Arh = "3", Ovs = "3")) %>%
#  mutate(P_level = recode(Site, Ulv = "1", Alr = "1", Fau = "1", Lav = "2", Hog = "2", Vik = "2", Gud = "3", Ram = #"3", Arh = "3", Skj = "4", Ves = "4", Ovs = "4"))%>%
#  mutate(Temp.C = recode(Site, Ulv = 6.17, Lav = 6.45,  Gud = 5.87, Skj = 6.58, Alr = 9.14, Hog = 9.17, Ram = 8.77, 
#                         Ves = 8.67, Fau = 10.3, Vik = 10.55, Arh = 10.6, Ovs = 10.78))%>%
#  mutate(P.mm = recode(Site, Ulv = 596, Alr = 789, Fau = 600, Lav = 1321, Hog = 1356, Vik = 1161, Gud = 1925, 
#                       Ram = 1848, Arh = 2044, Skj = 2725, Ves = 3029, Ovs = 2923))%>%
#  ungroup()



#### Calculate difference between total biomass in treatment and control
Biomass.compensation<- CO2veg_2017%>%
  select(Site, Block, Treatment, turfID, bryophyteCov, graminoidCov, forbCov, vegetationHeight, mossHeight, 
         B.biomass, G.biomass, F.biomass)%>% 
  mutate(F.biomass = ifelse(grepl("F", Treatment), 0, F.biomass), #assign 0 to biomass compensation for FG removed
         G.biomass = ifelse(grepl("G", Treatment), 0, G.biomass),
         B.biomass = ifelse(grepl("B", Treatment), 0, B.biomass))%>% # make sure removed FG biomass is 0
  mutate(Total.biomass = B.biomass + G.biomass + F.biomass,
         Total.Vasc = G.biomass + F.biomass)%>%
  ungroup()
# ! B.biomass = 0
#need to calculate control biomass - biomass of FG that will be removed in Treatment (treatment based)

BCI <- Biomass.compensation%>%
  gather(key= FGbiomass, value =biomass, B.biomass, G.biomass, F.biomass)%>%
    group_by(Site, Block, FGbiomass)%>%
    left_join((.) %>% filter(Treatment == "C")%>% select(Site, Block, FGbiomass, control.biomass= Total.biomass))%>%
    mutate(comp.biomass = (Total.biomass-control.biomass)/control.biomass)%>%
    filter(!Treatment %in% c("C", "FGB"))%>%
    mutate(T_level = recode(Site, Ulv = "1", Lav = "1",  Gud = "1", Skj = "1", Alr = "2", Hog = "2", Ram = "2", Ves = "2", Fau = "3", Vik = "3", Arh = "3", Ovs = "3")) %>%
    mutate(P_level = recode(Site, Ulv = "1", Alr = "1", Fau = "1", Lav = "2", Hog = "2", Vik = "2", Gud = "3", Ram = "3", Arh = "3", Skj = "4", Ves = "4", Ovs = "4"))%>%
    mutate(Temp.C = recode(Site, Ulv = 6.17, Lav = 6.45,  Gud = 5.87, Skj = 6.58, Alr = 9.14, Hog = 9.17, Ram = 8.77,                          Ves = 8.67, Fau = 10.3, Vik = 10.55, Arh = 10.6, Ovs = 10.78))%>%
    mutate(P.mm = recode(Site, Ulv = 596, Alr = 789, Fau = 600, Lav = 1321, Hog = 1356, Vik = 1161, Gud = 1925, 
                         Ram = 1848, Arh = 2044, Skj = 2725, Ves = 3029, Ovs = 2923))%>%
    ungroup()


Biomass.Control <- Biomass.compensation%>%
  mutate(Total.Cbiomass = B.biomass + G.biomass + F.biomass,
         F = B.biomass + G.biomass,
         G = F.biomass + B.biomass,
         B = G.biomass + F.biomass,
         FB = G.biomass,
         GF = B.biomass,
         GB = F.biomass,
         FGB = 0)%>% # calculate biomass remaining for different combination of removals 
  gather(key = "bTreatment", value = "Remain.biomass", F:FGB)%>%
  select(Site, Block, Treatment, bTreatment, B.biomass,  G.biomass, F.biomass , Remain.biomass, Total.Cbiomass)%>%
  filter(Treatment == "C")
  
  
BCI <- right_join(Biomass.compensation, Biomass.Control, by = c("Site", "Block", "Treatment" = "bTreatment"))%>%
    # need to subtract biomass of FG that will be removed from total biomass of control
 mutate(#ED = Remain.biomass *100,
        #OD = (Total.Cbiomass - Total.biomass) / Total.Cbiomass *100,
        #BCI = (ED-OD)/ED,
        #BCI2 = (Total.biomass- Remain.biomass )/ (Total.Cbiomass), 
        BCI3 = ((Total.biomass- Remain.biomass )/ (Total.Cbiomass -Remain.biomass)))%>% #based on Adler &Bradford
  filter(!Treatment %in% c("C", "FGB"))%>%
  mutate(T_level = recode(Site, Ulv = "Alpine", Lav = "Alpine",  Gud = "Alpine", Skj = "Alpine", Alr = "Sub-alpine", Hog = "Sub-alpine", Ram = "Sub-alpine", Ves = "Sub-alpine", Fau = "Boreal", Vik = "Boreal", Arh = "Boreal", Ovs = "Boreal")) %>%
  mutate(P_level = recode(Site, Ulv = "600mm", Alr = "600mm", Fau = "600mm", Lav = "1200mm", Hog = "1200mm", Vik = "1200mm", Gud = "2700mm", Ram = "2700mm", Arh = "2000mm", Skj = "2700mm", Ves = "2700mm", Ovs = "2700mm"))%>%
  mutate(Temp.C = recode(Site, Ulv = 6.17, Lav = 6.45,  Gud = 5.87, Skj = 6.58, Alr = 9.14, Hog = 9.17, Ram = 8.77, 
                         Ves = 8.67, Fau = 10.3, Vik = 10.55, Arh = 10.6, Ovs = 10.78))%>%
  mutate(P.mm = recode(Site, Ulv = 596, Alr = 789, Fau = 600, Lav = 1321, Hog = 1356, Vik = 1161, Gud = 1925, 
                       Ram = 1848, Arh = 2044, Skj = 2725, Ves = 3029, Ovs = 2923))%>%
  ungroup()

BCI%>%
  #filter(Treatment %in% c("FB", "GB", "GF"))%>%
  #filter(Treatment %in% c("F", "B", "G"))%>%
  ggplot(aes(P_level, BCI3, fill = Treatment))+
  geom_boxplot()+
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~T_level)+
  theme(axis.title.x=element_text(size = 18), axis.text.x=element_text(size = 14), axis.title = element_text(size = 18), axis.text.y = element_text(size = 14), strip.text = element_text(size = 14))  

# compare CI with B_CI to see if biomass compensation is the main driver of CI
# join CI data with B_CI data
CI_BCI_2 <- right_join(B.flux_CI, BCI, by= c("Site", "Block", "Treatment", "turfID"))
CI_BCI_2$Treatment <- factor(CI_BCI_2$Treatment, levels = c("G", "F", "B", "FB", "GB", "GF"))

library(RColorBrewer)
CI_BCI_2$P_level.x <- factor(CI_BCI_2$P_level.x, levels = c("600mm", "1200mm", "2700mm"))
CI_BCI_2$T_level.x <- factor(CI_BCI_2$T_level.x, levels = c("Alpine", "Sub-alpine", "Boreal"))

CI_BCI_2%>%
  filter(Cflux == "GPP")%>%
  filter(Treatment %in% c("FB" ,"GF", "GB", "G", "B", "F" ))%>%
  ggplot(aes( BCI3, CI, color = Treatment, fill = Treatment, shape= T_level.x ))+
  geom_point(size= 2.5, alpha = 0.5)+
  scale_shape_manual(values = c(24, 22, 25))+
  scale_color_brewer(palette = "Dark2", name = "FG present", labels = c("FB", "GB", "GF", "G", "F", "B"))+
  scale_fill_brewer(palette = "Dark2", name = "FG present", labels = c("FB", "GB", "GF", "G", "F", "B"))+
  geom_abline(intercept = 0, slope = 1)+
  geom_hline(yintercept= 0 , linetype="dashed", color = "grey")+
  geom_vline(xintercept = 0, linetype= "dashed", color = "grey")+
  facet_grid(~GPP)+
  facet_grid(~P_level.x)+
  guides(shape=guide_legend(title="Temperature level"))+
    labs(Title= "GPP", x= "Biomass Compensation Index", y = "C flux Compensation Index")+
  theme(axis.title.x=element_text(size = 14), axis.text.x=element_text(size = 12), axis.title = element_text(size = 14),  axis.title.y=element_text(size = 14), axis.text.y = element_text(size = 12), strip.background = element_rect(colour="black", fill="white"), panel.background = element_rect(fill= "white"), panel.border = element_rect(colour = "black", fill=NA), strip.text.x = element_text(size=12, face="bold"),  axis.line = element_line(colour = "black"))


ggplot( aes(presentFG, CI, fill = T_level))+
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_boxplot(outlier.shape = NA)+
  geom_point(size =1.5,  alpha = 0.6, position = position_jitterdodge(jitter.width = 0.1)  ) +
  scale_fill_manual(values =c("white", "grey70", "grey40"),
                    name="Temperature level", 
                    breaks=c("Alpine", "Sub-alpine", "Boreal"), 
                    labels = c("ALP", "SUB", "BOR"),
                    guide= FALSE)+
  scale_x_discrete(limits=c("gb","fb", "gf"), labels = c("gb" = "GB", "fb" ="FB", "gf"= "GF"   ))+
  theme_classic()+
  scale_y_discrete(limits = c(-2,-1,0,1,2,3))+
  facet_grid(~Cflux)+
  geom_text(data = ann_textC ,aes(x= 3.4, y= -2, label =lab), size= 5, inherit.aes = FALSE)+
  theme(axis.title.x=element_blank(), axis.text.x=element_text(size = 12), axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.y=element_text(size = 14), strip.background = element_rect(colour="black", fill="white"), panel.background = element_rect(fill= "white"), panel.border = element_rect(colour = "black", fill=NA), strip.text.x = element_text(size=12, face="bold"),  axis.line = element_line(colour = "black"), legend.position = "none" )


ggplot(BCI, aes(control.biomass, Total.biomass, col = Treatment))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1)+
  facet_grid(T_level~P_level)

### Collaps traitspectrum into PCA > calculate new CWM trait for plots > does this explain CI
load("O:\\FunCab\\Data\\FunCaB2\\CO2\\pcaScores.RData")


##### Do changes in traitspace between removals explain CI
load("O:\\FunCab\\Data\\FunCaB2\\CO2\\community_FD_Anom.RData") # load anomalie data from trait 
community_FD%>%
  ungroup()

 
#use significant anomalies as predictors for CI
# maybe also include anomalies of biomass change in model.
# lasso model > check R blog



####-----------------------------------------------------------------------------------------------------------------
#### Calculate compensation Index based on Cover 
specificF <- CO2veg_2017%>%
  gather(key= Cflux , value = flux, NEE, Reco, GPP)%>%
  group_by(Site, Block, Cflux)%>%
  left_join((.) %>% filter(Treatment == "C")%>% select(Site, Block, Cflux, control= flux))%>%
  group_by(Site, Treatment)%>%
  mutate(presentFG = recode(Treatment, FGB = "zero", GF = "b", FB= "g", GB = "f", B= "gf", G = "fb", F = "gb"))%>%
  mutate(totalCover = bryophyteCov+forbCov+graminoidCov)%>%# calculate cover sum per plot 
  filter(Treatment %in% c("GB" , "FB" , "GF"))%>% # filter for plots with single FG
  gather(key= FG, value= cover, bryophyteCov, forbCov, graminoidCov )%>%
  filter((Treatment == "GB" & FG == "forbCov") |(Treatment == "FB" & FG == "graminoidCov") |(Treatment =="GF" & FG == "bryophyteCov"))%>%
  mutate(SpecificFlux = flux/ cover)%>%
  ungroup()%>%
  select(Site, Block, FG, Cflux, SpecificFlux)

ED_calc <- CO2veg_2017%>%
  gather(key= Cflux , value = flux, NEE, Reco, GPP)%>%
  gather(key= FG, value= cover, bryophyteCov, forbCov, graminoidCov )%>%
  left_join(., specificF, by = c("Site", "Block", "FG", "Cflux"))%>%
  group_by(Site, Block, Treatment, turfID, Cflux)%>%
  summarise(treatmentED = sum(cover*SpecificFlux, na.rm = TRUE))%>%
  ungroup()%>%
  left_join((.) %>% filter(Treatment == "C") %>% select(Site, Block, Cflux, controlED = treatmentED))%>%
  mutate(ED = treatmentED / controlED *100)

OD_calc <- CO2veg_2017%>%
  gather(key= Cflux , value = flux, NEE, Reco, GPP)%>%
  left_join((.) %>% filter(Treatment == "C")%>% select(Site, Block, Cflux, controlflux= flux))%>%
  group_by(Site, Block, turfID, Cflux)%>%
  mutate( OD = (controlflux - flux)/controlflux *100)%>%
  ungroup()

flux_CI <- left_join(OD_calc, ED_calc, by = c("Site", "Block", "Treatment", "turfID", "Cflux"))%>%
  mutate(presentFG = recode(Treatment, FGB = "zero", GF = "b", FB= "g", GB = "f", B= "gf", G = "fb", F = "gb", C = "all"))%>%
  mutate(treatment_nr = recode(Treatment, FGB = "8", GF = "7", FB= "6", GB = "5", B= "2", G = "3", F = "4", C = "1"))%>%
  group_by(Site, Block, turfID, Cflux)%>%
  mutate(CI = (ED-OD)/ED)%>%
  mutate(Treatment = factor(Treatment, levels = c("C", "B", "G", "F", "GB", "FB", "GF", "FGB")))%>%
  mutate(T_level = recode(Site, Ulv = "1", Lav = "1",  Gud = "1", Skj = "1", Alr = "2", Hog = "2", Ram = "2", Ves = "2", Fau = "3", Vik = "3", Arh = "3", Ovs = "3")) %>%
  mutate(P_level = recode(Site, Ulv = "1", Alr = "1", Fau = "1", Lav = "2", Hog = "2", Vik = "2", Gud = "3", Ram = "3", Arh = "3", Skj = "4", Ves = "4", Ovs = "4"))%>%
  mutate(T_level = ordered(T_level, levels = c("1", "2", "3")))%>%
  mutate(P_level = ordered(P_level, levels = c("1", "2", "3", "4")))%>%
  mutate(Temp.C = recode(Site, Ulv = 6.17, Lav = 6.45,  Gud = 5.87, Skj = 6.58, Alr = 9.14, Hog = 9.17, Ram = 8.77, 
                         Ves = 8.67, Fau = 10.3, Vik = 10.55, Arh = 10.6, Ovs = 10.78))%>%
  mutate(P.mm = recode(Site, Ulv = 596, Alr = 789, Fau = 600, Lav = 1321, Hog = 1356, Vik = 1161, Gud = 1925, 
                       Ram = 1848, Arh = 2044, Skj = 2725, Ves = 3029, Ovs = 2923))
  




flux_CI%>% filter(!Cflux == "NEE")%>%
  filter(Treatment %in% c("F" , "G" , "B"))%>%
  ggplot( aes(T_level, CI, fill = presentFG))+
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_boxplot()+
  facet_grid(~Cflux)+
  theme(axis.title.x=element_text(size = 18), axis.text.x=element_text(size = 14), axis.title = element_text(size = 18), axis.text.y = element_text(size = 14), strip.text = element_text(size = 14))


flux_CI%>% filter(Cflux== "GPP" )%>%
  filter(Treatment %in% c("F" , "G" , "B"))%>%
  ggplot( aes(T_level, CI, col = Treatment))+
  geom_boxplot()

flux_CI%>% filter(Cflux== "Reco" )%>%
  filter(Treatment %in% c("F" , "G" , "B"))%>%
  ggplot( aes(T_level, CI, col = Treatment))+
  geom_boxplot()



flux_CI%>% ungroup()%>% filter(!Cflux == "NEE")%>% 
  #mutate(Treatment=replace(Treatment, Treatment == "B", "Moss")) %>%
  filter(!Treatment %in% c("FGB", "C", "F", "G", "B"))%>%
  select(Cflux, CI, graminoidCov, forbCov, bryophyteCov, Site, Treatment, Temp.C, P.mm, T_level, P_level) %>% 
  na.omit() %>% 
  group_by(Cflux)%>%
  do(tidy(glm(CI ~ graminoidCov + forbCov + bryophyteCov +graminoidCov:Temp.C + forbCov:Temp.C + bryophyteCov:Temp.C, data=. ))) %>%
  filter(!term == "(Intercept)") %>% 
  filter(!grepl("^sd_", term)) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>%
  as.data.frame()%>%
  #filter(!term == "Temp.C")%>%
  ggplot(aes(x =term, y = estimate, shape=Cflux, fill=Cflux, ymin = lower, ymax = upper)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = c(8.5, 16.5, 22.5), linetype = "dotted", size =1) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_shape_manual(labels = c("GPP", "Reco"), values = c(24,21)) + 
  #scale_fill_manual(labels = c("Alp", "Sub", "Bor"), values = c("blue", "green", "red")) +
  facet_grid(~Cflux)+ #, labeller=labeller(response = labels)
  theme(axis.title.x=element_text(size = 16), axis.text.x=element_text(size = 12), axis.title = element_text(size = 16), axis.text.y = element_text(size = 12), axis.title.y=element_blank(), strip.background = element_rect(colour="black", fill="white"), panel.background = element_rect(fill= "white"), panel.border = element_rect(colour = "black", fill=NA), strip.text.x = element_text(size=12, face="bold"),  axis.line = element_line(colour = "black"))+
  coord_flip()

flux_CI%>% ungroup()%>% filter(!Cflux == "NEE")%>% 
  #mutate(Treatment=replace(Treatment, Treatment == "B", "Moss")) %>%
  filter(!Treatment %in% c("FGB", "C", "FB", "GF", "GB"))%>%
  select(Cflux, CI, graminoidCov, forbCov, bryophyteCov, Site, Treatment, Temp.C, P.mm, T_level, P_level) %>% 
  na.omit() %>% 
  group_by(Cflux, T_level)%>%
  do(tidy(glm(CI ~ graminoidCov + forbCov + bryophyteCov + graminoidCov:Temp.C + forbCov:Temp.C + bryophyteCov:Temp.C + (1|Site), data=. ))) %>%
  filter(!term == "(Intercept)") %>% 
  filter(!grepl("^sd_", term)) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>%
  as.data.frame()%>%
  #filter(!term == "Temp.C")%>%
  ggplot(aes(x =term, y = estimate, shape=Cflux, fill=T_level , ymin = lower, ymax = upper)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = c(8.5, 16.5, 22.5), linetype = "dotted", size =1) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_shape_manual(labels = c("GPP", "Reco"), values = c(24,21)) + 
  scale_fill_manual(labels = c("1", "2", "3"), values = c("blue", "green", "red")) +
  facet_grid(~Cflux)+ #, labeller=labeller(response = labels)
  theme(axis.title.x=element_text(size = 16), axis.text.x=element_text(size = 12), axis.title = element_text(size = 16), axis.text.y = element_text(size = 12), axis.title.y=element_blank(), strip.background = element_rect(colour="black", fill="white"), panel.background = element_rect(fill= "white"), panel.border = element_rect(colour = "black", fill=NA), strip.text.x = element_text(size=12, face="bold"),  axis.line = element_line(colour = "black"), legend.position = "none" )+
  coord_flip()


#Temp.C*forbCov + Temp.C*graminoidCov + Temp.C*bryophyteCov


specificF%>% 
  mutate(T_level = recode(Site, Ulv = "1", Lav = "1",  Gud = "1", Skj = "1", Alr = "2", Hog = "2", Ram = "2", Ves = "2", Fau = "3", Vik = "3", Arh = "3", Ovs = "3")) %>%
  mutate(P_level = recode(Site, Ulv = "1", Alr = "1", Fau = "1", Lav = "2", Hog = "2", Vik = "2", Gud = "3", Ram = "3", Arh = "3", Skj = "4", Ves = "4", Ovs = "4")) %>%
  filter(Cflux== "Reco" , !FG == "bryophyteCov")%>%
  ggplot( aes(FG, SpecificFlux, col = T_level))+
  geom_boxplot( )+
  facet_wrap(~P_level)









#### Effect size plot ####
CO2veg_2017%>%
  ungroup()%>%
  select(Reco, GPP, NEE, Site, Treatment, turfID, Temp.C, P.mm, forbCov, graminoidCov, bryophyteCov)%>%
  gather(key = pred, value = value, -c(GPP, Reco, NEE, Treatment, turfID, Site))%>%
  gather(key = response, value = Cflux, -c(Site, Treatment, turfID, pred, value))%>%
  group_by(pred, response)%>%
  mutate(value = scale(value))%>%
  #group_by(Site, Treatment, turfID, response)%>%
  #spread(pred, value)%>%
  #group_by(pred, response)%>%
  do(tidy(lmer(Cflux ~ value + (1|Site/Treatment), data=. ))) %>%
  filter(!term == "(Intercept)") %>% 
  filter(!grepl("^sd_", term)) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>%
  as.data.frame()%>%
  ggplot(aes(x =pred, y = estimate, shape=response, fill= response , ymin = lower, ymax = upper)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = c(8.5, 16.5, 22.5), linetype = "dotted", size =1) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_shape_manual(labels = c("GPP", "Reco", "NEE"), values = c(24,21, 22)) + 
  scale_fill_manual(labels = c("GPP", "Reco", "NEE"), values = c("grey70", "black", "red")) + 
  facet_grid(~response)+ #, labeller=labeller(response = labels)
  theme(axis.title.x=element_text(size = 16), axis.text.x=element_text(size = 12), axis.title = element_text(size = 16), axis.text.y = element_text(size = 12), axis.title.y=element_blank(), strip.background = element_rect(colour="black", fill="white"), panel.background = element_rect(fill= "white"), panel.border = element_rect(colour = "black", fill=NA), strip.text.x = element_text(size=12, face="bold"),  axis.line = element_line(colour = "black"), legend.position = "none" )+
  coord_flip()


CO2veg_2017_all%>%
  ungroup()%>%
  select(Reco, GPP, nee.x, Site, Treatment, turfID, tempK, PAR.correct.x, Moisture_mean.x, forbCov, graminoidCov, bryophyteCov)%>%
  gather(key = pred, value = value, -c(GPP, Reco, nee.x, Treatment, turfID, Site))%>%
  gather(key = response, value = Cflux, -c(Site, Treatment, turfID, pred, value))%>%
  group_by(pred, response)%>%
  mutate(value = scale(value))%>%
  #group_by(Site, Treatment, turfID, response)%>%
  #spread(pred, value)%>%
  #group_by(pred, response)%>%
  do(tidy(lmer(Cflux ~ value + (1|Site/turfID), data=. ))) %>%
  filter(!term == "(Intercept)") %>% 
  filter(!grepl("^sd_", term)) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>%
  as.data.frame()%>%
  ggplot(aes(x =pred, y = estimate, shape=response, fill= response , ymin = lower, ymax = upper)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_vline(xintercept = c(8.5, 16.5, 22.5), linetype = "dotted", size =1) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_shape_manual(labels = c("GPP", "Reco", "nee.x"), values = c(24,21, 22)) + 
  scale_fill_manual(labels = c("GPP", "Reco", "nee.x"), values = c("grey70", "black", "red")) + 
  facet_grid(~response)+ #, labeller=labeller(response = labels)
  theme(axis.title.x=element_text(size = 16), axis.text.x=element_text(size = 12), axis.title = element_text(size = 16), axis.text.y = element_text(size = 12), axis.title.y=element_blank(), strip.background = element_rect(colour="black", fill="white"), panel.background = element_rect(fill= "white"), panel.border = element_rect(colour = "black", fill=NA), strip.text.x = element_text(size=12, face="bold"),  axis.line = element_line(colour = "black"), legend.position = "none" )+
  coord_flip()

#### compare soil respiration across grid
CO2veg_2017%>%
  filter(Treatment == "FGB")%>%
  ggplot(aes(x=tempK, y=Reco, col= Moisture_mean.x))+
  geom_point()+
  geom_smooth(method = "lm")
  


##### Explore Data; distribution of environmental condition between treatments #####
##### Soil moisture, Soil Temp, Air temp, PAR 


ggplot(CO2veg_2017, aes(x=Treatment, y = Moisture_mean))+
      geom_boxplot()+
      facet_wrap(~Site)

ggplot(CO2veg_2017, aes(x=Treatment, y = soilT))+
  geom_boxplot()+
  facet_wrap(~Site)

ggplot(CO2veg_2017, aes(x=Treatment, y = temp))+
  geom_boxplot()+
  facet_wrap(~Site)

ggplot(CO2veg_2017, aes(x=Treatment, y = PAR.correct, col=cover))+
  geom_boxplot()+
  facet_wrap(~Site)


##################### CO2 flux processing #############################################################################################################
#seperate L and D measurements and merge them in new file with new column GPP, selecting data with r2>=.9
CO2_2017_NEE<-subset(CO2_2017, cover== "L" & rsqd>=.8 | cover== "L" & rsqd<=.2 )   
CO2_2017_RECO<-subset(CO2_2017, cover== "D" & rsqd>=.8 | cover== "D" & rsqd<=.2 )  
CO2_2017_RECO$Reco<-CO2_2017_RECO$nee*-1
CO2_2017_RECO$tempK<-CO2_2017_RECO$temp+273.15
CO2_2017_GPP<- inner_join(CO2_2017_NEE, CO2_2017_RECO, by=c("Date", "chamber", "Site", "Block", "Treatment", "ToD", "Tlevel", "Plevel" ))
CO2_2017_GPP$GPP<-CO2_2017_GPP$nee.x- CO2_2017_GPP$nee.y #NEE-Reco


# Standardizing uncorrected data, Lloyd & Taylor 1994 , Thornley and Johnson (1990) Plant and Crop Modeling
ggplot(CO2_2017_GPP, aes(x=tempK, y=Reco, col=factor(Treatment)))+
  geom_point(na.rm= TRUE)+
  geom_smooth(method = "nls", formula= y~A*exp(-308.56/I(x-227.13)), method.args = list(start=c(A=0)), se=FALSE, na.rm= TRUE)+
  facet_wrap(~Site)+
  theme_bw()

ggplot(CO2_2017_GPP, aes(x=PAR.x, y=GPP, col= factor(Treatment)))+
  geom_point(na.rm= TRUE)+
  geom_smooth(method = "nls", formula= y~(A*B*x)/(A*x+B), method.args = list(start=c(A=0.01, B=2)), se=FALSE, na.rm= TRUE)+
  theme_bw()

ggplot(CO2_2017_data, aes(x= Treatment, y= GPP, col=Tlevel))+
    geom_boxplot()+
      facet_grid(~Plevel)

ggplot(CO2_2017_GPP, aes(x= Treatment, y= Reco, col=Tlevel))+
  geom_boxplot()+
  facet_grid(~Plevel)


veg_cflux <- left_join(CO2)






#### load biomass data ####
Biomass_2015<- read_excel("\\\\eir.uib.no\\home6\\ial008\\FunCab\\Data\\Vegetation data\\biomass_removals_2015.xlsx")
Biomass_2016<- read_excel("\\\\eir.uib.no\\home6\\ial008\\FunCab\\Data\\Vegetation data\\Removal_Biomass_151617.xlsx")
Biomass_2017<- read_excel("\\\\eir.uib.no\\home6\\ial008\\FunCab\\Data\\Vegetation data\\biomass_removals_2017.xlsx")
Biomass_2017$Biomass_g <- substr(Biomass_2017$Biomass_g,1,nchar(Biomass_2017$Biomass_g)-1)

# Bind biomass data of 2015-2017 together and make it Biomass_g numerical
Biomass_rem<- rbind(Biomass_2017, Biomass_2016)
Biomass_rem<-Biomass_rem %>%
  mutate(TurfID=paste0(Site, Block, Treatment)) %>%
  mutate(Treatment = recode(Treatment, "FG" = "GF"))%>%
  select(- Date, -Name, -Remark) 
Biomass_rem<- rbind(Biomass_rem, Biomass_2015)
Biomass_rem$Biomass_g<- as.numeric(Biomass_rem$Biomass_g)

Biomass_mean <- Biomass_rem %>%
  group_by(Site, Func_group, Round, Year)%>%
  mutate(biomass_m = mean(Biomass_g, na.rm = TRUE))%>%
  mutate(biomass_sd = sd(Biomass_g, na.rm = TRUE))%>%
  mutate(P_level = recode(Site, Ulv = "600 mm", Alr = "600 mm", Fau = "600 mm", Lav = "1200 mm", Hog = "1200 mm", Vik = "1200 mm", Gud = "2000 mm", Ram = "2000 mm", Arh = "2000 mm", Skj = "2700 mm", Ves = "2700 mm", Ovs = "2700 mm"))%>%
  mutate(T_level = recode(Site, Ulv = "Alpine", Lav = "Alpine",  Gud = "Alpine", Skj = "Alpine", Alr = "Sub-alpine", Hog = "Sub-alpine", Ram = "Sub-alpine", Ves = "Sub-alpine", Fau = "Boreal", Vik = "Boreal", Arh = "Boreal", Ovs = "Boreal"))%>%
  filter(!(P_level=="2000 mm"))%>%
  ungroup()
Biomass_mean$T_level<- factor(Biomass_mean$T_level, levels=c("Alpine","Sub-alpine","Boreal"))
Biomass_mean$P_level<- factor(Biomass_mean$P_level, levels=c("600 mm","1200 mm","2700 mm"))

Biomass_mean %>%
  filter(Round == "Round 1")%>%
  ggplot(aes(x=factor(Func_group), y= biomass_m, fill=factor(Year)))+
  geom_bar(stat = "identity", position = position_dodge(), col= "black")+
  scale_fill_grey(start = 0, end = 0.8)+
  geom_errorbar(aes(ymin=biomass_m, ymax=biomass_m+biomass_sd), width=.3, position = position_dodge(0.9)) +
  facet_grid(T_level~P_level)+
  labs(y= "Biomass (g)", x = "Functional Group")+
  theme_classic()

Biomass_rem %>%
  filter(Round == "Round 1")%>%
  filter(Func_group == "F") %>%
  ggplot(aes(x=factor(Year), y= Biomass_g, col=factor(Treatment)))+
  geom_boxplot()+
  facet_wrap(~Site)




load("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\funcabComp2.RData") 
## correct height data of specific plots
composition2015 <- within(composition, vegetationHeight[turfID == 'Ram4F'& Year == "2015"] <- 80)
subVals <- !is.na(composition2015$vegetationHeight) <= 10
composition2015$vegetationHeight[subVals] <- composition$vegetationHeight[subVals] * 10
composition2015 <- within(composition2015, vegetationHeight[turfID == 'Fau2C' & Year == "2015"] <- 155)
composition2015 <- within(composition2015, vegetationHeight[turfID == 'Alr1C' & Year == "2015"] <- 130)
composition2015 <- within(composition2015, vegetationHeight[turfID == 'Alr2C' & Year == "2015"] <- 197.5)
composition2015 <- within(composition2015, turfID[siteID == 'Lavisdalen' & Year == "2015" & blockID == "1" & Treatment == "G"] <- "Lav1G")


community_data2015 <-composition2015 %>%
  filter( Year == "2015")%>%
  mutate(Site= substr(siteID, 1,3))%>%
  mutate(T_level = recode(Site, Ulv = "1", Lav = "1",  Gud = "1", Skj = "1", Alr = "2", Hog = "2", Ram = "2", Ves = "2", Fau = "3", Vik = "3", Arh = "3", Ovs = "3")) %>%
  mutate(P_level = recode(Site, Ulv = "1", Alr = "1", Fau = "1", Lav = "2", Hog = "2", Vik = "2", Gud = "3", Ram = "3", Arh = "3", Skj = "4", Ves = "4", Ovs = "4"))%>%
  group_by(Site, blockID, Treatment, Year)%>%
  slice(1)%>%
  select(Site, turfID, Treatment, Year, forbCov, graminoidCov, bryophyteCov, vegetationHeight, mossHeight, P_level, T_level)%>%
  ungroup()

biomass15<- Biomass_rem%>%
  filter(Year == 2015)%>%
  spread(key = Func_group, value = Biomass_g)

####Filter out RTC and P_level 3 ####
biomass_cover <- left_join(biomass15, community_data2015,  by= c( "Year"= "Year", "TurfID"="turfID"))%>%
  filter(Year == 2015)%>%
  filter(!P_level == "3")%>%
  group_by(TurfID)%>%
  mutate(forbCov2 = mean(forbCov, na.rm =TRUE),
            graminoidCov2 = mean(graminoidCov, na.rm =TRUE),
            bryophyteCov2 = mean(bryophyteCov, na.rm =TRUE))%>%
  slice(1)%>%
  ungroup()

biomass_cover%>%
  ggplot(aes(y=F, x=forbCov, col=T_level))+
  geom_point(aes(shape = P_level), size= 4, na.rm= TRUE)+
  geom_smooth(method= "lm", formula = y ~ 0 + x )+
  #scale_color_gradient2(low = "blue", high = "red", mid="yellow", midpoint = 8.5)+
  #scale_x_continuous(limits = c(0,100))+
  #scale_y_continuous(limits = c(0,300))+
  #annotate("text", x=15, y=250, label= "R2 = 0.90, p<0.001", size =5)+
  theme_classic()

#### Biomass removals cover relationships 2015 ####
# number of observations per Functional group: 144

summary(lm(B ~ 0+ bryophyteCov, data=biomass_cover)) # R2 = 0.77
summary(lm(G ~ 0+ graminoidCov,   data=biomass_cover)) # R2 =0.86
summary(lm(F ~ 0+ forbCov, data=biomass_cover)) # R2 = 0.78

ggplot(data=biomass_cover, aes(y=B, x=totalBryophytes, col= Temp.C))+
  geom_point(aes(shape = P_level), size= 4, alpha = 0.8, na.rm= TRUE)+
  geom_smooth(method= "lm", formula = y ~ 0 + x )+
  scale_color_gradient2(low = "blue", high = "red", mid="yellow", midpoint = 8.5)+
  scale_x_continuous(limits = c(0,100))+
  scale_y_continuous(limits = c(0,40))+
  annotate("text", x=15, y=30, label= "R2 = 0.74, p<0.001", size =5)+
  theme_classic()

ggplot(data=biomass_cover, aes(y=G, x=TotalGraminoids, col= Temp.C))+
  geom_point(aes(shape = P_level), size= 4, alpha = 0.8, na.rm= TRUE)+
  geom_smooth(method= "lm", formula = y ~ 0 + x )+
  scale_color_gradient2(low = "blue", high = "red", mid="yellow", midpoint = 8.5)+
  scale_x_continuous(limits = c(0,100))+
  scale_y_continuous(limits = c(0,30))+
  annotate("text", x=15, y=30, label= "R2 = 0.85, p<0.001", size =5)+
  theme_classic()

ggplot(data=biomass_cover, aes(y=F, x=totalForbs, col= Temp.C))+
  geom_point(aes(shape = P_level), size= 4, alpha = 0.8, na.rm= TRUE)+
  geom_smooth(method= "lm", formula = y ~ 0 + x )+
  scale_color_gradient2(low = "blue", high = "red", mid="yellow", midpoint = 8.5)+
  scale_x_continuous(limits = c(0,100))+
  scale_y_continuous(limits = c(0,30))+
  annotate("text", x=15, y=30, label= "R2 = 0.75, p<0.001", size =5)+
  theme_classic()
