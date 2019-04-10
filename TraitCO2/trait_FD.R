#### Libraries ####
library("tidyverse")
library("lubridate")
library("mosaic")
library("FD")
library("SDMTools")
library("xlsx")
library("readxl")
#library("dplyr")
library("broom")

#### Load and process community data
## community cover 2015-2016 with TTC
load("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\funcabComp2.RData") 
## correct height data of specific plots
composition <- within(composition, vegetationHeight[turfID == 'Ram4F'& Year == "2015"] <- 80)
subVals <- !is.na(composition$vegetationHeight) <= 10
composition$vegetationHeight[subVals] <- composition$vegetationHeight[subVals] * 10
composition <- within(composition, vegetationHeight[turfID == 'Fau2C' & Year == "2015"] <- 155)
composition <- within(composition, vegetationHeight[turfID == 'Alr1C' & Year == "2015"] <- 130)
composition <- within(composition, vegetationHeight[turfID == 'Alr2C' & Year == "2015"] <- 197.5)
composition <- within(composition, turfID[siteID == 'Lavisdalen' & Year == "2015" & blockID == "1" & Treatment == "G"] <- "Lav1G")
# rename block Gud5 > Gud16 to match flux data
composition$turfID[composition$turfID == "Gud5C"] <- "Gud16C"
composition$turfID[composition$turfID == "Gud5B"] <- "Gud16B"
composition$turfID[composition$turfID == "Gud5F"] <- "Gud16F"
composition$turfID[composition$turfID == "Gud5G"] <- "Gud16G"
composition$turfID[composition$turfID == "Gud5FB"] <- "Gud16FB"
composition$turfID[composition$turfID == "Gud5GB"] <- "Gud16GB"
composition$turfID[composition$turfID == "Gud5GF"] <- "Gud16GF"
composition$turfID[composition$turfID == "Gud5FGB"] <- "Gud16FGB"

community_cover_1516 <- composition %>%
  filter(!Year == 2017)%>%
  select(turfID, Year, siteID, species, cover)%>%
  mutate(species=gsub("\\.", "_", species))%>%
  mutate(Site= substr(siteID, 1,3))%>%
  ungroup()


#Adding a dictionary to make the names in the traits dataset and the community datafram match
dict_com <- read.table(header = TRUE, stringsAsFactors = FALSE, text = 
                         "old new
                       Nar_stri Nar_str
                       Tarax Tar_sp
                       Euph_sp Eup_sp
                       Phle_alp Phl_alp
                       Rhin_min Rhi_min
                       Rum_ac-la Rum_acl
                       Trien_eur Tri_eur
                       Rub_idae Rub_ida
                       Saus_alp Sau_alp
                       Ave__pub Ave_pub
                       Car_atra Car_atr
                       Hypo_rad Hyp_rad
                       Hype_mac Hyp_mac
                       Hypo_mac Hyp_mac                       
                       Bart_alp Bar_alp
                       Car_pulic Car_pul
                       Carex_sp Car_sp
                       Hier_sp Hie_sp
                       Salix_sp Sal_sp
                       Vio_can Vio_riv")

community_cover_1516<-community_cover_1516%>%
  group_by(Site, Year, species)%>%
  mutate(mean_cover=mean(cover, na.rm=TRUE))%>% #If you want the turf data use mutate, if you want the site data use summarise
  ungroup()%>%
  group_by(turfID, Year)%>%
  mutate(sum_cover = sum(cover, na.rm=TRUE))%>%
  ungroup()%>%
  mutate(species = plyr::mapvalues(species, from = dict_com$old, to = dict_com$new))%>%
  ungroup()

#### Load trait data ####

# compile CWM an Fdiv trait values
# load imputation files for each traits for all species

trait_C <- read.csv("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\speciesSitePredictions_C.csv", header =TRUE, sep = ";", dec = ",")%>%
  mutate(Species = paste0(Site, "_", Species))%>%
  select( Site, Species, predictValue, predictionSE) #, predictionSE
trait_N <- read.csv("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\speciesSitePredictions_N.csv", header =TRUE, sep = ";", dec = ",")%>%
  select(predictValue, predictionSE)
trait_CN <- read.csv("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\speciesSitePredictions_CN.ratio.csv", header =TRUE, sep = ";", dec = ",")%>%
  select(predictValue, predictionSE)
trait_SLA <- read.csv("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\speciesSitePredictions_SLA.csv", header =TRUE, sep = ";", dec = ",")%>%
  select(predictValue, predictionSE)
trait_Lth <- read.csv("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\speciesSitePredictions_Lth_ave.csv", header =TRUE, sep = ";", dec = ",")%>%
  select(predictValue, predictionSE)
trait_LDMC <- read.csv("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\speciesSitePredictions_LDMC.csv", header =TRUE, sep = ";", dec = ",")%>%
  select(predictValue, predictionSE)
trait_logLA <- read.csv("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\speciesSitePredictions_logLA.csv", header =TRUE, sep = ";", dec = ",")%>%
  select(predictValue, predictionSE)
trait_logHeight <- read.csv("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\speciesSitePredictions_logHeight.csv", header =TRUE, sep = ";", dec = ",")%>%
  select(predictValue, predictionSE)

Species_traits <-bind_cols(trait_C, trait_N, trait_CN, trait_SLA, trait_Lth, trait_LDMC, trait_logLA, trait_logHeight)%>%
  rename(C = predictValue, N = predictValue1, CN = predictValue2, SLA = predictValue3, Lth = predictValue4, LDMC = predictValue5, LA = predictValue6, Height = predictValue7)

## check distribution of imputation values of traits
ggplot(Species_traits, aes(LA))+
geom_density()+
facet_wrap(~Site)
## Distribution of imputed values within range of measured traits

## Checked SE values for each trait- Emp_nig, Emp_her, Ver_alp and Sil_vul very large SE remove from dataset
Species_traits<- Species_traits%>%
  #filter(!grepl("Emp_",  Species))%>%
  #filter(!grepl("Ver_alp",  Species))%>%
  #filter(!grepl("Sil_vul",  Species))%>%
  select(Site, Species, C, N, CN, SLA, Lth, LDMC, LA, Height)
  

# calculation of CWM and FDvar; Community weighted mean and community weighted variance of trait values
# join imputed traits with species cover
# wt.var() calculate weighted variance

community_cover_1516 <- community_cover_1516%>%
  mutate(Species =paste0(Site,"_", species))

community_FD <- left_join(community_cover_1516, Species_traits, by= c("Site", "Species"))%>%
  select(Site, turfID, Year, Species, cover, C, N, CN, SLA, Lth, LDMC, LA, Height)%>%
  group_by(turfID, Site, Year)%>%
  filter(!is.na(cover)) %>%
  mutate(richness = sum(n_distinct(Species))) %>% 
  mutate(diversity = diversity(cover, index = "shannon")) %>% 
  mutate(evenness = (diversity/log(richness)))%>%
  summarize(richness = mean(richness),
            diversity = mean(diversity),
            evenness = mean(evenness), 
            Wmean_LDMC= weighted.mean(LDMC, cover, na.rm=TRUE),
            Wmean_Lth= weighted.mean(Lth, cover, na.rm=TRUE),
            Wmean_LA= weighted.mean(LA, cover, na.rm=TRUE),
            Wmean_SLA= weighted.mean(SLA, cover, na.rm=TRUE),
            Wmean_Height= weighted.mean(Height, cover, na.rm=TRUE),
            Wmean_CN = weighted.mean(CN, cover, na.rm=TRUE),
            Wmean_C = weighted.mean(C, cover, na.rm=TRUE),
            Wmean_N = weighted.mean(N, cover, na.rm=TRUE),
            Wvar_LDMC= wt.var(LDMC, cover),
            Wvar_Lth= wt.var(Lth, cover),
            Wvar_LA= wt.var(LA, cover),
            Wvar_SLA= wt.var(SLA, cover),
            Wvar_Height= wt.var(Height, cover),
            Wvar_C = wt.var(C, cover),
            Wvar_N = wt.var(N, cover),
            Wvar_CN = wt.var(CN, cover))%>%
  mutate(Year = as.character(Year))%>%
  #gather(key= Trait, value= value, -c(turfID:Year))%>%
  ungroup()





# make boxplots to explore FD_CWM and FDvar values , first unhash gather in code above
ggplot(community_FD, aes(Site, value))+
  geom_boxplot()+
  facet_wrap(~Trait, scales = "free")

##### Biomass data #####
biomass_others <- read.csv("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\biomass_2016_others_complete.csv", header=TRUE, sep=";", dec=",", stringsAsFactors = FALSE)

biomass_others <- biomass_others%>%
  select(siteID, plotID, functional.group, dry.weight)%>%
  mutate(dry.weight = dry.weight/0.0625) # recalculate to g/m2

biomass_forbs <- read.csv("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\biomass_2016_forbs_complete.csv", header=TRUE, sep=";", dec=",", stringsAsFactors = FALSE)

biomass_forbs <- biomass_forbs%>%
  select(siteID, plotID, functional.group, dry.weight)%>%
  group_by(siteID, plotID)%>%
  mutate(dry.weight = dry.weight/0.0625)%>% # recalculate to g/m2
  summarise(dry.weight=sum(dry.weight))%>%
  ungroup()%>%
  mutate(functional.group="forbs")

# Merge the two biomass datasets together #
biomass_XC_long <- bind_rows(biomass_others, biomass_forbs)%>%
  mutate(site = recode(siteID, Ulvehaugen = "Ulv", Alrust = "Alr", Fauske = "Fau", Lavisdalen = "Lav", Hogsete = "Hog", Vikesland = "Vik", Gudmedalen = "Gud", Rambera = "Ram", Arhelleren = "Arh", Skjellingahaugen = "Skj", Veskre = "Ves", Ovstedal = "Ovs")) %>%
  mutate(Block= substr(plotID, 3,3))%>%
  mutate(turf = substr(plotID, 1,2))%>%
  mutate(turfID=paste0(site, Block, turf))%>%
  select(-Block, -turf)%>%
  group_by(turfID)%>%
  filter(functional.group %in% c( "graminoids", "forbs" , "bryophytes"))%>%  # exclude non GFB from total.biomass sum
  mutate(total.biomass=sum(dry.weight))%>%
  mutate(Temp.C = recode(site, Ulv = "6.17", Lav = "6.45",  Gud = "5.87", Skj = "6.58", Alr = "9.14", Hog = "9.17", Ram = "8.77", Ves = "8.67", Fau = "10.3", Vik = "10.55", Arh = "10.6", Ovs = "10.78"))%>%
  mutate(P.mm = recode(site, Ulv = "596", Alr = "789", Fau = "600", Lav = "1321", Hog = "1356", Vik = "1161", Gud = "1925", Ram = "1848", Arh = "2044", Skj = "2725", Ves = "3029", Ovs = "2923")) %>%
  ungroup()

biomass_XC_long$Temp.C<- as.numeric(biomass_XC_long$Temp.C)
biomass_XC_long$P.mm<- as.numeric(biomass_XC_long$P.mm)
biomass_XC <- spread(biomass_XC_long, functional.group, dry.weight)%>%
  select(site, turfID, total.biomass, bryophytes, graminoids, forbs) 

### Add together biomass data from XC and removals of 2015
#biomass<- bind_rows(biomass_XC, biomass_removals)
biomass<- biomass_XC


#### Vegetation community data ####
community_1516 <-composition %>%
  filter(!Year == 2017)%>%
  mutate(Site= substr(siteID, 1,3))%>%
  ungroup()%>%
  select(Site, turfID, Year, functionalGroup, bryophyteCov, graminoidCov, forbCov, vegetationHeight, mossHeight, cover)%>%
  group_by(turfID, Year, functionalGroup)%>%
  mutate(sum_cover = sum(cover, na.rm=TRUE))%>%
  slice(1)%>%
  ungroup()%>%
  group_by(Year, turfID) %>%
  spread(key=functionalGroup, value= sum_cover)%>%
  mutate(forb_cover = sum(forb, na.rm = TRUE),
         gram_cover = sum(graminoid, na.rm = TRUE))%>%
  distinct(turfID, .keep_all = TRUE)%>%
  select(-forb, -graminoid, -cover)%>%
  ungroup()

community_1516$Year <- as.character(community_1516$Year)

######### Add Year Specific Climate data of sites to community data #######################################################################
# Load Gridded climate data 
load("O:/FunCab/Data/FunCaB/Climate/Data/GriddedDailyClimateData2009-2016.RData")
load("O:/FunCab/Data/FunCaB/Climate/Data/GriddedMonth_AnnualClimate2009-2016.Rdata")

#description Climate data
#Mean daily temperature (°C, Temperature)
#Relative air moisture (%, RelAirMoisture)
#Mean wind (meter / second, Wind)
#Mean cloud cover (in 8 parts, no dimension, CloudCover)
#Precipitation (mm, Precipitation)

# load packages
library(ggplot2)
library(dplyr)
library(tidyr)

#create new column for year and month in monthlyClimate
monthlyClimate$Year<- format(monthlyClimate$dateMonth,format= "%Y") 
monthlyClimate$Month<- format(monthlyClimate$dateMonth,format= "%b") 
numMonth<-function(x) 
  c(jan=1,feb=2,mar=3,apr=4,mai=5,jun=6,jul=7,aug=8,sep=9,okt=10,nov=11,des=12)[tolower(x)]
monthlyClimate$Month<-numMonth(monthlyClimate$Month)

#select data for different loggers for period 2014-2016
Temperature_Grid<- filter(monthlyClimate, Logger=="Temperature", Year>2014)
Temperature_Grid<- subset(Temperature_Grid, Month>=6 & Month<10)
Precipitation_Grid<- climate%>%
                      select(Precipitation, Site, Year, Month)%>%
                      mutate(Month = as.numeric(Month))%>%
                      filter(Year>2014)
Precipitation_Grid<- subset(Precipitation_Grid, Month>=6 & Month<10)
Airmoisture_Grid<- filter(monthlyClimate, Logger=="RelAirMoisture", Year>2014)
Airmoisture_Grid<- subset(Airmoisture_Grid, Month>=6 & Month<10)

#calculate means over summer period
T_mean<- Temperature_Grid %>%
  group_by(Site, Year)%>%
  summarise(mn_T = mean(value))

#calculate means over summer period
Prec_total<- Precipitation_Grid %>%
  group_by(Site, Year)%>%
  summarise(total_P = sum(Precipitation))%>%
  mutate(Year = as.character(Year))%>%
  ungroup


# combine vegetation data with traits, biomass and climate data
community_1516 <- left_join(community_1516, T_mean, by=c("Site"="Site", "Year"="Year"))
community_1516 <- left_join(community_1516, Prec_total, by=c("Site"="Site", "Year"="Year"))

# join trait data with community cover data 
wcommunity_traits <- left_join(community_1516, community_FD, by =c( "Site", "turfID", "Year"))
# join biomass data to community and trait data
wcommunity_traits <- left_join(wcommunity_traits, biomass, by= c("turfID" = "turfID", "Site" = "site"))


#### Carbon flux data ######
#### Reading in data and making dataframes ####
CO2_flux <- read.csv("O:\\FunCab\\Data\\FunCaB\\CO2\\CO2_GPP_1516Trait04122017.csv", header=TRUE, sep=",")
CO2_flux$turfID.x[CO2_flux$turfID.x == "Ves4C"] <- "Ves5C"
CO2_flux$turfID.x[CO2_flux$turfID.x == "Gud5C"] <- "Gud16C"# rename plot to match vegetation data
# remove data of TTC plots outside of FunCaB (2016data for RTC comparison) and plots that did not have species cover information
TTC_list <- c("Fau3C", "Ovs5C", "Ulv5C", "Alr4C") 
CO2_flux <- CO2_flux[ ! CO2_flux$turfID.x %in% TTC_list, ]


CO2_flux$date <- as.Date(CO2_flux$date, tz="", format="%Y-%m-%d")
CO2_flux$site <- as.character(CO2_flux$site)
CO2_flux$treatment <- as.character(CO2_flux$treatment)
CO2_flux$removal <- as.character(CO2_flux$removal)
CO2_flux$block <- as.character(CO2_flux$block)
CO2_flux$year <- as.character(CO2_flux$year)

# select important columns
CO2_flux <- CO2_flux %>%
  select(year, site, block, turfID = turfID.x, removal, treatment, vegHeight= vegHeight.x, soilT=soilT.x, Moisture = Moisture.x, PAR= PAR.x, tempK, Reco, GPP, Reco15, GPP700)

CO2_check<- CO2_flux %>%
  group_by(turfID, year)%>%
  slice(1) # one plot no CO2flux data

CO2_traits_community <- left_join(CO2_flux, wcommunity_traits, by=c("turfID", "year" = "Year", "site" ="Site"))%>%
  rename(Temp.C = mn_T, P.mm = total_P) 


CO2_mass_traits<- CO2_traits_community%>%
  select(-block, -removal)%>%
  rename(Bryo_biomass = bryophytes, Forb_biomass = forbs, Gram_biomass = graminoids, VegetationHeight = vegetationHeight, MossHeight =     mossHeight)%>%
  mutate(T_level = recode(site, Ulv = "1", Lav = "1",  Gud = "1", Skj = "1", Alr = "2", Hog = "2", Ram = "2", Ves = "2", Fau = "3", Vik = "3", Arh = "3", Ovs = "3")) %>%
  mutate(P_level = recode(site, Ulv = "1", Alr = "1", Fau = "1", Lav = "2", Hog = "2", Vik = "2", Gud = "3", Ram = "3", Arh = "3", Skj = "4", Ves = "4", Ovs = "4"))%>%
  ungroup()

# remove plot with lacking vegetation data or only flux measurement
CO2_mass_traits<- subset(CO2_mass_traits,  !paste(turfID,year,sep="_") %in% c("Lav1XC_2016", "Ram6C_2016",  "Alr3C_2015","Gud12C_2015", "Gud12C_2016"))
# no community data for Alr3C because the plot accidentally got cut with removals before vegetation analysis, other no cover data because missing cover values at species level
#Remove plots that are missing cover data per species and thus missing CWM data "Alr3C", "Ram6C" , "Gud12C", LAv1XC

# Linear regression for biomass estimation
summary(lm(Gram_biomass~ 0 + gram_cover , data=CO2_mass_traits)) # P<0.001 R2=0.90
summary(lm(Forb_biomass~ 0 + forb_cover , data=CO2_mass_traits)) # P<0.001 R2= 0.83
#summary(lm(Bryo_biomass~ 0 + bryophyteCov, data = CO2_mass_traits))


#ggplot(CO2_mass_traits, aes(x=bryophyteCov, y= Bryo_biomass))+
#  geom_point()+
#  geom_smooth(method = "lm")

# calculate Functional group biomass based on regression results of XC plots
CO2_mass_traits$G_c.biomass<- 0+2.88*(CO2_mass_traits$gram_cover)
CO2_mass_traits$F_c.biomass<- 0+1.83*(CO2_mass_traits$forb_cover)
CO2_mass_traits$Total_c.biomass<- CO2_mass_traits$G_c.biomass+CO2_mass_traits$F_c.biomass


# recode P_level, because of NA's
CO2_mass_traits$year<-as.factor(CO2_mass_traits$year)
CO2_mass_traits$P_level<- ordered(CO2_mass_traits$P_level)
CO2_mass_traits$T_level<- ordered(CO2_mass_traits$T_level)


#count entries per column that are not NA
apply(CO2_mass_traits, 2, function(x) length(which(!is.na(x))))



############################################################################################################################################################################################################################################

