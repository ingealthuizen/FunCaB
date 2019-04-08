#### Libraries ####
library("tidyverse")
library("lubridate")
library("mosaic")
library("FD")
library("SDMTools")
library("xlsx")
library("readxl")
library("dplyr")
#### Load trait data ####

traits <-read.csv("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\Leaf_traits.csv", header=TRUE, sep = ";", stringsAsFactors = FALSE)

#### Cleaning the trait data ####

traits <- traits %>%
  rename(Height=Height..mm., Lth_1=Lth.1..mm., Lth_2= Lth.2..mm., Lth_3= Lth.3..mm., Wet_mass=Wet.mass..g., Dry_mass=Dry.mass..g., Site=Location) %>%
  select(Date, Species, Individual, Height, Lth_1, Lth_2, Lth_3, Wet_mass, Dry_mass, Site)%>%
  mutate(Date = mdy(Date))%>%
  mutate(T_level = recode(Site, Ulv = "1", Lav = "1",  Gud = "1", Skj = "1", Alr = "2", Hog = "2", Ram = "2", Ves = "2", Fau = "3", Vik = "3", Arh = "3", Ovs = "3")) %>%
  mutate(Temp = recode(Site, Ulv=6.17, Lav=6.45, Gud=5.87, Skj=6.58, Alr=9.14, Hog=9.17, Ram=8.77, Ves=8.67, Fau=10.3, Vik=10.55, Arh=10.60, Ovs=10.78))%>%
  mutate(Precip= recode(Site, Ulv=596, Lav=1321, Gud=1925, Skj=2725, Alr=789, Hog=1356, Ram=1848, Ves=3029, Fau=600, Vik=1161, Arh=2044, Ovs=2923))%>%
  mutate(P_level = recode(Site, Ulv = "1", Alr = "1", Fau = "1", Lav = "2", Hog = "2", Vik = "2", Gud = "3", Ram = "3", Arh = "3", Skj = "4", Ves = "4", Ovs = "4")) %>%
  mutate(LDMC=Dry_mass/Wet_mass)%>%
  mutate(Site = factor(Site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs"))) %>%
  mutate(Lth_ave=rowMeans(select(traits, matches("^Lth\\.\\d")), na.rm = TRUE)) %>%
  mutate(Dry_mass = replace(Dry_mass, Dry_mass < 0.0005, NA))%>%
  filter(!(Site == "Vik" & Species == "Gal_ver"))%>% #remove because of limited data
  mutate(logHeight = log(Height))
  

#### Load leaf area data ####

LA <- read.csv2("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\Leaf_area_total.csv", stringsAsFactors = FALSE)

LA<-transform(LA, Leaf_area = as.numeric(Leaf_area))

LA <- LA %>%
  filter(Leaf_area > 0.1)


#### Merge the trait data and the leaf area data and make the means ####

ind_traitdata <- traits %>%
  mutate(ID = paste0(Site, "_", Species, "_", Individual, ".jpg")) %>%
  filter(!(ID=="Ves_Leo_aut_6.jpg"))%>%
  mutate(Site_sp=paste0(Site,"_", Species)) %>%
  full_join(LA, by=c("ID"="Image_file")) %>%
  mutate(SLA=Leaf_area/Dry_mass) %>%
  mutate(logLA = log(Leaf_area))


#### Load CN data ####
CN <- read.csv2("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\CNratio.csv", dec=".", sep=";")

#Making a dictionary for the CN name abreviations
dict_CN <- read.csv2("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\Dict_CN.csv", header = TRUE, sep=";", stringsAsFactors = FALSE)

#Making a dictionary for the site names in the CN file
dict_Site <- read.table(header = TRUE, stringsAsFactors = FALSE, text = 
                          "old new
                        AR Arh
                        OV Ovs
                        VE Ves
                        SK Skj
                        LA Lav
                        GU Gud
                        UL Ulv
                        VI Vik
                        HO Hog
                        AL Alr
                        FA Fau
                        RA Ram")

CN<-CN %>%
  mutate(Site= substr(Name, 1,2))%>%
  mutate(Species = substr(Name, 3,6)) %>%
  mutate(Individual = substr(Name, 7,8))%>%
  mutate(Species = plyr::mapvalues(Species, from = dict_CN$CN_ab, to = dict_CN$Species))%>%
  mutate(Site = plyr::mapvalues(Site, from = dict_Site$old, to = dict_Site$new))%>%
  mutate(ID = paste0(Site, "_", Species, "_", Individual, ".jpg"))%>%
  rename(N = N.., C = C..)%>%
  filter(!(Name=="VECAR101"))
# values not present in dict_CN: LEU2, LEO2, SAX1, TRI1

#### Merge the trait data and the CN data ####

ind_traitdata <- full_join(ind_traitdata, CN, by=c("ID"="ID"))%>%
  select(-Humidity.., -Name, -Weight, -Method, -N.Factor, -C.Factor, -N.Blank, -C.Blank, -Memo, -Info, -Date..Time, -Site.y, -Species.y, -Individual.y, -N.Area, -C.Area) %>%
  rename(Site = Site.x, Species = Species.x, Individual=Individual.x)%>%
  group_by(Species) %>%
  group_by(Site, Species)%>%
  mutate(CN_ratio = mean(CN.ratio, na.rm= TRUE))%>%
  ungroup%>%
  filter(!(Species=="Hyp_mac" & Site=="Alr"))%>%
  filter(!(Species=="Agr_cap" & Site =="Alr" & Individual=="9"))%>%
  filter(!(Species=="Car_vag" & Site == "Ves"))%>%
  filter(!(Species=="Fes_rub" & Site == "Ulv"))%>%
  filter(!(Species=="Fes_rub" & Site == "Gud"))%>%
  filter(!(Species=="Hie_pil" & Site == "Gud"))%>%
  filter(!(Species=="Pot_cra" & Site == "Gud"))%>%
  filter(!(Species=="Ran_acr" & Site == "Skj"))%>%
  filter(!(Species=="Sax_aiz"))%>%
  filter(!(Species=="Hie_pil" & Site == "Gud"))%>%
  filter(!(Species=="Vac_myr" & Site == "Ves"))%>%
  filter(!(Species=="Ver_alp" & Site == "Ves"))



#### create df for site level, temperature level and grid=global level traits #######

site_traits <- ind_traitdata%>%
  group_by(Site, Species) %>%
  summarise(
    SLA = mean(SLA, na.rm = TRUE),
    Lth = mean(Lth_ave, na.rm = TRUE),
    Height = mean(Height, na.rm = TRUE),
    LogHeight = mean(logHeight, na.rm=TRUE),
    LDMC = mean(LDMC, na.rm = TRUE),
    LA = mean(Leaf_area, na.rm = TRUE),
    logLA = mean(logLA, na.rm =TRUE),
    CN_ratio = mean(CN.ratio, na.rm = TRUE),
    C = mean(C, na.rm = TRUE),
    N = mean(N, na.rm = TRUE))%>%
  ungroup()

Tlevel_traits <- ind_traitdata%>%
  group_by(T_level, Species) %>%
  summarise(
    SLA = mean(SLA, na.rm = TRUE),
    Lth = mean(Lth_ave, na.rm = TRUE),
    Height = mean(Height, na.rm = TRUE),
    LogHeight = mean(logHeight, na.rm=TRUE),
    LDMC = mean(LDMC, na.rm = TRUE),
    LA = mean(Leaf_area, na.rm = TRUE),
    logLA = mean(logLA, na.rm =TRUE),
    CN_ratio = mean(CN.ratio, na.rm = TRUE),
    C = mean(C, na.rm = TRUE),
    N = mean(N, na.rm = TRUE))%>%
  ungroup()

global_traits <-ind_traitdata %>%
  group_by(Species) %>%
  summarise(
    SLA = mean(SLA, na.rm = TRUE),
    Lth = mean(Lth_ave, na.rm = TRUE),
    Height = mean(Height, na.rm = TRUE),
    LogHeight = mean(logHeight, na.rm=TRUE),
    LDMC = mean(LDMC, na.rm = TRUE),
    LA = mean(Leaf_area, na.rm = TRUE),
    logLA = mean(logLA, na.rm =TRUE),
    CN_ratio = mean(CN.ratio, na.rm = TRUE),
    C = mean(C, na.rm = TRUE),
    N = mean(N, na.rm = TRUE))%>%
  ungroup()

#### Add info about species ####
species_info<- read.csv2("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\species_info.csv", sep=";", stringsAsFactors = FALSE)

species_info <- species_info %>%
  select(species, family, functionalGroup, lifeSpan, occurrence, occurrence.2) %>%
  mutate(species=gsub("\\.", "_", species))


#### WEIGHTED MEANS ####

# Reading in and cleaning the community data so that it is ready to be used only for cover

## community cover 2015-2016 with TTC
load("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\funcabComp.RData") 
## correct height data of specific plots
composition <- within(composition, vegetationHeight[turfID == 'Ram4F'] <- 80)
subVals <- !is.na(composition$vegetationHeight) <= 10
composition$vegetationHeight[subVals] <- composition$vegetationHeight[subVals] * 10
composition <- within(composition, vegetationHeight[turfID == 'Fau2C' & Year == "2015"] <- 155)
composition <- within(composition, vegetationHeight[turfID == 'Alr2C' & Year == "2015"] <- 197.5)


community_cover_1516 <- composition %>%
  filter(!Year == 2017)%>%
  select(-blockID, -TTtreat)


community_cover_1516 <- community_cover_1516 %>%
  select(turfID, Year, siteID, species, cover)%>%
  mutate(species=gsub("\\.", "_", species))%>%
  mutate(Site= substr(siteID, 1,3))%>%
  mutate(T_level = recode(Site, Ulv = "1", Lav = "1",  Gud = "1", Skj = "1", Alr = "2", Hog = "2", Ram = "2", Ves = "2", Fau = "3", Vik = "3", Arh = "3", Ovs = "3")) %>%
  mutate(P_level = recode(Site, Ulv = "1", Alr = "1", Fau = "1", Lav = "2", Hog = "2", Vik = "2", Gud = "3", Ram = "3", Arh = "3", Skj = "4", Ves = "4", Ovs = "4"))%>%
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
# Species names already changed
cover_check<- community_cover_1516%>%
  group_by(turfID, Year)%>%
  slice(1)


#### Combining vegetation data with Traitdata ####
#### Joining vegetation data wiht Traitdata on different levels, site, Tlevel and entire grid ####
Lcommunity_1516 <- left_join(community_cover_1516, site_traits , by=c( "Site"="Site", "species"="Species"))# local traits 
Tcommunity_1516 <- left_join(community_cover_1516, Tlevel_traits,  by=c( "T_level"="T_level", "species"="Species"))# T_level traits
Gcommunity_1516 <- left_join(community_cover_1516, global_traits, by=c( "species"="Species"), suffix = c("", ".y")) # Grid traits
# create on big Traitdata df with Trait dat of all levels
Super_community_1516 <- bind_rows(L = Lcommunity_1516, T = Tcommunity_1516, G = Gcommunity_1516, .id = "Trait_level")



#Search for appropriate species specific traitdata across levels, first check availability at site level, then Tlevel, lastly gridlevel
wcommunity_1516<- Super_community_1516 %>%
  mutate(Trait_level = factor(Trait_level, levels= c("L", "T" , "G")))%>%
  select(Trait_level, species, Site, T_level, P_level, Year, turfID, cover, mean_cover, sum_cover,  SLA, Lth, Height, LDMC, LA, C, N, CN_ratio ) %>%
  gather(key= Trait , value = value, -(Trait_level:sum_cover)) %>%
  filter(!is.na(value))%>%
  arrange(Trait_level)%>%
  group_by(turfID, species, Trait, Year)%>%
  slice(1)

wcommunity_1516_wide<- wcommunity_1516%>%
  group_by(turfID,Year)%>%
  spread(Trait, value)%>%
  ungroup()

wcommunity_1516_single <- wcommunity_1516 %>%
  group_by(turfID, Year)%>%
  spread(Trait, value)%>%
  select(-Trait_level)%>%
  group_by(turfID, Year, Site, T_level, P_level, species)%>%
  summarise_all( funs(mean(., na.rm = TRUE)))%>%
  ungroup()

#### calculate per turf how much percentage is local, tlevel or global traits
Trait_origin <- wcommunity_1516_single %>%
  group_by(turfID, Year)%>%
  mutate(Total_trait_cover= sum(cover, na.rm = TRUE))%>%
  mutate(Total_trait_perc= sum(cover, na.rm = TRUE)/ sum_cover*100)%>%
  slice(1)%>%
  ungroup()

Trait_origin <- Tcommunity_1516 %>%
  filter(!SLA == "NA" |!CN_ratio =="NA")%>%
  group_by(turfID, Year)%>%
  mutate(Total_trait_cover= sum(cover, na.rm = TRUE))%>%
  mutate(Total_trait_perc= sum(cover, na.rm = TRUE)/ sum_cover*100)%>%
  slice(1)%>%
  ungroup()



complete_turf <- Trait_origin%>%
  #filter(community_covered_trait<70)%>%
  distinct(turfID, .keep_all=TRUE)



#### Weighting the traits data by the community, calculating CWM and cwv using the specific mean ####
#### function to calculate weighted variance
weighted.var <- function(x, w, na.rm = FALSE) {
  if (na.rm) {
    w <- w[i <- !is.na(x)]
    x <- x[i]
  }
  sum.w <- sum(w)
  sum.w2 <- sum(w^2)
  mean.w <- sum(x * w) / sum(w)
  (sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2, na.rm = na.rm)
}


library(FD)
wcommunity_df_1516 <- wcommunity_1516_wide %>%
  group_by(turfID, Site, Year)%>%
  filter(!is.na(cover)) %>%
           #filter(!is.na(Individual.x))%>%
  summarize(Wmean_LDMC= weighted.mean(LDMC, cover, na.rm=TRUE),
            Wmean_Lth= weighted.mean(Lth, cover, na.rm=TRUE),
            Wmean_LA= weighted.mean(LA, cover, na.rm=TRUE),
            Wmean_SLA= weighted.mean(SLA, cover, na.rm=TRUE),
            Wmean_Height= weighted.mean(Height, cover, na.rm=TRUE),
            Wmean_CN = weighted.mean(CN_ratio, cover, na.rm=TRUE),
            Wmean_C = weighted.mean(C, cover, na.rm=TRUE),
            Wmean_N = weighted.mean(N, cover, na.rm=TRUE),
            #Wmean_SLN= weighted.mean(SLA_mean*N_weight, cover, na.rm=TRUE), # ! unsure about SLN calculation!
            Wvar_LDMC= weighted.var(LDMC, cover, na.rm = TRUE),
            Wvar_Lth= weighted.var(Lth, cover, na.rm = TRUE),
            Wvar_LA= weighted.var(LA, cover, na.rm = TRUE),
            Wvar_CN = weighted.var(CN_ratio, cover, na.rm = TRUE),
            Wvar_C = weighted.var(C, cover, na.rm = TRUE),
            Wvar_N = weighted.var(N, cover, na.rm = TRUE),
            Wvar_CN = weighted.var(CN_ratio, cover, na.rm = TRUE),
            Cover = mean(cover, na.rm=TRUE))%>%
            ungroup()



#remove space from turfID
wcommunity_df_1516$turfID <- gsub('\\s+', '', wcommunity_df_1516$turfID) # remove space between number and TTC
#remove duplicate turfID's because of double entry
#wcommunity_df_1516<-wcommunity_df_1516[!duplicated(wcommunity_df_1516[,c('turfID','Year')]),]


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
  mutate(P_level = recode(site, Ulv = "1", Alr = "1", Fau = "1", Lav = "2", Hog = "2", Vik = "2", Gud = "3", Ram = "3", Arh = "3", Skj = "4", Ves = "4", Ovs = "4")) %>%
  mutate(T_level = recode(site, Ulv = "Alpine", Lav = "Alpine",  Gud = "Alpine", Skj = "Alpine", Alr = "Sub-alpine", Hog = "Sub-alpine", Ram = "Sub-alpine", Ves = "Sub-alpine", Fau = "Boreal", Vik = "Boreal", Arh = "Boreal", Ovs = "Boreal"))%>%
  mutate(Temp.C = recode(site, Ulv = "6.17", Lav = "6.45",  Gud = "5.87", Skj = "6.58", Alr = "9.14", Hog = "9.17", Ram = "8.77", 
                       Ves = "8.67", Fau = "10.3", Vik = "10.55", Arh = "10.6", Ovs = "10.78"))%>%
  mutate(P.mm = recode(site, Ulv = "596", Alr = "789", Fau = "600", Lav = "1321", Hog = "1356", Vik = "1161", Gud = "1925", 
                       Ram = "1848", Arh = "2044", Skj = "2725", Ves = "3029", Ovs = "2923")) %>%
  ungroup()

biomass_XC_long$Temp.C<- as.numeric(biomass_XC_long$Temp.C)
biomass_XC_long$P.mm<- as.numeric(biomass_XC_long$P.mm)
biomass_XC <- spread(biomass_XC_long, functional.group, dry.weight)%>%
  select(site, turfID, total.biomass, bryophytes, graminoids, forbs) 

### Add together biomass data from XC and removals of 2015
#biomass<- bind_rows(biomass_XC, biomass_removals)
biomass<- biomass_XC

### Reload community data for total cover of functional groups
#community_1516 <-comp2 %>%
#  select(turfID, Year, functionalGroup, soil, TotalGraminoids, totalForbs, totalBryophytes, vegetationHeight, mossHeight, sumCover)%>%
#  distinct(turfID, .keep_all = TRUE)%>%
#  mutate(fg2 = recode(functionalGroup, "forb" = "forb", "pteridophyte" = "forb", "woody" = "forb", "graminoid" = "graminoid"))%>% 
#  mutate(totalForbs = ifelse(is.na(totalForbs) & fg2 == "forb", sumCover, totalForbs))%>%
#  mutate(TotalGraminoids = ifelse(is.na(TotalGraminoids) & fg2 == "graminoid", sumCover, TotalGraminoids)) %>% 
#  group_by(Year, turfID, fg2) %>% 
#  mutate(totalForbs = sum(totalForbs))%>%
#  group_by(Year, turfID) %>% 
#  mutate(TotalGraminoids = sum (TotalGraminoids, na.rm=TRUE))%>%
#  filter(functionalGroup == "forb")
  
# NEED TO CALCULATE total cover for FUNCAB and TTC plots!!! calculate total cover forbs and gram per plot
community_1516 <-composition %>%
  filter(!Year == 2017)%>%
  select(turfID, Year, functionalGroup, bryophyteCov, graminoidCov, forbCov, vegetationHeight, mossHeight, cover, richness, diversity, evenness)%>%
  group_by(turfID, Year, functionalGroup)%>%
  mutate(sum_cover = sum(cover, na.rm=TRUE))%>%
  slice(1)%>%
  ungroup()%>%
  group_by(Year, turfID) %>%
  spread(key=functionalGroup, value= sum_cover)%>%
  mutate(forb_cover = sum(forb, na.rm = TRUE),
         gram_cover = sum(graminoid, na.rm = TRUE))%>%
  mutate(Vasc_cover = forb_cover+gram_cover)%>%
  distinct(turfID, .keep_all = TRUE)%>%
  select(-forb, -graminoid, -cover)%>%
  ungroup()

# remove space from turfID
community_1516$turfID <- gsub('\\s+', '', community_1516$turfID) # remove space between number and TTC
#community_1516$TTtreat <- community_1516$turfID # TTreat TTC name 
#community_1516<-community_1516[!duplicated(community_1516[,c('turfID','Year')]),]

######### Add Year Specific Climate data of sites to community data ######################################################################################
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
Precipitation_Grid<- filter(monthlyClimate, Logger=="Precipitation", Year>2014)
Precipitation_Grid<- subset(Precipitation_Grid, Month>=6 & Month<10)
Airmoisture_Grid<- filter(monthlyClimate, Logger=="RelAirMoisture", Year>2014)
Airmoisture_Grid<- subset(Airmoisture_Grid, Month>=6 & Month<10)

#calculate means over summer period
T_mean<- Temperature_Grid %>%
  group_by(Site, Year)%>%
  summarise(mn_T = mean(value))
T_mean$Year<- as.numeric(T_mean$Year)

#calculate means over summer period
Prec_total<- Precipitation_Grid %>%
  group_by(Site, Year)%>%
  summarise(total_P = sum(value)) 
Prec_total$Year<- as.numeric(Prec_total$Year)

wcommunity_df_1516 <- left_join(wcommunity_df_1516, T_mean, by=c("Site"="Site", "Year"="Year"))
wcommunity_df_1516 <- left_join(wcommunity_df_1516, Prec_total, by=c("Site"="Site", "Year"="Year"))

# join trait data with community cover data 
wcommunity_df_1516 <- left_join(wcommunity_df_1516, community_1516, by= c("turfID", "Year"))
# join biomass data to community and trait data
wcommunity_df_1516 <- left_join(wcommunity_df_1516, biomass, by= c("turfID"))

#### Carbon flux data ######

#### Reading in data and making dataframes ####
CO2_flux <- read.csv("O:\\FunCab\\Data\\FunCaB\\CO2\\CO2_GPP_1516Trait04122017.csv", header=TRUE, sep=",")

# remove data of TTC plots outside of FunCaB (2016data for RTC comparison) and plots that did not have species cover information
#TTC_list <- c("Fau3C", "Ovs5C", "Ulv5C", "Alr4C") #, "Gud12C" only one measurement in 2015
#CO2_flux <- CO2_flux[ ! CO2_flux$turfID.x %in% TTC_list, ]

#str(CO2_flux)
CO2_flux$date <- as.Date(CO2_flux$date, tz="", format="%Y-%m-%d")
CO2_flux$site <- as.character(CO2_flux$site)
CO2_flux$treatment <- as.character(CO2_flux$treatment)
CO2_flux$removal <- as.character(CO2_flux$removal)
CO2_flux$block <- as.character(CO2_flux$block)

# remove unimportant columns
CO2_flux <- CO2_flux %>%
  select(year, site, block, turfID = turfID.x, removal, treatment, vegHeight= vegHeight.x, soilT=soilT.x, Moisture = Moisture.x, PAR= PAR.x, tempK, Reco, GPP, Reco15, GPP700) 
  #mutate(site = recode(site, ULV = "Ulv", ALR = "Alr", FAU = "Fau", LAV = "Lav", HOG = "Hog", VIK = "Vik", GUD = "Gud", RAM = "Ram", ARH = "Arh", SKJ = "Skj", VES = "Ves", OVS = "Ovs")) #%>%
  #mutate(turfID=paste0(site, block, removal))


CO2_traits_community <- left_join(CO2_flux, wcommunity_df_1516, by=c("turfID"="turfID", "year"="Year", "site"="Site")) %>%
  mutate(Site = factor(site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs")))%>%
  mutate(T_level = recode(site, Ulv = "1", Lav = "1",  Gud = "1", Skj = "1", Alr = "2", Hog = "2", Ram = "2", Ves = "2", Fau = "3", Vik = "3", Arh = "3", Ovs = "3"))%>%
  mutate(P_level = recode(site, Ulv = "1", Alr = "1", Fau = "1", Lav = "2", Hog = "2", Vik = "2", Gud = "3", Ram = "3", Arh = "3", 
                          Skj = "4", Ves = "4", Ovs = "4")) %>%
  select(-Temp.C, -P.mm, -site, -site.y)%>%
  rename(Temp.C = mn_T, P.mm = total_P) 

# CO2_traits_community has 20 entries to much compared to CO2_flux
#community_duplicates<- wcommunity_df_1516[duplicated(wcommunity_df_1516$turfID), ] # remove duplicated rows
#### Merging with biomass data ####

#CO2_mass_traits <- left_join(CO2_traits_community, biomass, by=c("turfID"="turfID")) 
#CO2_mass_traits<- left_join(CO2_mass_traits, community_1516, by=c("turfID"="turfID", "Year"="Year")) # double entries check plot names!

CO2_mass_traits<- CO2_traits_community%>%
  select(-block, -removal)%>%
  rename(Bryo_biomass = bryophytes, Forb_biomass = forbs, Gram_biomass = graminoids, Gram_cover = graminoidCov, Forb_cover= forbCov, Bryo_cover = bryophyteCov, VegetationHeight = vegetationHeight, MossHeight = mossHeight)


# Linear regression for biomass estimation
summary(lm(Gram_biomass~ 0 + gram_cover , data=CO2_mass_traits)) # P<0.001 R2=0.89
summary(lm(Forb_biomass~ 0 + forb_cover , data=CO2_mass_traits)) # P<0.001 R2= 0.85

# calculate Functional group biomass based on regression results of XC plots
CO2_mass_traits$G_c.biomass<- 0+2.88*(CO2_mass_traits$gram_cover)
CO2_mass_traits$F_c.biomass<- 0+1.83*(CO2_mass_traits$forb_cover)
CO2_mass_traits$Total_c.biomass<- CO2_mass_traits$G_c.biomass+CO2_mass_traits$F_c.biomass


# recode P_level, because of NA's
precL<-c(1,2,3,4,1,2,3,4,1,2,3,4)
names(precL)<-c("Ulv","Lav","Gud","Skj","Alr","Hog","Ram","Ves","Fau","Vik","Arh","Ovs")
CO2_mass_traits$P_level<-as.factor(precL[CO2_mass_traits$Site])
CO2_mass_traits$year<-as.factor(CO2_mass_traits$year)
CO2_mass_traits$P_level<- ordered(CO2_mass_traits$P_level)
CO2_mass_traits$T_level<- ordered(CO2_mass_traits$T_level)


#count entries per column that are not NA
apply(CO2_mass_traits, 2, function(x) length(which(!is.na(x))))
# vegetationHeight is missing for and Ves4GB and 2 RTC's


# no community data for Alr3C because the plot accidentally got cut with removals before vegetation analysis, other no cover data because missing cover values at species level
#Remove plots that are missing cover data per species and thus missing CWM data "Alr3C", "Ram6C" , "Gud12C", LAv1XC  and plot missing total FG_cover
CO2_mass_traits <- CO2_mass_traits %>%
  filter(!is.na(CO2_mass_traits$Wmean_LDMC))
CO2_mass_traits <- CO2_mass_traits %>%
  filter(!is.na(CO2_mass_traits$Vasc_cover))

