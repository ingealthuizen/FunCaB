#### Libraries ####
library("tidyverse")
library("lubridate")
library("mosaic")
library("FD")
library("SDMTools")
library("xlsx")
library("readxl")
#### Load trait data ####

traits <-read.csv("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\leaftraits2016csv.csv", header=TRUE, sep = ";", stringsAsFactors = FALSE)


#### Cleaning the trait data ####

traits <- traits %>%
  rename(Height=Height..mm., Lth_1=Lth.1..mm., Lth_2= Lth.2..mm., Lth_3= Lth.3..mm., Wet_mass=Wet.mass..g., Dry_mass=Dry.mass..g., Site=Location) %>%
  select(-Lth.average..mm.)%>%
  mutate(Date = mdy(Date))%>%
  mutate(T_level = recode(Site, Ulv = "1", Lav = "1",  Gud = "1", Skj = "1", Alr = "2", Hog = "2", Ram = "2", Ves = "2", Fau = "3", Vik = "3", Arh = "3", Ovs = "3")) %>%
  mutate(Temp = recode(Site, Ulv=6.17, Lav=6.45, Gud=5.87, Skj=6.58, Alr=9.14, Hog=9.17, Ram=8.77, Ves=8.67, Fau=10.3, Vik=10.55, Arh=10.60, Ovs=10.78))%>%
  mutate(Precip= recode(Site, Ulv=596, Lav=1321, Gud=1925, Skj=2725, Alr=789, Hog=1356, Ram=1848, Ves=3029, Fau=600, Vik=1161, Arh=2044, Ovs=2923))%>%
  mutate(P_level = recode(Site, Ulv = "1", Alr = "1", Fau = "1", Lav = "2", Hog = "2", Vik = "2", Gud = "3", Ram = "3", Arh = "3", Skj = "4", Ves = "4", Ovs = "4")) %>%
  mutate(LDMC=Dry_mass/Wet_mass)%>%
  mutate(Site = factor(Site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs"))) %>%
  mutate(Lth_ave=rowMeans(select(traits, matches("^Lth\\.\\d")), na.rm = TRUE)) %>%
  mutate(Dry_mass = replace(Dry_mass, Dry_mass < 0.0005, NA))%>%
  mutate(LogHeight = log(Height))%>%
  filter(!(Species=="Hyp_mac" & Site=="Alr"))


#### Load leaf area data ####

LA <- read.csv2("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\Leaf area.csv", stringsAsFactors = FALSE)

LA<-transform(LA, Leaf_area = as.numeric(Leaf_area))

LA <- LA %>%
  filter(Leaf_area > 0.1)


#### Merge the trait data and the leaf area data and make the means ####

traitdata <- traits %>%
  mutate(ID = paste0(Site, "_", Species, "_", Individual, ".jpg")) %>%
  filter(!(ID=="Ves_Leo_aut_6.jpg"))%>%
  mutate(Site_sp=paste0(Site,"_", Species)) %>%
  full_join(LA, by=c("ID"="Image_file")) %>%
  mutate(SLA=Leaf_area/Dry_mass) %>%
  group_by(Species) %>%
  mutate(
    SLA_mean_global = mean(SLA, na.rm = TRUE),
    Lth_mean_global = mean(Lth_ave, na.rm = TRUE),
    Height_mean_global = mean(Height, na.rm = TRUE),
    LDMC_mean_global = mean(LDMC, na.rm = TRUE),
    LA_mean_global = mean(Leaf_area, na.rm = TRUE) #,
    #count = n()
  ) %>%
  ungroup() %>%
  group_by(Site, Species) %>%
  mutate(
    SLA_mean = mean(SLA, na.rm = TRUE),
    Lth_mean = mean(Lth_ave, na.rm = TRUE),
    Height_mean = mean(Height, na.rm = TRUE),
    logHeight_mean = mean(LogHeight, na.rm = TRUE),
    LDMC_mean = mean(LDMC, na.rm = TRUE),
    LA_mean = mean(Leaf_area, na.rm = TRUE)
  ) %>%
  ungroup()


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
  mutate(N_weight = (N/100)*Weight)%>%
  mutate(C_weight = (C/100)*Weight)%>%
  mutate(logCNratio = log(CN.ratio))%>% #logtransform CNratio
  filter(!(Name=="VECAR101"))


#### Merge the trait data and the CN data ####

traitdata <- traitdata %>%
  full_join(CN, by=c("ID"="ID"))%>%
  select(-Humidity, -Name, -Weight, -Method, -N.Factor, -C.Factor, -N.Blank, -C.Blank, -Memo, -Info, -Date..Time, -Site.y, -Species.y, -Individual.y,  -N.Area, -C.Area) %>%
  rename(Site = Site.x, Species = Species.x)%>%
  group_by(Site, Species) %>%
  mutate(CN_ratio_mean = mean(CN.ratio, na.rm = TRUE),
         logCN_ratio_mean = mean(logCNratio, na.rm=TRUE),
         C_mean = mean(C, na.rm = TRUE),
         N_mean = mean(N, na.rm = TRUE))%>%
  ungroup()



#### Add info about species ####
species_info<- read.csv2("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\species_info.csv", sep=";", stringsAsFactors = FALSE)


species_info <- species_info %>%
  select(species, family, functionalGroup, lifeSpan, occurrence, occurrence.2) %>%
  mutate(species=gsub("\\.", "_", species))


traitdata <- traitdata %>%
  left_join(species_info, by = c("Species"="species"))


#### WEIGHTED MEANS ####

# Reading in and cleaning the community data so that it is ready to be used only for cover

## community cover 2015-2016 with TTC
load("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\funcabCompDataWithTTCnew.RData")
community_cover_1516 <- comp2
community_cover_1516 <- community_cover_1516 %>%
  select(turfID, Year, siteID, blockID, TTtreat, species, cover)%>%
  mutate(cover = as.numeric(cover))%>%
  mutate(species=gsub("\\.", "_", species))%>%
  mutate(Site= substr(siteID, 1,3))
#filter(!is.na(cover))%>% #Takes out the species that is not present in the dataset


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
                       Bart_alp Bar_alp
                       Car_pulic Car_pul
                       Carex_sp Car_sp
                       Hier_sp Hie_sp
                       Salix_sp Sal_sp
                       Vio_can Vio_riv")



community_cover<-community_cover%>%
  group_by(Site, species)%>%
  mutate(mean_cover=mean(cover, na.rm=TRUE))%>% #If you want the turf data use mutate, if you want the site data use summarise
  ungroup()%>%
  mutate(species = plyr::mapvalues(species, from = dict_com$old, to = dict_com$new))

community_cover_1516<-community_cover_1516%>%
  group_by(Site, species)%>%
  mutate(mean_cover=mean(cover, na.rm=TRUE))%>% #If you want the turf data use mutate, if you want the site data use summarise
  ungroup()%>%
  mutate(species = plyr::mapvalues(species, from = dict_com$old, to = dict_com$new))


### This can probably be deleted !
#### calculate community traits for 2015 and 2016 FunCaB data without TTC's
community2016 <-read.csv2("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\funcab_composition_2016.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)
community2015 <-read.csv2("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\funcab_composition_2015.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)

community<-community2016 %>%
  filter(Site!="")%>%
  mutate(Site= substr(Site, 1,3))%>%
  filter(Measure == "Cover")

community_cover<-community%>%
  select(-subPlot, -year, -date, -Measure, -recorder, -Nid.herb, -Nid.gram, -Nid.rosett, -Nid.seedling, -liver, -lichen, -litter, -soil, -rock, -X.Seedlings) %>%
  select(-TotalGraminoids, -TotalForbs, -TotalBryophytes, -VegetationHeight, -MossHeight, -comment, -ver.seedl, -canum, -totalVascular, -totalBryophytes, -acro, -pleuro, -totalLichen)%>%
  gather(species, cover, Ach.mil:Vis.vul)%>%
  mutate(cover = as.numeric(cover))%>%
  filter(!is.na(cover))%>%  #Takes out the species that is not present in the dataset
  mutate(species=gsub("\\.", "_", species))%>%
  mutate(Site = as.character(Site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs")))


community2015<-community2015 %>%
  filter(Site!="")%>%
  mutate(Site= substr(Site, 1,3))%>%
  filter(Measure == "Cover")

community_cover2015<-community2015%>%
  select(-subPlot, -year, -date, -Measure, -recorder, -Nid.herb, -Nid.gram, -Nid.rosett, -Nid.seedling, -liver, -lichen, -litter, -soil, -rock, -X.Seedlings) %>%
  select(-TotalGraminoids, -TotalForbs, -TotalBryophytes, -VegetationHeight, -MossHeight, -comment, -ver.seedl, -canum, -totalVascular, -totalBryophytes, -acro, -pleuro, -totalLichen)%>%
  gather(species, cover, Ach.mil:Vis.vul)%>%
  mutate(cover = as.numeric(cover))%>%
  filter(!is.na(cover))%>%  #Takes out the species that is not present in the dataset
  mutate(species=gsub("\\.", "_", species))%>%
  mutate(Site = as.character(Site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs")))

#Adding a dictionary to make the names in the traits dataset and the community datafram match
dict_com <- read.table(header = TRUE, stringsAsFactors = FALSE, text = 
                         "old new
                       Nar_stri Nar_str
                       Tarax Tar_sp
                       Euph_sp Eup_sp
                       Phle_alp Phl_alp
                       Rhin_min Rhi_min
                       Rum_ac_la Rum_acl
                       Trien_eur Tri_eur
                       Rub_idae Rub_ida
                       Saus_alp Sau_alp
                       Ave__pub Ave_pub
                       Car_atra Car_atr
                       Hypo_rad Hyp_rad
                       Bart_alp Bar_alp
                       Car_pulic Car_pul
                       Carex_sp Car_sp
                       Hier_sp Hie_sp
                       Salix_sp Sal_sp
                       Vio_can Vio_riv")



community_cover2015<-community_cover2015%>%
  group_by(Site, species)%>%
  mutate(mean_cover=mean(cover, na.rm=TRUE))%>% #If you want the turf data use mutate, if you want the site data use summarise
  ungroup()%>%
  mutate(species = plyr::mapvalues(species, from = dict_com$old, to = dict_com$new))

community_cover_all<- bind_rows(community_cover2015, community_cover)


#### Joining the datasets and making that ready for analysis ####

wcommunity <- full_join(community_cover_all, traitdata, by=c( "Site"="Site", "species"="Species"))
wcommunity_1516 <- full_join(community_cover_1516, traitdata, by=c( "Site"="Site", "species"="Species"))

#### Weighting the traits data by the community ####
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

### ! unsure about SLN calculation! and Variance calculation is probably wrong!!!!

wcommunity_df_1516 <- wcommunity_1516 %>%
  group_by(turfID, Site)%>%
  filter(!is.na(mean_cover)) %>%
  summarise(Wmean_LDMC= weighted.mean(LDMC_mean, cover, na.rm=TRUE),
            Wmean_Lth= weighted.mean(Lth_mean, cover, na.rm=TRUE),
            Wmean_LA= weighted.mean(LA_mean, cover, na.rm=TRUE),
            Wmean_SLA= weighted.mean(SLA_mean, cover, na.rm=TRUE),
            Wmean_Height= weighted.mean(Height_mean, cover, na.rm=TRUE),
            Wmean_logHeight= weighted.mean(logHeight_mean, cover, na.rm=TRUE),
            Wmean_CN = weighted.mean(CN_ratio_mean, cover, na.rm=TRUE),
            Wmean_C = weighted.mean(C_mean, cover, na.rm=TRUE),
            Wmean_N = weighted.mean(N_mean, cover, na.rm=TRUE),
            Wmean_logCN = weighted.mean(logCN_ratio_mean, cover, na.rm=TRUE),
            Wmean_SLN= weighted.mean(SLA_mean*N_weight, cover, na.rm=TRUE), # ! unsure about SLN calculation!
            Wvar_LDMC= weighted.var(LDMC_mean, cover, na.rm = TRUE),
            Wvar_Lth= weighted.var(Lth_mean, cover, na.rm = TRUE),
            Wvar_LA= weighted.var(LA_mean, cover, na.rm = TRUE),
            Wvar_SLA= weighted.var(SLA_mean, cover, na.rm = TRUE),
            Wvar_Height= weighted.var(Height_mean, cover, na.rm = TRUE),
            Wvar_CN = weighted.var(CN_ratio_mean, cover, na.rm = TRUE),
            Wvar_C = weighted.var(C_mean, cover, na.rm = TRUE),
            Wvar_N = weighted.var(N_mean, cover, na.rm = TRUE),
            Wvar_CN = weighted.var(CN_ratio_mean, cover, na.rm = TRUE),
            Wvar_SLN= weighted.var(SLA_mean*N_weight, cover, na.rm = TRUE),
            Cover = mean(cover, na.rm=TRUE))%>%
    mutate(P_level = recode(Site, Ulv = "1", Alr = "1", Fau = "1", Lav = "2", Hog = "2", Vik = "2", Gud = "3", Ram = "3", Arh = "3", Skj = "4", Ves = "4", Ovs = "4")) %>%
  mutate(T_level = recode(Site, Ulv = "Alpine", Lav = "Alpine",  Gud = "Alpine", Skj = "Alpine", Alr = "Sub-alpine", Hog = "Sub-alpine", Ram = "Sub-alpine", Ves = "Sub-alpine", Fau = "Boreal", Vik = "Boreal", Arh = "Boreal", Ovs = "Boreal"))%>%
  ungroup()

##### Rename TTC plots
dict_TTC <- read.table(header = TRUE, stringsAsFactors = FALSE, text = 
                         "old new
                       51TTC Fau1C
                       57TTC Fau2C
                       68TTC Fau4C
                       73TTC Fau5C
                       29TTC Alr1C
                       31TTC Alr2C
                       134TTC Vik2C
                       140TTC Vik3C
                       141TTC Vik4C
                       146TTC Vik5C
                       101TTC Hog1C
                       110TTC Hog2C
                       115TTC Hog3C
                       286TTC Ovs1C
                       291TTC Ovs2C
                       297TTC Ovs3C
                       211TTC Arh1C
                       222TTC Arh3C
                       226TTC Arh4C
                       263TTC Ves1C
                       270TTC Ves2C
                       281TTC ves5C
                       194TTC Ram4C
                       198TTC Ram5C
                       6TTC Ulv2C
                       11TTC Ulv3C
                       236TTC Skj1C
                       243TTC Skj2C
                       246TTC Skj3C
                       251TTC Skj4C
                       511TTC Gud12C")

wcommunity_df_1516$turfID <- gsub('\\s+', '', wcommunity_df_1516$turfID) # remove space between number and TTC
wcommunity_df_1516<- wcommunity_df_1516%>%
    mutate(turfID = plyr::mapvalues(turfID, from = dict_TTC$old, to = dict_TTC$new)) # rename turfID to match FunCaB naming



##### Biomass data #####

biomass_others <- read.csv("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\biomass_2016_others_complete.csv", header=TRUE, sep=";", dec=",", stringsAsFactors = FALSE)

biomass_others <- biomass_others%>%
  select(siteID, plotID, functional.group, dry.weight)

biomass_forbs <- read.csv("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\biomass_2016_forbs_complete.csv", header=TRUE, sep=";", dec=",", stringsAsFactors = FALSE)

biomass_forbs <- biomass_forbs%>%
  select(siteID, plotID, functional.group, dry.weight)%>%
  group_by(siteID, plotID)%>%
  summarise(dry.weight=sum(dry.weight))%>%
  ungroup()%>%
  mutate(functional.group="forbs")

# Merge the two biomass datasets together #

biomass_XC <- bind_rows(biomass_others, biomass_forbs)%>%
  mutate(site = recode(siteID, Ulvehaugen = "Ulv", Alrust = "Alr", Fauske = "Fau", Lavisdalen = "Lav", Hogsete = "Hog", Vikesland = "Vik", Gudmedalen = "Gud", Rambera = "Ram", Arhelleren = "Arh", Skjellingahaugen = "Skj", Veskre = "Ves", Ovstedal = "Ovs")) %>%
  mutate(Block= substr(plotID, 3,3))%>%
  mutate(turf = substr(plotID, 1,2))%>%
  mutate(turfID=paste0(site, Block, turf))%>%
  select(-Block, -turf)%>%
  group_by(turfID)%>%
  #filter(functional.group!="litter")%>%
  mutate(total.biomass=sum(dry.weight))%>%
  ungroup()

biomass_XC <- spread(biomass_XC, functional.group, dry.weight)%>%
  select(site, turfID, total.biomass, bryophytes, graminoids, forbs)

biomass_removals <- read_excel("O:\\FunCab\\Data\\Vegetation data\\biomass_removals_2015.xlsx")
biomass_removals <- biomass_removals %>%
  spread(Functional.group, dry.weight)%>%
  rename(forbs = F, bryophytes = B, graminoids = G)%>%
  mutate(total.biomass= bryophytes+graminoids+forbs)%>%
  select(site, turfID, total.biomass, bryophytes, graminoids, forbs)

### Add together biomass data from XC and removals of 2015
biomass<- bind_rows(biomass_XC, biomass_removals)


##### Combine Vegetation data of all the year ########
community2016 <-read.csv2("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\funcab_composition_2016.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)
community2016<- community2016%>%
  select(turfID, year, Measure, litter, soil, rock, TotalGraminoids, TotalForbs, TotalBryophytes, VegetationHeight,   MossHeight)

community2015 <-read.csv2("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\funcab_composition_2015.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)
community2015<- community2015%>%
  select(turfID, year,Measure, litter, soil, rock, TotalGraminoids, TotalForbs, TotalBryophytes, VegetationHeight, MossHeight)

community_all <- bind_rows(community2016, community2015) %>%
  filter(Measure =="Cover")


### from here
community_1516 <-comp2 %>%
  select(turfID, TTtreat, Year, soil, TotalGraminoids, totalForbs, totalBryophytes, vegetationHeight, mossHeight)
community_1516$TTtreat <- community_1516$turfID # TTreat TTC name 
community_1516$TTtreat <- gsub('\\s+', '', community_1516$TTtreat) # remove space between number and TTC

community_1516<-community_1516[!duplicated(community_1516[,c('turfID','Year')]),]

##### Rename TTC plots
community_1516<- community_1516%>%
mutate(turfID = plyr::mapvalues(TTtreat, from = dict_TTC$old, to = dict_TTC$new)) # rename turfID to match FunCaB naming


###### Carbon flux data ######

#### Reading in data and making dataframes ####

CO2_flux <- read.csv("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\CO2_GPP_1516Trait.csv", header=TRUE, sep=",")
#str(CO2_flux)
CO2_flux$date <- as.Date(CO2_flux$date, tz="", format="%Y-%m-%d")
CO2_flux$site <- as.character(CO2_flux$site)
CO2_flux$treatment <- as.character(CO2_flux$treatment)
CO2_flux$removal <- as.character(CO2_flux$removal)
CO2_flux$block <- as.character(CO2_flux$block)

# add mean soil moisture to each CO2flux measurement
Soilmoisture<- read_excel("O:\\FunCab\\Data\\soil moisture\\Soilmoisture_1516.xlsx")
Soilmoisture$date<- as.Date(Soilmoisture$date, tz="", format="%Y-%m-%d")
Soilmoisture$Moisture<- as.numeric(Soilmoisture$Moisture)

# Not working correctly, C2flux 1646rows (inner/leftjoin 1762)
CO2_flux<- left_join(CO2_flux, Soilmoisture, by = c("date"= "date", "site"= "site", "removal" = "removal", "block"= "block"))%>%
      distinct(X, .keep_all = TRUE)

CO2_flux <- CO2_flux %>%
  select(year, site, block, removal, treatment = treatment.x, vegHeight= vegHeight.x, soilT=soilT.x, Moisture, PAR= PAR.x, tempK, Reco, GPP, Reco15, GPP1200)%>%
  mutate(site = recode(site, ULV = "Ulv", ALR = "Alr", FAU = "Fau", LAV = "Lav", HOG = "Hog", VIK = "Vik", GUD = "Gud", RAM = "Ram", ARH = "Arh", SKJ = "Skj", VES = "Ves", OVS = "Ovs")) %>%
  mutate(turfID=paste0(site, block, removal))

CO2_traits_community <- full_join(wcommunity_df_1516, CO2_flux, by=c("turfID"="turfID")) %>%
  mutate(site = factor(site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs")))%>%
  mutate(T_level = recode(site, Ulv = "Alpine", Lav = "Alpine",  Gud = "Alpine", Skj = "Alpine", Alr = "Sub-alpine", Hog = "Sub-alpine", Ram = "Sub-alpine", Ves = "Sub-alpine", Fau = "Boreal", Vik = "Boreal", Arh = "Boreal", Ovs = "Boreal"))


#### Merging with biomass data ####

CO2_mass_traits <- left_join(CO2_traits_community, biomass, by=c("turfID"="turfID"))
CO2_mass_traits<- left_join(CO2_mass_traits, community_1516, by=c("turfID"="turfID", "year"="Year"))
CO2_mass_traits<- CO2_mass_traits%>%
  select(-site.x, -site.y)%>%
  rename(Bryo_biomass = bryophytes, Forb_biomass = forbs, Gram_biomass = graminoids, Soil_cover = soil, Gram_cover = TotalGraminoids, Forb_cover= totalForbs, Bryo_cover = totalBryophytes, VegetationHeight = vegetationHeight, MossHeight = mossHeight )
CO2_mass_traits$Vasc_cover<- CO2_mass_traits$Forb_cover+CO2_mass_traits$Gram_cover

#count entries per column that are not NA
#apply(CO2_mass_traits, 2, function(x) length(which(!is.na(x))))

### change cover data to numeric
CO2_mass_traits[,c(36:44)] <- as.numeric(as.integer(unlist(CO2_mass_traits[,c(36:44)])))


# calculate Functional group biomass based on regression results of XC plots
CO2_mass_traits$G_c.biomass<- 2.436+0.119*CO2_mass_traits$Gram_cover
CO2_mass_traits$F_c.biomass<- 0.892+0.119*CO2_mass_traits$Forb_cover
CO2_mass_traits$B_c.biomass<- 2.710+0.147*CO2_mass_traits$Bryo_cover
CO2_mass_traits$Total_c.biomass<- CO2_mass_traits$Gram_cover+CO2_mass_traits$Forb_cover+CO2_mass_traits$Bryo_cover
CO2_mass_traits$P_level<- as.factor(CO2_mass_traits$P_level)
CO2_mass_traits$VegetationHeight<- as.numeric(CO2_mass_traits$VegetationHeight)

# recode P_level, because of NA's
precL<-c(1,2,3,4,1,2,3,4,1,2,3,4)
names(precL)<-c("Ulv","Lav","Gud","Skj","Alr","Hog","Ram","Ves","Fau","Vik","Arh","Ovs")
CO2_mass_traits$P_level<-as.factor(precL[CO2_mass_traits$Site])

#####Explore cover*height- biomass relationships #######
ggplot(data=CO2_mass_traits, aes(x=Gram_cover, y=Gram_biomass))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(data=CO2_mass_traits, aes(x=Forb_cover, y=Forb_biomass))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(data=CO2_mass_traits, aes(x=Bryo_cover, y=Bryo_biomass))+
  geom_point()+
  geom_smooth(method = "lm")

# Linear regression for biomass estimation
summary(lm(Gram_biomass~Gram_cover, data=CO2_mass_traits)) # P<0.001 R2=0.51
summary(lm(Gram_biomass~VegetationHeight* Gram_cover, data=CO2_mass_traits)) # P<0.001 R2=0.51
summary(lm(Forb_biomass~Forb_cover, data=CO2_mass_traits)) # P<0.001 R2=0.38
summary(lm(Forb_biomass~Forb_cover * VegetationHeight, data=CO2_mass_traits)) # P<0.001 R2=0.47
summary(lm(Bryo_biomass~Bryo_cover, data=CO2_mass_traits)) # P<0.001 R2=0.26
summary(lm(Bryo_biomass~Bryo_cover * VegetationHeight, data=CO2_mass_traits)) # P<0.001 R2=0.39

ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=G_c.biomass, x=Gram_biomass, col = T_level, shape = P_level), size= 2, na.rm= TRUE)+
  geom_abline(intercept = 0, slope = +1, color="black", size=1.5)+
  scale_x_continuous(limits = c(0,14))+
  scale_y_continuous(limits = c(0,14))

ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=F_c.biomass, x=Forb_biomass, col = T_level, shape = P_level), size= 2, na.rm= TRUE)+
  geom_abline(intercept = 0, slope = +1, color="black", size=1.5)+
  scale_x_continuous(limits = c(0,12))+
  scale_y_continuous(limits = c(0,12))

ggplot(data=CO2_mass_traits)+
  geom_point(aes(y=B_c.biomass, x=Bryo_biomass, col = T_level, shape = P_level), size= 2, na.rm= TRUE)+
  geom_abline(intercept = 0, slope = +1, color="black", size=1.5)+
  scale_x_continuous(limits = c(0,12))+
  scale_y_continuous(limits = c(0,12))
