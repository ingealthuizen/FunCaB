#### Libraries ####
library("tidyverse")
library("lubridate")
library("mosaic")

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
  mutate(N_weight = (N../100)*Weight)%>%
  mutate(C_weight = (C../100)*Weight)%>%
  filter(!(Name=="VECAR101"))


#### Merge the trait data and the CN data ####


traitdata <- traitdata %>%
  full_join(CN, by=c("ID"="ID"))%>%
  select(-Humidity.., -Name, -Weight, -Method, -N.Factor, -C.Factor, -N.Blank, -C.Blank, -Memo, -Info, -Date..Time, -Site.y, -Species.y, -Individual.y, -N.., -C.., -N.Area, -C.Area) %>%
  rename(Site = Site.x, Species = Species.x)%>%
  group_by(Site, Species) %>%
  mutate(CN_ratio_mean = mean(CN.ratio, na.rm = TRUE))%>%
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

community <-read.csv2("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\funcab_composition_2016.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)

community<-community %>%
  filter(Site!="")%>%
  mutate(Site= substr(Site, 1,3))%>%
  filter(Measure == "Cover")

community_cover<-community%>%
  select(-subPlot, -year, -date, -Measure, -recorder, -Nid.herb, -Nid.gram, -Nid.rosett, -Nid.seedling, -liver, -lichen, -litter, -soil, -rock, -X.Seedlings) %>%
  select(-TotalGraminoids, -totalForbs, -totalBryophytes, -vegetationHeight, -mossHeight, -comment, -ver.seedl, -canum, -totalVascular, -totalBryophytes.1, -acro, -pleuro, -totalLichen)%>%
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



community_cover<-community_cover%>%
  group_by(Site, species)%>%
  mutate(mean_cover=mean(cover, na.rm=TRUE))%>% #If you want the turf data use mutate, if you want the site data use summarise
  ungroup()%>%
  mutate(species = plyr::mapvalues(species, from = dict_com$old, to = dict_com$new))



#### Joining the datasets and making that ready for analysis ####


wcommunity <- full_join(community_cover, traitdata, by=c( "Site"="Site", "species"="Species"))

#### Weighting the traits data by the community ####

wcommunity_df <- wcommunity %>%
  group_by(turfID, Site)%>%
  filter(!is.na(mean_cover)) %>%
  summarise(Wmean_LDMC= weighted.mean(LDMC_mean, cover, na.rm=TRUE),
            Wmean_Lth= weighted.mean(Lth_mean, cover, na.rm=TRUE),
            Wmean_LA= weighted.mean(LA_mean, cover, na.rm=TRUE),
            Wmean_SLA= weighted.mean(SLA_mean, cover, na.rm=TRUE),
            Wmean_Height= weighted.mean(Height_mean, cover, na.rm=TRUE),
            Wmean_CN = weighted.mean(CN_ratio_mean, cover, na.rm=TRUE))%>%
  mutate(P_level = recode(Site, Ulv = "1", Alr = "1", Fau = "1", Lav = "2", Hog = "2", Vik = "2", Gud = "3", Ram = "3", Arh = "3", Skj = "4", Ves = "4", Ovs = "4")) %>%
  mutate(T_level = recode(Site, Ulv = "Alpine", Lav = "Alpine",  Gud = "Alpine", Skj = "Alpine", Alr = "Sub-alpine", Hog = "Sub-alpine", Ram = "Sub-alpine", Ves = "Sub-alpine", Fau = "Boreal", Vik = "Boreal", Arh = "Boreal", Ovs = "Boreal"))%>%
  ungroup()

##### Biomass data #####

biomass_others <- read.csv("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\biomass_2016_others.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)

biomass_others <- biomass_others%>%
  select(siteID, plotID, functional.group, dry.weight)%>%
  mutate(functional.group = replace(functional.group, functional.group=="bryophtyes", "bryophytes"))


biomass_forbs <- read.csv("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\biomass_2016_forbs.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)

biomass_forbs <- biomass_forbs%>%
  select(siteID, plotID, functional.group, dry.weight)%>%
  group_by(siteID, plotID)%>%
  summarise(dry.weight=sum(dry.weight))%>%
  ungroup()%>%
  mutate(functional.group="forb")

# Merge the two biomass datasets together #

biomass <- bind_rows(biomass_others, biomass_forbs)%>%
  mutate(site = recode(siteID, Ulvehaugen = "Ulv", Alrust = "Alr", Fauske = "Fau", Lavisdalen = "Lav", Hogsete = "Hog", Vikesland = "Vik", Gudmedalen = "Gud", Rambera = "Ram", Arhelleren = "Arh", Skjellingahaugen = "Skj", Veskre = "Ves", Oustedal = "Ovs")) %>%
  mutate(Block= substr(plotID, 3,3))%>%
  mutate(turf = substr(plotID, 1,2))%>%
  mutate(turfID=paste0(site, Block, turf))%>%
  select(-Block, -turf)%>%
  group_by(turfID)%>%
  filter(functional.group!="litter")%>%
  mutate(total.biomass=sum(dry.weight))%>%
  ungroup()

###### Carbon flux data ######

#### Reading in data and making dataframes ####

CO2_flux <- read.csv("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\CO2_GPP_1516Trait.csv", header=TRUE, sep=",")
#str(CO2_flux)

CO2_flux <- CO2_flux %>%
  select(site, block, treatment, vegHeight=vegHeight.x, Reco15, GPP1200)%>%
  mutate(site = recode(site, ULV = "Ulv", ALR = "Alr", FAU = "Fau", LAV = "Lav", HOG = "Hog", VIK = "Vik", GUD = "Gud", RAM = "Ram", ARH = "Arh", SKJ = "Skj", VES = "Ves", OVS = "Ovs")) %>%
  mutate(turfID=paste0(site, block, treatment))

CO2_traits_community <- full_join(wcommunity_df, CO2_flux, by=c("turfID"="turfID")) %>%
  mutate(site = factor(site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs")))%>%
  mutate(T_level = recode(site, Ulv = "Alpine", Lav = "Alpine",  Gud = "Alpine", Skj = "Alpine", Alr = "Sub-alpine", Hog = "Sub-alpine", Ram = "Sub-alpine", Ves = "Sub-alpine", Fau = "Boreal", Vik = "Boreal", Arh = "Boreal", Ovs = "Boreal"))

#### Merging with biomass data ####

CO2_mass_traits <- full_join(CO2_traits_community, distinct(biomass, turfID, total.biomass), by=c("turfID"="turfID"))
