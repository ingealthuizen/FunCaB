#### Libraries ####
library("tidyverse")
library("lubridate")
#library("mosaic")

#### Load trait data ####

traits <-read.csv("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\Leaf_traits.csv", header=TRUE, sep = ";", stringsAsFactors = FALSE)


#### Cleaning the trait data ####

traits <- traits %>%
  rename(Height=Height..mm., Lth_1=Lth.1..mm., Lth_2= Lth.2..mm., Lth_3= Lth.3..mm., Wet_mass=Wet.mass..g., Dry_mass=Dry.mass..g., Site=Location) %>%
  select(Date, Species, Individual, Height, Lth_1, Lth_2, Lth_3, Wet_mass, Dry_mass, Site)%>%
  mutate(Date = mdy(Date))%>%
  mutate(LDMC=Dry_mass/Wet_mass)%>%
  mutate(Site = factor(Site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs"))) %>%
  mutate(Lth_ave=rowMeans(select(traits, matches("^Lth\\.\\d")), na.rm = TRUE)) %>%
  mutate(Dry_mass = replace(Dry_mass, Dry_mass < 0.0005, NA))%>%
  filter(!LDMC>1)


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
  mutate(logLA = log(Leaf_area))%>%
  mutate(logHeight = log(Height))


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
traitdata <- ind_traitdata %>%
  full_join(CN, by=c("ID"="ID"))%>%
  select(-Humidity.., -Name, -Weight, -Method, -N.Factor, -C.Factor, -N.Blank, -C.Blank, -Memo, -Info, -Date..Time, -Site.y, -Species.y, -Individual.y, -N.Area, -C.Area) %>%
  rename(Site = Site.x, Species = Species.x, Individual=Individual.x)%>%
  group_by(Species) %>%
  mutate(CN_ratio_mean_global = mean(CN.ratio, na.rm = TRUE))%>%
  ungroup()%>%
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
  filter(!(Species=="Ver_alp" & Site == "Ves"))%>%
  mutate(Temp.C = recode(Site, Ulv = "6.17", Lav = "6.45",  Gud = "5.87", Skj = "6.58", Alr = "9.14", Hog = "9.17", Ram = "8.77", 
                  Ves = "8.67", Fau = "10.3", Vik = "10.55", Arh = "10.6", Ovs = "10.78"))%>%
  mutate(P.mm = recode(Site, Ulv = "596", Alr = "789", Fau = "600", Lav = "1321", Hog = "1356", Vik = "1161", Gud = "1925", 
                       Ram = "1848", Arh = "2044", Skj = "2725", Ves = "3029", Ovs = "2923")) 


load("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\funcabComp.RData")
FG_id <- composition%>%
  ungroup()%>%
  select(species, functionalGroup)%>%
  mutate(species=gsub("\\.", "_", species))%>%
  distinct(species, functionalGroup)



traitdata2 <- left_join(traitdata, FG_id, by = c("Species"="species"))
traitdata2  <- within(traitdata2 , functionalGroup[Species == 'Emp_nig'] <- "forb")
traitdata2  <- within(traitdata2 , functionalGroup[Species == 'Hyp_mac'] <- "forb")

  
write.csv(traitdata2, file = "O:\\FunCab\\Data\\FunCaB\\TraitCO2\\CompiledTraitdata2.csv")
