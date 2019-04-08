#### Libraries ####
library("tidyverse")
library("lubridate")
library("mosaic")
library("FD")
library("SDMTools")
library("xlsx")
library("readxl")
library("dplyr")

#### Load community data 2015-2016 ####
load("O:\\FunCab\\Data\\FunCaB\\TraitCO2\\funcabComp.RData") 
## correct height data of specific plots
composition <- within(composition, vegetationHeight[turfID == 'Ram4F'] <- 80)
subVals <- !is.na(composition$vegetationHeight) <= 10
composition$vegetationHeight[subVals] <- composition$vegetationHeight[subVals] * 10
composition <- within(composition, vegetationHeight[turfID == 'Fau2C' & Year == "2015"] <- 155)
composition <- within(composition, vegetationHeight[turfID == 'Alr2C' & Year == "2015"] <- 197.5)


community_cover_1516 <- composition %>%
  filter(!Year == 2017)%>%
  select(-blockID, -TTtreat)%>%
  ungroup()

# Process community data
community_cover_1516 <- community_cover_1516 %>%
  select(turfID, Year, siteID, species, functionalGroup, cover)%>%
  mutate(turfID_Y = paste0(turfID, "_" ,Year))%>%
  mutate(species=gsub("\\.", "_", species))%>%
  mutate(Site= substr(siteID, 1,3))

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

# calculate mean cover for species at site = mean_cover, and total cover within each turf = sum_cover
community_cover_1516<-community_cover_1516%>%
  group_by(Site, Year, species)%>%
  mutate(mean_cover=mean(cover, na.rm=TRUE))%>% #If you want the turf data use mutate, if you want the site data use summarise
  ungroup()%>%
  group_by(turfID, Year)%>%
  mutate(sum_cover = sum(cover, na.rm=TRUE))%>%
  ungroup()%>%
  mutate(species = plyr::mapvalues(species, from = dict_com$old, to = dict_com$new))%>%
  mutate(Temp.C = recode(Site, Ulv = "6.17", Lav = "6.45",  Gud = "5.87", Skj = "6.58", Alr = "9.14", Hog = "9.17", Ram = "8.77", 
                         Ves = "8.67", Fau = "10.3", Vik = "10.55", Arh = "10.6", Ovs = "10.78"))%>%
  mutate(P.mm = recode(Site, Ulv = "596", Alr = "789", Fau = "600", Lav = "1321", Hog = "1356", Vik = "1161", Gud = "1925", 
                       Ram = "1848", Arh = "2044", Skj = "2725", Ves = "3029", Ovs = "2923")) %>%
  ungroup()

# Species names already changed

write.csv(community_cover_1516, file = "O:\\FunCab\\Data\\FunCaB\\TraitCO2\\CompiledResponseData2.csv")


