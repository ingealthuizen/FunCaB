# Litter transplant 

library(readxl) #require packages
library(ggplot2)
library(graphics)
library(stats)
#library(plyr)
library(lattice)
library(dplyr)
library(reshape2)

#load litter transplant data
Litter<- read.table("\\\\eir.uib.no\\home6\\ial008\\FunCab\\Data\\Decomposition\\LitterTransplant.txt", header= TRUE, dec= ",")

Litter$Time<- as.numeric(Litter$Time)
Litter$Burial<- as.Date(Litter$Burial, format= "%d.%m.%Y")
Litter$Collection<- as.Date(Litter$Collection, format= "%d.%m.%Y")             


#remove empty or bad data points for analysis
Litter_clean<- subset(Litter,is.na(Comment))


#calculate mean litter loss of same litter from each Donorsite and treatment and timestep
DonorLitter<- Litter_clean%>%
  group_by(DonorSite, Treatment, Timestep, Time) %>%
  summarise(N.sample =length(W_loss), m.Loss = mean(W_loss), sd.Loss = sd(W_loss), se.Loss   = sd.Loss / sqrt(N.sample), m.remain = mean(W_remain), sd.remain = sd(W_remain), se.remain  = sd.remain / sqrt(N.sample))

#change order of sites in dataframe
DonorLitter$DonorSite <- factor(DonorLitter$DonorSite, levels = c("FAU","VIK","ARH","OVS","ALR","HOG","RAM","VES", "ULV","LAV","GUD",                                                                    "SKJ"))

#calculate mean litter loss of different litter at Burial site and treatment and timestep
BurialLitter<- Litter_clean%>%
  group_by(BurialSite, Treatment, Timestep, Time) %>%
  summarise(N.sample =length(W_loss), m.Loss = mean(W_loss), sd.Loss = sd(W_loss), se.Loss   = sd.Loss / sqrt(N.sample), m.remain = mean(W_remain), sd.remain = sd(W_remain), se.remain  = sd.remain / sqrt(N.sample))

#change order of sites in dataframe
BurialLitter$BurialSite <- factor(BurialLitter$BurialSite, levels=c("FAU","VIK","ARH","OVS","ALR","HOG","RAM","VES", "ULV","LAV","GUD",                                                                     "SKJ"))

# plots of %litter loss through time
#comparing same litter at different sites so treatment
ggplot(DonorLitter, aes(Time, col = factor(Treatment)))+
  geom_point(aes(y= m.Loss), size= 5, position = position_dodge(0.5))+
  geom_line(aes(y= m.Loss),size=1, position = position_dodge(0.5))+
  facet_wrap(~DonorSite)+
  ggtitle("%lost of DONORlitter (same litter/different site)")

# comparing different litter at same site
ggplot(BurialLitter, aes(Timestep, col = factor(Treatment)))+
  geom_point(aes(y= m.Loss), size= 5, position = position_dodge(0.5))+
  geom_line(aes(y= m.Loss),size=1, position = position_dodge(0.5))+
  facet_wrap(~BurialSite)+
  ggtitle("%lost at Burialsite (different litter/same site)")

# plots of %litter remain through time
ggplot(DonorLitter, aes(Time, col = factor(Treatment)))+
  geom_point(aes(y= m.remain), size= 5, position = position_dodge(0.5))+
  geom_line(aes(y= m.remain),size=1, position = position_dodge(0.5))+
  facet_wrap(~DonorSite)+
  ggtitle("%remain of DONORlitter (same litter/different site)")

ggplot(BurialLitter, aes(Time, col = factor(Treatment)))+
  geom_point(aes(y= m.remain), size= 5, position = position_dodge(0.5))+
  geom_line(aes(y= m.remain),size=1, position = position_dodge(0.5))+
  facet_wrap(~BurialSite)+
  ggtitle("%remain of DONORlitter (different litter/same site)")

 
