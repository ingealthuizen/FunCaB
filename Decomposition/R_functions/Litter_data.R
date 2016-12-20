# litter bag analysis
setwd("O:\\FunCab\\Data\\Rscript")
source("HighstatLibV10.R")
require(readxl) #require packages
require(ggplot2)
require(graphics)
require(stats)
library(plyr)
library(lattice)
require(dplyr)


Litter<-read_excel( "O:\\FunCab\\Data\\Decomposition\\litterbags calc.xlsx")
names(Litter)
str(Litter)

#exclude NA values
Litter<- na.exclude(Litter)

ggplot(Litter, aes(Incubation, Mass_Remain, col=factor(BurialSite)))+
  geom_jitter()

ggplot(Litter, aes(Incubation, Mass_Remain, col=factor(Treatment)))+
  geom_jitter()

# exclude Mass_Remain >1 = higher weight than start
I<- Litter$Mass_Remain>=1 
I

Litter2 <- Litter[c(-18, -178, -328, -356), ] # taking out ouliers S>0.6 and S<0
Litter2


ggplot(Litter2, aes(Incubation, Mass_Remain, col=factor(BurialSite)))+
  geom_jitter()+
  geom_smooth(method = "lm", se = FALSE)

ggplot(Litter2, aes(Incubation, Mass_Remain, col=factor(Treatment)))+
      geom_boxplot()
      
      