#=========================================================================================
# Evaluating drivers of global variation in net primary production using
# structural equation modeling and independent effects analysis
#=========================================================================================
# Prepared by Sean Michaletz (sean.michaletz@gmail.com), February 2016
# Revised by stm, 13 April 2018

# << INTRODUCTION >> ---------------------------------------------------------------------
# This file contains a walkthrough of analyses presented in:

# Michaletz ST, Kerkhoff AJ, Enquist BJ. 2018. Drivers of terrestrial plant production 
# across broad geographical gradients. Global Ecology and Biogeography 27:166-174.

# Available at https://www.researchgate.net/publication/321587633_Drivers_of_terrestrial_plant_production_across_broad_geographical_gradients


#=========================================================================================
# Contents
#=========================================================================================

#  Part 1:  Piecewise SEM beginning with Chu et al. Model B
#  Part 2:  Independent effects analysis

#=========================================================================================
# Introductory code: Load packages/libraries, set paths, load datasets
#=========================================================================================

#--Clear memory
rm(list=ls(all=T))

# << DIRECTORIES >>
#--Set working directory with a text string ending with "/".
setwd("E:/Documents/Enquist lab meetings/SEM/")


# << DATASETS >>
#--Load data.
# This is a *.csv created after removing extraneous headers from 
# https://media.nature.com/original/nature-assets/nature/journal/v537/n7620/extref/nature18269-s2.xlsx
GlobalNPP <- read.csv("nature18269-s2.csv", header=T)

# << PACKAGES >>
library(piecewiseSEM)
library(ggplot2)
library(scales)
library(gridExtra)
library(grid)
library(hier.part)
#library(semPlot)  #warning - has many dependencies and isn't especially useful anyway


#--Calculate NPP/Lgs
GlobalNPP$NPP_Lgs_gm2mo <- GlobalNPP$NPPTotal_gm2y/GlobalNPP$Lgs_mo

#--Create ln'ed variables
GlobalNPP$ln.Age_yr <- log(GlobalNPP$Age_yr)
GlobalNPP$ln.Biomass_gm2 <- log(GlobalNPP$BiomassTotal_gm2) 
GlobalNPP$ln.NPP_gm2y <- log(GlobalNPP$NPPTotal_gm2y)
GlobalNPP$ln.Lgs_mo <- log(GlobalNPP$Lgs_mo) 
GlobalNPP$ln.MAP_mm <- log(GlobalNPP$MAP_mm)
GlobalNPP$ln.GSP_mm <- log(GlobalNPP$GSP_mm)
GlobalNPP$ln.NPP_Lgs_gm2mo <- log(GlobalNPP$NPP_Lgs_gm2mo)

#--Calculate 1/kT
GlobalNPP$GSinvBT <- 1/(0.00008617*(GlobalNPP$GST_C+273.15))

#--Create new dataset that excludes all Luo data
GlobalNPP2 <- subset(GlobalNPP, Source!="Luo (1996); Ni et al. (2001)")

#--Define colorblind palette.
# Colorblind palette (http://www.cookbook-r.com/Graphs/Colors_%28ggplot2%29/)
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
               "#CC79A7", "#0072B2", "#D55E00", "#F0E442", "#999999")


#=========================================================================================
# Part 1: Piecewise SEM beginning with Chu et al. Model B
#=========================================================================================
#--Model 1: Chu et al. Model B
mod1 <- list(
  lm(ln.Biomass_gm2 ~ ln.Lgs_mo + GSinvBT + ln.GSP_mm, data = GlobalNPP2),
  lm(ln.NPP_gm2y ~ ln.Lgs_mo + GSinvBT + ln.GSP_mm + ln.Biomass_gm2, data = GlobalNPP2)
)

# Evaluate model fit and test for independence of age
sem.fit(mod1, GlobalNPP2, .progressBar = TRUE, add.vars = c("ln.Age_yr"))
# This model doens't reproduce the data well (P = 0), and we do not rejct the null that the
# model doesn't reproduce the data. The missing paths M ~ a and NPP ~ a are significant and needed
# to interpret the data. There are no missing paths from climate variables to age.
# AIC = 136, AICc = 138

# Complimentary method of assessing fit
#sem.model.fits(mod1)

# Extract path coefficients
(coef.table = sem.coefs(mod1, GlobalNPP2, standardize = 'scale')) # scaled in units of sd of mean
# Lgs indirect effects = -0.08191767*0.38929834
# GST indirect effects = -0.05386947*0.38929834
# GSP indirect effects = 0.14979859*0.38929834

# Plot using semPlot
semPaths(mod1, "std", curvePivot = TRUE)

# Plot partial effects
#nf <- layout(matrix(seq(1:6),nrow=3,ncol=2,byrow=TRUE))
#partial.resid(ln.NPP_gm2y ~ ln.Lgs_mo, mod1, GlobalNPP2, return.data.frame = FALSE)
#partial.resid(ln.NPP_gm2y ~ GSinvBT, mod1, GlobalNPP2, return.data.frame = FALSE)
#partial.resid(ln.NPP_gm2y ~ ln.GSP_mm, mod1, GlobalNPP2, return.data.frame = FALSE)
#partial.resid(ln.NPP_gm2y ~ ln.Biomass_gm2, mod1, GlobalNPP2, return.data.frame = FALSE)


#--Model 2: Direct paths from age to i) biomass and ii) NPP
mod2 <- list(
  lm(ln.Biomass_gm2 ~ ln.Lgs_mo + GSinvBT + ln.GSP_mm + ln.Age_yr, data = GlobalNPP2),
  lm(ln.NPP_gm2y ~ ln.Lgs_mo + GSinvBT + ln.GSP_mm + ln.Biomass_gm2 + ln.Age_yr, data = GlobalNPP2)
)

# Evaluate model fit
sem.fit(mod2, GlobalNPP2, .progressBar = TRUE)
# This throws errors, but they just state that there are no missing paths to test.
# This model reproduces the data well (P = 1)
# AIC = 26, AICc = 29

# Complimentary method of assessing fit
#sem.model.fits(mod2)

# Extract path coefficients
(coef.table = sem.coefs(mod2, GlobalNPP2, standardize = 'scale')) # scaled in units of sd of mean
# a indirect effects = 0.66611562*0.69184057
# Lgs indirect effects = -0.04046755*0.69184057
# GST indirect effects = 0.03110031*0.69184057
# GSP indirect effects = 0.23173200*0.69184057

# Plot partial effects
#nf <- layout(matrix(seq(1:6),nrow=3,ncol=2,byrow=TRUE))
#partial.resid(ln.NPP_gm2y ~ ln.Lgs_mo, mod2, GlobalNPP2, return.data.frame = FALSE)
#partial.resid(ln.NPP_gm2y ~ GSinvBT, mod2, GlobalNPP2, return.data.frame = FALSE)
#partial.resid(ln.NPP_gm2y ~ ln.GSP_mm, mod2, GlobalNPP2, return.data.frame = FALSE)
#partial.resid(ln.NPP_gm2y ~ ln.Biomass_gm2, mod2, GlobalNPP2, return.data.frame = FALSE)
#partial.resid(ln.NPP_gm2y ~ ln.Age_yr, mod2, GlobalNPP2, return.data.frame = FALSE)


#--Model 3: Remove Lgs as covariate and replace NPP by NPP/Lgs
mod3 <- list(
  lm(ln.Biomass_gm2 ~ GSinvBT + ln.GSP_mm + ln.Age_yr, data = GlobalNPP2),
  lm(ln.NPP_Lgs_gm2mo ~ GSinvBT + ln.GSP_mm + ln.Biomass_gm2 + ln.Age_yr, data = GlobalNPP2)
)

# Evaluate model fit
sem.fit(mod3, GlobalNPP2, .progressBar = TRUE)
# This model is preferred over model 2
# AIC = 22, AICc = 24

# Complimentary method of assessing fit
#sem.model.fits(mod3)

# Extract path coefficients
(coef.table = sem.coefs(mod3, GlobalNPP2, standardize = 'scale')) # scaled in units of sd of mean
# a indirect effects = 0.66707735*0.77559404
# GST indirect effects = 0.03842029*0.77559404
# GSP indirect effects = 0.20454072*0.77559404


# Plot partial effects
# These are *direct* (not total) effects of T and P on NPP
#nf <- layout(matrix(seq(1:4),nrow=2,ncol=2,byrow=TRUE))
#partial.resid(ln.NPP_Lgs_gm2mo ~ GSinvBT, mod3, GlobalNPP2, return.data.frame = FALSE)
#partial.resid(ln.NPP_Lgs_gm2mo ~ ln.GSP_mm, mod3, GlobalNPP2, return.data.frame = FALSE)
#partial.resid(ln.NPP_Lgs_gm2mo ~ ln.Biomass_gm2, mod3, GlobalNPP2, return.data.frame = FALSE)
#partial.resid(ln.NPP_Lgs_gm2mo ~ ln.Age_yr, mod3, GlobalNPP2, return.data.frame = FALSE)
panT <- partial.resid(ln.NPP_Lgs_gm2mo ~ GSinvBT, mod3, GlobalNPP2)
panP <- partial.resid(ln.NPP_Lgs_gm2mo ~ ln.GSP_mm, mod3, GlobalNPP2)
panM <- partial.resid(ln.NPP_Lgs_gm2mo ~ ln.Biomass_gm2, mod3, GlobalNPP2)
panA <- partial.resid(ln.NPP_Lgs_gm2mo ~ ln.Age_yr, mod3, GlobalNPP2)



## Store adjusted values in a dataframe
parReg <- data.frame(panT, panP, panM, panA)
colnames(parReg) <- c("AdjlnNPPLgs_GSinvBT", "AdjGSinvBT",  "AdjlnNPPLgs_GSP", "AdjlnGSP",  
                      "AdjlnNPPLgs_Biomass", "AdjlnBiomass", "AdjlnNPPLgs_Age", "AdjlnAge")

## Convert ln'ed data back to absolute numbers for plotting
parReg$AdjGSP <- exp(parReg$AdjlnGSP)
parReg$AdjBiomass <- exp(parReg$AdjlnBiomass)
parReg$AdjAge <- exp(parReg$AdjlnAge)
parReg$AdjNPPLgs_GSinvBT <- exp(parReg$AdjlnNPPLgs_GSinvBT)
parReg$AdjNPPLgs_GSP <- exp(parReg$AdjlnNPPLgs_GSP)
parReg$AdjNPPLgs_Biomass <- exp(parReg$AdjlnNPPLgs_Biomass)
parReg$AdjNPPLgs_Age <- exp(parReg$AdjlnNPPLgs_Age)

## Plot panels
panelT <-ggplot(parReg, aes(x=AdjGSinvBT, y=AdjNPPLgs_GSinvBT)) + 
  geom_point(shape=21, color="black") + 
  scale_y_continuous(trans="log", breaks = trans_breaks("log", function(x) exp(x), n=3), 
                     labels = trans_format("log", math_format(e^.x))) + 
  xlab(expression(atop(paste("Adjusted average growing"), paste("season temperature, <1/kT>" [gs], " (", eV^{-1}, ")")))) + 
  ylab(expression(" ")) +
  geom_smooth(method = "lm", se=FALSE, color="black") + 
  theme_bw(base_size=12) + 
  theme(legend.position="none", plot.title=element_text(hjust=0.94, vjust=-1.8))
print(panelT)

panelP <-ggplot(parReg, aes(x=AdjGSP, y=AdjNPPLgs_GSP)) + 
  geom_point(shape=21, color="black") + 
  scale_x_continuous(trans="log", breaks = trans_breaks("log", function(x) exp(x), n=3), 
                     labels = trans_format("log", math_format(e^.x))) + 
  scale_y_continuous(trans="log", breaks = trans_breaks("log", function(x) exp(x), n=4), 
                     labels = trans_format("log", math_format(e^.x))) +
  xlab(expression(atop(paste("Adjusted mean growing"), paste("season precipitation (mm)")))) + 
  ylab(expression(" ")) +
  geom_smooth(method = "lm", se=FALSE, color="black") + 
  theme_bw(base_size=12) + 
  theme(legend.position="none", plot.title=element_text(hjust=0.94, vjust=-1.8))
print(panelP)

panelM <-ggplot(parReg, aes(x=AdjBiomass, y=AdjNPPLgs_Biomass)) + 
  geom_point(shape=21, color="black") + 
  scale_x_continuous(trans="log", breaks = trans_breaks("log", function(x) exp(x), n=4), 
                     labels = trans_format("log", math_format(e^.x))) + 
  scale_y_continuous(trans="log", breaks = trans_breaks("log", function(x) exp(x), n=3), 
                     labels = trans_format("log", math_format(e^.x))) +
  xlab(expression(atop(paste("Adjusted stand biomass (g)"), paste(" ")))) + 
  ylab(expression(" ")) +
  geom_smooth(method = "lm", se=FALSE, color="black") + 
  theme_bw(base_size=12) + 
  theme(legend.position="none", plot.title=element_text(hjust=0.94, vjust=-1.8))
print(panelM)

panelA <-ggplot(parReg, aes(x=AdjAge, y=AdjNPPLgs_Age)) + 
  geom_point(shape=21, color="black") + 
  scale_x_continuous(trans="log", breaks = trans_breaks("log", function(x) exp(x), n=4), 
                     labels = trans_format("log", math_format(e^.x))) + 
  scale_y_continuous(trans="log", breaks = trans_breaks("log", function(x) exp(x), n=3), 
                     labels = trans_format("log", math_format(e^.x))) +
  xlab(expression(atop(paste("Adjusted age (yr)"), paste(" ")))) +
  ylab(expression(" ")) +
  geom_smooth(method = "lm", se=FALSE, color="black") + 
  theme_bw(base_size=12) + 
  theme(legend.position="none", plot.title=element_text(hjust=0.94, vjust=-1.8))
print(panelA)

# Plot panels together
grid.arrange(arrangeGrob(panelT, panelP, panelM, panelA, ncol=2, nrow=2, 
                         left = textGrob(expression(paste("Adjusted monthly net primary production (g   ", m^{-2}, " ", mo^{-1}, ")")), 
                                         rot = 90, vjust = 1, gp = gpar(fontsize = 12))))



#=========================================================================================
# Part 2: Independent effects analysis
#=========================================================================================
# << CHU TABLE S1 D >>
# Predicting ln(NPP/Lgs) from stand biomass, stand age, and seasonal climate variables

#--Create dataframe containing covariates ln(P), ln(a), 1/kT, ln(Mtot) 
covar.29 <- GlobalNPP2[,c(38,33,40,34)]
# Run independent effects analysis
hier.part(GlobalNPP2$ln.NPP_Lgs_gm2mo, covar.29, family = "gaussian", gof = "Rsqu")
# Save data
plotData <- hier.part(GlobalNPP2$ln.NPP_Lgs_gm2mo, covar.29, family = "gaussian", gof = "Rsqu")
plotData2 <- data.frame(variable = c("ln.GSP_mm", "ln.Age_yr", "GSinvBT", "ln.Biomass_gm2"), percent = plotData$I.perc$I)
## Relevel the variables by percent
plotData2$variable <-factor(plotData2$variable, levels=plotData2[order(-plotData2$percent), "variable"])

# Make a bar graph
ggplot(plotData2, aes(x=variable, y=percent)) + 
  geom_bar(stat = "identity") + 
  xlab(expression("Variable")) + 
  ylab(expression("Percent of independent effects")) + 
  theme_bw(base_size=12)

# Make stacked bar graph
ggplot(plotData2, aes(x="", y=percent, fill=variable))+
  geom_bar(stat = "identity") + 
  xlab(expression("")) + 
  ylab(expression("Percent of independent effects")) + 
  theme_bw(base_size=12) + 
  scale_fill_manual(values=cbPalette)

# Make a pie chart
# (you should never use a pie chart for real, they do a terrible job at visualizing data and
# can often be misleading - just ask Edward Tufte.)
ggplot(plotData2, aes(x="", y=percent, fill=variable))+
  geom_bar(width = 1, stat = "identity") + 
  xlab(expression("")) + 
  ylab(expression("Percent of independent effects")) + 
  theme_bw(base_size=12) + 
  scale_fill_manual(values=cbPalette) + coord_polar("y", start=0)



