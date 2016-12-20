# soilmoisture data2015 
require(readxl) #install packages
require(ggplot2)
require(graphics)
require(lattice)
require(plyr)
require(dplyr)

read.soilmoisture<-function(file){
    moisture<-read_excel(file, sheet=1, col_names=TRUE, col_type= NULL) #read excel file#read in soilmoisture data
    moisture$date<- as.Date(moisture$date, format="%d.%m.%y")
    moisture$date<-format(moisture$date, format = "%y-%m-%d")
    moisture$TurfID<-NULL
    moisture$blockSD<-NULL
    #moisture$treatment<-factor(moisture$Treatment)
    moisture<-moisture[!is.na(moisture$block), ] # remove rows with no data df[!(is.na(df$start_pc) | df$start_pc==""), ]
    moisture
}
moisture.data<-read.soilmoisture("\\\\eir.uib.no\\home6\\ial008\\FunCab\\Data\\soil moisture\\Soil moisture2015 complete.xlsx")

#calculate mean soilmoisture per plot
Moisture.M<- rowMeans(subset(moisture.data, select = c(M1, M2, M3, M4)))
Moisture<-cbind(moisture.data[,1:4], Moisture.M)


#first run R workfile.R to line 60
# add mean soil moisture to CO2 flux data
MergeLD.new<-merge(MergeLD, Moisture, by=c("site", "block", "treatment", "date")) # missing 15 measurements
head(MergeLD.new)
str(MergeLD.new)

# create xl file with all CO2 data and mean soil moisture
library(xlsx)
write.xlsx(MergeLD.new, "O:\\FunCab\\Data\\CO2 flux\\data2015_overview.xlsx")

xyplot(GPP ~ Moisture.M  | factor(site), data = MergeLD.new,
       xlab = "Moisture", ylab = "GPP")

# code for pairs panel.hist
panel.hist <- function(x, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

# code for pairs panel.cor
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(mergeLD.new[,30:37], diag.panel = panel.hist, upper.panel = panel.smooth, lower.panel = panel.cor)

NAPerVariable <- function(X1) {
  D <- is.na(X1)
  colSums(D)
}

H <- NAPerVariable(moisture.data[,5:12])
H

p<-ggplot(moisture.data, aes(site, Mean, group = date, col = date))
  p+geom_jitter()+
    scale_x_discrete(limits=c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS"))


#link moisture mean to fluxdata, according to date, site, block, treatment
head(mergeLD)
head(moisture.data)

FluxMoisture<-left_join(MergeLD, moisture.data, by= date, site, treatment, block)

# there are gonna be missing values, NO SEEDCLIM plots in mergeLD.   

flux.moisture<-merge(overviewsitesdata, moisture.data, by=c("site", "block", "treatment", "date"))
# overviewsitesdata does not have column date! > create new column with date
