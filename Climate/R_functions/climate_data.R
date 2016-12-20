# climate data
library(ggplot2)
library(plyr)
library(reshape2)

datadir <- "O:\\FunCab\\Data\\Climate data\\csv\\"

sites <- c("Alrust", "Arhelleren", "Fauske", "Gudmedalen" , "Hogsete", "Lavisdalen", "Ovstedal", "Rambera", "Skjellingahaugen", "Ulvehaugen", "Veskre", "Vikesland")

clim <- lapply(sites, function(S){
  k<-read.table(paste0(datadir, S, ".csv"), header = TRUE, sep=";", dec = ",")
  k$date <- as.POSIXct(k$date, format = "%d.%m.%Y %H:%M", tz = "NMT")
  k$SiteShort <- NULL
  k$ID <-NULL
  k
})
names(clim)<-sites
str(clim)


ALR2015<-clim[[1]][clim[[1]]$year == 2015,]
ALR2015jun<- ALR2015[ALR2015$month == 6,]
ALR2015jul<- ALR2015[ALR2015$month == 7,]
ALR2015aug<- ALR2015[ALR2015$month == 8,]

p<- ggplot(ALR2015jul, aes(hour, tempSoil, col=factor(day)))
           p+geom_line()

# can probably do this in a loop for all sites, but I don't know where to start        
ARH2015<-clim[[2]][clim[[2]]$year == 2015,]
FAU2015<-clim[[3]][clim[[3]]$year == 2015,]                      
GUD2015<-clim[[4]][clim[[4]]$year == 2015,]  
  GUD2015aug<- GUD2015[GUD2015$month == 8,]
  
p<- ggplot(GUD2015aug, aes(hour, tempSoil, col=factor(day)))
  p+geom_line()

HOG2015<-clim[[5]][clim[[5]]$year == 2015,] 
LAV2015<-clim[[6]][clim[[6]]$year == 2015,] 
OVS2015<-clim[[7]][clim[[7]]$year == 2015,] 
RAM2015<-clim[[8]][clim[[8]]$year == 2015,] 
SKJ2015<-clim[[9]][clim[[9]]$year == 2015,] 
ULV2015<-clim[[10]][clim[[10]]$year == 2015,] 
VES2015<-clim[[11]][clim[[11]]$year == 2015,] 
VIK2015<-clim[[12]][clim[[12]]$year == 2015,] 




format(clim[[1]]$date[1], format = "b%")
ggplot(clim[[1]], aes(x = date, y = precip)) + geom_path()


sapply(clim,function(x){
  clim1 <- melt(x, id.vars = c("Site", "date"), measure.vars = c("temp2m", "temp30cm", "tempGroundGrass", "tempSoil"))
  g <- ggplot(clim1, aes(x = date, y = value, colour = variable)) + geom_line() + ggtitle(x$Site[1])
  print(g)
})


clim1<-ddply(clim[[1]], .(format(date, "%b.%Y")), function(x) colMeans(x[, c("temp2m", "temp30cm", "tempGroundGrass", "tempSoil")], na.rm = TRUE))



Alrust.clim <-read.table("\\\\eir.uib.no\\home6\\ial008\\FunCab\\Data\\Climate data\\csv\\Alrust.csv", header = TRUE, sep= ";", dec = ",")

Alrust.clim$date<- as.POSIXct(Alrust.clim$date, format = "%d.%m.%Y %H:%M")
head(Alrust.clim)
str(Alrust.clim)

Alrust2015<- Alrust.clim[Alrust.clim$year == "2015", na.rm= TRUE]

p<-ggplot(Alrust2015, aes(tempSoil~date))
p+geom_point()

