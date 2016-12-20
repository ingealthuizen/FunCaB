#Create map of Site locations
#Load packages
library(rgdal)
library(ggmap)
library(ggplot2) 
library(lattice)
library(mgcv)

FC<-read.table("\\\\eir.uib.no\\home6\\ial008\\R beginners course\\FunCaBsites.txt", header=TRUE, dec = ".")
head(FC)
str(FC)

#googlemap
range(FC$Lat)
range(FC$Lon)
FCmap   <- get_map(location = c(4, 60, 9.5, 61.5),
                    zoom = 7,
                    maptype= "satellite",
                    col = "bw")         

p <- ggmap(FCmap)
p <- p + geom_point(data = FC,
                    aes(Lon, Lat), size = 2.5, color= "white", shape= 16)

p <- p + xlab("Longitude") + ylab("Latitude")  
p


FC$Xkm<-FC$UTMx/1000
FC$Ykm<-FC$UTMy/1000

