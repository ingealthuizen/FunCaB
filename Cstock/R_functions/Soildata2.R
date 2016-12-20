#soil data
read.soildata<-function(file){                             
  soil<-read.table(file, header=TRUE, fill=TRUE, stringsAsFactors = FALSE, na.strings = "NA", dec = ",")
  names(soil)<-c("Site", "Block", "Depth", "Wetweight", "Dry30", "Dry150", "Roots", "N", "C", "CN")
  soil
}

soildata2015<-read.soildata("\\\\eir.uib.no\\home6\\ial008\\FunCab\\Data\\Soil samples\\soildata_all2015.txt")
soildata2015

Temp<-c(6.17,6.45,5.87,6.58,9.14,9.17,8.77,8.67,10.3,10.55,10.60,10.78)
names(Temp)<-c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS")
soildata2015$Temp<-Temp[soildata2015$Site]

Prec<-c(596,1321,1925,2725,789,1356,1848,3029,600,1161,2044,2923)
names(Prec)<-c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS")
soildata2015$Prec<-Prec[soildata2015$Site]

str(soildata2015)

MyVar <- c("Roots", "Temp", "Prec")
Mydotplot(soildata2015[, MyVar])
pairs(soildata2015[,MyVar], 
      lower.panel = panel.cor)

Root.lm<- lm(Roots~ factor(Depth)*Prec, data=soildata2015)
  plot(Root.lm)
  step(Root.lm)
  
p<- ggplot(soildata2015, aes(factor(Depth), Roots, color=Prec))+
  geom_jitter()
p


#calculate means +sdev 
require(plyr)

mean.roots<-ddply(soildata2015, c("Site", "Depth"), summarise,
                N= sum(!is.na(Roots)),mean= mean(Roots, na.rm=TRUE),sd= sd(Roots, na.rm=TRUE),se= sd / sqrt(N))

roots.total<-aggregate(mean.roots[,4], list(site=mean.roots$site), sum) #calculate totals means over whole depth

mean.soilN<-ddply(soildata2015, c("site", "depth"), summarise,
             N= sum(!is.na(n)),mean= mean(n, na.rm=TRUE), sd= sd(n, na.rm=TRUE),se= sd / sqrt(N))

soilN<-aggregate(mean.soilN[,4], list(site=mean.soilN$site), sum)

mean.soilC<-ddply(soildata2015, c("site", "depth"), summarise,
             N= sum(!is.na(c)),mean= mean(c, na.rm=TRUE), sd= sd(c, na.rm=TRUE),se= sd / sqrt(N))

soilC<-aggregate(mean.soilC[,4], list(site=mean.soilC$site), sum)

mean.soilCN<-ddply(soildata2015, c("site", "depth"), summarise,
              N= sum(!is.na(CN)),mean= mean(CN, na.rm=TRUE), sd= sd(CN, na.rm=TRUE),se= sd / sqrt(N))

soil.CN<- merge(soilC, soilN, by=c("site"))
          soil.CN$CN<-soil.CN$x.x/soil.CN$x.y
soil.CN

  
# correlation test data against temp and precipitation



#Barplots of soil parameters
require(ggplot2)# install package

rootsplot<- ggplot(mean.roots, aes(x=factor(Site), y=mean, fill=factor(Depth)))+
  geom_bar(stat="identity", color="black", width=0.5)+ 
  #geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
  #              width=.2)+                    # Width of the error bars
  #position=position_dodge(.9))+
  scale_x_discrete(limits=c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS"))+
  #labels=c("L1", "L2", "L3","L4","S1","S2","S3","S4","A1","A2","A3","A4")) #order sites T & Prec 
  scale_y_continuous(limits=c(0,5), breaks=seq(0, 5, 0.5))+
  scale_fill_brewer(palette="YlOrBr")+
  labs(list(x="site", y="root biomass (g)")) # make title and x and y lable
  rootsplot

soilNplot<- ggplot(mean.soilN, aes(x=factor(site), y=mean, fill=factor(depth)))+
  geom_bar(stat="identity", color="black", width=0.8, position = "dodge")+
  #geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position = "dodge")+   # Width of the error bars
  #position=position_dodge(.9))+
  scale_x_discrete(limits=c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS"))+
  #labels=c("L1", "L2", "L3","L4","S1","S2","S3","S4","A1","A2","A3","A4")) #order sites T & Prec 
  scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, 0.1))+
  scale_fill_brewer(palette="Greens")+
  labs(list(x="site", y="N%")) # make title and x and y lable
  soilNplot

soilCplot<- ggplot(mean.soilC, aes(x=factor(site), y=mean, fill=factor(depth)))+
  geom_bar(stat="identity", color="black", width=0.8, position="dodge")+
  #geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
  #              width=.2)+                    # Width of the error bars
  #position=position_dodge(.9))+
  scale_x_discrete(limits=c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS"))+
  #labels=c("L1", "L2", "L3","L4","S1","S2","S3","S4","A1","A2","A3","A4")) #order sites T & Prec 
  scale_y_continuous(limits=c(0,15), breaks=seq(0, 15, 2))+
  scale_fill_brewer(palette="Blues")+
  labs(list(x="site", y="C%")) # make title and x and y lable
  soilCplot
  
soilCNplot<- ggplot(mean.soilCN, aes(x=factor(site), y=mean, fill=factor(depth)))+
  geom_bar(stat="identity", width=0.6, color="black", position = "dodge")+
  scale_x_discrete(limits=c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS"))+
  #labels=c("L1", "L2", "L3","L4","S1","S2","S3","S4","A1","A2","A3","A4")) #order sites T & Prec 
  #scale_y_continuous(limits=c(0,20), breaks=seq(0, 20, 0.5))+
  scale_fill_brewer(palette="Reds")+
  labs(list(x="site", y="CNratio")) # make title and x and y lable
  soilCNplot

# How to get error bars in right place?
  #stacked graph adding depths to one another, errorbars
  #depth12 <- numeric()
  #           for (i in 1:nrow(rootdata2)) {
  #           if ("10" %in% rootdata2$depth[rootdata2$site==rootdata2$site[i]] == TRUE)  {
  #           if ("20" %in% rootdata2$depth[rootdata2$site==rootdata2$site[i]] == TRUE)  {
  #           if (rootdata2$depth[i]=="20") {
  #              depth12[i] <- rootdata2$roots[rootdata2$site==rootdata2$site[i] & rootdata2$soil=="10"] +
  #              rootdata2$roots[rootdata2$site==rootdata2$site[i] & rootdata2$soil=="20"]
  #           } else {depth12[i] <- rootdata2$roots[rootdata2$site==rootdata2$site[i] & rootdata2$soil=="10"]}  
  #           } else {depth12[i] <- rootdata2$roots[rootdata2$site==rootdata2$site[i] & rootdata2$soil=="10"]}    
  #           } else {depth12[i] <- rootdata2$roots[rootdata2$site==rootdata2$site[i] & rootdata2$soil=="20"]}   
  #        }  
  #depth12
  #rootdata2 <- cbind(rootdata2,depth12)
  #summary(rootdata2)