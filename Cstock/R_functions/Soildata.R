#soil data
read.soildata<-function(file){                             
  soil<-read.table(file, header=TRUE, fill=TRUE, stringsAsFactors = FALSE, na.strings = "NA", dec = ",")
  names(soil)<-c("Site", "block","Temp", "Prec", "depth", "wetweight", "dry30", "dry150", "roots", "n", "c", "CN")
  soil
}
  
  soildata<-read.soildata("\\\\eir.uib.no\\home6\\ial008\\FunCab\\Data\\Soil samples\\soildata10.txt")
  soildata

Temp<-c(6.17,6.45,5.87,6.58,9.14,9.17,8.77,8.67,10.3,10.55,10.60,10.78)
  names(Temp)<-c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS")
  soildata$Temp<-Temp[soildata$Site]
  
Prec<-c(596,1321,1925,2725,789,1356,1848,3029,600,1161,2044,2923)
  names(Prec)<-c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS")
  soildata$Prec<-Prec[soildata$Site]  
  
# install package   
require(plyr)
require(ggplot2)  
  
roots.T<-ggplot(soildata, aes(Prec, roots, color=factor(Temp)))+
         geom_point(aes(fill=factor(Temp)),shape=19, size=4)+
         geom_smooth(aes(fill=factor(Temp)), method=lm, se=FALSE)
      roots.T
      
roots.model<- lm(roots~Prec*Temp, data=soildata) 

  #plot(roots.model)
  anova(roots.model)
  summary(roots.model)

carbon.T<-ggplot(soildata, aes(Prec, c, color=factor(Temp)))+
          geom_point(aes(fill=factor(Temp)),shape=19, size=4)+
          geom_smooth(aes(fill=factor(Temp)), method=lm, se=FALSE)
      carbon.T

carbon.model<- lm(c~Prec*Temp, data=soildata)      
      plot(carbon.model)
      drop1(carbon.model)
      summary(carbon.model)
      

nitro.T<-ggplot(soildata, aes(prec, n, color=factor(temp)))+
         geom_point(aes(fill=factor(temp)),shape=19, size=4)+
         geom_smooth(aes(fill=factor(temp)), method=lm, se=FALSE)
      nitro.T

nitro.model<- lm(n~prec*temp, data=soildata)      
      plot(nitro.model)
      anova(nitro.model)
      summary(nitro.model)

CNratio.T<-ggplot(soildata, aes(prec, CN, color=factor(temp)))+
        geom_point(aes(fill=factor(temp)),shape=19, size=4)+
        geom_smooth(aes(fill=factor(temp)), method=lm, se=FALSE)
      CNratio.T

CN.model<- lm(CN~prec*temp, data=soildata)      
      plot(CN.model)
      anova(CN.model)
      summary(CN.model)

      
#calculate means +sdev 

rootdata<-ddply(soildata, c("site", "prec"), summarise,
                N= sum(!is.na(roots)),mean= mean(roots, na.rm=TRUE),sd= sd(roots, na.rm=TRUE),se= sd / sqrt(N))
  temp<-factor(soildata$temp[order(soildata$site)][c(TRUE, FALSE, FALSE, FALSE)])
  rootdata<-cbind(rootdata,temp)
  rootdata

rootbarplot<- ggplot(rootdata, aes(x=prec, y=mean, fill=temp))+
  geom_bar(stat="identity", width=0.65, position=position_dodge(width=0.65), color="black")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(width=0.65),  width=.2)+
  scale_fill_manual(name="Temperature",labels=c("7.5°C","9.5°C","11.5°C"), values=c("56B4E9","#009E73","#E69F00))+
  scale_x_discrete(limits=c("600", "1200", "2000", "2700"))+
  labs(list(x="precipation (mm/y)", y="root biomass (g/m2)"))+
  theme(axis.title.y = element_text(size = rel(2.5), angle = 90))+
  theme(axis.title.x = element_text(size = rel(2.5)))+
  theme(axis.text = element_text(size= rel(1.5), colour = "black"))+
  theme(legend.text= element_text(size= rel(1.0), colour = "black"))
  rootbarplot
  

Ndata<-ddply(soildata, c("site", "prec"), summarise,
                N= sum(!is.na(n)),mean= mean(n, na.rm=TRUE), sd= sd(n, na.rm=TRUE),se= sd / sqrt(N))
    temp<-factor(soildata$temp[order(soildata$site)][c(TRUE, FALSE, FALSE, FALSE)])
    Ndata<-cbind(Ndata,temp)
    Ndata

Nbarplot<- ggplot(Ndata, aes(x=prec, y=mean, fill=temp))+
  geom_bar(stat="identity", width=0.85, position=position_dodge(width=0.85), color="black")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(width=0.85),  width=.2)+
  scale_fill_manual(name="Temperature",labels=c("7.5°C","9.5°C","11.5°C"), values=c("#56B4E9","#009E73","#E69F00")+
  scale_x_discrete(limits=c("600", "1200", "2000", "2700"))+
  labs(list(x="precipation (mm/y)", y="N%"))+
  theme(axis.title.y = element_text(size = rel(2.5), angle = 90))+
  theme(axis.title.x = element_text(size = rel(2.5)))+
  theme(axis.text = element_text(size= rel(1.5), colour = "black"))+
  theme(legend.title= element_text(size= rel(1.5), colour = "black"))+
  theme(legend.text= element_text(size= rel(1.5), colour = "black"))+
  theme(legend.position = c(.9, .8))
  Nbarplot  
    
    
Cdata<-ddply(soildata, c("site", "prec"), summarise,
               N= sum(!is.na(c)),mean= mean(c, na.rm=TRUE), sd= sd(c, na.rm=TRUE),se= sd / sqrt(N))
    temp<-factor(soildata$temp[order(soildata$site)][c(TRUE, FALSE, FALSE, FALSE)])
    Cdata<-cbind(Cdata,temp)
    Cdata
    
Cbarplot<- ggplot(Cdata, aes(x=prec, y=mean, fill=temp))+
  geom_bar(stat="identity", width=0.85, position=position_dodge(width=0.85), color="black", binwidth=0.2)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(width=0.85),  width=.2)+
  scale_fill_manual(name="Temperature",labels=c("7.5°C","9.5°C","11.5°C"), values=c("#56B4E9","#009E73","#E69F00"))+
  scale_x_discrete(limits=c("600", "1200", "2000", "2700"))+
  labs(list(x="precipation (mm/y)", y="C%"))+
  theme(axis.title.y = element_text(size = rel(2.5), angle = 90))+
  theme(axis.title.x = element_text(size = rel(2.5)))+
  theme(axis.text = element_text(size= rel(1.5), colour = "black"))+
  theme(legend.title= element_text(size= rel(1.5), colour = "black"))+
  theme(legend.text= element_text(size= rel(1.5), colour = "black"))+
  theme(legend.position = c(.9, .8))
  Cbarplot  

CNdata<-ddply(soildata, c("site", "prec"), summarise,
               N= sum(!is.na(CN)),mean= mean(CN, na.rm=TRUE), sd= sd(CN, na.rm=TRUE),se= sd / sqrt(N))
  temp<-factor(soildata$temp[order(soildata$site)][c(TRUE, FALSE, FALSE, FALSE)])
  CNdata<-cbind(CNdata,temp)
  CNdata
  
CNbarplot<- ggplot(CNdata, aes(x=prec, y=mean, fill=temp))+
  geom_bar(stat="identity", width=0.85, position=position_dodge(width=0.85), color="black", binwidth=0.2)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(width=0.85),  width=.2)+
  scale_fill_manual(name="Temperature",labels=c("7.5°C","9.5°C","11.5°C"), values=c("#66CCFF","#FF9933","#33CC33"))+
  scale_x_discrete(limits=c("600", "1200", "2000", "2700"))+
  labs(list(x="precipation (mm/y)", y="C:N ratio"))+
  theme(axis.title.y = element_text(size = rel(2.5), angle = 90))+
  theme(axis.title.x = element_text(size = rel(2.5)))+
  theme(axis.text = element_text(size= rel(1.5), colour = "black"))+
  theme(legend.title= element_text(size= rel(1.5), colour = "black"))+
  theme(legend.text= element_text(size= rel(1.5), colour = "black"))+
  theme(legend.position = c(.9, .2))
 CNbarplot



#posthoc test to see which site differ from which

  
      
#Boxplots of soil parameters
require(ggplot2) # install package

rootbarplot<- ggplot(rootdata, aes(x=factor(site), y=mean, fill=factor(depth)))+
  geom_bar(stat="identity",fill="sandybrown" ,color="black", width=0.65) +
  scale_fill_brewer(palette="Reds")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
               width=.2)+                    # Width of the error bars
                #position=position_dodge(.9))+
  scale_x_discrete(limits=c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS"))+
                  #labels=c("L1", "L2", "L3","L4","S1","S2","S3","S4","A1","A2","A3","A4")) #order sites T & Prec 
  scale_y_continuous(limits=c(0,2), breaks=seq(0, 2, 0.25))+
  
  labs(list(x="site", y="root biomass (g/m2)")) # make title and x and y lable
  rootbarplot
  
Nbarplot<- ggplot(Ndata, aes(x=factor(site), y=mean))+
    geom_bar(stat="identity", fill="olivedrab2", color="black", width=0.65)+
    #scale_fill_brewer(palette="Greens")+
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                  width=.2)+                    # Width of the error bars
    #position=position_dodge(.9))+
    scale_x_discrete(limits=c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS"))+
    #labels=c("L1", "L2", "L3","L4","S1","S2","S3","S4","A1","A2","A3","A4")) #order sites T & Prec 
    scale_y_continuous(limits=c(0,1.5), breaks=seq(0, 1.5, 0.1))+
  
  labs(list(x="site", y="N%")) # make title and x and y lable
  Nbarplot
    
Cbarplot<- ggplot(Cdata, aes(x=factor(site), y=mean, fill=factor(depth)))+
    geom_bar(stat="identity", fill="deepskyblue2", color="black", width=0.65)+
    #scale_fill_brewer(palette="564")+
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                  width=.2)+                    # Width of the error bars
    #position=position_dodge(.9))+
    scale_x_discrete(limits=c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS"))+
    #labels=c("L1", "L2", "L3","L4","S1","S2","S3","S4","A1","A2","A3","A4")) #order sites T & Prec 
    scale_y_continuous(limits=c(0,20), breaks=seq(0, 20, 2))+
    
    labs(list(x="site", y="C%")) # make title and x and y lable
    Cbarplot
