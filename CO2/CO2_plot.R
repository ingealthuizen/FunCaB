# flux data exploring plots

#multipanel plots exploring data spread per site 
xyplot(GPPnew ~ block | factor(site), data = mergeLD,
       xlab = "BLOCK",ylab = "GPP", jitter.x=3, pch=16, cex=1.2, scales = list(x=list(relation ="free")))


xyplot(GPP ~ temp.x | factor(site), data = mergeLD,
       xlab = "Temp", ylab = "GPP")

xyplot(GPP ~ PAR.x  | factor(site), data = mergeLD,
       xlab = "PAR", ylab = "GPP")

q<- ggplot(mergeLD, aes(PAR.x, GPP, col=factor(templevel.x)))
q+geom_point()

xyplot(Reco ~ starttime.y | factor(site), data = mergeLD, na.rm=TRUE,
       xlab = "date",ylab = "Reco", scales = list(x   = list(relation = "free"), y   = list(relation = "same")))

xyplot(temp.x ~ starttime.y | factor(site), data = mergeLD, 
       xlab = "date",ylab = "TEMP", scales = list(x   = list(relation = "free"), y   = list(relation = "same")))

xyplot(temp.x ~ timetime.x | factor(site), data = mergeLD,
       xlab = "date",ylab = "TEMP", scales = list(x   = list(relation = "free"), y   = list(relation = "same")))

xyplot(GPP ~ starttime.x | factor(site), data = mergeLD,
       xlab = "date",ylab = "GPP", scales = list(x   = list(relation = "free"), y   = list(relation = "same")))

# boxplots for exploring spread of flux values within sites 
p<-ggplot(mergeLD, aes(site,Reco))
  p+geom_boxplot()+
  scale_x_discrete(limits=c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS"))

p<- ggplot(mergeLD, aes(site,GPP))
p+geom_boxplot()+ 
  scale_x_discrete(limits=c("ULV","LAV","GUD","SKJ","ALR","HOG","RAM","VES","FAU","VIK","ARH","OVS"))

# plot of temp and prec effects on Reco and GPP
p<-ggplot(mergeLD, aes(factor(preclevel.x), Reco))
  p+geom_boxplot()#aes(fill=factor(templevel.x)))

p<-ggplot(mergeLD, aes(factor(templevel.x), Reco))
  p+geom_boxplot()#aes(fill=factor(preclevel.x)))  

p<-ggplot(mergeLD, aes(factor(preclevel.x), GPP))
  p+geom_boxplot()#aes(fill=factor(templevel.x)))

p<-ggplot(mergeLD, aes(factor(templevel.x), GPP))
  p+geom_boxplot()#aes(fill=factor(preclevel.x)))


p<- ggplot(subsetD, aes(templevel, Reco15, colour= factor(preclevel)))
p+geom_jitter()

p<- ggplot(subsetD, aes(preclevel, Reco15, colour = factor(templevel)))
p+geom_jitter()

p<- ggplot(subsetD, aes(factor(preclevel), Reco15, fill= factor(templevel))) 
p+geom_boxplot()

p<- ggplot(subsetD, aes(factor(templevel), Reco15)) 
#fill= factor(preclevel))) 
p+geom_boxplot()