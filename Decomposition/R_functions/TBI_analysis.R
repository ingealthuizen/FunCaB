#TBI analysis


# TBI exploring data 

#check for outliers Y (k and S)
#par(mfrow = c(1, 2))
#boxplot(TBI_variables$k, 
#        main = "decomposition")
#dotchart(TBI_variables$lk, 
#         xlab = "Range of data", 
#         ylab = "Order of the data")

# possible outlier Vik k= 0.26

# remove outliers (8 measurements)
TBI_variables<-TBI_variables[!(TBI_variables$S>0.6 |TBI_variables$S<0 |TBI_variables$k<=0 | TBI_variables$k>0.025),]

#remove rows with NA for k
TBI_variables<-TBI_variables[!is.na(TBI_variables$k),]

#check normality of decomposition rate k 
#hist(TBI_variables$k)
#hist(sqrt(TBI_variables$k))

#normalize decomposition data by square root transformation
TBI_variables$sqrt.k<- sqrt(TBI_variables$k)

# TBI exploring data 

# create myVar for plotting with all variables
MyVar <- c("k", "S", "gridTemp", "gridPrec", "Temp.Var", "Prec.Var", "pH", "AvailN", "Plant_CN", "Root", "soil_CN", "Litter.C", "Litter.N", "Litter.CN", "soil_moist", "Bryo", "Gram", "Forbs", "Litter", "Live", "Total", "P_div", "M_Shannon.H")

# Check for outliers
Mydotplot(TBI_variables[, MyVar]) 


# remove outliers (7 measurements)
TBI<-TBI[!(TBI$S>0.6 | TBI$S<0 |TBI$k<=0 | TBI$k>0.03),]



# create myVar with selected variables
MyVar <- c("gridTemp", "gridPrec", "AvailN", "Plant_CN", "Root", "soil_CN", "Litter.C", "Litter.CN", "soil_moist", "Litter", "Live", "Total", "P_div", "M_Shannon.H")

# Collinearity
pairs(TBI_variables[, MyVar], lower.panel = panel.cor)

#conditional boxplot to check for collinearity between a continuous covariate and a categorical 
boxplot(gridTemp ~ factor(Temp.x), 
        data = TBI_variables)

# check for relationships
MyVar <- c("k", "S", "Temp.x", "Prec.x", "year", "gridTemp", "gridPrec", "Temp.Var","RF", "AvailN", "Plant_comm", "Root", "soil_CN", "soil_moist", "Total", "P_div", "M_Shannon.H")

pairs(TBI_variables[, MyVar], lower.panel = panel.cor, diag.panel= panel.hist)

#Plot every continuous covariate versus Y
MyX  <- c("gridTemp", "gridPrec", "Temp.Var","RF", "AvailN", "Plant_comm", "Root", "soil_CN", "soil_moist", "Total", "P_div", "M_Shannon.H")
Myxyplot(TBI_variables, MyX, "sqrt.k", MyYlab = "Decomposition (k)")

ggplot(TBI_variables, aes(M_Shannon.H, k, col = site ))+
  geom_boxplot()

#==============================================================================================================================
ggplot(TBI_variables, aes(site, color= CNdata))+
  geom_point(aes(y= mn.CN, col= "Litter" ))+
  geom_point(aes(y= Plant_comm, col= "Live"))+
  #geom_line(aes(y= Temperature.y, col= "Station_sT"))+
  #scale_linetype_discrete(name= "loggertype", labels = c("Ibutton", "Gridded", "Station"))+
  #facet_wrap(~Site)+
  #labs(x = "Date 2014", y= "Temperature (C)")+
  theme_bw()





MyVar <- c("pH", "NO3N", "NH4N", "Plant_comm", "Root", "soil_CN", "soil_moist", "Bryo", "Gram", "Forbs", "Litter", "Total", "rich", "div" , "residuals")
Mydotplot(TBI_variables[, MyVar]) 
pairs(TBI_variables[, MyVar], lower.panel = panel.cor)



# calculate CV for GridTemp and GridPrec per site per year
CV<- function (mean, sd){
  (sd/mean)*100  }

# summary of TBI variables
TBI_variables %>%
  group_by(site, year)%>%
  summarise(mean.k= mean(k), sd.k = sd(k), cv.k = CV(mean, sd))

x<-TBI_variables %>%
  group_by(year, site)%>%
  summarise(mean.T= mean(gridTemp, na.rm =TRUE),  total.P= max(gridPrec, na.rm =TRUE))

TBI.oav <- aov(sqrt.k ~ factor(year), data=TBI_variables)
plot(TBI.oav)
summary(TBI.oav)
TukeyHSD(TBI.oav)


# multilinear regression model TBI data with climate variables
TBI.model<- lm(k ~ modelTemp + gridPrec + factor(Temp.x) + factor(Prec.x) + modelTemp:gridPrec + year, data = TBI_variables)
summary(TBI.model)
plot(TBI.model)
TBI_variables$Resid<- resid(TBI.model)



ggplot(TBI_variables, aes(x=value, y=Resid, col=Temp.x)) + 
  geom_point(shape= 1)+      
  geom_hline(yintercept = 0)+
  facet_wrap(~ variable, scales = "free_x")+
  scale_color_discrete(name= "elevation", labels = c("alp", "sub", "bor"))+
  theme_bw()+
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 15), legend.title=element_text(size=14),             legend.text=element_text(size=12))

