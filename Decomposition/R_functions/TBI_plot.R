# TBI plots 

#change names of Temperature and precipitation levels
levels(TBI_variables$Temp.x) <- c("Alp", "Sub", "Bor")
levels(TBI_variables$Prec.x) <- c("Prec1", "Prec2", "Prec3", "Prec4")

TBI_variables2<- TBI_variables
levels(TBI_variables2$Prec.x) <- c("All","All","All","All" )
levels(TBI_variables2$Temp.x) <- c("All","All","All")


TBI_variables2x<-bind_rows(TBI_variables2, TBI_variables)
TBI_variables2x$Temp.x_f<- factor(TBI_variables2x$Temp.x, levels= c("All","Alp", "Sub", "Bor"))
TBI_variables2x$Prec.x_f<- factor(TBI_variables2x$Prec.x, levels= c("All","Prec1", "Prec2", "Prec3", "Prec4"))

levels(TBI_variables2x$Prec.x) <- c("Prec1", "Prec2", "Prec3", "Prec4")

#add 
TBI_variables2<-TBI_variables%>%
                mutate(allprec.x == "all")
  
#first run TBI_ibutton

#change names of sites for manuscript
modelclimate$Site <- as.factor(modelclimate$Site)
levels(modelclimate$Site) <- c("Sub1", "Bor3", "Bor1", "Alp3", "Sub2", "Alp2", "Bor4", "Sub3", "Alp4", "Alp1", "Sub4", "Bor2")
modelclimate$Site_f = factor(modelclimate$Site, levels= c("Alp1","Alp2","Alp3","Alp4","Sub1","Sub2","Sub3","Sub4","Bor1","Bor2","Bor3", "Bor4"))

# TBI climate comparison
ggplot(modelclimate, aes(Date, color = Data))+
  geom_line(aes(y= Temperature.x, col= "Met.no" ))+
  geom_line(aes(y= Temperature.y, col= "Climatestation"))+
  geom_line(aes(y= mn_sT, col= "Ibuttons"))+
  geom_line(aes(y= new_T, col= "Modelled"))+
  facet_wrap(~Site_f, scales = "free_y")+
  labs(x = "Date", y= "Temperature (°C)")+
  #scale_color_manual(values = c("#33CCFF", "#FF6666", "#33FF99", "#CC00FF"),
  #                  name= "Temperature data")+
  theme_bw()+
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 15), legend.title=element_text(size=14), 
        legend.text=element_text(size=12))

ggplot(AllTemp, aes(Date, color = loggertype))+
  geom_line(aes(y= Temperature.x, col= "Gridded" ))+
  geom_line(aes(y= Temperature.y, col= "Station_sT"))+
  geom_line(aes(y= mn_sT, col= "Ibutton"))+
  geom_line(aes(y= new_T, col= "Zmodel"))+
  facet_wrap(~Site, scales = "free_y")+
  labs(x = "Date", y= "Temperature (°C)")+
  theme_bw()+
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 15), legend.title=element_text(size=14), 
        legend.text=element_text(size=12))

# first run TBI_ibutton, TBI_data and TBI_analysis

# create AllVar for plotting
AllVar <- c("S", "k", "gridTemp", "gridPrec", "year", "Slope", "Aspect", "pH", "NO3N", "NH4N", "Plant_comm", "Root", "SoilN", "SoilC", "soil_CN", "soil_moist", "Bryo", "Gram", "Forbs", "Litter", "Live", "Total", "rich", "div" )

MyVar <- c("S", "k", "gridTemp", "gridPrec", "pH", "NO3N", "NH4N", "Plant_comm", "Root", "soil_CN", "soil_moist", "Bryo", "Gram", "Forbs", "Litter", "Total", "rich", "div" )

# quick plots to look at relations between variables
Mydotplot(TBI_variables[, MyVar]) 
pairs(TBI_variables[, MyVar], lower.panel = panel.cor)


# gridTemp   col= factor(year)), 
ggplot(TBI_variables, aes(modelTemp, k, col= factor(Temp.x)))+
  geom_point(aes(shape = factor(year)), size= 3, width = 0.1)+
  geom_smooth(method = "lm", se = FALSE)+
  #facet_wrap(~site)+
  labs(y= "Decomposition rate (k)", x = "Temperature (°C)")+
  scale_color_discrete(name= "elevation", labels = c("alpine", "subalpine", "boreal"))+
  scale_shape_discrete(solid = FALSE, guide = FALSE)+
  theme_classic()+
   theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.title=element_text(size=14), 
         legend.text=element_text(size=12))


# MANUSCRIPT PLOT1A
ggplot(TBI_variables, aes(modelTemp))+
  geom_smooth(aes(y = k, col = "black"), method = "lm", linetype= "dotdash", size=2.5, se= FALSE)+
  geom_point(aes(y= k, col= factor(Temp.x) , shape = factor(year)), size= 5, width = 0.1)+
  geom_smooth(aes(y= k, col= factor(Temp.x)), method = "lm", size=1.5, se = FALSE)+
  #facet_wrap(~site)+
  labs(y= " Decomposition rate (k) ", x = "Temperature (°C)")+
  scale_color_manual(values =c("#3366FF","#33CC33", "#CC0000", "#000000"),
                       name="Elevation", 
                       breaks=c("1", "2", "3", "4"), 
                       labels = c("ALP", "SUB", "BOR", "ALL"))+
  scale_shape_manual(values = c(1, 6, 8),
                        name="Year", 
                        breaks=c("2014", "2015", "2016"), 
                        labels = c("2014", "2015", "2016")) +
  scale_x_continuous(breaks = c(7,8,9,10,11,12,13))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), axis.text.y = element_text(size = 18), legend.position = c(.1,.85),  legend.title = element_text(size= 20), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))

ggplot(TBI_variables, aes(gridPrec))+
  geom_smooth(aes(y = Ag, col = "black"), method = "lm", linetype= "dotdash", size=2.5, se= FALSE)+
  geom_point(aes(y= Ag, col= factor(year) , shape = factor(Temp.x)), size= 5, width = 0.1)+
  geom_smooth(aes(y= Ag, col= factor(year)), method = "lm", size=1.5, se = FALSE)+
  #facet_wrap(~site)+
  labs(y= " Decomposed rooibos tea (%) ", x = "Temperature (°C)")+
  scale_color_manual(values = c("#3366FF","#33CC33", "#CC0000", "#000000"),
                     name="Year", 
                     breaks=c("2014", "2015", "2016", "Combined"), 
                     labels = c("2014", "2015", "2016", "Combined")) +
  scale_shape_manual(values =c(1,2,3,4),
                     name="Temp.x", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("ALP", "SUB", "BOR", "ALL"))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), axis.text.y = element_text(size = 18), legend.position = c(.1,.8),  legend.title = element_text(size= 20), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))

ggplot(TBI_variables, aes(modelTemp))+
  geom_smooth(aes(y = k, col = "black"), method = "lm", linetype= "dotdash", size=2.5, se= FALSE)+
  geom_point(aes(y= k, col= factor(Prec.x) , shape = factor(year)), size= 5, width = 0.1)+
  geom_smooth(aes(y= k, col= factor(Prec.x)), method = "lm", size=1.5, se = FALSE)+
  #facet_wrap(~site)+
  labs(y= " Decomposition rate (k) ", x = "Temperature (°C)")+
  scale_color_manual(values =c("#3366FF","#33CC33", "#CC0000","#CC9900", "#500000"),
                     name="wetness", 
                     breaks=c("1", "2", "3", "4", "5"), 
                     labels = c("+DRY", "DRY", "WET", "+WET", "ALL"))+
  scale_shape_manual(values = c(1, 5, 8),
                     name="year", 
                     breaks=c("2014", "2015", "2016"), 
                     labels = c("2014", "2015", "2016")) +
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), axis.text.y = element_text(size = 18), legend.position = c(.1,.8),  legend.title = element_text(size= 20), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))

#Edit x-axis
#Relation with Precipitation for 
ggplot(TBI_variables, aes(gridPrec))+
  geom_smooth(aes(y = k, col = "black"), method = "lm", linetype= "dotdash", size=2.5, se= FALSE)+
  geom_point(aes(y= k, col= factor(Prec.x) , shape = factor(year)), size= 5, width = 0.1)+
  geom_smooth(aes(y= k, col= factor(Prec.x)), method = "lm", size=1.5, se = FALSE)+
  #facet_wrap(~site)+
  labs(y= " Decomposition rate (k) ", x = "Precipitation (mm)")+
  scale_color_manual(values =c( "#CC0000","#FF9900", "#3399FF","#000099", "#000000" ),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("+Dry", "Dry", "Wet", "+Wet"))+
  scale_shape_manual(values = c(1, 5, 8),
                     name="year", 
                     breaks=c("2014", "2015", "2016"), 
                     labels = c("2014", "2015", "2016")) +
  scale_x_continuous(breaks = c(100,200,300,400,500,600,700,800,900))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 24), axis.text.x=element_text(size = 18), axis.title = element_text(size = 24), axis.text.y = element_text(size = 18), legend.position = c(.9,.8),  legend.title = element_text(size= 20), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))

# MANUSCRIPT PLOT 1B
ggplot(TBI_variables, aes(gridPrec))+
  geom_smooth(aes(y = k, col = "black"), method = "lm", linetype= "dotdash", size=2.5, se= FALSE)+
  geom_point(aes(y= k, col= factor(Temp.x) , shape = factor(year)), size= 5, width = 0.1)+
  geom_smooth(aes(y= k, col= factor(Temp.x)), method = "lm", size=1.5, se = FALSE)+
  #facet_wrap(~site)+
  labs(y= " Decomposition rate (k) ", x = "Precipitation (mm)")+
  scale_color_manual(values =c("#3366FF","#33CC33", "#CC0000", "#500000" ),
                     name="Elevation", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("ALP", "SUB", "BOR", "ALL"))+
  scale_shape_manual(values = c(1, 6, 8),
                     name="Year", 
                     breaks=c("2014", "2015", "2016"), 
                     labels = c("2014", "2015", "2016")) +
  scale_x_continuous(breaks = c(100,200,300,400,500,600,700,800,900))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 24), axis.text.x=element_text(size = 18), axis.title = element_text(size = 24), axis.text.y = element_text(size = 18), legend.position = "none", strip.text.x = element_text(size = 18))


#Decomposition rate (k)

ggplot(TBI_variables, aes(modelTemp))+
  geom_point(aes( y= k))+
  geom_point(aes(y= k, col = factor(year)), size= 5, width = 0.1)+
  geom_smooth(aes(y= k, col= factor(year)), method = "lm", se = FALSE)+
    #facet_wrap(~site)+
  labs(y= "Decomposition rate (k)", x = "Temperature (°C)")+
  scale_color_discrete(name= "year", labels = c("2014", "2015", "2016"))+
  scale_shape_discrete(solid = FALSE, guide = FALSE)+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), axis.text.y = element_text(size = 18), legend.position = c(.1,.9), legend.title = element_text(size= 20), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))



# gridPrec
gglot(TBI_variables, aes(gridPrec))+
  geom_point(aes( y= k))+
  geom_smooth(aes(y = k, col = "yellow"), method = "lm", size= 2, se= FALSE)+
  geom_point(aes(y= k, col= factor(Prec.x) ,shape = factor(year)), size= 5, width = 0.1)+
  geom_smooth(aes(y= k, col= factor(Prec.x)) ,method = "lm", se = FALSE, size = 1)+
  #facet_wrap(~site)+
  labs(y= "Decomposition rate (k)", x = "Temperature (°C)")+
  scale_color_discrete(name= "elevation", labels = c("+dry", "dry", "wet", "+wet","total"))+
  scale_shape_discrete(solid = FALSE, guide = FALSE)+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), axis.text.y = element_text(size = 18), legend.position = c(.1,.9), legend.title = element_text(size= 20), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))

gglot(TBI_variables, aes(gridPrec))+
  geom_point(aes( y= k))+
  geom_smooth(aes(y = k, col = "yellow"), method = "lm", size= 2, se= FALSE)+
  geom_point(aes(y= k, col= factor(Temp.x) ,shape = factor(year)), size= 5, width = 0.1)+
  geom_smooth(aes(y= k, col= factor(Temp.x)) ,method = "lm", se = FALSE, size = 1)+
  #facet_wrap(~site)+
  labs(y= "Decomposition rate (k)", x = "Temperature (°C)")+
  scale_color_discrete(name= "elevation", labels = c("ALP", "SUB", "BOR", "ALL"))+
  scale_shape_discrete(solid = FALSE, guide = FALSE)+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), axis.text.y = element_text(size = 18), legend.position = c(.1,.9), legend.title = element_text(size= 20), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))


ggplot(TBI_variables, aes(gridPrec, k))+
  geom_point(aes(shape = factor(year)), size= 3, na.rm = FALSE)+
  geom_smooth(method = "lm", se = FALSE)+
  facet_wrap(~site)+
  labs(y= "Decomposition rate (k)", x = "Precipitation (mm)")
  scale_shape_discrete(name= "elevation", labels = c("+dry", "dry", "wet", "+wet"), solid = FALSE, guide= FALSE)+
  theme_bw()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.title=element_text(size=14),         legend.text=element_text(size=12))

ggplot(TBI_variables, aes(gridPrec, modelTemp, col = factor(Temp.x)))+
  geom_point()+

  geom_smooth( method = "lm")
  #scale_y_continuous( limits = c(0,70))
ggplot(TBI_variables, aes(gridPrec, k, col = Temp.x))+
  geom_point(aes(shape = factor(year)), size= 3)+

  geom_smooth(method = "lm")+
  #facet_grid(year~.)+
  la
bs(y= "Decomposition rate (k)", x = "Precipitation (mm)")+
  scale_color_discrete(name= "precipitation", labels = c("alp", "sub", "bor"))+
popo  
  theme_bw()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.title=element_text(size=14), 
        legend.text=element_text(size=12))


ggplot(TBI_variables, aes(gridPrec, k))+
  geom_point(aes(shape = factor(Prec.x)), size= 3)+
  geom_smooth(method = "lm")+
  #facet_grid(~year)+
  labs(y= "Decomposition rate (k)", x = "Precipitation (mm)")+
  scale_color_discrete(name= "precipitation", labels = c("+dry", "dry", "wet", "+wet"))+
  scale_shape_discrete(solid = FALSE, guide= FALSE)+
  theme_bw()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.title=element_text(size=14), 
        legend.text=element_text(size=12))
  

# boxplots with comparing Decomp rate K, modelT and gridP at different elevations for different years
#Boxplot for manuscript decomp K against elevation split with Precipition level
ggplot(TBI_variables, aes(Prec.x, k, fill = factor(Prec.x)))+
  geom_boxplot()+
  theme_classic()+
  facet_grid(.~Temp.x, switch= "both")+
  scale_fill_grey(start =0.4, end = 1.0, name= "Precipitation level", labels = c("+dry", "dry", "wet", "+wet"))+
  labs(x= "year", y = "Decomposition rate (k)")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title = element_text(size = 25), axis.text.y = element_text(size = 18), legend.position = c(0.8,0.8), legend.title = element_text(size= 20), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))

#Supplementary data
#Boxplot for manuscript decomp K against year split with Precipition level
ggplot(TBI_variables, aes(factor(year), k, fill = factor(year)))+
  stat_boxplot(geom ='errorbar', width = 0.4)+
  geom_boxplot()+
  theme_classic()+
  facet_grid(.~Prec.x, switch= "both")+
  scale_fill_grey(start =0.4, end = 1.0, name= "Year", labels = c("2014", "2015", "2016"))+
  labs(x= "year", y = "Decomposition rate (k)")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title = element_text(size = 25),axis.text.y = element_text(size = 18), legend.position = c(0.9,0.9), legend.title = element_text(size= 22), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))

ggplot(TBI_variables2x, aes(year, k, fill = factor(year)))+
  stat_boxplot(geom ='errorbar', width = 0.4)+
  geom_boxplot()+
  theme_classic()+
  facet_grid(.~Temp.x_f, switch= "both")+
  scale_fill_grey(start =0.4, end = 1.0, name= "year", labels = c("2014", "2015", "2016"))+
  labs(x= "Year", y = "Decomposition rate (k)")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title = element_text(size = 25),axis.text.y = element_text(size = 18), strip.text.x = element_text(size = 18), legend.position = "none" )

ggplot(TBI_variables, aes(factor(year), k, fill = factor(year)))+
  stat_boxplot(geom ='errorbar', width = 0.4)+
  geom_boxplot()+
  theme_classic()+
    scale_fill_grey(start =0.4, end = 1.0, guide=FALSE)+
  labs(x= "Year", y = "Decomposition rate (k)")+
  theme(axis.title = element_text(size = 25),axis.ticks.x=element_blank(), axis.text = element_text(size = 18))

ggplot(TBI_variables, aes(Temp.x, gridPrec, fill = factor(Temp.x)))+
  stat_boxplot(geom ='errorbar',width = 0.4)+
  geom_boxplot()+
  theme_classic()+
  facet_grid(.~year, switch= "both")+
  scale_fill_grey(start =0.4, end = 1.0, name= "elevation", labels = c("alpine", "sub-alpine", "boreal"))+
  labs(x= "year", y = "Precipitation (mm)")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title = element_text(size = 25),axis.text.y = element_text(size = 18), legend.position = c(.1,.8), legend.title = element_text(size= 20), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))

#Prec
ggplot(TBI_variables, aes(Prec.x, k, fill = factor(Prec.x)))+
  geom_boxplot()+
  theme_classic()+
  facet_grid(.~year, switch= "both")+
  scale_fill_grey(start =0.4, end = 1.0, name= "elevation", labels = c("+dry", "dry", "wet", "+wet"))+
  labs(x= "year", y = "decomposition rate (k)")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title = element_text(size = 25),axis.text.y = element_text(size = 18), legend.position = c(.1,.8), legend.title = element_text(size= 20), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))

ggplot(TBI_variables, aes(Prec.x, modelTemp, fill = factor(Prec.x)))+
  geom_boxplot()+
  theme_classic()+
  facet_grid(.~year, switch= "both")+
  scale_fill_grey(start =0.4, end = 1.0, name= "elevation", labels = c("+dry", "dry", "wet", "+wet"))+
  labs(x= "year", y = "Temperature (C)")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title = element_text(size = 25),axis.text.y = element_text(size = 18), legend.position = c(.1,.1), legend.title = element_text(size= 20), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))

ggplot(TBI_variables, aes(Prec.x, gridPrec, fill = factor(Prec.x)))+
  geom_boxplot()+
  theme_classic()+
  facet_grid(.~year, switch= "both")+
  scale_fill_grey(start =0.4, end = 1.0, name= "elevation", labels = c("+dry", "dry", "wet", "+wet"))+
  labs(x= "year", y = "Precipitation (mm)")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title = element_text(size = 25),axis.text.y = element_text(size = 18), legend.position = c(.1,.8), legend.title = element_text(size= 20), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))

# LOss of mass for different tea
ggplot(TBI_variables, aes(Temp.x, fill = factor(Temp.x)))+
  geom_boxplot(aes(y= decomp.R*100, col = "rooibos"))+
  geom_boxplot(aes(y= Ag*100, col = "green" ))+
  theme_classic()+
  facet_grid(.~year, switch= "both")+
  scale_fill_grey(start =0.4, end = 1.0, name= "elevation", labels = c("alpine", "sub-alpine", "boreal"))+
  scale_color_manual(values =c("#339900","#CC0000"),
                     name="tea", 
                     breaks=c("green", "rooibos"), 
                     labels = c("green", "rooibos"))+
  labs(x= "year", y = "Decomposed tea (%)")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title = element_text(size = 25), axis.text.y = element_text(size = 18), legend.position = c(.9,.45), legend.title = element_text(size= 20), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))


ggplot(TBI_variables2x, aes(factor(year), k, fill = factor(year)))+
  #stat_boxplot(geom ='errorbar', width = 0.4)+
  geom_boxplot(aes(y= decomp.R*100, col = "rooibos"))+
  geom_boxplot(aes(y= Ag*100, col = "green" ))+
  theme_classic()+
  facet_grid(.~Prec.x, switch= "both")+
  scale_fill_grey(start =0.4, end = 1.0, name= "Year", labels = c("2014", "2015", "2016"))+
  scale_color_manual(values =c("#339900","#CC0000"),
                     name="tea", 
                     breaks=c("green", "rooibos"), 
                     labels = c("green", "rooibos"))+
  labs(x= "year", y = "Decomposed tea (%)")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title = element_text(size = 25), axis.text.y = element_text(size = 18), legend.position = c(.9,.45), legend.title = element_text(size= 20), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))


ggplot(TBI_variables, aes(Prec.x, fill = factor(Prec.x)))+
  geom_boxplot(aes(y= decomp.R*100, col = "rooibos"))+
  geom_boxplot(aes(y= Ag*100, col = "green" ))+
  theme_classic()+
  facet_grid(.~year, switch= "both")+
  scale_fill_grey(start =0.4, end = 1.0, name= "Prec level", labels = c("+Dry", "Dry", "Wet", "+Wet"))+
  scale_color_manual(values =c("#339900","#CC0000"),
                     name="tea", 
                     breaks=c("green", "rooibos"), 
                     labels = c("green", "rooibos"), guide=FALSE)+
  labs(x= "year", y = "Decomposed tea (%)")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title = element_text(size = 25), axis.text.y = element_text(size = 18), legend.position = c(.9,.45), legend.title = element_text(size= 20), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))

# possible idea
ggplot(TBI_variables, aes(Ag, decomp.R, col=factor(Prec.x)))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  facet_wrap(~year)

# possible idea
ggplot(TBI_means, aes(k, S, col=factor(year)))+
  geom_point()
  

ggplot(TBI_2016, aes(Prec.x, fill = factor(Prec.x)))+
  geom_boxplot(aes(y= decomp.R*100, col = "rooibos"))+
  geom_boxplot(aes(y= Ag*100, col = "green" ))+
  theme_classic()+
  facet_grid(.~Temp.x, switch= "both")+
  scale_fill_grey(start =0.4, end = 1.0, name= "elevation", labels = c("Prec1", "Prec2", "Prec3", "Prec4"))+
  scale_color_manual(values =c("#339900","#CC0000"),
                     name="tea", 
                     breaks=c("green", "rooibos"), 
                     labels = c("green", "rooibos"))+
  labs(x= "year", y = "Decomposed tea (%)")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title = element_text(size = 25), axis.text.y = element_text(size = 18), legend.position = c(.9,.45), legend.title = element_text(size= 20), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))

ggplot(TBI_variables, aes(Prec.x, fill = factor(Prec.x)))+
  geom_boxplot(aes(y= decomp.R*100, col = "rooibos"))+
  geom_boxplot(aes(y= Ag*100, col = "green" ))+
  theme_classic()+
  facet_grid(.~Temp.x, switch= "both")+
  scale_fill_grey(start =0.4, end = 1.0, name= "elevation", labels =c("+dry", "dry", "wet", "+wet"))+
  scale_color_manual(values =c("#339900","#CC0000"),
                     name="tea", 
                     breaks=c("green", "rooibos"), 
                     labels = c("green", "rooibos"))+
  labs(x= "year", y = "Decomposed tea (%)")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title = element_text(size = 25), axis.text.y = element_text(size = 18), legend.position = c(.9,.45), legend.title = element_text(size= 20), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))

ggplot(TBI_variables, aes(modelTemp))+
  geom_point(aes(y= decomp.R*100, size = 5, col= "Rooibos"))+
  geom_point(aes(y= Ag*100,  size= 5, col= "Green" ))+
  geom_smooth(aes( y =decomp.R*100 ), method = "lm")+
  geom_smooth(aes( y =Ag*100), method = "lm")+
  theme_classic()+
  facet_grid(.~factor(year))+
  labs(x= "Temperature", y = "Decomposed tea (%)")+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), axis.text.y = element_text(size = 18), legend.position = c(.9,.5), legend.title = element_text(size= 20), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))

########Figure for manuscript 2AB
ggplot(TBI_variables, aes(modelTemp))+
  geom_point(aes(y= decomp.R*100, col= "Rooibos", shape= "Rooibos"), size=4)+ 
  geom_point(aes(y= Ag*100, col="Green", shape= "Green"), size=4)+ 
  geom_smooth(aes( y =decomp.R*100 ,  linetype= "Rooibos"), col="#000000", method = "lm", se=FALSE)+
  geom_smooth(aes( y =Ag*100,  linetype = "Green"), col="#000000", method = "lm", se=FALSE)+
  facet_grid(~Temp.x)+
  theme_classic()+
  labs(y= " Decomposed tea (%) ", x = "Temperature (°C)")+
  scale_color_manual(values =c("#339900","#CC0000"), 
                     name="Tea", 
                     breaks=c("Rooibos", "Green"), 
                     labels = c("Rooibos", "Green")) +
  scale_linetype_manual(values = c("solid", "dotdash"), 
                        name="Tea", 
                        breaks=c("Rooibos", "Green"), 
                        labels = c("Rooibos", "Green"))+
  scale_shape_manual(values = c(21, 21), 
                     name="Tea", 
                     breaks=c("Rooibos", "Green"), 
                     labels = c("Rooibos", "Green")) +
  guides(colour = guide_legend(override.aes = list(size=4)))+
theme(axis.title.x=element_text(size = 24), axis.text.x=element_text(size = 20), axis.title = element_text(size = 24), axis.text.y = element_text(size = 18), legend.position = c(.1,.9), legend.title = element_text(size= 22), legend.text = element_text(size= 22), strip.text.x = element_text(size = 18))

ggplot(TBI_variables, aes(gridPrec))+
  geom_point(aes(y= decomp.R*100, col= "Rooibos", shape= "Rooibos"), size=4)+ 
  geom_point(aes(y= Ag*100, col="Green", shape= "Green"), size=4)+ 
  geom_smooth(aes( y =decomp.R*100,  linetype= "Rooibos"), col="#000000", method = "lm", se=FALSE)+
  geom_smooth(aes( y =Ag*100,  linetype = "Green"), col="#000000", method = "lm", se=FALSE)+
  facet_wrap(~Temp.x)+
  labs(y= " Decomposed tea (%) ", x = "Precipitation (mm)")+
  scale_color_manual(values =c("#339900","#CC0000"), 
                     name="Tea", 
                     breaks=c("Rooibos", "Green"), 
                     labels = c("Rooibos", "Green")) +
  scale_shape_manual(values = c(21, 21), 
                     name="Tea", 
                     breaks=c("Rooibos", "Green"), 
                     labels = c("Rooibos", "Green")) +
  scale_linetype_manual(values = c("solid", "dotdash"), 
                        name="Tea", 
                        breaks=c("Rooibos", "Green"), 
                        labels = c("Rooibos", "Green"))+
  scale_x_continuous(breaks = c(300,500,700,900))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), axis.text.y = element_text(size = 18), legend.position = "none", strip.background = element_blank(), strip.text.x = element_blank())


ggplot(TBI_variables, aes(modelTemp, col=factor(Prec.x)))+
  geom_point(aes(y= decomp.R*100, shape= "Rooibos"), size=3)+
  geom_point(aes(y= Ag*100, shape= "Green"), size=3)+
  geom_smooth(aes( y =decomp.R*100 ,  linetype= "Rooibos"), method = "lm", se=FALSE)+
  geom_smooth(aes( y =Ag*100,  linetype = "Green"), method = "lm", se=FALSE)+
  facet_grid(~year)+
  theme_classic()+
  labs(y= " Decomposed tea (%) ", x = "Precipitation (mm)")+
  scale_shape_manual(values = c(1, 19), 
                     name="Tea", 
                     breaks=c("Rooibos", "Green"), 
                     labels = c("Rooibos", "Green")) +
  scale_linetype_manual(values = c("solid", "dotdash"), 
                        name="Tea", 
                        breaks=c("Rooibos", "Green"), 
                        labels = c("Rooibos", "Green"))+
  theme(axis.title.x=element_text(size = 24), axis.text.x=element_text(size = 18), axis.title = element_text(size = 24), axis.text.y = element_text(size = 18), legend.position = c(.1,.9),  legend.title = element_text(size= 20), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))

ggplot(TBI_variables, aes(modelTemp, col = factor(Prec.x)))+
  geom_point(aes(y= decomp.R*100, col =factor(Prec.x), size = 5))+
  geom_point(aes(y= Ag*100, col= factor(Prec.x), size= 5 ))+
  geom_smooth(aes( y =decomp.R*100, col =factor(Prec.x)), method = "lm", se=FALSE)+
  geom_smooth(aes( y =Ag*100, col =factor (Prec.x)), method = "lm", se=FALSE)+
  theme_classic()+
  facet_grid(.~Temp.x)+
  labs(x= "Temperature", y = "Decomposed tea (%)")+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), axis.text.y = element_text(size = 18), legend.position = c(.9,.5), legend.title = element_text(size= 20), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))

ggplot(TBI_variables, aes(Prec.x, fill = factor(Prec.x)))+
  geom_boxplot(aes(y= decomp.R*100, col = "rooibos"))+
  geom_boxplot(aes(y= Ag*100, col = "green" ))+
  theme_classic()+
  facet_grid(.~year, switch= "both")+
  scale_fill_grey(start =0.4, end = 1.0, name= "elevation", labels =c("+dry", "dry", "wet", "+wet"))+
  scale_color_manual(values =c("#339900","#CC0000"),
                     name="tea", 
                     breaks=c("green", "rooibos"), 
                     labels = c("green", "rooibos"))+
  labs(x= "year", y = "Decomposed tea (%)")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title = element_text(size = 25), axis.text.y = element_text(size = 18), legend.position = c(.9,.45), legend.title = element_text(size= 20), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))

ggplot(TBI_variables, aes(gridPrec))+
  geom_point(aes(y= decomp.R*100, shape= factor(year), size = 5, col= "Rooibos"))+
  geom_point(aes(y= Ag*100, shape= factor(year),  size= 5, col= "Green" ))+
  geom_smooth(aes( y =decomp.R*100 ), method = "lm")+
  geom_smooth(aes( y =Ag*100), method = "lm")+
  scale_shape_manual(values = c(1, 5, 8),
                     name="year", 
                     breaks=c("2014", "2015", "2016"), 
                     labels = c("2014", "2015", "2016")) +
  scale_color_manual(values =c("#339900","#CC0000"),
                     name="tea", 
                     breaks=c("green", "rooibos"), 
                     labels = c("green", "rooibos"))+
  theme_classic()+
  facet_grid(.~year)+
  labs(x= "Temperature (°C)", y = "Decomposed tea (%)")+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), axis.text.y = element_text(size = 18), legend.position = c(.9,.5), legend.title = element_text(size= 20), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))


ggplot(TBI_variables, aes(gridPrec, group = factor(Prec.x)))+
  geom_point(aes(y= decomp.R*100, col =factor(Prec.x), size = 5))+
  geom_point(aes(y= Ag*100, col= factor(Prec.x), size= 5 ))+
  geom_smooth(aes( y =decomp.R*100, col =factor(Prec.x)), method = "lm")+
  geom_smooth(aes( y =Ag*100, col =factor(Prec.x)), method = "lm")+
  theme_classic()+
  #facet_grid(.~year)+
  labs(x= "Precipitation", y = "Decomposed tea (%)")+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 20), axis.title = element_text(size = 25), axis.text.y = element_text(size = 18), legend.position = c(.9,.5), legend.title = element_text(size= 20), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))



#####RUN TBI_ibutton.R first
### Climate data plots for TBI 
ggplot(modelclimate, aes(Date, color = loggertype))+
  geom_line(aes(y= Temperature.x, col= "Gridded" ))+
  geom_line(aes(y= Temperature.y, col= "Station_sT"))+
  geom_line(aes(y= mn_sT, col= "Ibutton"))+
  geom_line(aes(y= new_T, col= "Zmodel"))+
  facet_wrap(~Site, scales = "free_y")+
  labs(x = "Date 2015", y= "Temperature (C)")+
  theme_bw()


# april 30 yday 120 nov 1 yday 305, method is gam with formula = y ~ s(x, bs = "cs")
ggplot(modelclimate, aes(yday, new_T, col = factor(Year)))+
  geom_smooth(method = "auto", se = TRUE)+
  xlim(120,305)+
  scale_color_grey(start= 0.1, end = 0.7, name= "Year", labels = c("2014", "2015", "2016"))+
  labs(x = "Julian day ", y= "Temperature (°C)")+
  theme_classic()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.title=element_text(size=14), 
        legend.text=element_text(size=12))



# compare Temperature data of Gridded, Station and Ibutton 2014
ggplot(TBI_temperature, aes(Date, color= loggertype))+
  geom_line(aes(y= Temperature.x, col= "Gridded" ))+
  geom_line(aes(y= mn_sT, col= "Ibutton_sT"))+
  geom_line(aes(y= Temperature.y, col= "Station_sT"))+
  scale_linetype_discrete(name= "loggertype", labels = c("Ibutton", "Gridded", "Station"))+
  facet_wrap(~Site)+
  labs(x = "Date 2014", y= "Temperature (C)")+
  theme_bw()

# compare Temperature data of Gridded Station 2015
ggplot(TBI_temp2015, aes(Date, color = loggertype))+
  geom_line(aes(y= Temperature.x, col= "Gridded" ))+
  geom_line(aes(y= Temperature.y, col= "Station_sT"))+
  facet_wrap(~Site)+
  labs(x = "Date 2015", y= "Temperature (C)")+
  theme_bw()


#plot Gridded climate for the different years of TBI
ggplot(Gridded.Temp, aes(yday, Temperature, color = year))+
  geom_smooth()+
  theme_bw()

# Barplot Precipitation per site for all years 
ggplot(Total.Prec, aes(site, gridPrec, fill = factor(year)))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_grey(start= 0.3, end = 0.9, name= "Year", labels = c("2014", "2015", "2016"))+
  theme_classic()
#labels= c("Fau", "Alr", "Ulv", "Vik", "Hog", "Lav", "Arh", "Ram", "Gud", "Ovs", "Ves", "Skj"))+


# differences between elevations
ggplot(TBI_variables, aes(pH,k, color=Temp.x))+
  geom_point()+
  theme_classic()+
  labs(x= "Elevation")+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15))


