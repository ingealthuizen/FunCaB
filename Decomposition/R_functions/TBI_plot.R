# TBI plots 

#first run TBI_ibutton
# TBI climate comparison
ggplot(modelclimate, aes(Date, color = Data))+
  geom_line(aes(y= Temperature.x, col= "Met.no" ))+
  geom_line(aes(y= Temperature.y, col= "Climatestation"))+
  geom_line(aes(y= mn_sT, col= "Ibuttons"))+
  geom_line(aes(y= new_T, col= "Modelled"))+
  facet_wrap(~Site, scales = "free_y")+
  labs(x = "Date", y= "Temperature (°C)")+
  theme_bw()+
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 15), legend.title=element_text(size=14), 
        legend.text=element_text(size=12))

ggplot(AllTemp, aes(Date, color = loggertype))+
  geom_line(aes(y= Temperature.x, col= "Gridded" ))+
  geom_line(aes(y= Temperature.y, col= "Station_sT"))+
  geom_line(aes(y= mn_sT, col= "Ibutton"))+
  geom_line(aes(y= new_T, col= "Zmodel"))+
  facet_wrap(~Site, scales = "free_y")+
  labs(x = "Dates", y= "Temperature (°C)")+
  theme_bw()+
  
  
# first run TBI_ibutton, TBI_data and TBI_analysis

# create AllVar for plotting
AllVar <- c("S", "k", "gridTemp", "gridPrec", "year", "Slope", "Aspect", "pH", "NO3N", "NH4N", "Plant_comm", "Root", "SoilN", "SoilC", "soil_CN", "soil_moist", "Bryo", "Gram", "Forbs", "Litter", "Live", "Total", "rich", "div" )

MyVar <- c("S", "k", "gridTemp", "gridPrec", "pH", "NO3N", "NH4N", "Plant_comm", "Root", "soil_CN", "soil_moist", "Bryo", "Gram", "Forbs", "Litter", "Total", "rich", "div" )

# quick plots to look at relations between variables
Mydotplot(TBI_variables[, MyVar]) 
pairs(TBI_variables[, MyVar], lower.panel = panel.cor)

# gridTemp   col= factor(year)), 
ggplot(TBI_variables1516, aes(modelTemp, k, col= factor(Temp.x)))+
  geom_jitter(aes(shape = factor(year)), size= 3, width = 0.1)+
  geom_smooth(method = "lm", se = FALSE)+
  #facet_wrap(~site)+
  labs(y= "Decomposition rate (k)", x = "Temperature (°C)")+
  scale_color_discrete(name= "elevation", labels = c("alpine", "subalpine", "boreal"))+
  scale_shape_discrete(solid = FALSE, guide = FALSE)+
  theme_classic()+
   theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.title=element_text(size=14), 
         legend.text=element_text(size=12))


# gridPrec
ggplot(TBI_variables1516, aes(gridPrec, k, col= factor(Prec.x)))+
  geom_point(aes(shape = factor(year)), size= 3, na.rm = FALSE)+
  geom_smooth(method = "lm", se = FALSE)+
  #facet_grid(~year)+
  labs(y= "Decomposition rate (k)", x = "Total precipitation (mm)")+
  scale_color_discrete(name= "Precipitation level", labels = c("+dry", "dry", "wet", "+wet"))+
  scale_shape_discrete(solid = FALSE, guide= FALSE)+
  theme_classic()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.title=element_text(size=14), 
        legend.text=element_text(size=12))

ggplot(TBI_variables, aes(gridPrec, k))+
  geom_point(aes(shape = factor(year)), size= 3, na.rm = FALSE)+
  geom_smooth(method = "lm", se = FALSE)+
  facet_wrap(~site)+
  labs(y= "Decomposition rate (k)", x = "Precipitation (mm)")
  scale_shape_discrete(name= "elevation", labels = c("alp", "sub", "bor"), solid = FALSE, guide= FALSE)+
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

# RainFactor (RF)
ggplot(TBI_variables, aes(RF, k, col= Prec.x))+
  geom_point(aes(shape = factor(Temp.x)), size= 3)+
  geom_smooth(method = "lm")+
  #facet_grid(~year)+
  labs(y= "Decomposition rate (k)", x = "RF")+
  scale_color_discrete(name= "precipitation", labels = c("alp", "sub", "bor"))+
  scale_shape_discrete(solid = FALSE, guide= FALSE)+
  theme_bw()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.title=element_text(size=14), 
        legend.text=element_text(size=12))


# difference in K across years
ggplot(TBI_variables, aes(factor(year), k))+
  geom_boxplot()+
  theme_classic()+
  labs(x= "Year", y = "Decomposition rate (k)")+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15))


ggplot(TBI_variables, aes(factor(year), k, col= Temp.x))+
  geom_boxplot()+
  #facet_wrap(~site)+
  labs(y= "Decomposition rate (k)", x = "year")+
  scale_color_discrete(name= "Elevation", labels = c("alp", "sub", "bor"))+
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
  

ggplot(TBI_variables, aes(site,))

ggplot(TBI_variables, aes(modelTemp, Ag, col=Temp.x))+
  geom_point()+
  geom_smooth( method = "lm", se = FALSE)+
  theme_classic()

ggplot(TBI_variables, aes(modelTemp, decomp.R, col=Temp.x))+
  geom_point()+
  geom_smooth( method = "lm", se= FALSE)+
  theme_classic()


ggplot(TBI_variables, aes(site, Litter.CN))+
  geom_point()


ggplot(modelclimate, aes(Date, color = loggertype))+
  geom_line(aes(y= Temperature.x, col= "Gridded" ))+
  geom_line(aes(y= Temperature.y, col= "Station_sT"))+
  geom_line(aes(y= mn_sT, col= "Ibutton"))+
  geom_line(aes(y= new_T, col= "Zmodel"))+
  facet_wrap(~Site, scales = "free_y")+
  labs(x = "Date 2015", y= "Temperature (C)")+
  theme_bw()


# april 30 yday 120 nov 1 yday 305, method is gam with formula = y ~ s(x, bs = "cs")
ggplot(modelclimate, aes(yday, new_T, color = factor(Year)))+
  geom_smooth(method = "auto")+
  #xlim(120,305)+
  scale_x_continuous(limits = c(120, 305))+
  scale_color_discrete(name= "Year", labels = c("2014", "2015", "2016"))+
  labs(x = "Julian day ", y= "Temperature (°C)")+
  theme_classic()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.title=element_text(size=14), 
        legend.text=element_text(size=12))


ggplot(Gridded.Temp, aes(yday, Temperature, color = year))+
  geom_smooth()+
  theme_bw()





#ggplot(TBI, aes(Site, k))+
#  geom_boxplot()+
#  theme_classic()+
#  facet_grid(year~.)+
#  labs(x= "Site", y = "Decomposition rate (k)")+
#  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15))



### Climate data plots for TBI 

#RUN TBI_ibutton.R first
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

ggplot(Gridded.Prec, aes(yday, Precipitation, color = year))+
  geom_smooth()+
  theme_bw()



  
