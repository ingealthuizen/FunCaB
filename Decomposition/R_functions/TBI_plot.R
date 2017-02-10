# TBI plots 

# create AllVar for plotting
AllVar <- c("S", "k", "gridTemp", "gridPrec", "year", "Slope", "Aspect", "pH", "NO3N", "NH4N", "Plant_comm", "Root", "SoilN", "SoilC", "soil_CN", "soil_moist", "Bryo", "Gram", "Forbs", "Litter", "Live", "Total", "rich", "div" )

MyVar <- c("S", "k", "gridTemp", "gridPrec", "pH", "NO3N", "NH4N", "Plant_comm", "Root", "soil_CN", "soil_moist", "Bryo", "Gram", "Forbs", "Litter", "Total", "rich", "div" )

# quick plots to look at relations between variables
Mydotplot(TBI_variables[, MyVar]) 
pairs(TBI_variables[, MyVar], lower.panel = panel.cor)

# gridTemp   
ggplot(TBI_variables, aes(modelTemp, k, col= Temp.x ))+
  geom_point(aes(shape = factor(year)), size= 3, na.rm = FALSE)+
  geom_smooth(method = "lm")+
  facet_grid(.~site)+
  labs(y= "Decomposition rate (k)", k = "Temperature (°C)")+
  scale_color_discrete(name= "elevation", labels = c("alp", "sub", "bor"))+
  scale_shape_discrete(solid = FALSE, guide= FALSE)+
  theme_bw()+
   theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.title=element_text(size=14), 
         legend.text=element_text(size=12))

ggplot(TBI_variables, aes(gridPrec, k))+
  geom_point(aes(shape = factor(year)), size= 3, na.rm = FALSE)+
  geom_smooth(method = "lm")+
  facet_wrap(~site)+
  labs(y= "Decomposition rate (k)", k = "Precipitation (mm)")+
  scale_shape_discrete(name= "elevation", labels = c("alp", "sub", "bor"), solid = FALSE, guide= FALSE)+
  theme_bw()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.title=element_text(size=14), 
        legend.text=element_text(size=12))
 
# gridPrec
ggplot(TBI_variables, aes(gridPrec, k, col= Prec.x))+
  geom_point(aes(shape = factor(year)), size= 3, na.rm = FALSE)+
  geom_smooth(method = "lm")+
  #facet_grid(~year)+
  labs(y= "Decomposition rate (k)", k = "Total precipitatoin (mm)")+
  scale_color_discrete(name= "elevation", labels = c("+dry", "dry", "wet", "+wet"))+
  scale_shape_discrete(solid = FALSE, guide= FALSE)+
  theme_bw()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.title=element_text(size=14), 
        legend.text=element_text(size=12))


ggplot(TBI_variables, aes(gridPrec, k, col = Temp.x))+
  geom_point(aes(shape = factor(year)), size= 3)+
  geom_smooth(method = "lm")+
  #facet_grid(year~.)+
  labs(y= "Decomposition rate (k)", x = "Precipitation (mm)")+
  scale_color_discrete(name= "precipitation", labels = c("alp", "sub", "bor"))+
  scale_shape_discrete(solid = FALSE, guide= FALSE)+
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


ggplot(TBI_variables, aes(factor(year), k))+
  geom_boxplot()+
  facet_wrap(~site)+
  labs(y= "Decomposition rate (k)", x = "year")+
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
  scale_x_continuous(limits = c(105, 305))+
  labs(x = "Julian day ", y= "Temperature (°C)")+
  theme_classic()


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



  
