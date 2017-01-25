# TBI plots 

# create AllVar for plotting
AllVar <- c("S", "k", "gridTemp", "gridPrec", "year", "Slope", "Aspect", "pH", "NO3N", "NH4N", "Plant_comm", "Root", "SoilN", "SoilC", "soil_CN", "soil_moist", "Bryo", "Gram", "Forbs", "Litter", "Live", "Total", "rich", "div" )

MyVar <- c("S", "k", "gridTemp", "gridPrec", "pH", "NO3N", "NH4N", "Plant_comm", "Root", "soil_CN", "soil_moist", "Bryo", "Gram", "Forbs", "Litter", "Total", "rich", "div" )

# quick plots to look at relations between variables
Mydotplot(TBI_variables[, MyVar]) 
pairs(TBI_variables[, MyVar], lower.panel = panel.cor)


ggplot(TBI, aes(k, gridTemp, col= Temp))+
  geom_point(aes(shape = factor(Temp)))+
    scale_shape(solid = FALSE)+
  geom_smooth(method = "lm")+
  labs(x= "Decomposition rate (k)", y = "Temperature (°C)")+
  theme_bw()

ggplot(TBI, aes(k, gridPrec, col= Prec))+
  geom_point(aes(shape = factor(Prec)))+
  scale_shape(solid = FALSE)+
  geom_smooth(method = "lm")+
  labs(x= "Decomposition rate (k)", y = "Temperature (°C)")+
  theme_bw()



ggplot(TBI, aes(year, k))+
  geom_boxplot()+
  
  theme_classic()
  
  