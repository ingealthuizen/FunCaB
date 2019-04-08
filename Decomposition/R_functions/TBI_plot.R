library(ggthemes)
# TBI plots 

#change names of Temperature and precipitation levels
levels(TBI_variables$Temp.x) <- c("ALP", "SUB", "BOR")
levels(TBI_variables$Prec.x) <- c("Prec1", "Prec2", "Prec3", "Prec4")

TBI_variables2<- TBI_variables
levels(TBI_variables2$Prec.x) <- c("All","All","All","All" )
levels(TBI_variables2$Temp.x) <- c("All","All","All")


TBI_variables2x<-bind_rows(TBI_variables2, TBI_variables)
TBI_variables2x$Temp.x_f<- factor(TBI_variables2x$Temp.x, levels= c("All","ALP", "SUB", "BOR"))
TBI_variables2x$Prec.x_f<- factor(TBI_variables2x$Prec.x, levels= c("All","Prec1", "Prec2", "Prec3", "Prec4"))

levels(TBI_variables2x$Prec.x) <- c("Prec1", "Prec2", "Prec3", "Prec4")

#add 
TBI_variables2<-TBI_variables%>%
                mutate(allprec.x == "All")
  
#first run TBI_ibutton


#change names of sites for manuscript
modelclimate$Site <- as.factor(modelclimate$Site)
levels(modelclimate$Site) <- c("SUB1", "BOR3", "BOR1", "ALP3", "SUB2", "ALP2", "BOR4", "SUB3", "ALP4", "ALP1", "SUB4", "BOR2")
modelclimate$Site_f = factor(modelclimate$Site, levels= c("ALP1","ALP2","ALP3","ALP4","SUB1","SUB2","SUB3","SUB4","BOR1","BOR2","BOR3", "BOR4"))

####### TBI climate comparison
ggplot(modelclimate, aes(Date, color = Datasource))+
  geom_line(aes(y= Temperature.x, col= "NMI" ))+
  geom_line(aes(y= new_T, col= "Modelled"))+
  geom_line(aes(y= Temperature.y, col= "Climatestation"))+
  geom_line(aes(y= mn_sT, col= "Ibuttons"))+
    facet_wrap(~Site_f)+
  labs(x = "Date", y= "Temperature (°C)")+
  #scale_color_manual(values = c("#33CCFF", "#FF6666", "#33FF99", "#CC00FF"),
  #                  name= "Temperature data")+
  theme_bw()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 18), legend.title=element_text(size=16), 
        legend.text=element_text(size=14), strip.text = element_text(size = 16), panel.spacing.x=unit(1, "lines"))


ggplot(AllTemp, aes(Date, color = loggertype))+
  geom_line(aes(y= Temperature.x, col= "Gridded" ))+
  geom_line(aes(y= Temperature.y, col= "Station_sT"))+
  geom_line(aes(y= mn_sT, col= "Ibutton"))+
  geom_line(aes(y= new_T, col= "Zmodel"))+
  facet_wrap(~Site, scales = "free_y")+
  labs(x = "Date", y= "Temperature (°C)")+
  theme_bw()+
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 18), legend.title=element_text(size=16), 
        legend.text=element_text(size=14))

# first run TBI_ibutton, TBI_data and TBI_analysis

# create AllVar for plotting
AllVar <- c("S", "k", "gridTemp", "gridPrec", "year", "Slope", "Aspect", "pH", "NO3N", "NH4N", "Plant_comm", "Root", "SoilN", "SoilC", "soil_CN", "soil_moist", "Bryo", "Gram", "Forbs", "Litter", "Live", "Total", "rich", "div" )

MyVar <- c("S", "k", "gridTemp", "gridPrec", "pH", "NO3N", "NH4N", "Plant_comm", "Root", "soil_CN", "soil_moist", "Bryo", "Gram", "Forbs", "Litter", "Total", "rich", "div" )

# quick plots to look at relations between variables
Mydotplot(TBI_variables[, MyVar]) 
pairs(TBI_variables[, MyVar], lower.panel = panel.cor)


# gridTemp   col= factor(year)), 
ggplot(TBI_variables, aes(modelTemp, S, col= factor(Temp.x)))+
  geom_point(aes(shape = factor(year)), size= 3, width = 0.1)+
  geom_smooth(method = "lm", se = FALSE)+
  #facet_wrap(~site)+
  labs(y= "Decomposition rate (k)", x = "Temperature (°C)")+
  scale_color_discrete(name= "elevation", labels = c("alpine", "subalpine", "boreal"))+
  scale_shape_discrete(solid = FALSE, guide = FALSE)+
  theme_classic()+
   theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15), legend.title=element_text(size=14), 
         legend.text=element_text(size=12))

#MANUsCRIPT PLOT CLIMATE DATA
TBI_grid<- TBI_variables%>%
  group_by(site, year) %>%
  summarise(m.T = mean(gridTemp), m.P = max(gridPrec), m.k = mean(new.k, na.rm =TRUE), m.S = mean(S, na.rm =TRUE), sd.k = sd(new.k, na.rm =TRUE), sd.S = sd(S, na.rm =TRUE), N.k =length(na.omit(new.k)), N.S = length(na.omit(S)), se.k   = sd.k / sqrt(N.k), se.S   = sd.S / sqrt(N.S))

#add columns with precipitation and temperature level
tempV<-c("Boreal","Boreal","Boreal","Boreal","Sub-alpine","Sub-alpine","Sub-alpine","Sub-alpine","Alpine","Alpine","Alpine","Alpine")
names(tempV)<-c("Ulv","Lav","Gud","Skj","Alr","Hog","Ram","Ves","Fau","Vik","Arh","Ovs")
#tempV[overviewsitesdata$site]
TBI_grid$templevel<-tempV[TBI_grid$site]
TBI_grid$templevel = factor(TBI_grid$templevel, levels=c("Alpine","Sub-alpine","Boreal"))

precL<-c(1,2,3,4,1,2,3,4,1,2,3,4)
names(precL)<-c("Ulv","Lav","Gud","Skj","Alr","Hog","Ram","Ves","Fau","Vik","Arh","Ovs")
TBI_grid$preclevel<-precL[TBI_grid$site]
TBI_grid$preclevel<-as.factor(TBI_grid$preclevel)

#calculate mean +Sd for temp and prec per year
TBI_climate_M<-TBI_variables%>%
  group_by(site) %>%
  summarise(m.T = mean(gridTemp, na.rm =TRUE), m.P = mean(gridPrec, na.rm =TRUE), sd.T = sd(modelTemp, na.rm =TRUE), sd.P = sd(gridPrec, na.rm =TRUE), se.T = sd(modelTemp)/sqrt(length(modelTemp)), se.P = sd(gridPrec)/sqrt(length(gridPrec)), m.k = mean(k), m.S = mean(S), sd.k = sd(k), sd.S = sd(S), se.k = sd(k)/sqrt(length(k)), se.S = sd(S)/sqrt(length(S)) )

tempV<-c(3,3,3,3,2,2,2,2,1,1,1,1)
names(tempV)<-c("Ulv","Lav","Gud","Skj","Alr","Hog","Ram","Ves","Fau","Vik","Arh","Ovs")
#tempV[overviewsitesdata$site]
TBI_climate_M$templevel<-tempV[TBI_climate_M$site]

precL<-c(1,2,3,4,1,2,3,4,1,2,3,4)
names(precL)<-c("Ulv","Lav","Gud","Skj","Alr","Hog","Ram","Ves","Fau","Vik","Arh","Ovs")
TBI_climate_M$preclevel<-precL[TBI_climate_M$site]

par(mar=c(5,5,2,2))
plot(TBI_grid$m.T ~ TBI_grid$m.P,
     xlab = "Summer Precipitation (mm)",
     ylab = "Summer Temperature (°C)",
     cex.axis=2, 
     cex.lab=2.5,
     xlim=c(100,1000),
     ylim=c(6,14),
     pch = c(24, 21, 22, 25)[as.numeric(TBI_grid$preclevel)], cex=2, # different 'pch' types 
     bg = c("dodgerblue", "lightgreen","red")[as.numeric(TBI_grid$templevel)])
  arrows(x0=TBI_climate_M$m.P-TBI_climate_M$sd.P, x1=TBI_climate_M$m.P+TBI_climate_M$sd.P, y0=TBI_climate_M$m.T, y1=TBI_climate_M$m.T,   length=0)
  arrows(x0=TBI_climate_M$m.P, x1=TBI_climate_M$m.P, y0=TBI_climate_M$m.T-TBI_climate_M$sd.T, y1=TBI_climate_M$m.T+TBI_climate_M$sd.T,   length=0)
  points(TBI_climate_M$m.P, TBI_climate_M$m.T, 
            pch = c(24, 21, 22, 25)[as.numeric(TBI_climate_M$preclevel)], cex=4.5, lwd=3, 
            bg = c("dodgerblue", "lightgreen","red")[as.numeric(TBI_climate_M$templevel)])

  
##### BUBBLE PLOT Judith
TBI_grid_plot<- TBI_grid
TBI_grid_plot$preclevel<- as.numeric(TBI_grid_plot$preclevel)
p1<- ggplot()
p1<- p1 + geom_point(data= subset(TBI_grid_plot, templevel== "ALP"), aes(x = m.P, y = m.T, size= m.k, fill=preclevel), shape=21)
p1<- p1 +   scale_fill_gradient(low= "lightblue", high= "darkblue")
p1<- p1 + scale_x_continuous(limits = c(200, 1000))
p1<- p1 + scale_y_continuous(limits = c(5,15))
p1<- p1 + scale_size(range = c(5,25))
p1<- p1 +theme_few()
p1<- p1 + theme(legend.position = "none")
p1

p2<- ggplot()
p2<- p2 + geom_point(data= subset(TBI_grid_plot, templevel== "SUB"), aes(x = m.P, y = m.T, size= m.k, fill=preclevel), shape=21)
p2<- p2 +   scale_fill_gradient(low= "lightgreen", high= "darkgreen")
p2<- p2 + scale_x_continuous(limits = c(200, 1000))
p2<- p2 + scale_y_continuous(limits = c(5,15))
p2<- p2 + scale_size(range = c(5,25))
p2<- p2 + theme_few()
p2<- p2 + theme(legend.position = "none")
p2

p3<- ggplot()
p3<- p3 + geom_point(data= subset(TBI_grid_plot, templevel== "BOR"), aes(x = m.P, y = m.T, size= m.k, fill=preclevel), shape=21)
p3<- p3 +   scale_fill_gradient(low= "pink", high= "darkred")
p3<- p3 + scale_x_continuous(limits = c(200, 1000))
p3<- p3 + scale_y_continuous(limits = c(5,15))
p3<- p3 + scale_size(range = c(5, 25))
p3<- p3 +theme_few()
p3<- p3 + theme(legend.position = "none")

p3


  

########Figure 2 Plot Temp, Prec and K and S over years, facet Templevel, col= Preclevel
# Temperature over years Figure 2a
ann_textA <- data.frame(year = 2016, m.T = 14,lab=c("a)","b)", "c)"),
                       templevel = factor(c("Alpine","Sub-alpine","Boreal"),levels = c("Alpine","Sub-alpine","Boreal")))
ann_textB <- data.frame(year = 2016, m.P = 1050 ,lab=c("d)","e)", "f)"),
                        templevel = factor(c("Alpine","Sub-alpine","Boreal"),levels = c("Alpine","Sub-alpine","Boreal")))
ann_textC <- data.frame(year = 2016, m.k = 0.015,lab=c("g)","h)", "i)"),
                        templevel = factor(c("Alpine","Sub-alpine","Boreal"),levels = c("Alpine","Sub-alpine","Boreal")))
ann_textD <- data.frame(year = 2016, m.S = 0.43,lab=c("j)","k)", "l)"),
                        templevel = factor(c("Alpine","Sub-alpine","Boreal"),levels = c("Alpine","Sub-alpine","Boreal")))


Figure6A<- ggplot(TBI_grid, aes(factor(year), group= factor(preclevel)))+
    geom_point(aes(y= m.T, fill= factor(templevel) , shape = factor(preclevel)), size= 5, position = position_dodge(0.5))+
    geom_line(aes(y= m.T, linetype= factor(preclevel)), size=1, position = position_dodge(0.5), show.legend = FALSE)+
    facet_wrap(~templevel)+
    geom_text(data = ann_textA ,aes(x= 3.3, y= 14, label =lab), size =7, inherit.aes = FALSE)+
    labs(y= "Temperature (°C)", x = " Year ")+
    scale_fill_manual(values =c("dodgerblue", "lightgreen","red"),
                      name="Temperature level", 
                      breaks=c("ALP", "SUB", "BOR"), 
                      labels = c("ALP", "SUB", "BOR"),
                      guide= FALSE)+
    scale_colour_manual(values =c("#000000", "#000000", "#000000","#000000"),
                        name="Temperature level", 
                        breaks=c("1", "2", "3", "4"), 
                        labels = c("ALP", "SUB", "BOR", "ALL"),
                        guide = FALSE)+
    scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("600mm", "1200mm", "2000mm", "2700mm"))+
    scale_y_continuous(breaks = c(6,7,8,9,10,11,12,13,14))+
    scale_linetype_discrete(name = "Year")+
    theme_few()+
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = c(0.1, 0.85),  legend.title = element_text(size= 22), legend.text = element_text(size= 22), strip.text.x = element_text(size = 18))



  
## Figure 2B
ggplot(TBI_grid, aes(factor(year), group= factor(preclevel)))+
  geom_point(aes(y= m.P, fill= factor(templevel) , shape = factor(preclevel)), size= 5, position = position_dodge(0.5))+
  geom_line(aes(y= m.P, linetype= factor(preclevel)), size=1, position = position_dodge(0.5), show.legend = FALSE)+
  facet_wrap(~templevel)+
  #geom_text(data = ann_textB ,aes(x= 3.3, y= 1050, label =lab), size =7, inherit.aes = FALSE)+
    labs(y= "Precipitation (mm)", x = " Year ")+
    scale_fill_manual(values =c("dodgerblue", "lightgreen","red"),
                      name="Temperature level", 
                      breaks=c("1", "2", "3"), 
                      labels = c("ALP", "SUB", "BOR"),
                      guide= FALSE)+
    scale_colour_manual(values =c("#000000", "#000000", "#000000","#000000"),
                        name="Temperature level", 
                        breaks=c("1", "2", "3", "4"), 
                        labels = c("ALP", "SUB", "BOR", "ALL"),
                        guide = FALSE)+
    scale_shape_manual(values = c(24, 21, 22, 25),
                       name="Precipitation level", 
                       breaks=c("1", "2", "3", "4"), 
                       labels = c("1", "2", "3", "4"))+
    theme_few()+
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = "none", strip.text.x = element_blank())
  
  #### Figure 6C
ggplot(TBI_grid, aes(factor(year), group= factor(preclevel)))+
  geom_errorbar(aes(ymin=m.k-se.k, ymax=m.k+se.k), width=.3, position = position_dodge(0.5)) +
  geom_point(aes(y= m.k, fill= factor(templevel) , shape = factor(preclevel)), size= 5, position = position_dodge(0.5))+
  geom_line(aes(y= m.k, linetype= factor(preclevel)), size=1, position = position_dodge(0.5), show.legend = FALSE)+
  facet_wrap(~templevel)+
  geom_text(data = ann_textC ,aes(x= 3.3, y= 0.015, label =lab), size =7, inherit.aes = FALSE)+
  labs(y= expression(paste("Decomposition rate ", (italic("k")))), x = " Year ")+
  scale_fill_manual(values =c("dodgerblue", "lightgreen","red"),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"),
                    guide= FALSE)+
  scale_colour_manual(values =c("#000000", "#000000", "#000000","#000000"),
                      name="Temperature level", 
                      breaks=c("1", "2", "3", "4"), 
                      labels = c("ALP", "SUB", "BOR", "ALL"),
                      guide = FALSE)+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4"))+
  scale_y_continuous(breaks = c(0.006, 0.008, 0.010, 0.012, 0.014, 0.016))+
  theme_few()+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = "none", strip.text.x = element_blank())

## Figure 6D
ggplot(TBI_grid, aes(factor(year), group = factor(preclevel)))+
  geom_errorbar(aes(ymin=m.S-se.S, ymax=m.S+se.S), width=.3, position = position_dodge(0.5)) +
  geom_point(aes(y= m.S, fill= factor(templevel) , shape = factor(preclevel)), size= 5, position = position_dodge(0.5))+
  geom_line(aes(y= m.S, group= factor(preclevel), linetype= factor(preclevel)), size=1, position = position_dodge(0.5))+
  facet_wrap(~templevel)+
  geom_text(data = ann_textD ,aes(x= 3.3, y= 0.43, label =lab), size =7, inherit.aes = FALSE)+
  labs(y= expression(paste("Stabilisation factor ", (italic("S")))), x = " Year ")+
  scale_fill_manual(values =c("dodgerblue", "lightgreen","red"),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"),
                    guide= FALSE)+
  scale_colour_manual(values =c("#000000", "#000000", "#000000","#000000"),
                      name="Temperature level", 
                      breaks=c("1", "2", "3", "4"), 
                      labels = c("ALP", "SUB", "BOR", "ALL"),
                      guide = FALSE)+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4"))+
  theme_few()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = "none", strip.text.x = element_blank())

### barplot of k and S of sites across year
ggplot(TBI_grid, aes(x=preclevel, y=m.k, fill=factor(year))) +
  geom_bar(colour="black", stat="identity", position = "dodge") +
  guides(fill=FALSE)+
  facet_grid(~templevel)



# MANUSCRIPT PLOT1A
ggplot(TBI_variables, aes(gridTemp))+
  geom_point(aes(y= new.k, fill= factor(Temp.x) , shape = factor(Prec.x)), size= 5, width = 0.1)+
  geom_smooth(data= TBI_variables, aes(y = new.k, col = "black"), method = "lm", linetype= "dashed", size=3, se= FALSE)+
  geom_smooth(data= subset(TBI_variables, Temp.x== 1), aes(y= new.k, col= factor(Temp.x)), method = "lm", linetype="dashed", 
              size= 1.5, se=FALSE)+
  geom_smooth(data= subset(TBI_variables, Temp.x== 2 | Temp.x== 3), aes(y= new.k, col= factor(Temp.x)), method = "lm", size= 1.5, 
              se=FALSE)+
  #facet_wrap(~year)+
  labs(y= expression(paste("Decomposition rate ", (italic("k")))),  x = "Temperature (°C)")+
  scale_fill_manual(values =c("dodgerblue", "lightgreen","red"),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"),
                    guide= FALSE)+
  scale_colour_manual(values =c("dodgerblue","lightgreen","red","#CCCCCC"),   
                       name="Temperature level", 
                       breaks=c("1", "2", "3", "4"), 
                       labels = c("ALP", "SUB", "BOR", "ALL"),
                       guide = FALSE)+
  scale_shape_manual(values = c(24, 21, 22, 25),
                        name="Precipitation level", 
                        breaks=c("1", "2", "3", "4"), 
                        labels = c("1", "2", "3", "4")) +
  scale_y_continuous(breaks = c(0.005, 0.010, 0.015, 0.020, 0.025))+
  scale_x_continuous(breaks = c(7,8,9,10,11,12,13,14,15))+
  #annotate("text", x=14.5, y=0.025, label= "a)", size =7)+
  guides(col=guide_legend(override.aes=list(size=5.5)))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 35), axis.text.x=element_text(size = 30), axis.title = element_text(size = 35), axis.text.y = element_text(size = 30), legend.position =  "none",  legend.title = element_text(size= 22), legend.text = element_text(size= 22), strip.text.x = element_text(size = 18))


# MANUSCRIPT PLOT 1B
ggplot(TBI_variables, aes(gridPrec))+
  geom_point(aes(y= new.k, fill= factor(Temp.x) , shape = factor(Prec.x)), size= 5, width = 0.1)+
  geom_smooth(aes(y = new.k, col = "black"), method = "lm", linetype= "solid", size=3, se= FALSE)+
  geom_smooth(aes(y= new.k, col= factor(Temp.x)), method = "lm", size=1.5, se = FALSE)+
  labs(y= expression(paste("Decomposition rate ", (italic("k")))), x = "Precipitation (mm)")+
  scale_fill_manual(values =c("dodgerblue","lightgreen", "red" ),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("Alpine", "Sub-alpine", "Boreal"))+
  scale_color_manual(values =c("dodgerblue", "lightgreen","red", "#CCCCCC"),
                     name="Temperature level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("Alpine", "Sub-alpine", "Boreal", "ALL"))+
                     scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("600mm", "1200mm", "2000mm", "2700mm")) +
  scale_y_continuous(breaks = c(0.005, 0.010, 0.015, 0.020, 0.025))+
  scale_x_continuous(breaks = c(100,200,300,400,500,600,700,800,900,1000), limits = c(100,1050))+
  #annotate("text", x=1000, y=0.025, label= "b)", size =7)+
  guides(fill=guide_legend(override.aes=list(size=6)))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 35), axis.text.x=element_text(size = 30), axis.title = element_text(size = 35), axis.text.y = element_text(size = 30), legend.position =  "none",  legend.title = element_text(size= 22), legend.text = element_text(size= 22), strip.text.x = element_text(size = 18))

#Stabilization factor S
ggplot(TBI_variables, aes(gridTemp))+
  geom_point(aes(y= S, fill= factor(Temp.x) , shape = factor(Prec.x)), size= 5, width = 0.1)+
  geom_smooth(data= TBI_variables, aes(y = S, col = "black"), method = "lm", linetype= "solid", size=3, se= FALSE)+
  geom_smooth(data= subset(TBI_variables, Temp.x== 1), aes(y= S, col= factor(Temp.x)), method = "lm", size=1.5, linetype= "dashed",
              se = FALSE)+
  geom_smooth(data= subset(TBI_variables, Temp.x== 2 | Temp.x== 3), aes(y= S, col= factor(Temp.x)), method = "lm", size=1.5, 
              se = FALSE)+
  labs(y= expression(paste("Stabilisation factor ", (italic("S")))), x = "Temperature (°C)")+
  #facet_wrap(~year)+
  scale_fill_manual(values =c("dodgerblue", "lightgreen","red"),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"),
                    guide= FALSE)+
  scale_colour_manual(values =c("dodgerblue", "lightgreen","red", "#CCCCCC" ), 
                      name="Temperature level", 
                      breaks=c("1", "2", "3", "4"), 
                      labels = c("ALP", "SUB", "BOR", "ALL"),
                      guide = FALSE)+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  scale_x_continuous(breaks = c(7,8,9,10,11,12,13,14))+
  #annotate("text", x=14.5, y=0.5, label= "c)", size =7)+
  guides(col=guide_legend(override.aes=list(size=5)))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 35), axis.text.x=element_text(size = 30), axis.title = element_text(size = 35), axis.text.y = element_text(size = 30), legend.position = "none",  strip.text.x = element_text(size = 18))

# Stabiliation factor S vs Precipitation
ggplot(TBI_variables, aes(gridPrec))+
  geom_point(aes(y= S, fill= factor(Temp.x) , shape = factor(Prec.x)), size= 5, width = 0.1)+
  geom_smooth(data= TBI_variables, aes(y = S, col = "black"), method = "lm", linetype= "solid", size=3, se= FALSE)+
  geom_smooth(data= subset(TBI_variables, Temp.x== 1 | Temp.x== 2), aes(y= S, col= factor(Temp.x)), method = "lm", size=1.5, 
             se = FALSE)+
  geom_smooth(data= subset(TBI_variables, Temp.x==3), aes(y= S, col= factor(Temp.x)), method = "lm", linetype= "dashed", size=1.5, 
              se = FALSE)+
  labs(y= expression(paste("Stabilisation factor ", (italic("S")))), x = "Precipitation (mm)")+
  #facet_wrap(~year)+
    scale_fill_manual(values =c("dodgerblue","lightgreen", "red" ),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"))+
  scale_color_manual(values =c("dodgerblue", "lightgreen", "red","#CCCCCC" ), 
                     name="Temperature level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("ALP", "SUB", "BOR", "ALL"),
                     guide=FALSE)+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  scale_x_continuous(breaks = c(100,200,300,400,500,600,700,800,900,1000), limits = c(100,1050))+
  #annotate("text", x=1000, y=0.5, label= "d)", size =7)+
  guides(fill=guide_legend(override.aes=list(size=6)))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 35), axis.text.x=element_text(size = 30), axis.title = element_text(size = 35), axis.text.y = element_text(size = 30), legend.position = "none", strip.text.x = element_text(size = 18))


####### precipitation levels

ggplot(TBI_variables, aes(gridTemp))+
  geom_point(aes(y= new.k, fill= factor(Temp.x) , shape = factor(Prec.x)), size= 5, width = 0.1)+
  geom_smooth(data= TBI_variables, aes(y = new.k, col = "black"), method = "lm", linetype= "solid", size=3, se= FALSE)+
  geom_smooth(aes(y= new.k, col= factor(Prec.x)), method = "lm", size=1.5, se = FALSE)+
  labs(y= " decomposition rate (k) ", x = "Precipitation (mm)")+
  #facet_wrap(~year)+
  scale_fill_manual(values =c("dodgerblue","lightgreen", "red" ),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"))+
  scale_color_manual(values = c("skyblue", "royalblue", "blue", "navy", "#CCCCCC"))+ 
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  scale_x_continuous(breaks = c(7,8,9,10,11,12,13,14))+
  annotate("text", x=15, y=0.025, label= "d)", size =7)+
  guides(fill=guide_legend(override.aes=list(size=6)))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = "none", strip.text.x = element_text(size = 18))


ggplot(TBI_variables, aes(gridPrec))+
  geom_point(aes(y= new.k, fill= factor(Temp.x) , shape = factor(Prec.x)), size= 5, width = 0.1)+
  geom_smooth(data= TBI_variables, aes(y = new.k, col = "all"), method = "lm", linetype= "solid", size=3, se= FALSE)+
  geom_smooth(aes(y= new.k, col= factor(Prec.x)), method = "lm", size=1.5, se = FALSE)+
  labs(y= " decomposition rate (k) ", x = "Precipitation (mm)")+
  #facet_wrap(~year)+
  scale_fill_manual(values =c("dodgerblue","lightgreen", "red" ),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"),
                    guide= FALSE)+
  scale_color_manual(values = c("skyblue", "royalblue", "blue", "navy", "#CCCCCC"))+ 
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  scale_x_continuous(breaks = c(100,200,300,400,500,600,700,800,900,1000))+
  annotate("text", x=1000, y=0.025, label= "d)", size =7)+
  guides(fill=guide_legend(override.aes=list(size=6)))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = c(0.8, 0.8), strip.text.x = element_text(size = 18))

ggplot(TBI_variables, aes(gridTemp))+
  geom_point(aes(y= S, fill= factor(Temp.x) , shape = factor(Prec.x)), size= 5, width = 0.1)+
  geom_smooth(data= TBI_variables, aes(y = S, col = "black"), method = "lm", linetype= "solid", size=3, se= FALSE)+
  geom_smooth(aes(y= S, col= factor(Prec.x)), method = "lm", size=1.5, se = FALSE)+
  labs(y= " Stabilization factor (S) ", x = "Temperature (°C)")+
  #facet_wrap(~year)+
  scale_fill_manual(values =c("dodgerblue","lightgreen", "red" ),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"))+
  scale_color_manual(values = c("skyblue", "royalblue", "blue", "navy", "#CCCCCC"))+ 
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  scale_x_continuous(breaks = c(7,8,9,10,11,12,13,14))+
  annotate("text", x=15, y=0.025, label= "d)", size =7)+
  guides(fill=guide_legend(override.aes=list(size=6)))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = "none", strip.text.x = element_text(size = 18))


ggplot(TBI_variables, aes(gridPrec))+
  geom_point(aes(y= S, fill= factor(Temp.x) , shape = factor(Prec.x)), size= 5, width = 0.1)+
  geom_smooth(data= TBI_variables, aes(y = S, col = "black"), method = "lm", linetype= "solid", size=3, se= FALSE)+
  geom_smooth(aes(y= S, col= factor(Prec.x)), method = "lm", size=1.5, se = FALSE)+
  labs(y= " Stabilization factor (S) ", x = "Precipitation (mm)")+
  #facet_wrap(~year)+
  scale_fill_manual(values =c("dodgerblue","lightgreen", "red" ),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"))+
  scale_color_manual(values = c("skyblue", "royalblue", "blue", "navy", "#CCCCCC"))+ 
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  scale_x_continuous(breaks = c(100,200,300,400,500,600,700,800,900,1000))+
  annotate("text", x=1000, y=0.5, label= "d)", size =7)+
  guides(fill=guide_legend(override.aes=list(size=6)))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = "none", strip.text.x = element_text(size = 18))

#### OBSERVED VS PREDICTED K
ggplot(TBI_means, aes(y=new.k, x=modelK))+
  geom_point(aes(fill= factor(Temp.x) , shape = factor(Prec.x)), size= 5, width = 0.1)+
  geom_abline(intercept = 0, slope = +1, color="black", size=1.5)+
  labs(x= expression(paste("Predicted ", (italic("k")))), y = expression(paste("Observed ", (italic("k")))))+
  #facet_wrap(~year)+
  scale_fill_manual(values =c("dodgerblue","lightgreen", "red" ),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"))+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  scale_x_continuous(limits = c(0,0.014))+
  scale_y_continuous(limits = c(0,0.014))+
  annotate("text", x=0.014, y=0.013, label= "a)", size =7)+
  guides(fill=guide_legend(override.aes=list(size=6)))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = "none", strip.text.x = element_text(size = 18))

#### OBSERVED VS PREDICTED S
ggplot(TBI_means, aes(x= modelS, y= S))+
  geom_point(aes(fill= factor(Temp.x) , shape = factor(Prec.x)), size= 5, width = 0.1)+
  geom_abline(intercept = 0, slope = +1, color="black", size=1.5)+
  labs(x= expression(paste("Predicted ", (italic("S")))), y = expression(paste("Observed ", (italic("S")))))+
  #facet_wrap(~year)+
  scale_fill_manual(values =c("dodgerblue","lightgreen", "red" ),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"))+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  scale_x_continuous(limits = c(0,0.4))+
  scale_y_continuous(limits = c(0,0.4))+
  annotate("text", x=0.41, y=0.37, label= "b)", size =7)+
  guides(fill=guide_legend(override.aes=list(size=6)))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = "none", strip.text.x = element_text(size = 18))


#### ENVIRONMENTAL FACTORS 
ggplot(TBI_means, aes(pH))+
  geom_point(aes(y= new.k, fill= factor(Temp.x) , shape = factor(Prec.x)), size= 5, width = 0.1)+
  geom_smooth(data= TBI_variables, aes(y = new.k, col = "black"), method = "lm", linetype= "solid", size=3, se= FALSE)+
  geom_smooth(aes(y= new.k, col= factor(Temp.x)), method = "lm", size=1.5, se = FALSE)+
  labs(y= " decomposition rate (k) ", x = "Soil pH")+
  #facet_wrap(~year)+
  scale_fill_manual(values =c("dodgerblue","lightgreen", "red" ),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"))+
  scale_color_manual(values =c("dodgerblue","lightgreen", "red", "#CCCCCC" ),
                     name="Temperature level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("ALP", "SUB", "BOR", "ALL"),
                     guide=FALSE)+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  guides(fill=guide_legend(override.aes=list(size=6)))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = "none", strip.text.x = element_text(size = 18))

ggplot(TBI_means, aes(soil_C.N))+
  geom_point(aes(y= new.k, fill= factor(Temp.x) , shape = factor(Prec.x)), size= 5, width = 0.1)+
  geom_smooth(data= TBI_variables, aes(y = new.k, col = "black"), method = "lm", linetype= "solid", size=3, se= FALSE)+
  geom_smooth(aes(y= new.k, col= factor(Temp.x)), method = "lm", size=1.5, se = FALSE)+
  labs(y= " decomposition rate (k) ", x = "Soil CN")+
  #facet_wrap(~year)+
  scale_fill_manual(values =c("dodgerblue","lightgreen", "red" ),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"))+
  scale_color_manual(values =c("dodgerblue","lightgreen", "red", "#CCCCCC" ),
                     name="Temperature level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("ALP", "SUB", "BOR", "ALL"),
                     guide=FALSE)+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  guides(fill=guide_legend(override.aes=list(size=6)))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = "none", strip.text.x = element_text(size = 18))

ggplot(TBI_means, aes(P_div))+
  geom_jitter(aes(y= new.k, fill= factor(Temp.x) , shape = factor(Prec.x)), size= 5, width = 0.1)+
  geom_smooth(data= TBI_variables, aes(y = new.k, col = "black"), method = "lm", linetype= "solid", size=3, se= FALSE)+
  geom_smooth(aes(y= new.k, col= factor(Temp.x)), method = "lm", size=1.5, se = FALSE)+
  labs(y= " decomposition rate (k) ", x = "Plant diversity")+
  #facet_wrap(~year)+
  scale_fill_manual(values =c("dodgerblue","lightgreen", "red" ),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"))+
  scale_color_manual(values =c("dodgerblue","lightgreen", "red", "#CCCCCC" ),
                     name="Temperature level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("ALP", "SUB", "BOR", "ALL"),
                     guide=FALSE)+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  guides(fill=guide_legend(override.aes=list(size=6)))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = "none", strip.text.x = element_text(size = 18))


ggplot(TBI_variables, aes(Litter.CN))+
  geom_point(aes(y= S, fill= factor(Temp.x) , shape = factor(Prec.x)), size= 5, width = 0.1)+
  geom_smooth(data= TBI_variables, aes(y = S, col = "black"), method = "lm", linetype= "solid", size=3, se= FALSE)+
  geom_smooth(aes(y= S, col= factor(Temp.x)), method = "lm", size=1.5, se = FALSE)+
  labs(y= " Stabilization factor (S) ", x = "Litter CN")+
  #facet_wrap(~year)+
  scale_fill_manual(values =c("dodgerblue","lightgreen", "red" ),
                    name="Temperature level", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"))+
  scale_color_manual(values =c("dodgerblue","lightgreen", "red", "#CCCCCC" ),
                     name="Temperature level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("ALP", "SUB", "BOR", "ALL"),
                     guide=FALSE)+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  guides(fill=guide_legend(override.aes=list(size=6)))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = "none", strip.text.x = element_text(size = 18))


############Supplementary data
ggplot(TBI_variables, aes(factor(year), S, fill = factor(year)))+
  stat_boxplot(geom ='errorbar', width = 0.4)+
  geom_boxplot()+
  theme_classic()+
  scale_fill_grey(start =0.4, end = 1.0, name= "Year", labels = c("2014", "2015", "2016"))+
  labs(x= "year", y = "Stabilization factor (S)")+
  annotate("text", x=1, y=0.5, label= "*", size =10)+
  annotate("text", x=3.5, y=0.5, label= "b)", size =10)+
  theme(axis.title = element_text(size = 25), axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 18), legend.position = "none", legend.title = element_text(size= 24), legend.text = element_text(size= 22),strip.text.x = element_text(size = 18)) 
ggplot(TBI_variables, aes(factor(year), new.k, fill = factor(year)))+
  stat_boxplot(geom ='errorbar', width = 0.4)+
  geom_boxplot()+
  theme_classic()+
  scale_fill_grey(start =0.4, end = 1.0, name= "Year", labels = c("2014", "2015", "2016"))+
  labs(x= "year", y = "Decomposition rate (k)")+
  annotate("text", x=1, y=0.025, label= "*", size =10)+
  annotate("text", x=3.5, y=0.025, label= "a)", size =10)+
  theme(axis.title = element_text(size = 25), axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 18), legend.position = "none", legend.title = element_text(size= 24), legend.text = element_text(size= 22),strip.text.x = element_text(size = 18))

#Boxplot for manuscript decomp K against year split with Precipition level
ggplot(TBI_variables, aes(factor(year), S, fill = factor(year)))+
  stat_boxplot(geom ='errorbar', width = 0.4)+
  geom_boxplot()+
  theme_classic()+
  facet_grid(.~Prec.x, switch= "both")+
  scale_fill_grey(start =0.4, end = 1.0, name= "Year", labels = c("2014", "2015", "2016"))+
  labs(x= "year", y = "Decomposition rate (k)")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title = element_text(size = 25),axis.text.y = element_text(size = 18), legend.position = c(0.92,0.85), legend.title = element_text(size= 24), legend.text = element_text(size= 22),strip.text.x = element_text(size = 18)) 

ggplot(TBI_variables2x, aes(year, k, fill = factor(year)))+
  stat_boxplot(geom ='errorbar', width = 0.4)+
  geom_boxplot()+
  theme_classic()+
  facet_grid(.~Temp.x_f, switch= "both")+
  scale_fill_grey(start =0.4, end = 1.0, name= "year", labels = c("2014", "2015", "2016"))+
  labs(x= "Year", y = "Decomposition rate (k)")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title = element_text(size = 25),axis.text.y = element_text(size = 18), strip.text.x = element_text(size = 18), legend.position = "none" )

ggplot(TBI_variables, aes(factor(year), S, fill = factor(year)))+
  stat_boxplot(geom ='errorbar', width = 0.4)+
  geom_boxplot()+
  theme_classic()+
  facet_grid(.~Prec.x, switch= "both")+
  scale_fill_grey(start =0.4, end = 1.0, name= "Year", labels = c("2014", "2015", "2016"))+
  labs(x= "year", y = "Stabilisation factor (S)")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title = element_text(size = 25),axis.text.y = element_text(size = 18), legend.position = "none", strip.text.x = element_text(size = 18))

ggplot(TBI_variables2x, aes(year, S, fill = factor(year)))+
  stat_boxplot(geom ='errorbar', width = 0.4)+
  geom_boxplot()+
  theme_classic()+
  facet_grid(.~Temp.x_f, switch= "both")+
  scale_fill_grey(start =0.4, end = 1.0, name= "year", labels = c("2014", "2015", "2016"))+
  labs(x= "Year", y = "Stabilisation factor (s)")+
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

ggplot(TBI_, aes())

ggplot(TBI_variables, aes(modelTemp, k, col = factor(year)))+
  geom_point()+
  #geom_boxplot(aes(y= S, col = "green" ))+
  theme_classic()+
  facet_grid(.~Temp.x, switch= "both")+
  scale_fill_grey(start =0.4, end = 1.0, name= "Precipitation level", labels = c("Prec1", "Prec2", "Prec3", "Prec4"))+
  labs(x= "year", y = "decomposition rate (k)")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title = element_text(size = 25), axis.text.y = element_text(size = 18), legend.position = c(.15,.15), legend.title = element_text(size= 20), legend.text = element_text(size= 20), strip.text.x = element_text(size = 18))

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
  geom_point(aes(y= decomp.R*100,  shape= "Rooibos"), size=4)+ 
  geom_point(aes(y= Ag*100,  shape= "Green"), size=4)+
  geom_smooth(aes( y =decomp.R*100 ,  linetype= "Rooibos"), col="#000000", method = "lm", se=FALSE)+
  geom_smooth(aes( y =Ag*100,  linetype = "Green"), col="#000000", method = "lm", se=FALSE)+
  facet_grid(~year)+
  theme_classic()+
  labs(y= " Decomposed tea (%) ", x = "Temperature (°C)")+
  scale_linetype_manual(values = c("solid", "dotdash"), 
                        name="Tea", 
                        breaks=c("Rooibos", "Green"), 
                        labels = c("Rooibos", "Green"),
                        guide = FALSE)+
  scale_shape_manual(values = c(1, 16), 
                     name="Tea", 
                     breaks=c("Rooibos", "Green"), 
                     labels = c("Rooibos", "Green"))+
  guides(shape=guide_legend(override.aes=list(size=5)))+
theme(axis.title.x=element_text(size = 24), axis.text.x=element_text(size = 22), axis.title = element_text(size = 24), axis.text.y = element_text(size = 22), legend.position = c(.1,.9), legend.title = element_text(size= 22), legend.text = element_text(size= 22), strip.text.x = element_text(size = 20))

ggplot(TBI_variables, aes(gridPrec))+
  geom_point(aes(y= decomp.R*100,  shape= "Rooibos"), size=4)+ 
  geom_point(aes(y= Ag*100,  shape= "Green"), size=4)+ 
  geom_smooth(aes( y =decomp.R*100,  linetype= "Rooibos"), col="#000000", method = "lm", se=FALSE)+
  geom_smooth(aes( y =Ag*100,  linetype = "Green"), col="#000000", method = "lm", se=FALSE)+
  facet_wrap(~year)+
  labs(y= " Decomposed tea (%) ", x = "Precipitation (mm)")+
  scale_shape_manual(values = c(1, 16), 
                     name="Tea", 
                     breaks=c("Rooibos", "Green"), 
                     labels = c("Rooibos", "Green")) +
  scale_linetype_manual(values = c("solid", "dotdash"), 
                        name="Tea", 
                        breaks=c("Rooibos", "Green"), 
                        labels = c("Rooibos", "Green"))+
  scale_x_continuous(breaks = c(300,500,700,900))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = "none", strip.background = element_blank(), strip.text.x = element_blank())

ggplot(TBI_variables, aes(modelTemp))+
  geom_point(aes(y= decomp.R*100, fill= factor(Temp.x) , shape = factor(Prec.x)), size=5)+ 
  geom_point(aes(y= Ag*100, fill= factor(Temp.x) , shape = factor(Prec.x)), size=5)+ 
  geom_smooth(aes( y =decomp.R*100,  linetype= "Rooibos"), col="#000000", method = "lm", se=FALSE)+
  geom_smooth(aes( y =Ag*100,  linetype = "Green"), col="#000000", method = "lm", se=FALSE)+
  facet_wrap(~year)+
  labs(y= " Decomposed tea (%) ", x = "Temperature (°C)")+
  scale_fill_manual(values =c("dodgerblue","lightgreen", "red" ),
                    guide = FALSE)+
  scale_colour_manual(values =c("#000000"))+
  scale_shape_manual(values = c(25, 22, 21, 24),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  scale_linetype_manual(values = c("solid", "dotdash"), 
                        name="Tea", 
                        breaks=c("Rooibos", "Green"), 
                        labels = c("Rooibos", "Green"))+
  scale_x_continuous(breaks = c(2,4,6,8,10,12, 14))+
  guides(shape=guide_legend(override.aes=list(size=5)))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = c(.08,.85), legend.title = element_text(size= 22), legend.text = element_text(size= 22), strip.text.x = element_text(size = 20))


ggplot(TBI_variables, aes(gridPrec))+
  geom_point(aes(y= decomp.R*100, fill= factor(Temp.x) , shape = factor(Prec.x)), size=5)+ 
  geom_point(aes(y= Ag*100, fill= factor(Temp.x) , shape = factor(Prec.x)), size=5)+ 
  geom_smooth(aes( y =decomp.R*100,  linetype= "Rooibos"), col="#000000", method = "lm", se=FALSE)+
  geom_smooth(aes( y =Ag*100,  linetype = "Green"), col="#000000", method = "lm", se=FALSE)+
  facet_wrap(~year)+
  labs(y= " Decomposed tea (%) ", x = "Precipitation (mm)")+
  scale_fill_manual(values =c("dodgerblue","lightgreen", "red" ),
                    guide= FALSE)+
  scale_colour_manual(values =c("#000000"))+
  scale_shape_manual(values = c(25, 22, 21, 24),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  scale_linetype_manual(values = c("solid", "dotdash"), 
                        name="Tea", 
                        breaks=c("Rooibos", "Green"), 
                        labels = c("Rooibos", "Green"))+
  scale_x_continuous(breaks = c(300,500,700,900))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = "none", strip.background = element_blank(), strip.text.x = element_blank())


########Figure 4A
ggplot(TBI_variables, aes(modelTemp))+
  geom_smooth(aes(y = k, col = "black"), method = "lm", linetype= "solid", size=2.5, se= FALSE)+
  geom_point(aes(y= k, fill= factor(Temp.x) , shape = factor(Prec.x)), size= 5, width = 0.1)+
  #geom_smooth(aes(y= k, col= factor(Temp.x)), method = "lm", size=1.5, se = FALSE)+
  facet_wrap(~year)+
  labs(y= " Decomposition rate (k) ", x = "Temperature (°C)")+
  scale_fill_manual(values =c("dodgerblue","lightgreen", "red" ),
                    name="Elevation", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"), 
                    guide = FALSE)+
  scale_colour_manual(values =c("#000000"),
                      guide=FALSE)+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  scale_x_continuous(breaks = c(7,8,9,10,11,12,13))+
  guides(shape=guide_legend(override.aes=list(size=5)))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = c(.15,.9), legend.title = element_text(size= 22), legend.text = element_text(size= 22), strip.text.x = element_text(size = 18))


########### Figure 4B
ggplot(TBI_variables, aes(gridPrec))+
  geom_smooth(aes(y = k, col = "black"), method = "lm", linetype= "solid", size=2.5, se= FALSE)+
  geom_point(aes(y= k, fill= factor(Temp.x) , shape = factor(Prec.x)), size= 5, width = 0.1)+
  #geom_smooth(aes(y= k, col= factor(Temp.x)), method = "lm", size=1.5, se = FALSE)+
  facet_wrap(~year)+
  labs(y= " Decomposition rate (k) ", x = "Precipitation (mm)")+
  scale_fill_manual(values =c("dodgerblue","lightgreen", "red" ),
                    name="Elevation", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"))+
  scale_colour_manual(values =c("#000000"))+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  scale_x_continuous(breaks = c(300,500,700,900))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = "none", strip.text.x = element_blank())


#fill is year!!
ggplot(TBI_variables, aes(modelTemp))+
  geom_smooth(aes(y = k, col = "black"), method = "lm", linetype= "solid", size=2.5, se= FALSE)+
  geom_point(aes(y= k, fill= factor(year) , shape = factor(Prec.x)), size= 5, width = 0.1)+
  #geom_smooth(aes(y= k, col= factor(Temp.x)), method = "lm", size=1.5, se = FALSE)+
  facet_wrap(~Temp.x)+
  labs(y= " Decomposition rate (k) ", x = "Temperature (°C)")+
  scale_fill_manual(values =c("grey","lightgrey", "white" ),
                    name="year", 
                    breaks=c("2014", "2015", "2016"), 
                    labels = c("2014", "2015", "2016"))+
  scale_colour_manual(values =c("#000000"),
                      guide=FALSE)+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  scale_x_continuous(breaks = c(7,8,9,10,11,12,13))+
  guides(shape=guide_legend(override.aes=list(size=5)))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = c(.15,.9), legend.title = element_text(size= 22), legend.text = element_text(size= 22), strip.text.x = element_text(size = 18))

ggplot(TBI_variables, aes(gridPrec))+
  geom_smooth(aes(y = k, col = "black"), method = "lm", linetype= "solid", size=2.5, se= FALSE)+
  geom_point(aes(y= k, fill= factor(year) , shape = factor(Prec.x)), size= 5, width = 0.1)+
  #geom_smooth(aes(y= k, col= factor(Temp.x)), method = "lm", size=1.5, se = FALSE)+
  facet_wrap(~Temp.x)+
  labs(y= " Decomposition rate (k) ", x = "Precipitation (mm)")+
  scale_fill_manual(values =c("grey","lightgrey", "white" ),
                    name="year", 
                    breaks=c("2014", "2015", "2016"), 
                    labels = c("2014", "2015", "2016"))+
  scale_colour_manual(values =c("#000000"))+
  scale_shape_manual(values = c(24, 21, 22, 25),
                     name="Precipitation level", 
                     breaks=c("1", "2", "3", "4"), 
                     labels = c("1", "2", "3", "4")) +
  scale_x_continuous(breaks = c(300,500,700,900))+
  theme_classic()+
  theme(axis.title.x=element_text(size = 25), axis.text.x=element_text(size = 22), axis.title = element_text(size = 25), axis.text.y = element_text(size = 22), legend.position = "none", strip.text.x = element_blank())


ggplot(TBI_variables, aes(modelTemp))+
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


