library(ggthemes)
library(gridExtra)
#install.packages("cowplot")
# TBI plots 

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


# Open a tiff file to write your plot output to
tiff(filename = "O:\\FunCab\\Manuscripts\\TBI\\figures\\ProofFigures\\Figure2ABC.tif", width = 15, height = 6, units = "in", res = 1200)
# Then put your plotting commands here
p1<- ggplot(TBI_grid, aes(factor(year), group= factor(preclevel)))+
  geom_point(aes(y= m.T, fill= factor(templevel) , shape = factor(preclevel)), size= 3, position = position_dodge(0.5))+
  geom_line(aes(y= m.T, linetype= factor(preclevel)), position = position_dodge(0.5), show.legend = FALSE)+
  facet_wrap(~templevel)+
  geom_text(data = ann_textA ,aes(x= 3.3, y= 13.5, label =lab), size= 4, inherit.aes = FALSE)+
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
                     labels = c("600mm", "1200mm", "2000mm", "2700mm"),
                     guide= FALSE)+
  scale_y_continuous(breaks = c(6,7,8,9,10,11,12,13,14))+
  scale_linetype_manual(name = "Precipitation level", values = c(1,2,3,4), labels = c("600mm", "1200mm", "2000mm", "2700mm"))+
  theme_classic()+
  theme(strip.text.x = element_text(size = 14), )
dev.off() # This saves the file


tiff(filename = "O:\\FunCab\\Manuscripts\\TBI\\figures\\ProofFigures\\Figure2DEF.tif", width = 15, height = 6, units = "in", res = 1200)
## Figure 2B
p2<- ggplot(TBI_grid, aes(factor(year), group= factor(preclevel)))+
  geom_point(aes(y= m.P, fill= factor(templevel) , shape = factor(preclevel)), size= 3, position = position_dodge(0.5))+
  geom_line(aes(y= m.P, linetype= factor(preclevel)), position = position_dodge(0.5), show.legend = FALSE)+
  facet_wrap(~templevel)+
  geom_text(data = ann_textB ,aes(x= 3.3, y= 1050, label =lab), size =4, inherit.aes = FALSE)+
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
  theme_classic()+
  theme(legend.position = "none", strip.text.x = element_blank())
dev.off() # This saves the file

tiff(filename = "O:\\FunCab\\Manuscripts\\TBI\\figures\\ProofFigures\\Figure2GHI.tif", width = 15, height = 6, units = "in", res = 1200)
#### Figure 6C
p3<- ggplot(TBI_grid, aes(factor(year), group= factor(preclevel)))+
  geom_errorbar(aes(ymin=m.k-se.k, ymax=m.k+se.k), width=.3, position = position_dodge(0.5)) +
  geom_point(aes(y= m.k, fill= factor(templevel) , shape = factor(preclevel)), size= 3, position = position_dodge(0.5))+
  geom_line(aes(y= m.k, linetype= factor(preclevel)), position = position_dodge(0.5), show.legend = FALSE)+
  facet_wrap(~templevel)+
  geom_text(data = ann_textC ,aes(x= 3.3, y= 0.015, label =lab), size =4, inherit.aes = FALSE)+
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
  theme_classic()+
  theme( legend.position = "none", strip.text.x = element_blank())
dev.off() # This saves the file

tiff(filename = "O:\\FunCab\\Manuscripts\\TBI\\figures\\ProofFigures\\Figure2JKL.tif", width = 15, height = 6, units = "in", res = 1200)
## Figure 6D
p4<- ggplot(TBI_grid, aes(factor(year), group = factor(preclevel)))+
  geom_errorbar(aes(ymin=m.S-se.S, ymax=m.S+se.S), width=.3, position = position_dodge(0.5)) +
  geom_point(aes(y= m.S, fill= factor(templevel) , shape = factor(preclevel)), size= 3, position = position_dodge(0.5))+
  geom_line(aes(y= m.S, group= factor(preclevel), linetype= factor(preclevel)), position = position_dodge(0.5))+
  facet_wrap(~templevel)+
  geom_text(data = ann_textD ,aes(x= 3.3, y= 0.43, label =lab), size =4, inherit.aes = FALSE)+
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
  theme_classic()+
  theme(legend.position = "none", strip.text.x = element_blank())
dev.off() # This saves the file

th <- theme(axis.title = element_text(size = 15), axis.text = element_text(size = 13), axis.title.x = element_blank(), axis.text.x = element_blank())
th2<- theme(axis.title = element_text(size = 15), axis.text = element_text(size = 13))
# set to final size (inches?)
x11(height = 210/25.4, width = 180/25.4)
cowplot::plot_grid(p1 + th, p2 +th, p3+ th, p4, ncol=1, align = "v")

Figure2<-cowplot::plot_grid(p1 + th, p2 +th, p3+ th, p4 + th2, ncol=1, align = "v")
cowplot::save_plot("O:\\FunCab\\Manuscripts\\TBI\\figures\\ProofFigures\\Figure2.eps", Figure2 , base_height = 11, base_width = 7)

# MANUSCRIPT PLOT1A
tiff(filename = "O:\\FunCab\\Manuscripts\\TBI\\figures\\ProofFigures\\Figure3A.tif", width = 13, height = 8, units = "in", res = 500)
p5<- ggplot(TBI_variables, aes(gridTemp))+
  geom_point(aes(y= new.k, fill= factor(Temp.x) , shape = factor(Prec.x)), size= 3, width = 0.1)+
  geom_smooth(data= TBI_variables, aes(y = new.k, col = "black"), method = "lm", linetype= "dashed", size=1, se= FALSE)+
  geom_smooth(data= subset(TBI_variables, Temp.x== 1), aes(y= new.k, col= factor(Temp.x)), method = "lm", linetype="dashed", 
              size= 1, se=FALSE)+
  geom_smooth(data= subset(TBI_variables, Temp.x== 2 | Temp.x== 3), aes(y= new.k, col= factor(Temp.x)), method = "lm", size= 1, 
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
  annotate("text", x=14.5, y=0.025, label= "a)", size =6)+
  guides(col=guide_legend(override.aes=list(size=5.5)))+
  theme_classic()+
  theme(legend.position =  "none")
dev.off() # This saves the file

tiff(filename = "O:\\FunCab\\Manuscripts\\TBI\\figures\\ProofFigures\\Figure3B.tif", width = 13, height = 8, units = "in", res = 500)
# MANUSCRIPT PLOT 1B
p6<- ggplot(TBI_variables, aes(gridPrec))+
  geom_point(aes(y= new.k, fill= factor(Temp.x) , shape = factor(Prec.x)), size= 3, width = 0.1)+
  geom_smooth(aes(y = new.k, col = "black"), method = "lm", linetype= "solid", size=1, se= FALSE)+
  geom_smooth(aes(y= new.k, col= factor(Temp.x)), method = "lm", size=1, se = FALSE)+
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
  scale_x_continuous(breaks = c(200,400,600,800,1000), limits = c(100,1050))+
  annotate("text", x=1025, y=0.025, label= "b)", size =6)+
  guides(fill=guide_legend(override.aes=list(size=6)))+
  theme_classic()+
  theme(legend.position =  "none")
dev.off() # This saves the file


tiff(filename = "O:\\FunCab\\Manuscripts\\TBI\\figures\\ProofFigures\\Figure3C.tif", width = 13, height = 8, units = "in", res = 500)
#Stabilization factor S
p7<- ggplot(TBI_variables, aes(gridTemp))+
  geom_point(aes(y= S, fill= factor(Temp.x) , shape = factor(Prec.x)), size= 3, width = 0.1)+
  geom_smooth(data= TBI_variables, aes(y = S, col = "black"), method = "lm", linetype= "solid", size=1, se= FALSE)+
  geom_smooth(data= subset(TBI_variables, Temp.x== 1), aes(y= S, col= factor(Temp.x)), method = "lm", size=1, linetype= "dashed",
              se = FALSE)+
  geom_smooth(data= subset(TBI_variables, Temp.x== 2 | Temp.x== 3), aes(y= S, col= factor(Temp.x)), method = "lm", size=1, 
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
  annotate("text", x=14.5, y=0.5, label= "c)", size =6)+
  guides(col=guide_legend(override.aes=list(size=5)))+
  theme_classic()+
  theme(legend.position = "none")
dev.off() # This saves the file

tiff(filename = "O:\\FunCab\\Manuscripts\\TBI\\figures\\ProofFigures\\Figure3D.tif", width = 12, height = 8, units = "in", res = 500)
# Stabiliation factor S vs Precipitation
p8<- ggplot(TBI_variables, aes(gridPrec))+
  geom_point(aes(y= S, fill= factor(Temp.x) , shape = factor(Prec.x)), size= 3, width = 0.1)+
  geom_smooth(data= TBI_variables, aes(y = S, col = "black"), method = "lm", linetype= "solid", size=1, se= FALSE)+
  geom_smooth(data= subset(TBI_variables, Temp.x== 1 | Temp.x== 2), aes(y= S, col= factor(Temp.x)), method = "lm", size=1, 
              se = FALSE)+
  geom_smooth(data= subset(TBI_variables, Temp.x==3), aes(y= S, col= factor(Temp.x)), method = "lm", linetype= "dashed", size=1, 
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
  scale_x_continuous(breaks = c(200,400,600,800,1000), limits = c(100,1050))+
  annotate("text", x=1025, y=0.5, label= "d)", size =6)+
  guides(fill=guide_legend(override.aes=list(size=6)))+
  theme_classic()+
  theme( legend.position = "none")
dev.off() # This saves the file


x11(height = 130/25.4, width = 200/25.4)
th2<- theme(axis.title = element_text(size = 15), axis.text = element_text(size = 13))

Figure3<-cowplot::plot_grid(p5 +th2 , p6 +th2 , p7+th2 , p8+th2, ncol=2, nrow = 2, align = "v")
cowplot::save_plot("O:\\FunCab\\Manuscripts\\TBI\\figures\\ProofFigures\\Figure3.eps", Figure3 , base_height = 10, base_width = 12)
