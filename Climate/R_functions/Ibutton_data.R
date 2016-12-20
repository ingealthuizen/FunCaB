# I-button data FunCaB plots
library(ggplot2)

read.ibutton<-function(file){                             
  ibut<-read.csv(file, header=FALSE, skip = 20, fill=TRUE)
  names(ibut)<-c("datetime", "C", "temp") #give names to columns
  ibut$C<-NULL # setting C column to null 
  ibut$datetime<- as.POSIXct(ibut$datetime, tz="", format="%m/%d/%y %I:%M:%S %p") #format for reading datetime
  #ibut$datetime<- format(ibut$datetime, format="%d.%m.%Y %H:%M:%S") #change datetime format
  ibut
}

ibut.data<-read.ibutton("P:\\Felles\\Funcab_Seedclim\\i-button loggers\\Lavisdalen\\3E3ABB41.csv")
plot(ibut.data)

ibut.data$day<-as.numeric(format(ibut.data$datetime, format="%d")) 
ibut.data$month<-as.numeric(format(ibut.data$datetime, format="%m")) 
ibut.data$year<-format(ibut.data$datetime, format="%Y") 

#? make start and stoptimes of Tea bag incubations >  calculate mean soil temperature with Ibutton data
# calc mean temp between start and stoptime
#meantemp<-function(ibut, start, stop){
#  temp<-approx(x=ibut$datetime, y=ibut$temp, xout = c(start, stop))$y
#  mean(temp)
#}

#meantemp(temp.data,start=temp.data$datetime[1],stop=temp.data$datetime[2])



x<-subset(ibut.data, year == "2015" & month>6 & month<11  )

ggplot(x, aes(datetime, temp))+
 geom_line()

mean(x$temp)
