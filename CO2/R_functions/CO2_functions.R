# import temperature data from Ibutton 
read.ibutton<-function(file){                             
  x<-read.csv(file, header=FALSE)
  names(x)<-c("datetime", "C", "temp", "temp2")
  x$temp<-x$temp+x$temp2/1000
  x$temp2<-NULL
  x$C<-NULL
  x$datetime<- as.POSIXct(x$datetime, tz="", format="%d-%m-%y %H:%M:%S")
  
  x
  }

z<-read.ibutton("\\\\eir.uib.no\\home6\\ial008\\FunCab\\Data\\CO2 flux\\temperature CO2 flux\\05-07-15_ovstedal&arhelleren.txt")
plot(z)

# calc mean temp between start and stoptime
meantemp<-function(x, start, stop){
  temp<-approx(x=x$datetime, y=x$temp, xout = c(start, stop))$y
  mean(temp)
}

meantemp(z,start=z$datetime[1],stop=z$datetime[2])

# import CO2 and PAR data from datalogger 
read.logger<-function(file){
  x<-read.table(file, header=FALSE, skip = 12, fill=TRUE, sep="\t", stringsAsFactors = FALSE)
  if(ncol(x)==3){  
    names(x)<-c("indicator", "datetime", "value")
    x$flag<-""
  }else if(ncol(x)>3){
    x<-x[,1:4]
    names(x)<-c("indicator", "datetime", "value", "flag")
  } 
  x<-x[x$indicator%in%c(1,2),]
  x$datetime<- as.POSIXct(x$datetime, tz="", format="%Y-%m-%d %H:%M:%S")
  
  list(CO2=x[x$indicator==2,2:4], PAR=x[x$indicator==1,2:4])
}

z2<-read.logger( "\\\\eir.uib.no\\home6\\ial008\\FunCab\\Data\\CO2 flux\\liCOR\\processed\\05-07-15_arhelleren.txt")
CO2<-z2$CO2
PAR<-z2$PAR

plot(CO2$datetime, CO2$value)
plot(PAR$datetime, PAR$value)

# import metadata of site 
read.metadata<-function(file){
  x<-read.table(file, header=TRUE, fill=TRUE, stringsAsFactors = FALSE)
  names(x)<-c("date", "starttime", "stoptime", "site", "block", "treatment", "cover")
  x$starttime<- as.POSIXct(paste(x$date, x$starttime), tz="", format="%d.%m.%Y %H:%M:%S")
  x$stoptime<- as.POSIXct(paste(x$date, x$stoptime), tz="", format="%d.%m.%Y %H:%M:%S")
  x$date<-NULL
  plot(x$starttime)#check
  points(x$stoptime, col=2)
  x
}

z3<-read.metadata("\\\\eir.uib.no\\home6\\ial008\\FunCab\\Data\\CO2 flux\\liCOR\\data files txt\\2015-07-05 Arhelleren pre.txt")
z3




process.data<-function(meta, logger, temp){
 lapply(1:nrow(meta), function( i){
   z<-meta[i,]    

  
    startCO2<-which(logger$CO2$datetime==z$starttime)#or next
    if(length(startCO2)==0){
      dif<-logger$CO2$datetime-z$starttime
      startCO2<-which.max(dif>0)
    }
  
    stopCO2<-which(logger$CO2$datetime==z$stoptime)#or most recent
    #stopifnot(length(stopCO2)>0,length(stopCO2)>0)
    if(length(stopCO2)==0){
      dif<-logger$CO2$datetime-z$stoptime
      stopCO2<- which.max(!dif<0)-1
      warning("taking most recent CO2 value")
    }
    
    co2<-logger$CO2[startCO2:stopCO2,]
    
    
    startpar<-which(logger$PAR$datetime==z$starttime)#or next
    if(length(startpar)==0){
      dif<-logger$PAR$datetime-z$starttime
      startpar<-which.max(dif>0)
    }
    stoppar<-which(logger$PAR$datetime==z$stoptime)#or most recent
    if(length(stoppar)==0){
      dif<-logger$PAR$datetime-z$stoptime
      stoppar<- which.max(!dif<0)-1
      warning("taking most recent PAR value")
    }

    par<-logger$PAR[startpar:stoppar,]
    
    temp2<-meantemp(temp, z$starttime, z$stoptime)
    
    
    list(CO2=co2, PAR=par, temp=temp2, meta=z)
  })
  
}

z4<-process.data(meta=z3, logger=z2, temp=z)
z4

check<-function(x){
  CO2<-sapply(x, function(r){
    all(r$CO2$datetime>=r$meta$starttime&r$CO2$datetime<=r$meta$stoptime)
  })
  
  CO2
}
check(z4)

import.everything<-function(metaFile, loggerFile, tempFile){
  meta<-read.metadata(metaFile)
  logger<-read.logger(loggerFile)
  temp<- read.ibutton(tempFile)
  process.data(meta=meta, logger=logger, temp=temp)
}

plot.data<-function(x, ...){#browser()
  x$CO2$time<-unclass(x$CO2$datetime-x$CO2$datetime[1])
  plot(x$CO2$time, x$CO2$value, main=paste(unlist(x$meta)[-(1:2)], collapse = " "),...)
  CO2.lm<- lm(value~time, data=x$CO2)#subset=flag!="x")
  abline(CO2.lm, col="red")
  CO2.lm
}
plot.data(z4[[1]])
plot.data(z4[[2]])

sapply(z4, function(i)coef(plot.data(i))[2])
x11(width=13, height=8)
par(mfrow=c(10,7), mar=c(.5,3,1,1), mgp=c(1.5,.5,0))
sapply(z4, function(i)coef(plot.data(i, xlab="", xaxt="n"))[2])
axis(1)

