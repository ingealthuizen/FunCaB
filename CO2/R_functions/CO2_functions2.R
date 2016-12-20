# import temperature data from Ibutton 
read.ibutton<-function(file){                             
  ibut<-read.csv(file, header=FALSE)
  names(ibut)<-c("datetime", "C", "temp", "temp2") #give names to columns
  ibut$temp<-ibut$temp+ibut$temp2/1000 # summing two temperature columns
  ibut$temp2<-NULL # setting temp2 column to null after it has been added to ibut$temp
  ibut$C<-NULL # setting C column to null 
  ibut$datetime<- as.POSIXct(ibut$datetime, tz="", format="%d-%m-%y %H:%M:%S") #right format for reading datetime
  
  ibut
  }

temp.data<-read.ibutton("\\\\eir.uib.no\\home6\\ial008\\FunCab\\Data\\CO2 flux\\temperature CO2 flux\\05-07-15_ovstedal&arhelleren.txt")
#plot(temp.data)

# calc mean temp between start and stoptime
meantemp<-function(ibut, start, stop){
  temp<-approx(x=ibut$datetime, y=ibut$temp, xout = c(start, stop))$y
  mean(temp)
}

meantemp(temp.data,start=temp.data$datetime[1],stop=temp.data$datetime[2])

# import CO2 and PAR data from datalogger 
read.logger<-function(file){
  log<-read.table(file, header=FALSE, skip = 12, fill=TRUE, sep="\t", stringsAsFactors = FALSE, col.names=c("indicator", "datetime", "value", "flag"))
  log$value<-suppressWarnings(as.numeric(log$value))
  log<-log[!is.na(log$value),] #skips data is not numeric
  
#  if(ncol(log)==3){  
#    names(log)<-c("indicator", "datetime", "value")
#    log$flag<-""
#  }else if(ncol(log)>3){
#    log<-log[,1:4]
 #   names(log)<-c("indicator", "datetime", "value", "flag")
#  } 
  log$flag[is.na(log$flag)]<-""
  log$keep<-log$flag!="x"
  log<-log[log$indicator%in%c(1,2),]
  datetime<- as.POSIXct(log$datetime, tz="", format="%Y-%m-%d %H:%M:%S")
  if(all(is.na(datetime))){
    datetime<- as.POSIXct(log$datetime, tz="", format="%d.%m.%Y %H:%M:%S") 
    
  }
  log$datetime<-datetime
  
  list(CO2=log[log$indicator==2,2:5], PAR=log[log$indicator==1,2:5])
}

log.data<-read.logger("O:\\FunCab\\Data\\CO2 flux\\liCOR\\processed\\05-07-15_arhelleren_cleaned.txt")
CO2<-log.data$CO2
PAR<-log.data$PAR

#plot(CO2$datetime, CO2$value)
#plot(PAR$datetime, PAR$value)

# import metadata of site 
read.metadata<-function(file){
  metdat<-read.table(file, header=TRUE, fill=TRUE, stringsAsFactors = FALSE)
  names(metdat)<-c("date", "starttime", "stoptime", "site", "block", "treatment", "cover", "airpress", "vegbiomass", "flag", "removal")
  metdat$starttime<- as.POSIXct(paste(metdat$date, metdat$starttime), tz="", format="%d.%m.%Y %H:%M:%S")
  metdat$stoptime<- as.POSIXct(paste(metdat$date, metdat$stoptime), tz="", format="%d.%m.%Y %H:%M:%S")
  metdat$date<-NULL
  plot(metdat$starttime)#check
  points(metdat$stoptime, col=2)
  metdat
}

meta.data<-read.metadata("O:\\FunCab\\Data\\CO2 flux\\liCOR\\data files txt\\2015-07-05 Arhelleren pre cleaned.txt")
meta.data


process.data<-function(meta, logger, temp){
cleaner<-lapply(1:nrow(meta), function( i){
   metdat<-meta[i,]    
   if(!is.na(metdat$flag)&metdat$flag=="x" )return(NULL) 
#linking start/stoptime metadata to CO2 and PAR data from logger  
    startCO2<-which(logger$CO2$datetime==metdat$starttime)#or next
    if(length(startCO2)==0){
      dif<-logger$CO2$datetime-metdat$starttime
      startCO2<-which.max(dif>0)
    }
  
    stopCO2<-which(logger$CO2$datetime==metdat$stoptime)#or most recent
    #stopifnot(length(stopCO2)>0,length(stopCO2)>0)
    if(length(stopCO2)==0){
      dif<-logger$CO2$datetime-metdat$stoptime
      stopCO2<- which.max(!dif<0)-1
      warning("taking most recent CO2 value")
    }
    
    co2<-logger$CO2[startCO2:stopCO2,]
    
    
    startpar<-which(logger$PAR$datetime==metdat$starttime)#or next
    if(length(startpar)==0){
      dif<-logger$PAR$datetime-metdat$starttime
      startpar<-which.max(dif>0)
    }
    stoppar<-which(logger$PAR$datetime==metdat$stoptime)#or most recent
    if(length(stoppar)==0){
      dif<-logger$PAR$datetime-metdat$stoptime
      stoppar<- which.max(!dif<0)-1
      warning("taking most recent PAR value")
    }

    par<-logger$PAR[startpar:stoppar,]
    
    temp2<-meantemp(temp, metdat$starttime, metdat$stoptime)
    
    
    list(CO2=co2, PAR=par, temp=temp2, meta=metdat)
  })

cleaner[!vapply(cleaner, is.null, FUN.VALUE = TRUE)]  
}

combine.data<-process.data(meta=meta.data, logger=log.data, temp=temp.data)
combine.data

check<-function(x){
  CO2<-sapply(x, function(r){
    all(r$CO2$datetime>=r$meta$starttime&r$CO2$datetime<=r$meta$stoptime)
  })
  
  CO2
}
check(combine.data) #check if start and stop time is right

import.everything<-function(metaFile, loggerFile, tempFile){
  meta<-read.metadata(metaFile)
  logger<-read.logger(loggerFile)
  temp<- read.ibutton(tempFile)
  process.data(meta=meta, logger=logger, temp=temp)
}

plot.data<-function(x, ...){#browser()
  x$CO2$time<-unclass(x$CO2$datetime-x$CO2$datetime[1])
  plot(x$CO2$time, x$CO2$value, main=paste(unlist(x$meta)[(3:6)], collapse = " "), pch=ifelse(x$CO2$keep, yes= 16, no=1),...)
  CO2cleaned.lm<- lm(value~time, data=x$CO2, subset=keep)
  CO2raw.lm<- lm(value~time, data=x$CO2)
  abline(CO2raw.lm, col="gray")
  abline(CO2cleaned.lm, col="blue")
  CO2cleaned.lm
}


plot.PAR<-function(x, ...){#browser()
  x$PAR$time<-unclass(x$PAR$datetime-x$PAR$datetime[1])
  plot(x$PAR$time, x$PAR$value, main=paste(unlist(x$meta)[(3:6)], collapse = " ", pch=ifelse(x$PAR$keep, yes= 16, no=1)),...)
}

#  plot.data(combine.data[[1]])
# plot.data(combine.data[[2]])

#  sapply(combine.data, function(i)coef(plot.data(i))[2])
#  x11(width=13, height=8)
# par(mfrow=c(10,7), mar=c(.5,3,1,1), mgp=c(1.5,.5,0))
# sapply(combine.data, function(i)coef(plot.data(i, xlab="", xaxt="n"))[2])
# axis(1)


#flux calculation

fluxcalc<-function(input){

  # constants specification for chamber
  
  height= 0.4 #m
  area= 0.25*0.25 #m^2
  vol= area*height # m^3
  R= 8.31442 # m^3 Pa/K/mol

  # flux
  time<-input$CO2$datetime
  time<-time-time[1]
  time<-unclass(time)
  co2<-input$CO2$value # umol/mol
  h2O<-8 # estimated value mmol/mol
  PAR<-mean(input$PAR$value)
  press<- input$meta$airpress # kPA estimated value based on altitude site
  temp<- input$temp # C 

  #ambient
  cprime<-co2/(1-(h2O/1000))

  # linear Regression
  CO2.lm<- lm(cprime~time, subset= input$CO2$keep)
  inter<- coef(CO2.lm)[1]
  dcdt<- coef(CO2.lm)[2]
  rsqd<- summary(CO2.lm)$r.sq
  nee<- -((vol*press)*(1000-h2O)*dcdt/ (R*area*(temp+273.15))) # D(Reco)= negative L(NEE)=positive
  
#	 Non-Linear, Exponential Regression (Leaky Fit Model) 
#  cnot = cprime[3]#almost certainly wrong
#  warning("cnot probably wrong")
# uptake.fm <- nls(cprime ~ (cnot - A)*exp(-time/B) + A, start=list(A=375, B=40), subset=time>use[1] & time<use[2]) #(A=375, B=40)
#  Css = summary(uptake.fm)$param[1]
#  tau = summary(uptake.fm)$param[2]
  
  
 # nee_exp <- ((camb-Css)/(area*tau))*(vol*pav*(1000-wav)/(R*(tav + 273.15))) #equation 3 in Saleska 1999
 # temp4 <- (cnot-Css)*exp(-time/tau) + Css #equation 4 in Saleska 1999
 # lines(time,temp4, col=4)
  #	   nee_exp
  
  cbind(input$meta, PAR=PAR, temp=temp, nee=nee, rsqd=rsqd)
}
# make table of metadata and flux

#import all datafiles
#read.sitefiles<-function(file){
#  require(readxl) #install package
# sites<-read_excel(file, sheet=1, col_names=TRUE, col_type= NULL) #read excel file
#  sites$dates<- as.Date(sites$dates, format="%d.%m.%y") 
#  sites<-sites[!is.na(sites$site), ] # remove rows with no data
  #import data from files of site.files
#  sites.data<-lapply(1:nrow(sites), function(i){
#    r<-sites[i, ]
#    # print(r)
#    import.everything(metaFile = r$meta, loggerFile = r$logger, tempFile = r$temp)
#  }) #process data from all files
#  unlist(sites.data, recursive = FALSE) # make on big list of data from all sites, without sublists
#}

#sites.data<-read.sitefiles("\\\\eir.uib.no\\home6\\ial008\\FunCab\\Data\\CO2 flux\\RcodeCO2\\sitefiles.xlsx")

#fluxcalc(sites.data[[1]]) #calculate flux 1 plot
#overviewsitesdata<-do.call(rbind, lapply(sites.data,fluxcalc)) #calculate flux all plots in all sites.
