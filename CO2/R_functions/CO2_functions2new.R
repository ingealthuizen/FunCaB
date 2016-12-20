require(ggplot2)
# import temperature data from Ibutton 
read.ibutton<-function(file){                             
  ibut<-read.csv(file, header=FALSE)
  names(ibut)<-c("datetime", "C", "temp", "temp2") #give names to columns
  ibut$temp<-ibut$temp+ibut$temp2/1000 # summing two temperature columns
  ibut$temp2<-NULL # setting temp2 column to null after it has been added to ibut$temp
  ibut$C<-NULL # setting C column to null 
  ibut$datetime<- as.POSIXct(ibut$datetime, tz="", format="%d.%m.%y %H:%M:%S") #right format for reading datetime
  
  ibut
}

temp.data<-read.ibutton("O:\\FunCab\\Data\\co2flux2016\\Temperature files\\20160729_SKJ_CH1_TEMP.txt")
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
  
  # leave out flagged values "x" (outliers) from 
  log$flag[is.na(log$flag)]<-""
  log$keep<-log$flag!="x"
  log<-log[log$indicator%in%c(1,2,3),]
  datetime<- as.POSIXct(log$datetime, tz="", format="%Y-%m-%d %H:%M:%S")
  if(all(is.na(datetime))){
    datetime<- as.POSIXct(log$datetime, tz="", format="%d.%m.%Y %H:%M:%S") 
    
  }
  log$datetime<-datetime
  
  list(CO2=log[log$indicator==2,2:5], PAR=log[log$indicator==1,2:5], H2O=log[log$indicator==3,2:5])
}

#load logger file and assign log.data to different parameters
log.data<-read.logger("O:\\FunCab\\Data\\co2flux2016\\Flux2016 Li1400\\20160729_SKJ_LI1400_CH1_1.txt")
CO2<-log.data$CO2
PAR<-log.data$PAR
H2O<-log.data$H2O

#plot(CO2$datetime, CO2$value)
#plot(PAR$datetime, PAR$value)
#plot(H2O$datetime, H2O$value)

# import metadata of site 
read.metadata<-function(file){
  metdat<-read.table(file, header=TRUE, fill=TRUE, stringsAsFactors = FALSE)
  names(metdat)<-c("date", "starttime", "stoptime", "chamber", "site", "block", "treatment", "cover", "airpress", "vegbiomass", "flag", "removal")
  metdat$starttime<- as.POSIXct(paste(metdat$date, metdat$starttime), tz="", format="%d.%m.%Y %H:%M:%S")
  metdat$stoptime<- as.POSIXct(paste(metdat$date, metdat$stoptime), tz="", format="%d.%m.%Y %H:%M:%S")
  #metdat$date<-NULL
  plot(metdat$starttime)#check
  points(metdat$stoptime, col=2)
  metdat
}

meta.data<-read.metadata("O:\\FunCab\\Data\\co2flux2016\\metadata files\\29072016_SKJ_ch1_1.txt")
meta.data

#look for start and stoptimes of each measurement in metadata file within logger.data and temp.data
process.data<-function(meta, logger, temp, plotme = FALSE){
  cleaner<-lapply(1:nrow(meta), function( i){
    #startoffset <- 0
    #nothappytime <- TRUE
    #while(nothappytime){
      
      metdat<-meta[i,]    
      if(!is.na(metdat$flag)&metdat$flag=="x" )return(NULL) # leave out measurements that are flagged "x"
      #linking start/stoptime from metadata to CO2, PAR and H2O data from logger  
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
      
      starth2o<-which(logger$H2O$datetime==metdat$starttime)#or next
      if(length(starth2o)==0){
        dif<-logger$H2O$datetime-metdat$starttime
        starth2o<-which.max(dif>0)
      }
      stoph2o<-which(logger$H2O$datetime==metdat$stoptime)#or most recent
      if(length(stoph2o)==0){
        dif<-logger$H2O$datetime-metdat$stoptime
        stoph2o<- which.max(!dif<0)-1
        warning("taking most recent H2O value")
      }
      
      h2o<-logger$H2O[starth2o:stoph2o,]
      
      temp2<-meantemp(temp, metdat$starttime, metdat$stoptime)
      
      
      x <- list(CO2=co2, PAR=par, H2O=h2o, temp=temp2, meta=metdat)
      # unhach from here to try China code part
      #if(plotme) {
      #  layout(matrix(c(1,1,1,2,2,2,2,2,2), nrow = 3, ncol = 3, byrow = TRUE))
      #  par(mar=c(4,5,2,2))
      #  plot.PAR(x)
      #  plot.CO2(x)
    
      #fit in China code for readjusting timeframe used for fitting line 
      #tstart <- readline("Enter preferred start time for fitting. \n Round to   nearest integer second. \n Do not use 0. \n  If default of 5s is preferred, press 'return':")
     # if(!grepl("^[0-9]+$", tstart)){
     #   tstart <- 5
     # }
     # tstart <- as.integer(tstart)  
      
     # tfinish <- readline("Enter preferred finish time for fitting. \n Round to nearest integer second. \n  If default of 120s is preferred, press 'return':")
     # if(!grepl("^[0-9]+$", tfinish)){
     #   tfinish <- 120
     # }
     # tfinish <- as.integer(tfinish)
      
      # after defining new time frame plot linear.fit to data
      ## Linear fitting code.
      #linear.fit <- lm(cprime[tstart:tfinish]~(time[tstart:tfinish]))
      
      # Calculate intercept
      #inter<- as.numeric(linear.fit$coeff[1])
      
      # Calculate slope
      #dcdt <- as.numeric(linear.fit$coeff[2])
      
      # Make plot of line.
      #abline(inter,dcdt, col=6)
      
      
    #} else{
     #   nothappytime <- FALSE
    #  }
     # x
   # }
  })
  
  cleaner[!vapply(cleaner, is.null, FUN.VALUE = TRUE)]  
}


#specifying data used in function process.data
#plotme == TRUE will create plots for every measurement specified in proces.data by start and stoptime in meta.data
combine.data<-process.data(meta=meta.data, logger=log.data, temp=temp.data, plotme = TRUE)
combine.data

#
#?Have output of a file with start and stoptime for all measurement and use this the further calculate flux data


# function to check if start and stoptime of CO2 is within timeframe specified in metadata
check<-function(x){
  CO2<-sapply(x, function(r){
    all(r$CO2$datetime>=r$meta$starttime&r$CO2$datetime<=r$meta$stoptime)
  })
  
  CO2
}
check(combine.data) #check if start and stop time is right

#function for importing datafiles to be used by the function process.data
import.everything<-function(metaFile, loggerFile, tempFile){
  meta<-read.metadata(metaFile)
  logger<-read.logger(loggerFile)
  temp<- read.ibutton(tempFile)
  process.data(meta=meta, logger=logger, temp=temp)
}

#function for plotting CO2 against time and adding regression line
plot.CO2<-function(x, ...){#browser()
  x$CO2$time<-unclass(x$CO2$datetime-x$CO2$datetime[1])
  plot(x$CO2$time, x$CO2$value, main=paste(unlist(x$meta)[(5:8)], collapse = " "), pch=ifelse(x$CO2$keep, yes= 16, no=1), ...) #xlab="time (s)", ylab="CO2 (ppm)", cex.lab=1.5
  CO2cleaned.lm<- lm(value~time, data=x$CO2, subset=keep)
  CO2raw.lm<- lm(value~time, data=x$CO2)
  abline(CO2raw.lm, col="gray")
  abline(CO2cleaned.lm, col="blue")
  CO2cleaned.lm
}


plot.PAR<-function(x, ...){#browser()
  x$PAR$time<-unclass(x$PAR$datetime-x$PAR$datetime[1])
  plot(x$PAR$time, x$PAR$value, xaxt='n', ann=FALSE)
}

#layout(matrix(c(1,1,1,2,2,2,2,2,2), nrow = 3, ncol = 3, byrow = TRUE))
#par(mar=c(4,5,2,2))
plot.PAR(combine.data[[1]])
plot.CO2(combine.data[[1]])



#plot all measurements in same window
  sapply(combine.data, function(i)coef(plot.CO2(i))[2])
  x11(width=13, height=8)
  par(mfrow=c(10,7), mar=c(.5,3,1,1), mgp=c(1.5,.5,0))
  sapply(combine.data, function(i)coef(plot.CO2(i, xlab="", xaxt="n"))[2])
  axis(1)


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
  h2O<-8 #mean(input$H2O$value) estimated value mmol/mol
  PAR<-mean(input$PAR$value)
  press<- input$meta$airpress # kPA estimated value based on altitude site
  temp<- input$temp # C 
  
  #ambient
  cprime<-co2/(1-(h2O/1010))
  
  # linear Regression
  CO2.lm<- lm(cprime~time, subset= input$CO2$keep)
  inter<- coef(CO2.lm)[1]
  dcdt<- coef(CO2.lm)[2]
  rsqd<- summary(CO2.lm)$r.sq
  nee<- -((vol*press)*(1010-h2O)*dcdt/ (R*area*(temp+273.15))) # D(Reco)= negative L(NEE)=positive
  
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