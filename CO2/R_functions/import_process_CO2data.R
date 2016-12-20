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

#temp.data<-read.ibutton("CO2/Data/Temperature_files_2016/20160607_FAU_CH1_TEMP.txt")
#plot(temp.data)


# calc mean temp between start and stoptime
meantemp<-function(ibut, start, stop){
  temp<-approx(x=ibut$datetime, y=ibut$temp, xout = c(start, stop))$y
  mean(temp)
}

#meantemp(temp.data,start=temp.data$datetime[1],stop=temp.data$datetime[2])


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
  log <- log[, 1:5]# remove junk
  
  list(CO2=log[log$indicator==2, -1], PAR=log[log$indicator==1, -1], H2O=log[log$indicator==3, -1])
}

#load logger file and assign log.data to different parameters
#log.data<-read.logger("CO2/Data/Fluxdata2016_Li1400/20160607_FAU_LI1400_CH1_1.txt")
#CO2<-log.data$CO2
#PAR<-log.data$PAR
#H2O<-log.data$H2O

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

#meta.data<-read.metadata("CO2/Data/metadata_2016/07062016_FAU_ch1_1.txt")


#look for start and stoptimes of each measurement in metadata file within logger.data and temp.data
process.data <- function(meta, logger, temp){
  cleaner<-lapply(1:nrow(meta), function( i){
    
    metdat<-meta[i,]    
    if(!is.na(metdat$flag)&metdat$flag=="x" )return(NULL) # leave out measurements that are flagged "x"
    #linking start/stoptime from metadata to CO2, PAR and H2O data from logger  
    startCO2<-which(logger$CO2$datetime==metdat$starttime)#or next
    if(length(startCO2)==0){
      dif<-logger$CO2$datetime-metdat$starttime
      startCO2<-which.max(dif>0)
    }
    
    stopCO2 <- which(logger$CO2$datetime == metdat$stoptime)#or most recent
    #stopifnot(length(stopCO2)>0,length(stopCO2)>0)
    if (length(stopCO2) == 0) {
      dif <- logger$CO2$datetime - metdat$stoptime
      stopCO2 <- which.max(!dif < 0) - 1
      warning("taking most recent CO2 value")
    }
    
    co2 <- logger$CO2[startCO2:stopCO2, ]
    co2$time <- unclass(co2$datetime - co2$datetime[1])
    co2 <- rename(co2, CO2 = value)
    
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
    par <- rename(par, PAR = value)
    
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
    h2o <- rename(h2o, H2O = value)
    
    temp2<-meantemp(temp, metdat$starttime, metdat$stoptime)
    
    dat <- cbind(co2, PAR = par$PAR, H2O = mean(h2o$H2O), temp = temp2)
    
    list(dat = dat, meta=metdat)
  })
  cleaner[!vapply(cleaner, is.null, FUN.VALUE = TRUE)]    
}  

setStartEnd <- function(x){
    # unhach from here to try China code part
      startHappy <- FALSE
      endHappy <- FALSE
      while(!(startHappy & endHappy)){
        layout(matrix(c(1,1,1,2,2,2,2,2,2), nrow = 3, ncol = 3, byrow = TRUE))
        par(mar=c(4,5,2,2))
        plot.PAR(x)
        plot.CO2(x)
      
        
        #fit in China code for readjusting timeframe used for fitting line 
        tstart <- readline("Enter preferred start time for fitting. \n Round to nearest integer second. press 'return':")
        if(!grepl("^[0-9]+$", tstart)){
          tstart <- 0
          startHappy <- TRUE
        } else {
          tstart <- as.integer(tstart)
          startHappy <- FALSE
        }
        
        
        
        tfinish <- readline("Enter preferred finish time for fitting. \n Round to nearest integer second. \n  If default of 120s is preferred, press 'return':")
        if(!grepl("^[0-9]+$", tfinish)){
          tfinish <- Inf
          endHappy <- TRUE
        } else{
          tfinish <- as.integer(tfinish)
          endHappy <- FALSE
        }
        x$dat$keep[x$dat$time < tstart | x$dat$time > tfinish] <- FALSE
      }   
     
  x

}

setStartEndTimes <- function(xx){
  lapply(xx, setStartEnd)
}


# function to check if start and stoptime of CO2 is within timeframe specified in metadata
check<-function(x){
  CO2<-sapply(x, function(r){
    all(r$CO2$datetime>=r$meta$starttime&r$CO2$datetime<=r$meta$stoptime)
  })
  
  CO2
}
#check(combine.data) #check if start and stop time is right


# importing combination of datafiles of specific day site time
import.everything<-function(metaFile, loggerFile, tempFile){
  meta<-read.metadata(metaFile)
  logger<-read.logger(loggerFile)
  temp<- read.ibutton(tempFile)
  process.data(meta=meta, logger=logger, temp=temp)
}

#importing all site file combination from a sitefile
read.sitefiles<-function(file){
  sites<-read_excel(file, sheet=1, col_names=TRUE, col_type= NULL) #read excel file
  sites$dates<- as.Date(sites$dates, format="%d.%m.%y") 
  sites<-sites[!is.na(sites$site), ] # remove rows with no data
  #import data from files of site.files
  sites.data<-lapply(1:nrow(sites), function(i){
    r<-sites[i, ]
    #   print(r)
    import.everything(metaFile = r$meta, loggerFile = r$logger, tempFile = r$temp)
  }) #process data from all files
  unlist(sites.data, recursive = FALSE) # make on big list of data from all sites, without sublists
}
