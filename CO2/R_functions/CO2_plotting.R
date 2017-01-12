#code for plotting CO2
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
