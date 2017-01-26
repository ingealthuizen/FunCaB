# Climate data ibutton TBI
library(readxl) #require packages
library(dplyr) 
library(tidyr) 
library(ggplot2)
library(reshape2)
library(stringr)

# load data ibutton TBI2014
TBI_ibut2014<-read_excel("O:/FunCab/Data/FunCaB/Climate/Data/ibutton/TBI_ibut2014.xlsx")
TBI_ibut2014$day<-format(TBI_ibut2014$datetime, "%d") 
TBI_ibut2014$month<-format(TBI_ibut2014$datetime, "%m") 
TBI_ibut2014$year<-format(TBI_ibut2014$datetime, "%Y") 

# Calculate average soilTemp per day for each logger
TBI_day_sT<-TBI_ibut2014 %>%
            group_by(day, month, year) %>%
            summarise_each(funs(mean(., na.rm =TRUE))) %>%
            arrange(month)

# rounding of temperature values to 2 dec and removing datetime column
is.num <- sapply(TBI_day_sT, is.numeric)
  TBI_day_sT[is.num] <- lapply(TBI_day_sT[is.num], round, 2)
  TBI_day_sT<- TBI_day_sT[ -c(4)]

# Transforming data into long format and renaming variables > logger and splitting them in site and block
Day_sT<- melt(TBI_day_sT, id.vars = c("month", "day", "year"))
  Day_sT$date<- as.Date(with(Day_sT, paste(day, month, year, sep = "-")), "%d-%m-%Y")

Day_sT<-rename(Day_sT, logger = variable)
  Day_sT$logger<- as.character(Day_sT$logger)

site<-as.data.frame(do.call(rbind, strsplit(Day_sT$logger, "-")))
  Day_sT<- bind_cols(Day_sT, site)
  Day_sT<-rename(Day_sT, Site = V1, Block = V2)

# calculate average soilTemp acros site based on the different ibutton loggers
grid_sT<-Day_sT %>%
          group_by(day, month, year, date, Site) %>%
          summarise(mn_sT = mean(value, na.rm =TRUE)) %>%
          arrange(Site, month)


#plot soilTemp from all loggers over incubation time
ggplot(Day_sT, aes(date, value, col= variable))+
  geom_line()

#plot mean soilTemp at site over incubation time
ggplot(grid_sT, aes(date, mn_sT, col= Site))+
  geom_line()
