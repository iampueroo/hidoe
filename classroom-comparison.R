classroom-comparison <- function(classroom1, startdate1, enddate1, classroom2, startdate2, enddate2, weatherstation) {
  library(plyr)
  library(reshape2)
  library(ggplot2)
  library(ggthemes) 
  library(scales)
  library(grid)
  library(gridExtra) 
  
  min_F <- 85
  colors <- c( "dimgrey", "#FC4F30", "#30A2DA")
  colorsHeat <- c("#E69720", "#FC4F30", "#E8E827") 
  stdt1 <- as.Date(startdate1, format="%Y-%m-%d")
  endt1 <- as.Date(enddate1, format="%Y-%m-%d")
  stdt2 <- as.Date(startdate2, format="%Y-%m-%d")
  endt2 <- as.Date(enddate2, format="%Y-%m-%d")
  
  # read in data ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
  arch <- read.csv(file="~/dropbox/rh1/hidoe/input-csv/classroom-features.csv", sep=",") #classroom architectural data
  
  o <- read.csv(file="~/dropbox/rh1/hidoe/output-csv/outdoor-master.csv", sep=",") #outdoor 
  o$Date <- as.Date(o$Date, format="%Y-%m-%d")
  o$Time <- format(strptime(o$Time, format="%H:%M"), format="%H:%M")
  o$Month <- factor(format(o$Date, "%B"), c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May", "June", "July"))
  o$Alias <- NULL #choose what weather station you want from Alias, but for now they're all the same so drop Alias
  
  cr <-read.csv(file="~/dropbox/rh1/hidoe/output-csv/classroom-master.csv", sep=",") #classroom
  cr$Date <- as.Date(cr$Date, format="%Y-%m-%d")
  cr$Time <- format(strptime(cr$Time, format="%H:%M"), format="%H:%M")
  cr$Month <- factor(format(cr$Date, "%B"), c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May", "June", "July"))
  cr <- merge(cr, arch[,c("RoomID", "Alias")], by="RoomID", all.x=TRUE) #get room alias 
  cr$SensorAlias <- as.character(cr$SensorAlias)
  cr <- cr[substr(ct$SensorAlias,nchar(cr$SensorAlias)-1,nchar(cr$SensorAlias))!="HD",]  #remove duplicate datetime...figure out why different
  cr1 <- cr[cr$Alias==classroom1,] #restrict to classroom specified
  cr2 <- cr[cr$Alias==classroom2,] #restrict to classroom specified
  
  
  # restrict to school hour/dates ------------------------------------------------------------------------------------------------------------------------------------------------------------------
  cr <- cr[cr$SchoolDate==1 & cr$SchoolHour==1,]
  o <- o[o$SchoolDate==1 & o$SchoolHour==1,]
  
  o.daily <- ddply(o,c("Date"), summarise, AvgOutdoorTemp=mean(Temp_F, na.rm=TRUE), AvgOutdoorUTCI=mean(UTCI_F, na.rm=TRUE)) #aggregated by date 
  cr.daily <- ddply(cr, c("Date", "Alias"), summarise, AvgRoomTemp=mean(Temp_F, na.rm=TRUE), MaxRoomTemp=max(Temp_F,na.rm=TRUE), AvgRoomUTCI=mean(UTCI_F, na.rm=TRUE), MaxRoomUTCI=max(UTCI_F, na.rm=TRUE)) #aggregated  by date
  
  o.daily <- melt(o.daily, id.vars=c("Date"))
  cr.daily <- melt(cr.daily, id.vars=c("Date"))
  cr.daily$level <- as.factor(ifelse(substr(as.character(cr.daily$variable), 1, 3)=="Avg", "Average", "Maximum"))
  o.daily$level <- "Outdoor"
  
}