CreateCSV <- function() {
  library(plyr)
  
  #rerun with additional data - may need to add relevant section for particular preprocessing steps
  
  #keep dates csv up to date with school days
  sdates <- read.csv(file="~/dropbox/rh1/hidoe/input-csv/school-days.csv", sep=",")  #school dates
  sdates$Date <- as.Date(sdates$Date, format="%m/%d/%y")
  shours <- read.csv(file="~/dropbox/rh1/hidoe/input-csv/school-hours.csv", sep=",")  #school hours
  shours$Time <- format(strptime(shours$Time, format="%H:%M"), format="%H:%M")
  
  #arch features - update everytime you have new data
  arch <- read.csv(file="~/dropbox/rh1/hidoe/input-csv/classroom-features.csv", sep=",") #classroom architectural data
  archKeepVars <- c("RoomID", "Alias", "Floor", "RoofColor", "SQFT", "Orientation", "Landscape", "Overhang", "School", "AC")
  
  ### classsroom csv 2013-2014
  setwd("~/dropbox/rh1/hidoe/input-csv/classroom/13.07.20 - 14.10.20") 
  cr <- do.call(rbind,lapply(dir(), read.csv)) #read all classroom sensor data
  names(cr) <- c("RoomID", "SensorAlias", "StartTime", "Temp_F", "RH", "Ill")
  cr.temp <- cr[!is.na(cr$Temp_F),c("RoomID", "StartTime", "Temp_F", "SensorAlias")] #pull out temp
  cr.rh<- cr[!is.na(cr$RH),c("RoomID", "StartTime", "RH")] #pull out RH
  cr <- merge(cr.temp, cr.rh, by=c("RoomID", "StartTime")) #merge back together to compress data frame
  cr$StartTime <- as.character(cr$StartTime)
  cr$RoomWind <- 0
  cr <- TemperatureToUTCI(cr, "Temp_F", "f", "RH", "RoomWind", "mi/hr") #calculate UTCI
  cr$Date <- as.Date(cr$StartTime, format="%m/%d/%y")  
  cr$Time <- format(strptime(cr$StartTime, format="%m/%d/%y %H:%M"), format="%H:%M") 
  cr$Month <- factor(format(cr$Date, "%B"), c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May"))
  cr <- cr[,c("RoomID", "Temp_F", "UTCI_F", "Date", "Time", "Month", "SensorAlias")]
  cr <- merge(cr, sdates, by="Date", all.x=FALSE) #restrict to school days
  cr <- merge(cr, shours, by="Time", all.x=FALSE) #restrict to school hours 
  
  ### outdoor csv 2013-2014
  o <- read.csv(file="~/dropbox/rh1/hidoe/input-csv/outdoor/13.07.20 - 14.10.20/HNEI_EwaWeather_Raw_Full.csv", sep=",") 
  o$Date <- as.Date(o$Hawaii.Time...10.Hours., format="%m/%d/%y")
  o$Time <- format(strptime(o$Hawaii.Time...10.Hours., format="%m/%d/%y %H:%M"), format="%H:%M")
  o$Month <- factor(format(o$Date, "%B"), c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May"))
  o <- TemperatureToUTCI(o, "Temp", "c", "RH", "Wind.Speed", "m/s") #calculate UTCI
  o <- o[,c("Temp_F", "UTCI_F", "Date", "Time", "Month")]
  o <- merge(o, sdates, by="Date", all.x=FALSE) #restrict to school days
  o <- merge(o, shours, by="Time", all.x=FALSE) #restrict to school hours 
  o$Alias <- "Ewa Elementary"
  
  
  
  
  #merge back with master
  #write to all-classroom/all-outdoor
  write.csv(cr, file="~/dropbox/rh1/hidoe/output-csv/classroom-master.csv", row.names = FALSE, na="")
  write.csv(o, file="~/dropbox/rh1/hidoe/output-csv/outdoor-master.csv", row.names = FALSE, na="")
}











