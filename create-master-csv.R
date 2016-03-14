createmastercsv <- function() {
  library(plyr)
  
  #keep dates csv up to date with school days
  sdates <- read.csv(file="~/dropbox/rh1/hidoe/input-csv/school-days.csv", sep=",")  #school dates
  sdates$Date <- as.Date(sdates$Date, format="%m/%d/%y")
  shours <- read.csv(file="~/dropbox/rh1/hidoe/input-csv/school-hours.csv", sep=",")  #school hours
  shours$Time <- format(strptime(shours$Time, format="%H:%M"), format="%H:%M")
  
  #arch features - update everytime you have new data
  arch <- read.csv(file="~/dropbox/rh1/hidoe/input-csv/classroom-features.csv", sep=",") #classroom architectural data
  
  ### classsroom csv 2013-2014 -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #setwd("~/dropbox/rh1/hidoe/input-csv/classroom/13.07.20 - 14.10.20") 
  #cr <- do.call(rbind,lapply(dir(), read.csv)) #read all classroom sensor data
  #names(cr) <- c("RoomID", "SensorAlias", "StartTime", "Temp_F", "RH", "Ill")
  #cr$SensorAlias <- as.character(cr$SensorAlias)
  #cr <- cr[substr(cr$SensorAlias,nchar(cr$SensorAlias)-1,nchar(cr$SensorAlias))!="HD",] 
  #cr$SensorAlias <- NULL
  #cr.temp <- cr[!is.na(cr$Temp_F),c("RoomID", "StartTime", "Temp_F")] #pull out temp 
  #cr.rh<- cr[!is.na(cr$RH),c("RoomID", "StartTime", "RH")] #pull out RH
  #cr <- merge(cr.temp, cr.rh, by=c("RoomID", "StartTime")) #merge back together to compress data frame
  #cr$RoomWind <- 0
  #cr <- temp2utci(cr, "Temp_F", "f", "RH", "RoomWind", "mi/hr") #calculate UTCI
  #cr$Date <- as.Date(cr$StartTime, format="%m/%d/%y")  
  #cr$Time <- format(strptime(cr$StartTime, format="%m/%d/%y %H:%M"), format="%H:%M") 
  #cr$Month <- format(cr$Date, "%B")
  #cr <- cr[,c("RoomID", "Temp_F", "UTCI_F", "Date", "Time", "Month")]
  #cr$SchoolDate <- ifelse(cr$Date %in% sdates$Date, 1, 0)
  #cr$SchoolHour <- ifelse(cr$Time %in% shours$Time, 1, 0)

  #write.csv(cr, file="~/dropbox/rh1/hidoe/output-csv/classroom-master-2016-03-01.csv", row.names = FALSE, na="")
  
  ### outdoor csv 2013-2014 -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #o <- read.csv(file="~/dropbox/rh1/hidoe/input-csv/outdoor/13.07.20 - 14.10.20/HNEI_EwaWeather_Raw_Full.csv", sep=",") 
  #o$Date <- as.Date(o$Hawaii.Time...10.Hours., format="%m/%d/%y")
  #o$Time <- format(strptime(o$Hawaii.Time...10.Hours., format="%m/%d/%y %H:%M"), format="%H:%M")
  #o$Month <- format(o$Date, "%B")
  #o <- temp2utci(o, "Temp", "c", "RH", "Wind.Speed", "m/s") #calculate UTCI
  #o <- o[,c("Temp_F", "UTCI_F", "Date", "Time", "Month")]
  #o$SchoolDate <- ifelse(o$Date %in% sdates$Date, 1, 0)
  #o$SchoolHour <- ifelse(o$Time %in% shours$Time, 1, 0)
  #o$Alias <- "Ewa Elementary"
  
  #write.csv(o, file="~/dropbox/rh1/hidoe/output-csv/outdoor-master-2016-03-01.csv", row.names = FALSE, na="") 
  
  
  
  ### 03-01-2016 inital write to master -------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #write.csv(cr, file="~/dropbox/rh1/hidoe/output-csv/classroom-master.csv", row.names = FALSE, na="")
  #write.csv(o, file="~/dropbox/rh1/hidoe/output-csv/outdoor-master.csv", row.names = FALSE, na="") 
  
  
  
  ### outdoor csv 2015-2016 -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  o <- read.csv(file="~/dropbox/rh1/hidoe/input-csv/outdoor/15.10.02 - 16.02.29/KHIEWABE4_KaimiloaES.csv", sep=",", 
                col.names = c("WS_ID", "Alias",	"HST_Date",	"HST_Time",	"Temp_F",	"DewPt_F",	"RelH",	"Solar_W/ms",	"Wind_mi/hr",	"Gust_mi/hr",	"Rain_inches"))
  o$Date <- as.Date(o$HST_Date, format="%m/%d/%y")
  o$Time <- format(strptime(o$HST_Time, format="%H:%M"), format="%H:%M")
  o$Month <- format(o$Date, "%B")
  o <- temp2utci(o, "Temp_F", "f", "RelH", "Wind_mi.hr", "mi/hr") #calculate UTCI
  o <- o[,c("Temp_F", "UTCI_F", "Date", "Time", "Month", "Alias")]
  o$SchoolDate <- ifelse(o$Date %in% sdates$Date, 1, 0)
  o$SchoolHour <- ifelse(o$Time %in% shours$Time, 1, 0)
  
  write.csv(o, file="~/dropbox/rh1/hidoe/output-csv/archive/outdoor-master-2016-03-14.csv", row.names = FALSE, na="") 
  
  ### Campbell P2 2015-2016 -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  cr <- read.csv(file="~/dropbox/rh1/hidoe/input-csv/classroom/15.08.27 - 16.03.08/10372577_P2.csv", sep=",", skip=1, col.names=c("ID", "GMT", "HST", "Temp_F", "RH", "x1", "x2"))
  cr <- cr[!is.na(cr$Temp_F),] 
  cr$RoomWind <- 0
  cr <- temp2utci(cr, "Temp_F", "f", "RH", "RoomWind", "mi/hr") #calculate UTCI
  cr$Date <- as.Date(cr$HST, format="%m/%d/%y")  
  cr$Time <- format(strptime(cr$HST, format="%m/%d/%y %H:%M"), format="%H:%M") 
  cr$Month <- format(cr$Date, "%B")
  cr$RoomID <- "HIDOE301_P02_001"
  cr <- cr[,c("RoomID", "Temp_F", "UTCI_F", "Date", "Time", "Month")]
  cr$SchoolDate <- ifelse(cr$Date %in% sdates$Date, 1, 0)
  cr$SchoolHour <- ifelse(cr$Time %in% shours$Time, 1, 0)
  
  write.csv(cr, file="~/dropbox/rh1/hidoe/output-csv/archive/classroom-master-2016-03-14.csv", row.names = FALSE, na="") 
  
  
  #merge back with master -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  crmaster <- read.csv(file="~/dropbox/rh1/hidoe/output-csv/classroom-master.csv", sep=",")
  omaster <- read.csv(file="~/dropbox/rh1/hidoe/output-csv/outdoor-master.csv", sep=",")
  
  omaster <- rbind(o, omaster) #assume dates don't overlap
  crmaster <- rbind(cr, crmaster) #assume dates don't overlap
  
  write.csv(omaster, file="~/dropbox/rh1/hidoe/output-csv/outdoor-master.csv", row.names=FALSE, na="")
  write.csv(crmaster, file="~/dropbox/rh1/hidoe/output-csv/classroom-master.csv", row.names=FALSE, na="")
  

  
}


 







