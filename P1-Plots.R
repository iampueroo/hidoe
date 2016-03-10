library(plyr)
library(reshape2)
library(ggplot2)
library(ggthemes) 
library(scales)
library(grid)
library(gridExtra) 
library(gtable)

colors <-c("#CC6666", "#9999CC", "#66CC99")
orderedMonths <- c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May")

sdates <- read.csv(file="~/dropbox/rh1/hidoe/day.csv", sep=",")  #school dates
sdates$Date <- as.Date(sdates$rd, format="%m/%d/%y")
sdates <- sdates[-c(1)]
shours <- read.csv(file="~/dropbox/rh1/hidoe/hour.csv", sep=",")  #school hours
shours$Time <- format(strptime(shours$Hour, format="%H:%M"), format="%H:%M")
shours$Hour <- NULL

onew <- read.csv(file="~/desktop/EwaWeather_2015.csv", sep=",")
onew$Date <- as.Date(onew$Time, format="%m/%d/%y")
onew$Month <- format(onew$Date, "%B")
onew$StartTime <- as.POSIXct(onew$Time, format="%m/%d/%y %H:%M")
onew$Time <- format(strptime(onew$Time, format="%m/%d/%y %H:%M"), format="%H:%M")
onew$Temp <- onew$TemperatureF
onew <- onew[,c("Time", "Temp", "Date", "Month", "StartTime")]
onew$Day <- format(onew$Date, "%m/%d")

oold <- read.csv(file="~/desktop/EwaWeather_2013-2014.csv", sep=",")
oold$Date <- as.Date(oold$DateTime, format="%m/%d/%y")
oold$Month <- format(oold$Date, "%B")
oold$StartTime <- as.POSIXct(oold$DateTime, format="%m/%d/%y %H:%M")
oold$Time <- format(strptime(oold$DateTime, format="%m/%d/%y %H:%M"), format="%H:%M")
oold$Temp <- oold$Temp_F
oold <- oold[,c("Time", "Temp", "Date", "Month", "StartTime")]
oold$Day <- format(oold$Date, "%m/%d")

p1new <- read.csv(file="~/desktop/HIDOE_8_Sensor_CampbellP002_2015.csv", sep=",")
p1new$Date <- as.Date(p1new$DateTime, format="%m/%d/%y")
p1new$Month <- format(p1new$Date, "%B")
p1new$StartTime <- as.POSIXct(p1new$DateTime, format="%m/%d/%y %H:%M")
p1new$Time <- format(strptime(p1new$DateTime, format="%m/%d/%y %H:%M"), format="%H:%M")
p1new <- p1new[,c("Time", "Temp", "Date", "Month", "StartTime")]
p1new$Day <- format(p1new$Date, "%m/%d")

p1old <- read.csv(file="~/desktop/HIDOE_8_Sensor_CampbellP002_2013-2014.csv", sep=",")
p1old$Sensor.ID..name. <- as.character(p1old$Sensor.ID..name.)
p1old <- p1old[(substr(p1old$Sensor.ID..name.,nchar(p1old$Sensor.ID..name.)-1,nchar(p1old$Sensor.ID..name.))!="HD"),] #remove non 15 minute intervals
p1old$Date <- as.Date(p1old$DateTime, format="%m/%d/%y")
p1old$Month <- format(p1old$Date, "%B")
p1old$StartTime <- as.POSIXct(p1old$DateTime, format="%m/%d/%y %H:%M")
p1old$Time <- format(strptime(p1old$DateTime, format="%m/%d/%y %H:%M"), format="%H:%M")
p1old$Temp <- p1old$Temp..F
p1old <- p1old[,c("Time", "Temp", "Date", "Month", "StartTime")]
p1old <- p1old[!is.na(p1old$Temp),]
p1old$Day <- format(p1old$Date, "%m/%d")

p1new <- p1new[complete.cases(p1new),]
p1old <- p1old[complete.cases(p1old),]
onew <- onew[complete.cases(onew),]
oold <- oold[complete.cases(oold),]

p1row <- rbind(p1new, p1old)
otrow <- rbind(onew, oold)
p1o <- merge(otrow, p1row, by=c("Time", "Date", "Month", "StartTime", "Day"), suffixes=c("Out", "In"), all.x=TRUE, all.y=TRUE)
p1o <- melt(p1o, id.vars=c("Time", "Date", "Month", "StartTime", "Day"))

ggplot() + 
  geom_hline(yintercept=85, color="black", linetype="dotted", size=0.5) +
  geom_line(data=oold, aes(x=StartTime, y=Temp), color="dimgrey", size=0.2, alpha=0.5) +
  geom_line(data=onew, aes(x=StartTime, y=Temp), color="dimgrey", size=0.2, alpha=0.5) +
  geom_line(data=p1old, aes(x=StartTime, y=Temp), color="#CC6666", size=0.2, alpha=0.5) +
  geom_line(data=p1new, aes(x=StartTime, y=Temp), color="#CC6666", size=0.2, alpha=0.5) +
  geom_smooth(data=p1o, aes(x=StartTime, y=value, color=variable), fill=NA) +
  scale_x_datetime(breaks=date_breaks("1 month"), labels=date_format("%b %Y")) +
  scale_y_continuous(breaks=seq(60,100,5), limits=c(60,100)) +
  scale_color_manual(name="", labels=c("Outdoor", "Classroom"), values=c("dimgrey", "#CC6666")) +
  ylab("Temperature (°F)") + xlab("Time") + ggtitle("Campbell P01: Temperature Profile") +
  theme_fivethirtyeight() +
  theme(text=element_text(size=9), legend.title=element_blank())


p1new <- merge(p1new, sdates, by="Date", all.x=FALSE) #restrict to school days
p1old <- merge(p1old, sdates, by="Date", all.x=FALSE) #restrict to school days
p1new <- merge(p1new, shours, by="Time", all.x=FALSE) #restrict to school hours
p1old <- merge(p1old, shours, by="Time", all.x=FALSE) #restrict to school hours

onew <- merge(onew, sdates, by="Date", all.x=FALSE) #restrict to school days
oold <- merge(oold, sdates, by="Date", all.x=FALSE) #restrict to school days
onew <- onew[(as.character(substr(onew$StartTime, 12, 13)) %in% c('08', '09', '10', '11', '12', '13') | onew$Time=="14:00"),] #restrict to school hours
oold <- merge(oold, shours, by="Time", all.x=FALSE) #restrict to school hours

p1  <- merge(p1new[,c("Temp", "Time", "Month", "Day")], p1old[,c("Temp", "Time", "Month", "Day")], by=c("Day", "Time", "Month"), suffixes = c("2015", "2013x2014"), all.x=TRUE, all.y=TRUE)
o  <- merge(onew[,c("Temp", "Time", "Month", "Day")], oold[,c("Temp", "Time", "Month", "Day")], by=c("Day", "Time", "Month"), suffixes = c("2015", "2013x2014"), all.x=TRUE, all.y=TRUE)


p1.hourly <- ddply(p1,c("Time", "Month"), summarize, AvgTempNew=mean(Temp2015, na.rm=TRUE), AvgTempOld=mean(Temp2013x2014, na.rm=TRUE))
p1.hourly <- melt(p1.hourly, id.vars=c("Time", "Month"))
o.hourly <- ddply(o,c("Time", "Month"), summarize, AvgTempNew=mean(Temp2015, na.rm=TRUE), AvgTempOld=mean(Temp2013x2014, na.rm=TRUE))
o.hourly <- melt(o.hourly, id.vars=c("Time", "Month"))

lims <- as.POSIXct(strptime(c("08:00","14:00"), format = "%H:%M"), tz="UTC")    
p1.hourly$Time <- as.POSIXct(p1.hourly$Time, format="%H:%M", tz="UTC")
p1.hourly$monthdisp <- factor(p1.hourly$Month, orderedMonths)
o.hourly$Time <- as.POSIXct(o.hourly$Time, format="%H:%M", tz="UTC")
o.hourly$monthdisp <- factor(o.hourly$Month, orderedMonths)                           

o.hourly <- o.hourly[complete.cases(o.hourly),]

ggplot() +
  geom_hline(yintercept=85, color="black", linetype="dotted", size=0.5) +
  geom_line(data=o.hourly, aes(x=Time, y=value, linetype=variable), color="dimgrey", size=0.5, alpha=0.8) +
  geom_line(data=p1.hourly, aes(x=Time, y=value, color=variable), size=1) +
  facet_grid(~monthdisp, drop=FALSE) +
  scale_y_continuous(breaks=seq(70,100,5), limits=c(70,100)) +
  scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
  scale_color_manual(name="", labels=c("Classroom (2015)", "Classroom (2013-2014)"), values=colors) +
  scale_linetype_manual(name="", labels=c("Outdoor (2015)", "Outdoor (2013-2014)"), values=c("longdash", "solid")) +
  ggtitle("Campbell P01: Average School Day Temperature") +
  theme_fivethirtyeight() +
  theme(text=element_text(size=9), legend.title=element_blank()) 


