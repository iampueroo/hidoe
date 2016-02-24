OverviewPlot <- function() {
  library(reshape2)
  library(ggplot2)
  library(ggthemes) 
  library(gtable)
  library(scales)
  library(grid)
  library(gridExtra) 
  library(plyr)
  library(lubridate)
  

  orderedMonths <- c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May")
  colors <- c("#30A2DA", "#FC4F30", "#E5AE38", "#6D904F", "#8B8B8B")
  
  sdates <- read.csv(file="~/dropbox/rh1/hidoe/day.csv", sep=",") 
  sdates$Date <- as.Date(sdates$rd, format="%m/%d/%y")
  sdates <- sdates[-c(1)]
  arch <- read.csv(file="~/dropbox/rh1/hidoe/classroom-csv.csv", sep=",") #classroom architectural data
  shours <- read.csv(file="~/dropbox/rh1/hidoe/hour.csv", sep=",") 
  shours$Time <- format(strptime(shours$Hour, format="%H:%M"), format="%H:%M")
  shours$Hour <- NULL
  
  setwd("~/dropbox/rh1/hidoe/csv")
  crt <- do.call(rbind,lapply(dir(), read.csv))
  names(crt) <- c("RoomID", "SensorAlias", "StartTime", "Temp", "RH", "Ill")
  crt <- merge(crt[,c(1,2,3,4)], arch[,c("RoomID", "Alias", "Floor", "RoofColor", "SQFT", "Orientation", "Landscape", "Overhang", "School", "AC")], by="RoomID", all.x=TRUE)
  
  crt$Date <- as.Date(crt$StartTime, format="%m/%d/%y")
  crt$Time <- format(strptime(crt$StartTime, format="%m/%d/%y %H:%M"), format="%H:%M")
  crt$Month <- format(crt$Date, "%B") 
  crt <- merge(crt, sdates, by="Date", all.x=FALSE) #restrict to school days
  crt <- merge(crt, shours, by="Time", all.x=FALSE) #restrict to school hours
  crt$SensorAlias <- as.character(crt$SensorAlias)
  crt$StartTime <- NULL
  crt <- crt[!is.na(crt$Temp),]
  crt <- crt[crt$AC!=1,] #remove classrooms with AC
  crt$AC <- NULL
  
  ot <- read.csv(file="~/dropbox/rh1/hidoe/outdoortemp.csv", sep=",") #outdoor temperature - already restricted to school day/hours
  names(ot) <- c("DateTime", "OutdoorTemp")
  ot$Date <- as.Date(ot$DateTime, format="%m/%d/%y")
  ot$Time <- format(strptime(ot$DateTime, format="%m/%d/%y %H:%M"), format="%H:%M")
  ot$DateTime <- NULL
  ot$Month <- format(ot$Date, "%B")
  
  cr <- ddply(crt, c("Time", "Alias", "Month", "School"), summarize, AvgTemp=mean(Temp))
  o <- ddply(ot, c("Time", "Month"), summarize, AvgTemp=mean(OutdoorTemp))
  o$Alias <- "Outdoor"
  o$School <- "Campbell"
  o2 <- o
  o2$School <- "Kaimiloa"
  o3 <- o
  o3$School <- "Ilima"
  
  agg <- rbind(o, o2, o3, cr)
  agg$monthdisp <- factor(agg$Month, orderedMonths)
  agg$Time <- as.POSIXct(agg$Time, format="%H:%M", tz="UTC")
  agg$School <- as.factor(agg$School)

  cro <- merge(crt, ot, by=c("Time", "Month", "Date"), all.x=TRUE, all.y=TRUE)
  cro$TimeUnit <- 1
  cro$OutTimeUnit <- ifelse(is.na(cro$OutdoorTemp),0,1)
  cro$InGE85 <- ifelse(with(cro, Temp>=85)==TRUE,1,0)
  cro$OutGE85 <- ifelse(with(cro, OutdoorTemp>=85)==TRUE,1,0)
  o.daily <- ddply(cro, "Month", summarize, OutGE85Count=sum(OutGE85, na.rm=TRUE), Count=sum(OutTimeUnit, na.rm=TRUE))
  o.daily$OutGE85 <- with(o.daily, round(OutGE85Count/Count,2))
  o.daily$monthdisp <- factor(o.daily$Month, orderedMonths)
  cro.daily <- ddply(cro, c("Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85), Count=sum(TimeUnit))
  cro.daily$InGE85 <- with(cro.daily, round(InGE85Count/Count,2))
  cro.daily$monthdisp <- factor(cro.daily$Month, orderedMonths)
  
  ggplot() + geom_bar(data=cro.daily, aes(x=Alias, y=InGE85, fill=Alias), alpha=0.5, stat="identity") + 
    geom_hline(data=o.daily, aes(yintercept=OutGE85), color="dimgrey", linetype="dashed", alpha=0.8) +
    facet_grid(School~monthdisp, drop=FALSE) +
    scale_y_continuous(breaks=seq(0,1,0.2)) +
    ggtitle("Portion of School Day Hotter than 85 F") +
    theme_fivethirtyeight() +
    theme(axis.text.x = element_blank())
            
  
               
  ggplot() + 
    geom_hline(yintercept=85, linetype="dashed", color="red", alpha=0.8) +
    geom_line(data=agg[agg$Alias!="Outdoor",], aes(x=Time, y=AvgTemp, group=Alias, color=Alias), size=0.5, alpha=0.35) +
    geom_line(data=agg[agg$Alias=="Outdoor",], aes(x=Time, y=AvgTemp, group=1), color="dimgrey") +
    facet_grid(School~monthdisp, drop=FALSE) +
    scale_y_continuous(breaks=seq(75,100,5)) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) +
    ggtitle("Average School Day Temperatures by School, Month") +
    theme_fivethirtyeight()
    

  
}