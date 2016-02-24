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
  colors <- c("#8B8B8B", "#30A2DA", "#FC4F30",  "#E5AE38", "#6D904F")
  
  #sdates <- read.csv(file="~/dropbox/rh1/hidoe/day.csv", sep=",") 
  #sdates$Date <- as.Date(sdates$rd, format="%m/%d/%y")
  #sdates <- sdates[-c(1)]
  #arch <- read.csv(file="~/dropbox/rh1/hidoe/classroom-csv.csv", sep=",") #classroom architectural data
  #shours <- read.csv(file="~/dropbox/rh1/hidoe/hour.csv", sep=",") 
  #shours$Time <- format(strptime(shours$Hour, format="%H:%M"), format="%H:%M")
  #shours$Hour <- NULL
  
  #setwd("~/dropbox/rh1/hidoe/csv")
  #crt <- do.call(rbind,lapply(dir(), read.csv))
  #names(crt) <- c("RoomID", "SensorAlias", "StartTime", "Temp", "RH", "Ill")
  #crt <- merge(crt[,c(1,2,3,4)], arch[,c("RoomID", "Alias", "Floor", "RoofColor", "SQFT", "Orientation", "Landscape", "Overhang", "School", "AC")], by="RoomID", all.x=TRUE)
  
  #crt$Date <- as.Date(crt$StartTime, format="%m/%d/%y")
  #crt$Time <- format(strptime(crt$StartTime, format="%m/%d/%y %H:%M"), format="%H:%M")
  #crt$Month <- format(crt$Date, "%B") 
  #crt <- merge(crt, sdates, by="Date", all.x=FALSE) #restrict to school days
  #crt <- merge(crt, shours, by="Time", all.x=FALSE) #restrict to school hours
  #crt$SensorAlias <- as.character(crt$SensorAlias)
  #crt$StartTime <- NULL
  #crt <- crt[!is.na(crt$Temp),]
  #crt <- crt[crt$AC!=1,] #remove classrooms with AC
  #crt$Season <- as.factor(ifelse(month(crt$Date) %in% c(1,2,3,4,11,12),"Winter","Summer")) 
  #crt$AC <- NULL
  
  ot <- read.csv(file="~/dropbox/rh1/hidoe/outdoortemp.csv", sep=",") #outdoor temperature - already restricted to school day/hours
  names(ot) <- c("DateTime", "OutdoorTemp")
  ot$Date <- as.Date(ot$DateTime, format="%m/%d/%y")
  ot$Time <- format(strptime(ot$DateTime, format="%m/%d/%y %H:%M"), format="%H:%M")
  ot$DateTime <- NULL
  ot$Month <- format(ot$Date, "%B")

  
  ot.daily <- ddply(ot,c("Date"), summarise, AvgOutdoorTemp=mean(OutdoorTemp)) #aggregated by date 
  crt.daily <- ddply(crt, c("Date", "Alias", "School"), summarise, AvgRoomTemp=mean(Temp, na.rm=TRUE), MaxRoomTemp=max(Temp,na.rm=TRUE)) #aggregated  by date
  cro <- data.frame(merge(crt.daily, ot.daily, by=c("Date"), all.x=TRUE, all.y=TRUE))  #merge to create giant dataset
  
  cro.melt <- melt(cro, id.vars=c("Date", "Alias", "School"))
  ggplot(cro.melt, aes(x=Date, y=value, colour=variable)) + geom_point(data=cro.melt[cro.melt$variable!="AvgOutdoorTemp",], alpha=0.15) + geom_smooth(size=1) +
    geom_hline(yintercept=85, linetype="dashed", color="dimgrey", size=0.8) + 
    facet_grid(~School) +
    scale_y_continuous(breaks=seq(70,110,5)) + 
    scale_colour_manual(values=colors) +
    scale_x_date(date_breaks="1 month", date_labels="%b '%y") + 
    ggtitle("Daily Temperature") + 
    xlab("Date") + ylab(expression(paste("Temperature (", degree ~ F, ")"))) +
    theme_fivethirtyeight()
  
   
  cro <- merge(crt, ot, by=c("Time", "Month", "Date"), all.x=TRUE, all.y=TRUE)
  cro$TimeUnit <- 1
  cro$OutTimeUnit <- ifelse(is.na(cro$OutdoorTemp),0,1)
  cro$InGE85 <- ifelse(with(cro, Temp>=85)==TRUE,1,0)
  cro$OutGE85 <- ifelse(with(cro, OutdoorTemp>=85)==TRUE,1,0)
  o.daily <- ddply(cro, "Season", summarize, OutGE85Count=sum(OutGE85, na.rm=TRUE), Count=sum(OutTimeUnit, na.rm=TRUE))
  o.daily$OutGE85 <- with(o.daily, round(OutGE85Count/Count,2))
  
  cro.daily <- ddply(cro, c("Alias", "Season", "School"), summarize, InGE85Count=sum(InGE85), Count=sum(TimeUnit))
  cro.daily$InGE85 <- with(cro.daily, round(InGE85Count/Count,2))
  
  cros.daily <- ddply(cro, c("Season", "School"), summarize, InGE85Count=sum(InGE85), Count=sum(TimeUnit))
  cros.daily$InGE85 <- with(cros.daily, round(InGE85Count/Count,2))
  
  anno <- o.daily
  anno$x <- 53
  anno$y <- anno$OutGE85 + .02
  anno$lab <- paste0("Outdoor average: ", anno$OutGE85)
  anno <- anno[,c("x", "y", "lab", "Season")]
  
  annc <- cros.daily[cros.daily$School=="Campbell",]
  annc$x <- 1
  annc$y <- annc$InGE85 + .02
  annc$lab <- paste0("School average: ", annc$InGE85)
  annc <- annc[,c("x", "y", "lab", "Season","School")]
  
  anni <- cros.daily[cros.daily$School=="Ilima",]
  anni$x <- 1
  anni$y <- anni$InGE85 + .02
  anni$lab <- paste0("School average: ", anni$InGE85)
  anni <- anni[,c("x", "y", "lab", "Season","School")]
  
  annk <- cros.daily[cros.daily$School=="Kaimiloa",]
  annk$x <- 1
  annk$y <- annk$InGE85 + .02
  annk$lab <- paste0("School average: ", annk$InGE85)
  annk <- annk[,c("x", "y", "lab", "Season","School")]
  

  ggplot() + geom_bar(data=cro.daily, aes(x=Alias, y=InGE85, fill=Alias), alpha=0.5, stat="identity") + 
    geom_hline(data=o.daily, aes(yintercept=OutGE85), color="dimgrey") +
    geom_text(data=anno, aes(x, y, label=lab, group=1), color="dimgrey", size=3, hjust="right") + 
    geom_text(data=annc, aes(x, y, label=lab, group=1), color="dimgrey", size=3, hjust="left") + 
    geom_text(data=anni, aes(x, y, label=lab, group=1), color="dimgrey", size=3, hjust="left") + 
    geom_text(data=annk, aes(x, y, label=lab, group=1), color="dimgrey", size=3, hjust="left") + 
    geom_hline(data=cros.daily, aes(yintercept=InGE85), color="dimgrey", linetype="dashed", alpha=0.8) +
    facet_grid(School~Season, drop=FALSE) +
    scale_y_continuous(breaks=seq(0,1,0.2)) +
    ggtitle("Portion of School Day Hotter than 85 F") +
    theme_fivethirtyeight() +
    theme(axis.text.x = element_text(angle=60, hjust=1)) + guides(fill=guide_legend(ncol=13))
  
  
  
  
  cr <- ddply(crt, c("Time", "Alias", "Season", "School"), summarize, AvgTemp=mean(Temp))
  ot$Season <- as.factor(ifelse(month(ot$Date) %in% c(1,2,3,4,11,12),"Winter","Summer")) 
  o <- ddply(ot, c("Time", "Season"), summarize, AvgTemp=mean(OutdoorTemp))
  
  o$Time <- as.POSIXct(o$Time, format="%H:%M", tz="UTC")
  cr$Time <- as.POSIXct(cr$Time, format="%H:%M", tz="UTC")
  

  
  ggplot() + 
    geom_hline(yintercept=85, linetype="dotted", color="black") +
    geom_line(data=cr, aes(x=Time, y=AvgTemp, group=Alias, color=Alias), size=0.5, alpha=0.5) +
    geom_line(data=o, aes(x=Time, y=AvgTemp, group=1), color="dimgrey", size=0.8) +
    facet_grid(Season~School) +
    scale_y_continuous(breaks=seq(75,100,5)) +
    scale_x_datetime(breaks=date_breaks("1 hour"), labels=date_format("%H:%M")) +
    ggtitle("Average School Day Temperatures by School, Season") +
    theme_fivethirtyeight() + guides(color=guide_legend(ncol=13))
  
  
  ot$Year <- as.factor(year(ot$Date))
  ot$Day <- as.character(format(ot$Date, "%m-%d"))
  otoverlap <- ot[ot$Month %in% c("August", "September", "October"),]
  ggplot(otoverlap, aes(x=Date, y=OutdoorTemp)) + geom_point(color="blue", alpha=0.5) + geom_smooth(size=1, color="red") + facet_grid(~Year) +
     theme_fivethirtyeight()
  
  year(otoverlap$Date) <- 2013
  
  ggplot() + geom_smooth(data=otoverlap, aes(x=Day, y=OutdoorTemp, color=Year, group=Year)) + scale_y_continuous(breaks=seq(70,100,5), limits=c(70,100)) +
    ggtitle("Comparison of Summer Months by Year") + theme_fivethirtyeight()
  

  
}