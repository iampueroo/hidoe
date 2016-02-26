AverageTempPlot <- function(rlist, var, minTemp) {
  
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
  colors <-c("#CC6666", "#9999CC", "#66CC99")
  
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
  #crt <- merge(crt[,c(1,2,3,4)], arch[,c("RoomID", "Alias", "Floor", "RoofColor", "SQFT", "Orientation", "Landscape", "Overhang")], by="RoomID", all.x=TRUE)
  
  #crt$Date <- as.Date(crt$StartTime, format="%m/%d/%y")
  #crt$Time <- format(strptime(crt$StartTime, format="%m/%d/%y %H:%M"), format="%H:%M")
  #crt$Month <- format(crt$Date, "%B") 
  #crt <- merge(crt, sdates, by="Date", all.x=FALSE) #restrict to school days
  #crt <- merge(crt, shours, by="Time", all.x=FALSE) #restrict to school hours
  #crt$SensorAlias <- as.character(crt$SensorAlias)
  #crt$StartTime <- NULL
  #crt <<- crt[!is.na(crt$Temp),]
  
  ot <- read.csv(file="~/dropbox/rh1/hidoe/outdoortemp.csv", sep=",") #outdoor temperature - already restricted to school day/hours
  names(ot) <- c("DateTime", "OutdoorTemp")
  ot$Date <- as.Date(ot$DateTime, format="%m/%d/%y")
  ot$Time <- format(strptime(ot$DateTime, format="%m/%d/%y %H:%M"), format="%H:%M")
  ot$DateTime <- NULL
  ot$Month <- format(ot$Date, "%B")
  otHot <- ot[ot$OutdoorTemp >= as.numeric(minTemp),] #restrict to minimum outside temperature threshold 

  o <- data.frame(ddply(ot, c("Time", "Month"), summarise, outavg=mean(OutdoorTemp)))
  rooms <- data.frame(Time=character(),  Month=character(), stringsAsFactors=FALSE)
  roomsHot <- data.frame(Time=character(),  Month=character(), stringsAsFactors=FALSE)
  for (i in 1:length(rlist)) {
    print(rlist[i])
    assign(paste0("r",i,".name"), rlist[i])
    
    assign(paste0("room",i), crt[(crt$Alias==rlist[i]) & ((substr(crt$SensorAlias,nchar(crt$SensorAlias)-1,nchar(crt$SensorAlias))!="HD")),]) 
    assign(paste0("roomHot",i), merge(get(paste0("room",i)), otHot[,c("Date", "Time", "Month")], by=c("Date", "Time", "Month"), all.x=FALSE)) #observations above minTemp
    assign(paste0("r",i), get(paste0("room",i)))
    assign(paste0("rlab",i), get(paste0("r",i))[1, c("Alias", "RoofColor", "Orientation", "Landscape", "Overhang", "Floor")])
    
    assign(paste0("room",i), ddply(get(paste0("room",i)), c("Time", "Month"), summarize, avg=mean(Temp)))
    assign(paste0("roomHot",i), ddply(get(paste0("roomHot",i)), c("Time", "Month"), summarize, avg=mean(Temp)))
    
    rooms <- merge(rooms, get(paste0("room",i)), by=c("Time","Month"), suffixes=c(i-1,i), all.x=TRUE, all.y=TRUE)
    roomsHot <- merge(roomsHot, get(paste0("roomHot",i)), by=c("Time","Month"), suffixes=c(i-1,i), all.x=TRUE, all.y=TRUE)
    
    assign(paste0("ann",i), data.frame(rt=as.POSIXct(format(strptime("01/01/16 8:00", format="%m/%d/%y %H:%M"), format="%H:%M"), format="%H:%M", tz="UTC"),
                                       value=94-1.7*(i-1),
                                       lab=rlist[i],
                                       monthdisp = factor("August", levels=orderedMonths)))
  }
  if(length(rlist)==3) {
    names(rooms)[names(rooms) == "avg"] <- "avg3"
    names(roomsHot)[names(roomsHot) == "avg"] <- "avg3"
  }

  if (var=="Floor") {varCols <- c("Floor")
  } else if (var=="Orientation") {varCols <- c("Orientation")
  } else if (var=="Roof Color") {varCols <- c("RoofColor")
  } else if (var=="Landscape") {varCols <- c("Landscape")
  } else if (var=="Overhang") {varCols <- c("Overhang")}
  
  ### Average monthly temperatures for all school hour/days
  rooms <- merge(rooms, o, by=c("Time", "Month"))
  rooms <- melt(rooms, id.vars = c("Time", "Month"))
  rooms$monthdisp <- factor(rooms$Month, orderedMonths)
  rooms$Time <- as.POSIXct(rooms$Time, format="%H:%M", tz="UTC")
  

  
  if (length(rlist)==3) {
    plot.title <- paste(r1.name,r2.name,r3.name, sep=" vs. ")
    sub <- paste(var, paste(rlab1[,varCols], rlab2[,varCols], rlab3[,varCols], sep=" vs. "), sep=": ") 
  } else {
    plot.title <- paste(r1.name,r2.name, sep=" vs. ")
    sub <-paste(var,paste(rlab1[,varCols], rlab2[,varCols], sep=" vs. "), sep=": ")
  }
  plot1.subtitle <- "\nTemperature Averaged over All Observations" 
  plot2.subtitle <- "Temperature Averaged over 'Hot' Observations (>85 F)"
  plot3.subtitle <- "School Day with Greatest Absolute Temperature Difference"
  
  
  pa <- ggplot() +
    geom_line(data=rooms[rooms$variable=="outavg",], aes(x=Time, y=value), color="gray") +
    geom_line(data=rooms[rooms$variable!="outavg",], aes(x=Time, y=value, color=variable), size=0.8) +
    facet_wrap(~monthdisp, drop=FALSE, ncol=10) +
    scale_y_continuous(breaks=seq(60,110,5)) +
    scale_colour_manual(values=colors) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) +
    ggtitle(bquote(atop(.(plot.title), atop(bold(.(sub)), .(plot1.subtitle),"")))) +
    geom_text(data=ann1, aes(x=rt, y=value, label=lab, group=1), color="#CC6666", size=4, hjust=0) + 
    geom_text(data=ann2, aes(x=rt, y=value, label=lab, group=1), color="#9999CC", size=4, hjust=0) +
    theme_fivethirtyeight(base_family="Helvetica") + theme(legend.position="none") 

  if (length(rlist)==3) {pa <- pa +  geom_text(data=ann3, aes(x=rt, y=value, label=lab, group=1), color="#66CC99", size=4, hjust=0)}
  
  ### Average monthly temperature on school hour/days where outdoor temperature is above certain threshold
  oHot <- data.frame(ddply(otHot, c("Time", "Month"), summarise, outavg=mean(OutdoorTemp)))
  roomsHot <- merge(roomsHot, oHot, by=c("Time", "Month"))
  roomsHot <- melt(roomsHot, id.vars=c("Time", "Month"))
  roomsHot$monthdisp <- factor(roomsHot$Month, orderedMonths)
  roomsHot$Time <- as.POSIXct(roomsHot$Time, forma="%H:%M", tz="UTC")
  
  plot.title <- ""
  
  lims <- as.POSIXct(strptime(c("08:00","14:00"), format = "%H:%M"), tz="UTC")    
  ph <- ggplot() +
    geom_line(data=roomsHot[roomsHot$variable=="outavg",], aes(x=Time, y=value), color="gray") +
    geom_line(data=roomsHot[roomsHot$variable!="outavg",], aes(x=Time, y=value, color=variable), size=0.8) +
    facet_wrap(~monthdisp, drop=FALSE, ncol=10) +
    scale_y_continuous(breaks=seq(60,110,5), limits=c(70,95)) +
    scale_colour_manual(values=colors) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot2.subtitle), "")))) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
    theme_fivethirtyeight(base_family="Helvetica") + theme(legend.position="none") 
  
  ###  Monthly school hour/day with largest temperature differential between classrooms 
  d <- merge(r1[,c("Date", "Time", "Month", "Temp")], r2[,c("Date", "Time", "Temp")], by=c("Date", "Time"), suffixes=c("1","2"))
  if (length(rlist)==3) {
    d <- merge(d, r3[,c("Date", "Time","Temp")], by=c("Date", "Time"))
    names(d)[names(d) == "Temp"] <- "Temp3"
    d$TempDiff <- d$Temp1-d$Temp3
  } else {d$TempDiff <- d$Temp1 - d$Temp2}
  maxDiffRows <- do.call("rbind", by(d, d$Month, function(x) x[which.max(abs(x$TempDiff)),]))
  
  oDiff <- merge(ot, maxDiffRows[c(1,3)], by=c("Date", "Month"), all.x=FALSE)
  roomsDiff <- merge(d, maxDiffRows[,c(1,3)], by=c("Date", "Month"), all.x=FALSE)
  roomsDiff$TempDiff <- NULL
  roomsDiff <- merge(roomsDiff, oDiff[,c("Time", "Month", "OutdoorTemp")], by=c("Time", "Month"))
  roomsDiff <- melt(roomsDiff, id.vars=c("Time", "Month", "Date"))
  roomsDiff$monthdisp <- factor(roomsDiff$Month, orderedMonths)
  roomsDiff$Time <- as.POSIXct(roomsDiff$Time, format="%H:%M", tz="UTC")
  
  # Annotation and plot
  maxDiffRows$Date<- format(maxDiffRows$Date, format="%b %d, %Y")
  maxDiffRows$x <- as.POSIXct(format(strptime("01/01/16 11:00", format="%m/%d/%y %H:%M"), format="%H:%M"), format="%H:%M", tz="UTC")
  maxDiffRows$y <- 64
  maxDiffRows$y2 <- 62.5
  maxDiffRows$lab1 <-   as.character(paste(maxDiffRows$Date, maxDiffRows$Time, sep=" "))
  maxDiffRows$lab2 <- as.character(round(maxDiffRows$TempDiff,2))
  maxDiffRows$monthdisp <- factor(maxDiffRows$Month, orderedMonths)
  maxDiffRows <- maxDiffRows[,c("x", "y", "y2", "lab1", "lab2", "monthdisp")]
  
  pd <- ggplot() +
    geom_line(data=roomsDiff[roomsDiff$variable=="OutdoorTemp",], aes(x=Time, y=value), color="gray") +
    geom_line(data=roomsDiff[roomsDiff$variable!="OutdoorTemp",], aes(x=Time, y=value, color=variable), size=0.8) +
    facet_wrap(~monthdisp, drop=FALSE, ncol=10) +
    scale_y_continuous(breaks=seq(60,110,5)) +
    scale_colour_manual(values=colors) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot3.subtitle), "")))) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) +
    geom_text(data=maxDiffRows, aes(x, y, label=lab1, group=1), color="dimgrey", size=3, hjust="center") + 
    geom_text(data=maxDiffRows, aes(x, y2, label=paste("Maximum Difference:", lab2, "F", sep=" "), group=1), color="dimgrey", size=3, hjust="center") +
    theme_fivethirtyeight(base_family="Helvetica") + theme(legend.position="none") 
  

  
  # Specs table
  #labels <- do.call(rbind,lapply(paste0("rlab",1:length(rlist)),get))
  if (length(rlist)==2) {labels <- rbind(rlab1, rlab2)
  } else if (length(rlist)==3) {labels <- rbind(rlab1, rlab2, rlab3)}
  rownames(labels) <- NULL
  pt <- tableGrob(labels, theme=ttheme_minimal())
  
  ### Plot on grid
  grid.arrange(pa,ph,pd, layout_matrix=rbind(c(1), c(2), c(3)))
  
}

