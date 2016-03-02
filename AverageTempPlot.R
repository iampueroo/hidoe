AverageTempPlot <- function(rlist, var, minTemp) {
  
  library(reshape2)
  library(ggplot2)
  library(ggthemes) 
  library(gtable)
  library(scales)
  library(grid)
  library(gridExtra) 
  library(plyr)
  
  orderedMonths <- c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May")
  colors <-c("#CC6666", "#9999CC", "#66CC99")
  
  crt <-read.csv(file="~/dropbox/rh1/hidoe/final-csv/all-classroom-sensor.csv", sep=",") #classroom
  crt$Date <- as.Date(crt$Date, format="%Y-%m-%d")
  crt$Time <- format(strptime(crt$Time, format="%H:%M"), format="%H:%M")
  crt$Month <- format(crt$Date, "%B") 
  
  ot <- read.csv(file="~/dropbox/rh1/hidoe/final-csv/all-outdoor-sensor.csv", sep=",") #outdoor 
  ot$Date <- as.Date(ot$Date, format="%Y-%m-%d")
  ot$Time <- format(strptime(ot$Time, format="%H:%M"), format="%H:%M")
  ot$Month <- format(ot$Date, "%B")
  
  otHot <- ot[ot$OutdoorUTCI>= as.numeric(minTemp),] #restrict to minimum outside temperature threshold 

  o <- data.frame(ddply(ot, c("Time", "Month"), summarise, outavg=mean(OutdoorUTCI, na.rm=TRUE)))
  rooms <- data.frame(Time=character(),  Month=character(), stringsAsFactors=FALSE)
  roomsHot <- data.frame(Time=character(),  Month=character(), stringsAsFactors=FALSE)
  for (i in 1:length(rlist)) {
    print(rlist[i])
    assign(paste0("r",i,".name"), rlist[i])
    
    assign(paste0("room",i), crt[crt$Alias==rlist[i],]) 
    assign(paste0("roomHot",i), merge(get(paste0("room",i)), otHot[,c("Date", "Time", "Month")], by=c("Date", "Time", "Month"), all.x=FALSE)) #observations above minTemp
    assign(paste0("r",i), get(paste0("room",i)))
    assign(paste0("rlab",i), get(paste0("r",i))[1, c("Alias", "RoofColor", "Orientation", "Landscape", "Overhang", "Floor")])
    
    assign(paste0("room",i), ddply(get(paste0("room",i)), c("Time", "Month"), summarize, avg=mean(UTCI, na.rm=TRUE)))
    assign(paste0("roomHot",i), ddply(get(paste0("roomHot",i)), c("Time", "Month"), summarize, avg=mean(UTCI, na.rm=TRUE)))
    
    rooms <- merge(rooms, get(paste0("room",i)), by=c("Time","Month"), suffixes=c(i-1,i), all.x=TRUE, all.y=TRUE)
    roomsHot <- merge(roomsHot, get(paste0("roomHot",i)), by=c("Time","Month"), suffixes=c(i-1,i), all.x=TRUE, all.y=TRUE)
  }
  if(length(rlist)==3) {
    names(rooms)[names(rooms) == "avg"] <- "avg3"
    names(roomsHot)[names(roomsHot) == "avg"] <- "avg3"
  }

  if (var=="Roof Color") {varCols <- c("RoofColor")
  } else {varCols <- c(var)}
  
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
  plot1.subtitle <- "\nUTCI Averaged over All Observations" 
  plot2.subtitle <- "UTCI Averaged over 'Hot' Observations (>85 F)"
  plot3.subtitle <- "School Day with Greatest Absolute UTCI Difference"
  
  pa <- ggplot() +
    geom_line(data=rooms[rooms$variable=="outavg",], aes(x=Time, y=value), color="dimgrey", size=0.2) +
    geom_line(data=rooms[rooms$variable!="outavg",], aes(x=Time, y=value, color=variable)) +
    facet_wrap(~monthdisp, drop=FALSE, ncol=10) +
    scale_y_continuous(breaks=seq(60,110,5), limits=c(70,95)) +
    scale_colour_manual(labels=rlist, values=colors) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) +
    ggtitle(bquote(atop(.(plot.title), atop(bold(.(sub)), .(plot1.subtitle),"")))) +
    theme_fivethirtyeight() +     
    theme(text=element_text(size=9), legend.position=c(0.06,0.15), legend.background=element_rect(color="grey", fill="#F0F0F0", size=0.4, linetype="solid"), legend.title=element_blank()) 


  ### Average monthly temperature on school hour/days where outdoor temperature is above certain threshold
  oHot <- data.frame(ddply(otHot, c("Time", "Month"), summarise, outavg=mean(OutdoorUTCI)))
  roomsHot <- merge(roomsHot, oHot, by=c("Time", "Month"))
  roomsHot <- melt(roomsHot, id.vars=c("Time", "Month"))
  roomsHot$monthdisp <- factor(roomsHot$Month, orderedMonths)
  roomsHot$Time <- as.POSIXct(roomsHot$Time, forma="%H:%M", tz="UTC")
  
  plot.title <- ""
  
  lims <- as.POSIXct(strptime(c("08:00","14:00"), format = "%H:%M"), tz="UTC")    
  ph <- ggplot() +
    geom_line(data=rooms[rooms$variable=="outavg",], aes(x=Time, y=value), color="dimgrey", size=0.2) +
    geom_line(data=rooms[rooms$variable!="outavg",], aes(x=Time, y=value, color=variable)) +
    facet_wrap(~monthdisp, drop=FALSE, ncol=10) +
    scale_y_continuous(breaks=seq(60,110,5), limits=c(70,95)) +
    scale_colour_manual(values=colors) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot2.subtitle), "")))) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
    theme_fivethirtyeight() + 
    theme(text=element_text(size=9), legend.position="none") 
  
  ###  Monthly school hour/day with largest temperature differential between classrooms 
  d <- merge(r1[,c("Date", "Time", "Month", "UTCI")], r2[,c("Date", "Time", "UTCI")], by=c("Date", "Time"), suffixes=c("1","2"))
  if (length(rlist)==3) {
    d <- merge(d, r3[,c("Date", "Time","UTCI")], by=c("Date", "Time"))
    names(d)[names(d) == "UTCI"] <- "UTCI3"
    d$UTCIDiff <- d$UTCI1-d$UTCI3
  } else {d$UTCIDiff <- d$UTCI1 - d$UTCI2}
  maxDiffRows <- do.call("rbind", by(d, d$Month, function(x) x[which.max(abs(x$UTCIDiff)),]))
  
  oDiff <- merge(ot, maxDiffRows[c(1,3)], by=c("Date", "Month"), all.x=FALSE)
  roomsDiff <- merge(d, maxDiffRows[,c(1,3)], by=c("Date", "Month"), all.x=FALSE)
  roomsDiff$UTCIDiff <- NULL
  roomsDiff <- merge(roomsDiff, oDiff[,c("Time", "Month", "OutdoorUTCI")], by=c("Time", "Month"))
  roomsDiff <- melt(roomsDiff, id.vars=c("Time", "Month", "Date"))
  roomsDiff$monthdisp <- factor(roomsDiff$Month, orderedMonths)
  roomsDiff$Time <- as.POSIXct(roomsDiff$Time, format="%H:%M", tz="UTC")
  
  # Annotation and plot
  maxDiffRows$Date<- format(maxDiffRows$Date, format="%b %d, %Y")
  maxDiffRows$x <- as.POSIXct(format(strptime("01/01/16 11:00", format="%m/%d/%y %H:%M"), format="%H:%M"), format="%H:%M", tz="UTC")
  maxDiffRows$y <- 64
  maxDiffRows$y2 <- 62.5
  maxDiffRows$lab1 <-   as.character(paste(maxDiffRows$Date, maxDiffRows$Time, sep=" "))
  maxDiffRows$lab2 <- as.character(round(maxDiffRows$UTCIDiff,2))
  maxDiffRows$monthdisp <- factor(maxDiffRows$Month, orderedMonths)
  maxDiffRows <- maxDiffRows[,c("x", "y", "y2", "lab1", "lab2", "monthdisp")]
  
  pd <- ggplot() +
    geom_line(data=roomsDiff[roomsDiff$variable=="OutdoorUTCI",], aes(x=Time, y=value), color="dimgrey", size=0.2) +
    geom_line(data=roomsDiff[roomsDiff$variable!="OutdoorUTCI",], aes(x=Time, y=value, color=variable)) +
    facet_wrap(~monthdisp, drop=FALSE, ncol=10) +
    scale_y_continuous(breaks=seq(60,110,5), limits=c(60,95)) +
    scale_colour_manual(values=colors) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot3.subtitle), "")))) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) +
    geom_text(data=maxDiffRows, aes(x, y, label=lab1, group=1), color="dimgrey", size=3, hjust="center") + 
    geom_text(data=maxDiffRows, aes(x, y2, label=paste("Maximum Difference:", lab2, "F", sep=" "), group=1), color="dimgrey", size=3, hjust="center") +
    theme_fivethirtyeight() + 
    theme(text=element_text(size=9), legend.position=c(0.06,0.15), legend.background=element_rect(color="grey", fill="#F0F0F0", size=0.4, linetype="solid"), legend.title=element_blank()) 
  

  
  # Specs table
  #labels <- do.call(rbind,lapply(paste0("rlab",1:length(rlist)),get))
  #if (length(rlist)==2) {labels <- rbind(rlab1, rlab2)
  #} else if (length(rlist)==3) {labels <- rbind(rlab1, rlab2, rlab3)}
  #rownames(labels) <- NULL
  #pt <- tableGrob(labels, theme=ttheme_minimal())
  
  ### Plot on grid
  grid.arrange(pa,ph,pd, layout_matrix=rbind(c(1), c(2), c(3)))
  
}

