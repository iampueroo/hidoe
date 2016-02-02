avgTempPlot <- function(rlist, minTemp) {
  
  library(reshape2)
  library(ggplot2)
  library(ggthemes) 
  library(gtable)
  library(scales)
  library(grid)
  library(gridExtra) 
  library(plyr)
  library(lubridate)
  
  ### Read and merge all sensor csvs - DO ONLY ONCE!!!
  #r <- read.csv(file="~/dropbox/rh1/hidoe/rooms.csv", sep=",")
  #s <- read.csv(file="~/dropbox/rh1/hidoe/school.csv", sep=",")
  #s$rd <- as.Date(s$rd, format="%m/%d/%y")
  
  #setwd("~/dropbox/rh1/hidoe/csv")
  #allcsv <- do.call(rbind,lapply(dir(), read.csv))
  #names(allcsv) <- c("RoomID", "Alias", "StartTime", "Temp", "RH", "Ill")
  #allcsv <- merge(allcsv, r, by="RoomID", all.x=TRUE)
  #allcsv$rdt <- as.POSIXct(allcsv$StartTime, format="%m/%d/%y %H:%M")
  #allcsv$rd <- as.Date(allcsv$StartTime, format="%m/%d/%y")
  #allcsv$rt <- format(strptime(allcsv$StartTime, format="%m/%d/%y %H:%M"), format="%H:%M")
  #allcsv$month <- format(allcsv$rd, "%B")
  #allcsv <- merge(allcsv, s, by="rd", all.x=FALSE)
  #allcsv$Alias <- as.character(allcsv$Alias)
  #allcsv <<- allcsv[(as.character(substr(allcsv$rdt, 12, 13)) %in% c('08', '09', '10', '11', '12', '13') | allcsv$rt=="14:00"),]
  
  orderedMonths <- c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May")
  colors <- c("#30A2DA", "#FC4F30", "#E5AE38", "#6D904F", "#8B8B8B")
  
  o <- read.csv(file="~/dropbox/rh1/hidoe/outdoortemp.csv", sep=",")
  o$rdt <- as.POSIXct(o$DateTime, format="%m/%d/%y %H:%M")
  o$rd <- as.Date(o$DateTime, format="%m/%d/%y")
  o$rt <- format(strptime(o$DateTime, format="%m/%d/%y %H:%M"), format="%H:%M")
  o$month <- format(o$rd, "%B")
  o <- o[-c(1)]
  allo <- o
  oHot <- o[o$Temp >= as.numeric(minTemp),] #restrict to outside temperature hotter than specified minimum temperature
  
  o <- data.frame(ddply(o, c("rt", "month"), summarise, outavg=mean(Temp)))
  rooms <- data.frame(rt=character(),  month=character(), stringsAsFactors=FALSE)
  roomsHot <- data.frame(rt=character(),  month=character(), stringsAsFactors=FALSE)
  for (i in 1:length(rlist)) {
    print(rlist[i])
    assign(paste0("r",i,".name"), rlist[i])
    
    assign(paste0("room",i), allcsv[(allcsv$RoomName==rlist[i]) & (!is.na(allcsv$Temp) & (substr(allcsv$Alias,nchar(allcsv$Alias)-1,nchar(allcsv$Alias))!="HD")),]) 
    assign(paste0("roomHot",i), merge(get(paste0("room",i)), oHot[,c("rdt", "rd", "rt")], by=c("rdt", "rd", "rt"), all.x=FALSE)) #observations above minTemp
    assign(paste0("r",i), get(paste0("room",i)))
    assign(paste0("rlab",i), get(paste0("r",i))[1, c("RoomName", "HasFans", "HasAC", "RoofColor", "Orientation", "Landscape", "Overhang")])
    
    assign(paste0("room",i), ddply(get(paste0("room",i)), c("rt", "month"), summarize, avg=mean(Temp)))
    assign(paste0("roomHot",i), ddply(get(paste0("roomHot",i)), c("rt", "month"), summarize, avg=mean(Temp)))
    
    rooms <- merge(rooms, get(paste0("room",i)), by=c("rt","month"), suffixes=c(i-1,i), all.x=TRUE, all.y=TRUE)
    roomsHot <- merge(roomsHot, get(paste0("roomHot",i)), by=c("rt","month"), suffixes=c(i-1,i), all.x=TRUE, all.y=TRUE)
    
    assign(paste0("ann",i), data.frame(rt=as.POSIXct(format(strptime("01/01/16 8:00", format="%m/%d/%y %H:%M"), format="%H:%M"), format="%H:%M", tz="UTC"),
                                       value=94-2.5*(i-1),
                                       lab=sub(".*-", "", rlist[i]),
                                       monthdisp = factor("August", levels=orderedMonths)))
  }
  if(length(rlist)==3) {
    names(rooms)[names(rooms) == "avg"] <- "avg3"
    names(roomsHot)[names(roomsHot) == "avg"] <- "avg3"
  }
  
  anno <- data.frame(rt=as.POSIXct(format(strptime("01/01/16 9:00", format="%m/%d/%y %H:%M"), format="%H:%M"), format="%H:%M", tz="UTC"),
                     value=94+2.5,
                     lab="Outside",
                     monthdisp = factor("August", levels=orderedMonths))
  
  ### Average monthly temperatures for all school hour/days
  rooms <- merge(rooms, o, by=c("rt", "month"))
  rooms <- melt(rooms, id.vars = c("rt", "month"))
  rooms$monthdisp <- factor(rooms$month, orderedMonths)
  rooms$rt <- as.POSIXct(rooms$rt, format="%H:%M", tz="UTC")
  
  pa <- ggplot() +
    geom_line(data=rooms[rooms$variable=="outavg",], aes(x=rt, y=value), color="gray") +
    geom_line(data=rooms[rooms$variable!="outavg",], aes(x=rt, y=value, color=variable), size=0.8) +
    ylim(70,95) +
    facet_wrap(~monthdisp, ncol=10) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) +
    scale_colour_manual(values=colors) +
    geom_label(data=ann1, aes(x=rt, y=value, label=lab, group=1), color="#30A2DA", size=4, hjust=0) + 
    geom_label(data=ann2, aes(x=rt, y=value, label=lab, group=1), color="#FC4F30", size=4, hjust=0) +
    theme_bw() +
    theme(legend.position="none")
    #geom_label(data=anno, aes(x=rt, y=value, label=lab, group=1), color="gray", size=5, hjust=0) + 
  
  if (length(rlist)==3) {pa <- pa + geom_label(data=ann3, aes(x=rt, y=value, label=lab, group=1), color="#E5AE38", size=4, hjust=0) + labs(x="", y="", title=paste(r1.name,r2.name,r3.name, sep=" vs. "))
  } else {pa <- pa + labs(x="", y="", title=paste(r1.name,r2.name, sep=" vs. "))}
  

  ### Average monthly temperature on school hour/days where outdoor temperature is above certain threshold
  oHot <- data.frame(ddply(oHot, c("rt", "month"), summarise, outavg=mean(Temp)))
  roomsHot <- merge(roomsHot, oHot, by=c("rt", "month"))
  roomsHot <- melt(roomsHot, id.vars=c("rt", "month"))
  roomsHot$monthdisp <- factor(roomsHot$month, orderedMonths)
  roomsHot$rt <- as.POSIXct(roomsHot$rt, forma="%H:%M", tz="UTC")
  
  #figure out how to set limits on x-axis
  ph <- ggplot() +
    geom_line(data=roomsHot[roomsHot$variable=="outavg",], aes(x=rt, y=value), color="gray") +
    geom_line(data=roomsHot[roomsHot$variable!="outavg",], aes(x=rt, y=value, color=variable), size=0.8) +
    facet_wrap(~monthdisp, ncol=10, drop=FALSE) +
    ylim(70,95) +
    labs(x="", y=expression(paste("Temperature (", degree ~ F, ")"))) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) +
    scale_colour_manual(values=colors) +
    theme_bw() + 
    theme(legend.position="none")
  
  ###  Monthly school hour/day with largest temperature differential between classrooms 
  d <- merge(r1[,c("rd", "rt", "month", "Temp")], r2[,c("rd", "rt", "Temp")], by=c("rd", "rt"), suffixes=c("1","2"))
  if (length(rlist)==3) {
    d <- merge(d, r3[,c("rd", "rt","Temp")], by=c("rd", "rt"))
    names(d)[names(d) == "Temp"] <- "Temp3"
    d$TempDiff <- d$Temp1-d$Temp3
  } else {d$TempDiff <- d$Temp1 - d$Temp2}
  maxDiffRows <- do.call("rbind", by(d, d$month, function(d) d[which.max(abs(d$TempDiff)),]))
  rownames(maxDiffRows) <- NULL 
  
  oDiff <- merge(allo, maxDiffRows[c(1,3)], by=c("rd", "month"), all.x=FALSE)
  roomsDiff <- merge(d, maxDiffRows[,c(1,3)], by=c("rd", "month"), all.x=FALSE)
  roomsDiff$TempDiff <- NULL
  roomsDiff <- merge(roomsDiff, oDiff[,c("rt", "month", "Temp")], by=c("rt", "month"))
  roomsDiff <- melt(roomsDiff, id.vars=c("rt", "month", "rd"))
  roomsDiff$monthdisp <- factor(roomsDiff$month, orderedMonths)
  roomsDiff$rt <- as.POSIXct(roomsDiff$rt, forma="%H:%M", tz="UTC")
  
  # Annotation and plot
  maxDiffRows$rd <- format(maxDiffRows$rd, format="%b %d, %Y")
  maxDiffRows$x <- as.POSIXct(format(strptime("01/01/16 9:00", format="%m/%d/%y %H:%M"), format="%H:%M"), format="%H:%M", tz="UTC")
  maxDiffRows$y <- 65
  maxDiffRows$y2 <- 63
  maxDiffRows$lab1 <-   as.character(paste(maxDiffRows$rd, maxDiffRows$rt, sep=" "))
  maxDiffRows$lab2 <- as.character(round(maxDiffRows$TempDiff,2))
  maxDiffRows$monthdisp <- factor(maxDiffRows$month, orderedMonths)
  maxDiffRows <- maxDiffRows[,c("x", "y", "y2", "lab1", "lab2", "monthdisp")]
  
  pd <- ggplot() +
    geom_line(data=roomsDiff[roomsDiff$variable=="Temp",], aes(x=rt, y=value), color="gray") +
    geom_line(data=roomsDiff[roomsDiff$variable!="Temp",], aes(x=rt, y=value, color=variable), size=0.8) +
    facet_wrap(~monthdisp, ncol=10, drop=FALSE) +
    ylim(60,100) +
    scale_colour_manual(values=colors) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) +
    labs(x="Time", y="") +
    geom_text(data=maxDiffRows, aes(x, y, label=lab1, group=1), color="dimgrey", size=3, hjust=0) + 
    geom_text(data=maxDiffRows, aes(x, y2, label=paste("Max Difference:", lab2, "F", sep=" "), group=1), color="dimgrey", size=3, hjust=0) +
    theme_bw() +
    theme(legend.position="none")
    
  # Specs table
  #labels <- do.call(rbind,lapply(paste0("rlab",1:length(rlist)),get))
  if (length(rlist)==2) {labels <- rbind(rlab1, rlab2)
  } else if (length(rlist)==3) {labels <- rbind(rlab1, rlab2, rlab3)}
  rownames(labels) <- NULL
  pt <- tableGrob(labels, theme=ttheme_minimal())
  
  ### Plot on grid
  grid.arrange(pa,ph,pd,pt, layout_matrix=rbind(c(1), c(1), c(2), c(2), c(3), c(3), c(4)))
  
}
  
