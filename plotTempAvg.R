avgTempPlot <- function(rlist, minTemp) {
  
  library(plyr)
  library(lubridate)
  library(ggplot2)
  library(ggthemes) 
  library(gtable)
  library(scales)
  library(grid)
  library(gridExtra) 
  
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
  #allcsv <- allcsv[(as.character(substr(allcsv$rdt, 12, 13)) %in% c('08', '09', '10', '11', '12', '13') | allcsv$rt=="14:00"),]
  
  
  ### Average monthly temperatures for all school hour/days
  o <- read.csv(file="~/dropbox/roundhouseone/HIDOE thermal comfort/outdoortemp.csv", sep=",")
  o$rdt <- as.POSIXct(o$DateTime, format="%m/%d/%y %H:%M")
  o$rd <- as.Date(o$DateTime, format="%m/%d/%y")
  o$rt <- format(strptime(o$DateTime, format="%m/%d/%y %H:%M"), format="%H:%M")
  o$month <- format(o$rd, "%B")
  o <- o[-c(1)]
  
  o.sum <- data.frame(ddply(o, c("rt", "month"), summarise, avg=mean(Temp)))
  o.sum$monthdisp <- factor(o.sum$month, c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May"))
  o.sum$rt <- as.POSIXct(o.sum$rt, format="%H:%M", tz="UTC")
  
  for (i in 1:length(rlist)) {
    print(rlist[i])
    assign(paste0("r",i,".name"), rlist[i])
    assign(paste0("room",i), allcsv[(allcsv$RoomName==rlist[i]) & (!is.na(allcsv$Temp) & (substr(allcsv$Alias,nchar(allcsv$Alias)-1,nchar(allcsv$Alias))!="HD")),]) 
    assign(paste0("room",i,".sum"), ddply(get(paste0("room",i)), c("rt", "month"), summarize, avg=mean(Temp)))
    assign(paste0("room",i,".sum"), mutate(get(paste0("room",i,".sum")), monthdisp=factor(month, c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May"))))
    assign(paste0("room",i,".sum"), mutate(get(paste0("room",i,".sum")), rt=as.POSIXct(rt, format="%H:%M", tz="UTC")))
  }
 
  pa <- ggplot() +
    geom_line(data=o.sum, aes(x=rt, y=avg, group=1), color="gray") +
    geom_line(data=room1.sum, aes(x=rt, y=avg, group=1), color="dodgerblue4") +
    geom_line(data=room2.sum, aes(x=rt, y=avg, group=1), color="firebrick") +
    geom_line(data=room3.sum, aes(x=rt, y=avg, group=1), color="springgreen4") +
    facet_wrap(~monthdisp, ncol=10) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) +
    ylim(70,95) +
    theme_bw() 
  
  if (length(rlist) == 2) {
    pa + labs(x="", y="", title=paste(r1.name, r2.name, sep=" vs. "))
  } else {
    pa + labs(x="", y="", title=paste(r1.name, r2.name, r3.name, sep=" vs. "))
  }
  
  
  ### Average monthly temperature on school hour/days where outdoor temperature is above certain threshold
  hoto <- o[o$Temp >= as.numeric(minTemp),]
  hoto.sum <- ddply(hoto, c("rt", "month"), summarise, avg=mean(Temp))
  hoto.sum$monthdisp <- factor(hoto.sum$month, c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May"))
  hoto.sum$rt <- as.POSIXct(hoto.sum$rt, format="%H:%M", tz="UTC") 
  
  for (i in 1:length(rlist)) {
    assign(paste0("hotroom",i), merge(get(paste0("room",i)), hoto[,c("rdt", "rd", "rt")], by=c("rdt", "rd", "rt"), all.x=FALSE))
    assign(paste0("hotroom",i,".sum"), ddply(get(paste0("hotroom",i)), c("rt", "month"), summarize, avg=mean(Temp)))
    assign(paste0("hotroom",i,".sum"), mutate(get(paste0("hotroom",i,".sum")), monthdisp=factor(month, c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May"))))
    assign(paste0("hotroom",i,".sum"), mutate(get(paste0("hotroom",i,".sum")), rt=as.POSIXct(rt, format="%H:%M", tz="UTC")))
  }
  
  #figure out how to set limits on x-axis
  ph <- ggplot() +
    geom_line(data=hoto.sum, aes(x=rt, y=avg, group=1), color="gray") +
    geom_line(data=hotroom1.sum, aes(x=rt, y=avg, group=1), color="dodgerblue4") +
    geom_line(data=hotroom2.sum, aes(x=rt, y=avg, group=1), color="firebrick") +
    facet_wrap(~monthdisp, ncol=10, drop=FALSE) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) +
    ylim(70,95) +
    labs(x="", y=expression(paste("Temperature ( ", degree ~ F, " )"))) +
    theme_bw()
  if (length(rlist) == 3) {ph + geom_line(data=hotroom3.sum, aes(x=rt, y=avg, group=1), color="springgreen4")}
  
   
  ###  Monthly school hour/day with largest temperature differential between classrooms 
  d <- merge(room1[,c("rd", "rt", "month", "Temp")], room2[,c("rd", "rt", "Temp")], by=c("rd", "rt"), suffixes=c("1","2"))
  if (length(rlist==3)) {
    colnames(room3)[5] <- "Temp3"
    d <- merge(d, room3[,c("rd", "rt","Temp3")], by=c("rd", "rt"))
    d$tempDiff <- d$Temp1-d$Temp3
  } else {d$TempDiff <- d$Temp1 - d$Temp2}
  maxDiffRows <- do.call("rbind", by(d, d$month, function(d) d[which.max(abs(d$tempDiff)),]))
  rownames(maxDiffRows) <- NULL 
  
  hottesto <- merge(o, maxDiffRows[c(1,3)], by=c("rd", "month"), all.x=FALSE)
  hottesto$monthdisp <- factor(hottesto$month, c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May"))
  hottesto$rt <- as.POSIXct(hottesto$rt, format="%H:%M", tz="UTC")
  
  for (i in 1:length(rlist)) {
    assign(paste0("hottestroom",i), merge(get(paste0("room",i)), maxDiffRows[,c(1,3)], by=c("rd", "month"), all.x=FALSE))
    assign(paste0("hottestroom",i), mutate(get(paste0("hottestroom",i)), monthdisp=factor(month, c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May"))))
    assign(paste0("hottestroom",i), mutate(get(paste0("hottestroom",i)), rt=as.POSIXct(rt, format="%H:%M", tz="UTC")))
  }
  
 
  # Annotation and plot
  maxDiffRows$rd <- format(maxDiffRows$rd, format="%b %d, %Y")
  maxDiffRows$x <- as.POSIXct(format(strptime("01/01/16 9:00", format="%m/%d/%y %H:%M"), format="%H:%M"), format="%H:%M", tz="UTC")
  maxDiffRows$y <- 65
  maxDiffRows$y2 <- 63
  maxDiffRows$lab1 <-   as.character(paste(maxDiffRows$rd, maxDiffRows$rt, sep=" "))
  maxDiffRows$lab2 <- as.character(round(maxDiffRows$tempDiff,2))
  maxDiffRows$monthdisp <- factor(maxDiffRows$month, c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May"))
  maxDiffRows <- maxDiffRows[-c(1,2,4,5,6)]

  
  pd <- ggplot() +
    geom_line(data=hottesto, aes(x=rt, y=Temp, group=1), color="gray") +
    geom_line(data=hottestroom1, aes(x=rt, y=Temp1, group=1), color="dodgerblue4") +
    geom_line(data=hottestroom2,  aes(x=rt, y=Temp2, group=1), color="firebrick") +
    facet_wrap(~monthdisp, ncol=10, drop=FALSE) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) +
    ylim(60,100) +
    labs(x="Time", y="") +
    theme_bw() +
    geom_text(data=maxDiffRows, aes(x, y, label=lab1, group=1), color="dimgrey", size=3, hjust=0) + 
    geom_text(data=maxDiffRows, aes(x, y2, label=paste("Max Difference:", lab2, "F", sep=" "), group=1), color="dimgrey", size=3, hjust=0)
  if (length(rlist) == 3) {pd + geom_line(data=hottestroom3, aes(x=rt, y=Temp3, group=1), color="springgreen4")}
 
   
  room1.label <- room1[1,c(8,9,10,11,12,13,14,15)]
  room2.label <- room2[1,c(8,9,10,11,12,13,14,15)]
  labels <- rbind(room1.label, room2.label)
  if (length(rlist) == 3) {
    room3.label <- room3[1,c(8,9,10,11,12,13,14,15)]
    labels <- rbind(labels, room3.label)
  }
  rownames(labels) <- NULL
  pt <- tableGrob(labels, theme=ttheme_minimal())
    
 
  ### Plot on grid
  grid.arrange(pa,ph,pd,pt, layout_matrix=rbind(c(1), c(2), c(3), c(4)))
  
  
}
  