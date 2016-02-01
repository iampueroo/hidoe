avgDiffPlot <- function(rlist1, rlist2, var, minTemp) {
  
  library(reshape2)
  library(plyr)
  library(lubridate)
  library(ggplot2)
  library(ggthemes) 
  library(gtable)
  library(scales)
  library(grid)
  library(gridExtra) 
  
  print(rlist1)
  print(rlist2)
  print(var)
  
  if (var=="Roof Color") {varCols <- c(7,13)
  } else if (var=="Orientation") {varCols <- c(8,14)
  } else if (var=="Landscape") {varCols <- c(9,15)
  } else if (var=="Overhang") {varCols <- c(10,16)
  } else if (var=="Floor") {varCols <- c()}
  
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
  #allcsv <<- allcsv[(as.character(substr(allcsv$rdt, 12, 13)) %in% c('08', '09', '10', '11', '12', '13') | allcsv$rt=="14:00"),]
  
  # Outside temperature 
  o <- read.csv(file="~/dropbox/roundhouseone/HIDOE thermal comfort/outdoortemp.csv", sep=",")
  o$rdt <- as.POSIXct(o$DateTime, format="%m/%d/%y %H:%M")
  o$rd <- as.Date(o$DateTime, format="%m/%d/%y")
  o$rt <- format(strptime(o$DateTime, format="%m/%d/%y %H:%M"), format="%H:%M")
  o <- o[-c(1)]
  
  
  ### Average monthly temperatures for all school hour/days
  allcsv$Alias <- as.character(allcsv$Alias)
  g1 <- allcsv[(allcsv$RoomName %in% rlist1) & (!is.na(allcsv$Temp) & substr(allcsv$Alias,nchar(allcsv$Alias)-1,nchar(allcsv$Alias))!="HD"),c(1,5,8,9,13,14,15,16,17,18)]
  g2 <- allcsv[(allcsv$RoomName %in% rlist2) & (!is.na(allcsv$Temp) & substr(allcsv$Alias,nchar(allcsv$Alias)-1,nchar(allcsv$Alias))!="HD"),c(1,5,8,9,13,14,15,16,17,18)]

  for (i in 1:length(rlist1)) {
    assign(paste0("c",i), merge(g1[g1$RoomName==rlist1[i],], g2[g2$RoomName==rlist2[i],], by=c("rdt", "rt", "month", "rd"), suffixes = c("1","2")))
    assign(paste0("c",i), mutate(get(paste0("c",i)), TempDiff=Temp1-Temp2))
  }

  # Labels
  if (var=="Floor") {
    l1 <- "First"
    l2 <- "Second"
  } else {
   l1 <- as.character(c1[2500,varCols][[1]])
   l2 <- as.character(c1[2500,varCols][[2]]) 
  }
  
  c <- data.frame(rdt=as.POSIXct(character(), format="%m/%d/%y %H:%M"), rt=character(),  month=character(), rd=as.Date(character()))
  if (length(rlist1)>1) {
    for (i in 1:length(rlist1)) {
      other <- get(paste0("c",i))
      if (i==1) {c <- merge(c, other[c(1,2,3,4,5,11,17)], by=c("rdt", "rt", "month", "rd"), suffixes=c(i,i+1),  all.x=TRUE, all.y=TRUE)} 
      else {c <- merge(c, other[c(1,2,3,4,5,11,17)], by=c("rdt", "rt", "month", "rd"), suffixes=c(i-1,i),  all.x=TRUE, all.y=TRUE)}
    }    
    c$AvgTempDiff <- rowMeans(c[,grepl("Diff", names(c))], na.rm=TRUE)
  } else {
    c <- c1
    c$AvgTempDiff <- c1$TempDiff
  }

  c.sum <- ddply(c, c("rt", "month"), summarise, totavg=mean(AvgTempDiff))
  for (i in 1:length(rlist1)) {
    j <- i-1
    colName <- paste0("TempDiff",i)
    assign(paste0("c",i,".sum"), do.call("ddply",list(c, c("rt", "month"), summarize, avg = call("mean",as.symbol(colName)))))
    other <- get(paste0("c",i,".sum"))
    c.sum <- merge(c.sum, other, by=c("rt", "month"), suffixes=c(i,i-1))
  }
  
  c.sum <- melt(c.sum, id.vars = c("rt", "month"))
  c.sum$monthdisp <- factor(c.sum$month, c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May"))
  c.sum$rt <- as.POSIXct(c.sum$rt, format="%H:%M", tz="UTC")
  
  plot.title <- paste(var, paste(l1, l2, sep=" vs. "), sep=": ")
  plot.subtitle <- "(temperature difference defined as first group minus second group)"
  
  pa <- ggplot() +
    geom_hline(yintercept = 0, linetype="dotted", alpha=0.5, color='firebrick') + 
    geom_line(data=c.sum[c.sum$variable!="totavg",], aes(x=rt, y=value, group = variable), color="blue", size=0.5, alpha=0.3) +
    geom_line(data=c.sum[c.sum$variable=="totavg",], aes(x=rt, y=value, group=1), color="dimgrey", size=1) +
    facet_wrap(~monthdisp, ncol=10, drop=FALSE) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) +
    labs(x="", y=expression(paste("Temperature Difference ( ", degree ~ F, " )"))) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_bw() 

 
  ### Average monthly temperature on school hour/days where outdoor temperature is above certain threshold
  hoto <- o[o$Temp >= as.numeric(minTemp),]
  hotc <- merge(c, hoto[,c("rdt", "rd", "rt")], by=c("rdt", "rt", "rd"))
  
  hotc.sum <- ddply(hotc, c("rt", "month"), summarise, totavg=mean(AvgTempDiff))
  for (i in 1:length(rlist1)) {
    j <- i-1
    colName <- paste0("TempDiff",i)
    assign(paste0("hotc",i,".sum"), do.call("ddply",list(hotc, c("rt", "month"), summarize, avg = call("mean",as.symbol(colName)))))
    other <- get(paste0("hotc",i,".sum"))
    hotc.sum <- merge(hotc.sum, other, by=c("rt", "month"), suffixes=c(i,i-1))
  }
  
  hotc.sum <- melt(hotc.sum, id.vars = c("rt", "month"))
  hotc.sum$monthdisp <- factor(hotc.sum$month, c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May"))
  hotc.sum$rt <- as.POSIXct(hotc.sum$rt, format="%H:%M", tz="UTC")
  
  ph <- ggplot() +
    geom_hline(yintercept = 0, linetype="dotted", alpha=0.5, color='firebrick') + 
    geom_line(data=hotc.sum[hotc.sum$variable!="totavg",], aes(x=rt, y=value, group = variable), color="blue", size=0.5, alpha=0.3) +
    geom_line(data=hotc.sum[hotc.sum$variable=="totavg",], aes(x=rt, y=value, group=1), color="dimgrey", size=1) +
    facet_wrap(~monthdisp, ncol=10, drop=FALSE) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) +
    labs(x="Time", y=expression(paste("Temperature Difference ( ", degree ~ F, " )"))) +
    theme_bw()
  
  
  ### Plot on grid
  grid.arrange(pa,ph, layout_matrix=rbind(c(1), c(2)))
  
}
  