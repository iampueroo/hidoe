avgDiffPlot <- function(rlist1, rlist2, var, minTemp) {
  
  library(reshape2)
  library(plyr)
  library(lubridate)
  library(ggplot2)
  library(ggthemes) 
  library(scales)
  library(grid)
  library(gridExtra) 
  library(gtable)
  
  print(rlist1)
  print(rlist2)
  print(var)
  
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
  keepVars <- c("Temp", "RoomName", "RoofColor", "Orientation", "Landscape", "Overhang", "rdt", "rd", "rt", "month")
  colors <- c("#30A2DA", "#FC4F30", "#E5AE38", "#6D904F", "#8B8B8B")
  
  if (var=="Roof Color") {varCols <- c(7,13)
  } else if (var=="Orientation") {varCols <- c(8,14)
  } else if (var=="Landscape") {varCols <- c(9,15)
  } else if (var=="Overhang") {varCols <- c(10,16)
  } else if (var=="Floor") {varCols <- c()} 
  
  
  # Outside temperature 
  o <- read.csv(file="~/dropbox/rh1/hidoe/outdoortemp.csv", sep=",")
  o$rdt <- as.POSIXct(o$DateTime, format="%m/%d/%y %H:%M")
  o$rd <- as.Date(o$DateTime, format="%m/%d/%y")
  o$rt <- format(strptime(o$DateTime, format="%m/%d/%y %H:%M"), format="%H:%M")
  o <- o[-c(1)]
  oHot <- o[o$Temp >= as.numeric(minTemp),] #restrict to outside temperature hotter than specified minimum temperature
  
  g1 <- allcsv[(allcsv$RoomName %in% rlist1) & (!is.na(allcsv$Temp) & substr(allcsv$Alias,nchar(allcsv$Alias)-1,nchar(allcsv$Alias))!="HD"), keepVars]
  g2 <- allcsv[(allcsv$RoomName %in% rlist2) & (!is.na(allcsv$Temp) & substr(allcsv$Alias,nchar(allcsv$Alias)-1,nchar(allcsv$Alias))!="HD"), keepVars]
  
  # Get each comparison
  for (i in 1:length(rlist1)) {
    assign(paste0("c",i), merge(g1[g1$RoomName==rlist1[i],], g2[g2$RoomName==rlist2[i],], by=c("rdt", "rt", "month", "rd"), suffixes = c("1","2")))
    assign(paste0("c",i), mutate(get(paste0("c",i)), TempDiff=Temp1-Temp2))
  }
  if (length(rlist1) == 1) {names(c1)[names(c1) == "TempDiff"] <- "TempDiff1"} 

  # Labels
  if (var=="Floor") {
    l1 <- "First"
    l2 <- "Second"
  } else {
   l1 <- as.character(c1[2500,varCols][[1]])
   l2 <- as.character(c1[2500,varCols][[2]]) 
  }
  
  ### Average monthly temperatures for all school hour/days
  c <- data.frame(rdt=as.POSIXct(character(), format="%m/%d/%y %H:%M"), rt=character(),  month=character(), rd=as.Date(character()))
  if (length(rlist1) > 1) {
    for (i in 1:length(rlist1)) {
      if (i==1) {c <- merge(c, get(paste0("c",i))[c(1,2,3,4,17)], by=c("rdt", "rt", "month", "rd"), suffixes=c(i,i+1),  all.x=TRUE, all.y=TRUE)} 
      else {c <- merge(c, get(paste0("c",i))[c(1,2,3,4,17)], by=c("rdt", "rt", "month", "rd"), suffixes=c(i-1,i),  all.x=TRUE, all.y=TRUE)}
    }    
    if(length(rlist1) %% 2 !=0) {names(c)[names(c) == "TempDiff"] <- paste0("TempDiff", length(rlist1))} #make names consistent
  } else {c <- c1}
  c$AvgTempDiff <- rowMeans(c[,grepl("Diff", names(c))], na.rm=TRUE)
  
  cAgg <- ddply(c, c("rt", "month"), summarise, totavg=mean(AvgTempDiff))
  for (i in 1:length(rlist1)) {
    colName <- paste0("TempDiff",i)
    assign(paste0("cAgg",i), do.call("ddply",list(c, c("rt", "month"), summarize, avg = call("mean",as.symbol(colName)))))
    cAgg <- merge(cAgg, get(paste0("cAgg",i)), by=c("rt", "month"), suffixes=c(i-1,i))
  }
  if(length(rlist1) %% 2 !=0) {names(cAgg)[names(Aagg) == "avg"] <- paste0("avg", length(rlist1))}

  #Annotation 
  maxDiffRows <- do.call("rbind", by(cAgg, cAgg$month, function(x) x[which.max(abs(x$totavg)),]))
  rownames(maxDiffRows) <- NULL 
  maxDiffRows$x <- as.POSIXct(format(strptime("01/01/16 9:00", format="%m/%d/%y %H:%M"), format="%H:%M"), format="%H:%M", tz="UTC")
  #maxDiffRows$y <- min(c.sum$totavg) - 2
  maxDiffRows$y <- -7
  maxDiffRows$lab <- as.character(round(maxDiffRows$totavg,2))
  maxDiffRows$monthdisp <- factor(maxDiffRows$month, orderedMonths)
  maxDiffRows <- maxDiffRows[c("monthdisp", "x", "y", "lab")]
  
  cAgg <- melt(cAgg, id.vars = c("rt", "month"))
  cAgg$monthdisp <- factor(cAgg$month, orderedMonths)
  cAgg$rt <- as.POSIXct(cAgg$rt, format="%H:%M", tz="UTC")
  
  needsLabel <- list()
  for (i in 1:length(rlist1)) {
    if (abs(aggregate(cAgg["value"], list(variable=cAgg$variable), mean, na.rm=TRUE)[i+1,2]) > abs(3*(aggregate(cAgg["value"], list(variable=cAgg$variable), mean, na.rm=TRUE))[1,2])) {
      needsLabel[i] <- 1
    }
  }
  
  plot.title <- paste(var, paste(l1, l2, sep=" vs. "), sep=": ")
  plot.subtitle <- "(positive temperature difference indicates first-listed cluster is hotter)"
  
  pa <- ggplot() +
    geom_hline(yintercept = 0, linetype="dotted", alpha=0.5, color='firebrick') + 
    geom_line(data=cAgg[cAgg$variable!="totavg",], aes(x=rt, y=value, group = variable), color="#30A2DA", size=0.5, alpha=0.35) +
    geom_line(data=cAgg[cAgg$variable=="totavg",], aes(x=rt, y=value, group=1), color="dimgrey", size=1) +
    facet_wrap(~monthdisp, ncol=10, drop=FALSE) +
    scale_colour_manual(values=colors) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) +
    ylim(-8,8) +
    labs(x="", y="") +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_bw() +
    geom_text(data=maxDiffRows, aes(x, y, label=paste("Max Difference:", lab, "F", sep=" "), group=1), color="dimgrey", size=3, hjust=0)
  

  ### Average monthly temperature on school hour/days where outdoor temperature is above certain threshold
  hotc <- merge(c, hoto[,c("rdt", "rd", "rt")], by=c("rdt", "rt", "rd"))
  
  hotc.sum <- ddply(hotc, c("rt", "month"), summarise, totavg=mean(AvgTempDiff))
  for (i in 1:length(rlist1)) {
    colName <- paste0("TempDiff",i)
    assign(paste0("hotc",i,".sum"), do.call("ddply",list(hotc, c("rt", "month"), summarize, avg = call("mean",as.symbol(colName)))))
    other <- get(paste0("hotc",i,".sum"))
    hotc.sum <- merge(hotc.sum, other, by=c("rt", "month"), suffixes=c(i-1,i))
  }
  if(length(rlist1) %% 2 !=0) {names(hotc.sum)[names(hotc.sum) == "avg"] <- paste0("avg", length(rlist1))}
  
  #Annotation 
  hotMaxDiffRows <- do.call("rbind", by(hotc.sum, hotc.sum$month, function(x) x[which.max(abs(x$totavg)),]))
  rownames(hotMaxDiffRows) <- NULL 
  hotMaxDiffRows$x <- as.POSIXct(format(strptime("01/01/16 9:00", format="%m/%d/%y %H:%M"), format="%H:%M"), format="%H:%M", tz="UTC")
  #hotMaxDiffRows$y <- min(hotc.sum$totavg) - 2
  hotMaxDiffRows$y <- -7
  hotMaxDiffRows$lab <- as.character(round(hotMaxDiffRows$totavg,2))
  hotMaxDiffRows$monthdisp <- factor(hotMaxDiffRows$month, c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May"))
  hotMaxDiffRows <- hotMaxDiffRows[c("monthdisp", "x", "y", "lab")]
  
  hotc.sum <- melt(hotc.sum, id.vars = c("rt", "month"))
  hotc.sum$monthdisp <- factor(hotc.sum$month, c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May"))
  hotc.sum$rt <- as.POSIXct(hotc.sum$rt, format="%H:%M", tz="UTC")
  
  ph <- ggplot() +
    geom_hline(yintercept = 0, linetype="dotted", alpha=0.5, color='firebrick') + 
    geom_line(data=hotc.sum[hotc.sum$variable!="totavg",], aes(x=rt, y=value, group = variable), color="blue", size=0.5, alpha=0.3) +
    geom_line(data=hotc.sum[hotc.sum$variable=="totavg",], aes(x=rt, y=value, group=1), color="dimgrey", size=1) +
    facet_wrap(~monthdisp, ncol=10, drop=FALSE) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) +
    ylim(-8,8) +
    labs(x="Time", y=expression(paste("Temperature Difference ( ", degree ~ F, " )"))) +
    theme_bw() +
    geom_text(data=hotMaxDiffRows, aes(x, y, label=paste("Max Difference:", lab, "F", sep=" "), group=1), color="dimgrey", size=3, hjust=0)
  
  
  
  ### Plot on grid
  grid.arrange(pa,ph, layout_matrix=rbind(c(1), c(2)))
  
}
  