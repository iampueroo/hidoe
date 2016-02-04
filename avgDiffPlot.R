avgDiffPlot <- function(rlist1, rlist2, var, minTemp) {
  
  library(reshape2)
  library(lubridate)
  library(plyr)
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
  
  g1 <- allcsv[(allcsv$RoomName %in% rlist1) & (!is.na(allcsv$Temp) & substr(allcsv$Alias,nchar(allcsv$Alias)-1,nchar(allcsv$Alias))!="HD"), keepVars] #group 1
  g2 <- allcsv[(allcsv$RoomName %in% rlist2) & (!is.na(allcsv$Temp) & substr(allcsv$Alias,nchar(allcsv$Alias)-1,nchar(allcsv$Alias))!="HD"), keepVars] #group 2 
  
  o <- read.csv(file="~/dropbox/rh1/hidoe/outdoortemp.csv", sep=",") #outside temperature
  o$rdt <- as.POSIXct(o$DateTime, format="%m/%d/%y %H:%M")
  o$rd <- as.Date(o$DateTime, format="%m/%d/%y")
  o$rt <- format(strptime(o$DateTime, format="%m/%d/%y %H:%M"), format="%H:%M")
  o <- o[-c(1)]
  oHot <- o[o$Temp >= as.numeric(minTemp),] #restrict to outside temperature hotter than specified minimum temperature
  
  # Get each room vs room comparison
  for (i in 1:length(rlist1)) {
    assign(paste0("c",i), merge(g1[g1$RoomName==rlist1[i],], g2[g2$RoomName==rlist2[i],], by=c("rdt", "rt", "month", "rd"), suffixes = c("1","2")))
    assign(paste0("c",i), mutate(get(paste0("c",i)), TempDiff=Temp1-Temp2))
  }
  if (length(rlist1) == 1) {names(c1)[names(c1) == "TempDiff"] <- "TempDiff1"} 

  # Labels
  if (var=="Floor") {
    varCols <- c()
    l1 <- "Ground"
    l2 <- "Top"
  } else {
    if (var=="Roof Color")         {varCols <- c(7,13)
    } else if (var=="Orientation") {varCols <- c(8,14)
    } else if (var=="Landscape")   {varCols <- c(9,15)
    } else if (var=="Overhang")    {varCols <- c(10,16)}
    l1 <- as.character(c1[2500,varCols][[1]])
    l2 <- as.character(c1[2500,varCols][[2]])   
  }  
  
  ### Average monthly temperatures for all school hour/days
  c <- data.frame(rdt=as.POSIXct(character(), format="%m/%d/%y %H:%M"), rt=character(),  month=character(), rd=as.Date(character())) #initialization
  if (length(rlist1) > 1) {
    for (i in 1:length(rlist1)) { #merge TempDiffs into one dataset
      if (i==1) {c <- merge(c, get(paste0("c",i))[c(1,2,3,4,17)], by=c("rdt", "rt", "month", "rd"), suffixes=c(i,i+1),  all.x=TRUE, all.y=TRUE)} 
      else {c <- merge(c, get(paste0("c",i))[c(1,2,3,4,17)], by=c("rdt", "rt", "month", "rd"), suffixes=c(i-1,i),  all.x=TRUE, all.y=TRUE)}}    
    if(length(rlist1) %% 2 !=0) {names(c)[names(c) == "TempDiff"] <- paste0("TempDiff", length(rlist1))} #make names consistent
  } else {c <- c1}
  c$AvgTempDiff <- rowMeans(c[,grepl("Diff", names(c))], na.rm=TRUE)
  cHot <- merge(c, oHot[,c("rdt", "rd", "rt")], by=c("rdt", "rt", "rd")) 
  
  a <- ddply(c, c("rt", "month"), summarise, totavg=mean(AvgTempDiff))
  aHot <- ddply(cHot, c("rt", "month"), summarise, totavg=mean(AvgTempDiff))
  
  for (i in 1:length(rlist1)) {
    colName <- paste0("TempDiff",i)
    assign(paste0("a",i), do.call("ddply",list(c, c("rt", "month"), summarize, avg = call("mean",as.symbol(colName)))))
    assign(paste0("aHot",i), do.call("ddply",list(cHot, c("rt", "month"), summarize, avg = call("mean",as.symbol(colName)))))
    
    a <- merge(a, get(paste0("a",i)), by=c("rt", "month"), suffixes=c(i-1,i))
    aHot <- merge(aHot, get(paste0("aHot",i)), by=c("rt", "month"), suffixes=c(i-1,i))
  }
  if(length(rlist1) %% 2 !=0) {#make name consistent
    names(a)[names(a) == "avg"] <- paste0("avg", length(rlist1))
    names(aHot)[names(aHot) == "avg"] <- paste0("avg", length(rlist1))
  } 
  
  # Begin annotation 
  maxDiffRows <- do.call("rbind", by(a, a$month, function(x) x[which.max(abs(x$totavg)),])) #get max of average difference by month
  rownames(maxDiffRows) <- NULL 

  a <- melt(a, id.vars = c("rt", "month"))
  a$monthdisp <- factor(a$month, orderedMonths)
  a$rt <- as.POSIXct(a$rt, format="%H:%M", tz="UTC")
 
  # Continue annotation
  maxDiffRows$x <- as.POSIXct(format(strptime("01/01/16 9:00", format="%m/%d/%y %H:%M"), format="%H:%M"), format="%H:%M", tz="UTC")
  #maxDiffRows$y <- min(a$value) + 2
  maxDiffRows$y <- -7
  maxDiffRows$lab <- as.character(round(maxDiffRows$totavg,2))
  maxDiffRows$monthdisp <- factor(maxDiffRows$month, orderedMonths)
  maxDiffRows <- maxDiffRows[c("monthdisp", "x", "y", "lab")]
  
  # Find outlier comparisons for lablelling purposes 
  needsLabel <- list()
  for (i in 1:length(rlist1)) {
    if (abs(aggregate(a["value"], list(variable=a$variable), mean, na.rm=TRUE)[i+1,2]) > abs(3*(aggregate(a["value"], list(variable=a$variable), mean, na.rm=TRUE))[1,2])) {
      needsLabel[i] <- 1
      
      # Dynamically input label position based on month and time (adjusted) of the absolute maximum value 
      tmp <- a[a$variable == paste0("avg",i),]
      max <- tmp[which.max(abs(tmp$value)),]
      if (as.character(substr(max$rt,12,13) %in% c('08', '09'))) {max$rt <- max$rt+1.5*60*60
      } else if (as.character(substr(max$rt,12,13) %in% c('13', '14'))) {max$rt <- max$rt-1.5*60*60}
      assign(paste0("ann",i), data.frame(rt=max$rt,
                                         value=max$value,
                                         lab=paste(paste0(paste0(substr(rlist1[i],1,1),"-"), sub(".*- ", "", rlist1[i])),paste0(paste0(substr(rlist2[i],1,1),"-"),sub(".*- ", "", rlist2[i])), sep=" vs. "),
                                         monthdisp = factor(max$month, levels=orderedMonths)))
    }
  }

  plot.title <- paste(var, paste(l1, l2, sep=" vs. "), sep=": ")
  plot.subtitle <- "(positive temperature difference indicates first-listed cluster is hotter)"
  
  pa <- ggplot() +
    geom_hline(yintercept = 0, linetype="dotted", alpha=0.5, color='#FC4F30') + 
    geom_line(data=a[a$variable!="totavg",], aes(x=rt, y=value, group = variable), color="#30A2DA", size=0.5, alpha=0.35) +
    geom_line(data=a[a$variable=="totavg",], aes(x=rt, y=value, group=1), color="dimgrey", size=1) +
    facet_wrap(~monthdisp, ncol=10, drop=FALSE) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) +
    labs(x="", y="") +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_bw() +
    theme(legend.position="none") +
    geom_text(data=maxDiffRows, aes(x, y, label=paste("Max Avg Difference:", lab, "F", sep=" "), group=1), color="dimgrey", size=3, hjust=0)
  
  for (i in 1:length(rlist1)) {
    if (exists(paste0("ann",i))) {
      pa <- pa + geom_label(data=get(paste0("ann",i)), aes(x=rt, y=value, label=lab, group=1), color="#30A2DA", size=3, alpha=0.5)
    }
  }
  
  ### Average monthly temperature on school hour/days where outdoor temperature is above certain threshold
  # Begin annotation 
  maxDiffRowsHot <- do.call("rbind", by(aHot, aHot$month, function(x) x[which.max(abs(x$totavg)),]))
  rownames(maxDiffRowsHot) <- NULL 
  
  aHot <- melt(aHot, id.vars = c("rt", "month"))
  aHot$monthdisp <- factor(aHot$month, orderedMonths)
  aHot$rt <- as.POSIXct(aHot$rt, format="%H:%M", tz="UTC")
  
  # Continue annotation 
  maxDiffRowsHot$x <- as.POSIXct(format(strptime("01/01/16 9:00", format="%m/%d/%y %H:%M"), format="%H:%M"), format="%H:%M", tz="UTC")
  #maxDiffRowsHot$y <- min(aHot$value) + 2
  maxDiffRowsHot$y <- -7
  maxDiffRowsHot$lab <- as.character(round(maxDiffRowsHot$totavg,2))  
  maxDiffRowsHot$monthdisp <- factor(maxDiffRowsHot$month, orderedMonths)
  maxDiffRowsHot <- maxDiffRowsHot[c("monthdisp", "x", "y", "lab")]
  
  ph <- ggplot() +
    geom_hline(yintercept = 0, linetype="dotted", alpha=0.5, color='#FC4F30') + 
    geom_line(data=aHot[aHot$variable!="totavg",], aes(x=rt, y=value, group = variable), color="#30A2DA", size=0.5, alpha=0.35) +
    geom_line(data=aHot[aHot$variable=="totavg",], aes(x=rt, y=value, group=1), color="dimgrey", size=1) +
    facet_wrap(~monthdisp, ncol=10, drop=FALSE) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) +
    labs(x="Time", y="") +
    theme_bw() +
    theme(legend.position="none") +
    geom_text(data=maxDiffRowsHot, aes(x, y, label=paste("Max Avg Difference:", lab, "F", sep=" "), group=1), color="dimgrey", size=3, hjust=0)
  
  ### Plot on grid
  grid.arrange(pa,ph, layout_matrix=rbind(c(1), c(2)), 
               left=textGrob(expression(paste("Temperature Difference (", degree ~ F, ")")), rot=90, vjust=1, gp=gpar(fontsize=12)))
  
}
  