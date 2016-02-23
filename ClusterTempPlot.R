ClusterTempPlot <- function(rlist1, rlist2, var, minTemp) {
  library(reshape2)
  library(ggplot2)
  library(ggthemes) 
  library(gtable)
  library(scales)
  library(grid)
  library(gridExtra) 
  library(plyr)
  library(lubridate)
  
  print(rlist1)
  print(rlist2)
  print(var)
  
  orderedMonths <- c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May")
  colors <- c("#30A2DA", "#FC4F30", "#E5AE38", "#6D904F", "#8B8B8B")
  
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
  
  keepVars <- c("Temp", "Alias", "RoofColor", "Floor", "Orientation", "Landscape", "Overhang", "Date", "Time", "Month")
  g1 <- crt[(crt$Alias %in% rlist1) & (substr(crt$SensorAlias,nchar(crt$SensorAlias)-1,nchar(crt$SensorAlias))!="HD"), keepVars] #group 1
  g2 <- crt[(crt$Alias %in% rlist2) & (substr(crt$SensorAlias,nchar(crt$SensorAlias)-1,nchar(crt$SensorAlias))!="HD"), keepVars] #group 2
  
  ot <- read.csv(file="~/dropbox/rh1/hidoe/outdoortemp.csv", sep=",") #outdoor temperature - already restricted to school day/hours
  names(ot) <- c("DateTime", "OutdoorTemp")
  ot$Date <- as.Date(ot$DateTime, format="%m/%d/%y")
  ot$Time <- format(strptime(ot$DateTime, format="%m/%d/%y %H:%M"), format="%H:%M")
  ot$DateTime <- NULL
  ot$Month <- format(ot$Date, "%B")
  otHot <- ot[ot$OutdoorTemp >= as.numeric(minTemp),] #restrict to minimum outside temperature threshold 
 
  
  #Get each classroom comparison
  for (i in 1:length(rlist1)) {
    assign(paste0("c",i), merge(g1[g1$Alias==rlist1[i],], g2[g2$Alias==rlist2[i],], by=c("Time", "Month", "Date"), suffixes = c("1","2")))
    assign(paste0("c",i), mutate(get(paste0("c",i)), TempDiff=Temp1-Temp2))
  }
  if (length(rlist1) == 1) {names(c1)[names(c1) == "TempDiff"] <- "TempDiff1"} 
 
  #Labels
  if (var=="Floor") {varCols <- c("Floor1", "Floor2")
  } else if (var=="Orientation") {varCols <- c("Orientation1", "Orientation2")
  } else if (var=="Roof Color") {varCols <- c("RoofColor1", "RoofColor2")
  } else if (var=="Landscape") {varCols <- c("Landscape1", "Landscape2")
  } else if (var=="Overhang") {varCols <- c("Overhang1", "Overhang2")}
  l1 <- as.character(c1[2500,varCols][[1]])
  l2 <- as.character(c1[2500,varCols][[2]]) 
  
  #Average monthly differences for all school hour/days
  c <- data.frame(Time=character(),  Month=character(), Date=as.Date(character())) #initialization
  if (length(rlist1) > 1) {
    for (i in 1:length(rlist1)) { #merge TempDiffs into one dataset
      if (i==1) {c <- merge(c, get(paste0("c",i))[,c(1,2,3,18)], by=c("Time", "Month", "Date"), suffixes=c(i,i+1),  all.x=TRUE, all.y=TRUE)} 
      else {c <- merge(c, get(paste0("c",i))[,c(1,2,3,18)], by=c("Time", "Month", "Date"), suffixes=c(i-1,i),  all.x=TRUE, all.y=TRUE)}}    
    
    if(length(rlist1) %% 2 !=0) {names(c)[names(c) == "TempDiff"] <- paste0("TempDiff", length(rlist1))} #make names consistent
    c$AvgTempDiff <- rowMeans(c[,grepl("Diff", names(c))], na.rm=TRUE)
  } else {
    c <- c1
    c$AvgTempDiff <- c$TempDiff
    names(c)[names(c) == "TempDiff"] <- "TempDiff1"
  }
  cHot <- merge(c, otHot[,c("Time", "Month", "Date")], by=c("Time", "Month", "Date")) 
  
  a <- ddply(c, c("Time", "Month"), summarize, totavg=mean(AvgTempDiff, na.rm=TRUE)) #aggregated by time
  aHot <- ddply(cHot, c("Time", "Month"), summarize, totavg=mean(AvgTempDiff, na.rm=TRUE)) #aggregated by time
  
  for (i in 1:length(rlist1)) {
    colName <- paste0("TempDiff",i)
    assign(paste0("a",i), do.call("ddply",list(c, c("Time", "Month"), summarize, avg = call("mean",as.symbol(colName), na.rm=TRUE))))
    assign(paste0("aHot",i), do.call("ddply",list(cHot, c("Time", "Month"), summarize, avg = call("mean",as.symbol(colName), na.rm=TRUE))))
    
    a <- merge(a, get(paste0("a",i)), by=c("Time", "Month"), suffixes=c(i-1,i))
    aHot <- merge(aHot, get(paste0("aHot",i)), by=c("Time", "Month"), suffixes=c(i-1,i))
  }
  if(length(rlist1) %% 2 !=0) {#make name consistent
    names(a)[names(a) == "avg"] <- paste0("avg", length(rlist1))
    names(aHot)[names(aHot) == "avg"] <- paste0("avg", length(rlist1))
  } 
  
  #Annotation 
  maxDiffRows <- do.call("rbind", by(a, a$Month, function(x) x[which.max(abs(x$totavg)),])) #get max of average difference by month
  maxDiffRows$x <- as.POSIXct(format(strptime("01/01/16 11:00", format="%m/%d/%y %H:%M"), format="%H:%M"), format="%H:%M", tz="UTC")
  maxDiffRows$y <- -6
  maxDiffRows$lab <- as.character(round(maxDiffRows$totavg,2))
  maxDiffRows$monthdisp <- factor(maxDiffRows$Month, orderedMonths)
  maxDiffRows <- maxDiffRows[c("monthdisp", "x", "y", "lab")]  
  
  
  a <- melt(a, id.vars = c("Time", "Month"))
  a$monthdisp <- factor(a$Month, orderedMonths)
  a$Time <- as.POSIXct(a$Time, format="%H:%M", tz="UTC")
  
  maxAllDiffRows <- do.call("rbind", by(a, a$Month, function(x) x[which.max(abs(x$value)),])) #get max of all differences by month
  maxAllDiffRows$x<- as.POSIXct(format(strptime("01/01/16 11:00", format="%m/%d/%y %H:%M"), format="%H:%M"), format="%H:%M", tz="UTC")
  maxAllDiffRows$y <- -6.5
  maxAllDiffRows$lab <- as.character(round(maxAllDiffRows$value,2))
  maxAllDiffRows <- maxAllDiffRows[c("monthdisp", "x", "y", "lab")]  
  
  
  plot.title <- paste(var, paste(l1, l2, sep=" vs. "), sep=": ")
  plot.subtitle <- "Temperature Difference Averaged over All Observations" 
  sub <- "Positive temperature difference indicates first listed cluster is hotter."
  
  pa <- ggplot() +
    geom_hline(yintercept = 0, linetype="dotted", color='#FC4F30') + 
    geom_line(data=a[a$variable!="totavg",], aes(x=Time, y=value, group = variable), color="blue", size=0.5, alpha=0.35) +
    geom_line(data=a[a$variable=="totavg",], aes(x=Time, y=value, group=1), color="dimgrey", size=1) +
    facet_wrap(~monthdisp, ncol=10, drop=FALSE) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), minor_breaks=date_breaks("1 hour")) +
    scale_y_continuous(breaks=seq(-8,8,1)) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), atop(.(sub)), "")))) +
    theme_fivethirtyeight() +
    theme(legend.position="none") +
    geom_text(data=maxDiffRows, aes(x, y, label=paste("Maximum Avg. Difference:", lab, "F", sep=" "), group=1), color="dimgrey", size=3, hjust="center") +
    geom_text(data=maxAllDiffRows, aes(x, y, label=paste("Maximum Difference:", lab, "F", sep=" "), group=1), color="dimgrey", size=3, hjust="center")
  
  
  ### Average monthly temperature on school hour/days where outdoor temperature is above certain threshold
  #Annotation 
  maxDiffRowsHot <- do.call("rbind", by(aHot, aHot$Month, function(x) x[which.max(abs(x$totavg)),]))
  maxDiffRowsHot$x <- as.POSIXct(format(strptime("01/01/16 11:00", format="%m/%d/%y %H:%M"), format="%H:%M"), format="%H:%M", tz="UTC")
  maxDiffRowsHot$y <- -6
  maxDiffRowsHot$lab <- as.character(round(maxDiffRowsHot$totavg,2))  
  maxDiffRowsHot$monthdisp <- factor(maxDiffRowsHot$Month, orderedMonths)
  maxDiffRowsHot <- maxDiffRowsHot[c("monthdisp", "x", "y", "lab")]
  
  
  aHot <- melt(aHot, id.vars = c("Time", "Month"))
  aHot$monthdisp <- factor(aHot$Month, orderedMonths)
  aHot$Time <- as.POSIXct(aHot$Time, format="%H:%M", tz="UTC")
  
  
  maxAllDiffRowsHot <- do.call("rbind", by(aHot, aHot$Month, function(x) x[which.max(abs(x$value)),])) #get max of all differences by month
  maxAllDiffRowsHot$x<- as.POSIXct(format(strptime("01/01/16 11:00", format="%m/%d/%y %H:%M"), format="%H:%M"), format="%H:%M", tz="UTC")
  maxAllDiffRowsHot$y <- -6.5
  maxAllDiffRowsHot$lab <- as.character(round(maxAllDiffRowsHot$value,2))
  maxAllDiffRowsHot <- maxAllDiffRowsHot[c("monthdisp", "x", "y", "lab")]  
  
  
  plot2.title <- ""
  plot2.subtitle <- "Temperature Difference Averaged over 'Hot' Observations (>85 F)"
  
  ph <- ggplot() +
    geom_hline(yintercept = 0, linetype="dotted", color='#FC4F30') + 
    geom_line(data=aHot[aHot$variable!="totavg",], aes(x=Time, y=value, group = variable), color="blue", size=0.5, alpha=0.35) +
    geom_line(data=aHot[aHot$variable=="totavg",], aes(x=Time, y=value, group=1), color="dimgrey", size=1) +
    facet_wrap(~monthdisp, ncol=10, drop=FALSE) +
    scale_y_continuous(breaks=seq(-8,8,1)) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), minor_breaks=date_breaks("1 hour")) +
    ggtitle(bquote(atop(.(plot2.title), atop(.(plot2.subtitle), atop(.(sub)), "")))) +
    theme_fivethirtyeight() +
    theme(legend.position="none") +
    geom_text(data=maxDiffRowsHot, aes(x, y, label=paste("Maximum Avg. Difference:", lab, "F", sep=" "), group=1), color="dimgrey", size=3, hjust="center") +
    geom_text(data=maxAllDiffRowsHot, aes(x, y, label=paste("Maximum Difference:", lab, "F", sep=" "), group=1), color="dimgrey", size=3, hjust="center")
  
  
  ### Classroom with biggest difference
  ex <- do.call("rbind", mget(paste0("c",1:length(rlist1)), envir=as.environment(-1)))
  exRows <- do.call("rbind", by(ex, ex$Month, function(x) x[which.max(abs(x$TempDiff)),]))
  
  exagg <- data.frame(Time=character(),  Month=character(), Date=as.Date(character()), Temp1=numeric(), Temp2=numeric()) #initialization
  ann1 <- data.frame(Time=character(), value=numeric(), lab=character(), monthdisp=factor())
  ann2 <- data.frame(Time=character(), value=numeric(), lab=character(), monthdisp=factor())
  for (i in 1:length(orderedMonths)) {
    row <- exRows[exRows$Month==orderedMonths[i],]
    ann1 <- rbind(ann1, data.frame(Time=as.POSIXct(format(strptime("01/01/16 11:00", format="%m/%d/%y %H:%M"), format="%H:%M"), format="%H:%M", tz="UTC"),
                                                        value=94,
                                                        #lab=paste(row$Alias1, l1, sep=": "),
                                                        lab=row$Alias1,
                                                        monthdisp = orderedMonths[i]))
    ann2 <- rbind(ann2, data.frame(Time=as.POSIXct(format(strptime("01/01/16 11:00", format="%m/%d/%y %H:%M"), format="%H:%M"), format="%H:%M", tz="UTC"),
                                                        value=92.5,
                                                        #lab=paste(row$Alias2, l2, sep=": "),
                                                        lab=row$Alias2,
                                                        monthdisp = orderedMonths[i]))
    for (i in 1:length(rlist1)) {
      if (row$Alias1==rlist1[i]) {
        e <- get(paste0("c",i))
        exagg <- rbind(exagg, e[(e$Month==row$Month & e$Date==row$Date),])
      }
    }

  }
  exagg <- merge(exagg[,c("Month", "Date", "Time", "Temp1", "Temp2", "Alias1", "Alias2", varCols)], ot, by=c("Month", "Date", "Time"), all.x=TRUE)
  
  exagg.melt <- melt(exagg[,c("Month", "Date", "Time", "Temp1", "Temp2", "OutdoorTemp"),], id.vars=c("Month", "Date", "Time"))
  exagg.melt$monthdisp <- factor(exagg.melt$Month, orderedMonths)
  exagg.melt$Time <- as.POSIXct(exagg.melt$Time, format="%H:%M", tz="UTC")
  
  exRowsLab <- data.frame(Time=as.POSIXct(format(strptime("01/01/16 11:00", format="%m/%d/%y %H:%M"), format="%H:%M"), format="%H:%M", tz="UTC"),
                          lab=as.character(paste(exRows$Date, exRows$Time, sep=" ")),
                          lab2=as.character(round(exRows$TempDiff,2)),
                          monthdisp=orderedMonths,
                          value=68,
                          value2=66.8)

  plot3.title <- ""
  plot3.subtitle <- "School Day with Greatest Absolute Temperature Difference"
  sub2 <- paste0("BLUE: ", l1, ",  RED: ", l2)
  
  pe <- ggplot() +
    geom_line(data=exagg.melt[exagg.melt$variable=="OutdoorTemp",], aes(x=Time, y=value), color="gray", size=0.8) +
    geom_line(data=exagg.melt[exagg.melt$variable!="OutdoorTemp",], aes(x=Time, y=value, color=variable), size=0.8) +
    facet_wrap(~monthdisp, ncol=10) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), minor_breaks=date_breaks("1 hour")) +
    scale_colour_manual(values=colors) +
    scale_y_continuous(breaks=seq(65,100,5)) + 
    theme_fivethirtyeight() +
    ggtitle(bquote(atop(.(plot3.title), atop(.(plot3.subtitle), atop(.(sub2)), "")))) +
    theme(legend.position="none") +
    geom_text(data=ann1, aes(x=Time, y=value, label=lab), color="#30A2DA", size=3.5, hjust="center") + 
    geom_text(data=ann2, aes(x=Time, y=value, label=lab), color="#FC4F30", size=3.5, hjust="center") +
    geom_text(data=exRowsLab, aes(x=Time, y=value, label=lab), color="dimgrey", size=3, hjust="center") +
    geom_text(data=exRowsLab, aes(x=Time, y=value2, label=paste("Maximum Difference:", lab2, "F", sep=" ")), color="dimgrey", size=3, hjust="center") 
    
    
  
  ### Plot on grid
  grid.arrange(pa,ph,pe, layout_matrix=rbind(c(1), c(2), c(3)))
  
}