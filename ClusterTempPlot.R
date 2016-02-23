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
  crt <- merge(crt[,c(1,2,3,4)], arch[,c("RoomID", "Alias", "Floor", "RoofColor", "SQFT", "Orientation", "Landscape", "Overhang")], by="RoomID", all.x=TRUE)
  
  crt$Date <- as.Date(crt$StartTime, format="%m/%d/%y")
  crt$Time <- format(strptime(crt$StartTime, format="%m/%d/%y %H:%M"), format="%H:%M")
  crt$Month <- format(crt$Date, "%B") 
  crt <- join(crt, sdates, by="Date", type="right", match="all") #restrict to school days
  crt <- join(crt, shours, by="Time", type="right", match="all") #restrict to school hours
  crt$SensorAlias <- as.character(crt$SensorAlias)
  crt$StartTime <<- NULL
  
  keepVars <- c("Temp", "Alias", "RoofColor", "Floor", "Orientation", "Landscape", "Overhang", "Date", "Time", "Month")
  g1 <- crt[(crt$Alias %in% rlist1) & (!is.na(crt$Temp) & substr(crt$Alias,nchar(crt$SensorAlias)-1,nchar(crt$SensorAlias))!="HD"), keepVars] #group 1
  g2 <- crt[(crt$Alias %in% rlist2) & (!is.na(crt$Temp) & substr(crt$Alias,nchar(crt$SensorAlias)-1,nchar(crt$SensorAlias))!="HD"), keepVars] #group 2
  
  ot <- read.csv(file="~/dropbox/rh1/hidoe/outdoortemp.csv", sep=",") #outdoor temperature - already restricted to school day/hours
  names(ot) <- c("DateTime", "OutdoorTemp")
  ot$Date <- as.Date(ot$DateTime, format="%m/%d/%y")
  ot$Time <- format(strptime(ot$DateTime, format="%m/%d/%y %H:%M"), format="%H:%M")
  ot$DateTime <- NULL
  ot$Month <- format(ot$Date, "%B")
  otHot <- ot[ot$OutdoorTemp >= as.numeric(minTemp),] #restrict to minimum outside temperature threshold 
 
  o <- data.frame(ddply(ot, c("Time", "Month"), summarize, AvgOutTemp=mean(OutdoorTemp)))
  
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
      else {c <- merge(c, get(paste0("c",i))[,c(1,2,3,18)], by=c("Time", "Month", "Date"), suffixes=c(i-1,i),  all.x=TRUE, all.y=TRUE)}
    }    
    
    if(length(rlist1) %% 2 !=0) {names(c)[names(c) == "TempDiff"] <- paste0("TempDiff", length(rlist1))} #make names consistent
    c$AvgTempDiff <- rowMeans(c[,grepl("Diff", names(c))], na.rm=TRUE)
  } else {
    c <- c1
    c$AvgTempDiff <- c$TempDiff
    names(c)[names(c) == "TempDiff"] <- "TempDiff1"
  }
  cHot <- merge(c, otHot[,c("Time", "Month", "Date")], by=c("Time", "Month", "Date")) 
  
  a <- ddply(c, c("Time", "Month"), summarize, totavg=mean(AvgTempDiff), na.rm=TRUE) #aggregated by time
  aHot <- ddply(cHot, c("Time", "Month"), summarize, totavg=mean(AvgTempDiff), na.rm=TRUE) #aggregated by time
  
  for (i in 1:length(rlist1)) {
    colName <- paste0("TempDiff",i)
    assign(paste0("a",i), do.call("ddply",list(c, c("Time", "Month"), summarize, avg = call("mean",as.symbol(colName)))))
    assign(paste0("aHot",i), do.call("ddply",list(cHot, c("Time", "Month"), summarize, avg = call("mean",as.symbol(colName)))))
    
    a <- merge(a, get(paste0("a",i)), by=c("Time", "Month"), suffixes=c(i-1,i))
    aHot <- merge(aHot, get(paste0("aHot",i)), by=c("Time", "Month"), suffixes=c(i-1,i))
  }
  if(length(rlist1) %% 2 !=0) {#make name consistent
    names(a)[names(a) == "avg"] <- paste0("avg", length(rlist1))
    names(aHot)[names(aHot) == "avg"] <- paste0("avg", length(rlist1))
  } 
  
  # Begin annotation 
  maxDiffRows <- do.call("rbind", by(a, a$Month, function(x) x[which.max(abs(x$totavg)),])) #get max of average difference by month
  rownames(maxDiffRows) <- NULL 
  
  a <- melt(a, id.vars = c("Time", "Month"))
  a$monthdisp <- factor(a$Month, orderedMonths)
  a$Time <- as.POSIXct(a$Time, format="%H:%M", tz="UTC")
  
  # Continue annotation
  maxDiffRows$x <- as.POSIXct(format(strptime("01/01/16 9:00", format="%m/%d/%y %H:%M"), format="%H:%M"), format="%H:%M", tz="UTC")
  maxDiffRows$y <- -7
  maxDiffRows$lab <- as.character(round(maxDiffRows$totavg,2))
  maxDiffRows$monthdisp <- factor(maxDiffRows$Month, orderedMonths)
  maxDiffRows <- maxDiffRows[c("monthdisp", "x", "y", "lab")]
  
  plot.title <- paste(var, paste(l1, l2, sep=" vs. "), sep=": ")
  plot.subtitle <- "(Positive temperature difference indicates first listed cluster is hotter.)"
  
  pa <- ggplot() +
    geom_hline(yintercept = 0, linetype="dotted", color='#FC4F30') + 
    geom_line(data=a[a$variable!="totavg",], aes(x=Time, y=value, group = variable), color="#30A2DA", size=0.5, alpha=0.35) +
    geom_line(data=a[a$variable=="totavg",], aes(x=Time, y=value, group=1), color="dimgrey", size=1) +
    facet_wrap(~monthdisp, ncol=10, drop=FALSE) +
    scale_y_continuous(breaks=seq(-8,8,2)) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) +
    labs(x="", y="") +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_bw() +
    theme(legend.position="none") +
    geom_text(data=maxDiffRows, aes(x, y, label=paste("Max Avg Difference:", lab, "F", sep=" "), group=1), color="dimgrey", size=3, hjust=0)
  
  
  ### Average monthly temperature on school hour/days where outdoor temperature is above certain threshold
  # Begin annotation 
  maxDiffRowsHot <- do.call("rbind", by(aHot, aHot$Month, function(x) x[which.max(abs(x$totavg)),]))
  rownames(maxDiffRowsHot) <- NULL 
  
  aHot <- melt(aHot, id.vars = c("Time", "Month"))
  aHot$monthdisp <- factor(aHot$Month, orderedMonths)
  aHot$Time <- as.POSIXct(aHot$Time, format="%H:%M", tz="UTC")
  
  # Continue annotation 
  maxDiffRowsHot$x <- as.POSIXct(format(strptime("01/01/16 11:00", format="%m/%d/%y %H:%M"), format="%H:%M"), format="%H:%M", tz="UTC")
  maxDiffRowsHot$y <- -7
  maxDiffRowsHot$lab <- as.character(round(maxDiffRowsHot$totavg,2))  
  maxDiffRowsHot$monthdisp <- factor(maxDiffRowsHot$Month, orderedMonths)
  maxDiffRowsHot <- maxDiffRowsHot[c("monthdisp", "x", "y", "lab")]
  
  ph <- ggplot() +
    geom_hline(yintercept = 0, linetype="dotted", color='#FC4F30') + 
    geom_line(data=aHot[aHot$variable!="totavg",], aes(x=Time, y=value, group = variable), color="#30A2DA", size=0.5, alpha=0.35) +
    geom_line(data=aHot[aHot$variable=="totavg",], aes(x=Time, y=value, group=1), color="dimgrey", size=1) +
    facet_wrap(~monthdisp, ncol=10, drop=FALSE) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M")) +
    labs(x="Time", y="") +
    theme_bw() +
    theme(legend.position="none") +
    geom_text(data=maxDiffRowsHot, aes(x, y, label=paste("Max Avg Difference:", lab, "F", sep=" "), group=1), color="dimgrey", size=3, hjust=0)
  
  ### Plot on grid:w
  
  grid.arrange(pa,ph, layout_matrix=rbind(c(1), c(2)), 
               left=textGrob(expression(paste("Temperature Difference (", degree ~ F, ")")), rot=90, vjust=1, gp=gpar(fontsize=12)))
  
  ex <- do.call(rbind, lapply(paste0("c",1:length(rlist1)), get))
  exRows <- do.call("rbind", by(ex, ex$Month, function(x) x[which.max(abs(x$TempDiff)),]))
  
  for (i in 1:length(orderedMonths)) {
    print(orderedMonths[i])
    row <- exRows[exRows$Month==orderedMonths[i]]
    for (i in 1:length(rlist1)) {
      if (row$Alias1==rlist1[i]) {
        # track what i is - use it to get data
        # add pulled data from ci to an empty dataframe which will eventually look like a/aHot - before it has been melted
        #keep merging those data frames - want data for every month
        #keep track of where data is coming from (c1, c2, etc) so you can put a title on every one (or footer?)
        #instead of plotting differences, plot actual temperature with outside temperature
        
        #each plot should be separate??? then you can arrange them using grid arrange
      }
    }
  }
  
  
   
}