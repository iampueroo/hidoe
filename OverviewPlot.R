  library(reshape2)
  library(ggplot2)
  library(ggthemes) 
  library(scales)
  library(grid)
  library(gridExtra) 
  library(gtable)
  library(plyr)
  library(lubridate)
  

  orderedMonths <- c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May")
  colors <- c("#8B8B8B", "#30A2DA", "#FC4F30",  "#E5AE38", "#6D904F")
  colors2 <- c("#E69720", "#FC4F30", "#E8E827")
  
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
  crt <- merge(crt[,c(1,2,3,4)], arch[,c("RoomID", "Alias", "Floor", "RoofColor", "SQFT", "Orientation", "Landscape", "Overhang", "School", "AC")], by="RoomID", all.x=TRUE)
  crt$SensorAlias <- as.character(crt$SensorAlias)
  crt <- crt[(substr(crt$SensorAlias,nchar(crt$SensorAlias)-1,nchar(crt$SensorAlias))!="HD"),]
  
  
  crt$Date <- as.Date(crt$StartTime, format="%m/%d/%y")
  crt$Time <- format(strptime(crt$StartTime, format="%m/%d/%y %H:%M"), format="%H:%M")
  crt$Month <- format(crt$Date, "%B") 
  crt <- merge(crt, sdates, by="Date", all.x=FALSE) #restrict to school days
  crt <- merge(crt, shours, by="Time", all.x=FALSE) #restrict to school hours
  crt$SensorAlias <- as.character(crt$SensorAlias)
  crt$StartTime <- NULL
  crt <- crt[!is.na(crt$Temp),]
  crt <- crt[crt$AC!=1,] #remove classrooms with AC
  crt$Season <- as.factor(ifelse(month(crt$Date) %in% c(1,2,3,4,11,12),"Winter","Summer")) 
  crt$AC <- NULL
  
  ot <- read.csv(file="~/dropbox/rh1/hidoe/outdoortemp.csv", sep=",") #outdoor temperature - already restricted to school day/hours
  names(ot) <- c("DateTime", "OutdoorTemp")
  ot$Date <- as.Date(ot$DateTime, format="%m/%d/%y")
  ot$Time <- format(strptime(ot$DateTime, format="%m/%d/%y %H:%M"), format="%H:%M")
  ot$DateTime <- NULL
  ot$Month <- format(ot$Date, "%B")
  
  ot2013 <- ot[year(ot$Date)==2013,]
  ot2014 <- ot[year(ot$Date)==2014,]
  hotMonths13 <- do.call("rbind", by(ot2013, ot2013$Month, function(x) x[which.max(x$OutdoorTemp),]))
  hotMonths14 <- do.call("rbind", by(ot2014, ot2014$Month, function(x) x[which.max(x$OutdoorTemp),]))
  hotMonths <- do.call("rbind", by(ot, ot$Month, function(x) x[which.max(x$OutdoorTemp),]))
  hotMonths <- hotMonths[,c("Date", "Month")]
  
  #Plots
  ot.daily <- ddply(ot,c("Date"), summarise, AvgOutdoorTemp=mean(OutdoorTemp)) #aggregated by date 
  crt.daily <- ddply(crt, c("Date", "Alias", "School"), summarise, AvgRoomTemp=mean(Temp, na.rm=TRUE), MaxRoomTemp=max(Temp,na.rm=TRUE)) #aggregated  by date
  cro <- data.frame(merge(crt.daily, ot.daily, by=c("Date"), all.x=TRUE, all.y=TRUE))  #merge to create giant dataset
  
  cro.melt <- melt(cro, id.vars=c("Date", "Alias", "School"))
  
  ann.o <- data.frame(x=as.Date("2013-08-01", format="%Y-%m-%d"),
                      y=74,
                      lab="Average Outdoor Temperature",
                      monthdisp=factor("August", levels=orderedMonths))
  
  ann.a <- data.frame(x=as.Date("2013-08-01", format="%Y-%m-%d"),
                      y=73.5,
                      lab="Average Classroom Temperature",
                      monthdisp=factor("August", levels=orderedMonths)) 
  
  ann.m <- data.frame(x=as.Date("2013-08-01", format="%Y-%m-%d"),
                      y=73,
                      lab="Maximum Classroom Temperature",
                      monthdisp=factor("August", levels=orderedMonths))
  
  ggplot(cro.melt[cro.melt$School=="Kaimiloa",], aes(x=Date, y=value, colour=variable)) + geom_point(data=cro.melt[cro.melt$School=="Kaimiloa" & cro.melt$variable!="AvgOutdoorTemp",], alpha=0.25) + geom_smooth(size=1) +
    geom_hline(yintercept=85, linetype="dashed", color="dimgrey", size=0.8) + 
    scale_y_continuous(breaks=seq(70,110,5), limits = c(70,110)) + 
    scale_colour_manual(values=colors) +
    scale_x_date(date_breaks="1 month", date_labels="%b '%y") + 
    ggtitle("Average Daily Temperature: Kaimiloa Elementary School") + 
    xlab("Date") + ylab(expression(paste("Temperature (", degree ~ F, ")"))) +
    geom_text(data=ann.o, aes(x=x, y=y, label=lab, group=1), color="#8B8B8B", size=4, hjust=0) + 
    geom_text(data=ann.a, aes(x=x, y=y, label=lab, group=1), color="#30A2DA", size=4, hjust=0) + 
    geom_text(data=ann.m, aes(x=x, y=y, label=lab, group=1), color="#FC4F30", size=4, hjust=0) + 
    theme_fivethirtyeight() +theme(legend.position="none") 
  ggplot(cro.melt[cro.melt$School=="Campbell",], aes(x=Date, y=value, colour=variable)) + geom_point(data=cro.melt[cro.melt$School=="Campbell" & cro.melt$variable!="AvgOutdoorTemp",], alpha=0.25) + geom_smooth(size=1) +
    geom_hline(yintercept=85, linetype="dashed", color="dimgrey", size=0.8) + 
    scale_y_continuous(breaks=seq(70,110,5), limits = c(70,110)) + 
    scale_colour_manual(values=colors) +
    scale_x_date(date_breaks="1 month", date_labels="%b '%y") + 
    ggtitle("Average Daily Temperature: James Campbell High School") + 
    xlab("Date") + ylab(expression(paste("Temperature (", degree ~ F, ")"))) +
     geom_text(data=ann.o, aes(x=x, y=y, label=lab, group=1), color="#8B8B8B", size=4, hjust=0) + 
     geom_text(data=ann.a, aes(x=x, y=y, label=lab, group=1), color="#30A2DA", size=4, hjust=0) + 
     geom_text(data=ann.m, aes(x=x, y=y, label=lab, group=1), color="#FC4F30", size=4, hjust=0) + 
    theme_fivethirtyeight()+theme(legend.position="none") 
  ggplot(cro.melt[cro.melt$School=="Ilima",], aes(x=Date, y=value, colour=variable)) + geom_point(data=cro.melt[cro.melt$School=="Ilima" & cro.melt$variable!="AvgOutdoorTemp",], alpha=0.25) + geom_smooth(size=1) +
    geom_hline(yintercept=85, linetype="dashed", color="dimgrey", size=0.8) + 
    scale_y_continuous(breaks=seq(70,110,5), limits = c(70,110)) + 
    scale_colour_manual(values=colors) +
    scale_x_date(date_breaks="1 month", date_labels="%b '%y") + 
    ggtitle("Average Daily Temperature: Ilima Intermediate School") + 
    xlab("Date") + ylab(expression(paste("Temperature (", degree ~ F, ")"))) +
    geom_text(data=ann.o, aes(x=x, y=y, label=lab, group=1), color="#8B8B8B", size=4, hjust=0) + 
    geom_text(data=ann.a, aes(x=x, y=y, label=lab, group=1), color="#30A2DA", size=4, hjust=0) + 
    geom_text(data=ann.m, aes(x=x, y=y, label=lab, group=1), color="#FC4F30", size=4, hjust=0) + 
    theme_fivethirtyeight()+theme(legend.position="none") 
  
  
  
  ### Bar plot
  cro <- merge(crt, ot, by=c("Time", "Month", "Date"), all.x=TRUE, all.y=TRUE)
  cro$TimeUnit <- ifelse(cro$Time=="08:00",0,.25)
  cro$OutTimeUnit <- ifelse((is.na(cro$OutdoorTemp) | cro$Time=="08:00"),0,.25)
  cro$InGE85 <- ifelse((cro$Temp>=85)==TRUE & cro$Time!="08:00",.25,0)
  cro$OutGE85 <-ifelse((cro$OutdoorTemp>=85)==TRUE & cro$Time!="08:00",.25,0)

  hot <- ddply(cro, c("Date"), summarize, Hot=sum(OutGE85, na.rm=TRUE)) 
  hot <- hot[hot$Hot>0,]
  
  o.daily <- ddply(cro, c("Date","Alias", "Month"), summarize, OutGE85Count=sum(OutGE85, na.rm=TRUE), Count=sum(OutTimeUnit, na.rm=TRUE))
  o.daily <- ddply(o.daily, "Month", summarize, AvgHotHours=mean(OutGE85Count))
  o.daily$AvgHotHours <- with(o.daily, round(AvgHotHours,2))
  o.daily$monthdisp <- factor(o.daily$Month, orderedMonths)
  
  cro.daily <- ddply(cro, c("Date","Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85, na.rm=TRUE), Count=sum(TimeUnit, na.rm=TRUE))
  cro.daily <- ddply(cro.daily, c("Month", "Alias"), summarize, AvgHotHours=mean(InGE85Count))
  cro.daily$AvgHotHours <- with(cro.daily, round(AvgHotHours,2))
  cro.daily$monthdisp <- factor(cro.daily$Month, orderedMonths)
  cro.daily$School <- sub("(.*?) - .*", "\\1", cro.daily$Alias)
  cro.daily$Hot <- ifelse(cro.daily$AvgHotHours>4, "Red", ifelse(cro.daily$AvgHotHours>2, "Orange", "Yellow"))
  
  cros.daily <- ddply(cro, c("Date","Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85, na.rm=TRUE), Count=sum(TimeUnit, na.rm=TRUE))
  cros.daily <- ddply(cros.daily, c("Month", "School"), summarize, AvgHotHours=mean(InGE85Count))
  cros.daily$AvgHotHours <- with(cros.daily, round(AvgHotHours,2))
  cros.daily$monthdisp <- factor(cros.daily$Month, orderedMonths)
  
  annc <- cros.daily[cros.daily$School=="Campbell",]
  annc$x <- 1
  annc$y <- annc$AvgHotHours + .15
  annc$lab <- paste0("School Average: ", annc$AvgHotHours)
  annc <- annc[,c("x", "y", "lab", "Month","School")]
  annc$monthdisp <- factor(annc$Month, orderedMonths)
  
  anni <- cros.daily[cros.daily$School=="Ilima",]
  anni$x <- 1
  anni$y <- anni$AvgHotHours + .15
  anni$lab <- paste0("School Average: ", anni$AvgHotHours)
  anni <- anni[,c("x", "y", "lab", "Month","School")]
  anni$monthdisp <- factor(anni$Month, orderedMonths)
  
  annk <- cros.daily[cros.daily$School=="Kaimiloa",]
  annk$x <- 1
  annk$y <- annk$AvgHotHours + .15
  annk$lab <- paste0("School Average: ", annk$AvgHotHours)
  annk <- annk[,c("x", "y", "lab", "Month","School")]
  annk$monthdisp <- factor(annk$Month, orderedMonths)
  
  anno <- o.daily
  anno$x <- 30
  anno$y <- anno$AvgHotHours + .15
  anno$lab <- paste0("Outdoor: ", anno$AvgHotHours)
  anno <- anno[,c("x", "y", "lab", "Month")]
  anno$monthdisp <- factor(anno$Month, orderedMonths)
  
  plot.title <- "James Campbell High School"
  plot.subtitle <- "Number of School Hours Above 85 F, All Days"
  
  bc1 <- ggplot() + geom_bar(data=cro.daily[cro.daily$School=="Campbell",], aes(x=Alias, y=AvgHotHours, fill=Hot), alpha=0.5, stat="identity") + 
    geom_hline(data=o.daily, aes(yintercept=AvgHotHours), color="dimgrey") +
    geom_hline(data=cros.daily[cros.daily$School=="Campbell",], aes(yintercept=AvgHotHours), color="dimgrey", linetype="dashed", alpha=0.8) +
    geom_text(data=anno, aes(x, y, label=lab, group=1), color="dimgrey", size=3, hjust="right") + 
    geom_text(data=annc, aes(x, y, label=lab, group=1), color="dimgrey", size=3, hjust="left") + 
    facet_grid(~monthdisp, drop=FALSE) +
    scale_fill_manual(values=colors2) +
    scale_y_continuous(breaks=seq(0,6,1), limits=c(0,6)) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_fivethirtyeight() +
    theme(axis.text.x = element_blank(), legend.position="none")
  
  plot.title <- "Ilima Intermediate School"
  plot.subtitle <- "Number of School Hours Above 85 F, All Days"
  
  anno <- o.daily
  anno$x <- 12
  anno$y <- anno$AvgHotHours + .15
  anno$lab <- paste0("Outdoor: ", anno$AvgHotHours)
  anno <- anno[,c("x", "y", "lab", "Month")]
  anno$monthdisp <- factor(anno$Month, orderedMonths)
  
  
  bi1 <- ggplot() + geom_bar(data=cro.daily[cro.daily$School=="Ilima",], aes(x=Alias, y=AvgHotHours, fill=Hot), alpha=0.5, stat="identity") + 
    geom_hline(data=o.daily, aes(yintercept=AvgHotHours), color="dimgrey") +
    geom_hline(data=cros.daily[cros.daily$School=="Ilima",], aes(yintercept=AvgHotHours), color="dimgrey", linetype="dashed", alpha=0.8) +
    geom_text(data=anno, aes(x, y, label=lab, group=1), color="dimgrey", size=3, hjust="right") + 
    geom_text(data=anni, aes(x, y, label=lab, group=1), color="dimgrey", size=3, hjust="left") + 
    facet_grid(~monthdisp, drop=FALSE) +
    scale_fill_manual(values=colors2) +
    scale_y_continuous(breaks=seq(0,6,1), limits=c(0,6)) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_fivethirtyeight() +
    theme(axis.text.x = element_blank(), legend.position="none")
  
  plot.title <- "Kaimiloa Elementary School"
  plot.subtitle <- "Number of School Hours Above 85 F, All Days"
  
  anno <- o.daily
  anno$x <- 10
  anno$y <- anno$AvgHotHours + .15
  anno$lab <- paste0("Outdoor: ", anno$AvgHotHours)
  anno <- anno[,c("x", "y", "lab", "Month")]
  anno$monthdisp <- factor(anno$Month, orderedMonths)
  
  bk1 <- ggplot() + geom_bar(data=cro.daily[cro.daily$School=="Kaimiloa",], aes(x=Alias, y=AvgHotHours, fill=Hot), alpha=0.5, stat="identity") + 
    geom_hline(data=o.daily, aes(yintercept=AvgHotHours), color="dimgrey") +
    geom_hline(data=cros.daily[cros.daily$School=="Kaimiloa",], aes(yintercept=AvgHotHours), color="dimgrey", linetype="dashed", alpha=0.8) +
    geom_text(data=anno, aes(x, y, label=lab, group=1), color="dimgrey", size=3, hjust="right") + 
    geom_text(data=annk, aes(x, y, label=lab, group=1), color="dimgrey", size=3, hjust="left") + 
    facet_grid(~monthdisp, drop=FALSE) +
    scale_fill_manual(values=colors2) +
    scale_y_continuous(breaks=seq(0,6,1), limits=c(0,6)) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_fivethirtyeight() +
    theme(axis.text.x = element_blank(), legend.position="none")
  
  ## Reaching 85
  croHot <- merge(cro, hot, by="Date")
  
  o.daily <- ddply(croHot, c("Date","Alias", "Month"), summarize, OutGE85Count=sum(OutGE85, na.rm=TRUE), Count=sum(OutTimeUnit, na.rm=TRUE))
  o.daily <- ddply(o.daily, "Month", summarize, AvgHotHours=mean(OutGE85Count))
  o.daily$AvgHotHours <- with(o.daily, round(AvgHotHours,2))
  o.daily$monthdisp <- factor(o.daily$Month, orderedMonths)
  
  cro.daily <- ddply(croHot, c("Date","Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85, na.rm=TRUE), Count=sum(TimeUnit, na.rm=TRUE))
  cro.daily <- ddply(cro.daily, c("Month", "Alias"), summarize, AvgHotHours=mean(InGE85Count))
  cro.daily$AvgHotHours <- with(cro.daily, round(AvgHotHours,2))
  cro.daily$monthdisp <- factor(cro.daily$Month, orderedMonths)
  cro.daily$School <- sub("(.*?) - .*", "\\1", cro.daily$Alias)
  cro.daily$Hot <- ifelse(cro.daily$AvgHotHours>4, "Red", ifelse(cro.daily$AvgHotHours>2, "Orange", "Yellow"))
  
  cros.daily <- ddply(croHot, c("Date","Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85, na.rm=TRUE), Count=sum(TimeUnit, na.rm=TRUE))
  cros.daily <- ddply(cros.daily, c("Month", "School"), summarize, AvgHotHours=mean(InGE85Count))
  cros.daily$AvgHotHours <- with(cros.daily, round(AvgHotHours,2))
  cros.daily$monthdisp <- factor(cros.daily$Month, orderedMonths)

  annc <- cros.daily[cros.daily$School=="Campbell",]
  annc$x <- 1
  annc$y <- annc$AvgHotHours + .15
  annc$lab <- paste0("School Average: ", annc$AvgHotHours)
  annc <- annc[,c("x", "y", "lab", "Month","School")]
  annc$monthdisp <- factor(annc$Month, orderedMonths)
  
  anni <- cros.daily[cros.daily$School=="Ilima",]
  anni$x <- 1
  anni$y <- anni$AvgHotHours + .15
  anni$lab <- paste0("School Average: ", anni$AvgHotHours)
  anni <- anni[,c("x", "y", "lab", "Month","School")]
  anni$monthdisp <- factor(anni$Month, orderedMonths)
  
  annk <- cros.daily[cros.daily$School=="Kaimiloa",]
  annk$x <- 1
  annk$y <- annk$AvgHotHours + .15
  annk$lab <- paste0("School Average: ", annk$AvgHotHours)
  annk <- annk[,c("x", "y", "lab", "Month","School")]
  annk$monthdisp <- factor(annk$Month, orderedMonths)
  
  anno <- o.daily
  anno$x <- 30
  anno$y <- anno$AvgHotHours + .15
  anno$lab <- paste0("Outdoor: ", anno$AvgHotHours)
  anno <- anno[,c("x", "y", "lab", "Month")]
  anno$monthdisp <- factor(anno$Month, orderedMonths)
  
  plot.title <- ""
  plot.subtitle <- "Number of School Hours Above 85 F, Days Reaching 85 F"
  
  bc2 <- ggplot() + geom_bar(data=cro.daily[cro.daily$School=="Campbell",], aes(x=Alias, y=AvgHotHours, fill=Hot), alpha=0.5, stat="identity") + 
    geom_hline(data=o.daily, aes(yintercept=AvgHotHours), color="dimgrey") +
    geom_hline(data=cros.daily[cros.daily$School=="Campbell",], aes(yintercept=AvgHotHours), color="dimgrey", linetype="dashed", alpha=0.8) +
    geom_text(data=anno, aes(x, y, label=lab, group=1), color="dimgrey", size=3, hjust="right") + 
    geom_text(data=annc, aes(x, y, label=lab, group=1), color="dimgrey", size=3, hjust="left") + 
    facet_grid(~monthdisp, drop=FALSE) +
    scale_fill_manual(values=colors2) +
    scale_y_continuous(breaks=seq(0,6,1), limits=c(0,6)) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_fivethirtyeight() +
    theme(axis.text.x = element_blank(), legend.position="none")
  
  anno <- o.daily
  anno$x <- 12
  anno$y <- anno$AvgHotHours + .15
  anno$lab <- paste0("Outdoor: ", anno$AvgHotHours)
  anno <- anno[,c("x", "y", "lab", "Month")]
  anno$monthdisp <- factor(anno$Month, orderedMonths)
  
  bi2 <- ggplot() + geom_bar(data=cro.daily[cro.daily$School=="Ilima",], aes(x=Alias, y=AvgHotHours, fill=Hot), alpha=0.5, stat="identity") + 
    geom_hline(data=o.daily, aes(yintercept=AvgHotHours), color="dimgrey") +
    geom_hline(data=cros.daily[cros.daily$School=="Ilima",], aes(yintercept=AvgHotHours), color="dimgrey", linetype="dashed", alpha=0.8) +
    geom_text(data=anno, aes(x, y, label=lab, group=1), color="dimgrey", size=3, hjust="right") + 
    geom_text(data=anni, aes(x, y, label=lab, group=1), color="dimgrey", size=3, hjust="left") + 
    facet_grid(~monthdisp, drop=FALSE) +
    scale_fill_manual(values=colors2) +
    scale_y_continuous(breaks=seq(0,6,1), limits=c(0,6)) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_fivethirtyeight() +
    theme(axis.text.x = element_blank(), legend.position="none")
  
  anno <- o.daily
  anno$x <- 10
  anno$y <- anno$AvgHotHours + .15
  anno$lab <- paste0("Outdoor: ", anno$AvgHotHours)
  anno <- anno[,c("x", "y", "lab", "Month")]
  anno$monthdisp <- factor(anno$Month, orderedMonths)
  
  bk2 <- ggplot() + geom_bar(data=cro.daily[cro.daily$School=="Kaimiloa",], aes(x=Alias, y=AvgHotHours, fill=Hot), alpha=0.5, stat="identity") + 
    geom_hline(data=o.daily, aes(yintercept=AvgHotHours), color="dimgrey") +
    geom_hline(data=cros.daily[cros.daily$School=="Kaimiloa",], aes(yintercept=AvgHotHours), color="dimgrey", linetype="dashed", alpha=0.8) +
    geom_text(data=anno, aes(x, y, label=lab, group=1), color="dimgrey", size=3, hjust="right") + 
    geom_text(data=annk, aes(x, y, label=lab, group=1), color="dimgrey", size=3, hjust="left") + 
    facet_grid(~monthdisp, drop=FALSE) +
    scale_fill_manual(values=colors2) +
    scale_y_continuous(breaks=seq(0,6,1), limits=c(0,6)) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_fivethirtyeight() +
    theme(axis.text.x = element_blank(), legend.position="none")
  
  ## Hottest day
  croMax <- merge(cro, hotMonths, by=c("Date","Month"))
  
  o.daily <- ddply(croMax, c("Date","Alias", "Month"), summarize, OutGE85Count=sum(OutGE85, na.rm=TRUE), Count=sum(OutTimeUnit, na.rm=TRUE))
  o.daily <- ddply(o.daily, "Month", summarize, AvgHotHours=mean(OutGE85Count))
  o.daily$AvgHotHours <- with(o.daily, round(AvgHotHours,2))
  o.daily$monthdisp <- factor(o.daily$Month, orderedMonths)
  
  cro.daily <- ddply(croMax, c("Date","Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85, na.rm=TRUE), Count=sum(TimeUnit, na.rm=TRUE))
  cro.daily <- ddply(cro.daily, c("Month", "Alias"), summarize, AvgHotHours=mean(InGE85Count))
  cro.daily$AvgHotHours <- with(cro.daily, round(AvgHotHours,2))
  cro.daily$monthdisp <- factor(cro.daily$Month, orderedMonths)
  cro.daily$School <- sub("(.*?) - .*", "\\1", cro.daily$Alias)
  cro.daily$Hot <- ifelse(cro.daily$AvgHotHours>4, "Red", ifelse(cro.daily$AvgHotHours>2, "Orange", "Yellow"))
  
  cros.daily <- ddply(croMax, c("Date","Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85, na.rm=TRUE), Count=sum(TimeUnit, na.rm=TRUE))
  cros.daily <- ddply(cros.daily, c("Month", "School"), summarize, AvgHotHours=mean(InGE85Count))
  cros.daily$AvgHotHours <- with(cros.daily, round(AvgHotHours,2))
  cros.daily$monthdisp <- factor(cros.daily$Month, orderedMonths)
  
  anno <- o.daily
  anno$x <- 30
  anno$y <- anno$AvgHotHours + .15
  anno$lab <- paste0("Outdoor: ", anno$AvgHotHours)
  anno <- anno[,c("x", "y", "lab", "Month")]
  anno$monthdisp <- factor(anno$Month, orderedMonths)
  
  annc <- cros.daily[cros.daily$School=="Campbell",]
  annc$x <- 1
  annc$y <- annc$AvgHotHours + .15
  annc$lab <- paste0("School Average: ", annc$AvgHotHours)
  annc <- annc[,c("x", "y", "lab", "Month","School")]
  annc$monthdisp <- factor(annc$Month, orderedMonths)
  
  anni <- cros.daily[cros.daily$School=="Ilima",]
  anni$x <- 1
  anni$y <- anni$AvgHotHours + .15
  anni$lab <- paste0("School Average: ", anni$AvgHotHours)
  anni <- anni[,c("x", "y", "lab", "Month","School")]
  anni$monthdisp <- factor(anni$Month, orderedMonths)
  
  annk <- cros.daily[cros.daily$School=="Kaimiloa",]
  annk$x <- 1
  annk$y <- annk$AvgHotHours + .15
  annk$lab <- paste0("School Average: ", annk$AvgHotHours)
  annk <- annk[,c("x", "y", "lab", "Month","School")]
  annk$monthdisp <- factor(annk$Month, orderedMonths)
  
  plot.subtitle <- "Number of School Hours Above 85 F, Hottest Day"
  
  bc3 <- ggplot() + geom_bar(data=cro.daily[cro.daily$School=="Campbell",], aes(x=Alias, y=AvgHotHours, fill=Hot), alpha=0.5, stat="identity") + 
    geom_hline(data=o.daily, aes(yintercept=AvgHotHours), color="dimgrey") +
    geom_hline(data=cros.daily[cros.daily$School=="Campbell",], aes(yintercept=AvgHotHours), color="dimgrey", linetype="dashed", alpha=0.8) +
    geom_text(data=anno, aes(x, y, label=lab, group=1), color="dimgrey", size=3, hjust="right") + 
    geom_text(data=annc, aes(x, y, label=lab, group=1), color="dimgrey", size=3, hjust="left") + 
    facet_grid(~monthdisp, drop=FALSE) +
    scale_fill_manual(values=colors2) +
    scale_y_continuous(breaks=seq(0,6,1), limits=c(0,6)) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_fivethirtyeight() +
    theme(axis.text.x = element_blank(), legend.position="none")
  
  anno <- o.daily
  anno$x <- 12
  anno$y <- anno$AvgHotHours + .15
  anno$lab <- paste0("Outdoor: ", anno$AvgHotHours)
  anno <- anno[,c("x", "y", "lab", "Month")]
  anno$monthdisp <- factor(anno$Month, orderedMonths)
  
  bi3 <- ggplot() + geom_bar(data=cro.daily[cro.daily$School=="Ilima",], aes(x=Alias, y=AvgHotHours, fill=Hot), alpha=0.5, stat="identity") + 
    geom_hline(data=o.daily, aes(yintercept=AvgHotHours), color="dimgrey") +
    geom_hline(data=cros.daily[cros.daily$School=="Ilima",], aes(yintercept=AvgHotHours), color="dimgrey", linetype="dashed", alpha=0.8) +
    geom_text(data=anno, aes(x, y, label=lab, group=1), color="dimgrey", size=3, hjust="right") + 
    geom_text(data=anni, aes(x, y, label=lab, group=1), color="dimgrey", size=3, hjust="left") + 
    facet_grid(~monthdisp, drop=FALSE) +
    scale_fill_manual(values=colors2) +
    scale_y_continuous(breaks=seq(0,6,1), limits=c(0,6)) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_fivethirtyeight() +
    theme(axis.text.x = element_blank(), legend.position="none")
  
  anno <- o.daily
  anno$x <- 10
  anno$y <- anno$AvgHotHours + .15
  anno$lab <- paste0("Outdoor: ", anno$AvgHotHours)
  anno <- anno[,c("x", "y", "lab", "Month")]
  anno$monthdisp <- factor(anno$Month, orderedMonths)
  
  bk3 <- ggplot() + geom_bar(data=cro.daily[cro.daily$School=="Kaimiloa",], aes(x=Alias, y=AvgHotHours, fill=Hot), alpha=0.5, stat="identity") + 
    geom_hline(data=o.daily, aes(yintercept=AvgHotHours), color="dimgrey") +
    geom_hline(data=cros.daily[cros.daily$School=="Kaimiloa",], aes(yintercept=AvgHotHours), color="dimgrey", linetype="dashed", alpha=0.8) +
    geom_text(data=anno, aes(x, y, label=lab, group=1), color="dimgrey", size=3, hjust="right") + 
    geom_text(data=annk, aes(x, y, label=lab, group=1), color="dimgrey", size=3, hjust="left") + 
    facet_grid(~monthdisp, drop=FALSE) +
    scale_fill_manual(values=colors2) +
    scale_y_continuous(breaks=seq(0,6,1), limits=c(0,6)) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_fivethirtyeight() +
    theme(axis.text.x = element_blank(), legend.position="none")
  
  grid.arrange(bk1,bk2,bk3, layout_matrix=rbind(c(1), c(2), c(3)))
  grid.arrange(bc1,bc2,bc3, layout_matrix=rbind(c(1), c(2), c(3)))
  grid.arrange(bi1,bi2,bi3, layout_matrix=rbind(c(1), c(2), c(3)))
  
  
  ### Line
  cr <- ddply(crt, c("Time", "Alias", "Month", "School"), summarize, AvgTemp=mean(Temp))
  o <- ddply(ot, c("Time", "Month"), summarize, AvgTemp=mean(OutdoorTemp))
  
  o$Time <- as.POSIXct(o$Time, format="%H:%M", tz="UTC")
  cr$Time <- as.POSIXct(cr$Time, format="%H:%M", tz="UTC")
  o$monthdisp <- factor(o$Month, orderedMonths)
  cr$monthdisp <- factor(cr$Month, orderedMonths)
  
  lims <- as.POSIXct(strptime(c("08:00","14:00"), format = "%H:%M"), tz="UTC")    
  plot.title <- "James Campbell High School"
  plot.subtitle <- "Average School Day Temperature, All Observations"
  
  lc1 <- ggplot() + 
    geom_hline(yintercept=85, linetype="dashed", color="dimgrey", size=0.5) +
    geom_line(data=cr[cr$School=="Campbell",], aes(x=Time, y=AvgTemp, group=Alias, color=Alias), size=0.5, alpha=0.5) +
    geom_line(data=o, aes(x=Time, y=AvgTemp, group=1), color="dimgrey", size=0.6) +
    facet_grid(~monthdisp, drop=FALSE) +
    scale_y_continuous(breaks=seq(70,100,5), limits=c(70,100)) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_fivethirtyeight() + theme(legend.position="none")

  plot.title <- "Kaimiloa Elementary School"
  
  lk1 <- ggplot() + 
    geom_hline(yintercept=85, linetype="dashed", color="dimgrey", size=0.5) +
    geom_line(data=cr[cr$School=="Kaimiloa",], aes(x=Time, y=AvgTemp, group=Alias, color=Alias), size=0.5, alpha=0.5) +
    geom_line(data=o, aes(x=Time, y=AvgTemp, group=1), color="dimgrey", size=0.8) +
    facet_grid(~monthdisp, drop=FALSE) +
    scale_y_continuous(breaks=seq(70,100,5), limits=c(70,100)) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_fivethirtyeight() + theme(legend.position="none")
  
  plot.title <- "Ilima Intermediate School"
  
  li1 <- ggplot() + 
    geom_hline(yintercept=85, linetype="dashed", color="dimgrey", size=0.5) +
    geom_line(data=cr[cr$School=="Ilima",], aes(x=Time, y=AvgTemp, group=Alias, color=Alias), size=0.5, alpha=0.5) +
    geom_line(data=o, aes(x=Time, y=AvgTemp, group=1), color="dimgrey", size=0.8) +
    facet_grid(~monthdisp, drop=FALSE) +
    scale_y_continuous(breaks=seq(70,100,5), limits=c(70,100)) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_fivethirtyeight() + theme(legend.position="none")
  
  ## Obs over 85
  otHot <- ot[ot$OutdoorTemp>=85,]
  crHot <- merge(otHot, crt, by=c("Date", "Time", "Month"))  
  cr <- ddply(crHot, c("Time", "Alias", "Month", "School"), summarize, AvgTemp=mean(Temp))
  o <- ddply(otHot, c("Time", "Month"), summarize, AvgTemp=mean(OutdoorTemp))
  
  o$Time <- as.POSIXct(o$Time, format="%H:%M", tz="UTC")
  cr$Time <- as.POSIXct(cr$Time, format="%H:%M", tz="UTC")
  o$monthdisp <- factor(o$Month, orderedMonths)
  cr$monthdisp <- factor(cr$Month, orderedMonths)
  
  plot.title <- ""
  plot.subtitle <- "Average School Day Temperature, Observations >85 F"
  
  lc2 <- ggplot() + 
    geom_hline(yintercept=85, linetype="dashed", color="dimgrey", size=0.5) +
    geom_line(data=cr[cr$School=="Campbell",], aes(x=Time, y=AvgTemp, group=Alias, color=Alias), size=0.5, alpha=0.5) +
    geom_line(data=o, aes(x=Time, y=AvgTemp, group=1), color="dimgrey", size=0.8) +
    facet_grid(~monthdisp, drop=FALSE) +
    scale_y_continuous(breaks=seq(75,105,5), limits=c(75,105)) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_fivethirtyeight() + theme(legend.position="none")
  
  lk2 <- ggplot() + 
    geom_hline(yintercept=85, linetype="dashed", color="dimgrey", size=0.5) +
    geom_line(data=cr[cr$School=="Kaimiloa",], aes(x=Time, y=AvgTemp, group=Alias, color=Alias), size=0.5, alpha=0.5) +
    geom_line(data=o, aes(x=Time, y=AvgTemp, group=1), color="dimgrey", size=0.8) +
    facet_grid(~monthdisp, drop=FALSE) +
    scale_y_continuous(breaks=seq(75,105,5), limits=c(75,105)) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_fivethirtyeight() + theme(legend.position="none")
  
  li2 <- ggplot() + 
    geom_hline(yintercept=85, linetype="dashed", color="dimgrey", size=0.5) +
    geom_line(data=cr[cr$School=="Ilima",], aes(x=Time, y=AvgTemp, group=Alias, color=Alias), size=0.5, alpha=0.5) +
    geom_line(data=o, aes(x=Time, y=AvgTemp, group=1), color="dimgrey", size=0.8) +
    facet_grid(~monthdisp, drop=FALSE) +
    scale_y_continuous(breaks=seq(75,105,5), limits=c(75,105)) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_fivethirtyeight() + theme(legend.position="none")
  
  ## Hottest day
  cr <- merge(crt, hotMonths, by=c("Date","Month"))
  o <- merge(ot, hotMonths, by=c("Date","Month"))
  
  o$Time <- as.POSIXct(o$Time, format="%H:%M", tz="UTC")
  cr$Time <- as.POSIXct(cr$Time, format="%H:%M", tz="UTC")
  o$monthdisp <- factor(o$Month, orderedMonths)
  cr$monthdisp <- factor(cr$Month, orderedMonths)
  
  plot.title <- ""
  plot.subtitle <- "Average School Day Temperature, Hottest Day"
  
  lc3 <- ggplot() + 
    geom_hline(yintercept=85, linetype="dashed", color="dimgrey", size=0.5) +
    geom_line(data=cr[cr$School=="Campbell",], aes(x=Time, y=Temp, group=Alias, color=Alias), size=0.5, alpha=0.5) +
    geom_line(data=o, aes(x=Time, y=OutdoorTemp, group=1), color="dimgrey", size=0.8) +
    facet_grid(~monthdisp, drop=FALSE) +
    scale_y_continuous(breaks=seq(70,100,5), limits=c(70,100)) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_fivethirtyeight() + theme(legend.position="none")
  
  lk3 <- ggplot() + 
    geom_hline(yintercept=85, linetype="dashed", color="dimgrey", size=0.5) +
    geom_line(data=cr[cr$School=="Kaimiloa",], aes(x=Time, y=Temp, group=Alias, color=Alias), size=0.5, alpha=0.5) +
    geom_line(data=o, aes(x=Time, y=OutdoorTemp, group=1), color="dimgrey", size=0.8) +
    facet_grid(~monthdisp, drop=FALSE) +
    scale_y_continuous(breaks=seq(70,100,5), limits=c(70,100)) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_fivethirtyeight() + theme(legend.position="none")
  
  li3 <- ggplot() + 
    geom_hline(yintercept=85, linetype="dashed", color="dimgrey", size=0.5) +
    geom_line(data=cr[cr$School=="Ilima",], aes(x=Time, y=Temp, group=Alias, color=Alias), size=0.5, alpha=0.5) +
    geom_line(data=o, aes(x=Time, y=OutdoorTemp, group=1), color="dimgrey", size=0.8) +
    facet_grid(~monthdisp, drop=FALSE) +
    scale_y_continuous(breaks=seq(70,100,5), limits=c(70,100)) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_fivethirtyeight() + theme(legend.position="none")
  
  grid.arrange(lk1,lk2,lk3, layout_matrix=rbind(c(1), c(2), c(3)))
  grid.arrange(lc1,lc2,lc3, layout_matrix=rbind(c(1), c(2), c(3)))
  grid.arrange(li1,li2,li3, layout_matrix=rbind(c(1), c(2), c(3)))
  
  #Overlap
  ot$Year <- as.factor(year(ot$Date))
  ot$Day <- as.character(format(ot$Date, "%m-%d"))
  otoverlap <- ot[ot$Month %in% c("August", "September", "October"),]
  ggplot(otoverlap, aes(x=Date, y=OutdoorTemp)) + geom_point(color="blue", alpha=0.5) + geom_smooth(size=1, color="red") + facet_grid(~Year) +
     theme_fivethirtyeight()
  
  year(otoverlap$Date) <- 2013
  
  ggplot() + geom_smooth(data=otoverlap, aes(x=Day, y=OutdoorTemp, color=Year, group=Year)) + scale_y_continuous(breaks=seq(70,100,5), limits=c(70,100)) +
    ggtitle("Comparison of Summer Months") + theme_fivethirtyeight()
  

  