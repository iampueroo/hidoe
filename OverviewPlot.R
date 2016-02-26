  OverviewPlot <- function(school) {
    library(plyr)
    library(reshape2)
    library(ggplot2)
    library(ggthemes) 
    library(scales)
    library(grid)
    library(gridExtra) 
    library(gtable)
    #library(lubridate)
    
    
    ### Defined variables
    minTemp <- 85
    orderedMonths <- c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May")
    colors <- c("#8B8B8B", "#30A2DA", "#FC4F30",  "#E5AE38", "#6D904F")
    colorsHeat <- c("#E69720", "#FC4F30", "#E8E827") 
  
    ## Input variables
    if (school=="Kaimiloa") {
      schoolTitle <- "Kaimiloa Elementary School"
      ann.size <- 10
    } else if (school=="Campbell") {
      schoolTitle <- "James Campbell High School"
      ann.size <- 30
    } else if (school=="Ilima") {
      schoolTitle <- "Ilima Intermediate School"
      ann.size <- 12
    }
    
    ### Input data 
    #sdates <- read.csv(file="~/dropbox/rh1/hidoe/day.csv", sep=",")  #school dates
    #sdates$Date <- as.Date(sdates$rd, format="%m/%d/%y")
    #sdates <- sdates[-c(1)]
    #arch <- read.csv(file="~/dropbox/rh1/hidoe/classroom-csv.csv", sep=",") #classroom architectural data
    #shours <- read.csv(file="~/dropbox/rh1/hidoe/hour.csv", sep=",")  #school hours
    #shours$Time <- format(strptime(shours$Hour, format="%H:%M"), format="%H:%M")
    #shours$Hour <- NULL
    
    #setwd("~/dropbox/rh1/hidoe/csv")
    #crt <- do.call(rbind,lapply(dir(), read.csv)) #read all classroom sensor data
    #names(crt) <- c("RoomID", "SensorAlias", "StartTime", "Temp", "RH", "Ill")
    #crt <- merge(crt[,c(1,2,3,4)], arch[,c("RoomID", "Alias", "Floor", "RoofColor", "SQFT", "Orientation", "Landscape", "Overhang", "School", "AC")], by="RoomID", all.x=TRUE)
    #crt$SensorAlias <- as.character(crt$SensorAlias)
    #crt <- crt[(substr(crt$SensorAlias,nchar(crt$SensorAlias)-1,nchar(crt$SensorAlias))!="HD"),] #remove non 15 minute intervals - keep for now but need to discuss how to resolve this (maybe smoothing)
    #crt$Date <- as.Date(crt$StartTime, format="%m/%d/%y")
    #crt$Time <- format(strptime(crt$StartTime, format="%m/%d/%y %H:%M"), format="%H:%M")
    #crt$Month <- format(crt$Date, "%B") 
    #crt <- merge(crt, sdates, by="Date", all.x=FALSE) #restrict to school days
    #crt <- merge(crt, shours, by="Time", all.x=FALSE) #restrict to school hours
    #crt$StartTime <- NULL
    #crt <- crt[!is.na(crt$Temp),] #remove RH/illuminance observations
    #crt <- crt[crt$AC!=1,] #remove classrooms with AC
    #crt$AC <- NULL
    #crt <<- crt     
    
    
    ot <- read.csv(file="~/dropbox/rh1/hidoe/outdoortemp.csv", sep=",") #outdoor temperature - switch to raw data with RH/wind speed
    names(ot) <- c("DateTime", "OutdoorTemp")
    ot$Date <- as.Date(ot$DateTime, format="%m/%d/%y")
    ot$Time <- format(strptime(ot$DateTime, format="%m/%d/%y %H:%M"), format="%H:%M")
    ot$DateTime <- NULL
    ot$Month <- format(ot$Date, "%B")
    
    #ot2013 <- ot[year(ot$Date)==2013,]
    #ot2014 <- ot[year(ot$Date)==2014,]
    #hotMonths13 <- do.call("rbind", by(ot2013, ot2013$Month, function(x) x[which.max(x$OutdoorTemp),]))
    #hotMonths14 <- do.call("rbind", by(ot2014, ot2014$Month, function(x) x[which.max(x$OutdoorTemp),]))
    hotMonths <- do.call("rbind", by(ot, ot$Month, function(x) x[which.max(x$OutdoorTemp),]))
    hotMonths <- hotMonths[,c("Date", "Month")]
    
    ### Check overlapping summer months
    #ot$Year <- as.factor(year(ot$Date))
    #ot$Day <- as.character(format(ot$Date, "%m-%d"))
    #otoverlap <- ot[ot$Month %in% c("August", "September", "October"),]
    #ggplot(otoverlap, aes(x=Date, y=OutdoorTemp)) + geom_point(color="blue", alpha=0.5) + geom_smooth(size=1, color="red") + facet_grid(~Year) + ggtitle("Average Temperature of Overlapping Months (2013-2014)") + theme_fivethirtyeight()
    
    #year(otoverlap$Date) <- 2013
    #ggplot() + geom_smooth(data=otoverlap, aes(x=Day, y=OutdoorTemp, color=Year, group=Year)) + scale_y_continuous(breaks=seq(70,100,5), limits=c(70,100)) + ggtitle("Average Temperature of Overlapping Months (2013-2014)") + theme_fivethirtyeight()
    
    ### Plots
    ot.daily <- ddply(ot,c("Date"), summarise, AvgOutdoorTemp=mean(OutdoorTemp)) #aggregated by date 
    crt.daily <- ddply(crt, c("Date", "Alias", "School"), summarise, AvgRoomTemp=mean(Temp, na.rm=TRUE), MaxRoomTemp=max(Temp,na.rm=TRUE)) #aggregated  by date
    cro.daily <- data.frame(merge(crt.daily, ot.daily, by=c("Date"), all.x=TRUE, all.y=TRUE)) 
    
    cro.daily <- melt(cro.daily, id.vars=c("Date", "Alias", "School"))
    
    ann.o <- data.frame(x=as.Date("2013-08-01", format="%Y-%m-%d"), y=72.5, lab="Average Outdoor Temperature", monthdisp=factor("August", levels=orderedMonths)) #average outdoor temp annotation
    ann.a <- data.frame(x=as.Date("2013-08-01", format="%Y-%m-%d"), y=72, lab="Average Classroom Temperature", monthdisp=factor("August", levels=orderedMonths)) #average classroom temp annotation
    ann.m <- data.frame(x=as.Date("2013-08-01", format="%Y-%m-%d"), y=71.5, lab="Maximum Classroom Temperature", monthdisp=factor("August", levels=orderedMonths)) #maximum classroom temp annotation
    
    p <- ggplot(cro.daily[cro.daily$School==school,], aes(x=Date, y=value, colour=variable)) + geom_point(data=cro.daily[cro.daily$School=="Kaimiloa" & cro.daily$variable!="AvgOutdoorTemp",], alpha=0.25) + geom_smooth(size=1) +
      geom_hline(yintercept=85, linetype="dashed", color="dimgrey", size=0.8) + 
      scale_y_continuous(breaks=seq(70,110,5), limits = c(70,110)) + 
      scale_colour_manual(values=colors) +
      scale_x_date(date_breaks="1 month", date_labels="%b '%y") + 
      ggtitle(paste("Average Daily Temperature: ", schoolTitle)) + 
      xlab("Date") + ylab(expression(paste("Temperature (", degree ~ F, ")"))) +
      geom_text(data=ann.o, aes(x=x, y=y, label=lab, group=1), color="#8B8B8B", size=3, hjust=0) + 
      geom_text(data=ann.a, aes(x=x, y=y, label=lab, group=1), color="#30A2DA", size=3, hjust=0) + 
      geom_text(data=ann.m, aes(x=x, y=y, label=lab, group=1), color="#FC4F30", size=3, hjust=0) + 
      theme_fivethirtyeight() +theme(legend.position="none", text=element_text(size=9)) 
    
    print(p)
    
    
    ### Bar plot
    ## All days
    crot <- merge(crt, ot, by=c("Time", "Month", "Date"), all.x=TRUE, all.y=TRUE)
    crot$TimeUnit <- ifelse(crot$Time=="08:00",0,.25) #start counting hours after 8 AM
    crot$OutTimeUnit <- ifelse((is.na(crot$OutdoorTemp) | crot$Time=="08:00"),0,.25) 
    crot$InGE85 <- ifelse((crot$Temp>=85)==TRUE & crot$Time!="08:00",.25,0)
    crot$OutGE85 <-ifelse((crot$OutdoorTemp>=85)==TRUE & crot$Time!="08:00",.25,0)

    o.daily <- ddply(crot, c("Date","Alias", "Month"), summarize, OutGE85Count=sum(OutGE85, na.rm=TRUE), Count=sum(OutTimeUnit, na.rm=TRUE))
    o.daily <- ddply(o.daily, "Month", summarize, AvgHotHours=mean(OutGE85Count))
    o.daily$AvgHotHours <- with(o.daily, round(AvgHotHours,2))
    o.daily$monthdisp <- factor(o.daily$Month, orderedMonths)
    
    cr.daily <- ddply(crot, c("Date","Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85, na.rm=TRUE), Count=sum(TimeUnit, na.rm=TRUE))
    cr.daily <- ddply(cr.daily, c("Month", "Alias"), summarize, AvgHotHours=mean(InGE85Count))
    cr.daily$AvgHotHours <- with(cr.daily, round(AvgHotHours,2))
    cr.daily$monthdisp <- factor(cr.daily$Month, orderedMonths)
    cr.daily$School <- sub("(.*?) - .*", "\\1", cr.daily$Alias)
    cr.daily$Hot <- ifelse(cr.daily$AvgHotHours>4, "Red", ifelse(cr.daily$AvgHotHours>2, "Orange", "Yellow"))
    
    s.daily <- ddply(crot, c("Date","Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85, na.rm=TRUE), Count=sum(TimeUnit, na.rm=TRUE))
    s.daily <- ddply(s.daily, c("Month", "School"), summarize, AvgHotHours=mean(InGE85Count))
    s.daily$AvgHotHours <- with(s.daily, round(AvgHotHours,2))
    s.daily$monthdisp <- factor(s.daily$Month, orderedMonths)
    
    ann <- s.daily[s.daily$School==school,]
    ann$x <- 1
    ann$y <- ann$AvgHotHours + .15
    ann$lab <- paste0("School Average: ", ann$AvgHotHours)
    ann <- ann[,c("x", "y", "lab", "Month","School")]
    ann$monthdisp <- factor(ann$Month, orderedMonths)
   
    anno <- o.daily
    anno$x <- ann.size
    anno$y <- anno$AvgHotHours + .15
    anno$lab <- paste0("Outdoor: ", anno$AvgHotHours)
    anno <- anno[,c("x", "y", "lab", "Month")]
    anno$monthdisp <- factor(anno$Month, orderedMonths)
    
    plot.title <- schoolTitle 
    plot.subtitle <- "Number of School Hours Above 85 F, All Days"
    
    b1 <- ggplot() + geom_bar(data=cr.daily[cr.daily$School==school,], aes(x=Alias, y=AvgHotHours, fill=Hot), alpha=0.5, stat="identity") + 
     geom_hline(data=o.daily, aes(yintercept=AvgHotHours), color="dimgrey") +
     geom_hline(data=s.daily[s.daily$School==school,], aes(yintercept=AvgHotHours), color="dimgrey", linetype="dashed", alpha=0.8) +
     geom_text(data=anno, aes(x, y, label=lab, group=1), color="dimgrey", size=2, hjust="right") + 
     geom_text(data=ann, aes(x, y, label=lab, group=1), color="dimgrey", size=2, hjust="left") + 
     facet_grid(~monthdisp, drop=FALSE) +
     scale_fill_manual(values=colorsHeat) +
     scale_y_continuous(breaks=seq(0,6,1), limits=c(0,6)) +
     ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
     theme_fivethirtyeight() +
     theme(axis.text.x = element_blank(), legend.position="none",  text=element_text(size=9))
    
    ## Reaching 85
    hot <- ddply(crot, c("Date"), summarize, Hot=sum(OutGE85, na.rm=TRUE)) 
    hot <- hot[hot$Hot>0,]
    crotHot <- merge(crot, hot, by="Date")
    
    o.daily <- ddply(crotHot, c("Date","Alias", "Month"), summarize, OutGE85Count=sum(OutGE85, na.rm=TRUE), Count=sum(OutTimeUnit, na.rm=TRUE))
    o.daily <- ddply(o.daily, "Month", summarize, AvgHotHours=mean(OutGE85Count))
    o.daily$AvgHotHours <- with(o.daily, round(AvgHotHours,2))
    o.daily$monthdisp <- factor(o.daily$Month, orderedMonths)
    
    cr.daily <- ddply(crotHot, c("Date","Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85, na.rm=TRUE), Count=sum(TimeUnit, na.rm=TRUE))
    cr.daily <- ddply(cr.daily, c("Month", "Alias"), summarize, AvgHotHours=mean(InGE85Count))
    cr.daily$AvgHotHours <- with(cr.daily, round(AvgHotHours,2))
    cr.daily$monthdisp <- factor(cr.daily$Month, orderedMonths)
    cr.daily$School <- sub("(.*?) - .*", "\\1", cr.daily$Alias)
    cr.daily$Hot <- ifelse(cr.daily$AvgHotHours>4, "Red", ifelse(cr.daily$AvgHotHours>2, "Orange", "Yellow"))
    
    s.daily <- ddply(crotHot, c("Date","Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85, na.rm=TRUE), Count=sum(TimeUnit, na.rm=TRUE))
    s.daily <- ddply(s.daily, c("Month", "School"), summarize, AvgHotHours=mean(InGE85Count))
    s.daily$AvgHotHours <- with(s.daily, round(AvgHotHours,2))
    s.daily$monthdisp <- factor(s.daily$Month, orderedMonths)
  
    ann <- s.daily[s.daily$School==school,]
    ann$x <- 1
    ann$y <- ann$AvgHotHours + .15
    ann$lab <- paste0("School Average: ", ann$AvgHotHours)
    ann <- ann[,c("x", "y", "lab", "Month","School")]
    ann$monthdisp <- factor(ann$Month, orderedMonths)
    
    anno <- o.daily
    anno$x <- ann.size
    anno$y <- anno$AvgHotHours + .15
    anno$lab <- paste0("Outdoor: ", anno$AvgHotHours)
    anno <- anno[,c("x", "y", "lab", "Month")]
    anno$monthdisp <- factor(anno$Month, orderedMonths)
    
    plot.title <- ""
    plot.subtitle <- "Number of School Hours Above 85 F, Days Reaching 85 F"
    
    b2 <- ggplot() + geom_bar(data=cr.daily[cr.daily$School==school,], aes(x=Alias, y=AvgHotHours, fill=Hot), alpha=0.5, stat="identity") + 
     geom_hline(data=o.daily, aes(yintercept=AvgHotHours), color="dimgrey") +
     geom_hline(data=s.daily[s.daily$School==school,], aes(yintercept=AvgHotHours), color="dimgrey", linetype="dashed", alpha=0.8) +
     geom_text(data=anno, aes(x, y, label=lab, group=1), color="dimgrey", size=2, hjust="right") + 
     geom_text(data=ann, aes(x, y, label=lab, group=1), color="dimgrey", size=2, hjust="left") + 
     facet_grid(~monthdisp, drop=FALSE) +
     scale_fill_manual(values=colorsHeat) +
     scale_y_continuous(breaks=seq(0,6,1), limits=c(0,6)) +
     ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
     theme_fivethirtyeight() +
      theme(axis.text.x = element_blank(), legend.position="none",  text=element_text(size=9))
    
    ## Hottest day
    crotMax <- merge(crot, hotMonths, by=c("Date","Month"))
    
    o.daily <- ddply(crotMax, c("Date","Alias", "Month"), summarize, OutGE85Count=sum(OutGE85, na.rm=TRUE), Count=sum(OutTimeUnit, na.rm=TRUE))
    o.daily <- ddply(o.daily, "Month", summarize, AvgHotHours=mean(OutGE85Count))
    o.daily$AvgHotHours <- with(o.daily, round(AvgHotHours,2))
    o.daily$monthdisp <- factor(o.daily$Month, orderedMonths)
    
    cr.daily <- ddply(crotMax, c("Date","Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85, na.rm=TRUE), Count=sum(TimeUnit, na.rm=TRUE))
    cr.daily <- ddply(cr.daily, c("Month", "Alias"), summarize, AvgHotHours=mean(InGE85Count))
    cr.daily$AvgHotHours <- with(cr.daily, round(AvgHotHours,2))
    cr.daily$monthdisp <- factor(cr.daily$Month, orderedMonths)
    cr.daily$School <- sub("(.*?) - .*", "\\1", cr.daily$Alias)
    cr.daily$Hot <- ifelse(cr.daily$AvgHotHours>4, "Red", ifelse(cr.daily$AvgHotHours>2, "Orange", "Yellow"))
    
    s.daily <- ddply(crotMax, c("Date","Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85, na.rm=TRUE), Count=sum(TimeUnit, na.rm=TRUE))
    s.daily <- ddply(s.daily, c("Month", "School"), summarize, AvgHotHours=mean(InGE85Count))
    s.daily$AvgHotHours <- with(s.daily, round(AvgHotHours,2))
    s.daily$monthdisp <- factor(s.daily$Month, orderedMonths)
    
    anno <- o.daily
    anno$x <- ann.size
    anno$y <- anno$AvgHotHours + .15
    anno$lab <- paste0("Outdoor: ", anno$AvgHotHours)
    anno <- anno[,c("x", "y", "lab", "Month")]
    anno$monthdisp <- factor(anno$Month, orderedMonths)
    
    ann <- s.daily[s.daily$School==school,]
    ann$x <- 1
    ann$y <- ann$AvgHotHours + .15
    ann$lab <- paste0("School Average: ", ann$AvgHotHours)
    ann <- ann[,c("x", "y", "lab", "Month","School")]
    ann$monthdisp <- factor(ann$Month, orderedMonths)
    
    plot.subtitle <- "Number of School Hours Above 85 F, Hottest Day"
    
    b3 <- ggplot() + geom_bar(data=cr.daily[cr.daily$School==school,], aes(x=Alias, y=AvgHotHours, fill=Hot), alpha=0.5, stat="identity") + 
     geom_hline(data=o.daily, aes(yintercept=AvgHotHours), color="dimgrey") +
     geom_hline(data=s.daily[s.daily$School==school,], aes(yintercept=AvgHotHours), color="dimgrey", linetype="dashed", alpha=0.8) +
     geom_text(data=anno, aes(x, y, label=lab, group=1), color="dimgrey", size=2, hjust="right") + 
     geom_text(data=ann, aes(x, y, label=lab, group=1), color="dimgrey", size=2, hjust="left") + 
     facet_grid(~monthdisp, drop=FALSE) +
     scale_fill_manual(values=colorsHeat) +
     scale_y_continuous(breaks=seq(0,6,1), limits=c(0,6)) +
     ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
     theme_fivethirtyeight() +
      theme(axis.text.x = element_blank(), legend.position="none",  text=element_text(size=9))
    
    grid.arrange(b1,b2,b3, layout_matrix=rbind(c(1), c(2), c(3)))
    
    
    ### Line
    ## All days
    cr.hourly <- ddply(crt, c("Time", "Alias", "Month", "School"), summarize, AvgTemp=mean(Temp))
    o.hourly <- ddply(ot, c("Time", "Month"), summarize, AvgTemp=mean(OutdoorTemp))
    
    o.hourly$Time <- as.POSIXct(o.hourly$Time, format="%H:%M", tz="UTC")
    cr.hourly$Time <- as.POSIXct(cr.hourly$Time, format="%H:%M", tz="UTC")
    o.hourly$monthdisp <- factor(o.hourly$Month, orderedMonths)
    cr.hourly$monthdisp <- factor(cr.hourly$Month, orderedMonths)
    
    lims <- as.POSIXct(strptime(c("08:00","14:00"), format = "%H:%M"), tz="UTC")    
    plot.title <- schoolTitle
    plot.subtitle <- "Average School Day Temperature, All Observations"
    
    l1 <- ggplot() + 
     geom_hline(yintercept=85, linetype="dashed", color="dimgrey", size=0.5) +
     geom_line(data=cr.hourly[cr.hourly$School==school,], aes(x=Time, y=AvgTemp, group=Alias, color=Alias), size=0.5, alpha=0.5) +
     geom_line(data=o.hourly, aes(x=Time, y=AvgTemp, group=1), color="dimgrey", size=0.6) +
     facet_grid(~monthdisp, drop=FALSE) +
     scale_y_continuous(breaks=seq(70,100,5), limits=c(70,100)) +
     scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
     ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
     theme_fivethirtyeight() + theme(legend.position="none",  text=element_text(size=9))
  
    ## Obs over 85
    otHot <- ot[ot$OutdoorTemp>=85,]
    crtHot <- merge(otHot, crt, by=c("Date", "Time", "Month"))  
    cr.hourly <- ddply(crtHot, c("Time", "Alias", "Month", "School"), summarize, AvgTemp=mean(Temp))
    o.hourly <- ddply(otHot, c("Time", "Month"), summarize, AvgTemp=mean(OutdoorTemp))
    
    o.hourly$Time <- as.POSIXct(o.hourly$Time, format="%H:%M", tz="UTC")
    cr.hourly$Time <- as.POSIXct(cr.hourly$Time, format="%H:%M", tz="UTC")
    o.hourly$monthdisp <- factor(o.hourly$Month, orderedMonths)
    cr.hourly$monthdisp <- factor(cr.hourly$Month, orderedMonths)
    
    plot.title <- ""
    plot.subtitle <- "Average School Day Temperature, Observations >85 F"
    
    l2 <- ggplot() + 
      geom_hline(yintercept=85, linetype="dashed", color="dimgrey", size=0.5) +
      geom_line(data=cr.hourly[cr.hourly$School==school,], aes(x=Time, y=AvgTemp, group=Alias, color=Alias), size=0.5, alpha=0.5) +
      geom_line(data=o.hourly, aes(x=Time, y=AvgTemp, group=1), color="dimgrey", size=0.6) +
      facet_grid(~monthdisp, drop=FALSE) +
      scale_y_continuous(breaks=seq(75,105,5), limits=c(75,105)) +
      scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
      ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
      theme_fivethirtyeight() + theme(legend.position="none",  text=element_text(size=9))
    
    ## Hottest day
    cr.hourly <- merge(crt, hotMonths, by=c("Date","Month"))
    o.hourly <- merge(ot, hotMonths, by=c("Date","Month"))
    
    o.hourly$Time <- as.POSIXct(o.hourly$Time, format="%H:%M", tz="UTC")
    cr.hourly$Time <- as.POSIXct(cr.hourly$Time, format="%H:%M", tz="UTC")
    o.hourly$monthdisp <- factor(o.hourly$Month, orderedMonths)
    cr.hourly$monthdisp <- factor(cr.hourly$Month, orderedMonths)
    
    plot.title <- ""
    plot.subtitle <- "Average School Day Temperature, Hottest Day"
    
    l3 <- ggplot() + 
      geom_hline(yintercept=85, linetype="dashed", color="dimgrey", size=0.5) +
      geom_line(data=cr.hourly[cr.hourly$School==school,], aes(x=Time, y=Temp, group=Alias, color=Alias), size=0.5, alpha=0.5) +
      geom_line(data=o.hourly, aes(x=Time, y=OutdoorTemp, group=1), color="dimgrey", size=0.6) +
      facet_grid(~monthdisp, drop=FALSE) +
      scale_y_continuous(breaks=seq(70,100,5), limits=c(70,100)) +
      scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
      ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
      theme_fivethirtyeight() + theme(legend.position="none",  text=element_text(size=9))
    
    grid.arrange(l1,l2,l3, layout_matrix=rbind(c(1), c(2), c(3)))
    
  }    
    