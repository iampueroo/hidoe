  OverviewPlot <- function(school) {
    library(plyr)
    library(reshape2)
    library(ggplot2)
    library(ggthemes) 
    library(scales)
    library(grid)
    library(gridExtra) 
    library(gtable)

    
    ### Defined variables
    minTemp <- 85
    orderedMonths <- c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May")
    colors <- c( "#30A2DA", "#FC4F30")
    colorsHeat <- c("#E69720", "#FC4F30", "#E8E827") 
    palette <- colorRampPalette(colors=c("green", "navy"))
    
  
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
    crt <-read.csv(file="~/dropbox/rh1/hidoe/final-csv/all-classroom-sensor.csv", sep=",") #classroom
    crt$Date <- as.Date(crt$Date, format="%Y-%m-%d")
    crt$Time <- format(strptime(crt$Time, format="%H:%M"), format="%H:%M")
    crt$Month <- format(crt$Date, "%B") 
    
    ot <- read.csv(file="~/dropbox/rh1/hidoe/final-csv/all-outdoor-sensor.csv", sep=",") #outdoor 
    ot$Date <- as.Date(ot$Date, format="%Y-%m-%d")
    ot$Time <- format(strptime(ot$Time, format="%H:%M"), format="%H:%M")
    ot$Month <- format(ot$Date, "%B")
    
    hotMonths <- do.call("rbind", by(ot, ot$Month, function(x) x[which.max(x$OutdoorUTCI),]))
    hotMonths <- hotMonths[,c("Date", "Month")]
    
    ### Check overlapping summer months
    #ot$Year <- as.factor(year(ot$Date))
    #ot$Day <- as.character(format(ot$Date, "%m-%d"))
    #otoverlap <- ot[ot$Month %in% c("August", "September", "October"),]
    #ggplot(otoverlap, aes(x=Date, y=OutdoorTemp)) + geom_point(color="blue", alpha=0.5) + geom_smooth(size=1, color="red") + facet_grid(~Year) + ggtitle("Average Temperature of Overlapping Months (2013-2014)") + theme_fivethirtyeight()
    
    #year(otoverlap$Date) <- 2013
    #ggplot() + geom_smooth(data=otoverlap, aes(x=Day, y=OutdoorTemp, color=Year, group=Year)) + scale_y_continuous(breaks=seq(70,100,5), limits=c(70,100)) + ggtitle("Average Temperature of Overlapping Months (2013-2014)") + theme_fivethirtyeight()
    
    ### Plots
    ## Profile
    ot.daily <- ddply(ot,c("Date"), summarise, AvgOutdoorTemp=mean(OutdoorTemp, na.rm=TRUE), AvgOutdoorUTCI=mean(OutdoorUTCI, na.rm=TRUE)) #aggregated by date 
    crt.daily <- ddply(crt, c("Date", "Alias", "School"), summarise, AvgRoomTemp=mean(Temp, na.rm=TRUE), MaxRoomTemp=max(Temp,na.rm=TRUE), AvgRoomUTCI=mean(UTCI, na.rm=TRUE), MaxRoomUTCI=max(UTCI, na.rm=TRUE)) #aggregated  by date
    cro.daily <- data.frame(merge(crt.daily, ot.daily, by=c("Date"), all.x=TRUE, all.y=TRUE)) 
    cro.daily <- melt(cro.daily, id.vars=c("Date", "Alias", "School"))
    
    p <- ggplot() +
      geom_hline(yintercept=minTemp, linetype="dotted", color="black", size=0.5) + 
      geom_point(data=cro.daily[cro.daily$School==school & cro.daily$variable %in% c("MaxRoomUTCI", "AvgRoomUTCI"),], aes(x=Date, y=value, color=variable), alpha=0.2) +
      geom_smooth(data=cro.daily[cro.daily$School==school & cro.daily$variable %in% c("AvgOutdoorUTCI", "AvgOutdoorTemp"),], aes(x=Date, y=value, linetype=variable, size=variable), color="dimgrey", fill=NA) +
      geom_smooth(data=cro.daily[cro.daily$School==school & cro.daily$variable %in% c("AvgRoomUTCI", "MaxRoomUTCI"),], aes(x=Date, y=value, color=variable), linetype="solid", size=0.8, fill=NA) +
      geom_smooth(data=cro.daily[cro.daily$School==school & cro.daily$variable %in% c("MaxRoomTemp"),], aes(x=Date, y=value), color="#FC4F30", linetype="dashed", size=0.5, fill=NA) +
      geom_smooth(data=cro.daily[cro.daily$School==school & cro.daily$variable %in% c("AvgRoomTemp"),], aes(x=Date, y=value), color="#30A2DA", linetype="dashed", size=0.5, fill=NA) +
      scale_y_continuous(breaks=seq(70,105,5), limits = c(70,105)) + 
      scale_x_date(date_breaks="1 month", date_labels="%b '%y") + 
      ggtitle(paste("Average Daily Profile: ", schoolTitle)) +
      scale_linetype_manual(name="", labels=c("Temperature", "Universal Thermal Climate Index"), values=c("dashed","solid")) +
      scale_colour_manual(name = "", labels=c("Classroom Average (UTCI)", "Classroom Maximum (UTCI)"), values=colors) +   
      scale_size_manual(name="", labels=c("Outdoor", ""), values=c(0.5,0.8)) +
      theme_fivethirtyeight() +
      theme(text=element_text(size=9), legend.title=element_blank(), legend.position=c(0.2,0.08), legend.background=element_rect(color="grey", fill="#F0F0F0", size=0.4, linetype="solid"), legend.box="horizontal") +
      guides(color = guide_legend(override.aes = list(fill=NA, alpha=0.8, size=1.5, linetype=0)), 
             linetype = guide_legend(override.aes = list(fill=NA, size=0.5, order=1)),  
             size = guide_legend(override.aes = list(color=c("dimgrey",NA), size=0.8)))
      
    print(p)
    
    ### Bar plot
    ## All days
    crot <- merge(crt, ot, by=c("Time", "Month", "Date"), all.x=TRUE)
    crot$TimeUnit <- ifelse(crot$Time=="08:00",0,.25) #start counting hours after 8 AM
    crot$OutTimeUnit <- ifelse((is.na(crot$OutdoorUTCI) | crot$Time=="08:00"),0,.25) 
    crot$InGE85 <- ifelse((crot$UTCI>=minTemp)==TRUE & crot$Time!="08:00",.25,0)
    crot$OutGE85 <-ifelse((crot$OutdoorUTCI>=minTemp)==TRUE & crot$Time!="08:00",.25,0)

    o.daily <- ddply(crot, c("Date","Alias", "Month"), summarize, OutGE85Count=sum(OutGE85, na.rm=TRUE), Count=sum(OutTimeUnit, na.rm=TRUE))
    o.daily <- ddply(o.daily, "Month", summarize, AvgHotHours=mean(OutGE85Count, na.rm=TRUE))
    o.daily$AvgHotHours <- with(o.daily, round(AvgHotHours,2))
    o.daily$monthdisp <- factor(o.daily$Month, orderedMonths)
    
    cr.daily <- ddply(crot, c("Date","Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85, na.rm=TRUE), Count=sum(TimeUnit, na.rm=TRUE))
    cr.daily <- ddply(cr.daily, c("Month", "Alias"), summarize, AvgHotHours=mean(InGE85Count, na.rm=TRUE))
    cr.daily$AvgHotHours <- with(cr.daily, round(AvgHotHours,2))
    cr.daily$monthdisp <- factor(cr.daily$Month, orderedMonths)
    cr.daily$School <- sub("(.*?) - .*", "\\1", cr.daily$Alias)
    cr.daily$Hot <- ifelse(cr.daily$AvgHotHours>4, "Red", ifelse(cr.daily$AvgHotHours>2, "Orange", "Yellow"))
    
    s.daily <- ddply(crot, c("Date","Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85, na.rm=TRUE), Count=sum(TimeUnit, na.rm=TRUE))
    s.daily <- ddply(s.daily, c("Month", "School"), summarize, AvgHotHours=mean(InGE85Count, na.rm=TRUE))
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
    plot.subtitle <- "Number of School Hours Above 85 F UTCI, All Days"
    
    b1 <- ggplot() + geom_bar(data=cr.daily[cr.daily$School==school,], aes(x=Alias, y=AvgHotHours, fill=Hot), alpha=0.5, stat="identity") + 
     geom_hline(data=o.daily, aes(yintercept=AvgHotHours), color="dimgrey") +
     geom_hline(data=s.daily[s.daily$School==school,], aes(yintercept=AvgHotHours), color="dimgrey", linetype="dashed", alpha=0.8) +
     geom_text(data=anno, aes(x, y, label=lab, group=1), color="dimgrey", size=2.5, hjust="right") + 
     geom_text(data=ann, aes(x, y, label=lab, group=1), color="dimgrey", size=2.5, hjust="left") + 
     facet_grid(~monthdisp, drop=FALSE) +
     scale_fill_manual(values=colorsHeat) +
     scale_y_continuous(breaks=seq(0,6,1), limits=c(0,6.2)) +
     ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
     theme_fivethirtyeight() +
     theme(axis.text.x = element_blank(), legend.position="none",  text=element_text(size=9))
    
    ## Reaching 85
    hot <- ddply(crot, c("Date"), summarize, Hot=sum(OutGE85, na.rm=TRUE)) 
    hot <- hot[hot$Hot>0,]
    crotHot <- merge(crot, hot, by="Date")
    
    o.daily <- ddply(crotHot, c("Date","Alias", "Month"), summarize, OutGE85Count=sum(OutGE85, na.rm=TRUE), Count=sum(OutTimeUnit, na.rm=TRUE))
    o.daily <- ddply(o.daily, "Month", summarize, AvgHotHours=mean(OutGE85Count, na.rm=TRUE))
    o.daily$AvgHotHours <- with(o.daily, round(AvgHotHours,2))
    o.daily$monthdisp <- factor(o.daily$Month, orderedMonths)
    
    cr.daily <- ddply(crotHot, c("Date","Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85, na.rm=TRUE), Count=sum(TimeUnit, na.rm=TRUE))
    cr.daily <- ddply(cr.daily, c("Month", "Alias"), summarize, AvgHotHours=mean(InGE85Count, na.rm=TRUE))
    cr.daily$AvgHotHours <- with(cr.daily, round(AvgHotHours,2))
    cr.daily$monthdisp <- factor(cr.daily$Month, orderedMonths)
    cr.daily$School <- sub("(.*?) - .*", "\\1", cr.daily$Alias)
    cr.daily$Hot <- ifelse(cr.daily$AvgHotHours>4, "Red", ifelse(cr.daily$AvgHotHours>2, "Orange", "Yellow"))
    
    s.daily <- ddply(crotHot, c("Date","Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85, na.rm=TRUE), Count=sum(TimeUnit, na.rm=TRUE))
    s.daily <- ddply(s.daily, c("Month", "School"), summarize, AvgHotHours=mean(InGE85Count, na.rm=TRUE))
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
    plot.subtitle <- "Number of School Hours Above 85 F UTCI, Days Reaching 85 F UTCI"
    
    b2 <- ggplot() + geom_bar(data=cr.daily[cr.daily$School==school,], aes(x=Alias, y=AvgHotHours, fill=Hot), alpha=0.5, stat="identity") + 
     geom_hline(data=o.daily, aes(yintercept=AvgHotHours), color="dimgrey") +
     geom_hline(data=s.daily[s.daily$School==school,], aes(yintercept=AvgHotHours), color="dimgrey", linetype="dashed", alpha=0.8) +
     geom_text(data=anno, aes(x, y, label=lab, group=1), color="dimgrey", size=2.5, hjust="right") + 
     geom_text(data=ann, aes(x, y, label=lab, group=1), color="dimgrey", size=2.5, hjust="left") + 
     facet_grid(~monthdisp, drop=FALSE) +
     scale_fill_manual(values=colorsHeat) +
     scale_y_continuous(breaks=seq(0,6,1), limits=c(0,6.2)) +
     ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
     theme_fivethirtyeight() +
     theme(axis.text.x = element_blank(), legend.position="none",  text=element_text(size=9))
    
    ## Hottest day
    crotMax <- merge(crot, hotMonths, by=c("Date","Month"))
    
    o.daily <- ddply(crotMax, c("Date","Alias", "Month"), summarize, OutGE85Count=sum(OutGE85, na.rm=TRUE), Count=sum(OutTimeUnit, na.rm=TRUE))
    o.daily <- ddply(o.daily, "Month", summarize, AvgHotHours=mean(OutGE85Count, na.rm=TRUE))
    o.daily$AvgHotHours <- with(o.daily, round(AvgHotHours,2))
    o.daily$monthdisp <- factor(o.daily$Month, orderedMonths)
    
    cr.daily <- ddply(crotMax, c("Date","Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85, na.rm=TRUE), Count=sum(TimeUnit, na.rm=TRUE))
    cr.daily <- ddply(cr.daily, c("Month", "Alias"), summarize, AvgHotHours=mean(InGE85Count, na.rm=TRUE))
    cr.daily$AvgHotHours <- with(cr.daily, round(AvgHotHours,2))
    cr.daily$monthdisp <- factor(cr.daily$Month, orderedMonths)
    cr.daily$School <- sub("(.*?) - .*", "\\1", cr.daily$Alias)
    cr.daily$Hot <- ifelse(cr.daily$AvgHotHours>4, "Red", ifelse(cr.daily$AvgHotHours>2, "Orange", "Yellow"))
    
    s.daily <- ddply(crotMax, c("Date","Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85, na.rm=TRUE), Count=sum(TimeUnit, na.rm=TRUE))
    s.daily <- ddply(s.daily, c("Month", "School"), summarize, AvgHotHours=mean(InGE85Count, na.rm=TRUE))
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
    
    plot.subtitle <- "Number of School Hours Above 85 F UTCI, Hottest Day"
    
    b3 <- ggplot() + geom_bar(data=cr.daily[cr.daily$School==school,], aes(x=Alias, y=AvgHotHours, fill=Hot), alpha=0.5, stat="identity") + 
     geom_hline(data=o.daily, aes(yintercept=AvgHotHours), color="dimgrey") +
     geom_hline(data=s.daily[s.daily$School==school,], aes(yintercept=AvgHotHours), color="dimgrey", linetype="dashed", alpha=0.8) +
     geom_text(data=anno, aes(x, y, label=lab, group=1), color="dimgrey", size=2.5, hjust="right") + 
     geom_text(data=ann, aes(x, y, label=lab, group=1), color="dimgrey", size=2.5, hjust="left") + 
     facet_grid(~monthdisp, drop=FALSE) +
     scale_fill_manual(values=colorsHeat) +
     scale_y_continuous(breaks=seq(0,6,1), limits=c(0,6.2)) +
     ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
     theme_fivethirtyeight() +
     theme(axis.text.x = element_blank(), legend.position="none",  text=element_text(size=9))
    
    grid.arrange(b1,b2,b3, layout_matrix=rbind(c(1), c(2), c(3)))
    
    
    ### Line
    ## All days
    cr.hourly <- ddply(crt, c("Time", "Alias", "Month", "School"), summarize, AvgTemp=mean(Temp, na.rm=TRUE), AvgUTCI=mean(UTCI, na.rm=TRUE))
    o.hourly <- ddply(ot, c("Time", "Month"), summarize, AvgTemp=mean(OutdoorTemp, na.rm=TRUE), AvgUTCI=mean(OutdoorUTCI, na.rm=TRUE))
    
    cr.hourly <- melt(cr.hourly, id.vars=c("Time", "Month", "Alias", "School"))
    o.hourly <- melt(o.hourly, id.vars=c("Time", "Month"))
    o.hourly$Time <- as.POSIXct(o.hourly$Time, format="%H:%M", tz="UTC")
    cr.hourly$Time <- as.POSIXct(cr.hourly$Time, format="%H:%M", tz="UTC")
    o.hourly$monthdisp <- factor(o.hourly$Month, orderedMonths)
    cr.hourly$monthdisp <- factor(cr.hourly$Month, orderedMonths)
    
    lims <- as.POSIXct(strptime(c("08:00","14:00"), format = "%H:%M"), tz="UTC")    
    plot.title <- schoolTitle
    plot.subtitle <- "Average School Day UTCI, All Observations"
    
    l1 <- ggplot() + 
     geom_hline(yintercept=minTemp, linetype="dotted", color="black", size=0.3) +
     geom_line(data=cr.hourly[cr.hourly$School==school & cr.hourly$variable=="AvgUTCI",], aes(x=Time, y=value, group=Alias, color=Alias), size=0.3, alpha=0.5, show.legend = FALSE) +
     geom_line(data=o.hourly, aes(x=Time, y=value, linetype=variable, size=variable), color="dimgrey") +
     facet_grid(~monthdisp, drop=FALSE) +
     scale_y_continuous(breaks=seq(70,100,5), limits=c(70,100)) +
     scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
     scale_color_manual(values=palette(ann.size)) +
     scale_size_manual(name="", values=c(0.5,0.8)) +
     scale_linetype_manual(name = "", values=c("longdash","solid")) +
     ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
     theme_fivethirtyeight() + theme(legend.position="none", text=element_text(size=9))
  
    ## Obs over 85
    otHot <- ot[ot$OutdoorUTCI>=minTemp,]
    crtHot <- merge(otHot, crt, by=c("Date", "Time", "Month"))  
    cr.hourly <- ddply(crtHot, c("Time", "Alias", "Month", "School"), summarize, AvgTemp=mean(Temp, na.rm=TRUE), AvgUTCI=mean(UTCI, na.rm=TRUE))
    o.hourly <- ddply(otHot, c("Time", "Month"), summarize, AvgTemp=mean(OutdoorTemp, na.rm=TRUE), AvgUTCI=mean(OutdoorUTCI, na.rm=TRUE))
    
    cr.hourly <- melt(cr.hourly, id.vars=c("Time", "Month", "Alias", "School"))
    o.hourly <- melt(o.hourly, id.vars=c("Time", "Month"))
    o.hourly$Time <- as.POSIXct(o.hourly$Time, format="%H:%M", tz="UTC")
    cr.hourly$Time <- as.POSIXct(cr.hourly$Time, format="%H:%M", tz="UTC")
    o.hourly$monthdisp <- factor(o.hourly$Month, orderedMonths)
    cr.hourly$monthdisp <- factor(cr.hourly$Month, orderedMonths)
    
    plot.title <- ""
    plot.subtitle <- "Average School Day UTCI, Observations >85 F UTCI"
    
    l2 <- ggplot() + 
      geom_hline(yintercept=minTemp, linetype="dotted", color="black", size=0.3) +
      geom_point(data=cr.hourly[cr.hourly$School==school & cr.hourly$variable=="AvgUTCI",], aes(x=Time, y=value, color=Alias), size=0.5, alpha=0.5) +
      geom_smooth(data=o.hourly, aes(x=Time, y=value, linetype=variable, size=variable), color="dimgrey", fill=NA) +
      facet_grid(~monthdisp, drop=FALSE) +
      scale_linetype_manual(name = "", values=c("longdash","solid")) +
      scale_y_continuous(breaks=seq(75,105,5), limits=c(75,105)) +
      scale_color_manual(values=palette(ann.size)) +
      scale_size_manual(name="", values=c(0.5,0.8)) +
      scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
      ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
      theme_fivethirtyeight() + theme(legend.position="none",  text=element_text(size=9))
    
    ## Hottest day
    cr.hourly <- merge(crt[,c("Time", "Date", "Month", "Alias", "Temp", "UTCI", "School")], hotMonths, by=c("Date","Month"))
    o.hourly <- merge(ot, hotMonths, by=c("Date","Month"))
    
    o.hourly <- melt(o.hourly, id.vars=c("Time", "Month", "Date"))
    o.hourly$Time <- as.POSIXct(o.hourly$Time, format="%H:%M", tz="UTC")
    cr.hourly$Time <- as.POSIXct(cr.hourly$Time, format="%H:%M", tz="UTC")
    o.hourly$monthdisp <- factor(o.hourly$Month, orderedMonths)
    cr.hourly$monthdisp <- factor(cr.hourly$Month, orderedMonths)
    
    plot.title <- ""
    plot.subtitle <- "Average School Day UTCI, Hottest Day"
    
    l3 <- ggplot() + 
      geom_hline(yintercept=minTemp, linetype="dotted", color="black", size=0.5) +
      geom_line(data=cr.hourly[cr.hourly$School==school,], aes(x=Time, y=UTCI, group=Alias, color=Alias), size=0.3, alpha=0.5, show.legend = FALSE) +
      geom_line(data=o.hourly, aes(x=Time, y=value, linetype=variable, size=variable), color="dimgrey") +
      facet_grid(~monthdisp, drop=FALSE) +
      scale_y_continuous(breaks=seq(70,100,5), limits=c(70,100)) +
      scale_color_manual(values=palette(ann.size)) +
      scale_size_manual(name="", values=c(0.5,0.8)) +
      scale_linetype_manual(name = "", values=c("longdash","solid")) +
      scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
      ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
      theme_fivethirtyeight() + theme(text=element_text(size=9), legend.position="none")
    
    grid.arrange(l1,l2,l3, layout_matrix=rbind(c(1), c(2), c(3)))
    
  }    
    