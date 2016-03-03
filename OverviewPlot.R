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
    minF <- 85
    orderedMonths <- c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May")
    colors <- c( "dimgrey", "#FC4F30", "#30A2DA")
    colorsHeat <- c("#E69720", "#FC4F30", "#E8E827") 
    palette <- colorRampPalette(colors=c("green", "purple"))
    
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
    cr <-read.csv(file="~/dropbox/rh1/hidoe/final-csv/all-classroom-sensor.csv", sep=",") #classroom
    cr$Date <- as.Date(cr$Date, format="%Y-%m-%d")
    cr$Time <- format(strptime(cr$Time, format="%H:%M"), format="%H:%M")
    cr$Month <- format(cr$Date, "%B") 
    
    o <- read.csv(file="~/dropbox/rh1/hidoe/final-csv/all-outdoor-sensor.csv", sep=",") #outdoor 
    o$Date <- as.Date(o$Date, format="%Y-%m-%d")
    o$Time <- format(strptime(o$Time, format="%H:%M"), format="%H:%M")
    o$Month <- format(o$Date, "%B")
    
    hottest <- do.call("rbind", by(o, o$Month, function(x) x[which.max(x$OutdoorUTCI),]))
    hottest <- hottest[,c("Date", "Month")]
    
    ### Check overlapping summer months
    #ot$Year <- as.factor(year(ot$Date))
    #ot$Day <- as.character(format(ot$Date, "%m-%d"))
    #otoverlap <- ot[ot$Month %in% c("August", "September", "October"),]
    #ggplot(otoverlap, aes(x=Date, y=OutdoorTemp)) + geom_point(color="blue", alpha=0.5) + geom_smooth(size=1, color="red") + facet_grid(~Year) + ggtitle("Average Temperature of Overlapping Months (2013-2014)") + theme_fivethirtyeight()
    
    #year(otoverlap$Date) <- 2013
    #ggplot() + geom_smooth(data=otoverlap, aes(x=Day, y=OutdoorTemp, color=Year, group=Year)) + scale_y_continuous(breaks=seq(70,100,5), limits=c(70,100)) + ggtitle("Average Temperature of Overlapping Months (2013-2014)") + theme_fivethirtyeight()
    
    ### Plots
    ## Profile
    o.daily <- ddply(o,c("Date"), summarise, AvgOutTemp=mean(OutdoorTemp, na.rm=TRUE), AvgOutdoorUTCI=mean(OutdoorUTCI, na.rm=TRUE)) #aggregated by date 
    cr.daily <- ddply(cr, c("Date", "Alias", "School"), summarise, AvgRoomTemp=mean(Temp, na.rm=TRUE), MaxRoomTemp=max(Temp,na.rm=TRUE), AvgRoomUTCI=mean(UTCI, na.rm=TRUE), MaxRoomUTCI=max(UTCI, na.rm=TRUE)) #aggregated  by date
    daily <- data.frame(merge(cr.daily, o.daily, by=c("Date"), all.x=TRUE, all.y=TRUE)) 
    daily <- melt(daily, id.vars=c("Date", "Alias", "School"))
    
    daily$measure <- as.factor(ifelse(substr(as.character(daily$variable), nchar(as.character(daily$variable))-3, nchar(as.character(daily$variable)))=="Temp", "Temp", "UTCI"))
    daily$level <- as.factor(ifelse(substr(as.character(daily$variable), 4, 6)=="Out", "Outdoor", 
                                    ifelse(substr(as.character(daily$variable), 1, 3)=="Avg", "Average", "Maximum")))
    
    p <- ggplot() +
      geom_hline(yintercept=minF, linetype="dotted", color="black", size=0.5) + 
      geom_point(data=daily[daily$School==school & daily$variable %in% c("AvgRoomUTCI", "MaxRoomUTCI"),], aes(x=Date, y=value, color=variable, shape=level), alpha=0.2) +
      geom_smooth(data=daily[daily$School==school,], aes(x=Date, y=value, linetype=variable, size=variable, color=variable), fill=NA) +
      scale_color_manual(name = "", labels=c("Avg Outdoor UTCI", "Avg Outdoor Temp", "Avg Classroom Temp", "Avg Classroom UTCI", "Max Classroom Temp", "Max Classroom UTCI"),
                         values=c("dimgrey", "dimgrey", "#30A2DA", "#30A2DA", "#FC4F30", "#FC4F30")) +   
      scale_linetype_manual(name="", values=c("dashed","dashed", "solid", "solid", "dashed", "solid"), guide=FALSE) +
      scale_size_manual(name="", values=c(0.5, 0.5, 0.8, 0.8, 0.5, 0.8), guide=FALSE) +
      scale_shape_manual(name="", labels=c("Classroom (Average UTCI)", "Classroom (Maxmimum UTCI)"), values=c(16, 16)) +
      scale_y_continuous(breaks=seq(70,105,5), limits = c(70,105)) + 
      scale_x_date(date_breaks="1 month", date_labels="%b '%y") + 
      ggtitle(paste(" Daily Universal Thermal Climate Index (UTCI) Profile: ", schoolTitle)) +
      theme_fivethirtyeight() +
      theme(text=element_text(size=9), legend.title=element_blank(), legend.position=c(0.148,0.08), legend.background=element_rect(color="grey", fill="#F0F0F0", size=0.4, linetype="solid"), legend.box="horizontal") +
      guides(color = guide_legend(override.aes = list(size=c(0.8,0.5,0.5,0.8,0.5,0.8), linetype=c(1,2,2,1,2,1), shape=NA)),
             shape = guide_legend(ncol=1, override.aes = list(alpha=c(0.6,0.6), color=c("#30A2DA", "#FC4F30"))))
    
    print(p)
    
    ### Bar plot
    ## All days
    cro <- merge(cr, o, by=c("Time", "Month", "Date"), all.x=TRUE)
    cro$TimeUnit <- ifelse(cro$Time=="08:00",0,.25) #start counting hours after 8 AM
    cro$OutTimeUnit <- ifelse((is.na(cro$OutdoorUTCI) | cro$Time=="08:00"),0,.25) 
    cro$InGE85 <- ifelse((cro$UTCI>=minF)==TRUE & cro$Time!="08:00",.25,0)
    cro$OutGE85 <-ifelse((cro$OutdoorUTCI>=minF)==TRUE & cro$Time!="08:00",.25,0)
    
    o.daily <- ddply(cro, c("Date","Alias", "Month"), summarize, OutGE85Count=sum(OutGE85, na.rm=TRUE), Count=sum(OutTimeUnit, na.rm=TRUE))
    o.daily <- ddply(o.daily, "Month", summarize, AvgHotHours=mean(OutGE85Count, na.rm=TRUE))
    o.daily$AvgHotHours <- with(o.daily, round(AvgHotHours,2))
    o.daily$monthdisp <- factor(o.daily$Month, orderedMonths)
    
    cr.daily <- ddply(cro, c("Date","Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85, na.rm=TRUE), Count=sum(TimeUnit, na.rm=TRUE))
    cr.daily <- ddply(cr.daily, c("Month", "Alias"), summarize, AvgHotHours=mean(InGE85Count, na.rm=TRUE))
    cr.daily$AvgHotHours <- with(cr.daily, round(AvgHotHours,2))
    cr.daily$monthdisp <- factor(cr.daily$Month, orderedMonths)
    cr.daily$School <- sub("(.*?) - .*", "\\1", cr.daily$Alias)
    cr.daily$Hot <- ifelse(cr.daily$AvgHotHours>4, "Red", ifelse(cr.daily$AvgHotHours>2, "Orange", "Yellow"))
    
    ann <- o.daily
    ann$x <- 1
    ann$y <- ann$AvgHotHours + .15
    ann$lab <- paste0("Outdoor Average: ", ann$AvgHotHours, " hours")
    ann <- ann[,c("x", "y", "lab", "Month")]
    ann$monthdisp <- factor(ann$Month, orderedMonths)
    
    plot.title <- schoolTitle 
    plot.subtitle <- "School Hours Above 85°F UTCI, All Days"
    
    b1 <- ggplot() + 
      geom_bar(data=cr.daily[cr.daily$School==school,], aes(x=Alias, y=AvgHotHours, fill=Hot), alpha=0.5, stat="identity") + 
      geom_hline(data=o.daily, aes(yintercept=AvgHotHours), color="dimgrey", size=0.3) +
      geom_text(data=ann, aes(x, y, label=lab, group=1), color="dimgrey", size=2.5, hjust="left") + 
      facet_grid(~monthdisp, drop=FALSE) +
      scale_fill_manual(name="", values=colorsHeat) +
      scale_y_continuous(breaks=seq(0,6,1), limits=c(0,6.15)) +
      ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
      theme_fivethirtyeight() +
      theme(axis.text.x = element_blank(), legend.position="none",  text=element_text(size=9))
    
    ## Reaching 85
    hot <- ddply(cro, c("Date"), summarize, Hot=sum(OutGE85, na.rm=TRUE)) 
    hot <- hot[hot$Hot>0,]
    croHot <- merge(cro, hot, by="Date")
    
    o.daily <- ddply(croHot, c("Date","Alias", "Month"), summarize, OutGE85Count=sum(OutGE85, na.rm=TRUE), Count=sum(OutTimeUnit, na.rm=TRUE))
    o.daily <- ddply(o.daily, "Month", summarize, AvgHotHours=mean(OutGE85Count, na.rm=TRUE))
    o.daily$AvgHotHours <- with(o.daily, round(AvgHotHours,2))
    o.daily$monthdisp <- factor(o.daily$Month, orderedMonths)
    
    cr.daily <- ddply(croHot, c("Date","Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85, na.rm=TRUE), Count=sum(TimeUnit, na.rm=TRUE))
    cr.daily <- ddply(cr.daily, c("Month", "Alias"), summarize, AvgHotHours=mean(InGE85Count, na.rm=TRUE))
    cr.daily$AvgHotHours <- with(cr.daily, round(AvgHotHours,2))
    cr.daily$monthdisp <- factor(cr.daily$Month, orderedMonths)
    cr.daily$School <- sub("(.*?) - .*", "\\1", cr.daily$Alias)
    cr.daily$Hot <- ifelse(cr.daily$AvgHotHours>4, "Red", ifelse(cr.daily$AvgHotHours>2, "Orange", "Yellow"))
    
    ann <- o.daily
    ann$x <- 1
    ann$y <- ann$AvgHotHours + .15
    ann$lab <- paste0("Outdoor Average: ", ann$AvgHotHours, " hours")
    ann <- ann[,c("x", "y", "lab", "Month")]
    ann$monthdisp <- factor(ann$Month, orderedMonths)
    
    plot.title <- ""
    plot.subtitle <- "School Hours Above 85°F UTCI, Days Reaching 85°F"
    
    b2 <- ggplot() + 
      geom_bar(data=cr.daily[cr.daily$School==school,], aes(x=Alias, y=AvgHotHours, fill=Hot), alpha=0.5, stat="identity") + 
      geom_hline(data=o.daily, aes(yintercept=AvgHotHours), color="dimgrey", size=0.3) +
      geom_text(data=ann, aes(x, y, label=lab, group=1), color="dimgrey", size=2.5, hjust="left") + 
      facet_grid(~monthdisp, drop=FALSE) +
      scale_fill_manual(name="", values=colorsHeat) +
      scale_y_continuous(breaks=seq(0,6,1), limits=c(0,6.15)) +
      ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
      theme_fivethirtyeight() +
      theme(axis.text.x = element_blank(), legend.position="none",  text=element_text(size=9))
    
    ## Hottest day
    croMax <- merge(cro, hottest, by=c("Date","Month"))
    
    o.daily <- ddply(croMax, c("Date","Alias", "Month"), summarize, OutGE85Count=sum(OutGE85, na.rm=TRUE), Count=sum(OutTimeUnit, na.rm=TRUE))
    o.daily <- ddply(o.daily, "Month", summarize, AvgHotHours=mean(OutGE85Count, na.rm=TRUE))
    o.daily$AvgHotHours <- with(o.daily, round(AvgHotHours,2))
    o.daily$monthdisp <- factor(o.daily$Month, orderedMonths)
    
    cr.daily <- ddply(croMax, c("Date","Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85, na.rm=TRUE), Count=sum(TimeUnit, na.rm=TRUE))
    cr.daily <- ddply(cr.daily, c("Month", "Alias"), summarize, AvgHotHours=mean(InGE85Count, na.rm=TRUE))
    cr.daily$AvgHotHours <- with(cr.daily, round(AvgHotHours,2))
    cr.daily$monthdisp <- factor(cr.daily$Month, orderedMonths)
    cr.daily$School <- sub("(.*?) - .*", "\\1", cr.daily$Alias)
    cr.daily$Room <- sub(".*- ", "", cr.daily$Alias)
    cr.daily$Hot <- ifelse(cr.daily$AvgHotHours>4, "Red", ifelse(cr.daily$AvgHotHours>2, "Orange", "Yellow"))
    
    ann <- o.daily
    ann$x <- 1
    ann$y <- ann$AvgHotHours + .15
    ann$lab <- paste0("Outdoor Average: ", ann$AvgHotHours, " hours")
    ann <- ann[,c("x", "y", "lab", "Month")]
    ann$monthdisp <- factor(ann$Month, orderedMonths)
    
    plot.subtitle <- "School Hours Above 85°F UTCI, Hottest Day"
    
    b3 <- ggplot() + 
      geom_bar(data=cr.daily[cr.daily$School==school,], aes(x=Room, y=AvgHotHours, fill=Hot), alpha=0.5, stat="identity") + 
      geom_hline(data=o.daily, aes(yintercept=AvgHotHours), color="dimgrey", size=0.3) +
      geom_text(data=ann, aes(x, y, label=lab, group=1), color="dimgrey", size=2.5, hjust="left") + 
      facet_grid(~monthdisp, drop=FALSE) +
      scale_fill_manual(name="", values=colorsHeat) +
      scale_y_continuous(breaks=seq(0,6,1), limits=c(0,6.15)) +
      ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
      theme_fivethirtyeight() +
      #theme(legend.position="none", text=element_text(size=9), axis.text.x=element_blank())
      theme(legend.position="none", text=element_text(size=9), axis.text.x=element_text(angle=90, hjust=1))
    
    grid.arrange(b1,b2,b3, layout_matrix=rbind(c(1), c(2), c(3)))
    
    
    ### Line
    ## All days
    cr.hourly <- ddply(cr, c("Time", "Alias", "Month", "School"), summarize, AvgUTCI=mean(UTCI, na.rm=TRUE))
    o.hourly <- ddply(o, c("Time", "Month"), summarize, AvgTemp=mean(OutdoorTemp, na.rm=TRUE), AvgUTCI=mean(OutdoorUTCI, na.rm=TRUE))
    
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
      geom_hline(yintercept=minF, linetype="dotted", color="black", size=0.5) +
      geom_line(data=cr.hourly[cr.hourly$School==school,], aes(x=Time, y=value, group=Alias, color=Alias), size=0.3, alpha=0.5, show.legend = FALSE) +
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
    oHot <- o[o$OutdoorUTCI>=minF,]
    crHot <- merge(oHot, cr, by=c("Date", "Time", "Month"))  
    cr.hourly <- ddply(crHot, c("Time", "Alias", "Month", "School"), summarize, AvgUTCI=mean(UTCI, na.rm=TRUE))
    o.hourly <- ddply(oHot, c("Time", "Month"), summarize, AvgTemp=mean(OutdoorTemp, na.rm=TRUE), AvgUTCI=mean(OutdoorUTCI, na.rm=TRUE))
    
    o.hourly$Time <- as.POSIXct(o.hourly$Time, format="%H:%M", tz="UTC")
    cr.hourly$Time <- as.POSIXct(cr.hourly$Time, format="%H:%M", tz="UTC")
    
    count <- unique(o.hourly[c("Month", "Time")])
    count <- count[with(count, order(Month, Time)), ]
    #count$diff <- c(NA, difftime(count$Time[1:(length(count$Time)-1)] , count$Time[2:length(count$Time)]))
    counts <- ddply(count, c("Month"), summarize, freq=length(Time))
    counts <- counts[counts$freq>=8, ]
   
    o.hourly <- merge(o.hourly, counts, by=c("Month")) 
    o.hourly$freq <- NULL
    cr.hourly <- merge(cr.hourly, counts, by=c("Month")) 
    cr.hourly$freq <- NULL
    
    
    cr.hourly <- melt(cr.hourly, id.vars=c("Time", "Month", "Alias", "School"))
    o.hourly <- melt(o.hourly, id.vars=c("Time", "Month"))
    o.hourly$monthdisp <- factor(o.hourly$Month, orderedMonths)
    cr.hourly$monthdisp <- factor(cr.hourly$Month, orderedMonths)
    
    plot.title <- ""
    plot.subtitle <- "Average School Day UTCI, Observations Above 85°F UTCI"
    
    l2 <- ggplot() + 
      geom_hline(yintercept=minF, linetype="dotted", color="black", size=0.5) +
      geom_line(data=cr.hourly[cr.hourly$School==school,], aes(x=Time, y=value, color=Alias), size=0.5, alpha=0.5) +
      geom_line(data=o.hourly, aes(x=Time, y=value, linetype=variable, size=variable), color="dimgrey") +
      facet_grid(~monthdisp, drop=FALSE) +
      scale_linetype_manual(name = "", values=c("longdash","solid")) +
      scale_y_continuous(breaks=seq(75,105,5), limits=c(75,105)) +
      scale_color_manual(values=palette(ann.size)) +
      scale_size_manual(name="", values=c(0.5,0.8)) +
      scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
      ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
      theme_fivethirtyeight() + theme(legend.position="none",  text=element_text(size=9))
    
    ## Hottest day
    cr.hourly <- merge(cr[,c("Time", "Date", "Month", "Alias", "UTCI", "School")], hottest, by=c("Date","Month"))
    o.hourly <- merge(o, hottest, by=c("Date","Month"))
    
    o.hourly <- melt(o.hourly, id.vars=c("Time", "Month", "Date"))
    o.hourly$Time <- as.POSIXct(o.hourly$Time, format="%H:%M", tz="UTC")
    cr.hourly$Time <- as.POSIXct(cr.hourly$Time, format="%H:%M", tz="UTC")
    o.hourly$monthdisp <- factor(o.hourly$Month, orderedMonths)
    cr.hourly$monthdisp <- factor(cr.hourly$Month, orderedMonths)
    
    plot.title <- ""
    plot.subtitle <- "Average School Day UTCI, Hottest Day"
    
    l3 <- ggplot() + 
      geom_hline(yintercept=minF, linetype="dotted", color="black", size=0.5) +
      geom_line(data=cr.hourly[cr.hourly$School==school,], aes(x=Time, y=UTCI, group=Alias, color=Alias), size=0.5, alpha=0.5, show.legend = FALSE) +
      geom_line(data=o.hourly, aes(x=Time, y=value, linetype=variable, size=variable), color="dimgrey") +
      facet_grid(~monthdisp, drop=FALSE) +
      scale_y_continuous(breaks=seq(70,100,5), limits=c(70,100)) +
      scale_color_manual(values=palette(ann.size)) +
      scale_size_manual(name="", values=c(0.5,0.8), guide=FALSE) +
      scale_linetype_manual(name = "", labels=c("Outdoor Temperature", "Outdoor UTCI"), values=c("longdash","solid")) +
      scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
      ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
      theme_fivethirtyeight() + theme(text=element_text(size=9),
                                      legend.title=element_blank(),
                                      legend.position=c(0.08,0.15), 
                                      legend.background=element_rect(color="grey", fill="#F0F0F0", size=0.4, linetype="solid"), legend.box="horizontal") 
    
    grid.arrange(l1,l2,l3, layout_matrix=rbind(c(1), c(2), c(3)))
    
  }    
  