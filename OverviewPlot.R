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
      schoolSave <- "kaimiloa"
    } else if (school=="Campbell") {
      schoolTitle <- "James Campbell High School"
      ann.size <- 30
      schoolSave <- "campbell"
    } else if (school=="Ilima") {
      schoolTitle <- "Ilima Intermediate School"
      ann.size <- 12
      schoolSave <- "ilima"
    }
    
    ### Input data 
    o <- read.csv(file="~/dropbox/rh1/hidoe/output-csv/outdoor-master.csv", sep=",") #outdoor 
    o$Date <- as.Date(o$Date, format="%Y-%m-%d")
    o$Time <- format(strptime(o$Time, format="%H:%M"), format="%H:%M")
    o$Month <- factor(format(o$Date, "%B"), orderedMonths)
    o$Alias <- NULL #choose what weather station you want from Alias, but for now they're all the same so drop Alias
    o <- o[o$SchoolDate==1 & o$SchoolHour==1,]
    
    cr <-read.csv(file="~/dropbox/rh1/hidoe/output-csv/classroom-master.csv", sep=",") #classroom
    cr$Date <- as.Date(cr$Date, format="%Y-%m-%d")
    cr$Time <- format(strptime(cr$Time, format="%H:%M"), format="%H:%M")
    cr$Month <- format(cr$Date, "%B") 
    cr$Month <- factor(format(cr$Date, "%B"), orderedMonths)
    cr <- cr[cr$SchoolDate==1 & cr$SchoolHour==1,]
    
    arch <- read.csv(file="~/dropbox/rh1/hidoe/input-csv/classroom-features.csv", sep=",") #classroom architectural data
    archKeepVars <- c("RoomID", "Alias", "School", "AC")
    cr <- merge(cr, arch[,c(archKeepVars)], by="RoomID", all.x=TRUE, all.y=TRUE)
    cr <- cr[cr$AC==0,]
    cr$AC <- NULL
    cr$SensorAlias <- as.character(cr$SensorAlias)
    cr <- cr[substr(cr$SensorAlias,nchar(cr$SensorAlias)-1,nchar(cr$SensorAlias))!="HD",] #remove HD (because of overlapping)
    cr$SensorAlias <- NULL
    
    
    hottest <- do.call("rbind", by(o, o$Month, function(x) x[which.max(x$UTCI_F),]))
    hottest <- hottest[,c("Date", "Month")]
    

    ### Plots
    ## Profile
    o.daily <- ddply(o,c("Date"), summarise, AvgOutdoorTemp=mean(Temp_F, na.rm=TRUE), AvgOutdoorUTCI=mean(UTCI_F, na.rm=TRUE)) #aggregated by date 
    cr.daily <- ddply(cr, c("Date", "Alias"), summarise, AvgRoomTemp=mean(Temp_F, na.rm=TRUE), MaxRoomTemp=max(Temp_F,na.rm=TRUE), AvgRoomUTCI=mean(UTCI_F, na.rm=TRUE), MaxRoomUTCI=max(UTCI_F, na.rm=TRUE))
    cr.daily$School <- sub("(.*?) - .*", "\\1", cr.daily$Alias)
    s.daily <- ddply(cr.daily, c("Date", "School"), summarise, AvgSchoolTemp=mean(AvgRoomTemp, na.rm=TRUE), MaxSchoolTemp=mean(MaxRoomTemp,na.rm=TRUE), AvgSchoolUTCI=mean(AvgRoomUTCI, na.rm=TRUE), MaxSchoolUTCI=mean(MaxRoomUTCI, na.rm=TRUE)) #average of avg and max by school
    
    o.daily.melt <- melt(o.daily, id.vars=c("Date")) 
    o.daily.melt$Alias <- "Outdoor"
    o.daily.melt$School <- "Outdoor"
    s.daily.melt <- melt(s.daily, id.vars=c("Date", "School")) 
    s.daily.melt$Alias <- s.daily.melt$School
    cr.daily.melt <- melt(cr.daily, id.vars=c("Date", "Alias", "School")) 
    daily.melt <- rbind(o.daily.melt, cr.daily.melt, s.daily.melt)
    daily.melt$measure <- as.factor(ifelse(substr(as.character(daily.melt$variable), nchar(as.character(daily.melt$variable))-3, nchar(as.character(daily.melt$variable)))=="Temp", "Temp", "UTCI"))
    daily.melt$level <- as.factor(ifelse(substr(as.character(daily.melt$variable), 1, 3)=="Avg", "Average", "Maximum"))
    
    p <- ggplot() +
      geom_hline(yintercept=minF, linetype="dotted", color="black", size=0.5) + 
      geom_point(data=daily.melt[daily.melt$School==school & daily.melt$variable %in% c("AvgRoomUTCI", "MaxRoomUTCI"),], aes(x=Date, y=value, color=variable, shape=level), alpha=0.2) +
      geom_smooth(data=daily.melt[daily.melt$Alias %in% c(school, "Outdoor"),], aes(x=Date, y=value, linetype=variable, size=variable, color=variable), fill=NA, span=0.4) +
      scale_color_manual(name = "", values=c("dimgrey", "dimgrey", "#30A2DA", "#30A2DA", "#30A2DA", "#FC4F30", "#FC4F30", "#FC4F30"), guide=FALSE) +   
      scale_linetype_manual(name="", labels=c("Avg Outdoor UTCI", "Avg Outdoor Temp", "Avg School Temp", "Max School Temp", "Avg School UTCI", "Max School UTCI"), values=c("dashed","solid", "dashed", "dashed", "solid", "solid")) +
      scale_size_manual(name="", values=c(0.5, 0.8, 0.5, 0.5, 0.8, 0.8), guide=FALSE) +
      scale_shape_manual(name="", labels=c("Classroom (Average UTCI)", "Classroom (Maxmimum UTCI)"), values=c(16, 16)) +
      scale_y_continuous(breaks=seq(70,105,5), limits = c(70,105)) + 
      scale_x_date(date_breaks="1 month", date_labels="%b '%y") + 
      ggtitle(paste(" Daily Universal Thermal Climate Index (UTCI) Profile: ", schoolTitle)) +
      theme_fivethirtyeight() +
      theme(text=element_text(size=9), legend.title=element_blank(), legend.position=c(0.13,0.08), legend.background=element_rect(color="grey", fill="#F0F0F0", size=0.4, linetype="solid"), legend.box="horizontal") +
      guides(linetype = guide_legend(override.aes = list(size=0.5, linetype=c(1,2,2,2,1,1), color=c("dimgrey", "dimgrey", "#30A2DA", "#FC4F30", "#30A2DA", "#FC4F30"))),
             shape = guide_legend(ncol=1, override.aes = list(alpha=c(0.6,0.6), color=c("#30A2DA", "#FC4F30"))))
    
    print(p)
    
    #summary statistics for datacard
    daily.stat <- ddply(daily.melt, c("variable", "Alias"), summarise,
          mean = mean(value), sd = sd(value),
          min = min(value), max = max(value))
    
    #save as csv for d3
    d3.daily <- daily.melt[order(daily.melt$Alias, daily.melt$variable, daily.melt$Date),]
    d3.daily$ID <- 1:nrow(d3.daily)
    
    models <- ldply(dlply(d3.daily, c("Alias", "variable"), function(x) {loess(value ~ as.numeric(Date), data=x, model=TRUE, span=0.4)$fitted}), data.frame)
    d3.daily$smooth <- models$X..i..
    

    ### Bar plot
    ## All days
    cro <- merge(cr, o, by=c("Time", "Month", "Date"), all.x=TRUE, suffixes=c("_CR", "_O"))
    cro$TimeUnit <- ifelse(cro$Time=="08:00",0,.25) #start counting hours after 8 AM
    cro$OutTimeUnit <- ifelse((is.na(cro$UTCI_F_O) | cro$Time=="08:00"),0,.25) 
    cro$InGE85 <- ifelse((cro$UTCI_F_CR>=minF)==TRUE & cro$Time!="08:00",.25,0)
    cro$OutGE85 <-ifelse((cro$UTCI_F_O>=minF)==TRUE & cro$Time!="08:00",.25,0)
    
    o.daily <- ddply(cro, c("Date","Alias", "Month"), summarize, OutGE85Count=sum(OutGE85, na.rm=TRUE), Count=sum(OutTimeUnit, na.rm=TRUE))
    o.daily <- ddply(o.daily, "Month", summarize, AvgHotHours=mean(OutGE85Count, na.rm=TRUE))
    o.daily$AvgHotHours <- with(o.daily, round(AvgHotHours,2))
    
    cr.daily <- ddply(cro, c("Date","Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85, na.rm=TRUE), Count=sum(TimeUnit, na.rm=TRUE))
    cr.daily <- ddply(cr.daily, c("Month", "Alias"), summarize, AvgHotHours=mean(InGE85Count, na.rm=TRUE))
    cr.daily$AvgHotHours <- with(cr.daily, round(AvgHotHours,2))
    
    o.daily$School <- "Outdoor"
    o.daily$Alias <- "Outdoor"
    cr.daily$School <- sub("(.*?) - .*", "\\1", cr.daily$Alias)
    
    d3.bar <- rbind(o.daily, cr.daily)
    
    cr.daily$Room <- sub(".*- ", "", cr.daily$Alias)
    cr.daily$Room <- ifelse(cr.daily$Alias=="Campbell - N (Robotics)", "N", cr.daily$Room)
    cr.daily$Hot <- ifelse(cr.daily$AvgHotHours>4, "Red", ifelse(cr.daily$AvgHotHours>2, "Orange", "Yellow"))
    
    ann <- o.daily
    ann$x <- 1
    ann$y <- ann$AvgHotHours + .15
    ann$lab <- paste0("Outdoor Average: ", ann$AvgHotHours, " hr")
    ann <- ann[,c("x", "y", "lab", "Month")]
    
    plot.title <- schoolTitle 
    plot.subtitle <- "School Hours Above 85°F UTCI, All Days"
    
    b1 <- ggplot() + 
      geom_bar(data=cr.daily[cr.daily$School==school,], aes(x=Room, y=AvgHotHours, fill=Hot), alpha=0.5, stat="identity") + 
      geom_hline(data=o.daily, aes(yintercept=AvgHotHours), color="dimgrey", size=0.3) +
      geom_text(data=ann, aes(x, y, label=lab, group=1), color="dimgrey", size=2.5, hjust="left") + 
      facet_grid(~Month, drop=FALSE) +
      scale_fill_manual(name="", values=colorsHeat) +
      scale_y_continuous(breaks=seq(0,6,1), limits=c(0,6.15)) +
      ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
      theme_fivethirtyeight() +
      theme(legend.position="none", text=element_text(size=9), axis.text.x=element_text(angle=90, hjust=1))
    
    ## Reaching 85
    
    hot <- ddply(cro, c("Date"), summarize, Hot=sum(OutGE85, na.rm=TRUE)) 
    hot <- hot[hot$Hot>0,]
    croHot <- merge(cro, hot, by="Date")
    
    o.daily <- ddply(croHot, c("Date","Alias", "Month"), summarize, OutGE85Count=sum(OutGE85, na.rm=TRUE), Count=sum(OutTimeUnit, na.rm=TRUE))
    o.daily <- ddply(o.daily, "Month", summarize, AvgHotHours=mean(OutGE85Count, na.rm=TRUE))
    o.daily$AvgHotHours <- with(o.daily, round(AvgHotHours,2))
    
    cr.daily <- ddply(croHot, c("Date","Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85, na.rm=TRUE), Count=sum(TimeUnit, na.rm=TRUE))
    cr.daily <- ddply(cr.daily, c("Month", "Alias"), summarize, AvgHotHours=mean(InGE85Count, na.rm=TRUE))
    cr.daily$AvgHotHours <- with(cr.daily, round(AvgHotHours,2))    
    
    o.daily$School <- "Outdoor"
    o.daily$Alias <- "Outdoor"
    cr.daily$School <- sub("(.*?) - .*", "\\1", cr.daily$Alias)
    
    d3.bar <- merge(d3.bar, rbind(o.daily, cr.daily)[, c("Month", "AvgHotHours", "School", "Alias")], by=c("Month", "School", "Alias"), suffixes=c("", "Hot"), all.x=TRUE)
    
    cr.daily$Room <- sub(".*- ", "", cr.daily$Alias)
    cr.daily$Room <- ifelse(cr.daily$Alias=="Campbell - N (Robotics)", "N", cr.daily$Room)
    cr.daily$Hot <- ifelse(cr.daily$AvgHotHours>4, "Red", ifelse(cr.daily$AvgHotHours>2, "Orange", "Yellow"))
    
    ann <- o.daily
    ann$x <- 1
    ann$y <- ann$AvgHotHours + .15
    ann$lab <- paste0("Outdoor Average: ", ann$AvgHotHours, " hr")
    ann <- ann[,c("x", "y", "lab", "Month")]
    ann$monthdisp <- factor(ann$Month, orderedMonths)
    
    plot.title <- ""
    plot.subtitle <- "School Hours Above 85°F UTCI, Days Reaching 85°F"
    
    b2 <- ggplot() + 
      geom_bar(data=cr.daily[cr.daily$School==school,], aes(x=Room, y=AvgHotHours, fill=Hot), alpha=0.5, stat="identity") + 
      geom_hline(data=o.daily, aes(yintercept=AvgHotHours), color="dimgrey", size=0.3) +
      geom_text(data=ann, aes(x, y, label=lab, group=1), color="dimgrey", size=2.5, hjust="left") + 
      facet_grid(~Month, drop=FALSE) +
      scale_fill_manual(name="", values=colorsHeat) +
      scale_y_continuous(breaks=seq(0,6,1), limits=c(0,6.15)) +
      ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
      theme_fivethirtyeight() +
      theme(legend.position="none", text=element_text(size=9), axis.text.x=element_text(angle=90, hjust=1))
    
    ## Hottest day
    croMax <- merge(cro, hottest, by=c("Date","Month"))
    
    o.daily <- ddply(croMax, c("Date","Alias", "Month"), summarize, OutGE85Count=sum(OutGE85, na.rm=TRUE), Count=sum(OutTimeUnit, na.rm=TRUE))
    o.daily <- ddply(o.daily, "Month", summarize, AvgHotHours=mean(OutGE85Count, na.rm=TRUE))
    o.daily$AvgHotHours <- with(o.daily, round(AvgHotHours,2))
    cr.daily <- ddply(croMax, c("Date","Alias", "Month", "School"), summarize, InGE85Count=sum(InGE85, na.rm=TRUE), Count=sum(TimeUnit, na.rm=TRUE))
    cr.daily <- ddply(cr.daily, c("Month", "Alias"), summarize, AvgHotHours=mean(InGE85Count, na.rm=TRUE))
    cr.daily$AvgHotHours <- with(cr.daily, round(AvgHotHours,2))                                
    
    o.daily$School <- "Outdoor"
    o.daily$Alias <- "Outdoor"
    cr.daily$School <- sub("(.*?) - .*", "\\1", cr.daily$Alias)
    
    d3.bar <- merge(d3.bar, rbind(o.daily, cr.daily)[, c("Month", "AvgHotHours", "School", "Alias")], by=c("Month", "School", "Alias"), suffixes=c("", "Hottest"), all.x=TRUE) 
    
    cr.daily$Room <- sub(".*- ", "", cr.daily$Alias)
    cr.daily$Room <- ifelse(cr.daily$Alias=="Campbell - N (Robotics)", "N", cr.daily$Room)
    cr.daily$Hot <- ifelse(cr.daily$AvgHotHours>4, "Red", ifelse(cr.daily$AvgHotHours>2, "Orange", "Yellow"))
    
    ann <- o.daily
    ann$x <- 1
    ann$y <- ann$AvgHotHours + .15
    ann$lab <- paste0("Outdoor Average: ", ann$AvgHotHours, " hr")
    ann <- ann[,c("x", "y", "lab", "Month")]
    ann$monthdisp <- factor(ann$Month, orderedMonths)
    
    plot.subtitle <- "School Hours Above 85°F UTCI, Hottest Day"
    
    b3 <- ggplot() + 
      geom_bar(data=cr.daily[cr.daily$School==school,], aes(x=Room, y=AvgHotHours, fill=Hot), alpha=0.5, stat="identity") + 
      geom_hline(data=o.daily, aes(yintercept=AvgHotHours), color="dimgrey", size=0.3) +
      geom_text(data=ann, aes(x, y, label=lab, group=1), color="dimgrey", size=2.5, hjust="left") + 
      facet_grid(~Month, drop=FALSE) +
      scale_fill_manual(name="", values=colorsHeat) +
      scale_y_continuous(breaks=seq(0,6,1), limits=c(0,6.15)) +
      ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
      theme_fivethirtyeight() +
      theme(legend.position="none", text=element_text(size=9), axis.text.x=element_text(angle=90, hjust=1))
    
    b <- arrangeGrob(b1,b2,b3, layout_matrix=rbind(c(1), c(2), c(3)))
    
    ### Line
    ## All days
    cr.hourly <- ddply(cr, c("Time", "Alias", "Month", "School"), summarize, AvgUTCI=mean(UTCI_F, na.rm=TRUE))
    o.hourly <- ddply(o, c("Time", "Month"), summarize, AvgTemp=mean(Temp_F, na.rm=TRUE), AvgUTCI=mean(UTCI_F, na.rm=TRUE))
    
    cr.hourly <- melt(cr.hourly, id.vars=c("Time", "Month", "Alias", "School"))
    o.hourly <- melt(o.hourly, id.vars=c("Time", "Month"))
    
    o.hourly$Alias <- "Outdoor"
    o.hourly$School <- "Outdoor"
    
    o.hourly$variable <- ifelse(o.hourly$variable=="AvgTemp", "AvgOutdoorTemp", "AvgOutdoorUTCI")
    d3.line <- rbind(o.hourly, cr.hourly)
    
    o.hourly$Time <- as.POSIXct(o.hourly$Time, format="%H:%M", tz="UTC")
    cr.hourly$Time <- as.POSIXct(cr.hourly$Time, format="%H:%M", tz="UTC")
    
    lims <- as.POSIXct(strptime(c("08:00","14:00"), format = "%H:%M"), tz="UTC")    
    plot.title <- schoolTitle
    plot.subtitle <- "Average School Day UTCI, All Observations"
    
    l1 <- ggplot() + 
      geom_hline(yintercept=minF, linetype="dotted", color="black", size=0.5) +
      geom_line(data=cr.hourly[cr.hourly$School==school,], aes(x=Time, y=value, group=Alias, color=Alias), size=0.3, alpha=0.5, show.legend = FALSE) +
      geom_line(data=o.hourly, aes(x=Time, y=value, linetype=variable, size=variable), color="dimgrey") +
      facet_grid(~Month, drop=FALSE) +
      scale_y_continuous(breaks=seq(70,100,5), limits=c(70,100)) +
      scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
      scale_color_manual(values=palette(ann.size)) +
      scale_size_manual(name="", values=c(0.5,0.8)) +
      scale_linetype_manual(name = "", values=c("longdash","solid")) +
      ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
      theme_fivethirtyeight() + theme(legend.position="none", text=element_text(size=9))

    
    ## Obs over 85
    oHot <- o[o$UTCI_F>=minF,]
    crHot <- merge(oHot[,c("Date", "Time", "Month")], cr, by=c("Date", "Time", "Month"))  
    cr.hourly <- ddply(crHot, c("Time", "Alias", "Month", "School"), summarize, AvgUTCI=mean(UTCI_F, na.rm=TRUE))
    o.hourly <- ddply(oHot, c("Time", "Month"), summarize, AvgTemp=mean(Temp_F, na.rm=TRUE), AvgUTCI=mean(UTCI_F, na.rm=TRUE))
    
    o.hourly$Time <- as.POSIXct(o.hourly$Time, format="%H:%M", tz="UTC")
    cr.hourly$Time <- as.POSIXct(cr.hourly$Time, format="%H:%M", tz="UTC")
    
    count <- unique(o.hourly[c("Month", "Time")])
    count <- count[with(count, order(Month, Time)), ]
    #count$diff <- c(NA, difftime(count$Time[1:(length(count$Time)-1)] , count$Time[2:length(count$Time)]))
    counts <- ddply(count, c("Month"), summarize, freq=length(Time))
    counts <- counts[counts$freq>=8, ]
    
    o.hourly <- merge(o.hourly, counts, by=c("Month")) 
    cr.hourly <- merge(cr.hourly, counts, by=c("Month")) 
    o.hourly$freq <- NULL
    cr.hourly$freq <- NULL
    
    cr.hourly <- melt(cr.hourly, id.vars=c("Time", "Month", "Alias", "School"))
    o.hourly <- melt(o.hourly, id.vars=c("Time", "Month"))
    
    o.hourly$Time <- format(strptime(o.hourly$Time, format="%Y-%m-%d %H:%M"), format="%H:%M")
    o.hourly$variable <- ifelse(o.hourly$variable=="AvgTemp", "AvgOutdoorTemp", "AvgOutdoorUTCI")
    o.hourly$Alias <- "Outdoor"
    o.hourly$School <- "Outdoor"
    cr.hourly$Time <- format(strptime(cr.hourly$Time, format="%Y-%m-%d %H:%M"), format="%H:%M")
    
    d3.line <- merge(d3.line, rbind(o.hourly, cr.hourly), by=c("Month", "School", "Alias", "variable", "Time"), suffixes=c("", "Hot"), all.x=TRUE)
    
    o.hourly$Time <- as.POSIXct(o.hourly$Time, format="%H:%M", tz="UTC")
    cr.hourly$Time <- as.POSIXct(cr.hourly$Time, format="%H:%M", tz="UTC")    
    
    plot.title <- ""
    plot.subtitle <- "Average School Day UTCI, Observations Above 85°F UTCI"
    
    l2 <- ggplot() + 
      geom_hline(yintercept=minF, linetype="dotted", color="black", size=0.5) +
      geom_line(data=cr.hourly[cr.hourly$School==school,], aes(x=Time, y=value, group=Alias, color=Alias), size=0.3, alpha=0.5, show.legend = FALSE) +
      geom_line(data=o.hourly, aes(x=Time, y=value, linetype=variable, size=variable), color="dimgrey") +
      facet_grid(~Month, drop=FALSE) +
      scale_linetype_manual(name = "", values=c("longdash","solid")) +
      scale_y_continuous(breaks=seq(75,105,5), limits=c(75,105)) +
      scale_color_manual(values=palette(ann.size)) +
      scale_size_manual(name="", values=c(0.5,0.8)) +
      scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
      ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
      theme_fivethirtyeight() + theme(legend.position="none",  text=element_text(size=9))
    
    
    ## Hottest day
    cr.hourly <- merge(cr[,c("Time", "Date", "Month", "Alias", "UTCI_F", "School")], hottest, by=c("Date","Month"))
    o.hourly <- merge(o, hottest, by=c("Date","Month"))
    
    o.hourly <- melt(o.hourly, id.vars=c("Time", "Month", "Date"))
    cr.hourly <- melt(cr.hourly, id.vars=c("Time", "Month", "Alias", "School", "Date"))
    
    o.hourly$variable <- ifelse(o.hourly$variable=="Temp_F", "AvgOutdoorTemp", "AvgOutdoorUTCI")
    cr.hourly$variable <- "AvgUTCI"
    
    o.hourly$Alias <- "Outdoor"
    o.hourly$School <- "Outdoor"
    
    d3.line <- merge(d3.line, rbind(o.hourly, cr.hourly), by=c("Month", "School", "Alias", "variable", "Time"), suffixes=c("", "Hottest"), all.x=TRUE)
    d3.line$HottestDay <- d3.line$Date
    d3.line$Date <- NULL
    d3.line <- d3.line[order(d3.line$Alias, d3.line$variable, d3.line$Month, d3.line$Time),]
    d3.line$ID <- 1:nrow(d3.line)
    
    o.hourly$Time <- as.POSIXct(o.hourly$Time, format="%H:%M", tz="UTC")
    cr.hourly$Time <- as.POSIXct(cr.hourly$Time, format="%H:%M", tz="UTC")
    
    plot.title <- ""
    plot.subtitle <- "Average School Day UTCI, Hottest Day"
    
    
    l3 <- ggplot() + 
      geom_hline(yintercept=minF, linetype="dotted", color="black", size=0.5) +
      geom_line(data=cr.hourly[cr.hourly$School==school,], aes(x=Time, y=value, group=Alias, color=Alias), size=0.3, alpha=0.5, show.legend = FALSE) +
      geom_line(data=o.hourly, aes(x=Time, y=value, linetype=variable, size=variable), color="dimgrey") +
      facet_grid(~Month, drop=FALSE) +
      scale_y_continuous(breaks=seq(70,100,5), limits=c(70,100)) +
      scale_color_manual(values=palette(ann.size)) +
      scale_size_manual(name="", values=c(0.5,0.8), guide=FALSE) +
      scale_linetype_manual(name = "", labels=c("Outdoor Temperature", "Outdoor UTCI"), values=c("longdash","solid")) +
      scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
      ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
      theme_fivethirtyeight() + theme(text=element_text(size=9),
                                      legend.title=element_blank(),
                                      legend.position=c(0.06,0.15), 
                                      legend.background=element_rect(color="grey", fill="#F0F0F0", size=0.4, linetype="solid"), legend.box="horizontal") 
    
    l <- arrangeGrob(l1,l2,l3, layout_matrix=rbind(c(1), c(2), c(3)))
    
    ggsave(filename=paste0("~/dropbox/rh1/hidoe/plots/avg-daily-",schoolSave,".pdf"), p, width=25, height=16, units="in")
    ggsave(filename=paste0("~/dropbox/rh1/hidoe/plots/avg-hourly-",schoolSave,".pdf"), l, width=25, height=16, units="in")
    ggsave(filename=paste0("~/dropbox/rh1/hidoe/plots/hours-above-85-",schoolSave,".pdf"), b, width=25, height=16, units="in")
    
    
    #write.csv(daily.stat, file="~/dropbox/rh1/hidoe/output-csv/daily-stats.csv", row.names = FALSE, na="")
    #write.csv(d3.line , file="~/dropbox/rh1/hidoe/output-csv/d3/d3-line.csv", row.names=FALSE, na="")
    #write.csv(d3.daily, file="~/dropbox/rh1/hidoe/output-csv/d3/d3-daily.csv", row.names=FALSE, na="")
    #write.csv(d3.bar, file="~/dropbox/rh1/hidoe/output-csv/d3/d3-bar.csv", row.names = FALSE, na="")
    
    
    
    
    ### Check overlapping summer months
    #ot$Year <- as.factor(year(ot$Date))
    #ot$Day <- as.character(format(ot$Date, "%m-%d"))
    #otoverlap <- ot[ot$Month %in% c("August", "September", "October"),]
    #ggplot(otoverlap, aes(x=Date, y=OutdoorTemp)) + geom_point(color="blue", alpha=0.5) + geom_smooth(size=1, color="red") + facet_grid(~Year) + ggtitle("Average Temperature of Overlapping Months (2013-2014)") + theme_fivethirtyeight()
    
    #year(otoverlap$Date) <- 2013
    #ggplot() + geom_smooth(data=otoverlap, aes(x=Day, y=OutdoorTemp, color=Year, group=Year)) + scale_y_continuous(breaks=seq(70,100,5), limits=c(70,100)) + ggtitle("Average Temperature of Overlapping Months (2013-2014)") + theme_fivethirtyeight()   
    
  }    
  