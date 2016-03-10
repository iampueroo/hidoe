classroom-plot <- function(classroom, weatherstation, startdate, enddate) {
  library(plyr)
  library(reshape2)
  library(ggplot2)
  library(ggthemes) 
  library(scales)
  library(grid)
  library(gridExtra) 
  #library(gtable)
  #library(lubridate) 
  
  min_F <- 85
  colors <- c( "dimgrey", "#FC4F30", "#30A2DA")
  colorsHeat <- c("#E69720", "#FC4F30", "#E8E827") 
  stdt <- as.Date(startdate, format="%Y-%m-%d")
  endt <- as.Date(enddate, format="%Y-%m-%d")
  
  # read in data ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
  arch <- read.csv(file="~/dropbox/rh1/hidoe/input-csv/classroom-features.csv", sep=",") #classroom architectural data
  
  o <- read.csv(file="~/dropbox/rh1/hidoe/output-csv/outdoor-master.csv", sep=",") #outdoor 
  o$Date <- as.Date(o$Date, format="%Y-%m-%d")
  o$Time <- format(strptime(o$Time, format="%H:%M"), format="%H:%M")
  o$Month <- factor(format(o$Date, "%B"), c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May", "June", "July"))
  o$Alias <- NULL #choose what weather station you want from Alias, but for now they're all the same so drop Alias
  
  cr <-read.csv(file="~/dropbox/rh1/hidoe/output-csv/classroom-master.csv", sep=",") #classroom
  cr$Date <- as.Date(cr$Date, format="%Y-%m-%d")
  cr$Time <- format(strptime(cr$Time, format="%H:%M"), format="%H:%M")
  cr$Month <- factor(format(cr$Date, "%B"), c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May", "June", "July"))
  cr <- merge(cr, arch[,c("RoomID", "Alias")], by="RoomID", all.x=TRUE) #get room alias 
  cr <- cr[cr$Alias==classroom,] #restrict to classroom specified

  # raw data plots -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  cro <- merge(o[, c("Temp_F", "UTCI_F", "Date", "Time", "Month")], cr[, c("Temp_F", "UTCI_F", "Date", "Time", "Month", "Alias")], by=c("Time", "Date", "Month"), suffixes=c("_O", "_CR"), all.y=TRUE)
  cro.melt <- melt(cro, id.vars=c("Time", "Date", "Month", "Alias"))
  cro.melt$DateTime <- as.POSIXct(paste(cro.melt$Date, cro.melt$Time), format="%Y-%m-%d %H:%M")
  
  
  t <- ggplot() + 
      geom_hline(yintercept=85, color="black", linetype="dotted", size=0.5) +
      geom_line(data=cro.melt[cro.melt$variable %in% c("Temp_F_O", "Temp_F_CR"),], aes(x=DateTime, y=value, color=variable, alpha=variable), size=0.5) +
      scale_alpha_manual(name="", values=c(0.25, 1), guide=FALSE) +
      scale_x_datetime(breaks=date_breaks("1 month"), labels=date_format("%b %Y")) +
      scale_y_continuous(breaks=seq(60,100,5), limits=c(60,100)) +
      scale_color_manual(name="", labels=c("Outdoor Temperature", "Classroom Temperature"), values=c("dimgrey", "#CC6666")) +
      ylab("Temperature (°F)") + xlab("Time") + ggtitle(paste0(classroom, ": Temperature Profile")) +
      theme_fivethirtyeight() + theme(text=element_text(size=9),
                                     legend.title=element_blank(),
                                     legend.position=c(0.10,0.08), 
                                     legend.background=element_rect(color="grey", fill="#F0F0F0", size=0.4, linetype="solid"), legend.box="horizontal") 
  
  # restrict to school hour/dates ------------------------------------------------------------------------------------------------------------------------------------------------------------------
  cr <- cr[cr$SchoolDate==1 & cr$SchoolHour==1,]
  o <- o[o$SchoolDate==1 & o$SchoolHour==1,]
  
  o.daily <- ddply(o,c("Date"), summarise, AvgOutdoorTemp=mean(Temp_F, na.rm=TRUE), AvgOutdoorUTCI=mean(UTCI_F, na.rm=TRUE)) #aggregated by date 
  cr.daily <- ddply(cr, c("Date"), summarise, AvgRoomTemp=mean(Temp_F, na.rm=TRUE), MaxRoomTemp=max(Temp_F,na.rm=TRUE), AvgRoomUTCI=mean(UTCI_F, na.rm=TRUE), MaxRoomUTCI=max(UTCI_F, na.rm=TRUE)) #aggregated  by date
  
  o.daily <- melt(o.daily, id.vars=c("Date"))
  cr.daily <- melt(cr.daily, id.vars=c("Date"))
  cr.daily$level <- as.factor(ifelse(substr(as.character(cr.daily$variable), 1, 3)=="Avg", "Average", "Maximum"))
  o.daily$level <- "Outdoor"
  
  cro.daily <- rbind(o.daily, cr.daily)
  
  u <- ggplot() +
      geom_hline(yintercept=min_F, linetype="dotted", color="black", size=0.5) + 
      geom_point(data=cr.daily[cr.daily$variable %in% c("AvgRoomUTCI", "MaxRoomUTCI"),], aes(x=Date, y=value, color=variable, shape=level), alpha=0.5) +
      geom_smooth(data=cro.daily, aes(x=Date, y=value, linetype=variable, size=variable, color=variable), fill=NA) +
      scale_color_manual(name = "", values=c("dimgrey", "dimgrey", "#30A2DA", "#30A2DA", "#FC4F30", "#FC4F30"), guide=FALSE) +   
      scale_linetype_manual(name="", labels=c("Average Outdoor Temperature", "Average Outdoor UTCI", "Average Classroom Temperature", "Maximum Classroom Temperature", "Average Classroom UTCI", "Maximum Classroom UTCI"), values=c("dashed","solid", "dashed", "dashed", "solid", "solid")) +
      scale_size_manual(name="", values=c(0.5, 0.8, 0.5, 0.5, 0.8, 0.8), guide=FALSE) +
      scale_shape_manual(name="", labels=c("Classroom (Average UTCI)", "Classroom (Maxmimum UTCI)"), values=c(16, 16)) +
      scale_y_continuous(breaks=seq(70,105,5), limits = c(70,105)) + 
      scale_x_date(date_breaks="1 month", date_labels="%b %Y") + 
      ggtitle(paste(classroom, ": Daily Universal Thermal Climate Index (UTCI) Profile")) +
      theme_fivethirtyeight() +
      theme(text=element_text(size=9), legend.title=element_blank(), legend.position=c(0.2,0.08), legend.background=element_rect(color="grey", fill="#F0F0F0", size=0.4, linetype="solid"), legend.box="horizontal") +
      guides(linetype = guide_legend(override.aes = list(size=0.5, color=c("dimgrey", "dimgrey", "#30A2DA", "#FC4F30", "#30A2DA", "#FC4F30"), shape=NA)),
            shape = guide_legend(ncol=1, override.aes = list(alpha=c(0.6,0.6), color=c("#30A2DA", "#FC4F30"))))
  
   ### line plots ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
   
   ## All days
   cr.hourly <- ddply(cr, c("Time", "Month"), summarize, AvgUTCI=mean(UTCI_F, na.rm=TRUE))
   o.hourly <- ddply(o, c("Time", "Month"), summarize, AvgTemp=mean(Temp_F, na.rm=TRUE), AvgUTCI=mean(UTCI_F, na.rm=TRUE))
   
   cr.hourly <- melt(cr.hourly, id.vars=c("Time", "Month"))
   o.hourly <- melt(o.hourly, id.vars=c("Time", "Month"))
   o.hourly$Time <- as.POSIXct(o.hourly$Time, format="%H:%M", tz="UTC")
   cr.hourly$Time <- as.POSIXct(cr.hourly$Time, format="%H:%M", tz="UTC")
   
   lims <- as.POSIXct(strptime(c("08:00","14:00"), format = "%H:%M"), tz="UTC")    
   plot.title <- paste0(classroom, ": ", format(stdt, "%b %Y"), " - ", format(endt, "%b %Y"))
   plot.subtitle <- "Average School Day UTCI, All Observations"
   
   l1 <- ggplot() + 
     geom_hline(yintercept=min_F, linetype="dotted", color="black", size=0.5) +
     geom_line(data=cr.hourly, aes(x=Time, y=value), size=0.8, alpha=0.5, group=1, color="forestgreen") +
     geom_line(data=o.hourly, aes(x=Time, y=value, linetype=variable, size=variable, group=variable), color="dimgrey") +
     facet_grid(~Month) +
     scale_y_continuous(breaks=seq(70,100,5), limits=c(70,100)) +
     scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
     scale_size_manual(name="", values=c(0.5,0.8)) +
     scale_linetype_manual(name = "", values=c("longdash","solid")) +
     ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
     theme_fivethirtyeight() + theme(legend.position="none", text=element_text(size=9))
   
   ## Obs over 85 - doesn't work!!
   #oHot <- o[o$UTCI>=min_F, c("Date", "Time", "Month", "HST")]
 
   
   ## Hottest day
   hottest <- do.call("rbind", by(o, o$Month, function(x) x[which.max(x$UTCI_F),c("Month", "Date")]))
   
   cr.hourly <- merge(cr, hottest, by=c("Date","Month"))
   o.hourly <- merge(o, hottest, by=c("Date","Month"))
   
   o.hourly$SchoolDate <- NULL
   o.hourly$SchoolHour <- NULL
   o.hourly <- melt(o.hourly, id.vars=c("Date", "Month", "Time"))
   o.hourly$Time <- as.POSIXct(o.hourly$Time, format="%H:%M", tz="UTC")
   cr.hourly$Time <- as.POSIXct(cr.hourly$Time, format="%H:%M", tz="UTC")
   
   plot.title <- ""
   plot.subtitle <- "Average School Day UTCI, Hottest Day"
   
   l3 <- ggplot() + 
     geom_hline(yintercept=min_F, linetype="dotted", color="black", size=0.5) +
     geom_line(data=cr.hourly, aes(x=Time, y=UTCI_F), size=0.8, alpha=0.5, group=1, color="forestgreen") +
     geom_line(data=o.hourly, aes(x=Time, y=value, linetype=variable, size=variable, group=variable), color="dimgrey") +
     facet_grid(~Month) +
     scale_y_continuous(breaks=seq(70,100,5), limits=c(70,100)) +
     scale_color_manual(values=palette(ann.size)) +
     scale_size_manual(name="", values=c(0.5,0.8), guide=FALSE) +
     scale_linetype_manual(name = "", labels=c("Outdoor Temperature", "Outdoor UTCI"), values=c("longdash","solid")) +
     scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
     ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
     theme_fivethirtyeight() + theme(text=element_text(size=9),
                                     legend.title=element_blank(),
                                     legend.position=c(0.08,0.15), 
                                     legend.background=element_rect(color="grey", fill="#F0F0F0", size=0.4, linetype="solid"), legend.box="horizontal") +
     guides(linetype = guide_legend(override.aes = list(size=0.5)))        
   
   
   
   l <- arrangeGrob(l1,l3, layout_matrix=rbind(c(1), c(2)))
   
  
  
  
  
}