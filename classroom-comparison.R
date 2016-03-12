classroom-comparison <- function(classroom1, startdate1, enddate1, classroom2, startdate2, enddate2, weatherstation) {
  library(plyr)
  library(reshape2)
  library(ggplot2)
  library(ggthemes) 
  library(scales)
  library(grid)
  library(gridExtra) 
  
  min_F <- 85
  colors <- c( "dimgrey", "#FC4F30", "#30A2DA")
  colorsHeat <- c("#E69720", "#FC4F30", "#E8E827") 
  stdt1 <- as.Date(startdate1, format="%Y-%m-%d")
  endt1 <- as.Date(enddate1, format="%Y-%m-%d")
  stdt2 <- as.Date(startdate2, format="%Y-%m-%d")
  endt2 <- as.Date(enddate2, format="%Y-%m-%d")
  
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
  cr1 <- cr[cr$Alias==classroom1,] #restrict to classroom specified
  cr2 <- cr[cr$Alias==classroom2,] #restrict to classroom specified
  
  
  # restrict to school hour/dates ------------------------------------------------------------------------------------------------------------------------------------------------------------------
  cr <- rbind(cr1, cr2)
  cr <- cr[cr$SchoolDate==1 & cr$SchoolHour==1,]
  o <- o[o$SchoolDate==1 & o$SchoolHour==1,]
 
  o.daily <- ddply(o,c("Date"), summarise, AvgTemp=mean(Temp_F, na.rm=TRUE), AvgUTCI=mean(UTCI_F, na.rm=TRUE)) #aggregated by date 
  cr.daily <- ddply(cr, c("Date", "Alias"), summarise, AvgUTCI=mean(UTCI_F, na.rm=TRUE), MaxUTCI=max(UTCI_F, na.rm=TRUE)) #aggregated  by date
  
  o.daily <- melt(o.daily, id.vars=c("Date"))
  o.daily$Alias <- "Outdoor"
  cr.daily <- melt(cr.daily, id.vars=c("Date", "Alias"))
  
  cro.daily <- rbind(o.daily, cr.daily)
  cro.daily$map <- paste(cro.daily$Alias, cro.daily$variable, sep=";")
  
  plot.title <- "Daily Universal Thermal Climate Index (UTCI) Profile"
  plot.subtitle <- paste(classroom1, "vs.", classroom2)
  
  u <- ggplot() +
    geom_hline(yintercept=min_F, linetype="dotted", color="black", size=0.5) + 
    geom_point(data=cro.daily[cro.daily$variable=="MaxUTCI",], aes(x=Date, y=value, color=map, shape=map), alpha=0.5) +
    geom_smooth(data=cro.daily, aes(x=Date, y=value, linetype=map, color=map, size=map), fill=NA, span=0.4) +  
    scale_color_manual(name = "", values=c("navy", "firebrick", "#30A2DA", "#FC4F30", "dimgrey", "dimgrey"), guide=FALSE) +   
    scale_linetype_manual(name="", labels=c(paste0("Average ", classroom1, " UTCI"), paste0("Maximum ", classroom1, " UTCI"), paste0("Average ", classroom2, " UTCI"), paste0("Maximum ", classroom2, " UTCI"), "Average Outdoor Temperature", "Average Outdoor UTCI"), 
                           values=c("solid", "solid", "solid", "solid", "dashed", "solid")) +
    scale_size_manual(name="", values=c(0.8, 0.8, 0.8, 0.8, 0.5, 0.8), guide=FALSE) +
    scale_shape_manual(name="", labels=c(paste(classroom1, "(Maximum UTCI)"), paste(classroom2, "(Maximum UTCI)")), values=rep(16, times=6)) +
    scale_y_continuous(breaks=seq(70,105,5), limits=c(min(cro.daily$value), max(cro.daily$value))) + 
    scale_x_date(date_breaks="1 month", date_labels="%b %Y") + 
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_fivethirtyeight() +
    theme(text=element_text(size=9), legend.title=element_blank(), legend.position=c(0.20,0.08), legend.background=element_rect(color="grey", fill="#F0F0F0", size=0.4, linetype="solid"), legend.box="horizontal") +
    guides(linetype = guide_legend(override.aes = list(size=0.5, color=c("navy", "firebrick", "#30A2DA", "#FC4F30", "dimgrey", "dimgrey"), shape=NA)),
           shape = guide_legend(ncol=1, override.aes = list(alpha=c(0.8,0.8), color=c("firebrick", "#FC4F30"))))
  
  ### line plots ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ## All days
  cr.hourly <- ddply(cr, c("Time", "Month", "Alias"), summarize, AvgUTCI=mean(UTCI_F, na.rm=TRUE))
  o.hourly <- ddply(o, c("Time", "Month"), summarize, AvgTemp=mean(Temp_F, na.rm=TRUE), AvgUTCI=mean(UTCI_F, na.rm=TRUE))
  
  cr.hourly <- melt(cr.hourly, id.vars=c("Time", "Month", "Alias"))
  o.hourly <- melt(o.hourly, id.vars=c("Time", "Month"))
  o.hourly$Alias <- "Outdoor"
  
  cro.hourly<- rbind(o.hourly, cr.hourly)
  cro.hourly$Time <- as.POSIXct(cro.hourly$Time, format="%H:%M", tz="UTC")
  cro.hourly$map <- paste(cro.hourly$Alias, cro.hourly$variable, sep=";")
  
  
  lims <- as.POSIXct(strptime(c("08:00","14:00"), format = "%H:%M"), tz="UTC")    
  plot.title <- paste0(classroom1, " vs. ", classroom2, " (", format(stdt1, "%b %Y"), "-", format(endt1, "%b %Y"), ")")
  plot.subtitle <- "Average School Day UTCI, All Observations"
  
  l1 <- ggplot() + 
    geom_hline(yintercept=min_F, linetype="dotted", color="black", size=0.5) +
    geom_line(data=cro.hourly, aes(x=Time, y=value, linetype=map, group=map, color=map, size=map)) +
    facet_grid(~Month) +
    scale_linetype_manual(name = "", values=c("solid","solid", "longdash", "solid")) +
    scale_size_manual(name="", values=c(0.8, 0.8, 0.5, 0.8)) +
    scale_color_manual(name = "", values=c("forestgreen", "green", "dimgrey", "dimgrey")) +  
    scale_y_continuous(breaks=seq(70,100,5), limits=c(70,100)) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_fivethirtyeight() + theme(legend.position="none", text=element_text(size=9))
  
  ## Obs over 85 - doesn't work!!
  oHot <- o[o$UTCI>=min_F, c("Date", "Time", "Month", "HST")]
  
  
  ## Hottest day
  hottest <- do.call("rbind", by(o, o$Month, function(x) x[which.max(x$UTCI_F),c("Month", "Date")]))
  
  cr.hourly <- merge(cr, hottest, by=c("Date","Month"))
  o.hourly <- merge(o, hottest, by=c("Date","Month"))
  
  o.hourly$SchoolDate <- NULL
  o.hourly$SchoolHour <- NULL
  o.hourly <- melt(o.hourly, id.vars=c("Date", "Month", "Time"))
  cr.hourly <- cr.hourly[,c("Date", "Month", "Alias", "UTCI_F", 'Time')]
  cr.hourly <- melt(cr.hourly, id.vars=c("Date", "Month", "Time", "Alias"))
  o.hourly$Alias <- "Outdoor"
  
  cro.hourly<- rbind(o.hourly, cr.hourly)
  cro.hourly$Time <- as.POSIXct(cro.hourly$Time, format="%H:%M", tz="UTC")
  cro.hourly$map <- paste(cro.hourly$Alias, cro.hourly$variable, sep=";")
  
  
  plot.title <- ""
  plot.subtitle <- "Average School Day UTCI, Hottest Day"
  
  l3 <- ggplot() + 
    geom_hline(yintercept=min_F, linetype="dotted", color="black", size=0.5) +
    geom_line(data=cro.hourly, aes(x=Time, y=value, linetype=map, group=map, color=map, size=map)) +
    facet_grid(~Month) +
    scale_linetype_manual(name = "", labels=c(paste0(classroom1, ": UTCI"), paste0(classroom2, ": UTCI"), "Outdoor Temperature", "Outdoor UTCI"), values=c("solid","solid", "longdash", "solid")) +
    scale_size_manual(name="", values=c(0.8, 0.8, 0.5, 0.8), guide=FALSE) +
    scale_color_manual(name = "",  values=c("forestgreen", "green", "dimgrey", "dimgrey"), guide=FALSE) +
    scale_y_continuous(breaks=seq(70,100,5), limits=c(70,100)) +
    scale_x_datetime(breaks=date_breaks("2 hour"), labels=date_format("%H:%M"), limits=lims) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    theme_fivethirtyeight() + theme(text=element_text(size=9),
                                    legend.title=element_blank(),
                                    legend.position=c(0.08,0.15), 
                                    legend.background=element_rect(color="grey", fill="#F0F0F0", size=0.4, linetype="solid"), legend.box="horizontal") +
    guides(linetype = guide_legend(ncol=2, override.aes = list(size=0.5, color=c("forestgreen", "green", "dimgrey", "dimgrey"))))        
  
  
  
  l <- arrangeGrob(l1,l3, layout_matrix=rbind(c(1), c(2)))
  
}