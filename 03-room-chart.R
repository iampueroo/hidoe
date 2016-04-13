roomchart <- function(classroomlist, temprange, rhrange, windspeedrange, winddirection, start, end, start2=NULL, end2=NULL) {
  library(readxl)
  library(readr)
  library(googlesheets)
  library(magrittr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggthemes) 
  library(scales)
  library(directlabels)
  library(gtable)
  library(grid)
  library(gridExtra)
  
  min.temp <- 85
  start.date <- as.Date(start, format="%m-%d-%Y")
  end.date <- as.Date(end, format="%m-%d-%Y")
  if (!is.null(start2)) { start.date2 <- as.Date(start2, format="%m-%d-%Y") }
  if (!is.null(end2)) { end.date2 <- as.Date(end2, format="%m-%d-%Y") }
  
  #derive min/max of criteria
  minT <- as.numeric(sub("(.*)-.*", "\\1", temprange)) 
  maxT <- as.numeric(sub(".*-(.*)", "\\1", temprange)) 
  minRH <- as.numeric(sub("(.*)-.*", "\\1", rhrange)) 
  maxRH <- as.numeric(sub(".*-(.*)", "\\1", rhrange)) 
  minW <- as.numeric(sub("(.*)-.*", "\\1", windspeedrange)) 
  maxW <- as.numeric(sub(".*-(.*)", "\\1", windspeedrange)) 
  
  
  ###Input data 
  sheets <- gs_ls() #google sheets
  gs <- gs_title("MASTER-sensor-weather-deployment") %>% gs_read(ws = "School List") 
  if (end.date <= as.Date("10-17-2014", format="%m-%d-%Y")) {wslist = rep(list("KHIEWABE3"), times=length(classroomlist))
  } else { wslist <- lapply(classroomlist, function(x) as.character(gs[gs$School==getschool(x), c("Closest WS - Any")])) }
  
  #o.old <- read_csv(file="~/BOX Sync/HIDOE-Data-Repository/Raw/Weather-Station/HNEI_EwaWeather_Raw_Full.csv") #outdoor - pilot 
  #o.old <- o.old[o.old$WS_ID %in% wslist,c("Hawaii Time (-10 Hours)", "Temp_F", "RH", "Wind Speed", "Wind Dir", "UTCI_F", "WS_ID")] #restrict to weather station
  #colnames(o.old) <- c("Datetime_HST", "Temp_F", "RH", "Windspeed", "Winddir", "UTCI_F", "WS_ID")
  #o.old$Date <- as.Date(o.old$Datetime_HST, format="%m/%d/%y")
  #o.old$Time <- format(strptime(o.old$Datetime_HST, format="%m/%d/%y %H:%M"), format="%H:%M")
  #o.old$Month <- factor(format(o.old$Date, "%B"), c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
  #o.old$Datetime_HST <- as.POSIXct(paste(o.old$Date, o.old$Time), format="%Y-%m-%d %H:%M")
  #o.old <- o.old[o.old$Date<=as.Date("2015-06-23"),] #use old weather station csv until new from Will
  
  o <- read_csv(file="~/BOX Sync/HIDOE-Data-Repository/Raw/Weather-Station/20160404-master-weather.csv") #new outdoor
  o$Date <- as.Date(o$Date_HST, format="%Y-%m-%d")
  o$Time <- format(strptime(o$Time_HST, format="%Y-%m-%d %H:%M:%S"), format="%H:%M")
  o$Month <- factor(format(o$Date, "%B"), c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
  o$Datetime_HST <- as.POSIXct(paste(o$Date, o$Time), format="%Y-%m-%d %H:%M")
  o <- temp2utci(o, "Temp_F", "RH", "Windspeed") #calculate UTCI
  o <- o[o$WS_ID %in% wslist, c("Datetime_HST", "Temp_F", "RH", "Windspeed", "UTCI_F", "WS_ID", "Date", "Time", "Month")]
  
  setwd("~/BOX Sync/HIDOE-Data-Repository/Processed/") 
  file <- list.files(pattern = "\\.csv$")[[length(list.files(pattern = "\\.csv$"))]]
  cr <- read_csv(file=paste0("~/BOX Sync/HIDOE-Data-Repository/Processed/", file)) 
  cr <- cr[cr$Alias %in% classroomlist, c("Datetime_HST", "Temp_F", "RH", "Alias")] #restrict to school
  cr$windspeed <- 0 
  cr <- temp2utci(cr, "Temp_F", "RH", "windspeed") #calculate UTCI
  cr$Date <- as.Date(cr$Datetime_HST, format="%m/%d/%y")
  cr$Time <- format(strptime(cr$Datetime_HST, format="%m/%d/%y %I:%M:%S %p"), format="%H:%M")
  cr$Month <- factor(format(cr$Date, "%B"), c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
  cr$Datetime_HST <- as.POSIXct(paste(cr$Date, cr$Time), format="%Y-%m-%d %H:%M")
  cr$Alias <- as.factor(cr$Alias)
  
  site <- read_excel("~/BOX Sync/HIDOE-Data-Repository/master-site-asset.xlsx")
  site <- site[c("School", "Short School Name", "Island", "Zone", "Climate", "Elevation")]
  colnames(site) <- c("SchoolLong", "School", "Island", "Zone", "Climate", "Elevation")
  
  # Restrictions
  o <- o[complete.cases((o)),] 
  cr <- cr[complete.cases((cr)),]
  
  ## Split by classroom
  cr.dfs <- split(cr, cr$Alias)
  o.dfs.temp <- split(o, o$WS_ID)
  o.dfs = list()
  for (i in 1:length(wslist)) { o.dfs[[i]] <- o.dfs.temp[[wslist[[i]]]] }
  
  #Set up comparison method
  if (length(classroomlist)==1) { #before and after of same classroom
    method <- "B/A same classroom"
    stopifnot(!is.null(start2), !is.null(end2))
    o.dfs.temp <- o.dfs
    o.dfs <- list()
    o.dfs[[1]] <- o.dfs.temp[[1]] %>% filter(., Date >= start.date & Date <= end.date)
    o.dfs[[2]] <- o.dfs.temp[[1]] %>% filter(., Date >= start.date2 & Date <= end.date2)
    
    cr.dfs.temp <- cr.dfs
    cr.dfs <- list()
    cr.dfs[[1]] <- cr.dfs.temp[[1]] %>% filter(., Date >= start.date & Date <= end.date)
    cr.dfs[[2]] <- cr.dfs.temp[[1]] %>% filter(., Date >= start.date2 & Date <= end.date2)
  } else if (!is.null(start2) & !is.null(end2)) { #before/after with different classrooms
    method <- "B/A different classroom"
    o.dfs[[1]] <- o.dfs[[1]] %>% filter(., Date >= start.date & Date <= end.date)
    o.dfs[[2]] <- o.dfs[[2]] %>% filter(., Date >= start.date2 & Date <= end.date2)
    
    cr.dfs[[1]] <- cr.dfs[[1]] %>% filter(., Date >= start.date & Date <= end.date)
    cr.dfs[[2]] <- cr.dfs[[2]] %>% filter(., Date >= start.date2 & Date <= end.date2)
  } else { # two classrooms
    method <- "different classroom"
    o.dfs <- lapply(o.dfs, function(x) x[(x$Date >= start.date & x$Date <= end.date), ]) 
    cr.dfs <- lapply(cr.dfs, function(x) x[(x$Date >= start.date & x$Date <= end.date), ])
  }
  
  #Evaluation conditions
  eval <- list()
  samplesizes <- list()
  for (i in 1:length(wslist)) {
    eval[[i]] <- o.dfs[[i]] %>% arrange(., Datetime_HST) %>% group_by(Date) %>% mutate(., tu = c(0, difftime(tail(Datetime_HST, -1), head(Datetime_HST, -1)))/60) %>% mutate(., Hour=sub("(.*):.*", "\\1", Time)) %>%
      group_by(Date, Hour) %>% summarize(avgT=mean(Temp_F),
                                         avgRH=mean(RH),
                                         avgW=mean(Windspeed)) %>% mutate(., tu = ifelse((avgT >= minT   & avgT <= maxT  ) &
                                                                                           (avgRH >= minRH & avgRH <= maxRH) &
                                                                                           (avgW >= minW   & avgW <= maxW  ),1,0)) %>% filter(., as.character(Hour) %in% c("08", "09", "10", "11", "12", "13")) %>% group_by(Date) %>% summarize(tus=sum(tu)) %>%
      filter(.,tus >= 4) #at least 4 school hours where average meets evaluation criteria
    
    cr.dfs[[i]] <- inner_join(cr.dfs[[i]], eval[[i]][,c("Date")], by=c("Date"))
    o.dfs[[i]] <- inner_join(o.dfs[[i]], eval[[i]][,c("Date")], by=c("Date"))
    
    samplesizes[[i]] <- length(unique(o.dfs[[i]]$Date))
  }
  
  o <- do.call("rbind", o.dfs) %>% do(unique(.)) #combine individual dataframes into single dataframe
  cr <- do.call("rbind", cr.dfs) %>% do(unique(.)) 
  
  o.hourly <- o %>% group_by(WS_ID, Time) %>% summarize(avgUTCI=mean(UTCI_F), avgTemp=mean(Temp_F), avgRH=mean(RH), avgWindspeed=mean(Windspeed)) %>% gather(variable, value, -WS_ID, -Time)
  cr.hourly <- cr %>% group_by(Alias, Time) %>% summarise(avgUTCI=mean(UTCI_F)) %>% gather(variable, value, -Alias, -Time)
  
  #Manipulations for plotting
  o.hourly$Time <- ggtime(o.hourly$Time)
  cr.hourly$Time <- ggtime(cr.hourly$Time)
  lims <- ggtime(c("0:00", "23:59"))
  lims[2] <- lims[2] + 1.5*60*60 #add hours to make space for labels
  bks <- ggtime(c("0:00", "02:00", "04:00", "06:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "16:00", "18:00", "20:00", "22:00", "23:59:99"))
  labs <- c("00:00", rep("", 3), "08:00", rep("", 5), "14:00", rep("", 4), "24:00")
  
  ann <- tbl_df(data.frame(Time=ggtime("02:00"),  UTCI_F=40.5,  lab = paste0(format(start.date, "%b %d %Y"), " to ", format(end.date, "%b %d %Y"))))
  
  plot.title <- pasteblock(classroomlist)
  plot.subtitle <- "Universal Thermal Climate Index Comparison"
  ## add conditios in annotation
  
  ggu <- ggplot() +
    annotate("rect", xmin = ggtime("08:00"), xmax = ggtime("14:00"), ymin = -Inf, ymax = Inf, fill="gray70", alpha=0.1) +
    geom_smooth(data=o.hourly[o.hourly$variable %in% c("avgUTCI", "avgTemp"), ], aes(x=Time, y=value, linetype=variable, size=variable), color="dimgrey", alpha=0.6, se=FALSE, method="loess", span=0.1) +
    geom_smooth(data=o.hourly[o.hourly$variable=="avgRH", ], aes(x=Time, y=value), color="#77AB43", alpha=0.6, size=0.5, linetype="dashed", se=FALSE, method="loess", span=0.1) +
    geom_line(data=cr.hourly, aes(x=Time, y=value, group=Alias, color=Alias), size=1.5, alpha=0.6) + 
    annotate("text", x=ggtime("00:05"), y=maxT-0.2, label=paste0(format(start.date, "%b %d %Y"), " to ", format(end.date, "%b %d %Y")), size=4, color="gray30", hjust="left", fontface="bold") + 
    annotate("text", x=ggtime("00:05"), y=maxT-1.2, label=paste0(wslist[[1]], ": ", samplesizes[[1]], " days"), size=3.5, color="gray30", hjust="left") +
    annotate("text", x=ggtime("00:05"), y=maxT-2.2, label=paste0(wslist[[2]], ": ", samplesizes[[2]], " days"), size=3.5, color="gray30", hjust="left") +
    annotate("text", x=lims[2], y=110, label="110%", size=3.5, color="#77AB43") +
    annotate("text", x=lims[2], y=100, label="100%", size=3.5, color="#77AB43") +
    annotate("text", x=lims[2], y=90, label="90%", size=3.5, color="#77AB43") +
    annotate("text", x=lims[2], y=80, label="80%", size=3.5, color="#77AB43") +
    annotate("text", x=lims[2], y=70, label="70%", size=3.5, color="#77AB43") +
    annotate("text", x=lims[2], y=60, label="60%", size=3.5, color="#77AB43") +
    annotate("text", x=lims[2], y=50, label="50%", size=3.5, color="#77AB43") +
    annotate("text", x=lims[2], y=40, label="40%", size=3.5, color="#77AB43") +
    scale_color_fivethirtyeight() +
    scale_linetype_manual(name="", values=c("avgUTCI"="solid", "avgTemp"="dashed")) +
    scale_size_manual(name="", values=c("avgUTCI"=1, "avgTemp"=0.5)) +
    scale_y_continuous(breaks=seq(50,100,10), labels=ggdeg(seq(50,100,10)), limits = c(minRH, maxT)) +
    scale_x_datetime(breaks=bks, labels=labs, limits=lims) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    labs(x=NULL, y=NULL) +
    theme_bw(base_family="sans") +
    theme(axis.ticks=element_blank(), legend.position="none", panel.border=element_blank(), legend.key=element_blank(),
          text=element_text(color="gray30"),
          plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"), strip.background=element_blank(),
          plot.margin = unit(c(2, 2, 2, 2), "lines"))
  
  ggu <- direct.label.ggplot(ggu, list("last.qp", cex=1, alpha=0.6))
  
  ggw <- ggplot() +
    geom_smooth(data=o.hourly[o.hourly$variable=="avgWindspeed", ], aes(x=Time, y=value), color="#E5AE38", alpha=0.6, size=0.5, se=FALSE, method="loess", span=0.1, linetype="dashed") +
    scale_y_continuous(breaks=seq(0,30,5), labels=ggmph(seq(0,30,5)), limits=c(0,20)) +
    scale_x_datetime(breaks=bks, labels=labs, limits=lims) +
    labs(x=NULL, y=NULL) +
    theme_bw(base_family="sans") +
    theme(axis.ticks=element_blank(), legend.position="none", panel.border=element_blank(), legend.key=element_blank(),
          plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"), strip.background=element_blank(),
          plot.margin = unit(c(2, 2, 2, 2), "lines"), 
          axis.text.x = element_blank(),
          panel.background = element_rect(fill=NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
          axis.text = element_text(colour="#E5AE38"))
  
  g1 <- ggplot_gtable(ggplot_build(ggu)) #extract gtable
  g2 <- ggplot_gtable(ggplot_build(ggw))
  pp <- c(subset(g1$layout, name == "panel", se = t:r)) #overlay second plot on first
  g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
  
  # axis tweaks
  ia <- which(g2$layout$name == "axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  #ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
  
  pdf(paste0("~/dropbox/rh1/hidoe/plots/", gsub("---", "", tolower(gsub(" ", "-", paste(classroomlist, collapse="-")))), ".pdf"), width=30, height=16)
  plot(g)
  dev.off()
  
}

temp2utci <- function(df, temp, rh, wind) {
  ### Convert temperature F to UTCI F
  library(dplyr)
  
  tempdata <- df[,c(temp, rh, wind)]
  colnames(tempdata) <- c("t", "rh", "ws")
  
  #conversions for UTCI calculations
  tempdata <-  tempdata %>% mutate(tc=(t-32)*(5/9)) 
  tempdata <-  tempdata %>% mutate(w=ws*0.44704) 
  tempdata <-  tempdata %>% mutate(rhd=rh/100)
  tempdata <-  tempdata %>% mutate(vp=6.11 * 10^ ((7.5 * tc) / (237.3 + tc)) * rhd / 10)
  
  tempdata <- tempdata %>% mutate(f1=tc +
                                    (0.607562052) +
                                    (-0.0227712343) * tc +
                                    (8.06470249 * 10^(-4)) * tc * tc +
                                    (-1.54271372 * 10^(-4)) * tc * tc * tc +
                                    (-3.24651735 * 10^(-6)) * tc * tc * tc * tc +
                                    (7.32602852 * 10^(-8)) * tc * tc * tc * tc * tc +
                                    (1.35959073 * 10^(-9)) * tc * tc * tc * tc * tc * tc +
                                    (-2.2583652) * w +
                                    (0.0880326035) * tc * w +
                                    (0.00216844454) * tc * tc * w +
                                    (-1.53347087 * 10^(-5)) * tc * tc * tc * w +
                                    (-5.72983704 * 10^(-7)) * tc * tc * tc * tc * w +
                                    (-2.55090145 * 10^(-9)) * tc * tc * tc * tc * tc * w +
                                    (-0.751269505) * w * w +
                                    (-0.00408350271) * tc * w * w +
                                    (-5.21670675 * 10^(-5)) * tc * tc * w * w +
                                    (1.94544667 * 10^(-6)) * tc * tc * tc * w * w +
                                    (1.14099531 * 10^(-8)) * tc * tc * tc * tc * w * w +
                                    (0.158137256) * w * w * w +
                                    (-6.57263143 * 10^(-5)) * tc * w * w * w +
                                    (2.22697524 * 10^(-7)) * tc * tc * w * w * w)
  tempdata <- tempdata %>% mutate(f2=(-4.16117031 * 10^(-8)) * tc * tc * tc * w * w * w +
                                    (-0.0127762753) * w * w * w * w +
                                    (9.66891875 * 10^(-6)) * tc * w * w * w * w +
                                    (2.52785852 * 10^(-9)) * tc * tc * w * w * w * w +
                                    (4.56306672 * 10^(-4)) * w * w * w * w * w +
                                    (-1.74202546 * 10^(-7)) * tc * w * w * w * w * w +
                                    (-5.91491269 * 10^(-6)) * w * w * w * w * w * w +
                                    (0.398374029) * 0 +
                                    (1.83945314 * 10^(-4)) * tc * 0 +
                                    (-1.7375451 * 10^(-4)) * tc * tc * 0 +
                                    (-7.60781159 * 10^(-7)) * tc * tc * tc * 0 +
                                    (3.77830287 * 10^(-8)) * tc * tc * tc * tc * 0 +
                                    (5.43079673 * 10^(-10)) * tc * tc * tc * tc * tc * 0 +
                                    (-0.0200518269) * w * 0 +
                                    (8.92859837 * 10^(-4)) * tc * w * 0 +
                                    (3.45433048 * 10^(-6)) * tc * tc * w * 0 +
                                    (-3.77925774 * 10^(-7)) * tc * tc * tc * w * 0 +
                                    (-1.69699377 * 10^(-9)) * tc * tc * tc * tc * w * 0 +
                                    (1.69992415 * 10^(-4)) * w * w * 0 +
                                    (-4.99204314 * 10^(-5)) * tc * w * w * 0 +
                                    (2.47417178 * 10^(-7)) * tc * tc * w * w * 0 +
                                    (1.07596466 * 10^(-8)) * tc * tc * tc * w * w * 0 +
                                    (8.49242932 * 10^(-5)) * w * w * w * 0 +
                                    (1.35191328 * 10^(-6)) * tc * w * w * w * 0 +
                                    (-6.21531254 * 10^(-9)) * tc * tc * w * w * w * 0 +
                                    (-4.99410301 * 10^(-6)) * w * w * w * w * 0 +
                                    (-1.89489258 * 10^(-8)) * tc * w * w * w * w * 0)
  tempdata <- tempdata %>% mutate(f3=(8.15300114 * 10^(-8)) * w * w * w * w * w * 0 +
                                    (7.5504309 * 10^(-4)) * 0 * 0 +
                                    (-5.65095215 * 10^(-5)) * tc * 0 * 0 +
                                    (-4.52166564 * 10^(-7)) * tc * tc * 0 * 0 +
                                    (2.46688878 * 10^(-8)) * tc * tc * tc * 0 * 0 +
                                    (2.42674348 * 10^(-10)) * tc * tc * tc * tc * 0 * 0 +
                                    (1.5454725 * 10^(-4)) * w * 0 * 0 +
                                    (5.2411097 * 10^(-6)) * tc * w * 0 * 0 +
                                    (-8.75874982 * 10^(-8)) * tc * tc * w * 0 * 0 +
                                    (-1.50743064 * 10^(-9)) * tc * tc * tc * w * 0 * 0 +
                                    (-1.56236307 * 10^(-5)) * w * w * 0 * 0 +
                                    (-1.33895614 * 10^(-7)) * tc * w * w * 0 * 0 +
                                    (2.49709824 * 10^(-9)) * tc * tc * w * w * 0 * 0 +
                                    (6.51711721 * 10^(-7)) * w * w * w * 0 * 0 +
                                    (1.94960053 * 10^(-9)) * tc * w * w * w * 0 * 0 +
                                    (-1.00361113 * 10^(-8)) * w * w * w * w * 0 * 0 +
                                    (-1.21206673 * 10^(-5)) * 0 * 0 * 0 +
                                    (-2.1820366 * 10^(-7)) * tc * 0 * 0 * 0 +
                                    (7.51269482 * 10^(-9)) * tc * tc * 0 * 0 * 0 +
                                    (9.79063848 * 10^(-11)) * tc * tc * tc * 0 * 0 * 0 +
                                    (1.25006734 * 10^(-6)) * w * 0 * 0 * 0 +
                                    (-1.81584736 * 10^(-9)) * tc * w * 0 * 0 * 0 +
                                    (-3.52197671 * 10^(-10)) * tc * tc * w * 0 * 0 * 0 +
                                    (-3.3651463 * 10^(-8)) * w * w * 0 * 0 * 0 +
                                    (1.35908359 * 10^(-10)) * tc * w * w * 0 * 0 * 0 +
                                    (4.1703262 * 10^(-10)) * w * w * w * 0 * 0 * 0)
  tempdata <- tempdata %>% mutate(f4=(-1.30369025 * 10^(-9)) * 0 * 0 * 0 * 0 +
                                    (4.13908461 * 10^(-10)) * tc * 0 * 0 * 0 * 0 +
                                    (9.22652254 * 10^(-12)) * tc * tc * 0 * 0 * 0 * 0 +
                                    (-5.08220384 * 10^(-9)) * w * 0 * 0 * 0 * 0 +
                                    (-2.24730961 * 10^(-11)) * tc * w * 0 * 0 * 0 * 0 +
                                    (1.17139133 * 10^(-10)) * w * w * 0 * 0 * 0 * 0 +
                                    (6.62154879 * 10^(-10)) * 0 * 0 * 0 * 0 * 0 +
                                    (4.0386326 * 10^(-13)) * tc * 0 * 0 * 0 * 0 * 0 +
                                    (1.95087203 * 10^(-12)) * w * 0 * 0 * 0 * 0 * 0 +
                                    (-4.73602469 * 10^(-12)) * 0 * 0 * 0 * 0 * 0 * 0 +
                                    (5.12733497) * vp +
                                    (-0.312788561) * tc * vp +
                                    (-0.0196701861) * tc * tc * vp +
                                    (9.9969087 * 10^(-4)) * tc * tc * tc * vp +
                                    (9.51738512 * 10^(-6)) * tc * tc * tc * tc * vp +
                                    (-4.66426341 * 10^(-7)) * tc * tc * tc * tc * tc * vp +
                                    (0.548050612) * w * vp +
                                    (-0.00330552823) * tc * w * vp +
                                    (-0.0016411944) * tc * tc * w * vp +
                                    (-5.16670694 * 10^(-6)) * tc * tc * tc * w * vp +
                                    (9.52692432 * 10^(-7)) * tc * tc * tc * tc * w * vp +
                                    (-0.0429223622) * w * w * vp +
                                    (0.00500845667) * tc * w * w * vp +
                                    (1.00601257 * 10^(-6)) * tc * tc * w * w * vp +
                                    (-1.81748644 * 10^(-6)) * tc * tc * tc * w * w * vp +
                                    (-1.25813502 * 10^(-3)) * w * w * w * vp +
                                    (-1.79330391 * 10^(-4)) * tc * w * w * w * vp +
                                    (2.34994441 * 10^(-6)) * tc * tc * w * w * w * vp)
  tempdata <- tempdata %>% mutate(f5=(1.29735808 * 10^(-4)) * w * w * w * w * vp +
                                    (1.2906487 * 10^(-6)) * tc * w * w * w * w * vp +
                                    (-2.28558686 * 10^(-6)) * w * w * w * w * w * vp +
                                    (-0.0369476348) * 0 * vp +
                                    (0.00162325322) * tc * 0 * vp +
                                    (-3.1427968 * 10^(-5)) * tc * tc * 0 * vp +
                                    (2.59835559 * 10^(-6)) * tc * tc * tc * 0 * vp +
                                    (-4.77136523 * 10^(-8)) * tc * tc * tc * tc * 0 * vp +
                                    (8.6420339 * 10^(-3)) * w * 0 * vp +
                                    (-6.87405181 * 10^(-4)) * tc * w * 0 * vp +
                                    (-9.13863872 * 10^(-6)) * tc * tc * w * 0 * vp +
                                    (5.15916806 * 10^(-7)) * tc * tc * tc * w * 0 * vp +
                                    (-3.59217476 * 10^(-5)) * w * w * 0 * vp +
                                    (3.28696511 * 10^(-5)) * tc * w * w * 0 * vp +
                                    (-7.10542454 * 10^(-7)) * tc * tc * w * w * 0 * vp +
                                    (-1.243823 * 10^(-5)) * w * w * w * 0 * vp +
                                    (-7.385844 * 10^(-9)) * tc * w * w * w * 0 * vp +
                                    (2.20609296 * 10^(-7)) * w * w * w * w * 0 * vp +
                                    (-7.3246918 * 10^(-4)) * 0 * 0 * vp +
                                    (-1.87381964 * 10^(-5)) * tc * 0 * 0 * vp +
                                    (4.80925239 * 10^(-6)) * tc * tc * 0 * 0 * vp +
                                    (-8.7549204 * 10^(-8)) * tc * tc * tc * 0 * 0 * vp +
                                    (2.7786293 * 10^(-5)) * w * 0 * 0 * vp +
                                    (-5.06004592 * 10^(-6)) * tc * w * 0 * 0 * vp +
                                    (1.14325367 * 10^(-7)) * tc * tc * w * 0 * 0 * vp)
  tempdata <- tempdata %>% mutate(f6=(2.53016723 * 10^(-6)) * w * w * 0 * 0 * vp +
                                    (-1.72857035 * 10^(-8)) * tc * w * w * 0 * 0 * vp +
                                    (-3.95079398 * 10^(-8)) * w * w * w * 0 * 0 * vp +
                                    (-3.59413173 * 10^(-7)) * 0 * 0 * 0 * vp +
                                    (7.04388046 * 10^(-7)) * tc * 0 * 0 * 0 * vp +
                                    (-1.89309167 * 10^(-8)) * tc * tc * 0 * 0 * 0 * vp +
                                    (-4.79768731 * 10^(-7)) * w * 0 * 0 * 0 * vp +
                                    (7.96079978 * 10^(-9)) * tc * w * 0 * 0 * 0 * vp +
                                    (1.62897058 * 10^(-9)) * w * w * 0 * 0 * 0 * vp +
                                    (3.94367674 * 10^(-8)) * 0 * 0 * 0 * 0 * vp +
                                    (-1.18566247 * 10^(-9)) * tc * 0 * 0 * 0 * 0 * vp +
                                    (3.34678041 * 10^(-10)) * w * 0 * 0 * 0 * 0 * vp +
                                    (-1.15606447 * 10^(-10)) * 0 * 0 * 0 * 0 * 0 * vp +
                                    (-2.80626406) * vp * vp +
                                    (0.548712484) * tc * vp * vp +
                                    (-0.0039942841) * tc * tc * vp * vp +
                                    (-9.54009191 * 10^(-4)) * tc * tc * tc * vp * vp +
                                    (1.93090978 * 10^(-5)) * tc * tc * tc * tc * vp * vp +
                                    (-0.308806365) * w * vp * vp +
                                    (0.0116952364) * tc * w * vp * vp +
                                    (4.95271903 * 10^(-4)) * tc * tc * w * vp * vp +
                                    (-1.90710882 * 10^(-5)) * tc * tc * tc * w * vp * vp +
                                    (0.00210787756) * w * w * vp * vp +
                                    (-6.98445738 * 10^(-4)) * tc * w * w * vp * vp +
                                    (2.30109073 * 10^(-5)) * tc * tc * w * w * vp * vp +
                                    (4.1785659 * 10^(-4)) * w * w * w * vp * vp)
  tempdata <- tempdata %>% mutate(f7=(-1.27043871 * 10^(-5)) * tc * w * w * w * vp * vp +
                                    (-3.04620472 * 10^(-6)) * w * w * w * w * vp * vp +
                                    (0.0514507424) * 0 * vp * vp +
                                    (-0.00432510997) * tc * 0 * vp * vp +
                                    (8.99281156 * 10^(-5)) * tc * tc * 0 * vp * vp +
                                    (-7.14663943 * 10^(-7)) * tc * tc * tc * 0 * vp * vp +
                                    (-2.66016305 * 10^(-4)) * w * 0 * vp * vp +
                                    (2.63789586 * 10^(-4)) * tc * w * 0 * vp * vp +
                                    (-7.01199003 * 10^(-6)) * tc * tc * w * 0 * vp * vp +
                                    (-1.06823306 * 10^(-4)) * w * w * 0 * vp * vp +
                                    (3.61341136 * 10^(-6)) * tc * w * w * 0 * vp * vp +
                                    (2.29748967 * 10^(-7)) * w * w * w * 0 * vp * vp +
                                    (3.04788893 * 10^(-4)) * 0 * 0 * vp * vp +
                                    (-6.42070836 * 10^(-5)) * tc * 0 * 0 * vp * vp +
                                    (1.16257971 * 10^(-6)) * tc * tc * 0 * 0 * vp * vp +
                                    (7.68023384 * 10^(-6)) * w * 0 * 0 * vp * vp +
                                    (-5.47446896 * 10^(-7)) * tc * w * 0 * 0 * vp * vp +
                                    (-3.5993791 * 10^(-8)) * w * w * 0 * 0 * vp * vp +
                                    (-4.36497725 * 10^(-6)) * 0 * 0 * 0 * vp * vp +
                                    (1.68737969 * 10^(-7)) * tc * 0 * 0 * 0 * vp * vp +
                                    (2.67489271 * 10^(-8)) * w * 0 * 0 * 0 * vp * vp +
                                    (3.23926897 * 10^(-9)) * 0 * 0 * 0 * 0 * vp * vp +
                                    (-0.0353874123) * vp * vp * vp +
                                    (-0.22120119) * tc * vp * vp * vp +
                                    (0.0155126038) * tc * tc * vp * vp * vp)
  tempdata <- tempdata %>% mutate(f8=(-2.63917279 * 10^(-4)) * tc * tc * tc * vp * vp * vp +
                                    (0.0453433455) * w * vp * vp * vp +
                                    (-0.00432943862) * tc * w * vp * vp * vp +
                                    (1.45389826 * 10^(-4)) * tc * tc * w * vp * vp * vp +
                                    (2.1750861 * 10^(-4)) * w * w * vp * vp * vp +
                                    (-6.66724702 * 10^(-5)) * tc * w * w * vp * vp * vp +
                                    (3.3321714 * 10^(-5)) * w * w * w * vp * vp * vp +
                                    (-0.00226921615) * 0 * vp * vp * vp +
                                    (3.80261982 * 10^(-4)) * tc * 0 * vp * vp * vp +
                                    (-5.45314314 * 10^(-9)) * tc * tc * 0 * vp * vp * vp +
                                    (-7.96355448 * 10^(-4)) * w * 0 * vp * vp * vp +
                                    (2.53458034 * 10^(-5)) * tc * w * 0 * vp * vp * vp +
                                    (-6.31223658 * 10^(-6)) * w * w * 0 * vp * vp * vp +
                                    (3.02122035 * 10^(-4)) * 0 * 0 * vp * vp * vp +
                                    (-4.77403547 * 10^(-6)) * tc * 0 * 0 * vp * vp * vp +
                                    (1.73825715 * 10^(-6)) * w * 0 * 0 * vp * vp * vp +
                                    (-4.09087898 * 10^(-7)) * 0 * 0 * 0 * vp * vp * vp +
                                    (0.614155345) * vp * vp * vp * vp +
                                    (-0.0616755931) * tc * vp * vp * vp * vp +
                                    (0.00133374846) * tc * tc * vp * vp * vp * vp +
                                    (0.00355375387) * w * vp * vp * vp * vp +
                                    (-5.13027851 * 10^(-4)) * tc * w * vp * vp * vp * vp +
                                    (1.02449757 * 10^(-4)) * w * w * vp * vp * vp * vp +
                                    (-0.00148526421) * 0 * vp * vp * vp * vp)
  tempdata <- tempdata %>% mutate(f9=(-4.11469183 * 10^(-5)) * tc * 0 * vp * vp * vp * vp +
                                    (-6.80434415 * 10^(-6)) * w * 0 * vp * vp * vp * vp +
                                    (-9.77675906 * 10^(-6)) * 0 * 0 * vp * vp * vp * vp +
                                    (0.0882773108) * vp * vp * vp * vp * vp +
                                    (-0.00301859306) * tc * vp * vp * vp * vp * vp +
                                    (0.00104452989) * w * vp * vp * vp * vp * vp +
                                    (2.47090539 * 10^(-4)) * 0 * vp * vp * vp * vp * vp +
                                    (0.00148348065) * vp * vp * vp * vp * vp * vp)
  
  tempdata <- tempdata %>% mutate(utcif=(f1+f2+f3+f4+f5+f6+f7+f8+f9)*(9/5)+32)   
  df$UTCI_F <- tempdata$utcif
  return(df)
}
ggtime <- function(time) {
  as.POSIXct(strptime(time, format = "%H:%M"), tz="UTC")
}
ggdeg <- function(x) {
  parse(text = paste(x, "*degree", sep = ""))
}
getschool <- function(classroom) {
  return(sub("(.*) -.*", "\\1", classroom))
}
pasteblock <- function(x) {
  return(paste(x, sep="", collapse=", "))
}
ggmph <- function(x) {
  return(paste(x, " mph", sep = ""))
}