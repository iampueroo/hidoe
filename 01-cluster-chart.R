clusterchart <- function(clustervar, clustervalue) {
  
  library(readxl)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(lazyeval)
  library(ggplot2)
  library(ggthemes) 
  
  # Static variables
  min.temp <- 85

  # Input data
  setwd("~/BOX Sync/HIDOE-Data-Repository/Processed/") 
  file <- list.files(pattern = "\\.csv$")[[length(list.files(pattern = "\\.csv$"))]]
  cr <- read_csv(file=paste0("~/BOX Sync/HIDOE-Data-Repository/Processed/", file)) 
  cr <- cr[, c("Datetime_HST", "Temp_F", "RH", "Alias")] #restrict to school
  cr$windspeed <- 0 
  cr <- temp2utci(cr, "Temp_F", "RH", "windspeed") #calculate UTCI
  cr$Date <- as.Date(cr$Datetime_HST, format="%m/%d/%y")
  cr$Time <- format(strptime(cr$Datetime_HST, format="%m/%d/%y %I:%M:%S %p"), format="%H:%M")
  cr$Month <- factor(format(cr$Date, "%B"), c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May", "June", "July"))
  cr$Datetime_HST <- as.POSIXct(paste(cr$Date, cr$Time), format="%Y-%m-%d %H:%M")
  cr$School <- sub("(.*) -.*", "\\1", cr$Alias)
  
  site <- read_excel("~/BOX Sync/HIDOE-Data-Repository/master-site-asset.xlsx")
  site <- site[c("School", "Short School Name", "Island", "Zone", "Climate", "Elevation")]
  colnames(site) <- c("SchoolLong", "School", "Island", "Zone", "Climate", "Elevation")
  
  # Restrictions
  cr <- cr[complete.cases((cr)),]
  s <- inner_join(cr, site[,c("School", clustervar)], by="School") %>% filter_(., interp(~var==clustervalue, var=as.name(clustervar)))
  
  ### Plot 
  ## UTCI
  
  #manipulations for plotting
  summer <- tbl_df(data.frame(Month=c("August", "September", "October", "November", "December", "January", "February", "March", "April", "May", "June", "July"))) %>% mutate(Month = factor(Month, Month))  #manipulations for plotting
  
  plot.subtitle <- "Universal Thermal Climate Index Profile"
  plot.title <- paste0("HIDOE ", clustervar, ": ", clustervalue)
  
  #interquartile plot
  s.monthly <- s %>% group_by(School, Month) %>% mutate(med=median(UTCI_F, na.rm=TRUE), 
                                                        uq = quantile(UTCI_F, 0.75, na.rm=TRUE),
                                                        lq = quantile(UTCI_F, 0.25, na.rm=TRUE)) %>% mutate(upper=uq+(uq-lq)*1.5,
                                                                                                            lower=lq-(uq-lq)*1.5) %>% distinct(School, Month) 
  
  
  s.extremes <- inner_join(s, s.monthly[,c("School", "Month", "uq", "lq")], by=c("School", "Month")) %>% filter(., UTCI_F>uq | UTCI_F<lq) %>% mutate(rUTCI_F=round(UTCI_F,2)) %>% distinct(School, Month, rUTCI_F)
  
  gg <- ggplot() +
    geom_hline(yintercept=min.temp, linetype="dotted", color="black", size=0.5) +
    geom_point(data=s.extremes, aes(x=Month, y=rUTCI_F), color=adjustcolor("dodgerblue4", alpha.f = 0.2), size=1.6, shape="_") +
    #geom_linerange(data=s.monthly, aes(x=Month, ymin=lower, ymax=upper), color="dodgerblue4", alpha=0.3, size=1.5) +
    geom_linerange(data=s.monthly, aes(x=Month, ymin=lq, ymax=uq), color="dodgerblue4", alpha=1, size=1.5) +
    geom_point(data=s.monthly, aes(x=Month, y=med), shape='-', size=5, color="wheat4") +
    facet_grid(~School, drop=FALSE) +
    ggtitle(bquote(atop(.(plot.title), atop(.(plot.subtitle), "")))) +
    scale_y_continuous(breaks=seq(60,130,5), limits=c(60,130), labels=ggdeg(seq(60,130,5))) +
    scale_x_discrete(labels=c("August"="A", "September"="S", "October"="O", "November"="N", "December"="D", "January"="J", "February"="F", "March"="M", "April"="A", "May"="M", "June"="J", "July"="J"), drop=FALSE) +
    labs(x=NULL, y=NULL) +
    theme_bw(base_family="sans") +
    theme(axis.text.x=element_text(size=rel(0.8), color="gray30"), panel.border=element_blank(), legend.key=element_blank(),
          text=element_text(color="gray30"), axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"), strip.background=element_blank(),
          plot.margin = unit(c(2, 2, 2, 2), "lines"), 
          strip.text.x = element_text(size = rel(1.1), color="gray30"))
  
  
  ggsave(filename=paste0("~/dropbox/rh1/hidoe/plots/", tolower(clustervar), "-", tolower(clustervalue), ".pdf"), gg, width=30, height=16, units="in")
  
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
ggdeg <- function(x) {
  parse(text = paste(x, "*degree", sep = ""))
}