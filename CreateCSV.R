library(plyr)


sdates <- read.csv(file="~/dropbox/rh1/hidoe/day.csv", sep=",")  #school dates
sdates$Date <- as.Date(sdates$rd, format="%m/%d/%y")
sdates <- sdates[-c(1)]
shours <- read.csv(file="~/dropbox/rh1/hidoe/hour.csv", sep=",")  #school hours
shours$Time <- format(strptime(shours$Hour, format="%H:%M"), format="%H:%M")
shours$Hour <- NULL
arch <- read.csv(file="~/dropbox/rh1/hidoe/classroom-csv.csv", sep=",") #classroom architectural data

setwd("~/dropbox/rh1/hidoe/csv")
crt <- do.call(rbind,lapply(dir(), read.csv)) #read all classroom sensor data
names(crt) <- c("RoomID", "SensorAlias", "StartTime", "Temp", "RH", "Ill")
crt$SensorAlias <- as.character(crt$SensorAlias)
crt <- crt[(substr(crt$SensorAlias,nchar(crt$SensorAlias)-1,nchar(crt$SensorAlias))!="HD"),] #remove non 15 minute intervals - keep for now but need to discuss how to resolve this (maybe smoothing)
crtTemp <- crt[!is.na(crt$Temp),c("RoomID", "StartTime", "Temp")]
crtRH <- crt[!is.na(crt$RH),c("RoomID", "StartTime", "RH")]
crt <- merge(crtTemp, crtRH, by=c("RoomID", "StartTime"))
crt <- merge(crt[,c(1,2,3,4)], arch[,c("RoomID", "Alias", "Floor", "RoofColor", "SQFT", "Orientation", "Landscape", "Overhang", "School", "AC")], by="RoomID", all.x=TRUE)
crt$Date <- as.Date(crt$StartTime, format="%m/%d/%y")
crt$Time <- format(strptime(crt$StartTime, format="%m/%d/%y %H:%M"), format="%H:%M")
crt$Month <- format(crt$Date, "%B") 
crt <- merge(crt, sdates, by="Date", all.x=FALSE) #restrict to school days
crt <- merge(crt, shours, by="Time", all.x=FALSE) #restrict to school hours
crt$StartTime <- NULL
crt <- crt[crt$AC!=1,] #remove classrooms with AC
crt$AC <- NULL
crt <- crt     

# Calculate classroom UTCIs
crt$WindSpeed <- 0
crt <- mutate(crt, TempC=(Temp-32)*(5/9))
crt <- mutate(crt, RelRH=RH/100)
crt <- mutate(crt, Vap=6.11 * 10^ ((7.5 * TempC) / (237.3 + TempC)) * RelRH / 10)
crt <- mutate(crt, f1=TempC +  (0.607562052) + 
                (-0.0227712343)*TempC +
                (8.06470249 * 10^(-4)) * TempC * TempC +
                (-1.54271372 * 10^(-4)) * TempC *TempC * TempC +
                (-3.24651735 * 10^(-6)) * TempC * TempC * TempC * TempC +
                (7.32602852 * 10^(-8)) * TempC * TempC * TempC * TempC * TempC +
                (1.35959073 * 10^(-9)) * TempC * TempC * TempC * TempC * TempC * TempC +
                (-2.2583652) * WindSpeed + (0.0880326035) * TempC * WindSpeed +
                (0.00216844454) * TempC * TempC * WindSpeed +
                (-1.53347087 * 10^(-5)) * TempC * TempC * TempC * WindSpeed +
                (-5.72983704 * 10^(-7)) * TempC * TempC * TempC * TempC * WindSpeed +
                (-2.55090145 * 10^(-9)) * TempC * TempC * TempC * TempC * TempC * WindSpeed +
                (-0.751269505) * WindSpeed * WindSpeed + (-0.00408350271) * TempC * WindSpeed * WindSpeed +
                (-5.21670675 * 10^(-5)) * TempC * TempC * WindSpeed * WindSpeed +
                (1.94544667 * 10^(-6)) * TempC * TempC * TempC * WindSpeed * WindSpeed +
                (1.14099531 * 10^(-8)) * TempC * TempC * TempC * TempC * WindSpeed * WindSpeed +
                (0.158137256) * WindSpeed * WindSpeed * WindSpeed +
                (-6.57263143 * 10^(-5)) * TempC * WindSpeed * WindSpeed * WindSpeed +
                (2.22697524 * 10^(-7)) * WindSpeed * WindSpeed * WindSpeed * WindSpeed * WindSpeed)
crt <- mutate(crt, f2=(-4.16117031 * 10^(-8)) * TempC * TempC * TempC * WindSpeed * WindSpeed * WindSpeed +
                (-0.0127762753) * WindSpeed * WindSpeed * WindSpeed * WindSpeed +
                (9.66891875 * 10^(-6)) * TempC * WindSpeed * WindSpeed * WindSpeed * WindSpeed +
                (2.52785852 * 10^(-9)) * TempC * TempC * WindSpeed * WindSpeed * WindSpeed * WindSpeed +
                (4.56306672 * 10^(-4)) * WindSpeed * WindSpeed * WindSpeed * WindSpeed * WindSpeed +
                (-1.74202546 * 10^(-7)) * TempC * WindSpeed * WindSpeed * WindSpeed * WindSpeed * WindSpeed +
                (-5.91491269 * 10^(-6)) * WindSpeed * WindSpeed * WindSpeed * WindSpeed * WindSpeed * WindSpeed +
                (0.398374029) * 0 + (1.83945314 * 10^(-4)) * TempC * 0 +
                (-1.7375451 * 10^(-4)) * TempC * TempC * 0 +
                (-7.60781159 * 10^(-7)) * TempC * TempC * TempC * 0 +
                (3.77830287 * 10^(-8)) * TempC * TempC * TempC * TempC * 0 +
                (5.43079673 * 10^(-10)) * TempC * TempC * TempC * TempC * TempC * 0 +
                (-0.0200518269) * WindSpeed * 0 +
                (8.92859837 * 10^(-4)) * TempC * WindSpeed * 0 +
                (3.45433048 * 10^(-6)) * TempC * TempC * WindSpeed * 0 +
                (-3.77925774 * 10^(-7)) * TempC * TempC * TempC * WindSpeed * 0 +
                (-1.69699377 * 10^(-9)) * TempC * TempC * TempC * TempC * WindSpeed * 0 +
                (1.69992415 * 10^(-4)) * WindSpeed * WindSpeed * 0 +
                (-4.99204314 * 10^(-5)) * TempC * WindSpeed * WindSpeed * 0 +
                (2.47417178 * 10^(-7)) * TempC * TempC * WindSpeed * WindSpeed * 0 +
                (1.07596466 * 10^(-8)) * TempC * TempC * TempC * WindSpeed * WindSpeed * 0 +
                (8.49242932 * 10^(-5)) * WindSpeed * WindSpeed * WindSpeed * 0 +
                (1.35191328 * 10^(-6)) * TempC * WindSpeed * WindSpeed * WindSpeed * 0 +
                (-6.21531254 * 10^(-9)) * TempC * TempC * WindSpeed * WindSpeed * WindSpeed * 0 +
                (-4.99410301 * 10^(-6)) * WindSpeed * WindSpeed * WindSpeed * WindSpeed * 0 +
                (-1.89489258 * 10^(-8)) * TempC * WindSpeed * WindSpeed * WindSpeed * WindSpeed * 0)
crt <- mutate(crt, f3=(8.15300114 * 10^(-8)) * WindSpeed * WindSpeed * WindSpeed * WindSpeed * WindSpeed * 0 +
                (7.5504309 * 10^(-4)) * 0 * 0 +
                (-5.65095215 * 10^(-5)) * TempC * 0 * 0 +
                (-4.52166564 * 10^(-7)) * TempC * TempC * 0 * 0 +
                (2.46688878 * 10^(-8)) * TempC * TempC * TempC * 0 * 0 +
                (2.42674348 * 10^(-10)) * TempC * TempC * TempC * TempC * 0 * 0 +
                (1.5454725 * 10^(-4)) * WindSpeed * 0 * 0 +
                (5.2411097 * 10^(-6)) * TempC * WindSpeed * 0 * 0 +
                (-8.75874982 * 10^(-8)) * TempC * TempC * WindSpeed * 0 * 0 +
                (-1.50743064 * 10^(-9)) * TempC * TempC * TempC * WindSpeed * 0 * 0 +
                (-1.56236307 * 10^(-5)) * WindSpeed * WindSpeed * 0 * 0 +
                (-1.33895614 * 10^(-7)) * TempC * WindSpeed * WindSpeed * 0 * 0 +
                (2.49709824 * 10^(-9)) * TempC * TempC * WindSpeed * WindSpeed * 0 * 0 +
                (6.51711721 * 10^(-7)) * WindSpeed * WindSpeed * WindSpeed * 0 * 0 +
                (1.94960053 * 10^(-9)) * TempC * WindSpeed * WindSpeed * WindSpeed * 0 * 0 +
                (-1.00361113 * 10^(-8)) * WindSpeed * WindSpeed * WindSpeed * WindSpeed * 0 * 0 +
                (-1.21206673 * 10^(-5)) * 0 * 0 * 0 + (-2.1820366 * 10^(-7)) * TempC * 0 * 0 * 0 +
                (7.51269482 * 10^(-9)) * TempC * TempC * 0 * 0 * 0 +
                (9.79063848 * 10^(-11)) * TempC * TempC * TempC * 0 * 0 * 0 +
                (1.25006734 * 10^(-6)) * WindSpeed * 0 * 0 * 0 +
                (-1.81584736 * 10^(-9)) * TempC * WindSpeed * 0 * 0 * 0 +
                (-3.52197671 * 10^(-10)) * TempC * TempC * WindSpeed * 0 * 0 * 0 +
                (-3.3651463 * 10^(-8)) * WindSpeed * WindSpeed * 0 * 0 * 0 +
                (1.35908359 * 10^(-10)) * TempC * WindSpeed * WindSpeed * 0 * 0 * 0 +
                (4.1703262 * 10^(-10)) * WindSpeed * WindSpeed * WindSpeed * 0 * 0 * 0)
crt <- mutate(crt, f4=(-1.30369025 * 10^(-9)) * 0 * 0 * 0 * 0 +
                (4.13908461 * 10^(-10)) * TempC * 0 * 0 * 0 * 0 +
                (9.22652254 * 10^(-12)) * TempC * TempC * 0 * 0 * 0 * 0 +
                (-5.08220384 * 10^(-9)) * WindSpeed * 0 * 0 * 0 * 0 +
                (-2.24730961 * 10^(-11)) * TempC * WindSpeed * 0 * 0 * 0 * 0 +
                (1.17139133 * 10^(-10)) * WindSpeed * WindSpeed * 0 * 0 * 0 * 0 +
                (6.62154879 * 10^(-10)) * 0 * 0 * 0 * 0 * 0 +
                (4.0386326 * 10^(-13)) * TempC * 0 * 0 * 0 * 0 * 0 +
                (1.95087203 * 10^(-12)) * WindSpeed * 0 * 0 * 0 * 0 * 0 +
                (-4.73602469 * 10^(-12)) * 0 * 0 * 0 * 0 * 0 * 0 +
                (5.12733497) * Vap +
                (-0.312788561) * TempC * Vap +
                (-0.0196701861) * TempC * TempC * Vap +
                (9.9969087 * 10^(-4)) * TempC * TempC * TempC * Vap + 
                (9.51738512 * 10^(-6)) * TempC * TempC * TempC * TempC * Vap +
                (-4.66426341 * 10^(-7)) * TempC * TempC * TempC * TempC * TempC * Vap +
                (0.548050612) * WindSpeed * Vap +
                (-0.00330552823) * TempC * WindSpeed * Vap +
                (-0.0016411944) * TempC * TempC * WindSpeed * Vap +
                (-5.16670694 * 10^(-6)) * TempC * TempC * TempC * WindSpeed * Vap +
                (9.52692432 * 10^(-7)) * TempC * TempC * TempC * TempC * WindSpeed * Vap +
                (-0.0429223622) * WindSpeed * WindSpeed * Vap +
                (0.00500845667) * TempC * WindSpeed * WindSpeed * Vap +
                (1.00601257 * 10^(-6)) * TempC * TempC * WindSpeed * WindSpeed * Vap +
                (-1.81748644 * 10^(-6)) * TempC * TempC * TempC * WindSpeed * WindSpeed * Vap +
                (-1.25813502 * 10^(-3)) * WindSpeed * WindSpeed * WindSpeed * Vap +
                (-1.79330391 * 10^(-4)) * TempC * WindSpeed * WindSpeed * WindSpeed * Vap +
                (2.34994441 * 10^(-6)) * TempC * TempC * WindSpeed * WindSpeed * WindSpeed * Vap)
crt <- mutate(crt, f5=(1.29735808 * 10^(-4)) * WindSpeed * WindSpeed * WindSpeed * WindSpeed * Vap +
                (1.2906487 * 10^(-6)) * TempC * WindSpeed * WindSpeed * WindSpeed * WindSpeed * Vap +
                (-2.28558686 * 10^(-6)) * WindSpeed * WindSpeed * WindSpeed * WindSpeed * WindSpeed * Vap +
                (-0.0369476348) * 0 * Vap + (0.00162325322) * TempC * 0 * Vap +
                (-3.1427968 * 10^(-5)) * TempC * TempC * 0 * Vap +
                (2.59835559 * 10^(-6)) * TempC * TempC * TempC * 0 * Vap +
                (-4.77136523 * 10^(-8)) * TempC * TempC * TempC * TempC * 0 * Vap +
                (8.6420339 * 10^(-3)) * WindSpeed * 0 * Vap +
                (-6.87405181 * 10^(-4)) * TempC * WindSpeed * 0 * Vap +
                (-9.13863872 * 10^(-6)) * TempC * TempC * WindSpeed * 0 * Vap +
                (5.15916806 * 10^(-7)) * TempC * TempC * TempC * WindSpeed * 0 * Vap +
                (-3.59217476 * 10^(-5)) * WindSpeed * WindSpeed * 0 * Vap +
                (3.28696511 * 10^(-5)) * TempC * WindSpeed * WindSpeed * 0 * Vap +
                (-7.10542454 * 10^(-7)) * TempC * TempC * WindSpeed * WindSpeed * 0 * Vap +
                (-1.243823 * 10^(-5)) * WindSpeed * WindSpeed * WindSpeed * 0 * Vap +
                (-7.385844 * 10^(-9)) * TempC * WindSpeed * WindSpeed * WindSpeed * 0 * Vap +
                (2.20609296 * 10^(-7)) * WindSpeed * WindSpeed * WindSpeed * WindSpeed * 0 * Vap +
                (-7.3246918 * 10^(-4)) * 0 * 0 * Vap +
                (-1.87381964 * 10^(-5)) * TempC * 0 * 0 * Vap +
                (4.80925239 * 10^(-6)) * TempC * TempC * 0 * 0 * Vap +
                (-8.7549204 * 10^(-8)) * TempC * TempC * TempC * 0 * 0 * Vap +
                (2.7786293 * 10^(-5)) * WindSpeed * 0 * 0 * Vap +
                (-5.06004592 * 10^(-6)) * TempC * WindSpeed * 0 * 0 * Vap +
                (1.14325367 * 10^(-7)) * TempC * TempC * WindSpeed * 0 * 0 * Vap)
crt <- mutate(crt, f6=(2.53016723 * 10^(-6)) * WindSpeed * WindSpeed * 0 * 0 * Vap +
                (-1.72857035 * 10^(-8)) * TempC * WindSpeed * WindSpeed * 0 * 0 * Vap +
                (-3.95079398 * 10^(-8)) * WindSpeed * WindSpeed * WindSpeed * 0 * 0 * Vap +
                (-3.59413173 * 10^(-7)) * 0 * 0 * 0 * Vap +
                (7.04388046 * 10^(-7)) * TempC * 0 * 0 * 0 * Vap +
                (-1.89309167 * 10^(-8)) * TempC * TempC * 0 * 0 * 0 * Vap +
                (-4.79768731 * 10^(-7)) * WindSpeed * 0 * 0 * 0 * Vap +
                (7.96079978 * 10^(-9)) * TempC * WindSpeed * 0 * 0 * 0 * Vap +
                (1.62897058 * 10^(-9)) * WindSpeed * WindSpeed * 0 * 0 * 0 * Vap +
                (3.94367674 * 10^(-8)) * 0 * 0 * 0 * 0 * Vap +
                (-1.18566247 * 10^(-9)) * TempC * 0 * 0 * 0 * 0 * Vap +
                (3.34678041 * 10^(-10)) * WindSpeed * 0 * 0 * 0 * 0 * Vap +
                (-1.15606447 * 10^(-10)) * 0 * 0 * 0 * 0 * 0 * Vap +
                (-2.80626406) * Vap * Vap +
                (0.548712484) * TempC * Vap * Vap +
                (-0.0039942841) * TempC * TempC * Vap * Vap +
                (-9.54009191 * 10^(-4)) * TempC * TempC * TempC * Vap * Vap +
                (1.93090978 * 10^(-5)) * TempC * TempC * TempC * TempC * Vap * Vap +
                (-0.308806365) * WindSpeed * Vap * Vap +
                (0.0116952364) * TempC * WindSpeed * Vap * Vap +
                (4.95271903 * 10^(-4)) * TempC * TempC * WindSpeed * Vap * Vap +
                (-1.90710882 * 10^(-5)) * TempC * TempC * TempC * WindSpeed * Vap * Vap +
                (0.00210787756) * WindSpeed * WindSpeed * Vap * Vap +
                (-6.98445738 * 10^(-4)) * TempC * WindSpeed * WindSpeed * Vap * Vap +
                (2.30109073 * 10^(-5)) * TempC * TempC * WindSpeed * WindSpeed * Vap * Vap +
                (4.1785659 * 10^(-4)) * WindSpeed * WindSpeed * WindSpeed * Vap * Vap)
crt <- mutate(crt, f7=(-1.27043871 * 10^(-5)) * TempC * WindSpeed * WindSpeed * WindSpeed * Vap * Vap +
                (-3.04620472 * 10^(-6)) * WindSpeed * WindSpeed * WindSpeed * WindSpeed * Vap * Vap +
                (0.0514507424) * 0 * Vap * Vap +
                (-0.00432510997) * TempC * 0 * Vap * Vap +
                (8.99281156 * 10^(-5)) * TempC * TempC * 0 * Vap * Vap +
                (-7.14663943 * 10^(-7)) * TempC * TempC * TempC * 0 * Vap * Vap +
                (-2.66016305 * 10^(-4)) * WindSpeed * 0 * Vap * Vap +
                (2.63789586 * 10^(-4)) * TempC * WindSpeed * 0 * Vap * Vap +
                (-7.01199003 * 10^(-6)) * TempC * TempC * WindSpeed * 0 * Vap * Vap +
                (-1.06823306 * 10^(-4)) * WindSpeed * WindSpeed * 0 * Vap * Vap +
                (3.61341136 * 10^(-6)) * TempC * WindSpeed * WindSpeed * 0 * Vap * Vap +
                (2.29748967 * 10^(-7)) * WindSpeed * WindSpeed * WindSpeed * 0 * Vap * Vap +
                (3.04788893 * 10^(-4)) * 0 * 0 * Vap * Vap +
                (-6.42070836 * 10^(-5)) * TempC * 0 * 0 * Vap * Vap +
                (1.16257971 * 10^(-6)) * TempC * TempC * 0 * 0 * Vap * Vap +
                (7.68023384 * 10^(-6)) * WindSpeed * 0 * 0 * Vap * Vap +
                (-5.47446896 * 10^(-7)) * TempC * WindSpeed * 0 * 0 * Vap * Vap +
                (-3.5993791 * 10^(-8)) * WindSpeed * WindSpeed * 0 * 0 * Vap * Vap +
                (-4.36497725 * 10^(-6)) * 0 * 0 * 0 * Vap * Vap +
                (1.68737969 * 10^(-7)) * TempC * 0 * 0 * 0 * Vap * Vap +
                (2.67489271 * 10^(-8)) * WindSpeed * 0 * 0 * 0 * Vap * Vap +
                (3.23926897 * 10^(-9)) * 0 * 0 * 0 * 0 * Vap * Vap +
                (-0.0353874123) * Vap * Vap * Vap +
                (-0.22120119) * TempC * Vap * Vap * Vap +
                (0.0155126038) * TempC * TempC * Vap * Vap * Vap)
crt <- mutate(crt, f8=(-2.63917279 * 10^(-4)) * TempC * TempC * TempC * Vap * Vap * Vap +
                (0.0453433455) * WindSpeed * Vap * Vap * Vap +
                (-0.00432943862) * TempC * WindSpeed * Vap * Vap * Vap +
                (1.45389826 * 10^(-4)) * TempC * TempC * WindSpeed * Vap * Vap * Vap +
                (2.1750861 * 10^(-4)) * WindSpeed * WindSpeed * Vap * Vap * Vap +
                (-6.66724702 * 10^(-5)) * TempC * WindSpeed * WindSpeed * Vap * Vap * Vap +
                (3.3321714 * 10^(-5)) * WindSpeed * WindSpeed * WindSpeed * Vap * Vap * Vap +
                (-0.00226921615) * 0 * Vap * Vap * Vap +
                (3.80261982 * 10^(-4)) * TempC * 0 * Vap * Vap * Vap +
                (-5.45314314 * 10^(-9)) * TempC * TempC * 0 * Vap * Vap * Vap +
                (-7.96355448 * 10^(-4)) * WindSpeed * 0 * Vap * Vap * Vap +
                (2.53458034 * 10^(-5)) * TempC * WindSpeed * 0 * Vap * Vap * Vap +
                (-6.31223658 * 10^(-6)) * WindSpeed * WindSpeed * 0 * Vap * Vap * Vap +
                (3.02122035 * 10^(-4)) * 0 * 0 * Vap * Vap * Vap +
                (-4.77403547 * 10^(-6)) * TempC * 0 * 0 * Vap * Vap * Vap +
                (1.73825715 * 10^(-6)) * WindSpeed * 0 * 0 * Vap * Vap * Vap +
                (-4.09087898 * 10^(-7)) * 0 * 0 * 0 * Vap * Vap * Vap +
                (0.614155345) * Vap * Vap * Vap * Vap +
                (-0.0616755931) * TempC * Vap * Vap * Vap * Vap +
                (0.00133374846) * TempC * TempC * Vap * Vap * Vap * Vap +
                (0.00355375387) * WindSpeed * Vap * Vap * Vap * Vap +
                (-5.13027851 * 10^(-4)) * TempC * WindSpeed * Vap * Vap * Vap * Vap +
                (1.02449757 * 10^(-4)) * WindSpeed * WindSpeed * Vap * Vap * Vap * Vap +
                (-0.00148526421) * 0 * Vap * Vap * Vap * Vap)
crt <- mutate(crt, f9=(-4.11469183 * 10^(-5)) * TempC * 0 * Vap * Vap * Vap * Vap +
                (-6.80434415 * 10^(-6)) * WindSpeed * 0 * Vap * Vap * Vap * Vap +
                (-9.77675906 * 10^(-6)) * 0 * 0 * Vap * Vap * Vap * Vap +
                (0.0882773108) * Vap * Vap * Vap * Vap * Vap +
                (-0.00301859306) * TempC * Vap * Vap * Vap * Vap * Vap +
                (0.00104452989) * WindSpeed * Vap * Vap * Vap * Vap * Vap +
                (2.47090539 * 10^(-4)) * 0 * Vap * Vap * Vap * Vap * Vap +
                (0.00148348065) * Vap * Vap * Vap * Vap * Vap * Vap)
crt <<- mutate(crt, UTCI=(f1+f2+f3+f4+f5+f6+f7+f8+f9)*(9/5)+32)


ot <- read.csv(file="~/dropbox/rh1/hidoe/outdoor-csv.csv", sep=",") #outdoor temperature
names(ot) <- c("DateTime", "OutdoorTemp", "OutdoorUTCI")
ot$Date <- as.Date(ot$DateTime, format="%m/%d/%y")
ot$Time <- format(strptime(ot$DateTime, format="%m/%d/%y %H:%M"), format="%H:%M")
ot$DateTime <- NULL
ot$Month <- format(ot$Date, "%B")
ot <- merge(ot, sdates, by="Date", all.x=FALSE) #restrict to school days
ot <- merge(ot, shours, by="Time", all.x=FALSE) #restrict to school hours
ot <<- ot

write.csv(crt, file="~/dropbox/rh1/hidoe/final-csv/all-classroom-sensor.csv", row.names=FALSE, na="")
write.csv(ot, file="~/dropbox/rh1/hidoe/final-csv/all-outdoor-sensor.csv", row.names=FALSE, na="")