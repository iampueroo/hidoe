### Roof Color ###
KP05 <- c("Kaimiloa - P05") #dark gray
KP10 <- c("Kaimiloa - P10") #black

CD302 <- c("Campbell - D302") #light gray
CD305 <- c("Campbell - D305")
CD309 <- c("Campbell - D309")
CD312 <- c("Campbell - D312")
CO307 <- c("Campbell - O307") #black
CO310 <- c("Campbell - O310")
CO302 <- c("Campbell - O302")
CO304 <- c("Campbell - O304")

lightgray <- c(CD302, CD305, CD309, CD312)
black     <- c(CO307, CO310, CO302, CO304)

### Landscape ###
II101 <- c("Ilima - I101") #asphalt
II104 <- c("Ilima - I104")
ID101 <- c("Ilima - D101") #grass
IC107 <- c("Ilima - C107")

CO102 <- c("Campbell - O102") #concrete, asphalt
CD109 <- c("Campbell - D109") #dirt

CO107 <- c("Campbell - O107") #concrete, dirt
CD105 <- c("Campbell - D105") #dirt

CO104 <- c("Campbell - O104") #concrete, grass
CD112 <- c("Campbell - D112") #dirt

ID102 <- c("Ilima - D102") #concrete, grass
IC106 <- c("Ilima - C106") #grass

asphalt <- c(II101, II104) 
grass   <- c(ID101, IC107)

### Floor
IC106 <- c("Ilima - C106") #ground
IC107 <- c("Ilima - C107")
ID101 <- c("Ilima - D101")
ID102 <- c("Ilima - D102")
KE103 <- c("Kaimiloa - E103")
KE106 <- c("Kaimiloa - E106")
KF105 <- c("Kaimiloa - F105")
KF106 <- c("Kaimiloa - F106")
IC206 <- c("Ilima - C206") #top
IC207 <- c("Ilima - C207")
ID201 <- c("Ilima - D201")
ID202 <- c("Ilima - D202")
KE203 <- c("Kaimiloa - E203")
KE206 <- c("Kaimiloa - E206")
KF213 <- c("Kaimiloa - F213")
KF214 <- c("Kaimiloa - F214")

CD102 <- c("Campbell - D102") #ground
CD105 <- c("Campbell - D105")
CD109 <- c("Campbell - D109")
CD112 <- c("Campbell - D112")
CO107 <- c("Campbell - O107")
CO102 <- c("Campbell - O102")
CO104 <- c("Campbell - O104")
CD202 <- c("Campbell - D202") #middle
CD205 <- c("Campbell - D205")
CD209 <- c("Campbell - D209")
CD212 <- c("Campbell - D212")
CO207 <- c("Campbell - O207")
CO202 <- c("Campbell - O202")
CO204 <- c("Campbell - O204")
CD302 <- c("Campbell - D302") #top
CD305 <- c("Campbell - D305")
CD309 <- c("Campbell - D309")
CD312 <- c("Campbell - D312")
CO310 <- c("Campbell - O310")
CO302 <- c("Campbell - O302")
CO304 <- c("Campbell - O304")

ground1 <- c(IC106, IC107, ID101, KE103, KE106, KF105, KF106)
top1    <- c(IC206, IC207, ID201, KE203, KE206, KF213, KF214)

ground2 <- c(CD102, CD105, CD109, CD112, CO107, CO102, CO104)
middle2 <- c(CD202, CD205, CD209, CD212, CO207, CO202, CO204)
top2    <- c(CD302, CD305, CD309, CD312, CO310, CO302, CO304)

### Overhang ###
CD105 <- c("Cambell - D105") #no overhang
CD205 <- c("Cambell - D205")
CO105 <- c("Cambell - O105")
CO205 <- c("Cambell - O205")
CD112 <- c("Cambell - D112") #7'-9'
CD212 <- c("Cambell - D212")
CO102 <- c("Cambell - O102")
CO202 <- c("Cambell - O202")

CD305 <- c("Campbell - D305") #1'-3'
CO307 <- c("Campbell - O307") 
CD312 <- c("Campbell - D312") #10'-12'
CO302 <- c("Campbell - O302") 

CJ109 <- c("Campbell - J109") #4'-6'
CJ105 <- c("Campbell - J105") #13'+

CD202 <- c("Campbell - D202") #no overhang
CD205 <- c("Campbell - D205")
CO207 <- c("Campbell - O207")
CO205 <- c("Campbell - O205")
CD302 <- c("Campbell - D302") #1'-3'
CD305 <- c("Campbell - D305")
CO310 <- c("Campbell - O310")
CO307 <- c("Campbell - O307")

CO204 <- c("Campbell - O204") #7'-9'
CO202 <- c("Campbell - O202")
CD209 <- c("Campbell - D209")
CD212 <- c("Campbell - D212")
CO304 <- c("Campbell - O304") #10'-12'
CO302 <- c("Campbell - O302")
CD309 <- c("Campbell - D309")
CD312 <- c("Campbell - D312")

zero1  <- c(CD105, CD205, CO105, CO205)
seven1 <- c(CD112, CD212, CO102, CO202)
            
one1 <- c(CD305, CO307)
ten1 <- c(CD312, CO302) 

zero2 <- c(CD202, CD205, CO207, CO205)
one2  <- c(CD302, CD305, CO310, CO307)

seven2 <- c(CO204, CO202, CD209, CD212)
ten2   <- c(CO304, CO302, CD309, CD312)

smdiffseven <- c(zero1, one1, CJ109)
lgdiffseven <- c(seven1, ten1, CJ105)

smdiffone <- c(zero2, seven2)
lgdiffone <- c(one2, ten2)


### Orientation ###
KF214 <- c("Kaimiloa - F214") #NE
KE206 <- c("Kaimiloa - E206") #NE,SE,SW

II103 <- c("Ilima - I103") #SE,NW
II101 <- c("Ilima - I101") #SE,NW,NE
II104 <- c("Ilima - I104") #SE,SW,NE

CD105 <- c("Campbell - D105") #NE,SW
CD112 <- c("Campbell - D112")
CD205 <- c("Campbell - D205")
CD212 <- c("Campbell - D212")
CD305 <- c("Campbell - D305")
CD312 <- c("Campbell - D312")
CO104 <- c("Campbell - O104")
CO204 <- c("Campbell - O204")
CO207 <- c("Campbell - O207")
CO304 <- c("Campbell - O304")
CO310 <- c("Campbell - O310")
CD102 <- c("Campbell - D102") #SE,NW
CD109 <- c("Campbell - D109")
CD202 <- c("Campbell - D202")
CD209 <- c("Campbell - D209")
CD302 <- c("Campbell - D302")
CD309 <- c("Campbell - D309")
CO102 <- c("Campbell - O102")
CO202 <- c("Campbell - O202")
CO205 <- c("Campbell - O205")
CO302 <- c("Campbell - O302")
CO307 <- c("Campbell - O307")

KP02 <- c("Kaimiloa - P02") #NE,SE,SW
KP10 <- c("Kaimiloa - P10") #NE,SE,SW,NW

nesw <- c(CD105, CD112, CD205, CD212, CD305, CD312, CO104, CO204, CO207, CO304, CO310)
senw <- c(CD102, CD109, CD202, CD209, CD302, CD309, CO102, CO202, CO205, CO302, CO307)