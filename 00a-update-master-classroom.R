updateclassroom <- function(schools) {
  library(readr)
  library(stringr)
  library(data.table)
 
  #Pohakea El - A5 has two sensors so ok 
  #Campbell High - O205 two sensors so ok
  #Kaimiloa El - P14 has one overlapping datetime of about ~0.02 degree difference, keep observation from later dataset
  
  all.filepaths <- as.list(list.files("~/BOX Sync/HIDOE-Data-Repository/Raw/Classroom-Sensor/csv", full.names = TRUE))
  new.filepaths <- all.filepaths[sapply(all.filepaths, function(x) basename(x) %in% schools)]
  #new.filepaths <- all.filepaths
  
  for (i in 1:length(new.filepaths)) {
    files <- list.files(new.filepaths[[i]], full.names = TRUE) 
    school <- basename(new.filepaths[[i]])
    
    dfs = list() #initialize list of files
    for (j in 1:length(files)) {
      f <- read_csv(files[j], skip=1)
      sn <- sub(".*_(.*)_.*.csv", "\\1", files[j]) #serial number
      rn <- sub(".*_.*_(.*).csv", "\\1", files[j]) #room number
      date <- sub("(.*)_.*_.*.csv", "\\1", files[j]) #date of retrieval
      alias <- paste(school, rn, sep=" - ")
      
      f <- na.omit(f[,c(2:4)])
      colnames(f) <- c("Datetime_HST", "Temp_F", "RH")
      f$Serial <- as.numeric(sn)
      f$Alias <- alias
      f$File <- date
      
      assign(paste0("f",j), f)
      dfs[[j]] <- get(paste0("f",j))
    }
    s <- do.call(rbind, dfs)
    s <- s[!duplicated(s[1:5]), ]
    if (nrow(unique(s[c("Datetime_HST", "Alias")])) < nrow(s)) {
      print (paste(school, "data not unique by datetime and alias!!!"))
      s$date <- as.numeric(str_sub(s$File, -8, -1)) # get date of data retrieval
      dups <- s[duplicated(s[,c("Datetime_HST", "Alias")]),]
      print(unique(dups$Alias))
      
      #keep duplicate observation with later date of data retrieval....
      s <- s[order(s$Datetime_HST, s$Alias, -s$date, s$Serial, s$Temp_F, s$RH),]
      s.DT <- data.table(s, key=c("Datetime_HST", "Alias", "Serial")) 
      s <- as.data.frame(s.DT[, head(.SD, 1), by=key(s.DT)])
      s$date <- NULL
    }
    
    write.csv(s, file=paste0("~/BOX Sync/HIDOE-Data-Repository/Processed/Classroom-Sensor/",tolower(gsub(" ", "-", school)),".csv"), row.names=FALSE, na="")
  } 
  
  setwd("~/BOX Sync/HIDOE-Data-Repository/Processed/Classroom-Sensor") 
  all.schools <- do.call(rbind,lapply(dir(), read_csv)) #read all school sensor data
  write.csv(all.schools, file=paste0("~/BOX Sync/HIDOE-Data-Repository/Processed/", gsub("-","",Sys.Date()), "-master-classroom.csv"), row.names=FALSE, na="")
  
}
