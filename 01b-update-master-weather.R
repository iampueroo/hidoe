updateweather<- function() {
  # should always be most updated -- since cumulative and in one file
  # but for test, set it up manually
  library(readxl)
 
  filepaths <- as.list(list.files("~/BOX Sync/HIDOE-Data-Repository/Raw/Weather-Station", pattern = "\\.xlsx$", full.names = TRUE))
  
  dfs = list() #initialize list of files
  for (i in 1:length(filepaths)) {
    f <- read_excel(filepaths[[i]])
    f <- f[,c(1:5,7,9)]
    colnames(f) <- c("WS_ID", "School", "Date_HST", "Time_HST", "Temp_F", "RH", "Windspeed")
      
    assign(paste0("f",i), f)
    dfs[[i]] <- get(paste0("f",i))
  } 
  w <- do.call(rbind, dfs)
  
  write.csv(w, file=paste0("~/BOX Sync/HIDOE-Data-Repository/Raw/Weather-Station/", gsub("-","",Sys.Date()), "-master-weather.csv"), row.names=FALSE, na="")
  
}