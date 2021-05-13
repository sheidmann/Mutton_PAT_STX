# 1_processing.R

# Sarah Heidmann
# Created 9 Oct 2018
# Last modified 13 May 2021

# Summary:
# Data inputs:
#     - mutton snapper acoustic data: 1_processed_JKM (raw data)
# Actions:
#     - removes detections at other arrays
#     - splits the date into multiple columns
#     - classifies detections as DAFM
# Data exports:
#     - 2_processed_SLH (formatted raw data)

# Load libraries
library(tidyverse)
library(lubridate)
library(sp)

##### Import data #####
sourcePath <- "data/1_processed_JKM/" # source is modifiable
filenames <- list.files(sourcePath) # extract the filenames

importMSX <- function(filename){
   # Read the file
   data <- read_csv(paste0(sourcePath, filename), 
                    col_types = cols(station = col_character(),
                                     transmitter = col_character())) %>%
      # Keep only columns of interest
      select("station","transmitter","detection_time_ast","lat_nad83","long_nad83") %>%
      # Set time zone
      mutate(detection_time_ast = force_tz(detection_time_ast, "America/Virgin")) %>%
      # Sort by time
      arrange(detection_time_ast)
   return(data)
}

msx_ls <- lapply(filenames, importMSX)
names(msx_ls) <- gsub(".csv", "", filenames)
msx_ls

sinkPath <- "data/2_processed_SLH/"


##### Delete detections at other arrays #####
# Load list of active STX receivers
statmaster <- read_csv("data/otherdata/uvi_msxwb_station_master_2017.csv",
                       col_types = cols(station = col_character()))

# Look at migratory detections
exportMigrate <- function(dataset, export = "FALSE"){
   trans <- dataset$transmitter[1]
   dat <- filter(dataset, !(station %in% statmaster$station))
   if(nrow(dat)>0 && export == "TRUE"){
      write_excel_csv(dat,paste0("data/migration/",trans,".csv"))
   }
   return(dat)
}
migrate_ls <- lapply(msx_ls, exportMigrate, export = "TRUE")

# Keep only detections on West Bank list
delStat <- function(dataset){
   dat <- filter(dataset, station %in% statmaster$station)
   return(dat)
}
msx_ls_proc <- lapply(msx_ls, delStat)

##### Split date #####
splitDate <- function(dataset){
   dataset <- dataset %>%
      mutate(year = year(detection_time_ast),
             date = date(detection_time_ast), 
             hour = hour(detection_time_ast), 
             min = minute(detection_time_ast),
             sec = second(detection_time_ast))
   print("Date split complete")
   return(dataset)
}
msx_ls_proc <- lapply(msx_ls_proc, splitDate)

##### Calculate time differences #####
calcTimeDiff <- function(dataset){
   # Subtract time
   dataset <- dataset %>%
      mutate(timediff= c(as.numeric(tail(detection_time_ast, -1)) -
                            as.numeric(head(detection_time_ast, -1)), NA))
   print("Time difference calculation complete")
   return(dataset)
}
msx_ls_proc <- lapply(msx_ls_proc, calcTimeDiff)

##### Add DAFM classification #####
# Find moon date in same month-year and set DAFM
# if DAFM < -7, find moon date in previous month and set DAFM 
moontab <- read_csv("data/otherdata/sttmoontable_14_18.csv") %>%
   mutate(datetimeUTC= as_datetime(paste(month, day, year, timeUTC, sep = " "),
          tz="UTC", format = "%b %d %Y %H:%M:%S")) %>%
   mutate(datetimeAST= with_tz(datetimeUTC, tzone="America/Virgin")) %>%
   mutate(date = as_date(datetimeAST))

# This is a slightly different algorithm than the original time I classified DAFM.
# The numbers should still be the same.
addDAFM <- function(dataset){
   dataset <- add_column(dataset, DAFM = as.integer(NA))
   for (i in 1:nrow(dataset)){
      obsDate <- dataset$date[i] # extract detection date
      possibles <- as.numeric(obsDate - moontab$date) # get DAFM for all moons
      closepos <- min(possibles[possibles>=0]) # find the smallest positive DAFM
      closeneg <- max(possibles[possibles<0]) # find the smallest negative DAFM
      # If the negative is less than 10 days before, use it. Otherwise, positive.
      if(closeneg > -10){
         dataset$DAFM[i] <- closeneg
      }else{
         dataset$DAFM[i] <- closepos
      }
   }
   print("DAFM classification complete")
   return(dataset)
}
msx_ls_proc <- lapply(msx_ls_proc, addDAFM)

##### Classify as MSSCA in/out #####
msscaInOut <- function(dataset){
   dat <- left_join(dataset, statmaster[c("station","mssca")], by="station")
   return(dat)
}
msx_ls_proc <- lapply(msx_ls_proc, msscaInOut)

##### Project coordinates #####
# add UTM20 positions (x_UTM20 and y_UTM20) so don’t have to project every time
projUTM20 <- function(dataset){
   dataset_s <- SpatialPoints(coords = dplyr::select(dataset, long_nad83, lat_nad83),
                              proj4string = CRS("+proj=longlat +datum=NAD83"))
   #project (UTM Zone 20N)
   dataset_sp <- spTransform(dataset_s, CRS("+proj=utm +zone=20 +datum=NAD83 +units=m"))
   coords <- as_tibble(dataset_sp) %>%
      rename(x_UTM20N=long_nad83, y_UTM20N=lat_nad83)
   dataset <- bind_cols(dataset, coords)
   return(dataset)
}
msx_ls_proc <- lapply(msx_ls_proc, projUTM20)

##### Export #####
for(trans in names(msx_ls_proc)){
   write_excel_csv(msx_ls_proc[[trans]], paste0(sinkPath, trans,".csv"))
}



