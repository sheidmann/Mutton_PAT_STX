# 3_binning.R

# Sarah Heidmann
# Created 30 Jan 2018
# Last modified 13 May 2020

# Summary:
# Data inputs:
#     - STX mutton snapper acoustic data: 3_cut (exported from 2_cutting.R)
# Actions:
#     - bins detections into an average position every half-hour
# Data exports:
#     - 4_binned_30min

# Load libraries
library(tidyverse)
library(lubridate)
library(sp)

##### Import data #####
sourcePath <- "data/3_cut/" # determine data location
filenames <- list.files(sourcePath) # extract filenames
importMSX <- function(filename){
   # Read the file
   dat <- read_csv(paste0(sourcePath, filename),
                   col_types = cols(station = col_character(),
                                    detection_time_ast=col_datetime(format="%Y/%m/%d %H:%M:%S"),
                                    timediff = col_integer())) %>%
      # Change time zone
      mutate(detection_time_ast = force_tz(detection_time_ast, 
                                           "America/Virgin"))
   # Return the dataset
   return(dat)
}
msx_ls <- lapply(filenames, importMSX) # read all the files into a list
names(msx_ls) <- gsub(".csv", "", filenames) # take .csv out of the names


# Import the moon table
# Since classifications are not done for time periods with no detections
moontab <- read_csv("data/otherdata/sttmoontable_14_18.csv") %>%
   mutate(datetimeUTC= as_datetime(paste(month, day, year, timeUTC, sep = " "),
                                   tz="UTC", format = "%b %d %Y %H:%M:%S")) %>%
   mutate(datetimeAST= with_tz(datetimeUTC, tzone="America/Virgin")) %>%
   mutate(date = as_date(datetimeAST))

# Read in the coordinates of the closed area
msxClosed <- read_csv("data/otherdata/msx_closedarea.csv")
# Convert to SpatialPolygon
msxClosed_spoly <- SpatialPolygons(list(Polygons(list(Polygon(coords = 
                                                                 cbind(msxClosed$long, 
                                                                       msxClosed$lat))),1)),
                                   proj4string = CRS("+proj=longlat +datum=NAD83"))

# Specify the location for the exported data
sinkPath <- "data/4_binned_30min/"

##### Bin the data #####
# Create a function to set day/night for times with no detections
# Adapted from the version in 1_processing.R
addDAFM_i <- function(idate){
   possibles <- as.numeric(idate - moontab$date) # get DAFM for all moons
   closepos <- min(possibles[possibles>=0]) # find the smallest positive DAFM
   closeneg <- max(possibles[possibles<0]) # find the smallest negative DAFM
   # If the negative is less than 10 days before, use it. Otherwise, positive.
   if(closeneg > -10){
      dafm <- closeneg
   }else{
      dafm <- closepos
   }
   # Return the result
   return(dafm)
}
# Create a function to lapply
posavg <- function(dataset){
   # Extract transmitter
   trans <- dataset$transmitter[1]
   # Create a column to ID the bin
   dataset <- dataset %>%
      mutate(halfhour = as.numeric(ifelse(min<30,
                                          paste0(as.character(dataset$hour),".0"),
                                          paste0(as.character(dataset$hour),".5"))))
   # When was the first detection?
   startdate <- dataset$date[1]
   starthalfhour <- dataset$halfhour[1]
   # When was the last detection?
   enddate <- dataset$date[nrow(dataset)]
   endhalfhour <- dataset$halfhour[nrow(dataset)]
   # Bin the detections for half-hours when present
   datasetbin <- dataset %>% group_by(transmitter, year, date, halfhour) %>%
      summarize(No.stations = length(unique(station)), # number of unique stations
                No.detections = length(station), # total number of detections
                DAFM = min(DAFM), # DAFM should all be the same, so get one
                # average all position columns
                avg_lat = round(mean(lat_nad83),5), 
                avg_long = round(mean(long_nad83),5),
                avg_x = round(mean(x_UTM20N),2),
                avg_y = round(mean(y_UTM20N),2),
                .groups = "drop")
   # Classify in/out MSSCA
   binsp <- SpatialPoints(coords = cbind(datasetbin$avg_long,datasetbin$avg_lat),
                          proj4string = CRS("+proj=longlat +datum=NAD83"))
   datasetbin$mssca <- ifelse(is.na(over(binsp,msxClosed_spoly)),"out","in")
   # Add the zeros for half-hours when fish not present
   # For each date in the range of the dataset
   for(idate in range(dataset$date)[1]:range(dataset$date)[2]){
      idate <- as.Date(idate, origin = "1970-01-01") # make it usable
      # For each half-hour of the day
      for(ihalfhour in seq(0.0,23.5,0.5)){
         # Subset the data
         datasetsub <- filter(dataset,date==idate & halfhour==ihalfhour)
         # If no detections, add row with 0s and NAs
         if(nrow(datasetsub)==0){ # if no detections, put 0s and NAs
            dafm <- addDAFM_i(idate)
            datasetbin <- datasetbin %>%
               add_row(tibble_row(transmitter=trans, year = year(idate), 
                                  date=idate, halfhour= ihalfhour,
                                  No.stations=0, No.detections=0, 
                                  DAFM=dafm, 
                                  avg_lat=NA, avg_long=NA, avg_x=NA, avg_y=NA,
                                  mssca=NA))
         }
      }
   }
   # Delete detections before first hour of first day 
   # And after last hour on last day
   datasetbin <- filter(datasetbin, 
                        !(date==enddate & halfhour>endhalfhour) &
                           !(date==startdate & halfhour<starthalfhour)) %>%
      arrange(date, halfhour) # sort it
   # Return the result
   return(datasetbin)
}
msx_ls_bin <- lapply(msx_ls, posavg)

# lapply(msx_ls, nrow)
# lapply(msx_ls_bin, nrow)


##### Export #####
for(dataset in msx_ls_bin){
   trans <- dataset$transmitter[1] # dynamically name the file
   write_excel_csv(dataset, paste0(sinkPath, trans, ".csv"))
}

##### Summarize time in MSSCA #####
all <- na.omit(bind_rows(msx_ls_bin)) %>%
   mutate(mssca = as.factor(mssca))

# Summarize overall
summary(all$mssca)[1] / nrow(all) *100
# 50.3% of position averages were protected

# Summarize by year
all %>%
   group_by(year, mssca) %>%
   summarize(count = length(mssca), .groups="drop") %>%
   pivot_wider(id_cols = "year", names_from = "mssca", values_from = "count") %>%
   mutate(sum = `in` + out) %>%
   mutate(`in` = `in`/sum *100,
          out = out/sum *100)
# 67.1% of positions protected in 2015
# 33.4% of positions protected in 2016
# 14.5% of positions protected in 2017