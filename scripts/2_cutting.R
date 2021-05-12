# 2_cutting.R

# Sarah Heidmann
# Created 14 Sept 2017
# Last modified 12 May 2021

# Summary:
# Data inputs:
#     - mutton snapper acoustic data: 2_processed_SLH (from 1_processing.R)
# Actions:
#     - cuts the day of tagging
#     - removes fish with any less than 5 detections or suspect activity
# Data exports:
#     - 3_cut (formatted raw data)

# Load libraries
library(tidyverse)
library(lubridate)

##### Import data #####
sourcePath <- "data/2_processed_SLH/" # source is modifiable
filenames <- list.files(sourcePath) # extract the filenames

importMSX <- function(filename){
   # Read the file
   dat <- read_csv(paste0(sourcePath, filename),
                   col_types = cols(station = col_character(),
                                    detection_time_ast=col_datetime(format="%Y/%m/%d %H:%M:%S"))) %>%
      # Change time zone
      mutate(detection_time_ast = force_tz(detection_time_ast, 
                                           "America/Virgin"))
   # Return the dataset
   return(dat)
}

msx_ls <- lapply(filenames, importMSX)
names(msx_ls) <- gsub(".csv", "", filenames)
msx_ls

sinkPath <- "data/3_cut/"

# Read the transmitter master sheet
transmaster <- read_csv("data/otherdata/uvi_transmitter_master_2017.csv",
                        col_types = cols(release_date = col_date(format="%m/%d/%y")))

##### Determine usable data #####
# List the 24 usable tags (from the 48 raw), determined in Excel
msxUseList <- c("A69-1601-24991", "A69-1601-24992", "A69-1601-24993",
                "A69-1601-24995", "A69-1601-24996", "A69-1601-24997",
                "A69-1601-24998", "A69-1601-25000", "A69-1601-25004",
                "A69-1601-25006", "A69-1601-25007", "A69-1601-25010",
                "A69-1601-25018", "A69-1601-25019", "A69-1601-25020", 
                "A69-1601-59283", "A69-1601-59284", "A69-1601-59287", 
                "A69-1601-59292", "A69-1601-59295", "A69-1601-59296", 
                "A69-1601-59301", "A69-9002-10832", "A69-9002-10833")
msx_ls_cut <- msx_ls[names(msx_ls) %in% msxUseList]

##### Trim the data #####
trimDay1 <- function(dataset){
   # Cut day of tagging
   # AKA data start at the midnight after tagging
   trans <- dataset$transmitter[1]
   start <- filter(transmaster,transmitter == trans)$release_date
   dataset <- filter(dataset, date > start)
   return(dataset)
}
msx_ls_cut <- lapply(msx_ls_cut, trimDay1)

##### Export the data #####
for(dataset in msx_ls_cut){
   trans <- dataset$transmitter[1] # dynamically name the file
   write_excel_csv(dataset, paste0(sinkPath, trans, ".csv"))
}
