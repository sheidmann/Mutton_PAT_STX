# arrive_depart.R

# Sarah Heidmann
# Created 29 Aug 2022
# Last modified 29 Aug 2022

# Looking at when mutton arrive and depart the array over time
# Recreating Feeley et al. 2018 Fig 4

# Summary:
# Data inputs:
#     - STX mutton snapper acoustic data: 3_cut
# Actions:
#     - recreates Feeley et al. 2018 Fig 4
# Data exports:
#     - plot

# Load libraries
library(tidyverse)
library(lubridate)

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

##### Summarize by date and DAFM #####
reduceDays <- function(dataset){
   output <- dataset %>% 
      group_by(transmitter, date, DAFM) %>% 
      summarize(present = 1, .groups = "drop") %>% 
      mutate(moondate = date - DAFM) %>% 
      group_by(transmitter, moondate) %>% 
      summarize(arrival = min(DAFM), departure = max(DAFM), .groups="drop")
   return(output)
}
#test <- reduceDays(msx_ls[[1]])
DAFMrange_ls <- lapply(msx_ls, reduceDays)

DAFMrange <- bind_rows(DAFMrange_ls)

DAFMrangesum <- DAFMrange %>% 
   group_by(moondate) %>% 
   summarize(meanarrive = mean(arrival), meandepart = mean(departure),
             n = length(arrival), 
             sdarrive = sd(arrival), sddepart = sd(departure),
             .groups="drop") %>% 
   mutate(semarrive = sdarrive / sqrt(n), semdepart = sddepart / sqrt(n))

##### Make the plot #####
ggplot(data= DAFMrangesum) +
   geom_point(aes(x=moondate, y=meanarrive), 
              color = "black", shape = 16, size = 3) +
   geom_errorbar(aes(x=moondate, 
                     ymin = meanarrive - semarrive, ymax = meanarrive + semarrive)) +
   geom_errorbar(aes(x=moondate, 
                     ymin = meandepart - semdepart, ymax = meandepart + semdepart)) +
   geom_point(aes(x=moondate, y=meandepart), 
              color = "grey70", shape = 15, size = 3) +
   geom_hline(yintercept=0, linetype = "dashed") +
   scale_x_date(name = "Date", date_breaks = "3 months", date_labels = "%Y-%b") +
   scale_y_continuous(name = "Days After the Full Moon", 
                      limits =c(-2,12), breaks=seq(-2,12,2)) +
   theme(panel.background = element_blank(), axis.line = element_line())
ggsave("outputs/Arrive_Depart_FeeleyFig4.jpeg")
