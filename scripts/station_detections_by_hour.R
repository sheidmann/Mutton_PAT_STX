# station_detections_by_hour.R

# Sarah Heidmann
# Created 29 Aug 2022
# Last modified 13 Oct 2022

# Looking at what receivers muttons are visiting by hour of the day
# Recreating Feeley et al. 2018 Fig 6

# Summary:
# Data inputs:
#     - STX mutton snapper acoustic data: 3_cut
# Actions:
#     - recreates Feeley et al. 2018 Fig 6
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

##### Summarize detections by station and hour #####
SumStatHour <- function(dataset) {
   output <- dataset %>% 
      group_by(transmitter, station, mssca, date, hour) %>% 
      summarize(detections = length(hour), .groups="drop")
   return(output)
}
#test <- SumStatHour(msx_ls[[1]])
StatHour_ls <- lapply(msx_ls, SumStatHour)

# Combine and summarize across fish
StatHour <- bind_rows(StatHour_ls)

StatHourSum <- StatHour %>% 
   group_by(station, mssca, hour, date) %>% 
   summarize(sumdetections = sum(detections), .groups="drop_last") %>% 
   summarize(meandetections = mean(sumdetections), 
             ndays = length(sumdetections), sddetections = sd(sumdetections),
             .groups = "drop") %>% 
   mutate(semdetections = sddetections / sqrt(ndays))
   
##### Make the plot #####
# Exact replica of Feeley
ggplot(data= StatHourSum) +
   geom_line(aes(x=hour, y=meandetections, color = station)) +
   geom_errorbar(aes(x=hour, color = station,
                     ymin=meandetections - semdetections, 
                     ymax=meandetections + semdetections),
                 width = 0.2) +
   scale_x_continuous(name = "Hour",
                      breaks = seq(0,23,1), labels = seq(0,23,1)) +
   scale_y_continuous(name = "Mean Detections / Hour", expand=c(0,0)) +
   theme(panel.background = element_blank(), axis.line = element_line())
ggsave("outputs/StationsByHour_FeeleyFig6.jpeg")

# Color by MSSCA
ggplot(data= StatHourSum) +
   geom_line(aes(x=hour, y=meandetections, color = mssca, group=station)) +
   geom_errorbar(aes(x=hour, color = mssca,
                     ymin=meandetections - semdetections, 
                     ymax=meandetections + semdetections),
                 width = 0.2) +
   scale_x_continuous(name = "Hour",
                      breaks = seq(0,23,1), labels = seq(0,23,1)) +
   scale_y_continuous(name = "Mean Detections / Hour", expand=c(0,0)) +
   theme(panel.background = element_blank(), axis.line = element_line())
ggsave("outputs/StationsByHour_FeeleyFig6_mssca.jpeg")

##### Redo with only 2016 #####
SumStatHour_16 <- function(dataset) {
   output <- dataset %>% 
      filter(year==2016) %>% 
      #filter(DAFM>=4 & DAFM <=6) %>% 
      group_by(transmitter, station, mssca, date, hour) %>% 
      summarize(detections = length(hour), .groups="drop")
   return(output)
}
StatHour_16_ls <- lapply(msx_ls, SumStatHour_16)

# Combine and summarize across fish
StatHour_16 <- bind_rows(StatHour_16_ls)

StatHourSum_16 <- StatHour_16 %>% 
   group_by(station, mssca, hour, date) %>% 
   summarize(sumdetections = sum(detections), .groups="drop_last") %>% 
   summarize(meandetections = mean(sumdetections), 
             ndays = length(sumdetections), sddetections = sd(sumdetections),
             .groups = "drop") %>% 
   mutate(semdetections = sddetections / sqrt(ndays))

# Exact replica of Feeley
ggplot(data= StatHourSum_16) +
   geom_line(aes(x=hour, y=meandetections, color = station)) +
   geom_errorbar(aes(x=hour, color = station,
                     ymin=meandetections - semdetections, 
                     ymax=meandetections + semdetections),
                 width = 0.2) +
   scale_x_continuous(name = "Hour",
                      breaks = seq(0,23,1), labels = seq(0,23,1)) +
   scale_y_continuous(name = "Mean Detections / Hour", expand=c(0,0)) +
   theme(panel.background = element_blank(), axis.line = element_line())
ggsave("outputs/StationsByHour_FeeleyFig6_2016.jpeg")

# Color by MSSCA
ggplot(data= StatHourSum_16) +
   geom_line(aes(x=hour, y=meandetections, color = mssca, group=station)) +
   geom_errorbar(aes(x=hour, color = mssca,
                     ymin=meandetections - semdetections, 
                     ymax=meandetections + semdetections),
                 width = 0.2) +
   scale_x_continuous(name = "Hour",
                      breaks = seq(0,23,1), labels = seq(0,23,1)) +
   scale_y_continuous(name = "Mean Detections / Hour", expand=c(0,0)) +
   theme(panel.background = element_blank(), axis.line = element_line())
ggsave("outputs/StationsByHour_FeeleyFig6_mssca_2016.jpeg")
