# tampo.R

# Sarah Heidmann
# Created 7 Dec 2023
# Last modified 7 Dec 2023

# This script is an exploration of Tampo mutton snapper detection data.

# Load libraries
library(tidyverse)
library(lubridate)

##### Import the data #####
sourcePath <- "data/otherdata/Tampo/Transmitters/"
filenames <- list.files(path = sourcePath)

importTampo <- function(filename){
   # Read the file
   data <- read_csv(paste0(sourcePath, filename),
                    col_types = cols(station = col_character())) %>% 
      # Change time zone
      mutate(detection_time_ast = force_tz(detection_time_ast, 
                                           "America/Virgin"),
             date = as.Date(detection_time_ast))
   return(data)
}

tam_raw <- lapply(filenames, importTampo)
names(tam_raw) <- gsub(".csv", "", filenames)

# Look at it
names(tam_raw)
lapply(tam_raw, nrow)

# Read the transmitter master sheet
tamTransmaster <- read_csv("data/otherdata/uvi_transmitter_master_2017.csv",
                          col_types = cols(release_date = col_date(format="%m/%d/%y"),
                                           TL = col_double())) %>% 
   filter(species=="Lutjanus analis" & release_site =="STJ Tampo")

##### Add DAFM classification #####
# Find moon date in same month-year and set DAFM
# if DAFM < -7, find moon date in previous month and set DAFM 
moontab <- read_csv("data/otherdata/sttmoontable_14_18.csv") %>%
   mutate(datetimeUTC= as_datetime(paste(month, day, year, timeUTC, sep = " "),
                                   tz="UTC", format = "%b %d %Y %H:%M:%S")) %>%
   mutate(datetimeAST= with_tz(datetimeUTC, tzone="America/Virgin")) %>%
   mutate(date = as_date(datetimeAST))

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
tam <- lapply(tam_raw, addDAFM)

##### Summarize by Day #####
classMonth <- function(dataset){
   dataset <- dataset %>% 
      mutate(year=format(date, format = "%Y"),
             monthint = as.integer(format(date, format = "%m")),
             monthname = format(date, format = "%b"))
   return(dataset)
}
tamDAFMsum <- lapply(tam, classMonth) %>% 
   bind_rows() %>% 
   group_by(transmitter, year, monthint, monthname, DAFM) %>% 
   summarize(detfreq = length(DAFM), .groups="drop") %>% 
   arrange(desc(detfreq))
tamDAFMsum2 <- tamDAFMsum %>% 
   group_by(year, monthint, monthname, DAFM) %>% 
   summarize(no.inds = length(unique(transmitter)),
             no.dets = sum(detfreq), .groups="drop") %>% 
   mutate(year = as.character(year))

##### Recreate Figure 2-- fish presence at array by month and DAFM #####
# Create the color list for plotting
msxColors <- brewer.pal(4,"Set2")
msxColors <- setNames(msxColors, c("2015","2016", "2017", "2014"))
tam_dafmonth <- ggbarplot(tamDAFMsum2, 
          x="DAFM", y="no.inds", fill="year", 
          xlab="Days after the full moon (DAFM)",
          ylab = "Number of fish present") +
   scale_y_continuous(expand = c(0, 0)) +
   #scale_x_continuous(breaks = c(-2:10,15,20)) +
   scale_fill_manual(name = "", values = msxColors) +
   facet_wrap(~factor(monthname, levels=c('Feb','Mar','Apr','May',
                                          'Jun','Jul','Aug','Sep',
                                          'Oct','Nov','Dec')), 
              ncol = 2) + # add scale="free_y" for diff y-axes
   theme(panel.background = element_blank(), axis.line = element_line(),
         legend.key = element_blank(), strip.background = element_blank(),
         legend.position = "top",
         text = element_text(family = "Times New Roman", size = 20),
         axis.text = element_text(size = 15))
# ggexport(tam_dafmonth,
#          filename="outputs/tampo_DAFM_202312.tiff",
#          width = 1000, height = 1000)
