# longbyhour.R
# Sarah Heidmann

# Created 30 Nov 2023
# Last modified 24 Apr 2024

# This script is based on a script of a similar name written by me in 2019
# It plots average longitude of all fish by time of day.
# It contains the code for Figures 7 & 8 of the manuscript.

# Load libraries
library(tidyverse)
library(RColorBrewer)
library(ggpubr)

##### Import data #####
msxUseList <- c("A69-1601-24991", "A69-1601-24992", "A69-1601-24993",
                "A69-1601-24995", "A69-1601-24996", "A69-1601-24997",
                "A69-1601-24998", "A69-1601-25000", "A69-1601-25004",
                "A69-1601-25006", "A69-1601-25007", "A69-1601-25010",
                "A69-1601-25018", "A69-1601-25019", "A69-1601-25020", 
                "A69-1601-59283", "A69-1601-59284", "A69-1601-59287", 
                "A69-1601-59292", "A69-1601-59295", "A69-1601-59296", 
                "A69-1601-59301", "A69-9002-10832", "A69-9002-10833")
filenames <- paste0(msxUseList, ".csv")
sourcePath <- "data/4_binned_30min/"

importMSX4 <- function(filename){
   # Read the file
   data <- read_csv(paste0(sourcePath, filename),
                    col_types = cols(transmitter = col_character(),
                                     date = col_date(),
                                     mssca = col_character())) %>% 
      mutate(PA = ifelse(No.stations>0,"P","A"))
   return(data)
}

msx4ls <- lapply(filenames, importMSX4)
names(msx4ls) <- gsub(".csv", "", filenames)

# Create the color list for plotting
msxColors <- brewer.pal(4,"Set2")
msxColors <- setNames(msxColors, c("2015","2016", "2017", "2014"))

# ADCP data
# Gives speed and direction for 20 bins from the head of the device to the surface
# Bins 1-9 are fairly consistent and show two groups (+ and - direction)
# so let's average them for analyses
adcp <- read_csv("data/currents/MSX_20150504_20151002.csv",
                 col_types = cols(`DateTime(UTC-4)`=
                                     col_datetime(format="%m/%d/%y %H:%M"))) %>%
   # Change time zone
   mutate(`DateTime(UTC-4)` = force_tz(`DateTime(UTC-4)`, 
                                       "America/Virgin")) %>%
   mutate(Direction.1_9 = round(rowMeans(select(.,`Direction 1`:`Direction 9`)), 
                                2),
          Speed.1_9 = round(rowMeans(select(., `Speed 1`:`Speed 9`)), 
                            2)) %>%
   # cosine of N is +1 and S is -1 (but I want E to be +1 and W to be -1)
   # So subtract 90 from degrees, convert to radians, and take cosine
   # s for scaled value (-1 to +1)
   mutate(Direction.1_9_s = cos((.$Direction.1_9 - 90)*pi/180))

##### Summarize the positions #####
# Find average longitude for each fish for each half-hour of the day
msxLongHour <- msx4ls %>% 
   bind_rows() %>% 
   filter(!is.na(avg_lat)) %>% 
   group_by(transmitter, year, halfhour) %>% 
   summarize(avglat = mean(avg_lat),
             avglong = mean(avg_long),
             n = length(avg_long),
             se = sd(avg_long)/sqrt(length(avg_long)), 
             .groups="drop") %>% 
   mutate(year=as.character(year),
          halfhour=as.numeric(halfhour))

# Do the fish aggregate (and perhaps spawn) later each day after full moon?
# Does peak abundance each day correspond with the time of slack low tide?
# Find min longitude (most negative) of fish each day for 4-6 DAFM
mindate <- as.Date("2015-06-06")
maxdate <- as.Date("2015-06-08")
longsum <- msx4ls %>% 
   bind_rows() %>% 
   filter(!is.na(avg_lat) & date >= mindate & date <= maxdate) %>% 
   group_by(date, halfhour) %>% 
   summarize(avglat = mean(avg_lat),
             avglong = mean(avg_long),
             n = length(avg_long),
             se = sd(avg_long)/sqrt(length(avg_long)), 
             .groups="drop") %>% 
   mutate(halfhour=as.numeric(halfhour)) %>% 
   arrange(avglong)
# I don't think we have enough fish for there to be a pattern here
# Try remaking movement plots below split by day


##### Longitude by time of day #####
spawnLims <- c(12,15)
# Make the base plot
longByHour_p <- ggplot(data = msxLongHour, 
       aes(x = halfhour, y = avglong, color=year,group=year,fill=year))+
   geom_point() +
   geom_errorbar(aes(ymin=avglong-se,ymax=avglong+se), width=0.25)+
   geom_smooth(formula = y~x, method="loess") +
   # Highlight spawning time
   # geom_vline(aes(xintercept=spawnLims[1]), color="red") +
   # geom_vline(aes(xintercept=spawnLims[2]), color="red") +
   # Highlight MSSCA boundaries
   geom_hline(aes(yintercept=-64.88333), color="blue") +
   geom_hline(aes(yintercept=-64.84167), color="blue") +
   # Formatting
   scale_colour_manual("Year", values = msxColors) +
   scale_fill_manual(values=msxColors, guide="none") +
   guides(color=guide_legend(override.aes=list(fill=NA))) +
   xlab("Time of Day") +
   #ylab("Longitude (W:E)") +
   ylab(paste0("Longitude \n(W",expression("\U02192"),"E)")) +
   scale_x_continuous(breaks = c(0:23), labels = c(0:23)) +
   theme(panel.background = element_blank(), axis.line = element_line(),
         legend.key = element_blank(), text = element_text(size = 20))

# Add depth detections
# Run depth.R to create depth_all
depthShapes <- c("shallow"=24,"deep"=25)
depthSum <- depth_all  %>% 
   filter(!is.na(depthcat))%>% 
   group_by(halfhour,long_nad83,depthcat) %>% 
   summarize(n=length(depthcat),.groups="drop")
longByHourDepth_p <- longByHour_p + 
   geom_point(data=depthSum,
              aes(x=halfhour,y=long_nad83,shape=depthcat,size=n),
              color="black",group=NA,fill=NA) +
   scale_shape_manual(values=depthShapes,name="",breaks=c("shallow","deep"))+
   guides(size="none")
# ggexport(longByHourDepth_p, filename="outputs/longbyhourdepth_figure_20231201.pdf",
#          width = 12, height = 7)
# ggexport(longByHourDepth_p, filename="outputs/longbyhourdepth_figure_20231201.jpeg",
#          width = 1000, height = 500)

# Flip the coordinates for reviewer
# This is figure 7 of the manuscript
longByHourDepth_p_flip <- ggplot(data = msxLongHour, 
                                 aes(x = halfhour, y = avglong, 
                                     color=year,group=year,fill=year))+
   geom_point() +
   geom_errorbar(aes(ymin=avglong-se,ymax=avglong+se), width=0.25)+
   geom_smooth(formula = y~x, method="loess") +
   # Add depth
   geom_point(data=depthSum,
              aes(x=halfhour,y=long_nad83,shape=depthcat,size=n),
              color="black",group=NA,fill=NA) +
   scale_shape_manual(values=depthShapes,name="",breaks=c("shallow","deep"))+
   guides(size="none") +
   # Highlight spawning time
   # geom_vline(aes(xintercept=spawnLims[1]), color="red") +
   # geom_vline(aes(xintercept=spawnLims[2]), color="red") +
   # Highlight MSSCA boundaries
   geom_hline(aes(yintercept=-64.88333), color="blue") +
   geom_hline(aes(yintercept=-64.84167), color="blue") +
   # Formatting
   coord_flip()+ # scale_x_reverse() +
   scale_colour_manual("Year", values = msxColors) +
   scale_fill_manual(values=msxColors, guide="none") +
   guides(color=guide_legend(override.aes=list(fill=NA))) +
   xlab("Time of Day") +
   ylab("Longitude (W:E)") +
   #ylab(paste0("Longitude \n(W",expression("\U02192"),"E)")) +
   scale_x_continuous(breaks = c(0:23), labels = c(0:23), expand=c(0,0)) +
   theme(panel.background = element_blank(), axis.line = element_line(),
         legend.key = element_blank(), text = element_text(size = 20), 
         legend.position = "bottom", 
         legend.box="vertical", legend.margin = margin(0,0,0,0,"cm")) 
# ggexport(longByHourDepth_p_flip, filename="outputs/longbyhourdepth_figure_20240423.tiff",
#          width = 2500, height = 2000, res=300)

# Add current direction
msscaLims <- c(0,6,20,23.5)
# Cut to 4-6 DAFM (2015-06-06 to 2015-06-08)- highest tide day is 2015-06-04 (2 DAFM)
# end Date is actually 7 DAFM because the ADCP data is by hour (so midnight on next day)
currents_p <- adcp %>%
   filter(`DateTime(UTC-4)` >= as.Date("2015-06-06") & 
             `DateTime(UTC-4)` <= as.Date("2015-06-09")) %>% 
   mutate(hour=hour(`DateTime(UTC-4)`)) %>% 
   # Initiate the plot
   ggplot(data = ., aes(x=hour, y=Direction.1_9_s, group = hour)) +
   # Highlight time in MSSCA
   geom_rect(aes(xmin=msscaLims[1],xmax=msscaLims[2],ymin=-1,ymax=1), 
             fill = "light blue", alpha = 0.1) +  
   geom_line(data=data.frame(x=c(msscaLims[1],msscaLims[1]),y=c(-1,1)),
             aes(x=x,y=y),color="blue",inherit.aes = FALSE) +
   geom_line(data=data.frame(x=c(msscaLims[2],msscaLims[2]),y=c(-1,1)),
             aes(x=x,y=y),color="blue",inherit.aes = FALSE) +
   geom_rect(aes(xmin=msscaLims[3],xmax=msscaLims[4],ymin=-1,ymax=1), 
             fill = "light blue", alpha = 0.1) + 
   # Highlight spawning time
   geom_rect(aes(xmin=spawnLims[1],xmax=spawnLims[2],ymin=-1,ymax=1), 
             fill = "tomato", alpha = 0.1) +
   geom_line(data=data.frame(x=c(spawnLims[1],spawnLims[1]),y=c(-1,1)),
             aes(x=x,y=y),color="red",inherit.aes = FALSE) +
   geom_line(data=data.frame(x=c(spawnLims[2],spawnLims[2]),y=c(-1,1)),
             aes(x=x,y=y),color="red",inherit.aes = FALSE) +
   # Add the data
   geom_boxplot(width = 0.8) +
   # Bound the plot
   geom_hline(yintercept = -1, color = "black") +
   geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
   geom_hline(yintercept = 1, color = "black") +
   # Add some arrows to show migration directions
   geom_segment(aes(x = 7, y = 0.5, xend = 11, yend = -0.75),
                arrow = arrow(length = unit(0.5, "cm"))) +
   geom_segment(aes(x = 17, y = -0.75, xend = 19, yend = 0.5),
                arrow = arrow(length = unit(0.5, "cm"))) +
   # Formatting
   scale_x_continuous(breaks = seq(0,23,1)) +
   scale_y_continuous(breaks = c(-1.0,-0.5,0.0,0.5,1.0), 
                      labels = c("W","NW/SW","N/S","NE/SE","E")) +
   xlab("Time of Day") +
   ylab(paste0("Current Direction \n(W",expression("\U02192"),"E Component)")) +
   theme(panel.background = element_blank(),
         axis.line.y = element_line(),
         text = element_text(size = 20))

# All together now
migrationDay_figure <- ggarrange(currents_p, 
                                 longByHourDepth_p+theme(legend.position = "bottom"), 
                                 labels = c("A","B"), ncol=1,align="v")
# ggexport(migrationDay_figure, filename = "outputs/migration_figure_20231201.jpeg",
#          width = 1000, height = 1000)
# ggexport(migrationDay_figure, filename = "outputs/Fig6_currentMovement_20240410.tiff",
#          res=300, width = 2900, height = 2000)


##### Redo by multiple shorter time periods #####
# We want to see if this pattern holds.
# Cut to 4-6 DAFM (2015-06-06 to 2015-06-08)- highest tide day is 2015-06-04 (2 DAFM)
CurrentTimeOfDay <- function(startDate, endDate,
                                     spawnLims = c(12,15), msscaLims = c(0,6,20,23.5)){
   monthTitle <- as.character(format(startDate, "%B %Y"))
   # Plot current direction
   currents_p <- adcp %>%
      filter(`DateTime(UTC-4)` >= startDate & 
                `DateTime(UTC-4)` <= endDate) %>% 
      mutate(hour=hour(`DateTime(UTC-4)`)) %>% 
      # Initiate the plot
      ggplot(data = ., aes(x=hour, y=Direction.1_9_s, group = hour)) +
      # Highlight time in MSSCA
      geom_rect(aes(xmin=msscaLims[1],xmax=msscaLims[2],ymin=-1,ymax=1), 
                fill = "light blue", alpha = 0.1) +  
      geom_line(data=data.frame(x=c(msscaLims[1],msscaLims[1]),y=c(-1,1)),
                aes(x=x,y=y),color="blue",inherit.aes = FALSE) +
      geom_line(data=data.frame(x=c(msscaLims[2],msscaLims[2]),y=c(-1,1)),
                aes(x=x,y=y),color="blue",inherit.aes = FALSE) +
      geom_rect(aes(xmin=msscaLims[3],xmax=msscaLims[4],ymin=-1,ymax=1), 
                fill = "light blue", alpha = 0.1) + 
      # Highlight spawning time
      geom_rect(aes(xmin=spawnLims[1],xmax=spawnLims[2],ymin=-1,ymax=1), 
                fill = "tomato", alpha = 0.1) +
      geom_line(data=data.frame(x=c(spawnLims[1],spawnLims[1]),y=c(-1,1)),
                aes(x=x,y=y),color="red",inherit.aes = FALSE) +
      geom_line(data=data.frame(x=c(spawnLims[2],spawnLims[2]),y=c(-1,1)),
                aes(x=x,y=y),color="red",inherit.aes = FALSE) +
      # Add the data- boxplot option
      # geom_boxplot(width = 0.8) +
      # Add the data- point and errorbar option
      stat_summary(fun = mean, geom = "point") + 
      stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.25) +
      # Bound the plot
      geom_hline(yintercept = -1, color = "black") +
      geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
      geom_hline(yintercept = 1, color = "black") +
      # Add some arrows to show migration directions
      geom_segment(aes(x = 7, y = 0.5, xend = 11, yend = -0.75),
                   arrow = arrow(length = unit(0.5, "cm"))) +
      geom_segment(aes(x = 17, y = -0.75, xend = 19, yend = 0.5),
                   arrow = arrow(length = unit(0.5, "cm"))) +
      # Formatting
      scale_x_continuous(breaks = seq(0,23,1)) +
      scale_y_continuous(breaks = c(-1.0,-0.5,0.0,0.5,1.0), 
                         labels = c("W","NW/SW","N/S","NE/SE","E")) +
      xlab("Time of Day") +
      ylab(paste0("Current Direction \n(W",expression("\U02192"),"E Component)")) +
      ggtitle(monthTitle) +
      theme(panel.background = element_blank(),
            axis.line.y = element_line(),
            text = element_text(size = 20))
   return(currents_p)
}
MovementTimeOfDay <- function(startDate, endDate,
                             spawnLims = c(12,15), msscaLims = c(0,6,20,23.5)){
   moveSum <- msx4ls %>% 
      bind_rows() %>% 
      filter(!is.na(avg_lat), date >= startDate, date <= endDate) %>% 
      group_by(transmitter, halfhour) %>% 
      summarize(avglat = mean(avg_lat),
                avglong = mean(avg_long),
                n = length(avg_long),
                se = sd(avg_long)/sqrt(length(avg_long)), 
                .groups="drop") %>% 
      mutate(halfhour=as.numeric(halfhour))
   # Make the fish plot
   longByHour_p <- ggplot(data = moveSum, 
                          aes(x = halfhour, y = avglong))+
      geom_point() +
      geom_errorbar(aes(ymin=avglong-se,ymax=avglong+se), width=0.25)+
      geom_smooth(formula = y~x, method="loess", color = "black") +
      # Highlight spawning time
      # geom_vline(aes(xintercept=spawnLims[1]), color="red") +
      # geom_vline(aes(xintercept=spawnLims[2]), color="red") +
      # Highlight MSSCA boundaries
      geom_hline(aes(yintercept=-64.88333), color="blue") +
      geom_hline(aes(yintercept=-64.84167), color="blue") +
      # Formatting
      xlab("Time of Day") +
      ylab("Longitude (W:E)") +
      #ylab(paste0("Longitude \n(W",expression("\U02192"),"E)")) +
      scale_x_continuous(breaks = c(0:23), labels = c(0:23), limits = c(0,24)) +
      scale_y_continuous(breaks = c(-64.91,-64.89,-64.87,-64.85,-64.83), 
                         labels = c(-64.91,-64.89,-64.87,-64.85,""),
                         limits = c(-64.915,-64.83)) +
      theme(panel.background = element_blank(), axis.line = element_line(),
            legend.key = element_blank(), text = element_text(size = 20))
   return(longByHour_p)
}

# Just one time period
# migrationDay_figure <- ggarrange(currents_p+ggtitle(monthTitle), 
#                                  longByHour_p+theme(legend.position = "bottom"), 
#                                  labels = c("A","B"), ncol=1,align="v")
# ggexport(migrationDay_figure, filename = "outputs/migration_figure_20231201.jpeg",
#          width = 1000, height = 1000)
# ggexport(migrationDay_figure, filename = "outputs/migration_figure_20231207.tiff",
#          width = 1000, height = 1000)


# end Date is actually 7 DAFM because the ADCP data is by hour (so midnight on next day)
# new time period of 2-6 DAFM in June 2015--"2015-06-04", "2015-06-09"
# and 2-6 DAFM in May 2015--"2015-05-05", "2015-05-10"
# and 2-6 DAFM in July 2015-- "2015-07-03", "2015-07-08"
# not clean. try the shorter time period of 4-6 DAFM
# original time period was 4-6 DAFM in June 2015
june2015_c <- CurrentTimeOfDay(startDate = as.Date("2015-06-06"),
                                           endDate = as.Date("2015-06-09"))
june2015_m <- MovementTimeOfDay(startDate = as.Date("2015-06-06"),
                               endDate = as.Date("2015-06-09"))
july2015_c <- CurrentTimeOfDay(startDate = as.Date("2015-07-05"),
                                     endDate = as.Date("2015-07-08"))
july2015_m <- MovementTimeOfDay(startDate = as.Date("2015-07-05"),
                                       endDate = as.Date("2015-07-08"))
aug2015_c <- CurrentTimeOfDay(startDate = as.Date("2015-08-04"),
                                          endDate = as.Date("2015-08-07"))
aug2015_m <- MovementTimeOfDay(startDate = as.Date("2015-08-04"),
                              endDate = as.Date("2015-08-07"))
# adcp data stops in october. Sep would be "2015-09-02", "2015-09-05"

# current direction next to fish location from 4-6 dafm for 3 months
# This is the code for Figure 8 of the manuscript.
multiMonth_currents <- ggarrange(june2015_c+theme(axis.title.x = element_blank()) +
                                    scale_x_continuous(breaks = seq(0,24,3), 
                                                       labels = seq(0,24,3), limits = c(0,24)) +
                                    coord_flip(), 
                                 july2015_c+theme(axis.title.y = element_blank(),
                                                  axis.title.x = element_blank()) +
                                    scale_x_continuous(breaks = seq(0,24,3), 
                                                       labels = seq(0,24,3), limits = c(0,24)) +
                                    coord_flip(), 
                                 aug2015_c+theme(axis.title.y = element_blank(),
                                                 axis.title.x = element_blank()) +
                                    scale_x_continuous(breaks = seq(0,24,3), 
                                                       labels = seq(0,24,3), limits = c(0,24)) +
                                    coord_flip(),
                                 june2015_m + 
                                    scale_x_continuous(breaks = seq(0,24,3), 
                                                       labels = seq(0,24,3), limits = c(0,24)) +
                                    coord_flip(),
                                 july2015_m+theme(axis.title.y = element_blank()) +
                                    scale_x_continuous(breaks = seq(0,24,3), 
                                                       labels = seq(0,24,3), limits = c(0,24)) +
                                    coord_flip(),
                                 aug2015_m+theme(axis.title.y = element_blank()) +
                                    scale_x_continuous(breaks = seq(0,24,3), 
                                                       labels = seq(0,24,3), limits = c(0,24)) +
                                    coord_flip(),
                                 ncol=3, nrow=2, 
                                 labels = c("A","C","E","B","D","F"),align="hv")
multiMonth_currents
# ggexport(multiMonth_currents, 
#          filename = "outputs/migration_currents_4-6DAFM_20240416.jpeg",
#          width = 2000, height = 1000)
# ggexport(multiMonth_currents,
#          filename = "outputs/migration_currents_4-6DAFM_20240423.tiff",
#          res=300, width = 4000, height = 3000)

# Analysis on whether fish movement was moving later in the day based on slack tide
slackSum <- read_csv("data/currents/slacktidesum.csv")
MovementTide <- function(startDate, endDate,
                              spawnLims = c(12,15), msscaLims = c(0,6,20,23.5)){
   slackSub <- filter(slackSum, date >= startDate, date < endDate) %>% 
      mutate(date = as.factor(date))
   moveTideSum <- msx4ls %>% 
      bind_rows() %>% 
      filter(!is.na(avg_lat), date >= startDate, date < endDate) %>% 
      # group_by(transmitter, date, halfhour) %>% 
      # summarize(avglat = mean(avg_lat),
      #           avglong = mean(avg_long),
      #           n = length(avg_long),
      #           se = sd(avg_long)/sqrt(length(avg_long)), 
      #           .groups="drop") %>% 
      mutate(halfhour=as.numeric(halfhour),
             date = as.factor(date))
   # Make the fish plot
   longByHour_p <- ggplot(data = moveTideSum, 
                          aes(x = halfhour, y = avg_long,
                              group=date, color = date))+
      geom_point() +
      #geom_errorbar(aes(ymin=avglong-se,ymax=avglong+se), width=0.25)+
      geom_smooth(formula = y~x, method="loess") +
      geom_vline(aes(xintercept=jitter(halfhour,.5),color=date), data=slackSub) +
      scale_x_continuous(limits = c(0,24))
   return(longByHour_p)
}
june2015_t <- MovementTide(startDate = as.Date("2015-06-06"),
                                endDate = as.Date("2015-06-09"))
july2015_t <- MovementTide(startDate = as.Date("2015-07-05"),
                                endDate = as.Date("2015-07-08"))
aug2015_t <- MovementTide(startDate = as.Date("2015-08-03"),
                               endDate = as.Date("2015-08-06"))
multiMonth_tides <- ggarrange(june2015_t, july2015_t, aug2015_t, ncol=1)
multiMonth_tides
# ggexport(multiMonth_tides,
#          filename = "outputs/slack_movement_4-6DAFM_20240424.jpeg",
#          width = 1000, height = 1000)
