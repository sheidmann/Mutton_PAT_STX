# currents.R

# Sarah Heidmann
# Created 18 Dec 2018
# Last modified 19 May 2020

# Summary:
# Data inputs:
#     - ADCP data
#     - oceanographic model output
# Actions:
#     - plots current direction from ADCP and oceanographic model
# Data exports:
#     - plots

# Load libraries
library(tidyverse)
library(lubridate)

##### Import data #####
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
range(adcp$`DateTime(UTC-4)`)

# Oceanographic model output
# by Sonaljit
# First row has lat/long of each point
# 1) the estimated spawning site (17.63831  -64.90665)
# 2) the ADCP during times we don’t already have data (17.63598	-64.87863)
# 3) inside the closed area (17.63623  -64.8589)
# 4) the western point (17.651526  -64.92555)
# v is north-south velocity and u is east/west velocity (all in m/s)
# Positive are north/east; negative are south/west
model <- read_csv("data/currents/usccom2015currents.csv") %>%
   mutate(datetimeAST = (Index * 3600) + parse_date_time("2015-01-01 00",
                                                orders = "%Y-%m-%d %H",
                                                tz = "America/Virgin")) %>%
   # Scale u2 to unit circle
   mutate(dir2_s = cos(atan(u2/v2))*ifelse(u2<=0, -1, 1))
range(na.omit(model$datetimeAST))


##### Plots #####
# This is the code for Figure 5 in the manuscript
# Cut to 4-6 DAFM (2015-06-06 to 2015-06-08)- highest tide day is 2015-06-04 (2 DAFM)
adcp %>%
   filter(`DateTime(UTC-4)` >= as.Date("2015-06-06") & 
             `DateTime(UTC-4)` <= as.Date("2015-06-08")) %>% 
   mutate(hour=hour(`DateTime(UTC-4)`)) %>% 
   # Initiate the plot
   ggplot(data = ., aes(x=hour, y=Direction.1_9_s, group = hour)) +
   # Fill in the center circle
   geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=-1), fill = "grey30", alpha = 0.1) +
   # Highlight morning and afternoon migrations
   geom_rect(aes(xmin=8,xmax=13,ymin=-1,ymax=1), fill = "light blue", alpha = 0.1) +   
   geom_rect(aes(xmin=16,xmax=21,ymin=-1,ymax=1), fill = "tomato", alpha = 0.1) +
   geom_line(data=data.frame(x=c(8,8),y=c(-1,1)),aes(x=x,y=y),color="blue",inherit.aes = FALSE) +
   geom_line(data=data.frame(x=c(13,13),y=c(-1,1)),aes(x=x,y=y),color="blue",inherit.aes = FALSE) +
   geom_line(data=data.frame(x=c(16,16),y=c(-1,1)),aes(x=x,y=y),color="red",inherit.aes = FALSE) +
   geom_line(data=data.frame(x=c(21,21),y=c(-1,1)),aes(x=x,y=y),color="red",inherit.aes = FALSE) +
   # Add the data
   geom_boxplot(width = 0.8) +
   # Bound the circles
   geom_hline(yintercept = -1, color = "black") +
   geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
   geom_hline(yintercept = 1, color = "black") +
   # Have to use polygon to make the line connect
   # geom_polygon(aes(x=hour,y=medspeed*3, group=1), data = speedsum, 
   #              fill ="NA", color="black", linetype="dotdash") +
   # Formatting
   scale_x_continuous(breaks = seq(0,23,1)) +
   ylim(-1.5, 1) + # negative limit adds white space in the center
   coord_polar(theta = "x", start = -0.1) +
   theme(panel.background = element_blank(),
         axis.text.y = element_blank(), axis.ticks.y = element_blank(),
         axis.title.x = element_blank(), axis.title.y = element_blank(),
         axis.text.x = element_text(size = 14, vjust = 1))
# ggsave("outputs/Figure5_202105.jpeg",
#        width = 10, height = 9, units = "in")

# Try to recreate with model
model %>%
   filter(datetimeAST >= as.Date("2015-06-06") & 
             datetimeAST <= as.Date("2015-06-08")) %>% 
   mutate(hour=hour(datetimeAST)) %>% 
   ggplot(data = ., aes(x=hour, y=dir2_s, group = hour)) +
   geom_boxplot(width = 0.8) +
   geom_hline(yintercept = -1, color = "black") +
   geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
   geom_hline(yintercept = 1, color = "black") +
   scale_x_continuous(breaks = seq(0,23,1)) +
   ylim(-1.5, 1) + # negative limit adds white space in the center
   coord_polar(theta = "x", start = -0.1) +
   theme(panel.background = element_blank(),
         axis.text.y = element_blank(), axis.ticks.y = element_blank(),
         axis.title.x = element_blank(), axis.title.y = element_blank(),
         axis.text.x = element_text(size = 14, vjust = 1))
# ggsave("outputs/figure5_model.jpeg",
#        width = 10, height = 9, units = "in")
# I don't think these line up very well.
# In fact they are approximately opposite. Is that my mistake?

model %>% 
   filter(datetimeAST >= as.Date("2015-06-06") & 
             datetimeAST <= as.Date("2015-06-08")) %>% 
   left_join(filter(adcp,`DateTime(UTC-4)` >= as.Date("2015-06-06") & 
                          `DateTime(UTC-4)` <= as.Date("2015-06-08")), 
             by=c("datetimeAST"="DateTime(UTC-4)")) %>%
   ggplot(data=., aes(x=dir2_s, y=Direction.1_9_s)) +
   geom_point()

