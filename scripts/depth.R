# depth.R
# Sarah Heidmann
# Created 25 Jan 2019
# Last modified 19 Jan 2024

# Looks at depth data for fish with pressure tags

# Load the libraries
library(tidyverse)
#library(ggpubr)

##### Import the data #####
# Only two fish with pressure tags with usable data
depth1 <- read_csv("data/1_processed_JKM/A69-9002-10832.csv") %>% 
# Invert depth for plotting
   mutate(z=-sensor_value)
depth2 <- read_csv("data/1_processed_JKM/A69-9002-10833.csv") %>% 
   mutate(z=-sensor_value)
depth_all <- bind_rows(depth1,depth2) %>% 
   # Create a column for the half-hour bin
   mutate(hour = as.integer(format(detection_time_ast,"%H")),
          min = as.integer(format(detection_time_ast, "%M"))) %>% 
   mutate(halfhour = as.numeric(ifelse(min<30,hour,hour+0.5)),
          depthcat = ifelse(z>-10,"shallow",
                            ifelse(z < -30,"deep",NA)))

##### Summary #####
nrow(depth1) # 7896 rows
summary(depth1$z)
sd(depth1$z) / sqrt(length(depth1$z))

nrow(depth2) # 2767 rows
summary(depth2$z)
sd(depth2$z) / sqrt(length(depth2$z))

# Overall summary
summary(depth_all$z) # most 18-22m
sd(depth_all$z)
sd(depth_all$z)/sqrt(nrow(depth_all)) #sem

# Rick wants a statistical test to compare the two depth profiles
depths <- bind_cols(z = depth1$z,fish = "10832") %>% 
   bind_rows(bind_cols(z=depth2$z, fish="10833"))
ggplot(data= depths,aes(x=z)) +
   geom_histogram() +
   facet_wrap(~fish)
# A little left-skewed but maybe okay
library(car)
leveneTest(z~fish, depths) #p<0.001- not homogeneous
# nonparametric 
wilcox.test(z ~ fish, data = depths) #p=0.1
# The depth profiles are not significantly different

# What % of detections occurred deeper than 30m?
depth_all %>% filter(z < -30) %>% nrow(.)/nrow(depth_all) # 0.5% deep
depth_all %>% filter(z > -10) %>% nrow(.)/nrow(depth_all) # 0.2% shallow

##### Depth by time of day #####
depth_all %>% 
   ggplot(data=., aes(x=hour,y=z,color = transmitter)) +
   geom_point()

# Depth over time
depth_all %>% 
   filter(detection_time_ast < as.Date("2015-07-01")) %>% 
   ggplot(data=.,aes(x=detection_time_ast,y=z,color=transmitter)) +
   geom_point()
