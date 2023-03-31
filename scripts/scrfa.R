# scrfa.R

# Sarah Heidmann
# Created 16 Jan 2020
# Last modified J31 Mar 2023

# Taking data from the Science and Conservation of Fish Aggregations database
# https://www.scrfa.org/database/Search-Results.php
# From March 2023
# Looking at the status of reports of populations

library(tidyverse)

scrfa <- read_csv("data/otherdata/scrfa_202303.csv")

# Look at it
nrow(scrfa)
head(scrfa)
length(unique(scrfa$SpawningID))

# Count current status
status <- plyr::count(scrfa$CurrentStatus) %>% 
   mutate(proptot = round(freq / sum(freq)*100,1),
          propknown = round(freq / sum(freq[1:4])*100,1))

status
