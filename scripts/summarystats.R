# summarystats.R
# Sarah Heidmann
# Created 9 Oct 2018
# Last modified 28 May 2024

# This script is an exploration of some basic descriptive statistics.
# It contains the code for Figure 2 of the manuscript.

# Load libraries
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(ggpubr)

##### Import the data #####
sourcePath <- "data/2_processed_SLH/"
filenames <- list.files(path = sourcePath)

importMSX2 <- function(filename){
   # Read the file
   data <- read_csv(paste0(sourcePath, filename),
                    col_types = cols(station = col_character(),
                                     detection_time_ast = col_datetime(format = 
                                                                  "%Y/%m/%d %H:%M:%S"),
                                     timediff = col_integer(),
                                     year = col_integer())) %>% 
   # Change time zone
   mutate(detection_time_ast = force_tz(detection_time_ast, 
                                        "America/Virgin"))
   return(data)
}

msx2ls <- lapply(filenames, importMSX2)
names(msx2ls) <- gsub(".csv", "", filenames)

# Look at it
names(msx2ls)
lapply(msx2ls, nrow)


# Copy one for testing
test <- msx2ls[["A69-1601-24991"]]

# Separate usable ones
msxUseList <- c("A69-1601-24991", "A69-1601-24992", "A69-1601-24993",
                "A69-1601-24995", "A69-1601-24996", "A69-1601-24997",
                "A69-1601-24998", "A69-1601-25000", "A69-1601-25004",
                "A69-1601-25006", "A69-1601-25007", "A69-1601-25010",
                "A69-1601-25018", "A69-1601-25019", "A69-1601-25020", 
                "A69-1601-59283", "A69-1601-59284", "A69-1601-59287", 
                "A69-1601-59292", "A69-1601-59295", "A69-1601-59296", 
                "A69-1601-59301", "A69-9002-10832", "A69-9002-10833")
msx2ls_use <- msx2ls[names(msx2ls) %in% msxUseList]

# Read the transmitter master sheet
wbTransmaster <- read_csv("data/otherdata/uvi_transmitter_master_2017.csv",
                        col_types = cols(release_date = col_date(format="%m/%d/%y"),
                                         TL = col_double())) %>% 
   filter(species=="Lutjanus analis" & release_site %in% 
             c("STX Mutton","STX Chumfish 18 - Mutton FSA",
               "STX Chumfish 19 - Mutton FSA",
               "STX Chumfish 20 - Mutton FSA",
               "STX Mutton 20 -Mutton FSA",
               "Chumfish33"))
# problems okay-- are NAs

# Station master sheet
msxStatmaster <- read_csv("data/otherdata/uvi_msxwb_station_master_2017.csv")

# Create the color list for plotting
msxColors <- brewer.pal(4,"Set2")
msxColors <- setNames(msxColors, c("2015","2016", "2017", "2014"))

##### fish tagged #####
# How many total?
nrow(wbTransmaster)
# How many tagged each year?
summary(as.factor(wbTransmaster$release_year))
# Tag date summary?
summary(wbTransmaster$release_date)
# Sex breakdown?
summary(as.factor(wbTransmaster$Sex))
# Size summary?
summary(wbTransmaster$TL)
# Size SEM?
sd(wbTransmaster$TL) / sqrt(length(wbTransmaster$TL))
# How many with pressure tags?
summary(as.factor(wbTransmaster$tag_model)) # all tagged
wbTransmaster %>% 
   filter(transmitter %in% msxUseList) %>% 
   pull(tag_model) %>% as.factor() %>% summary() # 2 kept in analyses

# Size frequency graph
sizefreq <- gghistogram(data = wbTransmaster, x="TL", y="..count..",
                        fill = "Sex",
                        binwidth = 2, xlab = "Total Length (cm)",
                        #palette = c("#00AFBB", "#CD5C5C", "#E7B800"),
                        palette = c("#A9A9A9","#FFFFFF","#000000"),
                        ylab = "Frequency", position = "stack") +
   geom_vline(xintercept = 52, color = "black", linetype = "dotted") +
   scale_y_continuous(expand =c(0,0)) +
   scale_x_continuous(breaks = seq(40,75,5)) +
   theme(text = element_text(size = 18))

##### Detections #####
# Number of fish with detections
length(msx2ls)

# First detection
firstdates <- c()
for(data in msx2ls){
   firstdates <- append(firstdates,min(data$date))
}
min(firstdates)

# Last detection
lastdates <- c()
for(data in msx2ls){
   lastdates <- append(lastdates,max(data$date))
}
max(lastdates)

# How many returned for months/years?
msxFishSum <- wbTransmaster %>% 
   select(transmitter,release_year, release_site,TL,Sex) %>% 
   # Add number of days, months, and years detected
   # Set as zero and changed if detected
   add_column(no.days = 0, no.months = 0, no.years = 0)
returndays <- 0
returnmonths <- 0
returnyears <- 0
monthlist <- c()
for(i in 1:length(msx2ls_use)){
   dataset <- msx2ls_use[[i]]
   trans <- dataset$transmitter[1]
   uniquedays <- unique(dataset$date)
   msxFishSum[msxFishSum$transmitter == trans,]$no.days <- length(uniquedays)
   if(length(uniquedays) > 1){
      returndays <- returndays + 1
   }
   uniquemonths <- unique(substr(as.character(dataset$date),1,7))
   msxFishSum[msxFishSum$transmitter == trans,]$no.months <- length(uniquemonths)
   if(length(uniquemonths) > 1){
      returnmonths <- returnmonths + 1
   }
   # print(uniquemonths)
   # print(length(uniquemonths))
   monthlist <- c(monthlist, length(uniquemonths))
   uniqueyears <- unique(dataset$year)
   msxFishSum[msxFishSum$transmitter == trans,]$no.years <- length(uniqueyears)
   if(length(uniqueyears) > 1){
      returnyears <- returnyears + 1
   }
}
returndays
returnmonths
returnyears

# summarize months returned within each season
monthlist <- monthlist[monthlist >1]
mean(monthlist)
sd(monthlist)

# Look at table
msxFishSum %>% filter(transmitter %in% msxUseList) # 24 kept
msxFishSum %>% filter(transmitter %in% msxUseList & 
                         no.months==1) %>% nrow() # 13 only one month
# this doesn't match timesreturned because one fish did one trip that extended into the next month. 14 is correct
msxFishSum %>% filter(transmitter %in% msxUseList & no.months>1) %>% 
   arrange(desc(no.months)) %>% print(n=11) # 11 multiple months
msxFishSum %>% filter(transmitter %in% msxUseList & no.years>1) # 8 multiple years


# Plot presence by month and DAFM (colored by year)
classMonth <- function(dataset){
   dataset <- dataset %>% 
      mutate(monthint = as.integer(format(date, format = "%m")),
             monthname = format(date, format = "%b"))
   return(dataset)
}
msxDAFMsum <- lapply(msx2ls_use, classMonth) %>% 
   bind_rows() %>% 
   group_by(transmitter, year, monthint, monthname, DAFM) %>% 
   summarize(detfreq = length(DAFM), .groups="drop") %>% 
   arrange(desc(detfreq))

# Earliest detections?
min(msxDAFMsum$monthint)
max(msxDAFMsum$monthint)
# Peak spawning months?
msxDAFMsum %>% 
   group_by(monthint) %>% 
   summarize(dets = sum(detfreq)) %>% 
   arrange(desc(dets))

# Which fish stayed late?
msxDAFMsum %>% filter(DAFM>14)
# 24991 (49.5 cm), 24996 (46.5 cm) through new moon

# Try separating by month and year simultaneously using lines and colors
msxDAFMsum2 <- msxDAFMsum %>% 
   filter(year>2014) %>% # only one fish on one day in 2014
   group_by(year, monthint, monthname, DAFM) %>% 
   summarize(no.inds = length(unique(transmitter)),
             no.dets = sum(detfreq), .groups="drop") %>% 
   mutate(year = as.character(year))

ggplot(data = msxDAFMsum2) +
   xlab("Days after the full moon (DAFM)") +
   ylab("Individuals detected") +
   geom_bar(aes(x=DAFM, y = no.inds, fill = year), stat = "identity") +
   scale_y_continuous(expand = c(0, 0)) +
   scale_x_continuous(breaks = c(-2:10,15,20)) +
   scale_fill_manual("Year", values = msxColors) +
   facet_wrap(~factor(monthname, levels=c('Feb','Mar','Apr','May',
                                           'Jun','Jul','Aug','Sep',
                                           'Oct','Nov')), 
              ncol = 2) + # add scale="free_y" for diff y-axes
   theme(panel.background = element_blank(), axis.line = element_line(),
         legend.key = element_blank(), strip.background = element_blank(),
         text = element_text(family = "Times New Roman", size = 20),
         axis.text = element_text(size = 10))

# ggpubr
# This is the code for Figure 2 of the manuscript.
month_labels <- msxDAFMsum2 %>%
   mutate(DAFM = 0, no.inds=22) %>%
   distinct(monthname, DAFM, no.inds)
dafmonth <- ggbarplot(msxDAFMsum2, 
                      x="DAFM", y="no.inds", fill="year", 
                      xlab="Days after the full moon (DAFM)",
                      ylab = "Number of fish present") +
   scale_y_continuous(expand = c(0, 0)) +
   #scale_x_continuous(breaks = c(-2:10,15,20)) +
   scale_fill_manual(name = "", values = msxColors) +
   facet_wrap(~factor(monthname, levels=c('Feb','Mar','Apr','May',
                                          'Jun','Jul','Aug','Sep',
                                          'Oct','Nov')), 
              # strip.position = "right",
              ncol = 2) + # add scale="free_y" for diff y-axes
   # annotate("text", x= -2, y = 20, label =c('Feb','Mar','Apr','May',
   #                                                  'Jun','Jul','Aug','Sep',
   #                                                  'Oct','Nov'))+
   geom_text(aes(label = monthname, family = "Times New Roman"), 
             data = month_labels, hjust=-0.2, size=10) +
   theme(panel.background = element_blank(), axis.line = element_line(),
         legend.key = element_blank(), strip.background = element_blank(),
         #strip.placement = "inside", #strip.text.y.left = element_text(angle=45),
         strip.text = element_blank(),
         legend.position = "top",
         text = element_text(family = "Times New Roman", size = 20),
         axis.text = element_text(size = 15))

# ggsave(filename="outputs/M14620_Fig2.tif", plot=dafmonth, dpi=300,
#        width = 4000, height = 4000, units = "px")
# ggexport(dafmonth,
#          filename="outputs/Figure2_202303.jpeg",
#          width = 600, height = 600)

# Which fish were there in late months?
msxDAFMsum %>% 
   filter(monthint>8) %>% 
   pull(transmitter) %>% unique()
#"24991" "25018" "25020" "59292" "59295"

##### Receivers visited #####
# 39 stations total (see 1_processing.R)
statsvisited <- c()
for(data in msx2ls){
   statsvisited <- unique(append(statsvisited,unique(data$station)))
}
statsvisited
summary(msxStatmaster$station %in% statsvisited)
msxStatmaster %>% filter(!(station %in% statsvisited)) %>% pull(station) #566
# 566 was the only station not to receive detections
# It's the western station in the inland row by station 540 at the curve
# It was only deployed May - July 2017

##### Time spent resident and absent from the array #####
# This section could be simplified into one summary table but I'm low on time
# It could have transmitter, Year, Month, TripID, 
# Duration of presence, and duration of absence after
# closest is tripdays

# Times returned and absent days
timesreturned <- c()
absdays <- list()
tripsbyyear <- data.frame(transmitter = rep(msxUseList, each = 3),
                          year = rep(c("2015","2016","2017"), 
                                     times = length(msxUseList)),
                          trips = 0)
for(data in msx2ls_use){
   trans <- data$transmitter[1]
   dates <- unique(data$date)
   count <- 1
   if(length(dates) > 1){
      for(i in 1:(length(dates)-1)){
         if(dates[i+1] > dates[i] + 7){
            count <- count + 1
            abs <- as.numeric(dates[i+1] - dates[i])
            absdays[[paste0(trans,"_",
                            as.character(count-1))]] <- abs
            year <- as.character(format(dates[i], format = "%Y"))
            tripsbyyear[tripsbyyear$transmitter==trans & 
                           tripsbyyear$year==year,]$trips <-
               tripsbyyear[tripsbyyear$transmitter==trans & 
                              tripsbyyear$year==year,]$trips + 1
         }
      }
   }
   timesreturned <- append(timesreturned,count)
}
names(timesreturned) <- names(msx2ls_use)
length(timesreturned) == length(msx2ls_use) # check it
# Look at times returned
timesreturned # All fish
median(timesreturned) # median trips
length(timesreturned[timesreturned==1]) # how many only once?
# this doesn't match msxFishSum because one fish did one trip that extended into the next month. 14 is correct
mean(timesreturned[timesreturned!=1]) # mean of those with multiple trips
sd(timesreturned[timesreturned!=1]) / # sem of those with multiple trips
   sqrt(length(timesreturned[timesreturned!=1])) 
ggplot() + 
   geom_histogram(aes(x=timesreturned), 
                  data = as.data.frame(timesreturned)) +
   theme(panel.background = element_blank())
# Look at days absent
absdays <- unlist(absdays)
absdays
median(absdays) # median number of days absent
min(absdays)
max(absdays)
mean(absdays) # 
sd(absdays) / sqrt(length(absdays))
ggplot() + 
   geom_histogram(aes(x=absdays), data = as.data.frame(absdays)) +
   theme(panel.background = element_blank())
# Look at trips per year
tripsbyyear
min(tripsbyyear[tripsbyyear$trips!=0,]$trips)
max(tripsbyyear[tripsbyyear$trips!=0,]$trips)
summary(tripsbyyear[tripsbyyear$trips!=0,]$trips)
sd(tripsbyyear[tripsbyyear$trips!=0,]$trips) / 
   sqrt(length(tripsbyyear[tripsbyyear$trips!=0,]$trips)) # SEM
ggplot() +
   geom_histogram(aes(x=trips), binwidth = 1, 
                  data = tripsbyyear[tripsbyyear$trips!=0,]) +
   scale_x_continuous(breaks = seq(0,9,1)) +
   theme(panel.background = element_blank())

# Number of days per trip
tripdays <- tibble()
for(data in msx2ls_use){
   trans <- data$transmitter[1]
   dates <- unique(data$date)
   count <- 1
   trip <- 1
   if(length(dates) > 1){
      for(i in 1:(length(dates)-1)){
         if(dates[i+1] < dates[i] + 7){
            count <- count + 1
         }else{
            tripdays <- bind_rows(tripdays, 
                                  tibble(transmitter=trans,
                                         year = format(dates[i],"%Y"),
                                         monthnum=as.integer(format(dates[i],"%m")),
                                         tripnum = trip,
                                         triplen = count,
                                         abslen = dates[i+1]-dates[i]))
            count <- 1
            trip <- trip + 1
         }
      }
   }else{
      tripdays <- bind_rows(tripdays, 
                            tibble(transmitter=trans,
                                   tripnum = trip,
                                   triplen = count))
   }
}
tripdays
median(tripdays$triplen) # median trip length
min(tripdays$triplen)
max(tripdays$triplen)
mean(tripdays$triplen) # mean trip length
sd(tripdays$triplen) / sqrt(nrow(tripdays)) # sem of trip length

# Did trip length change by month?
# linear regression- sig but doesn't meet assumptions
summary(lm(triplen~monthnum, data=tripdays))
ggplot(data=tripdays,aes(x=monthnum,y=triplen, color=transmitter)) +
   geom_point(alpha=0.3) +
   geom_smooth(method="lm", se=F)
# Mean and sem by month
tripdays %>% 
   group_by(monthnum) %>% 
   summarize(meanlen = mean(triplen),
             sdlen = sd(triplen),
             nlen = length(triplen),.groups="drop") %>% 
   mutate(semlen = sdlen / sqrt(nlen)) %>% 
   ggplot(data=.,aes(x=monthnum,y=meanlen)) +
   geom_point() +
   geom_errorbar(aes(ymin=meanlen-semlen,ymax=meanlen+semlen))

# Need to check absent days because the last trip a fish ever took should have an NA
# Also need to check single date of detections for 24992 in March 2016 between full moons
# How many days absent within a season?
tripdays %>% 
   filter(abslen < 40 & abslen>7) %>% 
   mutate(abslen = as.numeric(abslen)) %>% 
   pull(abslen) %>% summary()

# How many days absent between seasons?
tripdays %>% 
   filter(abslen > 40) %>% 
   mutate(abslen = as.numeric(abslen)) %>% 
   pull(abslen) %>% summary()

##### Directionality #####
# Was there directionality to departures?
lastdets <- tibble()
for(data in msx2ls_use){
   oneweek <- 60*60*24*7 # number of seconds in a week for use with timediff
   trans <- substr(data$transmitter[1],10,14)
   groupnum <- 1
   for(i in 1:(nrow(data)-1)){ # NA is the last row timediff
      if(data[i,]$timediff>oneweek){
         sub <- data[(i-4):i,] %>% 
            mutate(group = groupnum)
         lastdets <- bind_rows(lastdets,sub)
         groupnum <- groupnum + 1
      }
   }
}
lastdets
ggplot(data=lastdets) + 
   geom_path(aes(x=long_nad83,y=lat_nad83, group=group,color=group),
             arrow = arrow(length = unit(0.55, "cm"))) +
   coord_map() + 
   facet_wrap(~transmitter,ncol=3)
# No clear patterns, last detections happen all over the place