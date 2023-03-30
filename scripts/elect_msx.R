# elect_msx.R

# Sarah Heidmann

# Created 9 Oct 2018
# Last modified 30 March 2023

# This script is based on Google searching 
# As well as a script written by Daniel Holstein for MES 504
# - edited by Sennai Habtes 3/26/2016 

# In this script, focus on the electivity index using binned data

# Load libraries
library(tidyverse)
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

# Look at it
names(msx4ls)
head(msx4ls[[1]])
lapply(msx4ls, nrow)

# Copy one for testing
test <- msx4ls[["A69-1601-24991"]]

# Make a list of all the transmitters
msxTransList <- names(msx4ls)

# Read in the station metadata
msxStatmaster <- read_csv("data/otherdata/uvi_msxwb_station_master_2017.csv",
                          col_types = cols(station = col_character()))

##### Frequency of in/out by hour #####
# Sum frequencies across all fish
freqHalfHourAll <- msx4ls %>% 
   bind_rows() %>% 
   filter(!is.na(mssca)) %>% 
   group_by(halfhour, mssca) %>% 
   summarize(totFreq = sum(No.detections), .groups = "drop")

# Mosaic plot
freqHalfHourMatrix <- freqHalfHourAll %>% 
   pivot_wider(id_cols = halfhour, names_from=mssca, values_from = totFreq) %>% 
   column_to_rownames(var="halfhour") %>% 
   as.matrix()
mosaicplot(freqHalfHourMatrix)

# Chi-square test
freqHalfHourChi <- chisq.test(freqHalfHourMatrix)
freqHalfHourChi  # x2=4994.4, df=47, p <0.0001
# Calculate g-stat
g<-2*sum(freqHalfHourChi$observed*log(freqHalfHourChi$observed/freqHalfHourChi$expected))
g
# Degrees of freedom
df1 <- freqHalfHourChi$parameter
# Significance
1-pchisq(g,df1)
pchisq(g,df1,lower.tail=FALSE)

# Plot over time of day, frequency of in/out (dodged)
ggplot() + 
      geom_bar(aes(x=halfhour,y=totFreq, fill=mssca), data = freqHalfHourAll,
               stat = "identity", position = 'dodge') +
      theme(panel.background = element_blank())

# Stacked
ggplot() + 
      geom_bar(aes(x=halfhour,y=totFreq, fill=mssca), data = freqHalfHourAll,
               stat = "identity", position = 'stack') +
      theme(panel.background = element_blank())

# My version of mosaic
ggplot() + 
      geom_bar(aes(x=halfhour,y=totFreq, fill=mssca), data = freqHalfHourAll,
               stat = "identity", position = 'fill') +
      theme(panel.background = element_blank())

##### Chesson electivity index #####
# http://sources.ecopath.org/trac/ecopath/wiki/EwEugElectivity

# Chesson standardized forage ratio : S = (r/P) / SIGMA(r/P)
# n = number of groups; 0 (avoidance) < S < 1 (exclusive)
# Independent of prey availability

##### Chesson- station #####
# Using Chesson on station preference
# station availability weighted by # years deployed
# Get frequency count
# freqStat <- lapply(msx3ls,plyr::count, vars = c('station')) %>% 
#    # Then sum tables across list
#    bind_rows() %>% 
#    group_by(station) %>% 
#    summarize(totFreq = sum(freq), .groups="drop") %>% 
#    # Add station with no detections
#    add_row(station="566", totFreq=0) %>% 
#    # Proportional frequency
#    mutate(prop = totFreq / sum(totFreq)) %>% 
#    left_join(select(msxStatmaster, station, mssca, ChessonP), by="station") %>% 
#    mutate(RdivP = prop / ChessonP) %>% 
#    mutate(ChessonS = RdivP / sum(RdivP)) %>% 
#    arrange(desc(ChessonS))
# summary(freqStat$ChessonS)
# freqStat
# 512 has the highest index, then 526
# Top 7 are inside MSSCA

##### Chesson- presence/absence #####
# Use Chesson on presence/absence preference (not including DAFM)
freqPADAFM <- function(df){
   trans <- df$transmitter[1]
   freqDAFM <- xtabs(~DAFM + PA, data = df)
   # if(length(unique(df$DAFM))>1){
   #    chitest <- chisq.test(freqDAFM)
   #    print(chitest)
   # }
   return(as.data.frame(freqDAFM))
}
freqPAall <- lapply(msx4ls, freqPADAFM) %>% 
   bind_rows() %>% 
   group_by(PA) %>% 
   summarize(totFreq = sum(Freq)) %>% 
   mutate(prop = totFreq / sum(totFreq)) %>% 
   add_column(ChessonP = 0.5) %>% # Giving equal weight to presence/absence
   mutate(RdivP = prop / ChessonP) %>% 
   mutate(ChessonS = RdivP / sum(RdivP))
freqPAall
# absence has the higher index (0.97 vs 0.03)
##### Chesson- in/out #####
# Use Chesson on in/out preference (not including time of day)
# First calculate total weights to in and out
ChessonP_inout <- msxStatmaster %>% 
   group_by(mssca) %>% 
   summarize(ChessonP = sum(yearsDeployed) / sum(msxStatmaster$yearsDeployed))
# Now calculate index for in and out
freqMSSCA <- lapply(msx4ls,plyr::count, vars = c('mssca')) %>% 
   # Then sum tables across list
   bind_rows() %>% 
   group_by(mssca) %>% 
   summarize(totFreq = sum(freq), .groups="drop") %>% 
   # Proportional frequency
   mutate(prop = totFreq / sum(totFreq)) %>% 
   left_join(ChessonP_inout, by="mssca") %>% 
   mutate(RdivP = prop / ChessonP) %>% 
   mutate(ChessonS = RdivP / sum(RdivP)) %>% 
   arrange(desc(ChessonS))
freqMSSCA
# in has the higher index (0.55 vs 0.45)

##### Chesson- in/out by half hour #####
# Now on MSSCA preference across time of day
# First calculate total weights to half hours (split in/out into 48)
ChessonP_halfhour <- msxStatmaster %>% 
   group_by(mssca) %>% 
   summarize(ChessonP = sum(yearsDeployed) / sum(msxStatmaster$yearsDeployed) / 48)
# Can use freqHourAll from above
freqHalfHourEI <- freqHalfHourAll %>% 
   mutate(prop = totFreq / sum(totFreq)) %>% 
   left_join(ChessonP_halfhour, by="mssca") %>% 
   mutate(RdivP = prop / ChessonP) %>% 
   mutate(ChessonS = RdivP / sum(RdivP)) %>% 
   arrange(desc(ChessonS))
freqHalfHourEI
# Highest indices were inside @ 0530, 0600, and 0500; top 10 all in

# Chi-square test
freqHalfHourEIMatrix <- freqHalfHourEI %>% 
   pivot_wider(id_cols = halfhour, names_from=mssca, values_from = ChessonS) %>% 
   column_to_rownames(var="halfhour") %>% 
   as.matrix()
mosaicplot(freqHalfHourEIMatrix)
freqHalfHourEIchi <- chisq.test(freqHalfHourEIMatrix*96)
freqHalfHourEIchi
# Warning: chi-squared approximation may be incorrect
# Not significant but chi-square doesn't really work on proportions.

# Plot- manuscript figure 8
spawnlim <- c(14.5,16)
ggplot() +
   # Highlight spawning time
   geom_vline(aes(xintercept=spawnlim[1]), linetype="dotted") +
   geom_vline(aes(xintercept=spawnlim[2]), linetype="dotted") +
   geom_rect(aes(xmin=spawnlim[1], xmax = spawnlim[2], 
                 ymin=-Inf,ymax = Inf), fill = "gray90") +
   geom_line(aes(x=halfhour,y=ChessonS,color=mssca), data = freqHalfHourEI) +
   # Show expected Chesson value
   geom_hline(yintercept = 1/nrow(freqHalfHourEI), color = "black") +
   # Formatting
   scale_x_continuous(breaks = c(0:23), labels = c(0:23)) +
   scale_color_discrete(name= "MSSCA") +
   xlab("Time of Day (half-hour bins)") +
   ylab("Chesson Election Index") +
   theme(panel.background = element_blank(), legend.key = element_blank(),
         text = element_text(size = 20), axis.line = element_line(),
         legend.position = "top")
#ggsave("outputs/Figure8_ChessonHalfHour_20230330.jpeg")

##### Chesson- presence/absence by month #####
# Now by month
freqPAmonth <- function(df){
   trans <- df$transmitter[1]
   df2 <- df %>% 
      mutate(month = format(date, format = "%b"),
             year = format(date, format = "%Y")) %>% 
      group_by(transmitter, year, month) %>% 
      summarize(PA = ifelse("P" %in% PA, "P","A"), .groups="drop")
   freqmonth <- xtabs(~month + PA, data = df2)
   # if(length(unique(df2$month))>1){
   #    chitest <- chisq.test(freqmonth)
   #    print(chitest)
   # }
   return(as.data.frame(freqmonth))
}
freqmonthls <- lapply(msx4ls, freqPAmonth)
# Then sum tables across list
freqmonthAll <- freqmonthls %>% 
   bind_rows() %>% 
   group_by(month, PA) %>% 
   summarize(totFreq = sum(Freq), .groups="drop") %>% 
   mutate(month = ordered(month, 
                          levels = c("Jan","Feb","Mar","Apr","May","Jun",
                                     "Jul","Aug","Sep","Oct","Nov","Dec"))) %>% 
   # reformat for Chesson
   pivot_wider(id_cols = month, names_from = PA, values_from = totFreq) %>% 
   mutate(totFreq = A + P) %>% 
   mutate(ChessonP = totFreq / sum(totFreq)) %>% 
   mutate(prop = P / sum(totFreq)) %>% 
   mutate(RdivP = prop / ChessonP) %>% 
   mutate(ChessonS = RdivP / sum(RdivP)) %>% 
   arrange(desc(ChessonS))
freqmonthAllCast
# May and Apr are close, then Jun, then Mar then Jul

##### Chesson- presence absence by DAFM #####
# function to calculate frequency table for each fish (output is list)
freqPADAFM <- function(df){
   trans <- df$transmitter[1]
   freqDAFM <- xtabs(~DAFM + PA, data = df)
   # if(length(unique(df$DAFM))>1){
   #    chitest <- chisq.test(freqDAFM)
   #    print(chitest)
   # }
   return(as.data.frame(freqDAFM))
}
freqDAFMls <- lapply(msx4ls, freqPADAFM)
# Then sum tables across list
freqDAFMAll <- freqDAFMls %>% 
   bind_rows() %>% 
   # Format for Chesson
   mutate(DAFM = as.integer(as.character(DAFM))) %>% 
   group_by(DAFM, PA) %>% 
   summarize(totFreq = sum(Freq), .groups="drop") %>% 
   pivot_wider(id_cols = DAFM, names_from = PA, values_from = totFreq) %>% 
   mutate(totFreq = A + P) %>% 
   mutate(ChessonP = totFreq / sum(totFreq)) %>% 
   mutate(prop = P / sum(totFreq),
          prop_abs = A / sum(totFreq)) %>% 
   mutate(RdivP = prop / ChessonP,
          RdivP_abs = prop_abs / ChessonP) %>% 
   mutate(ChessonS = RdivP / sum(RdivP),
          ChessonS_abs = RdivP_abs / sum(RdivP_abs)) %>% 
   arrange(desc(ChessonS)) %>% 
   select(DAFM, ChessonP, ChessonS, ChessonS_abs)
freqDAFMAll
# 5 DAFM has the highest index (0.15), then 6 (0.14)
# Top 10 are 0-9 DAFM, 0-8 DAFM are all above expected value (ChessonP)

# Try using ggpubr
freqDAFM_plot <- freqDAFMAll %>% 
   pivot_longer(cols = c("ChessonS","ChessonS_abs","ChessonP"),
                names_to = "type",
                values_to = "value")
dafmChesson <- ggline(freqDAFM_plot, x="DAFM",y="value",color = "type",
                      plot_type = "l", ylab="Chesson Election Index") +
   scale_color_manual(name="", 
                      values=c(ChessonS="mediumaquamarine",
                               ChessonS_abs="orangered1",
                               ChessonP="black"),
                      breaks=c("ChessonS","ChessonS_abs","ChessonP"),
                      labels=c("Present","Absent","Expected")) +
   scale_x_continuous(breaks = c(-10:20), labels = c(-10:20)) +
   theme(text = element_text(size = 18),
         axis.text.x = element_text(size = 12))
# ggexport(dafmChesson,
#          filename="outputs/Figure4_20230330.jpeg",
#          width = 600)