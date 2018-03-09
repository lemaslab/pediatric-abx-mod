##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        February 27, 2018 
# IRB:
# Description: Data management for baby abx data extracted from RedCap. 
# Data: C:\Users\djlemas\Dropbox (UFL)\02_Projects\UFHEALTH\RedCap\redcap_export
# Analysis: Curate data for mode of delivery and infant outcomes. Goal is to
#           get the infant abx data ready for analysis.

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
# location="djlemas";location
# location="Dominick";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\UFHEALTH\\RedCap\\redcap_export\\",sep="");work.dir
data.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\UFHEALTH\\RedCap\\redcap_export\\",sep="");data.dir
out.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\UFHEALTH\\RedCap\\redcap_export\\",sep="");out.dir

# Set Working Directory
setwd(work.dir)
list.files()

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

# library(readxl)
library(data.table)
library(tidyr)
library(dplyr)
library(reshape2)

# **************************************************************************** #
# ***************  UFHealthEarlyLifeExp_DATA_Infant_ABX_2018-02-27_1411.csv                                              
# **************************************************************************** # 

#Read Data
data.file.name="UFHealthEarlyLifeExp_DATA_Infant_ABX_2018-02-27_1411.csv";data.file.name
data.file.path=paste0(data.dir,"\\",data.file.name);data.file.path
ufhealth.abx<- read.csv(data.file.path);ufhealth.abx

# look at data
dat=ufhealth.abx
head(dat); str(dat); names(dat)

# first thing we want i sunique list of abx
abx.op=unique(dat$baby_meds);abx.op
abx.ip=unique(dat$baby_med_ip);abx.ip

# episode calculation
dat.s=dat[,c(1,13:15)]
head(dat.s)

# drop NA observations
dat.s1=subset(dat.s, is.na(days2_baby_meds_ip)==F)
head(dat.s1)

# https://stackoverflow.com/questions/36187931/dplyr-grouping-and-summarizing-mutating-data-with-rolling-time-windows


# https://stackoverflow.com/questions/14454476/get-the-difference-between-dates-in-terms-of-weeks-months-quarters-and-years

# Convert to Datetime Objects
date_strings = dat.s1$baby_med_ip_date;date_strings[1:2]
datetimes = strptime(date_strings, format = "%m/%d/%Y");datetimes[1:2] # convert to datetime objects

# Difference in Days
# You can use the diff in days to get some of our later answers
diff_in_days = difftime(datetimes[2], datetimes[1], units = "days") # days
diff_in_days
#Time difference of 435.9583 days


# new variable
dat.s1$new=NA


head(dat.s1)

dat.s1$new=ifelse()

# need to first count antibiotics per day

dat.s1 %>% 
  group_by(part_id) %>%
  tally(baby_med_ip_date)
  summarise(count_days = sum(sleep_total), 
            min_sleep = min(sleep_total), 
            max_sleep = max(sleep_total),
            total = n())


df2=data.frame(
  id = c(01,01,01,01,01,02,02,03,04),
  date = c("2015-01-01 11:00", 
           "2015-01-06 13:29", 
           "2015-01-10 12:46", 
           "2015-01-25 14:45",
           "2015-02-15 13:30",
           "2015-01-01 10:00",
           "2015-05-05 15:20",
           "2015-01-01 15:19",
           "2015-08-01 13:15"),
  abx = c("AMPICILLIN","ERYTHROMYCIN","NEOMYCIN","AMPICILLIN","VANCOMYCIN","VANCOMYCIN","NEOMYCIN","PENICILLIN","ERYTHROMYCIN"));df2


id             date          abx    episode
1 2015-01-01 11:00   AMPICILLIN     1 
1 2015-01-06 13:29 ERYTHROMYCIN     1
1 2015-01-10 12:46     NEOMYCIN     1
1 2015-01-25 14:45   AMPICILLIN     2
1 2015-02-15 13:30   VANCOMYCIN     3
2 2015-01-01 10:00   VANCOMYCIN     1
2 2015-05-05 15:20     NEOMYCIN     1
3 2015-01-01 15:19   PENICILLIN     1
4 2015-08-01 13:15 ERYTHROMYCIN     1


df_full<- df2 %>%
  mutate(date=as.Date(date))  %>%
  complete(id, date=seq(from=min(.$date)-30,to=max(.$date), by=1), fill=list(n_widgets=0))
