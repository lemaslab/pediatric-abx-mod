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

# episode calculation (remove abx names for moment)
dat.s=dat[,c(1,15,13,14)]
head(dat.s)

# drop NA observations
dat.s1=subset(dat.s, is.na(days2_baby_meds_ip)==F)
head(dat.s1)
dat.s2=dat.s1[c(1:1000),c(1:3)];dat.s2
head(dat.s2)

# compute episode variable
head(dat.s2)
dat2=dat.s2 %>%
  group_by(part_id) %>%
  mutate(date = as.Date(baby_med_ip_date, format="%m/%d/%Y")) %>%
  mutate(temp1=first(date)) %>%
  mutate(temp2=ifelse(date%in%temp1,1,NA)) %>%

# create index
#-------------
index=unique(as.character(dat2$part_id));index
myIndex=length(index);myIndex
i=3
for (i in 1:length(index))
{ # second loop
  temp1=subset(dat2, part_id==index[i])
  dates=as.Date(temp1$baby_med_ip_date,format="%m/%d/%Y")
  obs=length(dates)
  for (j in 1:length(dates)){
    start_date=dates[j]
    past_date=start_date-7
    #future_date=start_date+7
    date_range=seq(past_date, start_date, by="days")
    dat2$episode=ifelse(start_date%in%date_range,dat2$temp2,dat2$temp2+1)
  }
} # end second loop

dat2[20:40,]

write.csv(dat2,file="test.csv")


  
  
  
  mutate(temp3=ifelse(temp2==1,1, ifelse(temp2==NA, )))

# mock data
df2=data.frame(
  id = c("A1","A2","A2","A3","A3","A3","A3","A3","A3","A3","A3"),
  date = c("2015-01-01 11:00", 
           "2015-01-05 13:29", 
           "2015-01-10 12:46", 
           "2015-01-25 14:45",
           "2015-01-25 13:30",
           "2015-01-26 10:00",
           "2015-01-27 15:20",
           "2015-02-01 15:19",
           "2015-02-01 13:15",
           "2015-02-08 13:15",
           "2015-02-25 13:15"),
  abx = c("AMPICILLIN","ERYTHROMYCIN","NEOMYCIN","AMPICILLIN","VANCOMYCIN","VANCOMYCIN","NEOMYCIN","PENICILLIN","ERYTHROMYCIN","NEOMYCIN","PENICILLIN"));df2

# want
id             date          abx     episode
1  A1 2015-01-01 11:00   AMPICILLIN   1
2  A2 2015-01-05 13:29 ERYTHROMYCIN   1
3  A2 2015-01-10 12:46     NEOMYCIN   1
4  A3 2015-01-25 14:45   AMPICILLIN   1  
5  A3 2015-01-25 13:30   VANCOMYCIN   1
6  A3 2015-01-26 10:00   VANCOMYCIN   1
7  A3 2015-01-27 15:20     NEOMYCIN   1
8  A3 2015-02-01 15:19   PENICILLIN   1
9  A3 2015-02-01 13:15 ERYTHROMYCIN   1
10 A3 2015-02-08 13:15     NEOMYCIN   2
11 A3 2015-02-25 13:15   PENICILLIN   3


# function
grpno <- function(x) cumsum(c(TRUE, diff(x) >=7 ))
transform(df2, episode = ave(as.numeric(as.Date(date)), id, FUN = grpno))

# or with dplyr
df2 %>%
  group_by(id) %>%
  mutate(episode = date %>% as.Date %>% as.numeric %>% grpno) %>%
  ungroup

# real data
head(dat.s2)

# function
grpno <- function(x) cumsum(c(TRUE, diff(x) >=7 ))
transform(dat.s2, episode = ave(as.numeric(as.Date(baby_med_ip_date)), part_id, FUN = grpno))


test=dat.s1 %>%
  group_by(part_id) %>%
  mutate(date = as.Date(baby_med_ip_date, format="%m/%d/%Y")) %>% 
  mutate(episode = date %>% as.Date %>% as.numeric) %>%
  ungroup
         
