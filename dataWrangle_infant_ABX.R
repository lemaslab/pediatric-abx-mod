##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        March 15, 2018 
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
head(dat);names(dat)
dat.s=dat[,c(1,11:15)]
head(dat.s)

# drop NA observations
dat.s2=subset(dat.s, is.na(days2_baby_meds_ip)==F)
head(dat.s2)
dat.s2$baby_med_ip_date=as.character(dat.s2$baby_med_ip_date)
str(dat.s2);head(dat.s2)
# write.csv(dat.s2, file="test.csv", row.names=F) # output and modified for test data
  #Read test Data
    # data.file.name="test2.csv";data.file.name
    # data.file.path=paste0(data.dir,"\\",data.file.name);data.file.path
    # ufhealth.test<- read.csv(data.file.path);ufhealth.test
    # test=ufhealth.test

# compute episode variable
head(dat.s2)
dat2=dat.s2 %>%
  group_by(part_id) %>%
  mutate(date = as.Date(baby_med_ip_date, format="%m/%d/%Y")) %>%
  mutate(date1=first(date)) %>%
  mutate(obsvn=date-date1) %>%
  mutate(abx_episode = cumsum(c(1,diff(obsvn)>=7))) %>%
  select(part_id,baby_mar_action_ip,baby_med_code_ip,baby_med_ip,days2_baby_meds_ip,baby_med_ip_date,abx_episode)

 
