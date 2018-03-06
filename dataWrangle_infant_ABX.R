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

# mock data
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

# function
grpno <- function(x) cumsum(c(TRUE, diff(x) >=7 ))
transform(df2, episode = ave(as.numeric(as.Date(date)), id, FUN = grpno))

# or with dplyr
df2 %>%
  group_by(id) %>%
  mutate(episode = date %>% as.Date %>% as.numeric %>% grpno) %>%
  ungroup

# real data
head(dat.s1)
test=dat.s1 %>%
  group_by(part_id) %>%
  mutate(date = as.Date(baby_med_ip_date, format="%m/%d/%Y")) %>% 
  mutate(episode=which(first(date)==1)
         
test2=test %>%
  group_by(part_id) %>%
  mutate(test=first(date)==T)
head(test)
test2[,c(1,3:5)]
test2$test

