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
head(dat);names(dat)
dat.s=dat[,c(1,15,14)]
head(dat.s)

# drop NA observations
dat.s1=subset(dat.s, is.na(days2_baby_meds_ip)==F)
head(dat.s1)
dat.s2=dat.s1[c(1:1000),c(1:2)];dat.s2
head(dat.s2)
dat.s2[1:30,]
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
  mutate(ith = cumsum(c(1,diff(obsvn)>=7)))

 #write.csv(dat2, file="test_output.csv", row.names=F)


  mutate(temp2=ifelse(date%in%date1,1,NA)) %>%
  mutate(lag.date = dplyr::lag(date, n=1, default=NA)) %>%
  mutate(dif=as.numeric(date-lag.date)) %>%
  select(part_id,date,lag.date,dif) %>%
  mutate(diff.2=if_else(dif>0,dif,0,missing=0)) %>%
  mutate(ep.logic=if_else(diff.2<7,TRUE,FALSE)) %>%
  #add_tally(ep.logic==F) %>%
  mutate(ep=1) %>%
  #mutate(ep1=if_else(diff.2==0,ep,dplyr::lag(ep+1))) %>%
  mutate
  mutate(count=)
  mutate(ep2=if_else(ep1==2,seq(0,10,by=1),ep1))
  
dat3=dat2 %>%
  group_by(part_id,ep1) %>%
  mutate(count = seq(n()))
  
  add_tally(ep.logic==F) %>%
  mutate(ep1=if_else(ep.1==0,dplyr::lag(ep.1+1),ep.1));dat2

# write.csv(dat2, file="test.csv")
dat2=newdata
dat3=subset(dat2, part_id=="Baby-0141")
dat4=dat3[95:110,]

data <- data.frame(v = c(1,1,1,2,2,2,2,3,4,4))
data %>%
  group_by(v) %>%
  #mutate(v.counter = row_number())
 mutate(cumsum = cumsum(v))

OrderHistory %>%
  group_by(customer,item) %>%
  mutate(count = seq(n()))


dat2=dat4

# create index
#-------------
myIndex=length(index);myIndex
i=3
for (i in 1:length(index))
{ # second loop
  temp1=subset(dat2, part_id==index[i])
  # dates=as.Date(temp1$baby_med_ip_date,format="%Y-%m-%d")
  dates=aindex=unique(as.character(dat2$part_id));index
s.Date(temp1$baby_med_ip_date,format="%m/%d/%Y") # real data
  obs=length(dates)
  for(j in 1:length(dat4$baby_med_ip_date)){
    start_date=dat4$baby_med_ip_date[j]
    past_date=start_date-7
    #future_date=start_date+7
    date_range=seq(past_date, start_date, by="days")
    temp1$episode=ifelse(date[j]%in%date_range,temp1$temp2,temp1$temp2+1)
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
           "2015-01-25 12:46", 
           "2015-01-25 14:45",
           "2015-01-25 13:30",
           "2015-01-26 10:00",
           "2015-01-27 15:20",
           "2015-02-01 15:19",
           "2015-02-01 13:15",
           "2015-02-08 13:15",
           "2015-02-25 13:15"),
  abx = c("AMPICILLIN","ERYTHROMYCIN","NEOMYCIN","AMPICILLIN","VANCOMYCIN","VANCOMYCIN","NEOMYCIN","PENICILLIN","ERYTHROMYCIN","NEOMYCIN","PENICILLIN"));df2
newdata=rename(dat2, part_id = id, baby_med_ip_date=date, baby_med_ip=abx);newdata
newdata$baby_med_ip_date=as.Date(newdata$baby_med_ip_date,format="%Y-%m-%d")


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
         
