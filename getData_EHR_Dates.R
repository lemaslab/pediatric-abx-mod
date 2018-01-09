
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #
           
# Author:      Dominick Lemas 
# Date:        January 09 2018
# IRB:
# Description: Import dates that were exported from RedCap to compute days2measure. 
# Data: C:\Users\Dominick\Dropbox (UFL)\IRB\UF\UFHealth\redcap_import\dates

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
# location="djlemas";location
# location="Dominick";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\IRB\\UF\\UFHealth\\redcap_import\\raw_data\\",sep="");work.dir
data.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\IRB\\UF\\UFHealth\\redcap_import\\raw_data\\",sep="");data.dir

# Set Working Directory
setwd(work.dir)
list.files()

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(readxl)
library(tidyverse)
library(data.table)
library(tidyr)
library(dplyr)
library(reshape2)
library(lubridate)

# **************************************************************************** #
# ***************                EHR_dates.xlsx                                              
# **************************************************************************** #      

# file parameters
n_max=10000
data.file.name="EHR_dates.xlsx";data.file.name

# **************************************************************************** #
# ***************                baby_wellness_dates                                              
# **************************************************************************** #

# baby_wellness_dates
#-----------------
# rows: 
# cols:  
# unique id: 
# repeat: 
# ICD9/10: 

# clear slate
# rm(baby.wellness,test, dat, dat.new,new,data,newdata,mdata, mdata.d, mdata.d1, mdata.d2, newdata2)

# read data
baby.wellness=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "baby_wellness_dates", range = NULL, col_names = TRUE,
                   col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                   guess_max = min(1000, n_max));baby.wellness

# data
newdata=baby.wellness
names(newdata);head(newdata)

# melt data
mdata <- melt(newdata, id=c("part_id","redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance","infant_dob"))
head(mdata);str(mdata);dim(mdata)
length(unique(mdata$part_id))  # 30540
length(is.na(mdata$value)==F)  # 2071363
mdata[1:50,]
mdata$redcap_repeat_instrument2=mdata$redcap_repeat_instrument

# remove non-sense rows (no data) 
mdata.d=subset(mdata, is.na(value)==F);dim(mdata.d) # 282053
head(mdata.d);
length(unique(mdata.d$part_id))
str(mdata.d); head(mdata.d)
newdata2 <- mdata.d[order(mdata.d$part_id, mdata.d$redcap_repeat_instrument),]
head(newdata2)
newdata2[1:50,]

# dplyr for data wrangle
#----------------------
dat.new=newdata2 %>%
  group_by(part_id) %>%
  spread(redcap_repeat_instrument, infant_dob) %>%  # makes this variable into its own columns (long to wide)
  select(part_id, redcap_event_name, redcap_repeat_instance,redcap_repeat_instrument2, variable, value, baby_demography) %>%
  mutate(infant_dob = first(baby_demography[!is.na(baby_demography)])) %>%  # Repeat the first observation for birth within each baby
  select(part_id,redcap_repeat_instrument2, redcap_event_name, redcap_repeat_instance, infant_dob, variable, value) %>% # drop and reorder variables
  mutate(date=as.Date(value, format="%Y-%m-%d"))

# check (it works!)
head(dat.new)
dat.new[1:50,]
str(dat.new)

# days_to variable(s)
dat.new$days_to_meas=dat.new$date-as.Date(dat.new$infant_dob,format="%Y-%m-%d")
head(dat.new)
range(dat.new$days_to_meas)
names(dat.new)

# modify redcap import
dat.new.sort=dat.new %>% 
  as_data_frame %>%
  select_("part_id","redcap_repeat_instrument2","redcap_repeat_instance","redcap_event_name","variable","days_to_meas")
names(dat.new.sort);head(dat.new.sort)
is.data.table(dat.new.sort)

# cast
dat.new3=dcast(dat.new.sort, part_id+redcap_repeat_instrument2+redcap_event_name+redcap_repeat_instance~variable)
head(dat.new3)
names(dat.new3)

# modify names (start here)
dat.new4=rename(dat.new3, redcap_repeat_instrument=redcap_repeat_instrument2,
                days2_baby_admit=infant_admit_date,
                days2_baby_vac=infant_immune_date,
                days2_baby_wellvisit=infant_obs_date,
                days2_baby_ht1=infant_ht1_date,
                days2_baby_hc1=infant_hc1_date,
                days2_baby_meds=infant_med_date,
                days2_baby_meds_ip=infant_med_ip_date);names(dat.new4)
head(dat.new4)
dt5=dat.new4

# export data
#-------------
batchSize=10000; # number of rows in single output file
data.file.name.export="baby_wellness_dates";data.file.name.export
out.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\IRB\\UF\\UFHealth\\redcap_import\\03_redcap_import_Jan18\\",sep="");out.dir

chunks=split(dt5, floor(0:(nrow(dt5)-1)/batchSize))
for (i in 1:length(chunks))
{ # second loop
  write.table(chunks[[i]],paste0(out.dir,data.file.name.export,i,'.csv'),row.names=F, sep="\t")
} # end second loop

# clear slate
rm(baby.wellness,chunks,dat.new,dat.new.sort,dat.new3,dat.new4,dt5,mdata,mdata.d,newdata,newdata2)




