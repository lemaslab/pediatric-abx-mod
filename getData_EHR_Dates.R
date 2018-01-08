
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #
           
# Author:      Dominick Lemas 
# Date:        Sept 09 2017
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
                          days2_admit=admit_date,
                          days2_inf_vaccine=infant_immune_date,
                          days2_wellvisit=observation_date,
                          days2_ht1=first_height_date,
                          days2_hc1=first_head_circumf_date,
                          days2_meds=med_date,
                          days2_meds_ip=med_ip_date,
                          days2_asthma_clinic=asthma_charge_date,
                          days2_fa_clinic=food_allergy_charge_date,
                          days2_ear_clinic=ear_infect_charge_date,
                          days2_eczema_clinic=eczema_charge_date,
                          days2_dermatitis_clinic=dermatitis_charge_date,
                          days2_erythema_clinic=erythema_charge_date,
                          days2_sebaceous_clinic=sebaceous_charge_date,
                          days2_hemangioma_clinic=hemangioma_charge_date,
                          days2_asthma_hosp=asthma_hospital_admit_date,
                          days2_dermatitis_hosp=dermatitis_hosp_admit_date,
                          days2_ear_hosp=ear_hospital_admit_date,
                          days2_eczema_hosp=eczema_hospital_admit_date,
                          days2_fa_hosp=food_allergy_hosp_admit_date,
                          days2_hemangioma_hosp=hemangioma_hosp_admit_date,
                          days2_sebaceous_hosp=sebaceous_hosp_admit_date,
                          days2_obesity_hosp=obesity_hospital_admit_date,
                          days2_erythema_hosp=toxicum_hosp_admit_date);names(dat.new4)
head(dat.new4)
dat.new5=dat.new4[,c(1:3,28,4:27)];names(dat.new5)
head(dat.new5)

# **************************************************************************** #
# ***************                # EXPORT DATA                                              
# **************************************************************************** #
# file parameters
data.file.name.export="baby.dates_08Sept17.csv";data.file.name.export
head(dat.new5)

# write file
write.table(dat.new5, file =(paste(data.dir,data.file.name.export,sep="")),row.names=F, sep=";")

