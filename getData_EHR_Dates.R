
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #
           
# Author:      Dominick Lemas 
# Date:        Sept 09 2017
# IRB:
# Description: compute infant "date to occurance" variables for import to RedCap

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

rm(list=ls())
graphics.off()

# Computer
# location="djlemas";location
# location="Dominick";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox\\IRB\\UF\\UFHealth\\redcap_import\\01_import_22July17\\EXPORT\\",sep="");work.dir
data.dir=work.dir
  
setwd(work.dir)
list.files()

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(data.table)
library(tidyr)
library(tidyverse)
library(reshape2)
library(lubridate)
library(dplyr)

# **************************************************************************** #
# ***************                # LOAD DATA                                              
# **************************************************************************** #      

# FIRST DATA SET
##--------------

# file parameters
data.file.name="UFHealth_Dates_DATA_30Aug17.csv";data.file.name

# load data 1
baby.data <- read.csv(paste(data.dir,data.file.name,sep=""))
head(baby.data)
str(baby.data);dim(baby.data)
names(baby.data)

# **************************************************************************** #
# ***************                # FORMAT DATE VARIABLE(S)                                             
# **************************************************************************** # 

# baby_id
baby.data$baby_id=as.character(baby.data$baby_id)

# modify data
dat=baby.data

head(dat)
str(dat)

dat[1,1:5]
str(dat)

# dates
dat[, 5:29] <-sapply(dat[, 5:29], as.character)

dat$dob=as.Date(dat$dob, "%m/%d/%Y")
dat$admit_date=as.Date(dat$admit_date, "%m/%d/%Y")
dat$immune_date=as.Date(dat$immune_date, "%m/%d/%Y")
dat$observation_date=as.Date(dat$observation_date, "%m/%d/%Y")
dat$first_height_date=as.Date(dat$first_height_date, "%m/%d/%Y")
dat$first_head_circumf_date=as.Date(dat$first_head_circumf_date, "%m/%d/%Y")
dat$med_date=as.Date(dat$med_date, "%m/%d/%Y")
dat$med_ip_date=as.Date(dat$med_ip_date, "%m/%d/%Y")
dat$asthma_charge_date=as.Date(dat$asthma_charge_date, "%m/%d/%Y")
dat$food_allergy_charge_date=as.Date(dat$food_allergy_charge_date, "%m/%d/%Y")
dat$ear_infect_charge_date=as.Date(dat$ear_infect_charge_date, "%m/%d/%Y")
dat$eczema_charge_date=as.Date(dat$eczema_charge_date, "%m/%d/%Y")
dat$dermatitis_charge_date=as.Date(dat$dermatitis_charge_date, "%m/%d/%Y")
dat$erythema_charge_date=as.Date(dat$erythema_charge_date, "%m/%d/%Y")
dat$sebaceous_charge_date=as.Date(dat$sebaceous_charge_date, "%m/%d/%Y")
dat$hemangioma_charge_date=as.Date(dat$hemangioma_charge_date, "%m/%d/%Y")
dat$asthma_hospital_admit_date=as.Date(dat$asthma_hospital_admit_date, "%m/%d/%Y")
dat$dermatitis_hosp_admit_date=as.Date(dat$dermatitis_hosp_admit_date, "%m/%d/%Y")
dat$ear_hospital_admit_date=as.Date(dat$ear_hospital_admit_date, "%m/%d/%Y")
dat$asthma_hospital_admit_date=as.Date(dat$asthma_hospital_admit_date, "%m/%d/%Y")
dat$dermatitis_hosp_admit_date=as.Date(dat$dermatitis_hosp_admit_date, "%m/%d/%Y")
dat$ear_hospital_admit_date=as.Date(dat$ear_hospital_admit_date, "%m/%d/%Y")
dat$eczema_hospital_admit_date=as.Date(dat$eczema_hospital_admit_date, "%m/%d/%Y")
dat$food_allergy_hosp_admit_date=as.Date(dat$food_allergy_hosp_admit_date, "%m/%d/%Y")
dat$hemangioma_hosp_admit_date=as.Date(dat$hemangioma_hosp_admit_date, "%m/%d/%Y")
dat$sebaceous_hosp_admit_date=as.Date(dat$sebaceous_hosp_admit_date, "%m/%d/%Y")
dat$obesity_hospital_admit_date=as.Date(dat$obesity_hospital_admit_date, "%m/%d/%Y")
dat$toxicum_hosp_admit_date=as.Date(dat$toxicum_hosp_admit_date, "%m/%d/%Y")

# check
head(dat);names(dat)

# test
test1=dat[,c(1,3:29)];names(test1);head(test1)

# *******************************************c*********************************#
# ***************                # FORMAT DATE OF BIRTH                                            
# **************************************************************************** # 

# melt data
mdata <- melt(test1, id=c("baby_id","redcap_repeat_instrument","redcap_repeat_instance","dob"))
head(mdata)[3];str(mdata);dim(mdata)
length(unique(mdata$baby_id))
length(is.na(mdata$value)==F)
mdata[1:50,]

# remove 
mdata.d=subset(mdata, is.na(value)==F);dim(mdata.d)
head(mdata.d);
length(unique(mdata.d$baby_id))
mdata.d[,"dob.new"]=NA
# mdata.d$dob=as.numeric(mdata.d$dob)
str(mdata.d)

mdata.d1=mdata.d
head(mdata.d1);
dim(mdata.d1)
mdata.d1$redcap_repeat_instrument2=mdata.d1$redcap_repeat_instrument
mdata.d1$dob2=mdata.d1$dob

# drop duplicate: Baby-0004
mdata.d2=mdata.d1[-122281,]

dat.new=mdata.d2 %>%
  group_by(baby_id) %>%
  spread(redcap_repeat_instrument, dob) %>%  # makes this variable into its own columns (long to wide)
  select(baby_id, redcap_repeat_instrument2,redcap_repeat_instance, variable, value,dob2, babybaby) %>%
  mutate(birth = first(babybaby))  # Repeat the first observation for birth within each baby

# check
head(dat.new)
dat.new[1,]

# **************************************************************************** #
# ***************                # days_to variable(s)                                             
# **************************************************************************** #      

dat.new$days_to_meas=dat.new$value-dat.new$birth
head(dat.new)
range(dat.new$days_to_meas)

# **************************************************************************** #
# ***************                # MODIFY FOR REDCAP IMPORT                                              
# **************************************************************************** #      

# drop
names(dat.new)
dat.new2=dat.new[,c(1:4,9)]
head(dat.new2)

# cast
dat.new3=dcast(dat.new2, baby_id+redcap_repeat_instrument2+redcap_repeat_instance~variable)
head(dat.new3)

# add
dat.new3$redcap_event_name=paste("visit_",dat.new3$redcap_repeat_instance,"_arm_1",sep="")
head(dat.new3)
names(dat.new3)

# modify names
dat.new4=rename(dat.new3, redcap_repeat_instrument=redcap_repeat_instrument2,
                          days2_admit=admit_date,
                          days2_vaccine=immune_date,
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

