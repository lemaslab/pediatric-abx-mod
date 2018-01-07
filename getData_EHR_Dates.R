
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
library(data.table)
library(tidyr)
library(dplyr)

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

# read data
baby.wellness=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "baby_wellness_dates", range = NULL, col_names = TRUE,
                   col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                   guess_max = min(1000, n_max));baby.wellness

# data
dat=baby.wellness
head(dat); str(dat)

# melt data
mdata <- melt(dat, id=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name","infant_dob"))
head(mdata)[3];str(mdata);dim(mdata)
length(unique(mdata$part_id))
length(is.na(mdata$value)==F)
mdata[1:50,]

# remove 
mdata.d=subset(mdata, is.na(value)==F);dim(mdata.d)
head(mdata.d);
length(unique(mdata.d$part_id))
mdata.d[,"dob.new"]=NA
# mdata.d$dob=as.numeric(mdata.d$dob)
str(mdata.d)

mdata.d1=mdata.d
head(mdata.d1);
dim(mdata.d1)
mdata.d1$redcap_repeat_instrument2=mdata.d1$redcap_repeat_instrument
mdata.d1$dob2=mdata.d1$infant_dob

# drop duplicate: Baby-0004
mdata.d2=mdata.d1[-122281,]

dat.new=mdata.d2 %>%
  group_by(part_id) %>%
  spread(redcap_repeat_instrument, infant_dob) %>%  # makes this variable into its own columns (long to wide)
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

