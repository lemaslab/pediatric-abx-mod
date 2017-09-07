
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #
           
# Author:      Dominick Lemas 
# Date:        Aug 30 2017
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

# **************************************************************************** #
# ***************                # LOAD DATA                                              
# **************************************************************************** #      

# FIRST DATA SET
##--------------

# file parameters
data.file.name="UFHealth_Dates_DATA_30Aug17.csv";data.file.name
# data.file.name="example.csv";data.file.name


# load data 1
baby.data <- read.csv(paste(data.dir,data.file.name,sep=""))
head(baby.data)
str(baby.data);dim(baby.data)
names(baby.data)

# load data 2
# data.file.name2="baby.baby.csv";data.file.name
# baby.dob <- read.csv(paste(data.dir,data.file.name2,sep=""))
# head(baby.dob)
# str(baby.dob);dim(baby.dob)
# names(baby.dob)
  # clip data 2
  # baby.dob.final=baby.dob[,c(1,4)]; head(baby.dob.final);dim(baby.dob.final)
  # length(unique(baby.dob.final$baby_id))

# **************************************************************************** #
# ***************                # FORMAT VARIABLE                                             
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

# melt data
mdata <- melt(test1, id=c("baby_id","redcap_repeat_instrument","redcap_repeat_instance","dob"))
head(mdata)[3];str(mdata);dim(mdata)
length(unique(mdata$baby_id))
length(is.na(mdata$value)==F)

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

# drop duplicate: Baby-0004
mdata.d2=mdata.d1[-122281,]

dat.new=mdata.d2 %>%
  group_by(baby_id) %>%
  spread(redcap_repeat_instrument, dob) %>%  # makes this variable into its own columns (long to wide)
  select(baby_id, redcap_repeat_instance, variable, value, babybaby) %>%
  mutate(birth = first(babybaby))  # Repeat the first observation for birth within each baby

# check
head(dat.new)





# subset baby_id & dob  
id.unique=as.character(unique(baby.dob.final$baby_id));length(id.unique) # 16684 observations
length(unique(mdata.d$baby_id)) # 145 observations
baby.dob.final.key=subset(baby.dob.final, baby.dob.final$baby_id%in%mdata.d$baby_id);dim(baby.dob.final.key)
length(unique(baby.dob.final.key$baby_id)) # 145 observations
str(baby.dob.final.key)
baby.dob.final.key$baby_id=as.character(baby.dob.final.key$baby_id)
baby.dob.final.key$dob=as.character(baby.dob.final.key$dob)

# test2=mdata.d[1:1000,]
# mdata=test2

# create loop
id.index=as.character(unique(baby.dob.final.key$baby_id));id.index
total.index<-length(mdata.d$baby_id);total.index
mdata.d[,"dob.new"]=NA
str(mdata.d)
head(mdata.d)
dim(mdata.d)


# start loops
for(i in 1:length(id.index)){
  for (j in 1:total.index){ 
  # Create column index
    mdata.d$dob.new[j]=ifelse(mdata.d$baby_id[j]%in%baby.dob.final.key$baby_id[i], baby.dob.final.key$dob[i], mdata.d$dob.new[j])
  }
}

# check output
head(mdata.d)
mdata[1:60,]
str(mdata.d)

# format new variables for time
mdata.d$dob.new=as.Date(mdata$dob.new, "%m/%d/%Y")

# calculate days to measure
mdata$days_to_measure=mdata$value-mdata$dob.new

head(mdata)

# *******************************************c********************************* #
# ***************                # days_to variable(s)                                             
# **************************************************************************** #      

        


# **************************************************************************** #
# ***************                # EXPORT DATA                                              
# **************************************************************************** #      

# file parameters
data.file.name.export="baby.dates_07Sept17.csv";data.file.name.export
head(mdata.d)

# write file
write.table(mdata.d, file =(paste(data.dir,data.file.name.export,sep="")),row.names=F, sep=";")


