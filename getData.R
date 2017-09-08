##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #
           
# Author:      Dominick Lemas 
# Date:        May 09 2017; July 22 2017 
# IRB:
# Description: Analysis of UFHealth data. 
# Data: 03_dump_16June17 (# Baby_16June17, 
                          # Baby_Billing_Codes_Clinic_16June17, 
                          # Baby_Billing_Codes_Hospital_16June17)

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
# location="djlemas";location
# location="Dominick";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox\\03_Analysis\\UFHealth\\",sep="");work.dir
data.dir=paste("C:\\Users\\",location,"\\Dropbox\\03_Analysis\\UFHealth\\data\\03_dump_16June17\\",sep="");data.dir
table.dir=
figure.dir=

# Set Working Directory
setwd(work.dir)
list.files()


# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(readxl)
# library(plyr)

##--------------
# **************************************************************************** #
# ***************                Load Data                                              
# **************************************************************************** #      

## FIRST DATA SET
##--------------

# file parameters
n_max=10000
data.file.name="Baby_16June17.xlsx";data.file.name

# baby delivery
#--------------
baby.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = NULL, range = NULL, col_names = TRUE,
          col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
          guess_max = min(1000, n_max));baby.dat

# baby wellness
#--------------
wellness.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Well Visit", range = NULL, col_names = TRUE,
                   col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                   guess_max = min(1000, n_max));wellness.dat


# **************************************************************************** #
# ***************                Merge df & compute variables                                             
# **************************************************************************** #      

# merge delivery to wellness
#---------------------------
delivery.wellness.dat=merge(baby.dat, wellness.dat, by=intersect(names(baby.dat), names(wellness.dat)),
      by.x="Baby-Id", by.y="Baby-Id")
head(delivery.wellness.dat)

# Compute days after birth
#-------------------------
delivery.wellness.dat$days_after_birth=delivery.wellness.dat$Observation_Date-delivery.wellness.dat$DOB
range(delivery.wellness.dat$days_after_birth)
hist(as.numeric(delivery.wellness.dat$days_after_birth))

# compute weeks after birth
delivery.wellness.dat$weeks_after_birth=delivery.wellness.dat$days_after_birth/7
range(delivery.wellness.dat$weeks_after_birth)
hist(as.numeric(delivery.wellness.dat$weeks_after_birth))

# computer years after birth
#---------------------------
delivery.wellness.dat$years_after_birth=delivery.wellness.dat$days_after_birth/365
range(delivery.wellness.dat$years_after_birth)
hist(as.numeric(delivery.wellness.dat$years_after_birth))

# compute mode_of_delivery (create list, alphabetize, count. this will ensure you are not missing something below)
#-------------------------
unique(delivery.wellness.dat$Delivery_Mode)
delivery.wellness.dat$mode_of_delivery=NA
delivery.wellness.dat$mode_of_delivery=ifelse(delivery.wellness.dat$Delivery_Mode=="Vaginal, Spontaneous Delivery","vaginal",delivery.wellness.dat$mode_of_delivery)
delivery.wellness.dat$mode_of_delivery=ifelse(delivery.wellness.dat$Delivery_Mode=="C-Section, Low Transverse","c-section",delivery.wellness.dat$mode_of_delivery)
delivery.wellness.dat$mode_of_delivery=ifelse(delivery.wellness.dat$Delivery_Mode=="C-Section, Unspecified","c-section",delivery.wellness.dat$mode_of_delivery)
delivery.wellness.dat$mode_of_delivery=ifelse(delivery.wellness.dat$Delivery_Mode=="Vaginal, Forceps","vaginal",delivery.wellness.dat$mode_of_delivery)
delivery.wellness.dat$mode_of_delivery=ifelse(delivery.wellness.dat$Delivery_Mode=="Vaginal, Vacuum (Extractor)","vaginal",delivery.wellness.dat$mode_of_delivery)
delivery.wellness.dat$mode_of_delivery=ifelse(delivery.wellness.dat$Delivery_Mode=="NOT INCLUDED IN ORIGINAL SOURCE",NA,delivery.wellness.dat$mode_of_delivery)
delivery.wellness.dat$mode_of_delivery=ifelse(delivery.wellness.dat$Delivery_Mode=="Vaginal, Breech","vaginal",delivery.wellness.dat$mode_of_delivery)
delivery.wellness.dat$mode_of_delivery=ifelse(delivery.wellness.dat$Delivery_Mode=="Vertical C-Section","c-section",delivery.wellness.dat$mode_of_delivery)
delivery.wellness.dat$mode_of_delivery=ifelse(delivery.wellness.dat$Delivery_Mode=="C-Section, Low Vertical","c-section",delivery.wellness.dat$mode_of_delivery)
delivery.wellness.dat$mode_of_delivery=ifelse(delivery.wellness.dat$Delivery_Mode=="C-Section, Classical","c-section",delivery.wellness.dat$mode_of_delivery)
delivery.wellness.dat$mode_of_delivery=ifelse(delivery.wellness.dat$Delivery_Mode=="Extramural Delivery",NA,delivery.wellness.dat$mode_of_delivery)
delivery.wellness.dat$mode_of_delivery=ifelse(delivery.wellness.dat$Delivery_Mode=="VBAC, Spontaneous","vaginal",delivery.wellness.dat$mode_of_delivery)
# check
names(delivery.wellness.dat)
delivery.wellness.dat[,c(6,18)]

# compute gestational age
#------------------------
unique(delivery.wellness.dat$Gestational_Age)
# is there an r package to convert to numerical

# **************************************************************************** #
# ***************                Mode of Delivery Analysis                                              
# **************************************************************************** #      

head(delivery.wellness.dat)
table(delivery.wellness.dat$mode_of_delivery)
table(delivery.wellness.dat$mode_of_delivery, delivery.wellness.dat$Race)

vaginal=subset(delivery.wellness.dat, mode_of_delivery=="vaginal" & years_after_birth>3,select=c(`Weight (kgs)`, years_after_birth, `Baby-Id`))
head(vaginal)








           



