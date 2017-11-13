##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #
           
# Author:      Dominick Lemas 
# Date:        November 13 2017 
# IRB:
# Description: Analysis of UFHealth data. 
# Data: C:\Users\Dominick\Dropbox (UFL)\IRB\UF\UFHealth\data6

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
# location="djlemas";location
# location="Dominick";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\IRB\\UF\\UFHealth\\data6\\",sep="");work.dir
data.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\IRB\\UF\\UFHealth\\data6\\",sep="");data.dir


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

## Mom Prenatals.xlsx
##-------------------

# file parameters
n_max=10000
data.file.name="Mom Prenatals.xlsx";data.file.name

# mom 
#-----
mom.mom.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Mom", range = NULL, col_names = TRUE,
          col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
          guess_max = min(1000, n_max));mom.mom.dat

# mom apt
#--------
mom.prenatal.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Prenatals by Appt", range = NULL, col_names = TRUE,
                   col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                   guess_max = min(1000, n_max));mom.prenatal.dat


## Mom Medications.xlsx
##-------------------

# file parameters
n_max=10000
data.file.name="Mom Medications.xlsx";data.file.name

# mom abx ip
#-----------
mom.abxip.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Mom Antibiotics IP Admin", range = NULL, col_names = TRUE,
                      col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                      guess_max = min(1000, n_max));mom.abxip.dat

# mom abx script
#---------------
mom.abxscript.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Mom Antibiotics Prescription", range = NULL, col_names = TRUE,
                        col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                        guess_max = min(1000, n_max));mom.abxscript.dat

# mom meds ip
#-----------
mom.medip1.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Mom IP Medications", range = NULL, col_names = TRUE,
                        col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                        guess_max = min(1000, n_max));mom.medip1.dat


# mom abx ip 2
#-----------
mom.medip2.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Mom IP Medications(1)", range = NULL, col_names = FALSE,
                        col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                        guess_max = min(1000, n_max));mom.medip2.dat

# mom prescriptions
#------------------
mom.script.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Mom Prescriptions", range = NULL, col_names = TRUE,
                        col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                        guess_max = min(1000, n_max));mom.script.dat


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








           



