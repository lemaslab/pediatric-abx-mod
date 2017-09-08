
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #
           
# Author:      Dominick Lemas 
# Date:        Aug 21 2017
# IRB:
# Description: modify food allergy data for import to RedCap

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
# location="djlemas";location
# location="Dominick";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox\\IRB\\UF\\UFHealth\\redcap_import\\01_import_22July17\\baby_billing_codes_CLINIC\\",sep="");work.dir
data.dir=work.dir
  
setwd(work.dir)
list.files()

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(data.table)
library(tidyr)
# library(stringr)
# library(reshape2)

# **************************************************************************** #
# ***************                # LOAD DATA                                              
# **************************************************************************** #      

# FIRST DATA SET
##--------------

# file parameters
data.file.name="baby.clinic_foodallergy.csv";data.file.name

# load data
baby.data <- read.csv(paste(data.dir,data.file.name,sep=""))
head(baby.data)
str(baby.data)
names(baby.data)

# format variables
# dates
# as character
baby.data$food_allergy_charge_date=as.character(baby.data$food_allergy_charge_date)

# **************************************************************************** #
# ***************                # COUNTER VARIABLE                                             
# **************************************************************************** #      

#https://stackoverflow.com/questions/12352378/an-increasing-counter-for-occurence-of-new-values-in-r

# create new data.frame
test=baby.data

# sort by baby_id and date
newdata <- test[order(test$baby_id, test$food_allergy_charge_date),]
head(newdata)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata)                                   # Create data.table
setkeyv(dt, c("baby_id", "food_allergy_charge_date", "food_allergy_icd"))  # Create key for data.table


# create "redcap_event_name" variabledt2 <- unique(dt)                                           # Get only unique rows by key
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "baby_id"]        # Create new variable
head(dt3)
dt3[1:30,]
range(dt3$redcap_repeat_instance) # 1 33
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)

# final check
data.final.01=dt3
table(data.final.01$redcap_event_name)
str(data.final.01)
data.final.01$redcap_event_name=as.factor(data.final.01$redcap_event_name)
levels(data.final.01$redcap_event_name)

# add other import variables
data.final.01$redcap_repeat_instrument="babyclinic_foodallergy";head(data.final.01);dim(data.final.01);str(data.final.01)
data.final.01=as.data.frame(data.final.01)
dim(data.final.01)
head(data.final.01)
data.final.01.drop=data.final.01
head(data.final.01.drop)

# **************************************************************************** #
# ***************                # EXPORT DATA                                              
# **************************************************************************** #      

# file parameters
data.file.name.export="baby.clinic_foodallergy_21Aug17.csv";data.file.name.export
head(data.final.01.drop)

# write file
write.table(data.final.01.drop, file =(paste(data.dir,data.file.name.export,sep="")),row.names=F, sep=";")


