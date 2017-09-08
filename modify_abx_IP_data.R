
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #
           
# Author:      Dominick Lemas 
# Date:        Aug 15 2017
# IRB:
# Description: modify perscription data for import to RedCap

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
# location="djlemas";location
# location="Dominick";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox\\IRB\\UF\\UFHealth\\redcap_import\\01_import_22July17\\baby\\baby.abx_IP\\",sep="");work.dir
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
data.file.name="baby.antibiotics_IP_administration.csv";data.file.name

# load data
baby.abx <- read.csv(paste(data.dir,data.file.name,sep=""))
head(baby.abx)
str(baby.abx)
names(baby.abx)

# format variables
# dates
# as character
baby.abx$taken_datetime=as.character(baby.abx$taken_datetime)
baby.abx$med_ip_date=baby.abx$taken_datetime
# split the date & times
abx.new=baby.abx %>% separate(taken_datetime, c("med_ip_date", "med_ip_time_EST"), " "); head(abx.new)

# characters
abx.new$medication=gsub(" ","_",abx.new$medication) 
abx.new$medication=gsub(",","&",abx.new$medication_IP) 
head(abx.new);str(abx.new)

# **************************************************************************** #
# ***************                # COUNTER VARIABLE                                             
# **************************************************************************** #      

#https://stackoverflow.com/questions/12352378/an-increasing-counter-for-occurence-of-new-values-in-r

# create new data.frame
test=abx.new

# sort by baby_id and date
newdata <- test[order(test$baby_id, test$med_ip_date),]
head(newdata)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata)                                   # Create data.table
setkeyv(dt, c("baby_id", "med_order_number", "MAR_action", "med_code", "med_ip_date", "medication", "med_ip_date"))  # Create key for data.table


# create "redcap_event_name" variabledt2 <- unique(dt)                                           # Get only unique rows by key
dt3 <- dt2[, redcap_repeat_instance := seq_len(.N), by = "baby_id"]        # Create new variable
head(dt3)
dt3[1:30,]
range(dt3$redcap_repeat_instance) # 1 537
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
data.final.01$redcap_repeat_instrument="babyantibiotics_ip";head(data.final.01);dim(data.final.01);str(data.final.01)
data.final.01=as.data.frame(data.final.01)
dim(data.final.01)
head(data.final.01)
data.final.01.drop=data.final.01[,c(1:4,7:11)]
head(data.final.01.drop)

# **************************************************************************** #
# ***************                # EXPORT DATA                                              
# **************************************************************************** #      

# file parameters
data.file.name.export="baby.abx_IP_18Aug17.csv";data.file.name.export
head(data.final.01.drop)

# write file
write.table(data.final.01.drop, file =(paste(data.dir,data.file.name.export,sep="")),row.names=F, sep=";")


