##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Keval Patel 
# Date:        March 13, 2018 
# IRB:
# Description: Data management for link mom-baby data extracted from RedCap. 
# Data: C:\Users\Dominick\Dropbox (UFL)\IRB\UF\UFHealth\redcap_export
# Analysis: Curate data for mode of delivery and infant outcomes

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
location="K";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox\\RedCap\\redcap_export\\",sep="");work.dir
data.dir=paste("C:\\Users\\",location,"\\Dropbox\\RedCap\\redcap_export\\",sep="");data.dir
# out.dir=paste("C:\\Users\\",location,"\\Dropbox\\RedCap\\redcap_export\\",sep="");out.dir


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
# ***************      UFHealth_EHR_MomBaby_Prenatal_16Jan18.csv                                              
# **************************************************************************** # 

# UFHealth_EHR_MomBaby_Prenatal_16Jan18.csv
#-----------------
# rows: 
# cols:  
# unique id: 
# repeat: 
# ICD9/10: 

#Read Data
data.file.name="UFHealth_EHR_MomBaby_Prenatal_16Jan18.csv";data.file.name
data.file.path=paste0(data.dir,"\\",data.file.name);data.file.path
ufhealth.mod<- read.csv(data.file.path);ufhealth.mod

# look at data
dat=ufhealth.mod
head(dat); str(dat); names(dat)

# limit to just a few variables
dat.s=dat[,c("part_id","delivery_mode","baby_birth_wt_gr")]
str(dat.s)

# subset to include only babies 
test=is.na(dat.s$delivery_mode);test[1:30]
which(is.na(dat.s$baby_birth_wt_gr))
newdata <- subset(dat.s, is.na(dat.s$delivery_mode)=='FALSE' & is.na(dat.s$baby_birth_wt_gr)=="FALSE")
head(newdata)

# format data
newdata$part_id=as.character(newdata$part_id)
newdata$delivery_mode=as.character(newdata$delivery_mode)
str(newdata)

# recode variables
unique(newdata$delivery_mode) # how many unique entries for MOD; 24 (below)

# [1]* "Vaginal&_Spontaneous_Delivery"   
# [2]* "C-Section&_Low_Transverse"       
# [3]* "Vaginal&_Vacuum_(Extractor)"     
# [4]* "C-Section&_Unspecified"         
# [5]* "Vaginal&_Forceps"                 
# [6]* "Vertical_C-Section"              
# [7]* "C-Section&_Low_Vertical"         
# [8]* "NOT_INCLUDED_IN_ORIGINAL_SOURCE"
# [9]* "VBAC&_Spontaneous"               
# [10]* "C-Section&_Classical"            
# [11]* "Vaginal&_Breech"                 
# [12]* "Extramural_Delivery"            
# [13]* "Other" 

# condense down to binary (vaginal/c-section)
newdata$mod=NA
newdata$mod=ifelse(newdata$delivery_mode=="Vaginal&_Spontaneous_Delivery","vaginal",newdata$mod)
newdata$mod=ifelse(newdata$delivery_mode=="C-Section&_Low_Transverse","c-section",newdata$mod)
newdata$mod=ifelse(newdata$delivery_mode=="Vaginal&_Vacuum_(Extractor)","vaginal",newdata$mod)
newdata$mod=ifelse(newdata$delivery_mode=="C-Section&_Unspecified","c-section",newdata$mod)
newdata$mod=ifelse(newdata$delivery_mode=="Vaginal&_Forceps","vaginal",newdata$mod)
newdata$mod=ifelse(newdata$delivery_mode=="Vertical_C-Section","vaginal",newdata$mod)
newdata$mod=ifelse(newdata$delivery_mode=="C-Section&_Low_Vertical","c-section",newdata$mod)
newdata$mod=ifelse(newdata$delivery_mode=="NOT_INCLUDED_IN_ORIGINAL_SOURCE",NA,newdata$mod)
newdata$mod=ifelse(newdata$delivery_mode=="VBAC&_Spontaneous","vaginal",newdata$mod)
newdata$mod=ifelse(newdata$delivery_mode=="C-Section&_Classical","c-section",newdata$mod)
newdata$mod=ifelse(newdata$delivery_mode=="Vaginal&_Breech","vaginal",newdata$mod)
newdata$mod=ifelse(newdata$delivery_mode=="Extramural_Delivery",NA,newdata$mod)
newdata$mod=ifelse(newdata$delivery_mode=="Other",NA,newdata$mod)
head(newdata) # did recode work? yes
unique(newdata$mod)
