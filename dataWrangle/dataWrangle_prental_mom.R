##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        January 31, 2018 
# IRB:
# Description: Data management for link mom-baby data extracted from RedCap. 
# Data: C:\Users\Dominick\Dropbox (UFL)\IRB\UF\UFHealth\redcap_export
# Analysis: Curate data for maternal prenatal visits

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
# location="djlemas";location
# location="Dominick";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\UFHEALTH\\RedCap\\redcap_export\\",sep="");work.dir
data.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\UFHEALTH\\RedCap\\redcap_export\\",sep="");data.dir
out.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\UFHEALTH\\RedCap\\redcap_export\\",sep="");out.dir

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
dat.s=dat[,c("part_id","redcap_repeat_instance","mom_prenat_ht_inch_link","mom_prenat_wt_lb_link","days2_prenatal_apt_link")]
str(dat.s)
head(dat.s)

# subset to include only baby with days2-measure (i.e at least 1 prenatavl visit)
# note need data export with appropriate mom-id
newdata <- subset(dat.s, is.na(dat.s$days2_prenatal_apt_link)=="FALSE")
head(newdata)
str(newdata)

# format variables
newdata$part_id=as.character(newdata$part_id)

# look at data
hist(newdata$days2_prenatal_apt_link)
hist(newdata$redcap_repeat_instance)
head(newdata)

# reshape the data
mdata <- melt(newdata, id=c("part_id","mom_prenat_ht_inch_link","mom_prenat_wt_lb_link","redcap_repeat_instance"))
head(mdata)
mdata[1:50,]

dat.new3=dcast(mdata, part_id+mom_prenat_ht_inch_link+mom_prenat_wt_lb_link~variable+redcap_repeat_instance)
head(dat.new3)