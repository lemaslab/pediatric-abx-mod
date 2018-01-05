##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #
           
# Author:      Dominick Lemas 
# Date:        November 30, 2017 (START HERE)
# IRB:
# Description: Analysis of UFHealth maternal prenatal medication data 
# Data: C:\Users\Dominick\Dropbox (UFL)\IRB\UF\UFHealth\redcap_import

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
# ***************                Mom Medications.xlsx                                              
# **************************************************************************** # 

# file parameters
n_max=10000
data.file.name="Mom Medications.xlsx";data.file.name

# **************************************************************************** #
# ***************                mom_antibiotics_ip                                              
# **************************************************************************** #

mom.abxip.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Mom Antibiotics IP Admin", range = NULL, col_names = TRUE,
                      col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                      guess_max = min(1000, n_max));mom.abxip.dat

# Data Description
#-----------------
# rows: 
# cols: 
# unique id: 
# repeat: 
# ICD9/10: 

# dates
#------
mom.abxip.dat$`Taken Datetime`=as.character(mom.abxip.dat$`Taken Datetime`)
#mom.abxip.dat$mom_abxip_date=as.POSIXlt(mom.abxip.dat$`Taken Datetime`)
str(mom.abxip.dat); head(mom.abxip.dat)
mom.abxip.dat[1:5,5:7]

# create new data.frame
test=mom.abxip.dat

# sort by id and date
newdata=rename(test, part_id = `Mom ID`, mom_abxip_date=`Taken Datetime`,mom_abxip_action= `MAR Action`, mom_prenat_abx=Antibiotics)
head(newdata)
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$mom_abxip_date,format='%Y-%m-%d %H:%M:%S')),]
head(newdata2)
names(newdata2)=tolower(names(newdata2))

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="mom_antibiotics_ip"
names(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id","mom_abxip_date","mom_abxip_action","mom_prenat_abx","redcap_repeat_instrument"))  # Create key for data.table
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        # Create new variable
# dt3[c(1:15),c(1,2,6)]
range(dt3$redcap_repeat_instance) # 574

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3); unique(dt3$redcap_event_name)
names(dt3)

# characters
dt3$mom_prenat_abx=gsub(" ","_",dt3$mom_prenat_abx) 
dt3$mom_prenat_abx=gsub(",","&",dt3$mom_prenat_abx) 
head(dt3)

# order columns for export
col.names=names(dt3);col.names
col.first=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name");col.first
col.next=subset(col.names, !(col.names%in%col.first));col.next
colFixed=append(col.first, col.next, after=length(col.first));colFixed
dt4=setcolorder(dt3, colFixed)
names(dt4);head(dt4)
dt5=dt4

# export data
#-------------
batchSize=10000; # number of rows in single output file
data.file.name.export=as.character(dt5[2,2]);data.file.name.export
out.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\IRB\\UF\\UFHealth\\redcap_import\\02_redcap_import_Nov17\\",sep="");out.dir

chunks=split(dt5, floor(0:(nrow(dt5)-1)/batchSize))
for (i in 1:length(chunks))
{ # second loop
  write.table(chunks[[i]],paste0(out.dir,data.file.name.export,i,'.csv'),row.names=F, sep=";")
} # end second loop

# clear slate
rm(baby.dat, dat, newdata, newdata2, newdata3, dt, dt3, dt4, dt5)

# **************************************************************************** #
# ***************                mom_antibiotics                                              
# **************************************************************************** #

mom.abxscript.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Mom Antibiotics Prescription", range = NULL, col_names = TRUE,
                        col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                        guess_max = min(1000, n_max));mom.abxscript.dat

# Data Description
#-----------------
# rows: 
# cols: 
# unique id: 
# repeat: 
# ICD9/10: 

# dates
mom.abxscript.dat$`Order Datetime`=as.character(mom.abxscript.dat$`Order Datetime`)
str(mom.abxscript.dat); head(mom.abxscript.dat)

# create new data.frame
test=mom.abxscript.dat

# sort by id and date
newdata=rename(test, part_id = `Mom ID`, mom_abx2_date=`Order Datetime`, mom_prenat_abx2=Antibiotics)
head(newdata)
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$mom_abx2_date,format='%Y-%m-%d %H:%M:%S')),]
head(newdata2)
names(newdata2)=tolower(names(newdata2))

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="mom_antibiotics_rx"
names(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id","mom_abx2_date","mom_prenat_abx2"))  # Create key for data.table
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        # Create new variable
range(dt3$redcap_repeat_instance) # 55

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3); unique(dt3$redcap_event_name)
names(dt3)

# characters
dt3$mom_prenat_abx2=gsub(" ","_",dt3$mom_prenat_abx2) 
head(dt3)

# order columns for export
col.names=names(dt3);col.names
col.first=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name");col.first
col.next=subset(col.names, !(col.names%in%col.first));col.next
colFixed=append(col.first, col.next, after=length(col.first));colFixed
dt4=setcolorder(dt3, colFixed)
names(dt4);head(dt4)
dt5=dt4

# export data
#-------------
batchSize=10000; # number of rows in single output file
data.file.name.export=as.character(dt5[2,2]);data.file.name.export
out.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\IRB\\UF\\UFHealth\\redcap_import\\02_redcap_import_Nov17\\",sep="");out.dir

chunks=split(dt5, floor(0:(nrow(dt5)-1)/batchSize))
for (i in 1:length(chunks))
{ # second loop
  write.table(chunks[[i]],paste0(out.dir,data.file.name.export,i,'.csv'),row.names=F, sep=";")
} # end second loop

# clear slate
rm(baby.dat, dat, newdata, newdata2, newdata3, dt, dt3, dt4, dt5)

# **************************************************************************** #
# ***************                mom_medications_ip 1 & 2                                             
# **************************************************************************** #

# data1
mom.medip1.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Mom IP Medications", range = NULL, col_names = TRUE,
                        col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                        guess_max = min(1000, n_max));mom.medip1.dat

# data 2
mom.medip2.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Mom IP Medications(1)", range = NULL, col_names = TRUE,
                        col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                        guess_max = min(1000, n_max));mom.medip2.dat

# combine datasets
mom.meds.dat=bind_rows(mom.medip1.dat,mom.medip2.dat);mom.meds.dat

# Data Description
#-----------------
# rows: 
# cols: 
# unique id: 
# repeat: 
# ICD9/10: 

# dates
mom.meds.dat$`Taken Datetime`=as.character(mom.meds.dat$`Taken Datetime`)
str(mom.meds.dat); head(mom.meds.dat)

# create new data.frame
test=mom.meds.dat

# sort by id and date
newdata=rename(test, part_id = `Mom ID`, mom_medip_date=`Taken Datetime`, mom_prenat_medip=Medication, mom_prenat_med_act=`MAR Action`)
head(newdata)
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$mom_medip_date,format='%Y-%m-%d %H:%M:%S')),]
head(newdata2)
names(newdata2)=tolower(names(newdata2))

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="mom_medications_ip"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id","mom_medip_date","mom_prenat_med_act","mom_prenat_medip","redcap_repeat_instrument"))  
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        # Create new variable
range(dt3$redcap_repeat_instance) # 55

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3); unique(dt3$redcap_event_name)
names(dt3)

# characters
dt3$mom_prenat_medip=gsub(" ","_",dt3$mom_prenat_medip) 
head(dt3)

# order columns for export
col.names=names(dt3);col.names
col.first=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name");col.first
col.next=subset(col.names, !(col.names%in%col.first));col.next
colFixed=append(col.first, col.next, after=length(col.first));colFixed
dt4=setcolorder(dt3, colFixed)
names(dt4);head(dt4)
dt5=dt4

# export data
#-------------
batchSize=10000; # number of rows in single output file
data.file.name.export=as.character(dt5[2,2]);data.file.name.export
out.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\IRB\\UF\\UFHealth\\redcap_import\\02_redcap_import_Nov17\\",sep="");out.dir

chunks=split(dt5, floor(0:(nrow(dt5)-1)/batchSize))
for (i in 1:length(chunks))
{ # second loop
  write.table(chunks[[i]],paste0(out.dir,data.file.name.export,i,'.csv'),row.names=F, sep=";")
} # end second loop

# clear slate
rm(baby.dat, dat, newdata, newdata2, newdata3, dt, dt3, dt4, dt5)

# **************************************************************************** #
# ***************                mom_perscriptions                                              
# **************************************************************************** #

mom.script.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Mom Prescriptions", range = NULL, col_names = TRUE,
                        col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                        guess_max = min(1000, n_max));mom.script.dat

# Data Description
#-----------------
# rows: 
# cols: 
# unique id: 
# repeat: 
# ICD9/10: 

# dates
mom.script.dat$`Order Datetime`=as.character(mom.script.dat$`Order Datetime`)
str(mom.script.dat); head(mom.script.dat)

# create new data.frame
test=mom.script.dat

# sort by id and date
newdata=rename(test, part_id = `Mom ID`, mom_med_rx_date=`Order Datetime`, mom_prenat_med_rx=Medication)
head(newdata)
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$mom_med_rx_date,format='%Y-%m-%d %H:%M:%S')),]
head(newdata2)
names(newdata2)=tolower(names(newdata2))

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="mom_medications_rx"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id","mom_med_rx_date","mom_prenat_med_rx","redcap_repeat_instrument"))  
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        # Create new variable
range(dt3$redcap_repeat_instance) # 55

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3); unique(dt3$redcap_event_name)
names(dt3)

# characters
dt3$mom_prenat_med_rx=gsub(" ","_",dt3$mom_prenat_med_rx) 
head(dt3)

# order columns for export
col.names=names(dt3);col.names
col.first=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name");col.first
col.next=subset(col.names, !(col.names%in%col.first));col.next
colFixed=append(col.first, col.next, after=length(col.first));colFixed
dt4=setcolorder(dt3, colFixed)
names(dt4);head(dt4)
dt5=dt4

# export data
#-------------
batchSize=10000; # number of rows in single output file
data.file.name.export=as.character(dt5[2,2]);data.file.name.export
out.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\IRB\\UF\\UFHealth\\redcap_import\\02_redcap_import_Nov17\\",sep="");out.dir

chunks=split(dt5, floor(0:(nrow(dt5)-1)/batchSize))
for (i in 1:length(chunks))
{ # second loop
  write.table(chunks[[i]],paste0(out.dir,data.file.name.export,i,'.csv'),row.names=F, sep=";")
} # end second loop

# clear slate
rm(baby.dat, dat, newdata, newdata2, newdata3, dt, dt3, dt4, dt5)