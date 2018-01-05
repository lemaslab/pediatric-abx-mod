##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #
           
# Author:      Dominick Lemas 
# Date:        January 05, 2018 
# IRB:
# Description: Import infant birth data to RedCap from flat files. 
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
# ***************                Baby.xlsx                                              
# **************************************************************************** #      

# file parameters
n_max=10000
data.file.name="Baby.xlsx";data.file.name

# **************************************************************************** #
# ***************                baby_demography                                              
# **************************************************************************** #

baby.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Baby", range = NULL, col_names = TRUE,
          col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
          guess_max = min(1000, n_max));baby.dat

# Data Description
#-----------------
# rows: 16684
# cols: 10 
# unique id: 16684
# repeat: 1
# ICD9/10: NA

# data
dat=baby.dat

# rename
newdata=rename(dat, part_id = `Baby-Id`, infant_dob=DOB, infant_race=Race, 
               infant_ethnicity=Ethnicity, infant_birth_wt_gr=`Birth Weight (grams)`, 
               infant_admit_date=`Admit Date`, infant_admit_source=`Admit Source`,
               infant_nicu_los=`NICU LOS`,infant_gest_age=Gestational_Age, delivery_mode=Delivery_Mode);newdata

# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 16684
length(newdata$part_id)         # 16684
names(newdata); head(newdata)

# sort (not needed in this case)
newdata2=newdata

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_demography"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt3=newdata3            
dt3$redcap_repeat_instance=1

# redcap_event_name
dt3$redcap_event_name="visit_1_arm_1"

# characters
dt3$delivery_mode=gsub(" ","_",dt3$delivery_mode) 
dt3$delivery_mode=gsub(",","&",dt3$delivery_mode) 
head(dt3);names(dt3)

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
  write.table(chunks[[i]],paste0(out.dir,data.file.name.export,i,'.csv'),row.names=F, sep="\t")
  } # end second loop

# clear slate
rm(baby.dat, dat, newdata, newdata2, newdata3, dt, dt3, dt4, dt5)

# **************************************************************************** #
# ***************                baby_vaccines                                               
# **************************************************************************** #

baby.vaccine=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Vaccines", range = NULL, col_names = TRUE,
                   col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                   guess_max = min(1000, n_max));baby.vaccine

# Data Description
#-----------------
# rows: 105611
# cols: 3
# unique id: 13068
# repeat: 34
# ICD9/10: NA

# data
dat=baby.vaccine

# rename
newdata=rename(dat, part_id = `Baby-Id`, infant_immune_date=Immune_Date, infant_vac_name=Immunization_Name)
names(newdata); head(newdata)

# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 13068
length(newdata$part_id)         # 105611
names(newdata); head(newdata)

# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_immune_date,format='%Y-%m-%d %H:%M:%S')),]
names(newdata2); head(newdata2)

# character format for immunization name
newdata2$infant_vac_name=gsub(" ","_",newdata2$infant_vac_name) 
newdata2$infant_vac_name=gsub(",","&",newdata2$infant_vac_name) 

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_vaccines"

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "infant_immune_date","infant_vac_name","redcap_repeat_instrument"))  
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        
head(dt3); range(dt3$redcap_repeat_instance) 
max(unique(dt3$redcap_repeat_instance)) # 34

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

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
rm(baby.vaccine, dat, newdata, newdata2, newdata3, dt, dt3, dt4, dt5)

# **************************************************************************** #
# ***************                baby_wellvisit                                               
# **************************************************************************** #

baby.well=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Well Visit", range = NULL, col_names = TRUE,
                       col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                       guess_max = min(1000, n_max));baby.well

# Data Description
#-----------------
# rows: 51441
# cols: 5
# unique id: 7667
# repeat: 43
# ICD9/10: NA

# data
dat=baby.well

# rename
newdata=rename(dat, part_id = `Baby-Id`, infant_obs_date=Observation_Date, infant_ht_cm=`Height (cm)`, infant_wt_kgs=`Weight (kgs)`, infant_head_circ_cm=`Head Circumference (cm)`)
names(newdata); head(newdata)

# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 
length(newdata$part_id)         # 
names(newdata); head(newdata)

# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_obs_date,format='%Y-%m-%d %H:%M:%S')),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_wellvisit"

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "infant_obs_date","infant_ht_cm","infant_wt_kgs","redcap_repeat_instrument"))  
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        
head(dt3); range(dt3$redcap_repeat_instance) 
max(unique(dt3$redcap_repeat_instance)) # 43

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

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
rm(baby.well, dat, newdata, newdata2, newdata3, dt, dt3, dt4, dt5)

# **************************************************************************** #
# ***************                baby_mom_baby_link                                               
# **************************************************************************** #

baby.mom=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Baby-Mom Link", range = NULL, col_names = TRUE,
                    col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                    guess_max = min(1000, n_max));baby.mom

# Data Description
#-----------------
# rows: 29798
# cols: 2
# unique id: 28069
# repeat: 5
# ICD9/10: NA

# data
dat=baby.mom

# rename
newdata=rename(dat, part_id = `Baby-Id`, mom_baby_link=`Mom-Id`)
names(newdata); head(newdata)

# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 28069
length(newdata$part_id)         # 29798
names(newdata); head(newdata)

# sort
newdata2 <- newdata[order(newdata$part_id),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_mom_baby_link"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "mom_baby_link","redcap_repeat_instrument"))  
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        
head(dt3); range(dt3$redcap_repeat_instance) 
max(unique(dt3$redcap_repeat_instance)) # 5

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

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
rm(baby.mom, dat, newdata, newdata2, newdata3, dt, dt3, dt4, dt5)

# **************************************************************************** #
# ***************                baby_first_height                                               
# **************************************************************************** #

baby.ht=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "First Height", range = NULL, col_names = TRUE,
                   col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                   guess_max = min(1000, n_max));baby.ht

# Data Description
#-----------------
# rows: 16441
# cols: 3
# unique id: 16441
# repeat: 1
# ICD9/10: NA

# data
dat=baby.ht

# rename
newdata=rename(dat, part_id = `Baby-Id`, infant_ht1_date=`1st Height date`, infant_ht1_cm=`Height (cm)`);newdata
names(newdata); head(newdata)

# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 16441
length(newdata$part_id)         # 16441
names(newdata); head(newdata)

# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_ht1_date,format='%Y-%m-%d %H:%M:%S')),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_first_height"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "infant_ht1_date","infant_ht1_cm","redcap_repeat_instrument"))  
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        
head(dt3); range(dt3$redcap_repeat_instance) 
max(unique(dt3$redcap_repeat_instance)) # 1

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

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
rm(baby.ht, dat, newdata, newdata2, newdata3, dt, dt3, dt4, dt5)

# **************************************************************************** #
# ***************                baby_first_head_circumference                                               
# **************************************************************************** #

baby.hc=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "First Head Circumference", range = NULL, col_names = TRUE,
                  col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                  guess_max = min(1000, n_max));baby.hc

# Data Description
#-----------------
# rows: 16353
# cols: 3
# unique id: 16346
# repeat: 2
# ICD9/10: NA

# data
dat=baby.hc

# rename
newdata=rename(dat, part_id = `Baby-Id`, infant_hc1_date=`1st Head Circumference date`, infant_hc1_cm=`Head Circumference (cm)`);newdata
names(newdata); head(newdata)

# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 16346
length(newdata$part_id)         # 16353
names(newdata); head(newdata)

# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_hc1_date,format='%Y-%m-%d %H:%M:%S')),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_first_head_circumference"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "infant_hc1_date","infant_hc1_cm","redcap_repeat_instrument"))  
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        
head(dt3); range(dt3$redcap_repeat_instance) 
max(unique(dt3$redcap_repeat_instance)) # 2

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

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
rm(baby.hc, dat, newdata, newdata2, newdata3, dt, dt3, dt4, dt5)

# **************************************************************************** #
# ***************                baby_antibiotics_perscriptions                                               
# **************************************************************************** #

baby.script=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Antibiotics Prescription", range = NULL, col_names = TRUE,
                  col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                  guess_max = min(1000, n_max));baby.script

# Data Description
#-----------------
# rows: 5156
# cols: 5
# unique id: 2717
# repeat: 25
# ICD9/10: NA

# data
dat=baby.script

# rename
newdata=rename(dat, part_id = `Baby-Id`, infant_med_order=`Med Order #`, infant_med_code=Med_Code, infant_meds=Medication, infant_med_date=`Med Order Datetime`);newdata
names(newdata); head(newdata)

# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 2717
length(newdata$part_id)         # 5156
names(newdata); head(newdata)

# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_med_date,format='%Y-%m-%d %H:%M:%S')),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_antibiotics_perscriptions"
names(newdata3); head(newdata3)

# modify medication string
newdata3$infant_meds=gsub(" ","_",newdata3$infant_meds) 
newdata3$infant_meds=gsub(",","&",newdata3$infant_meds) 
head(newdata3);str(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "infant_med_date","infant_med_order","infant_med_code","infant_meds","redcap_repeat_instrument"))  
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        
head(dt3); range(dt3$redcap_repeat_instance) 
max(unique(dt3$redcap_repeat_instance)) # 25

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

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
rm(baby.script, dat, newdata, newdata2, newdata3, dt, dt3, dt4, dt5)

# **************************************************************************** #
# ***************                baby_antibiotics_ip                                               
# **************************************************************************** #

baby.abx=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Antibiotics IP Administration", range = NULL, col_names = TRUE,
                      col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                      guess_max = min(1000, n_max));baby.abx

# Data Description
#-----------------
# rows: 70367
# cols: 6
# unique id: 15998
# repeat: 537
# ICD9/10: 

# data
dat=baby.abx

# rename
newdata=rename(dat, part_id = `Baby-Id`,infant_med_order_ip="Med Order #",infant_mar_action_ip="MAR_Action",infant_med_code_ip="Med_Code",infant_med_ip_date="Taken_Datetime",infant_med_ip="Medication");newdata
names(newdata); head(newdata)

# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 15998
length(newdata$part_id)         # 70367
names(newdata); head(newdata)

# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_med_ip_date,format='%Y-%m-%d %H:%M:%S')),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_antibiotics_ip"
names(newdata3); head(newdata3)

# modify medication string
newdata3$infant_med_ip=gsub(" ","_",newdata3$infant_med_ip) 
newdata3$infant_med_ip=gsub(",","&",newdata3$infant_med_ip) 
head(newdata3);str(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3);names(dt)           
setkeyv(dt, c("part_id", "infant_med_ip_date","infant_med_order_ip","infant_mar_action_ip","infant_med_code_ip","infant_med_ip","redcap_repeat_instrument"))  
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        
head(dt3); range(dt3$redcap_repeat_instance) 
max(unique(dt3$redcap_repeat_instance)) #537

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

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
rm(baby.abx, dat, newdata, newdata2, newdata3, dt, dt3, dt4, dt5)

