##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #
           
# Author:      Dominick Lemas 
# Date:        December 05, 2017 
# IRB:
# Description: Analysis of infant data in UFHealth data. 
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
# ***************                Mom Prenatals.xlsx                                              
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

# redcap_repeat_instrument
baby.dat$redcap_repeat_instrument="baby_demography"
# redcap_repeat_instance
baby.dat$redcap_repeat_instance=1
# redcap_event_name
baby.dat$redcap_event_name="visit_1_arm_1"

# create new data.frame
test=baby.dat

# sort by id and date
newdata=rename(test, part_id = `Baby-Id`, infant_dob=DOB, infant_race=Race, 
               infant_ethnicity=Ethnicity, infant_birth_wt_gr=`Birth Weight (grams)`, 
               infant_admit_date=`Admit Date`, infant_admit_source=`Admit Source`,
               infant_nicu_los=`NICU LOS`,infant_gest_age=Gestational_Age)
head(newdata); names(newdata)
names(newdata)=tolower(names(newdata))
dt3=newdata

# characters
dt3$delivery_mode=gsub(" ","_",dt3$delivery_mode) 
dt3$delivery_mode=gsub(",","&",dt3$delivery_mode) 
head(dt3)

# sort/rename columns
head(dt3); names(dt3)
dt5=dt3[,c(1,11:13,2:10)]
head(dt5);names(dt5)
dt5[,c(7:13)]

# export data
batchSize=10000; # number of rows in single output file
data.file.name.export=as.character(dt5[2,2]);data.file.name.export
out.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\IRB\\UF\\UFHealth\\redcap_import\\02_redcap_import_Nov17\\",sep="");out.dir

chunks=split(dt5, floor(0:(nrow(dt5)-1)/batchSize))
for (i in 1:length(chunks))
  { # second loop
  write.table(chunks[[i]],paste0(out.dir,data.file.name.export,i,'.csv'),row.names=F, sep="\t")
  } # end second loop

# **************************************************************************** #
# ***************                baby_vaccines                                               
# **************************************************************************** #

baby.vaccine=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Vaccines", range = NULL, col_names = TRUE,
                   col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                   guess_max = min(1000, n_max));baby.vaccine

# dates
baby.vaccine$Immune_Date=as.character(baby.vaccine$Immune_Date)
str(baby.vaccine); head(baby.vaccine)

# characters
baby.vaccine$Immunization_Name=gsub(" ","_",baby.vaccine$Immunization_Name) 
baby.vaccine$Immunization_Name=gsub(",","&",baby.vaccine$Immunization_Name) 

# create new data.frame
test=baby.vaccine

# rename, sort by id and date
newdata=rename(test, part_id = `Baby-Id`, infant_immune_date=Immune_Date, infant_vac_name=Immunization_Name)
names(newdata); head(newdata)
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_immune_date,format='%Y-%m-%d %H:%M:%S')),]

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_vaccines"

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "infant_immune_date","infant_vac_name","redcap_repeat_instrument"))  
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        
head(dt3); range(dt3$redcap_repeat_instance) 
unique(dt3$redcap_repeat_instance)

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

# order columns for export
dt4=dt3[,c(1,4:6,2:3)];
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


# **************************************************************************** #
# ***************                baby_wellvisit                                               
# **************************************************************************** #

baby.well=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Well Visit", range = NULL, col_names = TRUE,
                       col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                       guess_max = min(1000, n_max));baby.well

# rename
newdata=rename(baby.well, part_id = `Baby-Id`, infant_obs_date=Observation_Date, infant_ht_cm=`Height (cm)`, infant_wt_kgs=`Weight (kgs)`, infant_head_circ_cm=`Head Circumference (cm)`)
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
unique(dt3$redcap_repeat_instance)

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

# order columns for export
dt4=dt3[,c(1,6:8,2:5)];
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

# **************************************************************************** #
# ***************                baby_mom_baby_link                                               
# **************************************************************************** #

baby.mom=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Baby-Mom Link", range = NULL, col_names = TRUE,
                    col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                    guess_max = min(1000, n_max));baby.mom

# rename
newdata=rename(baby.mom, part_id = `Baby-Id`, mom_baby_link=`Mom-Id`)
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
unique(dt3$redcap_repeat_instance)

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

# order columns for export
col.names=names(dt3);col.names
first=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name");first
first.names=which(col.names%in%first);first.names
second=which(!col.names%in%first);second
second.names=which(!col.names%in%first);second.names
dt4=dt3[,c(1,3,4,5,2)];dt4
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

# **************************************************************************** #
# ***************                baby_first_height                                               
# **************************************************************************** #

baby.ht=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "First Height", range = NULL, col_names = TRUE,
                   col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                   guess_max = min(1000, n_max));baby.ht

# rename
newdata=rename(baby.ht, part_id = `Baby-Id`, infant_ht1_date=`1st Height date`, infant_ht1_cm=`Height (cm)`);newdata
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
unique(dt3$redcap_repeat_instance)

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

# order columns for export
col.names=names(dt3);col.names
colFixed=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name","infant_ht1_date","infant_ht1_cm");colFixed
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


# **************************************************************************** #
# ***************                baby_first_head_circumference                                               
# **************************************************************************** #

baby.hc=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "First Head Circumference", range = NULL, col_names = TRUE,
                  col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                  guess_max = min(1000, n_max));baby.hc

# rename
newdata=rename(baby.hc, part_id = `Baby-Id`, infant_hc1_date=`1st Head Circumference date`, infant_hc1_cm=`Head Circumference (cm)`);newdata
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
unique(dt3$redcap_repeat_instance)

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

# order columns for export
col.names=names(dt3);col.names
colFixed=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name","infant_hc1_date","infant_hc1_cm");colFixed
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

# **************************************************************************** #
# ***************                baby_antibiotics_perscriptions                                               
# **************************************************************************** #

baby.script=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Antibiotics Prescription", range = NULL, col_names = TRUE,
                  col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                  guess_max = min(1000, n_max));baby.script

# rename
newdata=rename(baby.script, part_id = `Baby-Id`, infant_med_order=`Med Order #`, infant_med_code=Med_Code, infant_meds=Medication, infant_med_date=`Med Order Datetime`);newdata
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
unique(dt3$redcap_repeat_instance)

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

# order columns for export
col.names=names(dt3);col.names
colFixed=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name","infant_med_order","infant_med_code","infant_meds","infant_med_date");colFixed
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

# **************************************************************************** #
# ***************                baby_antibiotics_ip                                               
# **************************************************************************** #

baby.abx=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Antibiotics IP Administration", range = NULL, col_names = TRUE,
                      col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                      guess_max = min(1000, n_max));baby.abx;names(baby.abx)

# rename
newdata=rename(baby.abx, part_id = `Baby-Id`,infant_med_order_ip="Med Order #",infant_mar_action_ip="MAR_Action",infant_med_code_ip="Med_Code",infant_med_ip_date="Taken_Datetime",infant_med_ip="Medication");newdata
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
unique(dt3$redcap_repeat_instance) #537

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

# order columns for export
col.names=names(dt3);col.names
colFixed=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name","infant_med_order_ip","infant_mar_action_ip","infant_med_code_ip","infant_med_ip_date","infant_med_ip");colFixed
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

# **************************************************************************** #
# ***************                baby_vaccines                                               
# **************************************************************************** #

