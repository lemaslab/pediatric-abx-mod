##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #
           
# Author:      Dominick Lemas 
# Date:        December 06, 2017 
# IRB:
# Description: Analysis of infant clinic data in UFHealth data. 
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
data.file.name="Baby-Billing Codes (Clinic).xlsx";data.file.name


# **************************************************************************** #
# ***************                baby_clinic_asthma                                               
# **************************************************************************** #

baby.asthma=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Asthma (Clinic)", range = NULL, col_names = TRUE,
                   col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                   guess_max = min(1000, n_max));baby.asthma

# data
dat=baby.asthma

# rename
newdata=rename(dat, part_id = `Baby Id`, infant_asthma_date=`Service/Charge Date`, infant_asthma_icd=`Asthma ICD9/ICD10`);newdata
# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 16346
length(newdata$part_id)         # 16353
names(newdata); head(newdata)
# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_asthma_date,format='%Y-%m-%d')),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_clinic_asthma"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "infant_asthma_date","infant_asthma_icd","redcap_repeat_instrument"))  
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        
head(dt3); range(dt3$redcap_repeat_instance) 
unique(dt3$redcap_repeat_instance) # 75

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

# order columns for export
col.names=names(dt3);col.names
colFixed=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name","infant_asthma_date","infant_asthma_icd");colFixed
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
# ***************                baby_clinic_foodallergy                                               
# **************************************************************************** #

baby.allergy=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Food Allergy (Clinic)", range = NULL, col_names = TRUE,
                      col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                      guess_max = min(1000, n_max));baby.allergy

# data
dat=baby.allergy

# rename
newdata=rename(dat, part_id = `Baby Id`, infant_fa_date=`Service/Charge Date`, infant_fa_icd=`Food Allergy ID9/ICD10`);newdata
# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 16346
length(newdata$part_id)         # 16353
names(newdata); head(newdata)
# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_fa_date,format='%Y-%m-%d')),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_clinic_foodallergy"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "infant_fa_date","infant_fa_icd","redcap_repeat_instrument"))  
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        
head(dt3); range(dt3$redcap_repeat_instance) 
max(unique(dt3$redcap_repeat_instance)) # 33

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

# order columns for export
col.names=names(dt3);col.names
colFixed=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name","infant_fa_date","infant_fa_icd");colFixed
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
# ***************                baby_clinic_ear                                                
# **************************************************************************** #

baby.ear=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Ear Infection (Clinic)", range = NULL, col_names = TRUE,
                       col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                       guess_max = min(1000, n_max));baby.ear

# data
dat=baby.ear

# rename
newdata=rename(dat, part_id = `Baby Id`, infant_ear_infect_date=`Service/Charge Date`, infant_ear_infect_icd=`Ear Infection ICD9/ICD10`);newdata
# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 16346
length(newdata$part_id)         # 16353
names(newdata); head(newdata)
# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_ear_infect_date,format='%Y-%m-%d')),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_clinic_ear"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "infant_ear_infect_date","infant_ear_infect_icd","redcap_repeat_instrument"))  
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        
head(dt3); range(dt3$redcap_repeat_instance) 
max(unique(dt3$redcap_repeat_instance)) # 21

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

# order columns for export
col.names=names(dt3);col.names
colFixed=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name","infant_ear_infect_date","infant_ear_infect_icd");colFixed
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
# ***************                baby_clinic_eczema                                               
# **************************************************************************** #

baby.eczema=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Dermatitis-Eczema (Clinic)", range = NULL, col_names = TRUE,
                   col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                   guess_max = min(1000, n_max));baby.eczema

# data
dat=baby.eczema

# rename
newdata=rename(dat, part_id = `Baby Id`, infant_eczema_date=`Service/Charge Date`, infant_eczema_icd=`Dermatitis-Eczema ICD9/ICD10`);newdata
# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 2849
length(newdata$part_id)         # 6507
names(newdata); head(newdata)

# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_eczema_date,format='%Y-%m-%d')),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_clinic_eczema"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "infant_eczema_date","infant_eczema_icd","redcap_repeat_instrument"))  
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        
head(dt3); range(dt3$redcap_repeat_instance) 
max(unique(dt3$redcap_repeat_instance)) # 36

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

# order columns for export
col.names=names(dt3);col.names
colFixed=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name","infant_eczema_date","infant_eczema_icd");colFixed
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
# ***************                baby_clinic_dermatitis                                               
# **************************************************************************** #

baby.derm=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Seborrheic Dermatitis (Clinic)", range = NULL, col_names = TRUE,
                      col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                      guess_max = min(1000, n_max));baby.derm

# data
dat=baby.derm

# rename
newdata=rename(dat, part_id = `Baby Id`, infant_dermatitis_date=`Service/Charge Date`, infant_dermatitis_icd=`Seborrheic Dermatitis ICD9/ICD10`);newdata
# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 2849
length(newdata$part_id)         # 6507
names(newdata); head(newdata)

# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_dermatitis_date,format='%Y-%m-%d')),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_clinic_dermatitis"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "infant_dermatitis_date","infant_dermatitis_icd","redcap_repeat_instrument"))  
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        
head(dt3); range(dt3$redcap_repeat_instance) 
max(unique(dt3$redcap_repeat_instance)) # 7

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

# order columns for export
col.names=names(dt3);col.names
colFixed=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name","infant_dermatitis_date","infant_dermatitis_icd");colFixed
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
# ***************                baby_clinic_erythema                                               
# **************************************************************************** #

baby.tox=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Erythema Toxicum (Clinic)", range = NULL, col_names = TRUE,
                    col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                    guess_max = min(1000, n_max));baby.tox

# data
dat=baby.tox

# rename
newdata=rename(dat, part_id = `Baby Id`, infant_erythema_date=`Service/Charge Date`, infant_erythema_icd=`Erythema Toxicum-ICD9/ICD10`);newdata
# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 2849
length(newdata$part_id)         # 6507
names(newdata); head(newdata)

# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_erythema_date,format='%Y-%m-%d')),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_clinic_erythema"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "infant_erythema_date","infant_erythema_icd","redcap_repeat_instrument"))  
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        
head(dt3); range(dt3$redcap_repeat_instance) 
max(unique(dt3$redcap_repeat_instance)) # 10

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

# order columns for export
col.names=names(dt3);col.names
colFixed=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name","infant_erythema_date","infant_erythema_icd");colFixed
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
# ***************                 baby_clinic_sebaceous                                              
# **************************************************************************** #

baby.seb=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Nevus Sebaceous (Clinic)", range = NULL, col_names = TRUE,
                   col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                   guess_max = min(1000, n_max));baby.seb

# data
dat=baby.seb

# rename
newdata=rename(dat, part_id = `Baby Id`, infant_sebaceous_date=`Service/Charge Date`, infant_sebaceous_icd=`Nevus Sebaceous ICD9/ICD10`);newdata
# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 2849
length(newdata$part_id)         # 6507
names(newdata); head(newdata)

# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_sebaceous_date,format='%Y-%m-%d')),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_clinic_sebaceous"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "infant_sebaceous_date","infant_sebaceous_icd","redcap_repeat_instrument"))  
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        
head(dt3); range(dt3$redcap_repeat_instance) 
max(unique(dt3$redcap_repeat_instance)) # 8

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

# order columns for export
col.names=names(dt3);col.names
colFixed=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name","infant_sebaceous_date","infant_sebaceous_icd");colFixed
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
# ***************                 baby_clinic_hemangioma                                              
# **************************************************************************** #

baby.hem=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Hemangioma (Clinic)", range = NULL, col_names = TRUE,
                   col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                   guess_max = min(1000, n_max));baby.hem

# data
dat=baby.hem

# rename
newdata=rename(dat, part_id = `Baby Id`, infant_hemangioma_date=`Service/Charge Date`, infant_hemangioma_icd=`Hemangioma ICD9/ICD10`);newdata
# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 473
length(newdata$part_id)         # 1035
names(newdata); head(newdata)

# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_hemangioma_date,format='%Y-%m-%d')),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_clinic_hemangioma"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "infant_hemangioma_date","infant_hemangioma_icd","redcap_repeat_instrument"))  
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        
head(dt3); range(dt3$redcap_repeat_instance) 
max(unique(dt3$redcap_repeat_instance)) # 18

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

# order columns for export
col.names=names(dt3);col.names
colFixed=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name","infant_hemangioma_date","infant_hemangioma_icd");colFixed
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
# ***************                 baby_obesity_clinic                                              
# **************************************************************************** #

baby.ob=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Obesity (Clinic)", range = NULL, col_names = TRUE,
                   col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                   guess_max = min(1000, n_max));baby.ob

# data
dat=baby.ob

# rename
newdata=rename(dat, part_id = `Baby Id`, infant_obesity_date=`Service/Charge Date`, infant_obesity_icd=`Obesity ICD9/ICD10`);newdata
# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 316
length(newdata$part_id)         # 490
names(newdata); head(newdata)

# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_obesity_date,format='%Y-%m-%d')),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_clinic_obesity"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "infant_obesity_date","infant_obesity_icd","redcap_repeat_instrument"))  
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        
head(dt3); range(dt3$redcap_repeat_instance) 
max(unique(dt3$redcap_repeat_instance)) # 11

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

# order columns for export
col.names=names(dt3);col.names
colFixed=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name","infant_obesity_date","infant_obesity_icd");colFixed
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
# ***************                 baby_hospital_dermatitis                                              
# **************************************************************************** #

# **************************************************************************** #
# ***************                 baby_hospital_ear                                              
# **************************************************************************** #

# **************************************************************************** #
# ***************                 baby_hospital_eczema                                              
# **************************************************************************** #

