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
# ***************                Baby-Billing Codes (Hospital).xlsx                                              
# **************************************************************************** #      

# file parameters
n_max=10000
data.file.name="Baby-Billing Codes (Hospital).xlsx";data.file.name

# **************************************************************************** #
# ***************                 baby_hospital_asthma                                              
# **************************************************************************** #

baby.asthma=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Asthma", range = NULL, col_names = TRUE,
                      col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                      guess_max = min(1000, n_max));baby.asthma

# data
dat=baby.asthma

# rename
newdata=rename(dat, part_id = `Baby-Id`, infant_asthma_hosp_date=`Admit Date`, infant_asthma_hosp_icd=`ICD9/ICD10 Code`);newdata

# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 2465
length(newdata$part_id)         # 5293
names(newdata); head(newdata)

# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_asthma_hosp_date,format='%Y-%m-%d')),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_hospital_asthma"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "infant_asthma_hosp_date","infant_asthma_hosp_icd","redcap_repeat_instrument"))  
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        
head(dt3); range(dt3$redcap_repeat_instance) 
max(unique(dt3$redcap_repeat_instance)) # 29

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

# order columns for export
col.names=names(dt3);col.names
colFixed=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name","infant_asthma_hosp_date","infant_asthma_hosp_icd");colFixed
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

baby.derm=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Seborrheic Dermatitis", range = NULL, col_names = TRUE,
                      col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                      guess_max = min(1000, n_max));baby.derm

# data
dat=baby.derm

# rename
newdata=rename(dat, part_id = `Baby-Id`, infant_derm_hosp_date=`Admit Date`, infant_derm_hosp_icd=`ICD9/ICD10 Code`);newdata

# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 455
length(newdata$part_id)         # 687
names(newdata); head(newdata)

# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_derm_hosp_date,format='%Y-%m-%d')),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_hospital_dermatitis"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "infant_derm_hosp_date","infant_derm_hosp_icd","redcap_repeat_instrument"))  
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        
head(dt3); range(dt3$redcap_repeat_instance) 
max(unique(dt3$redcap_repeat_instance)) # 17

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

# order columns for export
col.names=names(dt3);col.names
colFixed=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name","infant_derm_hosp_date","infant_derm_hosp_icd");colFixed
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
# ***************                 baby_hospital_ear                                              
# **************************************************************************** #

baby.ear=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Ear Infection", range = NULL, col_names = TRUE,
                    col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                    guess_max = min(1000, n_max));baby.ear

# data
dat=baby.ear;dat

# rename
newdata=rename(dat, part_id = `Baby-Id`, infant_ear_hosp_date=`Admit Date`, infant_ear_hosp_icd=`ICD9/ICD10 Code`);newdata

# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 1669
length(newdata$part_id)         # 2548
names(newdata); head(newdata)

# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_ear_hosp_date,format='%Y-%m-%d')),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_hospital_ear"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "infant_ear_hosp_date","infant_ear_hosp_icd","redcap_repeat_instrument"))  
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        
head(dt3); range(dt3$redcap_repeat_instance) 
max(unique(dt3$redcap_repeat_instance)) # 28

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

# order columns for export
col.names=names(dt3);col.names
colFixed=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name","infant_ear_hosp_date","infant_ear_hosp_icd");colFixed
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
# ***************                 baby_hospital_eczema                                              
# **************************************************************************** #

baby.eczema=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Dermatitis-Eczema", range = NULL, col_names = TRUE,
                   col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                   guess_max = min(1000, n_max));baby.eczema

# data
dat=baby.eczema;dat

# rename
newdata=rename(dat, part_id = `Baby-Id`, infant_eczema_hosp_date=`Admit Date`, infant_eczema_hosp_icd=`ICD9/ICD10 Code`);newdata

# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 1669
length(newdata$part_id)         # 2548
names(newdata); head(newdata)

# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_eczema_hosp_date,format='%Y-%m-%d')),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_hospital_eczema"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "infant_eczema_hosp_date","infant_eczema_hosp_icd","redcap_repeat_instrument"))  
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        
head(dt3); range(dt3$redcap_repeat_instance) 
max(unique(dt3$redcap_repeat_instance)) # 28

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3)
unique(dt3$redcap_event_name)
names(dt3);head(dt3)

# order columns for export
col.names=names(dt3);col.names
colFixed=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name","infant_eczema_hosp_date","infant_eczema_hosp_icd");colFixed
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
# ***************                 baby_hospital_foodallergy                                              
# **************************************************************************** #

baby.food=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Food Allergy", range = NULL, col_names = TRUE,
                      col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                      guess_max = min(1000, n_max));baby.food

# data
dat=baby.food;dat

# rename
newdata=rename(dat, part_id = `Baby-Id`, infant_fa_hosp_date=`Admit Date`, infant_fa_hosp_icd=`ICD9/ICD10 Code`);newdata

# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 980
length(newdata$part_id)         # 1339
names(newdata); head(newdata)

# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_fa_hosp_date,format='%Y-%m-%d')),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_hospital_foodallergy"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "infant_fa_hosp_date","infant_fa_hosp_icd","redcap_repeat_instrument"))  
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
colFixed=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name","infant_fa_hosp_date","infant_fa_hosp_icd");colFixed
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
# ***************                 baby_hospital_hemangioma                                              
# **************************************************************************** #

baby.hemang=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Hemangioma", range = NULL, col_names = TRUE,
                    col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                    guess_max = min(1000, n_max));baby.hemang

# data
dat=baby.hemang;dat

# rename
newdata=rename(dat, part_id = `Baby-Id`, infant_hemang_hosp_date=`Admit Date`, infant_hemang_hosp_icd=`ICD9/ICD10 Code`);newdata
newdata.1=newdata[,c(1,3,4)]
newdata=newdata.1

# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 511
length(newdata$part_id)         # 533
names(newdata); head(newdata)

# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_hemang_hosp_date,format='%Y-%m-%d')),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_hospital_hemangioma"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "infant_hemang_hosp_date","infant_hemang_hosp_icd","redcap_repeat_instrument"))  
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
colFixed=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name","infant_hemang_hosp_date","infant_hemang_hosp_icd");colFixed
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
# ***************                 baby_hospital_sebaceous                                              
# **************************************************************************** #

baby.seb=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Nevus sebaceous", range = NULL, col_names = TRUE,
                      col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                      guess_max = min(1000, n_max));baby.seb

# data
dat=baby.seb;dat

# rename
newdata=rename(dat, part_id = `Baby-Id`, infant_sebaceous_hosp_date=`Admit Date`, infant_sebaceous_hosp_icd=`ICD9/ICD10 Code`);newdata

# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 31
length(newdata$part_id)         # 31
names(newdata); head(newdata)

# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_sebaceous_hosp_date,format='%Y-%m-%d')),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_hospital_sebaceous"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "infant_sebaceous_hosp_date","infant_sebaceous_hosp_icd","redcap_repeat_instrument"))  
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
colFixed=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name","infant_sebaceous_hosp_date","infant_sebaceous_hosp_icd");colFixed
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
# ***************                 baby_hospital_obesity                                              
# **************************************************************************** #

baby.ob=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Obesity", range = NULL, col_names = TRUE,
                   col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                   guess_max = min(1000, n_max));baby.ob

# data
dat=baby.ob;dat

# rename
newdata=rename(dat, part_id = `Baby-Id`, infant_obesity_hosp_date=`Admit Date`, infant_obesity_hosp_icd=`ICD9/ICD10 Code`);newdata

# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 26
length(newdata$part_id)         # 30
names(newdata); head(newdata)

# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_obesity_hosp_date,format='%Y-%m-%d')),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_hospital_obesity"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "infant_obesity_hosp_date","infant_obesity_hosp_icd","redcap_repeat_instrument"))  
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
colFixed=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name","infant_obesity_hosp_date","infant_obesity_hosp_icd");colFixed
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
# ***************                 baby_hospital_erythema                                              
# **************************************************************************** #

baby.tox=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Erythema toxicum", range = NULL, col_names = TRUE,
                  col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                  guess_max = min(1000, n_max));baby.tox

# data
dat=baby.tox;dat

# rename
newdata=rename(dat, part_id = `Baby-Id`, infant_erythema_hosp_date=`Admit Date`, infant_erythema_hosp_icd=`ICD9/ICD10 Code`);newdata

# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 1032
length(newdata$part_id)         # 1037
names(newdata); head(newdata)

# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_erythema_hosp_date,format='%Y-%m-%d')),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="baby_hospital_erythema"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "infant_erythema_hosp_date","infant_erythema_hosp_icd","redcap_repeat_instrument"))  
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
colFixed=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name","infant_erythema_hosp_date","infant_erythema_hosp_icd");colFixed
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


