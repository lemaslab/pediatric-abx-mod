##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #
           
# Author:      Dominick Lemas 
# Date:        November 30, 2017 
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