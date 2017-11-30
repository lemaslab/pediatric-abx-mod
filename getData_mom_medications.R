##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #
           
# Author:      Dominick Lemas 
# Date:        November 29, 2017 
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

# dates
#------
mom.abxip.dat$`Taken Datetime`=as.character(mom.abxip.dat$`Taken Datetime`)
mom.abxip.dat$mom_abxip_date=as.POSIXlt(mom.abxip.dat$`Taken Datetime`)
str(mom.abxip.dat)

# create new data.frame
test=mom.abxip.dat

# sort by id and date
newdata=rename(test, part_id = `Mom ID`, mom_abxip_action= `MAR Action`, mom_prenat_abx=Antibiotics)
newdata2 <- newdata[order(newdata$part_id, newdata$mom_abxip_date),]
A=newdata2[c(1:15),c(1,5)]
newdata3=subset(newdata2, select=c("part_id","mom_abxip_action", "mom_prenat_abx", "mom_abxip_date"))
names(newdata3)=tolower(names(newdata3))
newdata3$mom_abxip_date=as.character(newdata3$mom_abxip_date)

# redcap_repeat_instrument
newdata3$redcap_repeat_instrument="mom_antibiotics_ip"
names(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("part_id", "mom_abxip_action","mom_prenat_abx","mom_abxip_date","redcap_repeat_instrument"))  # Create key for data.table
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        # Create new variable

dt3[c(1:15),c(1,4)]

; range(dt3$redcap_repeat_instance) # 574

# create "redcap_event_name" variable
dt3$redcap_event_name=paste("visit_",dt3$redcap_repeat_instance,"_arm_1",sep="")
head(dt3); unique(dt3$redcap_event_name)
names(dt3)

# characters
dt3$mom_prenat_abx=gsub(" ","_",dt3$mom_prenat_abx) 
dt3$mom_prenat_abx=gsub(",","&",dt3$mom_prenat_abx) 
head(dt3)

# sort/rename columns
#--------------------
head(dt3); names(dt3)
dt5=dt3[,c(1,5:7,2:4)]
head(dt5);names(dt5)

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
# ***************                mom_antibiotics                                              
# **************************************************************************** #

mom.abxscript.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Mom Antibiotics Prescription", range = NULL, col_names = TRUE,
                        col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                        guess_max = min(1000, n_max));mom.abxscript.dat

# **************************************************************************** #
# ***************                mom_medications_ip                                              
# **************************************************************************** #

mom.medip1.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Mom IP Medications", range = NULL, col_names = TRUE,
                        col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                        guess_max = min(1000, n_max));mom.medip1.dat

# **************************************************************************** #
# ***************                mom_medications_ip2                                              
# **************************************************************************** #

mom.medip2.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Mom IP Medications(1)", range = NULL, col_names = FALSE,
                        col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                        guess_max = min(1000, n_max));mom.medip2.dat

# **************************************************************************** #
# ***************                mom_perscriptions                                              
# **************************************************************************** #

mom.script.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Mom Prescriptions", range = NULL, col_names = TRUE,
                        col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                        guess_max = min(1000, n_max));mom.script.dat







           



