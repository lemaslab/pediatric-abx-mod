##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        January 10, 2018 
# IRB:
# Description: Link mom-baby data that has been extracted from RedCap. 
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
out.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\IRB\\UF\\UFHealth\\redcap_import\\03_redcap_import_Jan18\\",sep="");out.dir

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
library(reshape2)

# **************************************************************************** #
# ***************                Baby.xlsx                                              
# **************************************************************************** #      

# file parameters
n_max=10000
data.file.name="Baby.xlsx";data.file.name


# **************************************************************************** #
# ***************                baby_demography                                              
# **************************************************************************** #

# baby_demography
#-----------------
# rows: 16684
# cols: 10 
# unique id: 16684
# repeat: 1
# ICD9/10: NA

# Note: import baby_id and birth_date

baby.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Baby", range = NULL, col_names = TRUE,
                   col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                   guess_max = min(1000, n_max));baby.dat

# data
dat=baby.dat

# rename
newdata=rename(dat, part_id = `Baby-Id`, baby_dob=DOB, baby_race=Race, 
               baby_ethnicity=Ethnicity, baby_birth_wt_gr=`Birth Weight (grams)`, 
               baby_admit_date=`Admit Date`, baby_admit_source=`Admit Source`,
               baby_nicu_los=`NICU LOS`,baby_gest_age=Gestational_Age, delivery_mode=Delivery_Mode);newdata

# subset to only dob
dat2=newdata %>%
  select(part_id,baby_dob);head(dat2)

# **************************************************************************** #
# ***************                mom_baby_link                                              
# **************************************************************************** #

# Note: import mom-baby link

baby.mom=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Baby-Mom Link", range = NULL, col_names = TRUE,
                   col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                   guess_max = min(1000, n_max));baby.mom
# data 
dat.link=baby.mom

# rename
newdata.link=rename(dat.link, part_id = `Baby-Id`, mom_id=`Mom-Id`)
names(newdata.link); head(newdata.link)


# merge baby and mom data
mom.baby.link <- left_join(dat2, newdata.link, by = c('part_id'));dim(mom.baby.link) # 16684
mdata.d=subset(mom.baby.link, is.na(mom_id)==F);dim(mdata.d) # 16607

# sort
newdata2 <- mdata.d[order(mdata.d$mom_id),]
names(newdata2); head(newdata2)

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="mom_baby_demography"
names(newdata3); head(newdata3)

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            
setkeyv(dt, c("mom_id","baby_dob"))  
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "mom_id"]        
head(dt3); range(dt3$redcap_repeat_instance) 
max(unique(dt3$redcap_repeat_instance)) # 5
table(dt3$redcap_repeat_instance)

# rename data
mom.baby.merge=dt3

# **************************************************************************** #
# ***************                Mom Prenatals.xlsx                                              
# **************************************************************************** #      

# file parameters
n_max=10000
data.file.name="Mom Prenatals.xlsx";data.file.name

# **************************************************************************** #
# ***************                mom_demography                                              
# **************************************************************************** #

# Note: import maternal demography and merge with "mom.baby.merge"

# read data
mom.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Mom", range = NULL, col_names = TRUE,
                  col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                  guess_max = min(1000, n_max));mom.dat

# rename
newdata.mom=rename(mom.dat, mom_id=`Mom-Id`, mom_race2=Race, mom_ethnicity2=Ethnicity)
names(newdata.mom); head(newdata.mom)

# merge
mom.baby.demo <- left_join(mom.baby.merge, newdata.mom, by = c('mom_id'));dim(mom.baby.demo) # 16607

# create "redcap_event_name" variable
mom.baby.demo$redcap_event_name=paste("visit_",mom.baby.demo$redcap_repeat_instance,"_arm_1",sep="")
head(mom.baby.demo)
unique(mom.baby.demo$redcap_event_name)
names(mom.baby.demo);head(mom.baby.demo)

# order columns for export
col.names=names(mom.baby.demo);col.names
col.first=c("part_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name");col.first
col.next=subset(col.names, !(col.names%in%col.first));col.next
colFixed=append(col.first, col.next, after=length(col.first));colFixed
dt4=setcolorder(mom.baby.demo, colFixed)
names(dt4);head(dt4)

# drop baby_dob
drops <- c("baby_dob")
dt5=dt4[ , !(names(dt4) %in% drops)]
head(dt5)

# export data
#-------------
batchSize=10000; # number of rows in single output file
data.file.name.export=as.character(dt5[2,2]);data.file.name.export

chunks=split(dt5, floor(0:(nrow(dt5)-1)/batchSize))
for (i in 1:length(chunks))
{ # second loop
  write.table(chunks[[i]],paste0(out.dir,data.file.name.export,i,'.csv'),row.names=F, sep=";")
} # end second loop

# **************************************************************************** #
# ***************                mom_prenatal_apt                                               
# **************************************************************************** #

# Note: import maternal prenatal_apt and merge with "mom.baby.merge"

# read data
prenat.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Prenatals by Appt", range = NULL, col_names = TRUE,
                     col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                     guess_max = min(1000, n_max));prenat.dat

# data
dat=prenat.dat

# rename
newdata=rename(dat, mom_id = `Mom ID`, mom_prenat_apt_date2= `Appt Time`, mom_prenat_enc_type2= `Enc Type`,mom_prenat_ht2=Height, mom_prenat_wt_oz2=Weight)
names(newdata); head(newdata)

# merge
mom.baby.visits <- left_join(newdata, mom.baby.merge, by = c('mom_id'));dim(mom.baby.demo) # 16607

# days_to variable(s)
mom.baby.visits$days2_prenatal_apt=as.Date(mom.baby.visits$mom_prenat_apt_date2,format="%Y-%m-%d")-as.Date(mom.baby.visits$baby_dob,format="%Y-%m-%d")
head(mom.baby.visits)

# seems to be looking like the merge is working. need to subset for 1 year prior (365 days).


