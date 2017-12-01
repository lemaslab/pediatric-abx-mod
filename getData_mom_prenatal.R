##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #
           
# Author:      Dominick Lemas 
# Date:        November 30, 2017 
# IRB:
# Description: Analysis of UFHealth data. 
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
# library(plyr)

# **************************************************************************** #
# ***************                Mom Prenatals.xlsx                                              
# **************************************************************************** #      

# file parameters
n_max=10000
data.file.name="Mom Prenatals.xlsx";data.file.name

# **************************************************************************** #
# ***************                mom_demography                                              
# **************************************************************************** #

mom.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Mom", range = NULL, col_names = TRUE,
          col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
          guess_max = min(1000, n_max));mom.dat

# format variables
#----------------
# redcap_repeat_instrument
mom.dat$redcap_repeat_instrument="mom_demography"
# redcap_repeat_instance
mom.dat$redcap_repeat_instance=1
# redcap_event_name
mom.dat$redcap_event_name="visit_1_arm_1"

# sort/rename columns
#--------------------
mom.dat.FINAL=mom.dat[,c(1,4:6,2:3)];mom.dat.FINAL
head(mom.dat.FINAL)
names(mom.dat.FINAL)=tolower(names(mom.dat.FINAL))
colnames(mom.dat.FINAL)[colnames(mom.dat.FINAL) == 'mom-id'] <- 'part_id'
colnames(mom.dat.FINAL)[colnames(mom.dat.FINAL) == 'race'] <- 'mom_race'
colnames(mom.dat.FINAL)[colnames(mom.dat.FINAL) == 'ethnicity'] <- 'mom_ethnicity'
unique(mom.dat.FINAL$redcap_repeat_instance)

# export data
#-------------
batchSize=10000; # number of rows in single output file
data.file.name.export=as.character(mom.dat.FINAL[2,2]);data.file.name.export
out.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\IRB\\UF\\UFHealth\\redcap_import\\02_redcap_import_Nov17\\",sep="");out.dir

chunks=split(mom.dat.FINAL, floor(0:(nrow(mom.dat.FINAL)-1)/batchSize))
for (i in 1:length(chunks))
  { # second loop
  write.table(chunks[[i]],paste0(out.dir,data.file.name.export,i,'.csv'),row.names=F, sep="\t")
  } # end second loop


# **************************************************************************** #
# ***************                mom_prenatal_apt                                               
# **************************************************************************** #

prenat.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Prenatals by Appt", range = NULL, col_names = TRUE,
                   col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                   guess_max = min(1000, n_max));prenat.dat

# format variables
#----------------

# dates
prenat.dat$`Appt Time`=as.character(prenat.dat$`Appt Time`)
# prenat.dat$mom_apt_date = strptime(prenat.dat$`Appt Time`,format='%Y-%m-%d %H:%M:%S')
# prenat.dat$date=as.POSIXlt(prenat.dat$`Appt Time`)
# prenat.dat$mom_apt_date = strptime(prenat.dat$`Appt Time`,format='%Y-%m-%d %H:%M:%S')
str(prenat.dat); head(prenat.dat)


# create new data.frame
test=prenat.dat

# rename, sort by id and date
newdata=rename(test, part_id = `Mom ID`, mom_prenat_apt_date= `Appt Time`, mom_prenat_enc_type= `Enc Type`,mom_prenat_ht=Height, mom_prenat_wt_oz=Weight)
names(newdata); head(newdata)
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$mom_prenat_apt_date,format='%Y-%m-%d %H:%M:%S')),]

# redcap_repeat_instrument
newdata3=newdata2
newdata3$redcap_repeat_instrument="mom_prenatal_apt"

# create "redcap_repeat_instance" variable
dt <- as.data.table(newdata3)            # Create data.table
setkeyv(dt, c("part_id", "mom_prenat_apt_date","mom_prenat_ht","mom_prenat_wt_oz","mom_prenat_enc_type","redcap_repeat_instrument"))  # Create key for data.table
dt3 <- dt[, redcap_repeat_instance := seq_len(.N), by = "part_id"]        # Create new variable
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

# compute "mom_prenat_ht_inch"
dt4$tmp=dt4$mom_prenat_ht
dt4$tmp=gsub("'"," ",dt4$tmp)
dt4$tmp=gsub('"'," ",dt4$tmp)
dt4$tmp=trimws(dt4$tmp, "b") 
dt4$tmp=gsub('  '," ",dt4$tmp)
dt4$mom_prenat_ht_inch=sapply(strsplit(as.character(dt4$tmp)," "), function(x){12*as.numeric(x[1]) + as.numeric(x[2])})
dt4=dt4[,-c("tmp")]

# compute "mom_prenat_wt_lb"
dt4$mom_prenat_wt_lb=dt4$mom_prenat_wt_oz/16

# format "mom_prenat_ht"
dt4$mom_prenat_ht=paste0("&",dt4$mom_prenat_ht,"&")
dt4$mom_prenat_ht=gsub(" ","_",dt4$mom_prenat_ht) 
head(dt4)
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