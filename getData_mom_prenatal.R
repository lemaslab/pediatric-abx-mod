##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #
           
# Author:      Dominick Lemas 
# Date:        November 13 2017 
# IRB:
# Description: Analysis of UFHealth data. 
# Data: C:\Users\Dominick\Dropbox (UFL)\IRB\UF\UFHealth\data6

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
# location="djlemas";location
# location="Dominick";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\IRB\\UF\\UFHealth\\data6\\",sep="");work.dir
data.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\IRB\\UF\\UFHealth\\data6\\",sep="");data.dir


# Set Working Directory
setwd(work.dir)
list.files()

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(readxl)
# library(plyr)


# **************************************************************************** #
# ***************                Mom Prenatals.xlsx                                              
# **************************************************************************** #      

# file parameters
n_max=10000
data.file.name="Mom Prenatals.xlsx";data.file.name

# mom 
#-----
mom.mom.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Mom", range = NULL, col_names = TRUE,
          col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
          guess_max = min(1000, n_max));mom.mom.dat

# format variables
#----------------
# redcap_repeat_instrument
  mom.mom.dat$redcap_repeat_instrument="mom_demography"
# redcap_repeat_instance
  mom.mom.dat$redcap_repeat_instance=1
# redcap_event_name
  mom.mom.dat$redcap_event_name="visit_1_arm_1"

# sort columns
#-------------
  mom.mom.dat.FINAL=mom.mom.dat[,c(1,4:6,2:3)]

# mom apt
#--------
mom.prenatal.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Prenatals by Appt", range = NULL, col_names = TRUE,
                   col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                   guess_max = min(1000, n_max));mom.prenatal.dat

# **************************************************************************** #
# ***************                Mom Medications.xlsx                                              
# **************************************************************************** # 

# file parameters
n_max=10000
data.file.name="Mom Medications.xlsx";data.file.name

# mom abx ip
#-----------
mom.abxip.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Mom Antibiotics IP Admin", range = NULL, col_names = TRUE,
                      col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                      guess_max = min(1000, n_max));mom.abxip.dat

# mom abx script
#---------------
mom.abxscript.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Mom Antibiotics Prescription", range = NULL, col_names = TRUE,
                        col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                        guess_max = min(1000, n_max));mom.abxscript.dat

# mom meds ip
#-----------
mom.medip1.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Mom IP Medications", range = NULL, col_names = TRUE,
                        col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                        guess_max = min(1000, n_max));mom.medip1.dat


# mom abx ip 2
#-----------
mom.medip2.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Mom IP Medications(1)", range = NULL, col_names = FALSE,
                        col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                        guess_max = min(1000, n_max));mom.medip2.dat

# mom prescriptions
#------------------
mom.script.dat=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "Mom Prescriptions", range = NULL, col_names = TRUE,
                        col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                        guess_max = min(1000, n_max));mom.script.dat







           



