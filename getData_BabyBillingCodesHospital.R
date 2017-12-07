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
newdata=rename(dat, part_id = `Baby Id`, infant_asthma_date=`Service/Charge Date`, infant_asthma_icd=`Asthma ICD9/ICD10`);newdata
# unique ID? Some moms had multiple babies in data set
length(unique(newdata$part_id)) # 16346
length(newdata$part_id)         # 16353
names(newdata); head(newdata)
# sort
newdata2 <- newdata[order(newdata$part_id, as.Date(newdata$infant_asthma_date,format='%Y-%m-%d')),]
names(newdata2); head(newdata2)

# **************************************************************************** #
# ***************                 baby_hospital_dermatitis                                              
# **************************************************************************** #

# **************************************************************************** #
# ***************                 baby_hospital_ear                                              
# **************************************************************************** #


# **************************************************************************** #
# ***************                 baby_hospital_eczema                                              
# **************************************************************************** #

# **************************************************************************** #
# ***************                 baby_hospital_foodallergy                                              
# **************************************************************************** #


# **************************************************************************** #
# ***************                 baby_hospital_hemangioma                                              
# **************************************************************************** #


# **************************************************************************** #
# ***************                 baby_hospital_sebaceous                                              
# **************************************************************************** #


# **************************************************************************** #
# ***************                 baby_hospital_obesity                                              
# **************************************************************************** #


# **************************************************************************** #
# ***************                 baby_hospital_erythema                                              
# **************************************************************************** #



