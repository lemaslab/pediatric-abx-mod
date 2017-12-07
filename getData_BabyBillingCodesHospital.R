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
data.file.name="Baby-Billing Codes (Hospital).xlsx";data.file.name

# **************************************************************************** #
# ***************                 baby_hospital_dermatitis                                              
# **************************************************************************** #

# **************************************************************************** #
# ***************                 baby_hospital_ear                                              
# **************************************************************************** #

# **************************************************************************** #
# ***************                 baby_hospital_eczema                                              
# **************************************************************************** #

