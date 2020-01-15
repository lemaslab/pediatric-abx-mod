##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        Jan 14, 2020
# IRB:
# Description: narrow/broad abx over time paper #1
# Data: C:\Users\djlemas\Dropbox (UFL)\02_Projects\UFHEALTH\figures
# UPDATE: Modifying sample size.

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Directory Locations
work.dir=paste(Sys.getenv("USERPROFILE"),"\\Dropbox (UFL)\\02_Projects\\UFHEALTH\\figures\\",sep="");work.dir
data.dir=paste(Sys.getenv("USERPROFILE"),"\\Dropbox (UFL)\\02_Projects\\UFHEALTH\\medication_docs\\",sep="");data.dir
out.dir=paste(Sys.getenv("USERPROFILE"),"\\Dropbox (UFL)\\02_Projects\\UFHEALTH\\figures\\",sep="");out.dir

# Set Working Directory
setwd(work.dir)
list.files()

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(readxl)
library(dplyr)
library(ggplot2)

# **************************************************************************** #
# *****      load data: AntibioticPrescriptionsbyYear_16Apr2019.xlsx      
# **************************************************************************** # 

# read data
data.file.name="AntibioticPrescriptionsbyYear_16Apr2019.xlsx";data.file.name

# file parameters
n_max=1000000

# read data
abx=read_xlsx(paste(data.dir,data.file.name,sep=""), sheet = "RawDataAntibioticPresc", range = NULL, col_names = TRUE,
                   col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                   guess_max = min(1000, n_max));abx




