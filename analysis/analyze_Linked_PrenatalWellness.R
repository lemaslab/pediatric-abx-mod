##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        January 16, 2018 
# IRB:
# Description: Analysis of linked mom-baby data, wellness and prental visits. 
# Data: C:\Users\djlemas\Dropbox (UFL)\02_Projects\UFHEALTH\RedCap\redcap_export

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
# location="djlemas";location
# location="Dominick";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\UFHEALTH\\RedCap\\redcap_export\\",sep="");work.dir
data.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\UFHEALTH\\RedCap\\redcap_export\\",sep="");data.dir
# out.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\UFHEALTH\\RedCap\\redcap_export\\",sep="");out.dir

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
# ***************                UFHealth_EHR_MomBaby_Prenatal_16Jan18                                               
# **************************************************************************** #

#Read Data (filled blanks with NA in excel)
data=read.table('UFHealth_EHR_MomBaby_Prenatal_16Jan18.csv', sep=",", header=T, na.strings = " ")
head(data)
str(data)
dim(data)

# format variables
data$part_id=as.character(data$part_id)
data$baby_dob=as.character(data$baby_dob)


# subset to include only babies 
newdata <- data[ which(is.na(data$baby_dob)=='F'),]
