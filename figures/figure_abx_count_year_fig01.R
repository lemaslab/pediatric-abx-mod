##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        March 14, 2019 
# IRB:
# Description: Count of Abx by year for paper #1
# Data: C:\Users\djlemas\Dropbox (UFL)\02_Projects\UFHEALTH\figures

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
location="djlemas";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\UFHEALTH\\figures\\",sep="");work.dir
data.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\UFHEALTH\\figures\\",sep="");data.dir
out.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\UFHEALTH\\figures\\",sep="");out.dir

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
# *****      load data: fig1_infant_abx_count_V1_14Mar19.csv      
# **************************************************************************** # 

# read data
data.file.name="fig1_infant_abx_count_V1_14Mar19.csv";data.file.name
data.file.path=paste0(data.dir,"\\",data.file.name);data.file.path
counts<- read.csv(data.file.path);counts

# diagnostics
dat=counts
dim(dat)

# how many participants
sum(dat$frequency) # 4024

# plot abx episode number by participant count number.
ggplot(data=dat, aes(x=abx_episode, y=percent)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=percent), vjust=-0.3, size=3.5)+
  theme_minimal()+
  xlab("Number of Antibiotic Episodes") + ylab("Percentage")

