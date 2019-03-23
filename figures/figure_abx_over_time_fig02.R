##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        March 14, 2019 
# IRB:
# Description: Cummulative mean function of Abx over time paper #1
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
# *****      load data: Recurrent_Events_Data_for_fig02.csv      
# **************************************************************************** # 

# read data
data.file.name="Recurrent_Events_Data_for_fig02.csv";data.file.name
data.file.path=paste0(data.dir,"\\",data.file.name);data.file.path
time<- read.csv(data.file.path);time

# diagnostics
dat=time
dim(dat)
names(dat)

# plot with days on x-axis
#-------------------------
ggplot(dat, aes(x=upper_abx, y=CMF, col=csection)) + geom_line()+
# p+geom_ribbon(aes(ymin=dat$LowerCMF, ymax=dat$UpperCMF), linetype=0.5, alpha=0.001)
  theme_minimal()+
  xlab("Postnatal Age in Day") + ylab("Mean Antibiotic Episodes")+
  scale_color_discrete(name="Mode of Delivery",labels=c("Vaginal Delivery", "Cesarean Section"))

# convert days to months
dat$months=dat$upper_abx/365

# plot with years on x-axis
#--------------------------
ggplot(dat, aes(x=months, y=CMF, col=csection)) + geom_line()+
  # p+geom_ribbon(aes(ymin=dat$LowerCMF, ymax=dat$UpperCMF), linetype=0.5, alpha=0.001)
  theme_minimal()+
  xlab("Postnatal Age in Years") + ylab("Mean Antibiotic Episodes")+
  scale_color_discrete(name="Mode of Delivery",labels=c("Vaginal Delivery", "Cesarean Section"))
