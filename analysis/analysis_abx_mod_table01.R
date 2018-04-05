##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        April 05, 2018 
# IRB:
# Description: Table 1: abx and mode-of-delivery EHR data 
# Data: C:\Users\djlemas\Dropbox (UFL)\02_Projects\UFHEALTH\RedCap\rdata

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
# location="djlemas";location
# location="Dominick";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\UFHEALTH\\RedCap\\rdata\\",sep="");work.dir
data.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\UFHEALTH\\RedCap\\rdata\\",sep="");data.dir
out.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\UFHEALTH\\tables\\",sep="");out.dir

# Set Working Directory
setwd(work.dir)
list.files()

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

# library(readxl)
library(data.table)
library(tidyr)
library(dplyr)
library(reshape2)

# **************************************************************************** #
# *****      l             load data: abx_mod_04Apr18.rdata       
# **************************************************************************** # 

load(file="abx_mod_05Apr18.rdata")
head(dat2)
names(dat2)

# quick diagnostics
length(unique(dat2$part_id)) # 13661

# **************************************************************************** #
# *****      subset the data: gest_age (wk) >37 & <42, birth_wt (gr) >500                                               
# **************************************************************************** # 

dat3=dat2 %>%
  filter(gest_age_wk>=37, gest_age_wk<=42, baby_birth_wt_gr>500)   

# check
range(dat3$gest_age_wk)
range(dat3$baby_birth_wt_gr)
length(unique(dat3$part_id)) # 5194

# **************************************************************************** #
# *****      formatt/subset the data: single observations                                              
# **************************************************************************** # 

# create table
names(dat3)
range(dat3$redcap_repeat_instance)

dat4=dat3 %>%
  filter(redcap_repeat_instance==1)
# check
range(dat4$gest_age_wk)
range(dat4$baby_birth_wt_gr)
length(unique(dat4$part_id)) # 4942

head(dat4)
dat4$abx_episode_max

# https://stackoverflow.com/questions/34587317/using-dplyr-to-create-summary-proportion-table-with-several-categorical-factor-v

dat4 %>%
  summarize(dat4)
m_mtcars <- melt(mtcars,measure.vars=c("gear","carb","cyl"))
res <- m_mtcars %>%
  group_by(am, variable, value) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))


mtcars %>%
  gather(variable, value, gear, carb, cyl) %>% # head()
  group_by(am, variable, value) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))
  
  
