##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        March 17, 2018 
# IRB:
# Description: Data management for baby abx, mode of delivery and mom demography
#              data extracted from RedCap. 
# Data: C:\Users\djlemas\Dropbox (UFL)\02_Projects\UFHEALTH\RedCap\redcap_export
# Analysis: Curate data for mode of delivery and infant outcomes. Goal is to
#           get the infant and mom data ready for analysis.

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Computer
# location="djlemas";location
# location="Dominick";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\UFHEALTH\\RedCap\\redcap_export\\",sep="");work.dir
data.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\UFHEALTH\\RedCap\\redcap_export\\",sep="");data.dir
out.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\UFHEALTH\\RedCap\\redcap_export\\",sep="");out.dir

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
# ****  UFHealthEarlyLifeExp_DATA_2_Infant_ABX_MOD_Mom_Visits_018-03-18_1152.csv                                              
# **************************************************************************** # 

# Read Data
data.file.name="UFHealthEarlyLifeExp_DATA_2_Infant_ABX_MOD_Mom_Visits_018-03-18_1152.csv";data.file.name
data.file.path=paste0(data.dir,"\\",data.file.name);data.file.path
ufhealth.abx<- read.csv(data.file.path,na.strings=c("","NA"));ufhealth.abx

  # need to create NA from blanks
    # ufhealth.abx[ufhealth.abx==" "|ufhealth.abx==" "]<-NA
    # ufhealth.abx[ufhealth.abx=="<NA>"]=NA 
    # head(ufhealth.abx)

# look at data
dat=ufhealth.abx
head(dat); str(dat); names(dat)
dat$baby_med_date

# unique list of infant abx ip
abx.op=unique(dat$baby_meds);abx.op
abx.ip=unique(dat$baby_med_ip);abx.ip

# reshape
# head(dat);names(dat)
# dat.reshape=melt(dat, id=c("part_id","redcap_repeat_instrument"));head(dat.reshape)
# head(dat.na)

# **************************************************************************** #
# ***********      Create mom-baby demography data set (with mode of delivery)                                             
# **************************************************************************** # 

# link mom-baby demography
head(dat);names(dat)
dat2=dat %>%
  group_by(part_id) %>%
  select(part_id,redcap_repeat_instrument,baby_race, baby_ethnicity,mom_race_link, mom_ethnicity_link,baby_birth_wt_gr,delivery_mode,baby_gest_age) %>%
  filter(redcap_repeat_instrument %in% c("baby_demography", "linked_mom_demography")) %>%
  filter(grepl("Baby",part_id));head(dat2);names(dat2)

# mom-baby demography
dat.mom_baby=dat2 %>%  # Limit only to babys with linked data 
  group_by(part_id) %>%
  mutate(mom_race = first(mom_race_link[!is.na(mom_race_link)]),
         mom_ethnicity=first(mom_ethnicity_link[!is.na(mom_ethnicity_link)])) %>%
  select(part_id, redcap_repeat_instrument, baby_race, mom_race,baby_ethnicity, mom_ethnicity,baby_birth_wt_gr,delivery_mode,baby_gest_age) %>%
  filter(redcap_repeat_instrument=="baby_demography") 
  # check data
  length(unique(dat.mom_baby$part_id)) # 16684
  head(dat.mom_baby);names(dat.mom_baby)

# **************************************************************************** #
# ***************      Format mode-of-delivery variables                                              
# **************************************************************************** #

# recode variables
unique(dat.mom_baby$delivery_mode) # how many unique entries for MOD; 24 (below)

# [1] Vaginal&_Spontaneous_Delivery   C-Section&_Low_Transverse       Vaginal&_Vacuum_(Extractor)    
# [4] C-Section&_Unspecified          Vaginal&_Forceps                Vertical_C-Section             
# [7] C-Section&_Low_Vertical         NOT_INCLUDED_IN_ORIGINAL_SOURCE VBAC&_Spontaneous              
# [10] C-Section&_Classical            Vaginal&_Breech                 Extramural_Delivery            
# [13] Other                           C-Section,_Low_Transverse       Vaginal,_Spontaneous_Delivery  
# [16] C-Section,_Unspecified          C-Section,_Low_Vertical         Vaginal,_Vacuum_(Extractor)    
# [19] C-Section,_Classical            VBAC,_Spontaneous               Vaginal,_Breech                
# [22] Vaginal,_Forceps

# **************************************************************************** #
# *****      Combine baby in-patient and out-patient abx (with mode of delivery)                                              
# **************************************************************************** # 

# In-patient data
head(dat);names(dat)
dat.abx.ip=dat %>%
  group_by(part_id) %>%
  select(part_id,redcap_repeat_instrument,delivery_mode, baby_med_order_ip, 
         baby_mar_action_ip, baby_med_code_ip, 
         baby_med_ip, days2_baby_meds_ip, baby_med_ip_date) %>%
  filter(redcap_repeat_instrument %in% c("baby_demography", "baby_antibiotics_ip")) %>%
  mutate(mode_of_delivery = first(delivery_mode[!is.na(delivery_mode)])) %>%
  select(part_id,redcap_repeat_instrument,mode_of_delivery,baby_med_order_ip, baby_mar_action_ip, 
         baby_med_code_ip, baby_med_ip, days2_baby_meds_ip, baby_med_ip_date) %>%
  filter(redcap_repeat_instrument %in% c("baby_antibiotics_ip"))
  # check data
  head(dat.abx.ip);names(dat.abx.ip)
## rename variables for merge
#----------------------------
dat.abx.ip.final=rename(dat.abx.ip, baby_med_order=baby_med_order_ip, baby_mar_action=baby_mar_action_ip,
                        baby_med_code=baby_med_code_ip, baby_meds=baby_med_ip, days2_baby_meds=days2_baby_meds_ip,
                        baby_med_date=baby_med_ip_date);head(dat.abx.ip.final);names(dat.abx.ip.final)
       

# out-patient data
dat.abx.op=dat %>%
  group_by(part_id) %>%
  select(part_id,redcap_repeat_instrument,delivery_mode, baby_med_order, 
         baby_med_code, baby_meds, days2_baby_meds, baby_med_date) %>%
  filter(redcap_repeat_instrument %in% c("baby_demography", "baby_antibiotics_rx")) %>%
  mutate(mode_of_delivery = first(delivery_mode[!is.na(delivery_mode)])) %>%
  select(part_id,redcap_repeat_instrument,mode_of_delivery,baby_med_order, 
         baby_med_code, baby_meds, days2_baby_meds, baby_med_date) %>%
  filter(redcap_repeat_instrument %in% c("baby_antibiotics_rx"))
  # check data
  head(dat.abx.op);names(dat.abx.op)
  # modify for merge
  dat.abx.op$baby_mar_action=NA
## arrange variables for merge
#----------------------------
dat.abx.op.final=dat.abx.op %>%
    group_by(part_id) %>%
    select(part_id,redcap_repeat_instrument,mode_of_delivery,
           baby_med_order, baby_mar_action, baby_med_code, 
           baby_meds, days2_baby_meds, baby_med_date)
  
names(dat.abx.ip.final); head(dat.abx.ip.final);str(dat.abx.ip.final);dim(dat.abx.ip.final)  
names(dat.abx.op.final); head(dat.abx.op.final);str(dat.abx.op.final);dim(dat.abx.op.final)

# combine data.frames
dat.abx.ALL=rbind(as.data.frame(dat.abx.ip.final),as.data.frame(dat.abx.op.final))
names(dat.abx.ALL);head(dat.abx.ALL)
str(dat.abx.ALL)
dat.abx.ALL$baby_med_date=as.Date(dat.abx.ALL$baby_med_date, format="%m/%d/%Y")

# sort by part_id (checked and works!)
dat.abx.ALL.sort=arrange(dat.abx.ALL,part_id, baby_med_date)
head(dat.abx.ALL.sort)

# **************************************************************************** #
# *****      episode calculation (with mode of delivery)                                              
# **************************************************************************** # 

# status of data
dim(dat.abx.ALL.sort)  # 75523     9
dat.s2=dat.abx.ALL.sort
names(dat.s2)

# select out mar_action (need to code out subset)


# compute episode variable
head(dat.s2)
dat2=dat.s2 %>%
  group_by(part_id) %>%
  mutate(date = as.Date(baby_med_date, format="%m/%d/%Y")) %>%
  mutate(date1=first(date)) %>%
  mutate(obsvn=date-date1) %>%
  mutate(abx_episode = cumsum(c(1,diff(obsvn)>=7))) %>%
  select(part_id,redcap_repeat_instrument,mode_of_delivery,baby_med_order,baby_mar_action,baby_med_code,baby_meds,days2_baby_meds,baby_med_date,abx_episode)
names(dat2)
head(dat2)

# **************************************************************************** #
# *****      Wellness Visit Variables (with mode of delivery)                                              
# **************************************************************************** # 

# Notes on Wellness Visit (https://ufhealth.org/well-child-visits), 
# child wellness visits should occur at the following times: 
# By 1 month, 2 months, 4 months, 6 months, 9 months, 12 months, 15 months, 18 months, 2 year, 2.5 years, 3 years, each year until 21. Other sources have the same guidance:
# https://medlineplus.gov/ency/article/001928.htm
# https://www.healthychildren.org/English/family-life/health-management/Pages/Well-Child-Care-A-Check-Up-for-Success.aspx 

# time variable (intervals)
range(dat2$days2_baby_meds)
dat2$wellness.visit=NA
dat2$wellness.visit[1:20]

# 3 days variable (0-3 days)
dat2$wellness.visit=ifelse(dat2$days2_baby_meds<=3,"t.3_days", dat2$wellness.visit)  

# 2 weeks Variable (14 days)
dat2$wellness.visit=ifelse(dat2$days2_baby_meds>3 & dat2$days2_baby_meds<=14,"t.2_wks", dat2$wellness.visit)  

# 1 month Variable (15-30 days)
dat2$wellness.visit=ifelse(dat2$days2_baby_meds>14 & dat2$days2_baby_meds<=30,"t.1_mo", dat2$wellness.visit)

# 2 month Variable (31-60 days)
dat2$wellness.visit=ifelse(dat2$days2_baby_meds>30 & dat2$days2_baby_meds<=60,"t.2_mo", dat2$wellness.visit)

# 4 month Variable (61-120 days)
dat2$wellness.visit=ifelse(dat2$days2_baby_meds>60 & dat2$days2_baby_meds<=120,"t.4_mo", dat2$wellness.visit)

# 6 month Variable (121-180 days)
dat2$wellness.visit=ifelse(dat2$days2_baby_meds>120 & dat2$days2_baby_meds<=180,"t.6_mo", dat2$wellness.visit)

# 9 month Variable (181-270 days)
dat2$wellness.visit=ifelse(dat2$days2_baby_meds>181 & dat2$days2_baby_meds<=270,"t.9_mo", dat2$wellness.visit)

# 12 month Variable (271-365 days)
dat2$wellness.visit=ifelse(dat2$days2_baby_meds>271 & dat2$days2_baby_meds<=365,"t.12_mo", dat2$wellness.visit)

# 15 months variable (366-450 days)
dat2$wellness.visit=ifelse(dat2$days2_baby_meds>366 & dat2$days2_baby_meds<=450,"t.15_mo", dat2$wellness.visit)

# 18 month variable (451-540 days)
dat2$wellness.visit=ifelse(dat2$days2_baby_meds>451 & dat2$days2_baby_meds<=540,"t.18_mo", dat2$wellness.visit)

# 2 year variable (540-730 days)
dat2$wellness.visit=ifelse(dat2$days2_baby_meds>540 & dat2$days2_baby_meds<=730,"t.2_yr", dat2$wellness.visit)

# 2.5 year variable (731-910 days)
dat2$wellness.visit=ifelse(dat2$days2_baby_meds>731 & dat2$days2_baby_meds<=910,"t.2.5_yr", dat2$wellness.visit)

# 3 year variable (911-1095 days)
dat2$wellness.visit=ifelse(dat2$days2_baby_meds>911 & dat2$days2_baby_meds<=1095,"t.3_yr", dat2$wellness.visit)

# 4 year variable (1096-1460 days)
dat2$wellness.visit=ifelse(dat2$days2_baby_meds>1096 & dat2$days2_baby_meds<=1460,"t.4_yr", dat2$wellness.visit)

# 5 year variable (1461-1825 days)
dat2$wellness.visit=ifelse(dat2$days2_baby_meds>1461,"t.5_yr", dat2$wellness.visit)

# order the factor levels
dat2$wellness.visit=as.factor(dat2$wellness.visit)
dat2$wellness.visit <- factor(dat2$wellness.visit, levels = c("t.3_days","t.2_wks","t.1_mo","t.2_mo","t.4_mo","t.6_mo","t.9_mo","t.12_mo","t.15_mo",
                                                "t.18_mo","t.2_yr","t.2.5_yr","t.3_yr","t.4_yr", "t.5_yr"))
levels(dat2$wellness.visit)


# **************************************************************************** #
# *****      Counts by wellness.visit (with mode of delivery)                                              
# **************************************************************************** # 

# report highest episode within part_id and wellness.visit
head(dat2)
names(dat2)


# counts of abx episodes within wellness visits
dat2 %>%
  group_by(wellness.visit) %>%
  count(abx_episode)

# counts of abx within each time cat
dat2 %>%
  group_by(wellness.visit) %>%
  tally(abx_episode)

# mean of abx within each time cat
dat2 %>%
  group_by(wellness.visit) %>%
  tally(mean(abx_episode))

# spread the wellness categorical variable
dat.wide<- dcast(dat2, part_id + redcap_repeat_instrument + mode_of_delivery + baby_med_order 
             + baby_mar_action + baby_med_code + baby_meds + days2_baby_meds 
             + baby_med_date ~ wellness.visit)
dat=as.data.table(dat.wide)

A=dat %>%
  as.data.table() %>%
  transmute(t.3day=t.3_days,
            t.2wk=t.3_days+t.2_wks,
            t.1mo=t.3_days+t.2_wks+t.1_mo,
            t.2mo=t.3_days+t.2_wks+t.1_mo+t.2_mo,
            t.4mo=t.3_days+t.2_wks+t.1_mo+t.2_mo+t.4_mo,
            t.6mo=t.3_days+t.2_wks+t.1_mo+t.2_mo+t.4_mo+t.6_mo,
            t.9mo=t.3_days+t.2_wks+t.1_mo+t.2_mo+t.4_mo+t.6_mo+t.9_mo,
            t.12mo=t.3_days+t.2_wks+t.1_mo+t.2_mo+t.4_mo+t.6_mo+t.9_mo+t.12_mo,
            t.15mo=t.3_days+t.2_wks+t.1_mo+t.2_mo+t.4_mo+t.6_mo+t.9_mo+t.12_mo+t.15_mo,
            t.18mo=t.3_days+t.2_wks+t.1_mo+t.2_mo+t.4_mo+t.6_mo+t.9_mo+t.12_mo+t.15_mo+t.18_mo,
            t.2yr=t.3_days+t.2_wks+t.1_mo+t.2_mo+t.4_mo+t.6_mo+t.9_mo+t.12_mo+t.15_mo+t.18_mo+t.2_yr,
            t.3yr=t.3_days+t.2_wks+t.1_mo+t.2_mo+t.4_mo+t.6_mo+t.9_mo+t.12_mo+t.15_mo+t.18_mo+t.2_yr+t.3_yr,
            t.4yr=t.3_days+t.2_wks+t.1_mo+t.2_mo+t.4_mo+t.6_mo+t.9_mo+t.12_mo+t.15_mo+t.18_mo+t.2_yr+t.3_yr+t.4_yr,
            t.5yr=t.3_days+t.2_wks+t.1_mo+t.2_mo+t.4_mo+t.6_mo+t.9_mo+t.12_mo+t.15_mo+t.18_mo+t.2_yr+t.3_yr+t.4_yr+t.5_yr) %>%
    as.data.table() %>%
    tally(t.3day,t.2wk,t.1mo,t.2mo)

A
range(A$t.3day)
names(A)
names(dat)
http://stcorp.nl/R_course/tutorial_dplyr.html

