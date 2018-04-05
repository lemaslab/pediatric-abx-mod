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
out.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\UFHEALTH\\RedCap\\rdata\\",sep="");out.dir

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

# look at data
dat=ufhealth.abx
head(dat); str(dat); names(dat)
dat$baby_med_date

# unique list of infant abx ip
abx.op=unique(dat$baby_meds);abx.op
abx.ip=unique(dat$baby_med_ip);abx.ip

# **************************************************************************** #
# ***********      Create mom-baby demography data set (with mode of delivery)                                             
# **************************************************************************** # 

# keep repeat counter for later

# link mom-baby demography
head(dat);names(dat)
dat2=dat %>%
  group_by(part_id) %>%
  #select(part_id,redcap_repeat_instrument,redcap_repeat_instance, baby_race, baby_ethnicity,mom_race_link, mom_ethnicity_link,baby_birth_wt_gr,delivery_mode,baby_gest_age) %>%
  #filter(redcap_repeat_instrument %in% c("baby_demography", "linked_mom_demography")) %>%
  filter(grepl("Baby",part_id));head(dat2);names(dat2)
length(unique(dat$part_id)) # 30540 (mom annd baby)
length(unique(dat2$part_id)) # 16684

# mom-baby demography
dat.mom_baby=dat2 %>%  # Recode gaps in data due to redcap export
  group_by(part_id) %>%
  mutate(mom_race_link = first(mom_race_link[!is.na(mom_race_link)]),
         mom_ethnicity_link=first(mom_ethnicity_link[!is.na(mom_ethnicity_link)]),
         baby_race=first(baby_race[!is.na(baby_race)]),
         baby_ethnicity=first(baby_ethnicity[!is.na(baby_ethnicity)]),
         baby_dob=first(baby_dob[!is.na(baby_dob)]),
         baby_birth_wt_gr=first(baby_birth_wt_gr[!is.na(baby_birth_wt_gr)]),
         delivery_mode=first(delivery_mode[!is.na(delivery_mode)]),
         baby_gest_age=first(baby_gest_age[!is.na(baby_gest_age)]),
         baby_nicu_los=first(baby_nicu_los[!is.na(baby_nicu_los)])) 
  # check data
  length(unique(dat.mom_baby$part_id)) # 16684
  head(dat.mom_baby);names(dat.mom_baby)

# **************************************************************************** #
# *****      Combine baby in-patient and out-patient abx                                               
# **************************************************************************** # 

# In-patient data (carry everything BUT baby_meds,baby_med_code,baby_med_order,days2_baby_meds,baby_med_date)
head(dat.mom_baby);names(dat.mom_baby)
dat.abx.ip=dat.mom_baby %>%
  group_by(part_id) %>%
  select(part_id,redcap_event_name,redcap_repeat_instrument,redcap_repeat_instance,  #base variables
         baby_med_order_ip, baby_mar_action_ip, baby_med_code_ip, # baby_abx_ip
         baby_med_ip, days2_baby_meds_ip, baby_med_ip_date,       # baby_abx_ip
         baby_race,baby_ethnicity,baby_dob,baby_birth_wt_gr,delivery_mode,baby_admit_date,
         baby_gest_age,baby_nicu_los,
         mom_ethnicity_link,mom_race_link,
         mom_prenat_apt_date_link,mom_prenat_ht_link,mom_prenat_wt_oz_link,days2_prenatal_apt_link,
         mom_prenat_enc_type_link,mom_prenat_ht_inch_link,mom_prenat_wt_lb_link) %>% 
  filter(redcap_repeat_instrument %in% c("baby_demography", "baby_antibiotics_ip", "linked_mom_prenatal_apt"))

# check data
  head(dat.abx.ip);names(dat.abx.ip)
## rename variables for merge
#----------------------------
dat.abx.ip.final=rename(dat.abx.ip, baby_med_order=baby_med_order_ip, baby_mar_action=baby_mar_action_ip,
                        baby_med_code=baby_med_code_ip, baby_meds=baby_med_ip, days2_baby_meds=days2_baby_meds_ip,
                        baby_med_date=baby_med_ip_date);head(dat.abx.ip.final);names(dat.abx.ip.final)
       
# out-patient data
dat.abx.op=dat.mom_baby %>%
  group_by(part_id) %>%
  select(part_id,redcap_event_name,redcap_repeat_instrument,redcap_repeat_instance,  #base variables 
         baby_med_code, baby_meds, days2_baby_meds, baby_med_date,  # baby_abx_rx
         baby_race,baby_ethnicity,baby_dob,baby_birth_wt_gr,delivery_mode,baby_admit_date,
         baby_gest_age,baby_nicu_los,
         mom_ethnicity_link,mom_race_link,
         mom_prenat_apt_date_link,mom_prenat_ht_link,mom_prenat_wt_oz_link,days2_prenatal_apt_link,
         mom_prenat_enc_type_link,mom_prenat_ht_inch_link,mom_prenat_wt_lb_link) %>%
  filter(redcap_repeat_instrument %in% c("baby_demography","baby_antibiotics_rx","linked_mom_prenatal_apt")) 
  
# check data
  head(dat.abx.op);names(dat.abx.op)
# modify for merge
  dat.abx.op$baby_mar_action=NA
  dat.abx.op$baby_med_order=NA
## arrange variables for merge
#----------------------------
  names(dat.abx.ip.final);length(names(dat.abx.ip.final))
  names(dat.abx.op);length(names(dat.abx.op))
dat.abx.op.final=dat.abx.op %>%
    group_by(part_id) %>%
    select(intersect(names(dat.abx.ip.final),names(dat.abx.op)))
  
names(dat.abx.ip.final); head(dat.abx.ip.final);str(dat.abx.ip.final);dim(dat.abx.ip.final)  
names(dat.abx.op.final); head(dat.abx.op.final);str(dat.abx.op.final);dim(dat.abx.op.final)

# combine data.frames
dat.abx.ALL=rbind(as.data.frame(dat.abx.ip.final),as.data.frame(dat.abx.op.final))
names(dat.abx.ALL);head(dat.abx.ALL)
str(dat.abx.ALL)
dat.abx.ALL$baby_med_date=as.Date(dat.abx.ALL$baby_med_date, format="%m/%d/%Y")

# sort by part_id (checked and works!)
dat.abx.ALL.sort=arrange(dat.abx.ALL,part_id, baby_med_date)
head(dat.abx.ALL.sort);names(dat.abx.ALL.sort)
length(unique(dat.abx.ALL.sort$part_id)) # 16684
dim(dat.abx.ALL.sort)
names(dat.abx.ALL.sort)

# **************************************************************************** #
# ********      Format gestational age variables: gest_age_wk                                              
# **************************************************************************** #

dat=tbl_df(dat.abx.ALL.sort)
names(dat)
dat$baby_gest_age

new=dat %>%
  separate(baby_gest_age, c("gest_wk","gest_day"), " ")
  new$gest_wk=as.numeric(new$gest_wk)
new1=new %>%
  mutate(gest_age_days=gest_wk*7) %>%
  separate(gest_day, c("day","temp_blank"), "/") 
new1$day=as.numeric(new1$day)
new2=new1 %>%
  mutate(gest_age_days=gest_age_days+day,
         gest_age_wk=gest_age_days/7)
  
# **************************************************************************** #
# ***************      Format mode-of-delivery variables                                              
# **************************************************************************** #

# recode variables
dat.abx.ALL.sort=new2
dat.abx.ALL.sort$gest_wk
unique(dat.abx.ALL.sort$delivery_mode) # how many unique entries for MOD; 22 (below)

# condense down to binary (vaginal/c-section)
dat.abx.ALL.sort$mod=NA
dat.abx.ALL.sort$mod=ifelse(dat.abx.ALL.sort$delivery_mode=="Vaginal&_Spontaneous_Delivery","vaginal",dat.abx.ALL.sort$mod)
dat.abx.ALL.sort$mod=ifelse(dat.abx.ALL.sort$delivery_mode=="C-Section&_Low_Transverse","c-section",dat.abx.ALL.sort$mod)
dat.abx.ALL.sort$mod=ifelse(dat.abx.ALL.sort$delivery_mode=="Vaginal&_Vacuum_(Extractor)","vaginal",dat.abx.ALL.sort$mod)
dat.abx.ALL.sort$mod=ifelse(dat.abx.ALL.sort$delivery_mode=="C-Section&_Unspecified","c-section",dat.abx.ALL.sort$mod)
dat.abx.ALL.sort$mod=ifelse(dat.abx.ALL.sort$delivery_mode=="Vaginal&_Forceps","vaginal",dat.abx.ALL.sort$mod)
dat.abx.ALL.sort$mod=ifelse(dat.abx.ALL.sort$delivery_mode=="Vertical_C-Section","vaginal",dat.abx.ALL.sort$mod)
dat.abx.ALL.sort$mod=ifelse(dat.abx.ALL.sort$delivery_mode=="C-Section&_Low_Vertical","c-section",dat.abx.ALL.sort$mod)
dat.abx.ALL.sort$mod=ifelse(dat.abx.ALL.sort$delivery_mode=="NOT_INCLUDED_IN_ORIGINAL_SOURCE",NA,dat.abx.ALL.sort$mod)
dat.abx.ALL.sort$mod=ifelse(dat.abx.ALL.sort$delivery_mode=="VBAC&_Spontaneous","vaginal",dat.abx.ALL.sort$mod)
dat.abx.ALL.sort$mod=ifelse(dat.abx.ALL.sort$delivery_mode=="C-Section&_Classical","c-section",dat.abx.ALL.sort$mod)
dat.abx.ALL.sort$mod=ifelse(dat.abx.ALL.sort$delivery_mode=="Vaginal&_Breech","vaginal",dat.abx.ALL.sort$mod)
dat.abx.ALL.sort$mod=ifelse(dat.abx.ALL.sort$delivery_mode=="Extramural_Delivery",NA,dat.abx.ALL.sort$mod)
dat.abx.ALL.sort$mod=ifelse(dat.abx.ALL.sort$delivery_mode=="Other",NA,dat.abx.ALL.sort$mod)
dat.abx.ALL.sort$mod=ifelse(dat.abx.ALL.sort$delivery_mode=="C-Section,_Low_Transverse","c-section",dat.abx.ALL.sort$mod)
dat.abx.ALL.sort$mod=ifelse(dat.abx.ALL.sort$delivery_mode=="Vaginal,_Spontaneous_Delivery","vaginal",dat.abx.ALL.sort$mod)
dat.abx.ALL.sort$mod=ifelse(dat.abx.ALL.sort$delivery_mode=="C-Section,_Unspecified","c-section",dat.abx.ALL.sort$mod)
dat.abx.ALL.sort$mod=ifelse(dat.abx.ALL.sort$delivery_mode=="C-Section,_Low_Vertical","c-section",dat.abx.ALL.sort$mod)
dat.abx.ALL.sort$mod=ifelse(dat.abx.ALL.sort$delivery_mode=="Vaginal,_Vacuum_(Extractor)","vaginal",dat.abx.ALL.sort$mod)
dat.abx.ALL.sort$mod=ifelse(dat.abx.ALL.sort$delivery_mode=="C-Section,_Classical","c-section",dat.abx.ALL.sort$mod)
dat.abx.ALL.sort$mod=ifelse(dat.abx.ALL.sort$delivery_mode=="VBAC,_Spontaneous","vaginal",dat.abx.ALL.sort$mod)
dat.abx.ALL.sort$mod=ifelse(dat.abx.ALL.sort$delivery_mode=="Vaginal,_Breech","vaginal",dat.abx.ALL.sort$mod)
dat.abx.ALL.sort$mod=ifelse(dat.abx.ALL.sort$delivery_mode=="Vaginal,_Forceps","vaginal",dat.abx.ALL.sort$mod)
head(dat.abx.ALL.sort) # did recode work? yes
names(dat.abx.ALL.sort)

# **************************************************************************** #
# *****      subset according to medication order                                              
# **************************************************************************** # 

names(dat.abx.ALL.sort)
unique(dat.abx.ALL.sort$baby_mar_action)

dat.new=dat.abx.ALL.sort %>%
  filter(baby_mar_action %in% c("GIVEN", "GIVEN BY OTHER"))

unique(dat.new$baby_mar_action)

# [1] GIVEN                      <NA>                       GIVEN BY OTHER            
# [4] DUE                        MISSED                     CANCELED ENTRY            
# [7] START/GIVEN                HELD                       IV STOP                   
# [10] NEW BAG                    MAR HOLD                   GIVEN-1ST DOSE EDUCATION  
# [13] IV RESUME                  NEW SYRINGE/CARTRIDGE      NEW BAG-1ST DOSE EDUCATION
# [16] STOPPED                    PENDING                    RESTARTED                 
# [19] BOLUS                      DRUG LEVEL(S) DUE          IV PAUSE                  
# [22] RETURN TO CABINET          SEE ALTERNATIVE           

# **************************************************************************** #
# *****     episode calculation (with mode of delivery)                                              
# **************************************************************************** # 

# status of data
dim(dat.new)  # 173139     28
dat.s2=dat.new
names(dat.s2)

# compute episode variable
head(dat.s2)
dat2=dat.s2 %>%
  group_by(part_id) %>%
  mutate(date = as.Date(baby_med_date, format="%m/%d/%Y")) %>%
  mutate(date1=first(date)) %>%
  mutate(obsvn=date-date1) %>%
  mutate(abx_episode = cumsum(c(1,diff(obsvn)>=7)),
         abx_episode_max=max(abx_episode))
names(dat2)
head(dat2)
dat2$abx_episode_max

# visualize
hist(dat2$abx_episode)
range(dat2$abx_episode)

# **************************************************************************** #
# *****      Wellness Visit Variables (with mode of delivery)                                              
# **************************************************************************** # 

# Notes on Wellness Visit (https://ufhealth.org/well-child-visits), 
# child wellness visits should occur at the following times: 
# By 1 month, 2 months, 4 months, 6 months, 9 months, 12 months, 15 months, 18 months, 2 year, 2.5 years, 3 years, each year until 21. Other sources have the same guidance:
# https://medlineplus.gov/ency/article/001928.htm
# https://www.healthychildren.org/English/family-life/health-management/Pages/Well-Child-Care-A-Check-Up-for-Success.aspx 

# time variable (intervals)
dat2$gest_wk
range(dat2$days2_baby_meds, na.rm=T)
dat2$wellness.visit=NA

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

# need to define some broad categories (0-6 months, 6-12 months, ect)

# Condense time variables into 2-3 buckets
# time variable (intervals)
dat2$wellness.visit_cats=NA

# 0-6 months (0-180 days)
dat2$wellness.visit_cats=ifelse(dat2$days2_baby_meds<=180,"t.<6mo", dat2$wellness.visit_cats)  

# 6-12 months (181-365 days)
dat2$wellness.visit_cats=ifelse(dat2$days2_baby_meds>180 & dat2$days2_baby_meds<=365,"t.6_12mo", dat2$wellness.visit_cats) 

# 12-24 months (366-730 days)
dat2$wellness.visit_cats=ifelse(dat2$days2_baby_meds>365 & dat2$days2_baby_meds<=730,"t.12_24mo", dat2$wellness.visit_cats) 

# 24-36 months (731-1095 days)
dat2$wellness.visit_cats=ifelse(dat2$days2_baby_meds>730 & dat2$days2_baby_meds<=1095,"t.24_36mo", dat2$wellness.visit_cats) 

# >36 months (1096 days)
dat2$wellness.visit_cats=ifelse(dat2$days2_baby_meds>1095,"t.>36mo", dat2$wellness.visit_cats) 

# what are the current ordering of factors?
levels(as.factor(dat2$wellness.visit_cats))

# order the factor levels
dat2$wellness.visit_cats=as.factor(dat2$wellness.visit_cats)
levels(dat2$wellness.visit_cats)
dat2$wellness.visit_cats <- factor(dat2$wellness.visit_cats, levels = c("t.<6mo","t.6_12mo","t.12_24mo", 
                                                                        "t.24_36mo","t.>36mo"))


# **************************************************************************** #
# *****      Export data                                              
# **************************************************************************** # 
now=Sys.Date(); today=format(now, format="%d%b%y")
save(dat2, file=paste0(out.dir,"abx_mod_",today,".rdata"))

