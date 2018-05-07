##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        May 01, 2018 
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
misc.dir=paste("C:\\Users\\",location,"\\Dropbox (UFL)\\02_Projects\\UFHEALTH\\RedCap\\",sep="");misc.dir

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
library(reshape2)

# **************************************************************************** #
# ****  UFHealthEarlyLifeExp_DATA_2018-05-01_1303.csv                                              
# **************************************************************************** # 

# Read Data
data.file.name="UFHealthEarlyLifeExp_DATA_2018-05-01_1303.csv";data.file.name
data.file.path=paste0(data.dir,"\\",data.file.name);data.file.path
ufhealth.abx<- read.csv(data.file.path,na.strings=c("","NA"));ufhealth.abx

# look at data
dat=ufhealth.abx
head(dat); str(dat); names(dat)
dat$baby_med_date
length(unique(dat$part_id))  # 30540 (mom & baby)

# **************************************************************************** #
# ***********      Create mom-baby demography data set (with mode of delivery)                                             
# **************************************************************************** # 

# link mom-baby demography
head(dat);names(dat)
dat2=dat %>%
  group_by(part_id) %>%
  filter(grepl("Baby",part_id));
head(dat2);names(dat2)
length(unique(dat$part_id)) # 30540 (mom annd baby)
length(unique(dat2$part_id)) # 16684 (baby)

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
unique(dat.mom_baby$redcap_repeat_instrument)

dat.abx.ip=dat.mom_baby %>%
  group_by(part_id) %>%
  select(part_id,redcap_event_name,redcap_repeat_instrument,redcap_repeat_instance,  #base variables
         baby_med_order_ip, baby_mar_action_ip, baby_med_code_ip, # baby_abx_ip
         baby_med_ip, days2_baby_meds_ip, baby_med_ip_date,       # baby_abx_ip
         baby_race,baby_ethnicity,baby_dob,baby_birth_wt_gr,delivery_mode,baby_admit_date,
         baby_gest_age,baby_nicu_los,
         mom_ethnicity_link,mom_race_link,
         mom_prenat_apt_date_link,mom_prenat_ht_link,mom_prenat_wt_oz_link,days2_prenatal_apt_link,
         mom_prenat_enc_type_link,mom_prenat_ht_inch_link,mom_prenat_wt_lb_link,
         baby_ht_cm, baby_wt_kgs, baby_head_circ_cm, days2_baby_wellvisit) %>% 
  filter(redcap_repeat_instrument %in% c("baby_demography","baby_antibiotics_ip","linked_mom_prenatal_apt","baby_wellvisit"))

# check data
  head(dat.abx.ip);names(dat.abx.ip)
  length(unique(dat.abx.ip$part_id)) # 16684
  temp=dat.abx.ip$redcap_repeat_instrument=="baby_antibiotics_ip"
  table(temp)
## rename variables for merge
#----------------------------
dat.abx.ip.final=rename(dat.abx.ip, baby_med_order=baby_med_order_ip, baby_mar_action=baby_mar_action_ip,
                        baby_med_code=baby_med_code_ip, baby_meds=baby_med_ip, days2_baby_meds=days2_baby_meds_ip,
                        baby_med_date=baby_med_ip_date);head(dat.abx.ip.final);names(dat.abx.ip.final)
# last check
unique(dat.abx.ip.final$redcap_repeat_instrument)  
         
# out-patient data
dat.abx.op=dat.mom_baby %>%
  group_by(part_id) %>%
  select(part_id,redcap_event_name,redcap_repeat_instrument,redcap_repeat_instance,  #base variables 
         baby_med_code, baby_meds, days2_baby_meds, baby_med_date,  # baby_abx_rx
         baby_race,baby_ethnicity,baby_dob,baby_birth_wt_gr,delivery_mode,baby_admit_date,
         baby_gest_age,baby_nicu_los,
         mom_ethnicity_link,mom_race_link,
         mom_prenat_apt_date_link,mom_prenat_ht_link,mom_prenat_wt_oz_link,days2_prenatal_apt_link,
         mom_prenat_enc_type_link,mom_prenat_ht_inch_link,mom_prenat_wt_lb_link,
         baby_ht_cm, baby_wt_kgs, baby_head_circ_cm, days2_baby_wellvisit) %>%
  filter(redcap_repeat_instrument %in% c("baby_demography","baby_antibiotics_rx","linked_mom_prenatal_apt","baby_wellvisit")) 
  
# check data
  head(dat.abx.op);names(dat.abx.op)
  temp=dat.abx.op$redcap_repeat_instrument=="baby_antibiotics_rx"
  table(temp)
# modify for merge
  dat.abx.op$baby_mar_action="GIVEN_RX"
  dat.abx.op$baby_med_order=NA

## arrange variables for merge
#----------------------------
  # abx_ip
  names(dat.abx.ip.final);length(names(dat.abx.ip.final));dim(dat.abx.ip.final)
  length(unique(dat.abx.ip.final$part_id)) # 16684 participants
  temp=dat.abx.ip.final$redcap_repeat_instrument=="baby_antibiotics_ip"
  table(temp)

  # abx_rx
  names(dat.abx.op);length(names(dat.abx.op));dim(dat.abx.op)
  length(unique(dat.abx.op$part_id))
  temp=dat.abx.op$redcap_repeat_instrument=="baby_antibiotics_rx"
  table(temp)
  
# limit to variable names shared between both data sets (ip and rx)
  dat.abx.op.final=dat.abx.op %>%
    group_by(part_id) %>%
    select(intersect(names(dat.abx.ip.final),names(dat.abx.op)))
  
names(dat.abx.ip.final); head(dat.abx.ip.final);str(dat.abx.ip.final);dim(dat.abx.ip.final)  
names(dat.abx.op.final); head(dat.abx.op.final);str(dat.abx.op.final);dim(dat.abx.op.final)

# combine data.frames
dat.abx.ALL=rbind(as.data.frame(dat.abx.ip.final),as.data.frame(dat.abx.op.final))
names(dat.abx.ALL);head(dat.abx.ALL)
table(dat.abx.ALL$redcap_repeat_instrument)
str(dat.abx.ALL)
dat.abx.ALL$baby_med_date=as.Date(dat.abx.ALL$baby_med_date, format="%m/%d/%Y")

# sort by part_id (checked and works!)
dat.abx.ALL.sort=arrange(dat.abx.ALL,part_id, baby_med_date)
head(dat.abx.ALL.sort);names(dat.abx.ALL.sort)
length(unique(dat.abx.ALL.sort$part_id)) # 16684
dim(dat.abx.ALL.sort)
names(dat.abx.ALL.sort)

# check abx counts within groups (ip and rx)
table(dat.abx.ALL.sort$redcap_repeat_instrument)

# **************************************************************************** #
# ***********      Create broad/narrow antibiotics variables                                              
# **************************************************************************** # 

# unique list of infant abx ip
abx.op=unique(dat.abx.ALL.sort$baby_meds);abx.op
abx.ip=unique(dat.abx.ALL.sort$baby_med_ip);abx.ip

# what are the counts/observations for "abx_ip" and "abx_rx"
unique(dat.abx.ALL.sort$redcap_repeat_instrument)
table(dat.abx.ALL.sort$redcap_repeat_instrument)
# baby_antibiotics_ip     baby_antibiotics_rx         baby_demography          baby_wellvisit 
# 70367                    5156                   33368                  102882 
# linked_mom_demography linked_mom_prenatal_apt 
# 0                   64248  

# read ABX Classification data
data.file.name="abx_medication_names\\abx_classification_06May18_v3.xlsx";data.file.name

# abx.op- data
abx.names=read_xlsx(paste(misc.dir,data.file.name,sep=""), sheet = "abx.combined.5-6-18", range = NULL, col_names = TRUE,
                 col_types = NULL, na = "", trim_ws = TRUE, skip = 0);abx.names
names(abx.names)
# what is the count for op/ip
table(abx.names$redcap_repeat_instrument)
# baby_antibiotics_ip baby_antibiotics_rx 
# 53                  82 

length(abx.names$baby_meds) #135
length(unique(abx.names$baby_meds)) # 102  (need to know where these observations went)
which(duplicated(abx.names$baby_meds)) # dropped duplicates

table(abx.names$classification)
# Broad     Drop   Narrow 
# 41        20     41 

# Merge Variables into dataframe
abx.name=abx.names[2:5]
dat.abx.ALL.sort_02=left_join(dat.abx.ALL.sort, abx.name, by = c("baby_meds"))
names(dat.abx.ALL.sort_02)

# drop duplicate variables and rename
dat.abx.ALL.sort_03=dat.abx.ALL.sort_02 %>% 
  select (-c(redcap_repeat_instrument.y)) %>%
  rename(redcap_repeat_instrument=redcap_repeat_instrument.x)
table(dat.abx.ALL.sort_03$redcap_repeat_instrument)
dim(dat.abx.ALL.sort_03)  # 276021     33

# **************************************************************************** #
# ********      Format gestational age variables: gest_age_wk                                              
# **************************************************************************** #

dat=tbl_df(dat.abx.ALL.sort_03)
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

# check data
length(unique(dat.abx.ALL.sort$part_id)) # 16684
dim(dat.abx.ALL.sort)
names(dat.abx.ALL.sort)

# check abx counts within groups (ip and rx)
table(dat.abx.ALL.sort$redcap_repeat_instrument)

# **************************************************************************** #
# *****     episode calculation (with mode of delivery)                                              
# **************************************************************************** # 

# status of data
dim(dat.abx.ALL.sort)  # 343359     39
dat.s2=dat.abx.ALL.sort
names(dat.s2)

# drop out non-antibiotics
#-------------------------
unique(dat.s2$baby_meds)
table(dat.s2$classification)
    # Broad   Drop   Narrow 
    # 6754    5127   63642
dat.s3=dat.s2[dat.s2$classification != "Drop", ]
str(dat.s3)
head(dat.s3)
table(dat.s3$classification)  # droped only the "dropped"
    # Broad Narrow 
    # 6754  63642

# compute episode_total variable
#-------------------------------
head(dat.s3)
dat2=dat.abx.ALL.sort %>%
  group_by(part_id) %>%
  mutate(date = as.Date(baby_med_date, format="%m/%d/%Y")) %>%
  mutate(date1=first(date)) %>%
  mutate(obsvn=date-date1) %>%
  mutate(abx_episode_total = cumsum(c(1,diff(obsvn)>=10))) %>%
  select (-c(obsvn, date1, date, day, temp_blank)) 
names(dat2)
head(dat2)
dat2$abx_episode_total
range(dat2$abx_episode_total, na.rm=T) # 1-33

# compute narrow variable (start here)
head(dat2)
dat2$data.narrow2=NA
dat2$baby_med_date
dat3=dat2 %>%
  group_by(part_id) %>%
  mutate(date = as.Date(baby_med_date, format="%m/%d/%Y",origin="1970-01-01")) %>%
  mutate(date1=as.character(first(date))) %>%
  mutate(data.narrow2=ifelse(classification=="Narrow", date1 , data.narrow2)) %>%
  mutate(data.narrow=as.Date(data.narrow2, format="%Y-%m-%d")) %>%
  mutate(obsvn=date-data.narrow) %>%
  mutate(abx_episode_narrow = ifelse(classification=="Narrow", cumsum(c(1,diff(obsvn)>=10)), NA)) %>%
  select (-c(obsvn, date1, date, data.narrow, data.narrow2)) 
dat3$abx_episode_narrow
range(dat3$abx_episode_narrow, na.rm=T) # 1-10

# compute broad variable
head(dat3)
dat3$baby_med_date
dat3$data.broad2=NA
dat4=dat3 %>%
  group_by(part_id) %>%
  mutate(date = as.Date(baby_med_date, format="%m/%d/%Y",origin="1970-01-01")) %>%
  mutate(date1=as.character(first(date))) %>%
  mutate(data.broad2=ifelse(classification=="Broad", date1 , data.broad2)) %>%
  mutate(data.broad=as.Date(data.broad2, format="%Y-%m-%d")) %>%
  mutate(obsvn=date-data.broad) %>%
  mutate(abx_episode_broad = ifelse(classification=="Broad", cumsum(c(1,diff(obsvn)>=10)), NA)) %>%
  select (-c(obsvn, date1, date, data.broad, data.broad2)) 
names(dat4)
dat4$abx_episode_broad
range(dat4$abx_episode_broad, na.rm=T)

# visualize
hist(dat4$abx_episode_total)
hist(dat4$abx_episode_narrow)
hist(dat4$abx_episode_broad)


# **************************************************************************** #
# *****      Wellness Visit Variables (with mode of delivery)                                              
# **************************************************************************** # 

# Notes on Wellness Visit (https://ufhealth.org/well-child-visits), 
# child wellness visits should occur at the following times: 
# By 1 month, 2 months, 4 months, 6 months, 9 months, 12 months, 15 months, 18 months, 2 year, 2.5 years, 3 years, each year until 21. Other sources have the same guidance:
# https://medlineplus.gov/ency/article/001928.htm
# https://www.healthychildren.org/English/family-life/health-management/Pages/Well-Child-Care-A-Check-Up-for-Success.aspx 

# time variable (intervals)
dat4$gest_wk
range(dat4$days2_baby_wellvisit, na.rm=T)
dat4$wellness_visit=NA

# 3 days variable (0-3 days)
dat4$wellness_visit=ifelse(dat4$days2_baby_wellvisit<=3,"t.3_days", dat4$wellness_visit)  

# 2 weeks Variable (14 days)
dat4$wellness_visit=ifelse(dat4$days2_baby_wellvisit>3 & dat4$days2_baby_wellvisit<=14,"t.2_wks", dat4$wellness_visit)  

# 1 month Variable (15-30 days)
dat4$wellness_visit=ifelse(dat4$days2_baby_wellvisit>14 & dat4$days2_baby_wellvisit<=30,"t.1_mo", dat4$wellness_visit)

# 2 month Variable (31-60 days)
dat4$wellness_visit=ifelse(dat4$days2_baby_wellvisit>30 & dat4$days2_baby_wellvisit<=60,"t.2_mo", dat4$wellness_visit)

# 4 month Variable (61-120 days)
dat4$wellness_visit=ifelse(dat4$days2_baby_wellvisit>60 & dat4$days2_baby_wellvisit<=120,"t.4_mo", dat4$wellness_visit)

# 6 month Variable (121-180 days)
dat4$wellness_visit=ifelse(dat4$days2_baby_wellvisit>120 & dat4$days2_baby_wellvisit<=180,"t.6_mo", dat4$wellness_visit)

# 9 month Variable (181-270 days)
dat4$wellness_visit=ifelse(dat4$days2_baby_wellvisit>181 & dat4$days2_baby_wellvisit<=270,"t.9_mo", dat4$wellness_visit)

# 12 month Variable (271-365 days)
dat4$wellness_visit=ifelse(dat4$days2_baby_wellvisit>271 & dat4$days2_baby_wellvisit<=365,"t.12_mo", dat4$wellness_visit)

# 15 months variable (366-450 days)
dat4$wellness_visit=ifelse(dat4$days2_baby_wellvisit>366 & dat4$days2_baby_wellvisit<=450,"t.15_mo", dat4$wellness_visit)

# 18 month variable (451-540 days)
dat4$wellness_visit=ifelse(dat4$days2_baby_wellvisit>451 & dat4$days2_baby_wellvisit<=540,"t.18_mo", dat4$wellness_visit)

# 2 year variable (540-730 days)
dat4$wellness_visit=ifelse(dat4$days2_baby_wellvisit>540 & dat4$days2_baby_wellvisit<=730,"t.2_yr", dat4$wellness_visit)

# 2.5 year variable (731-910 days)
dat4$wellness_visit=ifelse(dat4$days2_baby_wellvisit>731 & dat4$days2_baby_wellvisit<=910,"t.2.5_yr", dat4$wellness_visit)

# 3 year variable (911-1095 days)
dat4$wellness_visit=ifelse(dat4$days2_baby_wellvisit>911 & dat4$days2_baby_wellvisit<=1095,"t.3_yr", dat4$wellness_visit)

# 4 year variable (1096-1460 days)
dat4$wellness_visit=ifelse(dat4$days2_baby_wellvisit>1096 & dat4$days2_baby_wellvisit<=1460,"t.4_yr", dat4$wellness_visit)

# 5 year variable (1461-1825 days)
dat4$wellness_visit=ifelse(dat4$days2_baby_wellvisit>1461,"t.5_yr", dat4$wellness_visit)

# order the factor levels
dat4$wellness_visit=as.factor(dat4$wellness_visit)
dat4$wellness_visit <- factor(dat4$wellness_visit, levels = c("t.3_days","t.2_wks","t.1_mo","t.2_mo","t.4_mo","t.6_mo","t.9_mo","t.12_mo","t.15_mo",
                                                "t.18_mo","t.2_yr","t.2.5_yr","t.3_yr","t.4_yr", "t.5_yr"))
levels(dat4$wellness_visit)

# need to define some broad categories (0-6 months, 6-12 months, ect)

# Condense time variables into 2-3 buckets
# time variable (intervals)
dat4$wellness_visit_cats=NA

# 0-6 months (0-180 days)
dat4$wellness_visit_cats=ifelse(dat4$days2_baby_wellvisit<=180,"t.<6mo", dat4$wellness_visit_cats)  

# 6-12 months (181-365 days)
dat4$wellness_visit_cats=ifelse(dat4$days2_baby_wellvisit>180 & dat4$days2_baby_wellvisit<=365,"t.6_12mo", dat4$wellness_visit_cats) 

# 12-24 months (366-730 days)
dat4$wellness_visit_cats=ifelse(dat4$days2_baby_wellvisit>365 & dat4$days2_baby_wellvisit<=730,"t.12_24mo", dat4$wellness_visit_cats) 

# 24-36 months (731-1095 days)
dat4$wellness_visit_cats=ifelse(dat4$days2_baby_wellvisit>730 & dat4$days2_baby_wellvisit<=1095,"t.24_36mo", dat4$wellness_visit_cats) 

# >36 months (1096 days)
dat4$wellness_visit_cats=ifelse(dat4$days2_baby_wellvisit>1095,"t.>36mo", dat4$wellness_visit_cats) 

# what are the current ordering of factors?
levels(as.factor(dat4$wellness_visit_cats))

# order the factor levels
dat4$wellness_visit_cats=as.factor(dat4$wellness_visit_cats)
levels(dat4$wellness_visit_cats)
dat4$wellness_visit_cats <- factor(dat4$wellness_visit_cats, levels = c("t.<6mo","t.6_12mo","t.12_24mo", 
                                                                        "t.24_36mo","t.>36mo"))
table(dat4$wellness_visit_cats)

# **************************************************************************** #
# *****      Longitudinal Variables                                              
# **************************************************************************** # 

# NOTE: what are reasonable cutt-offs.

range(dat4$days2_baby_meds, na.rm=T)
hist(dat4$days2_baby_meds)
range(dat4$days2_baby_wellvisit, na.rm=T)
hist(dat4$days2_baby_wellvisit)

names(dat4)
dim(dat4)  # 276021     41
length(unique(dat4$part_id))  # 16684

# limit to only participants with ANY wellness visits
dat5=dat4 %>%
  group_by(part_id) %>%
  filter(any(redcap_repeat_instrument=="baby_wellvisit"))

dim(dat5) # 197901     41
length(unique(dat5$part_id))  # 7667

# 2week dummy
dat6=dat5 %>%
  group_by(part_id) %>%
  mutate(two_wk_dummy=ifelse(days2_baby_wellvisit<30,TRUE,FALSE),
         one_year_dummy=ifelse(days2_baby_wellvisit<365 & days2_baby_wellvisit>=30,TRUE,FALSE),
         two_year_dummy=ifelse(days2_baby_wellvisit<730 & days2_baby_wellvisit>=365,TRUE,FALSE),
         three_year_dummy=ifelse(days2_baby_wellvisit<1095 & days2_baby_wellvisit>=730,TRUE,FALSE),
         three_year_plus=ifelse(days2_baby_wellvisit>=1095,TRUE,FALSE))

# **************************************************************************** #
# *****      Export data                                              
# **************************************************************************** # 
now=Sys.Date(); today=format(now, format="%d%b%y")
save(dat6, file=paste0(out.dir,"abx_mod_",today,".rdata"))

