##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        April 04, 2018 
# IRB:
# Description: Analysis of abx and mode-of-delivery EHR data 
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
library(broom)
library(tidyverse)

# **************************************************************************** #
# *****      l             load data: abx_mod_04Apr18.rdata       
# **************************************************************************** # 

load(file="abx_mod_05Apr18.rdata")
head(dat2)
names(dat2)

# check
range(dat2$gest_age_wk, na.rm=T)
range(dat2$baby_birth_wt_gr, na.rm=T) 
table(is.na(dat2$baby_dob))

# **************************************************************************** #
# *****      subset the data: gest_age (wk) >37 & <42, birth_wt (gr) >500                                              
# **************************************************************************** # 

dat3=dat2 %>%
  filter(gest_age_wk>=37, gest_age_wk<=42, baby_birth_wt_gr>500, is.na(baby_dob)==F, is.na(mod)==F)   

# check
range(dat3$gest_age_wk)
range(dat3$baby_birth_wt_gr) 
table(is.na(dat3$baby_dob))
table(dat3$mod)

# how many participants in analysis?
dat3 %>%
  group_by(mod) %>%
  summarize(count=n_distinct(part_id),
            abx_mean=mean(abx_episode),
            abx_sd=sd(abx_episode)) 

# **************************************************************************** #
# *****      Summarize the data set: wellness_cats first then wellness.visit second                                              
# **************************************************************************** # 

# create a tibble
dat2_df=tbl_df(dat3)
dat2_df

# Mean abx_episodes for entire cohort 
summarize(dat2_df, abx_mean_epi=mean(abx_episode, na.rm=T)) # 1.90

# Compute variable with highest rank abx_episodes for each participant
# within each wellness visits category
dat3_df=dat2_df %>%
  group_by(part_id, wellness.visit_cats) %>%
  mutate(abx_max=last(abx_episode)) %>%
  select(part_id,wellness.visit, wellness.visit_cats,mod,baby_birth_wt_gr,gest_wk,days2_baby_meds,abx_episode,abx_max)

# WELLNESS CATS
#-------------
#  Descriptive look abx_episode according to wellness-cats and mod
wellness.abx=dat3_df %>%
  group_by(wellness.visit_cats, mod) %>% 
  summarise(count = n(),                                   # total count
            count.unique = n_distinct(part_id),            # count of unique participants
            abx_epi_mean=mean(abx_episode, na.rm=T),       # mean abx_episode
            abx_epi_sd=sd(abx_episode, na.rm=T),           # sd abx_episode
            abx_max_mean=mean(abx_max, na.rm=T),           # mean abx_episode_max
            abx_max_sd=sd(abx_max, na.rm=T));wellness.abx  # sd abx_episode_max

# WELLNESS LOTS OF CATS
#----------------------
# Descriptive look abx_episode according to wellness and mod
wellness.abx2=dat3_df %>%
  group_by(wellness.visit, mod) %>%
  summarise(count = n(),                                    # total count
            count.unique = n_distinct(part_id),             # count of unique participants
            abx_epi_mean=mean(abx_episode, na.rm=T),        # mean abx_episode
            abx_epi_sd=sd(abx_episode, na.rm=T),            # sd abx_episode
            abx_max_mean=mean(abx_max, na.rm=T),            # mean abx_episode_max
            abx_max_sd=sd(abx_max, na.rm=T));wellness.abx2  # sd abx_episode_max

# **************************************************************************** #
# *****      Analysis of MOD and abx_episode/ days2_abx                                              
# **************************************************************************** # 

# need to add sd and output as proper table

# WELLNESS CATS
#-------------
#  ALL TIME POINTS
# t-test: abx_episode and days2_baby_med ~ mod
out.all=dat3_df %>% ungroup() %>%
  select(days2_baby_meds, abx_episode, mod) %>%
  gather(key = variable, value = value, -mod) %>%
  group_by(mod, variable) %>% 
  summarise(value = list(value)) %>% 
  spread(mod, value) %>% 
  group_by(variable) %>% 
  mutate(p_value = round(t.test(unlist(`c-section`), unlist(vaginal))$p.value,4),
         t_value = round(t.test(unlist(`c-section`), unlist(vaginal))$statistic,2),
         mean_csec = round(t.test(unlist(`c-section`), unlist(vaginal))$estimate[1],2),
         mean_vag = round(t.test(unlist(`c-section`), unlist(vaginal))$estimate[2],2),
         time_point="ALL");out.all

#  TIME POINT: 0-6 months 
# t-test: abx_episode and days2_baby_med ~ mod
out.6mo=dat3_df %>% ungroup() %>%
  select(days2_baby_meds, abx_episode, mod, wellness.visit_cats) %>%
  filter(wellness.visit_cats=="t.<6mo") %>% 
  gather(key = variable, value = value, -mod, -wellness.visit_cats) %>%
  group_by(mod, variable) %>% 
  summarise(value = list(value)) %>% 
  spread(mod, value) %>% 
  group_by(variable) %>% 
  mutate(p_value = round(t.test(unlist(`c-section`), unlist(vaginal))$p.value,4),
         t_value = round(t.test(unlist(`c-section`), unlist(vaginal))$statistic,2),
         mean_csec = round(t.test(unlist(`c-section`), unlist(vaginal))$estimate[1],2),
         mean_vag = round(t.test(unlist(`c-section`), unlist(vaginal))$estimate[2],2),
         time_point="t.<6mo");out.6mo

#  TIME POINT: t.6_12months 
# t-test: abx_episode and days2_baby_med ~ mod
out.6_12mo=dat3_df %>% ungroup() %>%
  select(days2_baby_meds, abx_episode, mod, wellness.visit_cats) %>%
  filter(wellness.visit_cats=="t.6_12mo") %>% 
  gather(key = variable, value = value, -mod, -wellness.visit_cats) %>%
  group_by(mod, variable) %>% 
  summarise(value = list(value)) %>% 
  spread(mod, value) %>% 
  group_by(variable) %>% 
  mutate(p_value = round(t.test(unlist(`c-section`), unlist(vaginal))$p.value,4),
         t_value = round(t.test(unlist(`c-section`), unlist(vaginal))$statistic,2),
         mean_csec = round(t.test(unlist(`c-section`), unlist(vaginal))$estimate[1],2),
         mean_vag = round(t.test(unlist(`c-section`), unlist(vaginal))$estimate[2],2),
         time_point="t.6_12mo");out.6_12mo

#  TIME POINT: t.12_24months 
# t-test: abx_episode and days2_baby_med ~ mod
out.12_24mo=dat3_df %>% ungroup() %>%
  select(days2_baby_meds, abx_episode, mod, wellness.visit_cats) %>%
  filter(wellness.visit_cats=="t.12_24mo") %>% 
  gather(key = variable, value = value, -mod, -wellness.visit_cats) %>%
  group_by(mod, variable) %>% 
  summarise(value = list(value)) %>% 
  spread(mod, value) %>% 
  group_by(variable) %>% 
  mutate(p_value = round(t.test(unlist(`c-section`), unlist(vaginal))$p.value,4),
         t_value = round(t.test(unlist(`c-section`), unlist(vaginal))$statistic,2),
         mean_csec = round(t.test(unlist(`c-section`), unlist(vaginal))$estimate[1],2),
         mean_vag = round(t.test(unlist(`c-section`), unlist(vaginal))$estimate[2],2),
         time_point="t.12_24mo");out.12_24mo

#  TIME POINT: t.24_36months 
# t-test: abx_episode and days2_baby_med ~ mod
out.24_36mo=dat3_df %>% ungroup() %>%
  select(days2_baby_meds, abx_episode, mod, wellness.visit_cats) %>%
  filter(wellness.visit_cats=="t.24_36mo") %>% 
  gather(key = variable, value = value, -mod, -wellness.visit_cats) %>%
  group_by(mod, variable) %>% 
  summarise(value = list(value)) %>% 
  spread(mod, value) %>% 
  group_by(variable) %>% 
  mutate(p_value = round(t.test(unlist(`c-section`), unlist(vaginal))$p.value,4),
         t_value = round(t.test(unlist(`c-section`), unlist(vaginal))$statistic,2),
         mean_csec = round(t.test(unlist(`c-section`), unlist(vaginal))$estimate[1],2),
         mean_vag = round(t.test(unlist(`c-section`), unlist(vaginal))$estimate[2],2),
         time_point="t.24_36mo");out.24_36mo

#  TIME POINT: t.>36months 
# t-test: abx_episode and days2_baby_med ~ mod
out.36mo=dat3_df %>% ungroup() %>%
  select(days2_baby_meds, abx_episode, mod, wellness.visit_cats) %>%
  filter(wellness.visit_cats=="t.>36mo") %>% 
  gather(key = variable, value = value, -mod, -wellness.visit_cats) %>%
  group_by(mod, variable) %>% 
  summarise(value = list(value)) %>% 
  spread(mod, value) %>% 
  group_by(variable) %>% 
  mutate(p_value = round(t.test(unlist(`c-section`), unlist(vaginal))$p.value,4),
         t_value = round(t.test(unlist(`c-section`), unlist(vaginal))$statistic,2),
         mean_csec = round(t.test(unlist(`c-section`), unlist(vaginal))$estimate[1],2),
         mean_vag = round(t.test(unlist(`c-section`), unlist(vaginal))$estimate[2],2),
         time_point="t.>36mo");out.36mo

# combine results into single data.frame
results_well_cats=bind_rows(out.6mo,out.6_12mo,out.12_24mo,out.24_36mo,out.36mo);results_well_cats
results_well_cats.final=results_well_cats %>%
  select(variable,p_value,t_value,mean_csec,mean_vag,time_point)
now=Sys.Date(); today=format(now, format="%d%b%y")
write.csv(results_well_cats.final, file=paste0(out.dir,"MOD_ABX_wellcats_ttest_",today,".csv"), row.names=F)


# WELLNESS LOTS OF CATS
#----------------------

#  TIME POINT: t.3_days (no variation) : DONE
# t-test: abx_episode and days2_baby_med ~ mod
# out.3day=dat3_df %>% ungroup() %>%
#   select(days2_baby_meds, abx_episode, mod, wellness.visit) %>%
#   filter(!abx_episode=="NA", !days2_baby_meds=="NA", !mod=="NA", wellness.visit=="t.3_days") %>% 
#   gather(key = variable, value = value, -mod, -wellness.visit) %>%
#   group_by(mod, variable) %>% 
#   summarise(value = list(value)) %>% 
#   spread(mod, value) %>% 
#   group_by(variable) %>% 
#   mutate(p_value = t.test(unlist(`c-section`), unlist(vaginal))$p.value,
#          t_value = t.test(unlist(`c-section`), unlist(vaginal))$statistic,
#          time_point="t.3days")

#  TIME POINT: t.2_wks (sig.dif) : DONE
# t-test: abx_episode and days2_baby_med ~ mod
out.2wk=dat3_df %>% ungroup() %>%
  select(days2_baby_meds, abx_episode, mod, wellness.visit) %>%
  filter(!abx_episode=="NA", !days2_baby_meds=="NA", !mod=="NA", wellness.visit=="t.2_wks") %>% 
  gather(key = variable, value = value, -mod, -wellness.visit) %>%
  group_by(mod, variable) %>% 
  summarise(value = list(value)) %>% 
  spread(mod, value) %>% 
  group_by(variable) %>% 
  mutate(p_value = t.test(unlist(`c-section`), unlist(vaginal))$p.value,
         t_value = t.test(unlist(`c-section`), unlist(vaginal))$statistic,
         time_point="t.2_wks")

#  TIME POINT: t.1_mo (sig.dif) # DONE
# t-test: abx_episode and days2_baby_med ~ mod
out.1mo=dat3_df %>% ungroup() %>%
  select(days2_baby_meds, abx_episode, mod, wellness.visit) %>%
  filter(!abx_episode=="NA", !days2_baby_meds=="NA", !mod=="NA", wellness.visit=="t.1_mo") %>% 
  gather(key = variable, value = value, -mod, -wellness.visit) %>%
  group_by(mod, variable) %>% 
  summarise(value = list(value)) %>% 
  spread(mod, value) %>% 
  group_by(variable) %>% 
  mutate(p_value = t.test(unlist(`c-section`), unlist(vaginal))$p.value,
         t_value = t.test(unlist(`c-section`), unlist(vaginal))$statistic,
         time_point="t.1_mo")

#  TIME POINT: t.2_mo (sig.dif) # DONE
# t-test: abx_episode and days2_baby_med ~ mod
out.2mo=dat3_df %>% ungroup() %>%
  select(days2_baby_meds, abx_episode, mod, wellness.visit) %>%
  filter(!abx_episode=="NA", !days2_baby_meds=="NA", !mod=="NA", wellness.visit=="t.2_mo") %>% 
  gather(key = variable, value = value, -mod, -wellness.visit) %>%
  group_by(mod, variable) %>% 
  summarise(value = list(value)) %>% 
  spread(mod, value) %>% 
  group_by(variable) %>% 
  mutate(p_value = t.test(unlist(`c-section`), unlist(vaginal))$p.value,
         t_value = t.test(unlist(`c-section`), unlist(vaginal))$statistic,
         time_point="t.2_mo")

#  TIME POINT: t.4_mo (sig.dif) # DONE
# t-test: abx_episode and days2_baby_med ~ mod
out.4mo=dat3_df %>% ungroup() %>%
  select(days2_baby_meds, abx_episode, mod, wellness.visit) %>%
  filter(!abx_episode=="NA", !days2_baby_meds=="NA", !mod=="NA", wellness.visit=="t.4_mo") %>% 
  gather(key = variable, value = value, -mod, -wellness.visit) %>%
  group_by(mod, variable) %>% 
  summarise(value = list(value)) %>% 
  spread(mod, value) %>% 
  group_by(variable) %>% 
  mutate(p_value = t.test(unlist(`c-section`), unlist(vaginal))$p.value,
         t_value = t.test(unlist(`c-section`), unlist(vaginal))$statistic,
         time_point="t.4_mo")

#  TIME POINT: t.6_mo (sig.dif) # DONE
# t-test: abx_episode and days2_baby_med ~ mod
out.6mo=dat3_df %>% ungroup() %>%
  select(days2_baby_meds, abx_episode, mod, wellness.visit) %>%
  filter(!abx_episode=="NA", !days2_baby_meds=="NA", !mod=="NA", wellness.visit=="t.6_mo") %>% 
  gather(key = variable, value = value, -mod, -wellness.visit) %>%
  group_by(mod, variable) %>% 
  summarise(value = list(value)) %>% 
  spread(mod, value) %>% 
  group_by(variable) %>% 
  mutate(p_value = t.test(unlist(`c-section`), unlist(vaginal))$p.value,
         t_value = t.test(unlist(`c-section`), unlist(vaginal))$statistic,
         time_point="t.6_mo")

#  TIME POINT: t.9_mo (sig.dif) # DONE
# t-test: abx_episode and days2_baby_med ~ mod
out.9mo=dat3_df %>% ungroup() %>%
  select(days2_baby_meds, abx_episode, mod, wellness.visit) %>%
  filter(!abx_episode=="NA", !days2_baby_meds=="NA", !mod=="NA", wellness.visit=="t.9_mo") %>% 
  gather(key = variable, value = value, -mod, -wellness.visit) %>%
  group_by(mod, variable) %>% 
  summarise(value = list(value)) %>% 
  spread(mod, value) %>% 
  group_by(variable) %>% 
  mutate(p_value = t.test(unlist(`c-section`), unlist(vaginal))$p.value,
         t_value = t.test(unlist(`c-section`), unlist(vaginal))$statistic,
         time_point="t.9_mo")

#  TIME POINT: t.12_mo (sig.dif) # DONE
# t-test: abx_episode and days2_baby_med ~ mod
out.12mo=dat3_df %>% ungroup() %>%
  select(days2_baby_meds, abx_episode, mod, wellness.visit) %>%
  filter(!abx_episode=="NA", !days2_baby_meds=="NA", !mod=="NA", wellness.visit=="t.12_mo") %>% 
  gather(key = variable, value = value, -mod, -wellness.visit) %>%
  group_by(mod, variable) %>% 
  summarise(value = list(value)) %>% 
  spread(mod, value) %>% 
  group_by(variable) %>% 
  mutate(p_value = t.test(unlist(`c-section`), unlist(vaginal))$p.value,
         t_value = t.test(unlist(`c-section`), unlist(vaginal))$statistic,
         time_point="t.12_mo")

#  TIME POINT: t.15_mo (sig.dif) # DONE
# t-test: abx_episode and days2_baby_med ~ mod
out.15mo=dat3_df %>% ungroup() %>%
  select(days2_baby_meds, abx_episode, mod, wellness.visit) %>%
  filter(!abx_episode=="NA", !days2_baby_meds=="NA", !mod=="NA", wellness.visit=="t.15_mo") %>% 
  gather(key = variable, value = value, -mod, -wellness.visit) %>%
  group_by(mod, variable) %>% 
  summarise(value = list(value)) %>% 
  spread(mod, value) %>% 
  group_by(variable) %>% 
  mutate(p_value = t.test(unlist(`c-section`), unlist(vaginal))$p.value,
         t_value = t.test(unlist(`c-section`), unlist(vaginal))$statistic,
         time_point="t.15_mo")

#  TIME POINT: t.18_mo (sig.dif) # DONE
# t-test: abx_episode and days2_baby_med ~ mod
out.18mo=dat3_df %>% ungroup() %>%
  select(days2_baby_meds, abx_episode, mod, wellness.visit) %>%
  filter(!abx_episode=="NA", !days2_baby_meds=="NA", !mod=="NA", wellness.visit=="t.18_mo") %>% 
  gather(key = variable, value = value, -mod, -wellness.visit) %>%
  group_by(mod, variable) %>% 
  summarise(value = list(value)) %>% 
  spread(mod, value) %>% 
  group_by(variable) %>% 
  mutate(p_value = t.test(unlist(`c-section`), unlist(vaginal))$p.value,
         t_value = t.test(unlist(`c-section`), unlist(vaginal))$statistic,
         time_point="t.18_mo")

#  TIME POINT: t.2_yr (sig.dif) # DONE
# t-test: abx_episode and days2_baby_med ~ mod
out.2yr=dat3_df %>% ungroup() %>%
  select(days2_baby_meds, abx_episode, mod, wellness.visit) %>%
  filter(!abx_episode=="NA", !days2_baby_meds=="NA", !mod=="NA", wellness.visit=="t.2_yr") %>% 
  gather(key = variable, value = value, -mod, -wellness.visit) %>%
  group_by(mod, variable) %>% 
  summarise(value = list(value)) %>% 
  spread(mod, value) %>% 
  group_by(variable) %>% 
  mutate(p_value = t.test(unlist(`c-section`), unlist(vaginal))$p.value,
         t_value = t.test(unlist(`c-section`), unlist(vaginal))$statistic,
         time_point="t.2_yr")

#  TIME POINT: t.2.5_yr (sig.dif) # DONE
# t-test: abx_episode and days2_baby_med ~ mod
out.2_5yr=dat3_df %>% ungroup() %>%
  select(days2_baby_meds, abx_episode, mod, wellness.visit) %>%
  filter(!abx_episode=="NA", !days2_baby_meds=="NA", !mod=="NA", wellness.visit=="t.2.5_yr") %>% 
  gather(key = variable, value = value, -mod, -wellness.visit) %>%
  group_by(mod, variable) %>% 
  summarise(value = list(value)) %>% 
  spread(mod, value) %>% 
  group_by(variable) %>% 
  mutate(p_value = t.test(unlist(`c-section`), unlist(vaginal))$p.value,
         t_value = t.test(unlist(`c-section`), unlist(vaginal))$statistic,
         time_point="t.2.5_yr")

#  TIME POINT: t.3_yr (sig.dif) # DONE
# t-test: abx_episode and days2_baby_med ~ mod
out.3yr=dat3_df %>% ungroup() %>%
  select(days2_baby_meds, abx_episode, mod, wellness.visit) %>%
  filter(!abx_episode=="NA", !days2_baby_meds=="NA", !mod=="NA", wellness.visit=="t.3_yr") %>% 
  gather(key = variable, value = value, -mod, -wellness.visit) %>%
  group_by(mod, variable) %>% 
  summarise(value = list(value)) %>% 
  spread(mod, value) %>% 
  group_by(variable) %>% 
  mutate(p_value = t.test(unlist(`c-section`), unlist(vaginal))$p.value,
         t_value = t.test(unlist(`c-section`), unlist(vaginal))$statistic,
         time_point="t.3_yr")

#  TIME POINT: t.4_yr (sig.dif) # DONE
# t-test: abx_episode and days2_baby_med ~ mod
out.4yr=dat3_df %>% ungroup() %>%
  select(days2_baby_meds, abx_episode, mod, wellness.visit) %>%
  filter(!abx_episode=="NA", !days2_baby_meds=="NA", !mod=="NA", wellness.visit=="t.4_yr") %>% 
  gather(key = variable, value = value, -mod, -wellness.visit) %>%
  group_by(mod, variable) %>% 
  summarise(value = list(value)) %>% 
  spread(mod, value) %>% 
  group_by(variable) %>% 
  mutate(p_value = t.test(unlist(`c-section`), unlist(vaginal))$p.value,
         t_value = t.test(unlist(`c-section`), unlist(vaginal))$statistic,
         time_point="t.4_yr")

#  TIME POINT: t.5_yr (sig.dif) # DONE
# t-test: abx_episode and days2_baby_med ~ mod
out.5yr=dat3_df %>% ungroup() %>%
  select(days2_baby_meds, abx_episode, mod, wellness.visit) %>%
  filter(!abx_episode=="NA", !days2_baby_meds=="NA", !mod=="NA", wellness.visit=="t.5_yr") %>% 
  gather(key = variable, value = value, -mod, -wellness.visit) %>%
  group_by(mod, variable) %>% 
  summarise(value = list(value)) %>% 
  spread(mod, value) %>% 
  group_by(variable) %>% 
  mutate(p_value = t.test(unlist(`c-section`), unlist(vaginal))$p.value,
         t_value = t.test(unlist(`c-section`), unlist(vaginal))$statistic,
         time_point="t.5_yr")

# combine results into single data.frame
results=bind_rows(out.all,out.2wk,out.1mo,out.2mo,out.4mo,out.6mo,out.9mo,
                  out.12mo,out.15mo,out.18mo,out.2yr,out.2_5yr,out.3yr,
                  out.4yr,out.5yr);results
