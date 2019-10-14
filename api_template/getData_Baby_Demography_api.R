##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #
           
# Author:      Dominick Lemas 
# Date:        October 14, 2019 
# IRB:
# Description: Export infant demographic data from redcap using API 
# Data: REDCAP::UFHealth Early Life Exposures and Pediatric Outcomes(IRB201601899)

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(keyringr)
library(redcapAPI)
library(REDCapR)
library(data.table)
library(tidyr)
library(dplyr)

# **************************************************************************** #
# ***************                API                      
# **************************************************************************** #

# Get Redcap API Token
# # https://cran.r-project.org/web/packages/keyringr/vignettes/Avoiding_plain_text_passwords_in_R_with_keyringr.html
credential_label <- "redcap_ehr"
credential_path <- paste(Sys.getenv("USERPROFILE"), '\\DPAPI\\passwords\\', Sys.info()["nodename"], '\\', credential_label, '.txt', sep="")
uri <- "https://redcap.ctsi.ufl.edu/redcap/api/"
redcap_ehr<-decrypt_dpapi_pw(credential_path)
print(redcap_ehr)

# **************************************************************************** #
# ***************                pull data
# **************************************************************************** #

# Create connections
rcon <- redcapConnection(url=uri, token=redcap_ehr)

# list of instruments
exportInstruments(rcon)

# list of events
exportEvents(rcon)

# list records
exportRecords(rcon)

# Pull data set 
#-------------

# variables of interest
desired_fields_v1=c("part_id", "baby_dob", "baby_gender")

# events of interest
#-------------------

# participants of interest
#------------------------

baby <- redcap_read(
  redcap_uri = uri, 
  token      = redcap_ehr, 
  fields     = desired_fields_v1
)$data

# explore the data
head(baby)
str(baby)

# format data
baby$baby_dob=as.Date(baby$baby_dob)
    
