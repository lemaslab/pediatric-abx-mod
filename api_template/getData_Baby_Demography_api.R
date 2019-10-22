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

# fancy informatics way of getting records of interest                                          
test=seq(1,10,1)
test2=paste0("Baby-000",test)


# variables of interest
desired_fields_v1=c("part_id", "baby_dob", "baby_gender")

# records of interest
desired_records_v1=c("Baby-0001", "Baby-0002", "Baby-0003",
                     "Baby-0004", "Baby-0005", "Baby-0006",
                     "Baby-0007", "Baby-0008", "Baby-0009",
                     "Baby-0010", "Baby-0011", "Baby-0012",
                     "Baby-0013", "Baby-0014", "Baby-0015",
                     "Baby-0016", "Baby-0017", "Baby-0018",
                     "Baby-0019", "Baby-0020", "Baby-0021",
                     "Baby-0022", "Baby-0023", "Baby-0024",
                     "Baby-0025", "Baby-0026", "Baby-0027",
                     "Baby-0028", "Baby-0029", "Baby-0030",
                     "Baby-0031", "Baby-0032", "Baby-0033",
                     "Baby-0034", "Baby-0035", "Baby-0036",
                     "Baby-0037", "Baby-0038", "Baby-0039",
                     "Baby-0040", "Baby-0041", "Baby-0042",
                     "Baby-0043", "Baby-0044", "Baby-0045",
                     "Baby-0046", "Baby-0047", "Baby-0048",
                     "Baby-0049", "Baby-0050", "Baby-0051",
                     "Baby-0052", "Baby-0053", "Baby-0054",
                     "Baby-0055", "Baby-0056", "Baby-0057",
                     "Baby-0058", "Baby-0059", "Baby-0060",
                     "Baby-0061", "Baby-0062", "Baby-0063",
                     "Baby-0064", "Baby-0065", "Baby-0066",
                     "Baby-0067", "Baby-0068", "Baby-0069",
                     "Baby-0070", "Baby-0071", "Baby-0072",
                     "Baby-0073", "Baby-0074", "Baby-0075",
                     "Baby-0076", "Baby-0077", "Baby-0078",
                     "Baby-0079", "Baby-0080", "Baby-0081",
                     "Baby-0082", "Baby-0083", "Baby-0084",
                     "Baby-0085", "Baby-0086", "Baby-0087",
                     "Baby-0088", "Baby-0089", "Baby-0090",
                     "Baby-0091", "Baby-0092", "Baby-0093",
                     "Baby-0094", "Baby-0095", "Baby-0096",
                     "Baby-0097", "Baby-0098", "Baby-0099",
                     "Baby-0100", "Baby-0101", "Baby-0102",
                     "Baby-0103", "Baby-0104", "Baby-0105",
                     "Baby-0106", "Baby-0107", "Baby-0108",
                     "Baby-0109", "Baby-0110", "Baby-0111",
                     "Baby-0112", "Baby-0113", "Baby-0114",
                     "Baby-0115", "Baby-0116", "Baby-0117",
                     "Baby-0118", "Baby-0119", "Baby-0120",
                     "Baby-0121", "Baby-0122", "Baby-0123",
                     "Baby-0124", "Baby-0125", "Baby-0126",
                     "Baby-0127", "Baby-0128", "Baby-0129",
                     "Baby-0130", "Baby-0131", "Baby-0132",
                     "Baby-0133", "Baby-0134", "Baby-0135",
                     "Baby-0136", "Baby-0137", "Baby-0138",
                     "Baby-0139", "Baby-0140", "Baby-0141",
                     "Baby-0142", "Baby-0143", "Baby-0144",
                     "Baby-0145", "Baby-0146", "Baby-0147",
                     "Baby-0148", "Baby-0149") 

# events of interest
#-------------------

# participants of interest
#------------------------

baby <- redcap_read(
  batch_size = 100L,
  redcap_uri = uri, 
  token      = redcap_ehr, 
  fields     = desired_fields_v1,
  records = desired_records_v1
)$data

# explore the data
head(baby)
str(baby)

# format data
baby$baby_dob=as.Date(baby$baby_dob)
    
