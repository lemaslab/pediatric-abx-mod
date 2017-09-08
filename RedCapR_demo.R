# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        Sept 09 2017
# IRB:
# Description: run the RedCapR package to export and import RedCap data

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

rm(list=ls())
graphics.off()

# Notes
# https://cran.r-project.org/web/packages/REDCapR/README.html
# https://github.com/OuhscBbmc/REDCapR

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(REDCapR)

# Set project-wide values.

uri <- "https://bbmc.ouhsc.edu/redcap/api/"
token <- "9A81268476645C4E5F03428B8AC3AA7B" #`UnitTestPhiFree` user and simple project (pid 153)

# Read all records and fields.

#Return all records and all variables.
ds_all_rows_all_fields <- redcap_read(redcap_uri=uri, token=token)$data
ds_all_rows_all_fields #Inspect the returned dataset
str(ds_all_rows_all_fields)

# Read a subset of the records.
#Return only records with IDs of 1 and 3
desired_records_v1 <- c(1, 3)
ds_some_rows_v1 <- redcap_read(
  redcap_uri = uri, 
  token      = token, 
  records    = desired_records_v1
)$data
str(ds_some_rows_v1)

# Read a subset of the fields.
#Return only the fields record_id, name_first, and age
desired_fields_v1 <- c("record_id", "name_first", "age")
ds_some_fields_v1 <- redcap_read(
  redcap_uri = uri, 
  token      = token, 
  fields     = desired_fields_v1
)$data

# Read a subset of records, conditioned on the values in some variables.
######
## Step 1: First call to REDCap
desired_fields_v3 <- c("record_id", "dob", "weight")
ds_some_fields_v3 <- redcap_read(
  redcap_uri = uri, 
  token      = token, 
  fields     = desired_fields_v3
)$data

######
## Step 2: identify desired records, based on age & weight
before_1960 <- (ds_some_fields_v3$dob <= as.Date("1960-01-01"))
heavier_than_70_kg <- (ds_some_fields_v3$weight > 70)
desired_records_v3 <- ds_some_fields_v3[before_1960 & heavier_than_70_kg, ]$record_id

desired_records_v3 #Peek at IDs of the identified records

######
## Step 3: second call to REDCap
#Return only records that met the age & weight criteria.
ds_some_rows_v3 <- redcap_read(
  redcap_uri = uri, 
  token      = token, 
  records    = desired_records_v3
)$data

