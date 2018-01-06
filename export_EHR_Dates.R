# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        Sept 14 2017
# IRB:
# Description: export infant "date to occurance" files for import to RedCap

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

rm(list=ls())
graphics.off()

# Computer
# location="djlemas";location
# location="Dominick";location

# Directory Locations
work.dir=paste("C:\\Users\\",location,"\\Dropbox\\IRB\\UF\\UFHealth\\redcap_import\\01_import_22July17\\EXPORT\\",sep="");work.dir
data.dir=work.dir

setwd(work.dir)
list.files()

# **************************************************************************** #
# ***************                # LOAD DATA                                              
# **************************************************************************** #      

# file parameters
data.file.name="baby.dates_08Sept17.csv";data.file.name

# load data 1
baby.data <- read.csv(paste(data.dir,data.file.name,sep=""))
head(baby.data)
str(baby.data);dim(baby.data)
names(baby.data)

# **************************************************************************** #
# ***************                # FORMAT VARIABLE(S)                                             
# **************************************************************************** # 

# instruments
instrument=as.character(unique(baby.data$redcap_repeat_instrument)); instrument
inst.key=c("babyantibiotics_ip",
           "babyantibiotics_perscriptions",
           "babybaby", "babyclinic_asthma", 
           "babyclinic_dermatitis",
           "babyclinic_ear", 
           "babyclinic_eczema", 
           "babyclinic_erythema", 
           "babyclinic_foodallergy",  
           "babyclinic_hemangioma",
           "babyclinic_sebaceous", 
           "babyfirst_head_circumference", 
           "babyfirst_height",
           "babyhospital_asthma",
           "babyhospital_dermatitis", 
           "babyhospital_ear", 
           "babyhospital_eczema", 
           "babyhospital_erythema", 
           "babyhospital_foodallergy", 
           "babyhospital_hemangioma", 
           "babyhospital_obesity", 
           "babyhospital_sebaceous",
           "babyvaccines",
           "babywellvisit");inst.key;length(inst.key)        

# date variables
date=names(baby.data);
date.value=c("days2_meds_ip",          # done
             "days2_meds",             # done 
             "days2_admit",            # done
             "days2_asthma_clinic",    # done
             "days2_dermatitis_clinic", # done
             "days2_ear_clinic",       # done
             "days2_eczema_clinic",    # done
             "days2_erythema_clinic",  # done
             "days2_fa_clinic",        # done
             "days2_hemangioma_clinic", # done
             "days2_sebaceous_clinic", # done
             "days2_hc1",              # done
             "days2_ht1",              # done
             "days2_asthma_hosp",      # done
             "days2_dermatitis_hosp",  # done
             "days2_ear_hosp",         # done
             "days2_eczema_hosp",      # done
             "days2_erythema_hosp",    # done
             "days2_fa_hosp",          # done
             "days2_hemangioma_hosp",  # done
             "days2_obesity_hosp",     # done
             "days2_sebaceous_hosp",   # done
             "days2_vaccine",          # done
             "days2_wellvisit")        # done
date.value;length(date.value)
             
# create key-value set
temp1 <- setNames(as.list(date.value), inst.key);temp1
                
# create a file for each key-value
for (j in 1:length(temp1)){ # first loop

  out=baby.data[baby.data$redcap_repeat_instrument==names(temp1)[j],
              c("baby_id","redcap_repeat_instrument","redcap_repeat_instance","redcap_event_name",temp1[[j]])]

  # write file
  batchSize=10000; # number of rows in single output file
  data.file.name.export=names(temp1)[j];data.file.name.export
  out.dir=paste("C:\\Users\\",location,"\\Dropbox\\IRB\\UF\\UFHealth\\redcap_import\\01_import_22July17\\EXPORT\\Dates\\",sep="");work.dir


    chunks=split(out, floor(0:(nrow(out)-1)/batchSize))
    for (i in 1:length(chunks)){ # second loop
    write.table(chunks[[i]],paste0(out.dir,data.file.name.export,i,'.csv'),row.names=F, sep="\t")
    } # end second loop
} # end first loop

# setwd(out.dir)
# list.files()

# [1] "babyantibiotics_ip1.csv"            "babyantibiotics_ip2.csv"            "babyantibiotics_ip3.csv"           
# [4] "babyantibiotics_ip4.csv"            "babyantibiotics_ip5.csv"            "babyantibiotics_ip6.csv"           
# [7] "babyantibiotics_ip7.csv"            "babyantibiotics_ip8.csv"            "babyantibiotics_perscriptions1.csv"
# [10] "babybaby1.csv"                      "babybaby2.csv"                      "babyclinic_asthma1.csv"            
# [13] "babyclinic_asthma2.csv"             "babyclinic_dermatitis1.csv"         "babyclinic_ear1.csv"               
# [16] "babyclinic_eczema1.csv"             "babyclinic_erythema1.csv"           "babyclinic_foodallergy1.csv"       
# [19] "babyclinic_hemangioma1.csv"         "babyclinic_sebaceous1.csv"          "babyfirst_head_circumference1.csv" 
# [22] "babyfirst_head_circumference2.csv"  "babyfirst_height1.csv"              "babyfirst_height2.csv"             
# [25] "babyhospital_asthma1.csv"           "babyhospital_dermatitis1.csv"       "babyhospital_ear1.csv"             
# [28] "babyhospital_eczema1.csv"           "babyhospital_erythema1.csv"         "babyhospital_foodallergy1.csv"     
# [31] "babyhospital_hemangioma1.csv"       "babyhospital_obesity1.csv"          "babyhospital_sebaceous1.csv"       
# [34] "babyvaccines1.csv"                  "babyvaccines10.csv"                 "babyvaccines11.csv"                
# [37] "babyvaccines2.csv"                  "babyvaccines3.csv"                  "babyvaccines4.csv"                 
# [40] "babyvaccines5.csv"                  "babyvaccines6.csv"                  "babyvaccines7.csv"                 
# [43] "babyvaccines8.csv"                  "babyvaccines9.csv"                  "babywellvisit1.csv"                
# [46] "babywellvisit2.csv"                 "babywellvisit3.csv"                 "babywellvisit4.csv"                
# [49] "babywellvisit5.csv"                 "babywellvisit6.csv" 
