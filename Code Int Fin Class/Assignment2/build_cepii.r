library(tidyverse)
library(readxl)

# Build Koijen and Yogo (2020)

# raw data path
data_root = "/Users/thupham/Documents/Columbia/IntFinance/Data Int Fin Class/"
data_path = paste0(data_root,"rawdata/CEPII_Gravity/") 

# create build directory
build_data_path = paste0(data_root,"build/") 
dir.create(build_data_path)

#################################################
#           Import Raw Data
#################################################
fname <- "Gravity_V202102.csv"
raw_data = read_csv(paste0(data_path,fname))

#################################################
#                   Clean
#################################################
clean_data <- raw_data %>% 
             select(year, investor=iso3_o,issuer=iso3_d,dist=distw,comlang=comlang_off,gdp=gdp_d,gdp_cap=gdpcap_d) %>%
             filter(year>=2002) %>%
             mutate(dist=dist/1000,
                    gdp = log(gdp),
                    gdp_cap = log(gdp_cap))

# set distance between same country = 0, comlang = 1
#########################################
#           Write dataset
#########################################
clean_data %>% write_rds(file=paste0(build_data_path,"cepii.rds"))
