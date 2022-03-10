library(tidyverse)
library(haven)

# Build Koijen and Yogo (2020)

# raw data path
data_root = "/Users/thupham/Documents/Columbia/IntFinance/Data Int Fin Class/"
data_path = paste0(data_root,"rawdata/factset_fx/") 

# create build directory
build_data_path = paste0(data_root,"build/") 
dir.create(build_data_path)

#################################################
#           Import Raw Data
#################################################
fname <- "factset_fx_quarter.dta"
raw_data = read_dta(paste0(data_path,fname))

map_name<- "country_currency.dta"
country_map <- read_dta(paste0(data_root,"rawdata/",map_name))
#################################################
#                   Clean
#################################################
clean_data <- raw_data %>% 
  # exchange rate in USD per foreign currency
  mutate(fx = 1/fx_per_usd) %>%
  # convert stata quarter to year/quarter, stata date starts 01jan1960
  mutate(year = 1960+floor(quarter/4),
         quarter = quarter%%4+1) %>%
  filter(year >= 2002,
         quarter==4) %>%
  select(year, fx,iso_currency_code=iso_currency)

# Merge with country codes
clean_data_m <- clean_data %>%
                left_join(country_map) %>%
                rename(currency=iso_currency_code,
                       issuer = iso_country_code)

#########################################
#           Write dataset
#########################################
clean_data_m %>% write_rds(file=paste0(build_data_path,"factset_fx.rds"))
