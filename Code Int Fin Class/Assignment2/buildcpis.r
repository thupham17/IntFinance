library(tidyverse)
library(readxl)
library(countrycode)

# Build CPIS data

# raw data path
data_root = "/Users/thupham/Documents/Columbia/IntFinance/Data Int Fin Class/"
data_path = paste0(data_root,"rawdata/IMF/") 

# create build directory
build_data_path = paste0(data_root,"build/") 
dir.create(build_data_path)

#################################################
#           Import Raw Data
#################################################
fname <- "CPIS_11-08-2021 08-43-03-67_timeSeries.csv"
raw_data = read_csv(paste0(data_path,fname))

#################################################
#                   Clean
#################################################

# Check data/summary
raw_data %>% group_by(`Indicator Code`,`Indicator Name`) %>% summarize(count =n()) %>% View()
iso_imf_temp <- bind_rows(raw_data %>% distinct(`Country Name`,`Country Code`) %>% select('imf_code'=`Country Code`,country=`Country Name`),
                         raw_data %>% distinct(`Counterpart Country Name`,`Counterpart Country Code`) %>% select('imf_code'=`Counterpart Country Code`,country=`Counterpart Country Name`)) %>%
              distinct() %>%
              mutate(iso3_code_using_imf = countrycode(imf_code,origin="imf",destination="iso3c")) %>%
              mutate(iso3_code_using_name = countrycode(country,origin="country.name",destination="iso3c",custom_match = c('Kosovo, Rep. of'='KOS','Curaçao and Sint Maarten'='ANT','Netherlands Antilles'='ANT','Curaçao, Kingdom of the Netherlands'='CUW','Sint Maarten, Kingdom of the Netherlands'='SXM','Aruba, Kingdom of the Netherlands'='ABW'))) %>%
              mutate(iso3_code = ifelse(!is.na(iso3_code_using_name),iso3_code_using_name,iso3_code_using_imf))

# Check missing
iso_imf_temp %>% filter(is.na(iso3_code))

iso_imf_map <- iso_imf_temp %>% select(imf_code,iso3_code)
  
clean_data <- raw_data %>% 
              janitor::clean_names() %>%
              filter(attribute=="Value") %>%
              left_join(iso_imf_map %>% select(country_code=imf_code,country_iso3c=iso3_code)) %>%
              left_join(iso_imf_map %>% select(counterpart_country_code=imf_code,counterpart_iso3c=iso3_code)) %>%
              select(-attribute) %>%
              # Pivot Longer
              mutate_at(vars(starts_with("x")),as.numeric) %>%
              pivot_longer(cols=starts_with("x"),names_to="year",values_to="holding") %>%
              filter(!is.na(holding)) %>%
              mutate(year=as.numeric(str_replace(year,"x",""))) %>%
              # select variables
               select(contains("iso3c"),contains("code"),year,holding) %>%
               rename_at(vars(contains("code")),~str_replace(.,"_code",""))

#########################################
#           Write dataset
#########################################
clean_data %>% write_rds(file=paste0(build_data_path,"cpis.rds")) 

# map/helper files
iso_imf_temp %>% write_rds(file=paste0(build_data_path,"imf_iso_map.rds")) 
raw_data %>% distinct(`Indicator Code`,`Indicator Name`,`Sector Name`,`Sector Code`) %>% 
              janitor::clean_names() %>%
              write_rds(file=paste0(build_data_path,"cpis_asset_sector_names.rds")) 