library(tidyverse)
library(readxl)
library(countrycode)
library(plm)

# Merge CPIS, CEPII and FX data

# Dataset path
data_root = "/Users/thupham/Documents/Columbia/IntFinance/Data Int Fin Class/"
build_data_path = paste0(data_root,"build/") 

#################################################
#           Import Datasets
#################################################
cepii = read_rds(paste0(build_data_path,"cepii.rds"))
cpis = read_rds(paste0(build_data_path,"cpis.rds"))
fx = read_rds(paste0(build_data_path,"factset_fx.rds"))

ky_countries <- read_csv(paste0(data_root,"KY_countries.csv"))

#################################################
#                   Merge
#################################################
ky_countries<- ky_countries %>% mutate(imf_code = countrycode(country, 
                                               origin="country.name",
                                                destination = "imf",
                                                custom_match = c('Guernsey' = "113",'Isle of Man'='118',"Jersey"="117")),
                                        iso3_code = countrycode(country, 
                                                              origin="country.name",
                                                              destination = "iso3c",
                                                              custom_match = c('Kosovo' = "KOS",'Netherlands Antilles'='ANT')
                                                              )
              )

                        
investor_countries <- ky_countries%>%filter(type=="Investor")%>%pull(iso3_code)
issuer_countries <-ky_countries%>%filter(type=="issuer")%>%pull(iso3_code)
# Filter CPIS
cpis_clean <- cpis %>% filter(year>=2002,
                        year<=2019,
                        indicator =="I_A_D_T_T_BP6_USD",
                        sector=="T",
                        counterpart_sector=="T") %>%
                        rename(investor=country_iso3c,issuer=counterpart_iso3c) %>%
                        mutate(investor=ifelse(investor %in% investor_countries,investor, "OTH"),
                               issuer=ifelse(issuer %in% issuer_countries,issuer, "OTH")) %>%
                        group_by(investor,issuer,year) %>%
                        summarize(cpi_holding=sum(holding,na.rm=TRUE))

data_merge <- cpis_clean %>%
              left_join(cepii) %>%
              left_join(fx)

#################################################
#                   Q1
#################################################
data_merge <- data_merge %>% 
              mutate(outside_holding = ifelse(issuer %in% issuer_countries,0,cpi_holding)) %>%
              group_by(investor,year) %>% 
              mutate(A_it = sum(cpi_holding,na.rm=TRUE), O_it = sum(outside_holding,na.rm=TRUE)) %>%
              ungroup() %>%
              mutate(w_it_0 = O_it/A_it,
                     w_it_n = cpi_holding/A_it,
                     ln_shares = log(w_it_0/w_it_n)) %>%
              filter(investor %in% investor_countries,
                     issuer %in% issuer_countries) 

# Check NaNs - negative CPI?
data_merge %>% filter(is.na(ln_shares)) %>% View()

data_reg <- data_merge %>% drop_na() %>% filter(!is.na(ln_shares)) %>% filter(ln_shares!=Inf) %>% mutate(investor_issuer = paste(investor,issuer,sep="_")) %>%
            select(investor_issuer,year,ln_shares,dist, comlang,gdp,gdp_cap) %>%
            mutate(investor_issuer=as_factor(investor_issuer),year=as_factor(year))

# Regress
est_model <- plm(ln_shares ~ dist+comlang+gdp+gdp_cap, data=data_reg, index=c("investor_issuer", "year"),effect = "twoways",model = "random")
coeftest(est_model, vcov. = vcovHC, type = "HC1")
summary(est_model)

data_reg<- data_reg %>% mutate(pred_demand= exp(as.numeric(est_model$model[[1]] - est_model$residuals)))

#################################################
#                   Q2
#################################################
data_out<- data_reg %>% separate(investor_issuer, c("investor", "issuer")) %>%
            group_by(investor,year) %>% 
            mutate(pred_total_demand = sum(pred_demand)) %>%
            ungroup() %>% 
              mutate(pred_demand_share = pred_demand/(1+pred_total_demand))  %>%
            mutate(year=as.numeric(as.character(year)))%>%
            left_join(data_merge %>% select(investor,issuer,year,w_it_n))

data_plot <- data_out %>% filter(investor=="USA",issuer =="DEU") 

ggplot(data_plot, aes(x=w_it_n, y=pred_demand_share)) +
  geom_point(size=2, shape=23)

#################################################
#                   Q3
#################################################

