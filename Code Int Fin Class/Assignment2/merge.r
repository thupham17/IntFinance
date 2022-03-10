library(tidyverse)
library(readxl)
library(countrycode)
library(fastDummies)

#library(plm)

# Merge CPIS, CEPII and FX data

# Dataset path
data_root = "/Users/thupham/Documents/Columbia/IntFinance/Data Int Fin Class/"
build_data_path = paste0(data_root,"build/") 
fig_data_path = paste0(data_root,"figures/") 

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
# Country types as defined in Koijen Yojo
ky_countries<- ky_countries %>% mutate(iso3_code = countrycode(country, 
                                                              origin="country.name",
                                                              destination = "iso3c",
                                                              custom_match = c('Kosovo' = "KOS",'Netherlands Antilles'='ANT')
                                                              )
              )

issuer_countries <-ky_countries%>%filter(type=="issuer")%>%pull(iso3_code)
#Note Curacao and Sint Maarten part of Netherland Antilles
investor_countries <- ky_countries%>%filter(type=="Investor")%>%pull(iso3_code)  %>% append(c("CUW","SXM"))
tax_haven <- ky_countries%>%filter(market=="tax haven")%>%pull(iso3_code) %>% append(c("CUW","SXM"))

# Filter CPIS
cpis_clean <- cpis %>% rename(investor=country_iso3c,issuer=counterpart_iso3c) %>%
                        filter(
                          year>=2002,
                          year<=2019,
                          indicator =="I_A_D_T_T_BP6_USD",
                          sector=="T",
                          counterpart_sector=="T",
                          # remove aggregate rows
                          issuer != "001"
                        ) %>%
                        mutate(investor=ifelse(investor %in% investor_countries,investor, "OTH"),
                               issuer=ifelse(issuer %in% issuer_countries,issuer, "OTH")) %>%
                        group_by(investor,issuer,year) %>%
                        summarize(cpi_holding=sum(holding,na.rm=TRUE)) %>%
                        ungroup()


# Merge with gravity variables and exchange rates
merge <- cpis_clean %>%
          left_join(cepii) %>%
          left_join(fx)

#################################################
#                   Q1
#################################################
data_reg <- merge %>% 
            # drop investments in tax havens to remove double counting
            #filter(!issuer %in% tax_haven)%>%
            mutate(outside_holding = ifelse(issuer == "OTH",0,cpi_holding)) %>%
            group_by(investor,year) %>% 
            # calculate total holdings and outside holdings for each investor
            mutate(A_it = sum(cpi_holding,na.rm=TRUE), O_it = sum(outside_holding,na.rm=TRUE)) %>%
            ungroup() %>%
            # calculate holding weights
            mutate(w_it_O = O_it/A_it,
                   w_it_N = 1-w_it_O,
                   w_it_n = cpi_holding/A_it,
                   ln_shares = log(w_it_n/w_it_O))

# Check NaNs - negative CPI?
data_reg %>% filter(is.na(ln_shares)) %>% View()

# add dummies for investor and time fixed effect
data_reg_dummies <- data_reg %>% 
                    select(ln_shares,dist,comlang,gdp,gdp_cap,investor,issuer,year) %>% 
                    dummy_cols(.,select_columns = c('investor','year')) %>% 
                    #remove missing ln_shares and drop incomplete rows
                    filter(!is.na(ln_shares),
                           !ln_shares%in% c(Inf,-Inf)) %>%
                    drop_na()

# Regression
est_model <- lm(ln_shares~.,data=data_reg_dummies%>% select(-investor,-issuer,-year))
summary(est_model)

# Add back predict ln_shares to dataset
data_reg <- data_reg_dummies %>% 
            mutate(pred_ln_shares=est_model$fitted.values) %>%
            select(investor,issuer,year,pred_ln_shares) %>% 
            right_join(data_reg)

#################################################
#                   Q2
#################################################
data_reg<-data_reg %>%
          mutate(pred_w_it_n = exp(pred_ln_shares)*w_it_O) 

plot_fn <- function(data,country) {
  plt <- ggplot(data %>% filter(investor==country) , aes(x=w_it_n, y=pred_w_it_n)) +
    geom_point(size=1,shape = 0) + 
    xlab("Actual Portfolio Shares") + 
    ylab("Predicted Portfolio Shares") +
    ggtitle(country) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_abline(color="red")
    #geom_smooth(method = "lm", se = FALSE)
  return(plt)
}

# Plot actual vs predicted allocations
usa_plt <- plot_fn(data_reg,"USA")
germany_plt <- plot_fn(data_reg,"DEU")

usa_plt
german_plt

usa_ger_plt <- ggplot(data_reg %>% filter(investor=="USA",issuer=="GBR"), aes(x=year)) +
  geom_point(aes(y=w_it_n,col="Actual Portfolio Shares")) +
  geom_point(aes(y=pred_w_it_n,col="Predicted Portfolio Shares")) + 
  xlab("Year") + 
  ylab("Portfolio Shares") +
  ggtitle("US investment in UK") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom") +
  scale_colour_manual(values = c("blue","red"))


#################################################
#                   Q3
#################################################
data_fx<- data_reg %>%
        group_by(investor, year) %>%
        # for each investor - total share invested in 36 issuer countries - (note pred_w_it_n is NA for issuer = "OTH")
        mutate(pred_w_it_N = sum(pred_w_it_n,na.rm=TRUE),
               # ignore if predicted share of outside holding is less than 0
               pred_w_it_O = ifelse((1-pred_w_it_N)<0,NA_real_,1-pred_w_it_N),
               pred_A_it = O_it/pred_w_it_O,
               #A_it_diff = (A_it-pred_A_it)/mean(A_it,na.rm=TRUE)
              ) %>%
        ungroup() %>%
        mutate(oth_inv_n = ifelse(investor=="OTH",cpi_holding,0),
               pred_holding_it_n = pred_A_it*pred_w_it_n
               ) %>%
        group_by(issuer,year) %>%
        # for each issuer total invested from 88 investor countries
        mutate(oth_inv_I = sum(oth_inv_n,na.rm=TRUE),
               holding_I = sum(cpi_holding,na.rm=TRUE)-oth_inv_I,
               #holding_I= sum(holding_it_n,na.rm=TRUE),
               pred_holding_I = sum(pred_holding_it_n,na.rm=TRUE),
               pred_fx = (mean(fx)/holding_I)*pred_holding_I) %>%
        ungroup()

data_fx_agg <- data_fx %>% group_by(issuer,year) %>% summarize_at(c("fx","pred_fx","holding_I","holding_I"),mean)

#data_fx %>% filter(pred_w_it_N>1)%>% select(investor,year,pred_w_it_N,w_it_N) %>% distinct() %>% View()

pred_exchange <- ggplot(data_fx_agg %>% filter(issuer=="GBR"), aes(x=year)) +
                  geom_point(aes(y=fx,col="Actual FX")) +
                  geom_point(aes(y=1/pred_fx,col="Predicted FX")) + 
                  xlab("Year") + 
                  ylab("FX") +
                  ggtitle("Exchange Rates") +
                  theme(plot.title = element_text(hjust = 0.5),
                        legend.position="bottom") +
                  scale_colour_manual(values = c("blue","red"))


#################################################
#                   Q4
#################################################
che_self_distace <- cepii %>% filter(issuer=="CHE",investor=="CHE") %>%pull(dist) %>% unique()

distw_new <- cepii %>% 
            mutate(investor_issuer_org = paste0(investor,"_",issuer)) %>%
            filter(issuer=="CHE"|investor=="CHE") %>% 
            select(investor_issuer_org,investor,issuer,year,dist_new=dist) %>%
            mutate(issuer = case_when(investor_issuer_org=="NZL_CHE"~"CHE",
                                      issuer=="CHE"~"NZL",
                                      TRUE~issuer),
                   investor = case_when(investor_issuer_org=="CHE_NZL"~"CHE",
                                      investor=="CHE"~"NZL",
                                      TRUE~investor)) %>% 
            mutate(dist_new=ifelse(investor_issuer_org%in%c("NZL_CHE","CHE_NZL","CHE_CHE"),che_self_distace,dist_new)) %>%
            select(-investor_issuer_org)

data_reg_nz <- data_reg %>%
                left_join(distw_new) %>%
               mutate(dist=ifelse(is.na(dist_new),dist,dist_new)) %>%
               select(-dist_new,-pred_ln_shares)

# add dummies for investor and time fixed effect
data_reg_dummies_nz <- data_reg_nz %>% 
  select(ln_shares,dist,comlang,gdp,gdp_cap,investor,issuer,year) %>% 
  dummy_cols(.,select_columns = c('investor','year')) %>% 
  #remove missing ln_shares and drop incomplete rows
  filter(!is.na(ln_shares),
         !ln_shares%in% c(Inf,-Inf)) %>%
  drop_na()

# Predict
pred_nz <- predict(est_model,data_reg_dummies_nz)

# Regression
#est_model_nz <- lm(ln_shares~.,data=data_reg_dummies_nz%>% select(-investor,-issuer,-year))
#summary(est_model_nz)

# Add back predict ln_shares to dataset 
data_reg_nz <- data_reg_dummies_nz %>% 
              mutate(pred_ln_shares=pred_nz) %>%
              select(investor,issuer,year,pred_ln_shares) %>% 
              right_join(data_reg_nz) %>%
              filter(issuer=="NZL"|investor=="NZL") %>% 
              mutate(pred_w_it_n = exp(pred_ln_shares)*w_it_O,
                     pred_cpi_holding = pred_w_it_n*A_it)
              #left_join(data_reg %>% select(investor,issuer,year,w_it_n_old=w_it_n,A_it_old=A_it,cpi_holding_old=cpi_holding)) 

data_reg_nz_issuer_agg_yr <- data_reg_nz %>% 
                            filter(issuer=="NZL") %>%
                            mutate(other_cpi_holding_n = ifelse(investor=="OTH",cpi_holding,0)) %>%
                            group_by(year) %>% 
                            summarize(pred_cpi_holding=sum(pred_cpi_holding,na.rm=TRUE),
                                      cpi_holding=sum(cpi_holding,na.rm = TRUE)-sum(other_cpi_holding_n))

data_reg_nz_investor_agg_yr <- data_reg_nz %>% 
  filter(investor=="NZL") %>%
  #mutate(other_cpi_holding_n = ifelse(investor=="OTH",cpi_holding,0)) %>%
  group_by(year) %>% 
  summarize(pred_cpi_holding=sum(pred_cpi_holding,na.rm=TRUE),
            cpi_holding=sum(A_it,na.rm=TRUE)-sum(O_it,na.rm=TRUE))

plot_bars <- function(data,invest=TRUE) {
  invest_direction <- ifelse(invest,"into","out of")
  plt <- data %>% 
    mutate_at(vars(contains("holding")),~./10^(9)) %>%
    rename(Predicted=pred_cpi_holding,Actual=cpi_holding) %>%
    pivot_longer(cols=c("Predicted","Actual"),names_to="holding") %>%
    ggplot(., aes(x=year, y=value, fill=holding)) +
    geom_bar(stat='identity', position='dodge') + 
    xlab("Year") + 
    ylab("Investment (billions)") +
    ggtitle(paste("Investment",invest_direction,"New Zealand",sep=" ")) +
    labs(fill = "Holding") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position="bottom")
  
  return(plt)
}

nz_issuer_by_year <- plot_bars(data_reg_nz_issuer_agg_yr)
nz_investor_by_year <- plot_bars(data_reg_nz_investor_agg_yr)

nz_issuer_by_year
nz_investor_by_year
#################################################
#                   Save figures
#################################################


png(file=paste0(fig_data_path,"us_allocations.png"),
    width=600, height=350)
usa_plt
dev.off()

png(file=paste0(fig_data_path,"ger_allocations.png"),
    width=600, height=350)
germany_plt
dev.off()

nz_issuer_by_year <- plot_bars(data_reg_nz_issuer_agg_yr)
png(file=paste0(fig_data_path,"nz_issuer_by_year.png"),
    width=600, height=350)
nz_issuer_by_year
dev.off()


nz_investor_by_year <- plot_bars(data_reg_nz_investor_agg_yr,FALSE) 
png(file=paste0(fig_data_path,"nz_investor_by_year.png"),
    width=600, height=350)
nz_investor_by_year
dev.off()

png(file=paste0(fig_data_path,"us_ger_allocations.png"),
    width=600, height=350)
usa_germ_plt
dev.off()

png(file=paste0(fig_data_path,"pred_fx_uk.png"),
    width=600, height=350)
pred_exchange
dev.off()

