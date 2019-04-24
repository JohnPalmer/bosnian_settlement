# !diagnostics off
rm(list=ls())

library(data.table)
library(tidyverse)

cbsa2005_counties2005 = read_csv("~/research/acs/data/cbsa2005_counties2005.csv")

cbsa2015_2_county2014 = read_csv("~/research/acs/data/cbsa2015_2_county2014.csv") %>% filter(!is.na(cbsa))

E2009 = as.tibble(readRDS(file="~/research/acs/data/summary_file_tracts_bosnians_median_income.Rds")) %>% mutate(FIPS = as.integer(paste0(STATE, COUNTY))) %>% left_join(cbsa2005_counties2005, by=c("FIPS"="FIPS"))

E2016 = as.tibble(readRDS(file="~/research/acs/data/summary_file_tracts_bosnians_median_income_2016.Rds")) %>% mutate(FIPS = paste0(STATE, COUNTY, TRACT), CountyFIPS = as.integer(paste0(STATE, COUNTY))) %>% left_join(cbsa2015_2_county2014, by=c("CountyFIPS"="county14"))


CBSA_counts = readRDS("~/research/acs/data/cbsa_bosnian_counts.Rds") %>% arrange(desc(N_B_2009)) %>% mutate(Name = as.character(Name))


i = 1
cbsa_track_dist = bind_rows(lapply(1:10, function(i){
  this_cbsa = pull(CBSA_counts, CBSA)[i]
  this_cbsa_name = pull(CBSA_counts, Name)[i]
  these_tracts = E2009 %>% filter(CBSA_Code==this_cbsa & !is.na(median_income)) 
  these_quants = quantile(these_tracts$median_income, seq(.1,.9, .1), na.rm=TRUE)
  these_tracts$median_income_quant = NA
  these_tracts$median_income_quant[which(these_tracts$median_income < these_quants[1])] = 1
  for(i in 2:(length(these_quants))){
    these_tracts$median_income_quant[which(these_tracts$median_income >= these_quants[i-1] & these_tracts$median_income < these_quants[i])] = i
  }
  these_tracts$median_income_quant[which(these_tracts$median_income >= these_quants[length(these_quants)])] = length(these_quants)+1
  these_tracts_agg = these_tracts %>% group_by(median_income_quant, CBSA_Code, CBSA_Title) %>% summarise(N_Bos=sum(N_Bos, na.rm = TRUE), N_Bos_ME90 = sqrt(sum(N_Bos_ME90^2, na.rm = TRUE))) 
  return(these_tracts_agg)
}))

cbsa_track_dist %>% group_by(CBSA_Code, CBSA_Title) %>% summarise(sum(N_Bos, na.rm=TRUE))


ggplot(cbsa_track_dist, aes(x=median_income_quant, y=N_Bos)) + geom_bar(stat="identity")  + facet_wrap(~CBSA_Title, ncol=2) + geom_errorbar(aes(ymin=if_else( (N_Bos-N_Bos_ME90)<0,0,(N_Bos-N_Bos_ME90)), ymax=N_Bos+N_Bos_ME90), width=.2) + ylab("Bosnia-born population") + xlab("Tract median income quantile")
ggsave("~/research/acs/figures/tract_quants_2009.pdf")


# 2016
i = 1
cbsa_track_dist_16 = bind_rows(lapply(1:10, function(i){
  this_cbsa = pull(CBSA_counts, CBSA)[i]
  this_cbsa_name = pull(CBSA_counts, Name)[i]
  these_tracts = E2016 %>% filter(cbsa==this_cbsa & !is.na(median_income)) 
  these_quants = quantile(these_tracts$median_income, seq(.1,.9, .1), na.rm=TRUE)
  these_tracts$median_income_quant = NA
  these_tracts$median_income_quant[which(these_tracts$median_income < these_quants[1])] = 1
  for(i in 2:(length(these_quants))){
    these_tracts$median_income_quant[which(these_tracts$median_income >= these_quants[i-1] & these_tracts$median_income < these_quants[i])] = i
  }
  these_tracts$median_income_quant[which(these_tracts$median_income >= these_quants[length(these_quants)])] = length(these_quants)+1
  these_tracts_agg = these_tracts %>% group_by(median_income_quant, cbsa, cbsaname15) %>% summarise(N_Bos=sum(N_Bos, na.rm = TRUE), N_Bos_ME90 = sqrt(sum(N_Bos_ME90^2, na.rm = TRUE))) 
  return(these_tracts_agg)
}))

cbsa_track_dist_16 %>% group_by(cbsa, cbsaname15) %>% summarise(sum(N_Bos, na.rm=TRUE))


ggplot(cbsa_track_dist_16, aes(x=median_income_quant, y=N_Bos)) + geom_bar(stat="identity")  + facet_wrap(~cbsaname15, ncol=2) + geom_errorbar(aes(ymin=if_else( (N_Bos-N_Bos_ME90)<0,0,(N_Bos-N_Bos_ME90)), ymax=N_Bos+N_Bos_ME90), width=.2) + ylab("Bosnia-born population") + xlab("Tract median income quantile")
ggsave("~/research/acs/figures/tract_quants_2016.pdf")


# counrtywide

quants = quantile(E2009$median_income, seq(.1,.9, .1), na.rm=TRUE)
E2009$median_income_quant = NA
E2009$median_income_quant[which(E2009$median_income < quants[1])] = 1
for(i in 2:(length(quants))){
  E2009$median_income_quant[which(E2009$median_income >= quants[i-1] & E2009$median_income < quants[i])] = i
}
E2009$median_income_quant[which(E2009$median_income >= quants[length(quants)])] = length(quants)+1
E2009_us_quants = E2009 %>% group_by(median_income_quant) %>% summarise(N_Bos=sum(N_Bos), N_Bos_ME90 = sqrt(sum(N_Bos_ME90^2))) %>% filter(! is.na(median_income_quant))
ggplot(E2009_us_quants, aes(x=median_income_quant, y=N_Bos)) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=if_else( (N_Bos-N_Bos_ME90)<0,0,(N_Bos-N_Bos_ME90)), ymax=N_Bos+N_Bos_ME90), width=.2) + ylab("Bosnia-born population") + xlab("Tract median income quantile")

quants = quantile(E2016$median_income, seq(.1,.9, .1), na.rm=TRUE)
E2016$median_income_quant = NA
E2016$median_income_quant[which(E2016$median_income < quants[1])] = 1
for(i in 2:(length(quants))){
  E2016$median_income_quant[which(E2016$median_income >= quants[i-1] & E2016$median_income < quants[i])] = i
}
E2016$median_income_quant[which(E2016$median_income >= quants[length(quants)])] = length(quants)+1
E2016_us_quants = E2016 %>% group_by(median_income_quant) %>% summarise(N_Bos=sum(N_Bos), N_Bos_ME90 = sqrt(sum(N_Bos_ME90^2))) %>% filter(! is.na(median_income_quant))
ggplot(E2016_us_quants, aes(x=median_income_quant, y=N_Bos)) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=if_else( (N_Bos-N_Bos_ME90)<0,0,(N_Bos-N_Bos_ME90)), ymax=N_Bos+N_Bos_ME90), width=.2) + ylab("Bosnia-born population") + xlab("Tract median income quantile")


E2016_us_quants$ACS = "2012-2016"
E2009_us_quants$ACS = "2005-2006"
combo_quants = bind_rows(E2016_us_quants, E2009_us_quants)
ggplot(combo_quants, aes(x=median_income_quant, y=N_Bos)) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=if_else( (N_Bos-N_Bos_ME90)<0,0,(N_Bos-N_Bos_ME90)), ymax=N_Bos+N_Bos_ME90), width=.2) + ylab("Bosnia-born population") + xlab("Tract median income quantile") + facet_wrap(~ACS, ncol=2)



