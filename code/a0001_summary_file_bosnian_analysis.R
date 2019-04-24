rm(list=ls())
library(data.table)
library(tidyverse)

E2016 = as.tibble(readRDS(file="~/research/acs/data/E2016_CBSA_bos.Rds")) %>%
  full_join(select(as.tibble(readRDS(file="~/research/acs/data/M2016_CBSA_bos.Rds")), LOGRECNO, MoE_B=BOSCOUNT, MoE_FB=FOREIGNBORN, MoE_T=TOTALPOP)) %>% 
  select(LOGRECNO, GEOID, Name, BOSCOUNT, MoE_B, FB=FOREIGNBORN, MoE_FB, TOTALPOP, MoE_T) %>% 
#  mutate(ci90ll=BOSCOUNT-MoE, ci90ul=BOSCOUNT+MoE) %>%
#  mutate(ci90ll=case_when(ci90ll<0~as.integer(0), ci90ll>=0~ci90ll)) %>%
  mutate(CBSA=as.integer(substr(GEOID, 8, 12)), year=2016) %>%
  arrange(desc(BOSCOUNT)) %>% select(CBSA, N_B=BOSCOUNT, MoE_B=MoE_B, N_FB=FB, MoE_FB=MoE_FB, N_T=TOTALPOP, MoE_T=MoE_T, year)

E2009 = as.tibble(readRDS(file="~/research/acs/data/E2009_CBSA_bos.Rds")) %>% 
  filter(SUMLEVEL==310) %>% 
  full_join(select(as.tibble(readRDS(file="~/research/acs/data/M2009_CBSA_bos.Rds")), LOGRECNO, MoE_B=BOSCOUNT, MoE_FB=FOREIGNBORN, MoE_T=TOTALPOP)) %>% 
  select(LOGRECNO, Name,  BOSCOUNT, MoE_B, FB=FOREIGNBORN, MoE_FB, TOTALPOP, MoE_T, CBSA) %>%
  arrange(desc(BOSCOUNT)) %>%
  mutate(year=2009) %>%
select(Name, CBSA, N_B=BOSCOUNT,MoE_B, N_FB=FB, MoE_FB=MoE_FB, N_T=TOTALPOP,MoE_T=MoE_T, year)

D = E2009 %>% full_join(E2016, by="CBSA", suffix = c("_2009", "_2016")) %>%
  mutate(diff_B=N_B_2016-N_B_2009, MoE_diff_B=sqrt((MoE_B_2009^2)+(MoE_B_2016^2)), sig_diff_B=(abs(diff_B)/MoE_diff_B)>1, diff_T=N_T_2016-N_T_2009, MoE_diff_T=sqrt((MoE_T_2009^2)+(MoE_T_2016^2)), sig_diff_T=(abs(diff_T)/MoE_diff_T)>1, diff_FB=N_FB_2016-N_FB_2009, MoE_diff_FB=sqrt((MoE_FB_2009^2)+(MoE_FB_2016^2)), sig_diff_FB=(abs(diff_FB)/MoE_diff_FB)>1) %>%
  arrange(desc(N_B_2009))

D_long = bind_rows(E2009, E2016)

top10_short_names = c("Chicago", "St. Louis", "Atlanta", "Phoenix", "Detroit", "New York", "Tampa", "Des Moines", "Utica", "Jacksonville")
top10_cbsas = pull(D, CBSA)[1:10]

top10df = data_frame(name=factor(top10_short_names, levels=top10_short_names), CBSA= top10_cbsas)

D_long_10 = D_long %>% filter(CBSA %in% top10_cbsas) %>% left_join(top10df, by="CBSA") 

ggplot(D_long_10, aes(x=name, y=N_B, fill=factor(year))) + geom_bar(stat="identity", position="dodge") + geom_errorbar(position=position_dodge(width=0.9), width=.2, aes(ymin=N_B-MoE_B, ymax=N_B+MoE_B)) + theme_bw() +xlab("Metropolitan Area") + ylab("Bosnian-born population") + labs(fill="End-year")
ggsave("~/research/acs/figures/CBSA_pop_bars.pdf") 


diffs = D %>% filter(sig_diff_B) %>% select(Name, N_B_2009, N_B_2016, diff_B, diff_FB, sig_diff_FB, diff_T, sig_diff_T, MoE_diff_B) %>% arrange(diff_B) 

diffnames = c("Atlanta", "San Jose", "Louisville", "Nashville", "Boulder", "Sheboygan", "Rochester", "Washington", "Fargo")

diffs = diffs %>% mutate(name=factor(diffnames, levels=diffnames))

ggplot(diffs, aes(x=name, y=diff_B)) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=diff_B-MoE_diff_B, ymax=diff_B+MoE_diff_B), width=.2) + theme_bw() + xlab("Metropolitan Area") + ylab("Bosnian-born population change")
ggsave("~/research/acs/figures/CBSA_change_bars.pdf") 


saveRDS(D, file="~/research/acs/data/cbsa_bosnian_counts.Rds")
