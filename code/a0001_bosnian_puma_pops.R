rm(list=ls())

library(data.table)
library(tidyverse)
library(Hmisc)

E2009 = as.tibble(readRDS(file="~/research/acs/data/summary_file_pumas_bosnians_median_income.Rds")) %>% mutate(bos_prop = N_Bos/N, ST=as.integer(STATE), PUMA=as.integer(PUMA5), median_income=median_income*1.12, median_income_ME90=median_income_ME90*sqrt(1.12))


E2016 = as.tibble(readRDS(file="~/research/acs/data/summary_file_pumas_bosnians_median_income_2016.Rds")) %>% mutate(bos_prop = N_Bos/N, ST=as.integer(STATE), PUMA=as.integer(PUMA5))

p2p = read_csv("~/research/acs/data/puma2puma.csv", skip=1)
names(p2p) = names(read_csv("~/research/acs/data/puma2puma.csv"))
p2p = p2p %>% mutate(stint = as.integer(state), puma12int = as.integer(puma12), puma2kint = as.integer(puma2k))

D = as.tibble(readRDS(file="~/research/acs/data/ss09pus_POBP_bos.Rds")) %>% mutate(PINCP_r=replace(PINCP, PINCP == "bbbbbbb", NA), ADJINC_r=ADJINC*.000001, PINCP_ADJ_r = PINCP_r * ADJINC_r, ref_year=case_when(ADJINC==1119794~2005, ADJINC==1080505~2006, ADJINC==1051849~2007, ADJINC==1014521~2008, ADJINC==999480~ 2009), nat=CIT==4, LANX_r=factor(LANX), ENG_r=factor(ENG), FER_yes=FER==2, SEX_female=SEX==2, YOEP_r=replace(YOEP, YOEP=="bbbb", NA), yse=ref_year-YOEP_r, entry_period=case_when(YOEP<1992~"pre_war", YOEP %in% 1992:1995~"war", YOEP %in% 1996:1999~"immediate_post_war", YOEP>1999~"post_war"), working_age=AGEP%in%15:64, working_age_income=if_else(working_age, PINCP_ADJ_r, NULL)) 

Dh = as.tibble(readRDS(file="~/research/acs/data/ss09hus_POBP_bos.Rds")) %>% mutate(HINCP_r=replace(HINCP, HINCP == "bbbbbbb", NA), ADJINC_r=ADJINC*.000001, HINCP_ADJ_r = HINCP_r * ADJINC_r * 1.12) #adjusting by 1.12 to put into 2016 dollars 

D16 = as.tibble(readRDS(file="~/research/acs/data/ss16pus_POBP_bos.Rds")) %>% mutate(PINCP_r=replace(PINCP, PINCP == "bbbbbbb", NA), ADJINC_r=ADJINC*.000001, PINCP_ADJ_r = PINCP_r * ADJINC_r, ref_year=case_when(ADJINC==1119794~2005, ADJINC==1080505~2006, ADJINC==1051849~2007, ADJINC==1014521~2008, ADJINC==999480~ 2009), nat=CIT==4, LANX_r=factor(LANX), ENG_r=factor(ENG), FER_yes=FER==2, SEX_female=SEX==2, YOEP_r=replace(YOEP, YOEP=="bbbb", NA), yse=ref_year-YOEP_r, entry_period=case_when(YOEP<1992~"pre_war", YOEP %in% 1992:1995~"war", YOEP %in% 1996:1999~"immediate_post_war", YOEP>1999~"post_war"), working_age=AGEP%in%15:64, working_age_income=if_else(working_age, PINCP_ADJ_r, NULL)) %>% left_join(p2p, by=c("ST"="stint", "PUMA"="puma12int")) 

Dh16 = as.tibble(readRDS(file="~/research/acs/data/ss16hus_POBP_bos.Rds")) %>% mutate(HINCP_r=replace(HINCP, HINCP == "bbbbbbb", NA), ADJINC_r=ADJINC*.000001, HINCP_ADJ_r = HINCP_r * ADJINC_r) 


# 2009
puma_n = D %>% group_by(ST, PUMA) %>% summarise(n=n())
D = D %>% left_join(puma_n)

repcols = grep("PWGTP", names(D))
puma2009 = bind_rows(lapply(2:length(repcols), function(i) D %>% filter(n>6) %>% select(PWGTP, ST=ST, PUMA=PUMA, x=working_age_income, w=repcols[i]) %>% group_by(ST, PUMA) %>% summarise(n = n(), mean_working_age_income_r=weighted.mean(x, w=w, na.rm=TRUE), mean_working_age_income=weighted.mean(x, w=PWGTP, na.rm=TRUE), median_working_age_income_r=wtd.quantile(x, weights=w,probs = .5, na.rm=TRUE), median_working_age_income=wtd.quantile(x, weights=PWGTP, probs=.5, na.rm=TRUE)))) %>% mutate(sq_diff_means= (mean_working_age_income_r-mean_working_age_income)^2, sq_diff_medians= (median_working_age_income_r-median_working_age_income)^2) %>% group_by(ST, PUMA,n, mean_working_age_income, median_working_age_income) %>% summarise(mean_income_se=sqrt((4/80)*sum(sq_diff_means)), median_income_se=sqrt((4/80)*sum(sq_diff_medians))) %>% filter(n>1) %>% left_join(E2009, by=c("ST"="ST", "PUMA"="PUMA")) %>% arrange(desc(N_Bos))

# housholds

puma_n = Dh %>% group_by(ST, PUMA) %>% summarise(n=n())
Dh = Dh %>% left_join(puma_n)


p2k_names = p2p %>% group_by(stint, puma2kint) %>% summarise(PUMAname = first(PUMAname)) %>% ungroup()

repcols = grep("WGTP", names(Dh))
puma2009h = bind_rows(lapply(2:length(repcols), function(i) Dh %>% filter(n>6, !is.na(HINCP_ADJ_r)) %>% select(WGTP, ST=ST, PUMA=PUMA, x=HINCP_ADJ_r, w=repcols[i]) %>% group_by(ST, PUMA) %>% summarise(n = n(), bosnian_median_income_r=wtd.quantile(x, weights=w,probs = .5, na.rm=TRUE), bosnian_median_income=wtd.quantile(x, weights=WGTP, probs=.5, na.rm=TRUE)))) %>% mutate(sq_diff_medians= (bosnian_median_income_r-bosnian_median_income)^2) %>% group_by(ST, PUMA,n, bosnian_median_income) %>% summarise(bosnian_median_income_se=sqrt((4/80)*sum(sq_diff_medians))) %>% left_join(E2009, by=c("ST"="ST", "PUMA"="PUMA")) %>% arrange(desc(N_Bos)) %>% mutate(year=2009) %>% left_join(p2k_names, by=c("ST"="stint", "PUMA"="puma2kint")) %>% mutate(city=case_when(str_detect(PUMAname, "Chicago")~"Chicago",  str_detect(PUMAname, "St. Louis")~"St. Louis", str_detect(PUMAname, "Oneida")~"Utica", TRUE~"other"))

# full 2009
repcols = grep("WGTP", names(Dh))

bind_rows(lapply(2:length(repcols), function(i) Dh %>% filter(!is.na(HINCP_ADJ_r)) %>% select(WGTP, x=HINCP_ADJ_r, w=repcols[i])  %>% summarise(n = n(), bosnian_median_income_r=wtd.quantile(x, weights=w,probs = .5, na.rm=TRUE), bosnian_median_income=wtd.quantile(x, weights=WGTP, probs=.5, na.rm=TRUE)))) %>% mutate(sq_diff_medians=(bosnian_median_income_r-bosnian_median_income)^2) %>% summarise(bosnian_median_income_se=sqrt((4/80)*sum(sq_diff_medians)), bosnian_median_income=min(bosnian_median_income)) 

# 2016
puma_n = Dh16 %>% group_by(ST, PUMA) %>% summarise(n=n())
Dh16 = Dh16 %>% left_join(puma_n)

repcols = grep("WGTP", names(Dh16))
puma2016h = bind_rows(lapply(2:length(repcols), function(i) Dh16 %>% filter(n>6, !is.na(HINCP_ADJ_r)) %>% select(WGTP, ST=ST, PUMA=PUMA, x=HINCP_ADJ_r, w=repcols[i]) %>% group_by(ST, PUMA) %>% summarise(n = n(), bosnian_median_income_r=wtd.quantile(x, weights=w,probs = .5, na.rm=TRUE), bosnian_median_income=wtd.quantile(x, weights=WGTP, probs=.5, na.rm=TRUE)))) %>% mutate(sq_diff_medians=(bosnian_median_income_r-bosnian_median_income)^2) %>% group_by(ST, PUMA,n, bosnian_median_income) %>% summarise(bosnian_median_income_se=sqrt((4/80)*sum(sq_diff_medians))) %>% left_join(E2016, by=c("ST"="ST", "PUMA"="PUMA")) %>% arrange(desc(N_Bos)) %>% mutate(year=2016) %>% mutate(city=case_when(str_detect(Name, "Chicago")~"Chicago",  str_detect(Name, "St. Louis")~"St. Louis", str_detect(Name, "Utica")~"Utica", TRUE~"other"))

puma_both_h = bind_rows(puma2009h, puma2016h) %>% mutate(city_f=factor(city, levels=c("Chicago", "St. Louis", "Utica", "other")))


ggplot(puma_both_h%>%filter(n>6), aes(x=bosnian_median_income, y=median_income)) + geom_errorbar(aes(ymin=median_income-median_income_ME90, ymax=median_income+median_income_ME90), colour="dark grey", width=.1) + geom_errorbarh(aes(xmin=bosnian_median_income-(1.645*bosnian_median_income_se), xmax=bosnian_median_income+(1.645*bosnian_median_income_se)), colour="dark grey", alpha=1) + geom_abline(intercept = 0, slope=1, lty=2) + geom_point(aes(size=N_Bos, fill=city_f), alpha=1, shape=21) + theme_bw() + xlab("Bosnian-born median income") + ylab("Median income") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + facet_grid(.~year) + labs(size="Bosnians", fill="City") + coord_cartesian(ylim=c(30000,100000), xlim=c(0,150000)) + scale_fill_manual(values=c("Chicago"="#d7191c", "St. Louis"="#fdae61",  "Utica"="#386cb0", "other"=NA))
ggsave("~/research/acs/figures/puma_household_income.pdf")


ggplot(puma_both_h%>%mutate(STPUMA=paste0(ST, PUMA)) %>% filter(n>6, STPUMA %in% key_puma2kints | str_detect(Name, "Utica")), aes(x=bosnian_median_income, y=median_income)) + geom_errorbar(aes(ymin=median_income-median_income_ME90, ymax=median_income+median_income_ME90), colour="dark grey", width=.1) + geom_errorbarh(aes(xmin=bosnian_median_income-(1.645*bosnian_median_income_se), xmax=bosnian_median_income+(1.645*bosnian_median_income_se)), colour="dark grey", alpha=1) + geom_abline(intercept = 0, slope=1, lty=2) + geom_point(aes(size=N_Bos), alpha=1, shape=1) + theme_bw() + xlab("Bosnian-born median income") + ylab("Median income") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + facet_grid(.~year) + labs(size="Bosnians") + coord_cartesian(ylim=c(30000,100000), xlim=c(0,150000)) 
ggsave("~/research/acs/figures/puma_household_income_key_pumas.pdf")


# full 2016
bind_rows(lapply(2:length(repcols), function(i) Dh16 %>% filter(!is.na(HINCP_ADJ_r)) %>% select(WGTP, x=HINCP_ADJ_r, w=repcols[i])  %>% summarise(n = n(), bosnian_median_income_r=wtd.quantile(x, weights=w,probs = .5, na.rm=TRUE), bosnian_median_income=wtd.quantile(x, weights=WGTP, probs=.5, na.rm=TRUE)))) %>% mutate(sq_diff_medians=(bosnian_median_income_r-bosnian_median_income)^2) %>% summarise(bosnian_median_income_se=sqrt((4/80)*sum(sq_diff_medians)), bosnian_median_income=min(bosnian_median_income))


### OLD STUFF


i=1
D %>% group_by(ST, county, cntyname) %>% select(repcols[i], working_age_income, yse, SEX_female, AGEP, nat, FER_yes) %>% summarise(N=sum(repcols[i]), mean_working_age_income=weighted.mean(working_age_income, w=repcols[i], na.rm=TRUE), mean_yse=weighted.mean(yse, w=repcols[i], na.rm=TRUE), pFemale=weighted.mean(SEX_female, weights=repcols[i], na.rm=TRUE), mean_age=weighted.mean(AGEP, weights=repcols[i], na.rm=TRUE), pnat=weighted.mean(nat, weights=repcols[i], na.rm=TRUE), pFER_yes=weighted.mean(FER_yes, weights=repcols[i], na.rm=TRUE)) %>% arrange(desc(N))

D %>% select(c("ST", "county_r", "cntyname_r"))

make_se = function(D, x, groups, rep_root="PWGTP"){
  D %>% group_by(groups) %>% select(grep(rep_root, names(.))) %>% summarise_all(sum)
}







puma_ests = D %>% group_by(ST, PUMA) %>% select(PWGTP) %>% summarise(N=sum(PWGTP))
puma_reps = D %>% group_by(ST, PUMA) %>% select(grep("PWGTP", names(.))) %>% summarise_all(sum)
ses = sqrt((4/80)*rowSums((puma_ests %>% pull(N) - (puma_reps %>% ungroup() %>% select(grep("PWGTP", names(.))) %>% select(2:81)))^2))
puma_ests$se = ses
puma_ests = puma_ests %>% mutate(ci95ll = (N - 1.96*se), ci95ul = (N + 1.96*se), ci95ll = case_when(ci95ll<0~0, ci95ll>=0~ci95ll)) %>% arrange(desc(N))

puma_ests16 = D16 %>% group_by(ST, PUMA) %>% select(PWGTP) %>% summarise(N=sum(PWGTP))
puma_reps16 = D16 %>% group_by(ST, PUMA) %>% select(grep("PWGTP", names(.))) %>% summarise_all(sum)
ses = sqrt((4/80)*rowSums(( (puma_ests16 %>% pull(N)) - (puma_reps16 %>% ungroup() %>% select(grep("PWGTP", names(.))) %>% select(2:81)))^2))
puma_ests16$se = ses
puma_ests16 = puma_ests16 %>% mutate(ci95ll = (N - (1.96*se)), ci95ul = (N + (1.96*se)), ci95ll = case_when(ci95ll<0~0, ci95ll>=0~ci95ll)) %>% arrange(desc(N))  

# fix join vs
full_join(puma_ests, puma_ests16, by=c("PUMA", "ST"), suffix=c("2009", "2016"))



state_ests = D %>% group_by(ST) %>% select(PWGTP) %>% summarise(N=sum(PWGTP))
state_reps = D %>% group_by(ST) %>% select(grep("PWGTP", names(.))) %>% summarise_all(sum)
ses = sqrt((4/80)*rowSums((state_ests %>% pull(N) - (state_reps %>% ungroup() %>% select(grep("PWGTP", names(.))) %>% select(2:81)))^2))
state_ests$se = ses
state_ests = state_ests %>% mutate(ci95ll = (N - 1.96*se), ci95ul = (N + 1.96*se), ci95ll = case_when(ci95ll<0~0, ci95ll>=0~ci95ll)) %>% arrange(desc(N))

state_ests16 = D16 %>% group_by(ST) %>% select(PWGTP) %>% summarise(N=sum(PWGTP))
state_reps16 = D16 %>% group_by(ST) %>% select(grep("PWGTP", names(.))) %>% summarise_all(sum)
ses = sqrt((4/80)*rowSums((state_ests16 %>% pull(N) - (state_reps16 %>% ungroup() %>% select(grep("PWGTP", names(.))) %>% select(2:81)))^2))
state_ests16$se = ses
state_ests16 = state_ests16 %>% mutate(ci95ll = (N - 1.96*se), ci95ul = (N + 1.96*se), ci95ll = case_when(ci95ll<0~0, ci95ll>=0~ci95ll)) %>% arrange(desc(N))


full_join(state_ests, state_ests16, by=c("ST"), suffix=c("2009", "2016"))
