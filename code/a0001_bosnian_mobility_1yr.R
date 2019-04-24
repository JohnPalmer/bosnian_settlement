library(data.table)
library(tidyverse)
library(Hmisc)

p2p = read_csv("~/research/acs/data/puma2puma.csv", skip=1)
names(p2p) = names(read_csv("~/research/acs/data/puma2puma.csv"))
p2p = p2p %>% mutate(stint = as.integer(state), puma12int = as.integer(puma12), puma2kint = as.integer(puma2k))

p2010_2_cbsa2010 = read_csv("~/research/acs/data/puma2010_2_cbsa2010.csv", skip=1) 
names(p2010_2_cbsa2010) = names(read_csv("~/research/acs/data/puma2010_2_cbsa2010.csv"))
p2010_2_cbsa2010 = p2010_2_cbsa2010 %>% mutate(stint = as.integer(state), puma2kint = as.integer(puma2k)) %>% filter(cbsa10 != 99999)

D = as.tibble(readRDS(file="~/research/acs/data/ss09pus_POBP_bos.Rds")) %>% mutate(PINCP_r=replace(PINCP, PINCP == "bbbbbbb", NA), ADJINC_r=ADJINC*.000001, PINCP_ADJ_r = PINCP_r * ADJINC_r, ref_year=case_when(ADJINC==1119794~2005, ADJINC==1080505~2006, ADJINC==1051849~2007, ADJINC==1014521~2008, ADJINC==999480~ 2009), nat=CIT==4, LANX_r=factor(LANX), ENG_r=factor(ENG), FER_yes=FER==2, SEX_female=SEX==2, YOEP_r=replace(YOEP, YOEP=="bbbb", NA), yse=ref_year-YOEP_r, entry_period=case_when(YOEP<1992~"pre_war", YOEP %in% 1992:1995~"war", YOEP %in% 1996:1999~"immediate_post_war", YOEP>1999~"post_war"), working_age=AGEP%in%15:64, working_age_income=if_else(working_age, PINCP_ADJ_r, NULL)) # %>% left_join(select(p2010_2_cbsa2010, stint, puma2kint, cbsa10, cbsaname10), by=c("ST"="stint", "PUMA"="puma2kint")) %>% left_join(select(p2010_2_cbsa2010, stint, puma2kint, mig_cbsa=cbsa10, mig_cbsa_name=cbsaname10), by=c("MIGSP"="stint", "MIGPUMA"="puma2kint"))

moves_from = D %>% filter(MIG==3) %>% group_by(MIGSP, MIGPUMA) %>% select(PWGTP) %>% summarise(N=sum(PWGTP), n=n()) %>% arrange(desc(N))

moves_to = D %>% filter(MIG==3) %>% group_by(ST, PUMA) %>% select(PWGTP) %>% summarise(N=sum(PWGTP), n=n()) %>% arrange(desc(N))


D16 = as.tibble(readRDS(file="~/research/acs/data/ss16pus_POBP_bos.Rds")) %>% mutate(PINCP_r=replace(PINCP, PINCP == "bbbbbbb", NA), ADJINC_r=ADJINC*.000001, PINCP_ADJ_r = PINCP_r * ADJINC_r, ref_year=case_when(ADJINC==1119794~2005, ADJINC==1080505~2006, ADJINC==1051849~2007, ADJINC==1014521~2008, ADJINC==999480~ 2009), nat=CIT==4, LANX_r=factor(LANX), ENG_r=factor(ENG), FER_yes=FER==2, SEX_female=SEX==2, YOEP_r=replace(YOEP, YOEP=="bbbb", NA), yse=ref_year-YOEP_r, entry_period=case_when(YOEP<1992~"pre_war", YOEP %in% 1992:1995~"war", YOEP %in% 1996:1999~"immediate_post_war", YOEP>1999~"post_war"), working_age=AGEP%in%15:64, working_age_income=if_else(working_age, PINCP_ADJ_r, NULL)) 

moves_from16 = D16 %>% filter(MIG==3) %>% group_by(MIGSP, MIGPUMA) %>% select(PWGTP) %>% summarise(N=sum(PWGTP), n=n()) %>% arrange(desc(N))

moves_to16 = D16 %>% filter(MIG==3) %>% group_by(ST, PUMA) %>% select(PWGTP) %>% summarise(N=sum(PWGTP), n=n()) %>% arrange(desc(N))



repcols = grep("PWGTP", names(D))
i=1
D %>% group_by(ST, county, cntyname) %>% select(repcols[i], working_age_income, yse, SEX_female, AGEP, nat, FER_yes) %>% summarise(N=sum(repcols[i]), mean_working_age_income=weighted.mean(working_age_income, weights=repcols[i], na.rm=TRUE), mean_yse=weighted.mean(yse, weights=repcols[i], na.rm=TRUE), pFemale=weighted.mean(SEX_female, weights=repcols[i], na.rm=TRUE), mean_age=weighted.mean(AGEP, weights=repcols[i], na.rm=TRUE), pnat=weighted.mean(nat, weights=repcols[i], na.rm=TRUE), pFER_yes=weighted.mean(FER_yes, weights=repcols[i], na.rm=TRUE)) %>% arrange(desc(N))

D %>% select(c("ST", "county_r", "cntyname_r"))

make_se = function(D, x, groups, rep_root="PWGTP"){
  D %>% group_by(groups) %>% select(grep(rep_root, names(.))) %>% summarise_all(sum)
}


cnty_ests = D %>% group_by(ST, county_r, cntyname_r) %>% select(PWGTP, working_age_income) %>% summarise(N=sum(PWGTP), med_working_age_income=median(working_age_income*PWGTP, na.rm=TRUE))
cnty_reps = D %>% group_by(ST, county_r, cntyname_r) %>% select(grep("PWGTP", names(.))) %>% summarise_all(sum)
ses = sqrt((4/80)*rowSums((cnty_ests %>% pull(N) - (cnty_reps %>% ungroup() %>% select(grep("PWGTP", names(.))) %>% select(2:81)))^2))
cnty_ests$se = ses
cnty_ests = cnty_ests %>% mutate(ci90ll = (N - 1.645*se), ci90ul = (N + 1.645*se), ci90ll = case_when(ci90ll<0~0, ci90ll>=0~ci90ll)) %>% arrange(desc(N))




cnty_ests16 = D16 %>% group_by(ST, county_r, cntyname_r) %>% select(PWGTP) %>% summarise(N=sum(PWGTP))
cnty_reps16 = D16 %>% group_by(ST, county_r, cntyname_r) %>% select(grep("PWGTP", names(.))) %>% summarise_all(sum)
ses = sqrt((4/80)*rowSums((cnty_ests16 %>% pull(N) - (cnty_reps16 %>% ungroup() %>% select(grep("PWGTP", names(.))) %>% select(2:81)))^2))
cnty_ests16$se = ses
cnty_ests16 = cnty_ests16 %>% mutate(ci90ll = (N - 1.645*se), ci90ul = (N + 1.645*se), ci90ll = case_when(ci90ll<0~0, ci90ll>=0~ci90ll)) %>% arrange(desc(N))

cnty_ests %>% full_join(cnty_ests16, by=c("ST", "county_r", "cntyname_r"), suffix=c("2009", "2016"))






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
