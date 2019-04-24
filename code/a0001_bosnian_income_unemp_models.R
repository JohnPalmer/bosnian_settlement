library(data.table)
library(tidyverse)
library(rstanarm)

D = as.tibble(readRDS(file="~/research/acs/data/ss09pus_POBP_bos.Rds")) %>% mutate(PINCP_r=replace(PINCP, PINCP == "bbbbbbb", NA), ADJINC_r=ADJINC*.000001, PINCP_ADJ_r = PINCP_r * ADJINC_r, ref_year=case_when(ADJINC==1119794~2005, ADJINC==1080505~2006, ADJINC==1051849~2007, ADJINC==1014521~2008, ADJINC==999480~ 2009), nat=CIT==4, LANX_r=factor(LANX), ENG_r=factor(ENG), FER_yes=FER==2, SEX_female=SEX==2, YOEP_r=replace(YOEP, YOEP=="bbbb", NA), yse=ref_year-YOEP_r, entry_period=case_when(YOEP<1992~"pre_war", YOEP %in% 1992:1995~"war", YOEP %in% 1996:1999~"immediate_post_war", YOEP>1999~"post_war")) %>% filter(!is.na(PINCP_r) & AGEP<65) 

summary(D$CIT_r)

M = lm(PINCP_ADJ_r~SEX_female+AGEP+nat+yse, data=D)

M = stan_lmer(PINCP_ADJ_r~SEX_female+AGEP+nat+yse + (1 | serialno) + (1 | PUMA) + (1 | ST) , data=D)

summary(M, pars = c("SEX_femaleTRUE", "AGEP", "natTRUE", "yse"))


D16 = as.tibble(readRDS(file="~/research/acs/data/ss16pus_POBP_bos.Rds")) %>% mutate(PINCP_r=replace(PINCP, PINCP == "bbbbbbb", NA), ADJINC_r=ADJINC*.000001, PINCP_ADJ_r = PINCP_r * ADJINC_r, ref_year=case_when(ADJINC==1119794~2005, ADJINC==1080505~2006, ADJINC==1051849~2007, ADJINC==1014521~2008, ADJINC==999480~ 2009), nat=CIT==4, LANX_r=factor(LANX), ENG_r=factor(ENG), FER_yes=FER==2, SEX_female=SEX==2, YOEP_r=replace(YOEP, YOEP=="bbbb", NA), yse=ref_year-YOEP_r, entry_period=case_when(YOEP<1992~"pre_war", YOEP %in% 1992:1995~"war", YOEP %in% 1996:1999~"immediate_post_war", YOEP>1999~"post_war")) %>% filter(!is.na(PINCP_r) & AGEP<65) 
