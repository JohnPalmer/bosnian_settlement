## Data prep done on server with 60 GB of RAM ##
rm(list=ls())
library(data.table)
library(Hmisc)

### 2005-2009

chunks_suffixes = c("a", "b", "c", "d")

D = rbindlist(lapply(chunks_suffixes, function(s) fread(paste0("~/research/acs/untracked_data/2005-2009/ss09pus", s, ".csv"))))

Dh = rbindlist(lapply(chunks_suffixes, function(s) fread(paste0("~/research/acs/untracked_data/2005-2009/ss09hus", s, ".csv"))))

D_bos = D[POBP=="150"]

saveRDS(D_bos, file="~/research/acs/data/ss09pus_POBP_bos.Rds")

Dh_bos = Dh[serialno %in% D_bos$serialno]

saveRDS(Dh_bos, file="~/research/acs/data/ss09hus_POBP_bos.Rds")

### 2012-2016

chunks_suffixes = c("a", "b", "c", "d")

D = rbindlist(lapply(chunks_suffixes, function(s) fread(paste0("~/research/acs/untracked_data/2012-2016/ss16pus", s, ".csv"))))

Dh = rbindlist(lapply(chunks_suffixes, function(s) fread(paste0("~/research/acs/untracked_data/2012-2016/ss16hus", s, ".csv"))))

D_bos = D[POBP=="150"]

saveRDS(D_bos, file="~/research/acs/data/ss16pus_POBP_bos.Rds")

Dh_bos = Dh[SERIALNO %in% D_bos$SERIALNO]

saveRDS(Dh_bos, file="~/research/acs/data/ss16hus_POBP_bos.Rds")



(bos_pop_est = D_bos[, sum(PWGTP)])

bos_pop_se = sqrt(sum((D_bos[, sum(PWGTP)] - as.numeric(D_bos[,lapply(.SD, sum), .SDcols=(grep("PWGTP", names(D_bos)))][,2:80]))^2)*4/80)

(bos_pop_ci95 = bos_pop_est + 1.96*bos_pop_se*c(-1,1))

D_bos_origin = D[POBP=="150" | ANC1P=="177" | ANC2P=="177" | LANP=="1276"]

(bos_pop_origin_est = D_bos_origin[, sum(PWGTP)])

bos_pop_origin_se = sqrt(sum((D_bos_origin[, sum(PWGTP)] - as.numeric(D_bos_origin[,lapply(.SD, sum), .SDcols=(grep("PWGTP", names(D_bos_origin)))][,2:80]))^2)*4/80)

(bos_pop_origin_ci95 = bos_pop_origin_est + 1.96*bos_pop_origin_se*c(-1,1))

lang_yugo = as.character(c(1274:1278))
anc_yugo = as.character(c(109, 130,131, 152, 154, 176, 177))
pob_yugo = as.character(147, 150:154, 168)

D_yugo_origin = D[POBP %in% pob_yugo | ANC1P %in% anc_yugo | ANC2P %in% anc_yugo | LANP %in% lang_yugo]

(yugo_pop_origin_est = D_yugo_origin[, sum(PWGTP)])

yugo_pop_origin_se = sqrt(sum((D_yugo_origin[, sum(PWGTP)] - as.numeric(D_yugo_origin[,lapply(.SD, sum), .SDcols=(grep("PWGTP", names(D_yugo_origin)))][,2:80]))^2)*4/80)

(yugo_pop_origin_ci95 = yugo_pop_origin_est + 1.96*yugo_pop_origin_se*c(-1,1))
