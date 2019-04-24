rm(list=ls())
# done on cluster-ceab, where the untracked files are located.
library(data.table)

stateTLs <- c("al","ak","az","ar","ca","co","ct","de","dc","fl","ga","hi","id","il","in","ia","ks","ky","la","me","md","ma","mi","mn","ms","mo","mt","ne","nv","nh","nj","nm","ny","nc","nd","oh","ok","or","pa","ri","sc","sd","tn","tx","ut","vt","va","wa","wv","wi","wy")

G2009 = rbindlist(lapply(stateTLs, function(s) as.data.table(read.fwf(paste0("~/research/acs/untracked_data/2005-2009/Tracks_Block_Groups/g20095", s, ".txt"),  widths=c(6,2,3,2, 7, -1,-1,-1,2,2,3,5,5, 6, -172, 200, -50), col.names=c("FILEID", "STUSAB", "SUMLEVEL", "COMPONENT", "LOGRECNO", "STATECE", "STATE", "COUNTY", "COUSUB", "PLACE", "TRACT", "Name"), colClasses="character", header = FALSE, fileEncoding="ISO-8859-1"))))
               
E2009 = rbindlist(lapply(stateTLs, function(s) fread(paste0("~/research/acs/untracked_data/2005-2009/Tracks_Block_Groups/e20095", s, "0018000.txt"), na.strings = c(".", " ", "NA", "N/A"), colClasses = c(rep("NULL", 2), "character", rep("NULL", 2), "character", rep("NULL", 100), "numeric", rep("NULL", 119)), col.names = c("STUSAB", "LOGRECNO", "N_Bos"))))

E2009_inc = rbindlist(lapply(stateTLs, function(s) fread(paste0("~/research/acs/untracked_data/2005-2009/Tracks_Block_Groups/e20095", s, "0053000.txt"), na.strings = c(".", " ", "NA", "N/A"), colClasses = c(rep("NULL", 2), "character", rep("NULL", 2), "character", rep("NULL", 170), "numeric", rep("NULL", 27)), col.names=c("STUSAB", "LOGRECNO", "median_income")))) # table with median income

E2009 = merge(E2009, E2009_inc, all.x=TRUE, all.y=FALSE)

E2009[, STUSAB:=toupper(STUSAB)]
E2009 = merge(E2009, G2009, all.x=TRUE, all.y=FALSE)

E2009 = E2009[SUMLEVEL=="140"] # this is all of them anyway, but just to check. Note that E2009_inc has more rows because it also has block groups. E2009 has only tracts

M2009 = rbindlist(lapply(stateTLs, function(s) fread(paste0("~/research/acs/untracked_data/2005-2009/Tracks_Block_Groups/m20095", s, "0018000.txt"), na.strings = c(".", " ", "NA", "N/A"), colClasses = c(rep("NULL", 2), "character", rep("NULL", 2), "character", rep("NULL", 100), "numeric", rep("NULL", 119)), col.names = c("STUSAB", "LOGRECNO", "N_Bos_ME90"))))

M2009_inc = rbindlist(lapply(stateTLs, function(s) fread(paste0("~/research/acs/untracked_data/2005-2009/Tracks_Block_Groups/m20095", s, "0053000.txt"), na.strings = c(".", " ", "NA", "N/A"), colClasses = c(rep("NULL", 2), "character", rep("NULL", 2), "character", rep("NULL", 170), "numeric", rep("NULL", 27)), col.names=c("STUSAB", "LOGRECNO", "median_income_ME90")))) # table with median income

M2009 = merge(M2009, M2009_inc, all.x=TRUE, all.y=FALSE)

M2009[, STUSAB:=toupper(STUSAB)]

E2009 = merge(E2009, M2009)

saveRDS(E2009, file="~/research/acs/data/summary_file_tracts_bosnians_median_income.Rds")


G2016 = rbindlist(lapply(stateTLs, function(s) fread(paste0("~/research/acs/data/ACS_2016_geography_files/g20165", s, ".csv"), colClasses=c(rep("character", 5), rep("NULL", 3), rep("character", 6), rep("NULL", 35), "character", rep("NULL", 3)), col.names=c("FILEID", "STUSAB", "SUMLEVEL", "COMPONENT", "LOGRECNO", "STATECE", "STATE", "COUNTY", "COUSUB", "PLACE", "TRACT", "Name"))))

E2016 = rbindlist(lapply(stateTLs, function(s) fread(paste0("~/research/acs/untracked_data/2012-2016/Tracts-BlockGroups/data/tab4/sumfile/prod/2012thru2016/group2/e20165", s, "0010000.txt"), na.strings = c(".", " ", "NA", "N/A"), colClasses = c(rep("NULL", 2), "character", rep("NULL", 2), "character", rep("NULL", 42), "numeric", rep("NULL", 165)), col.names = c("STUSAB", "LOGRECNO", "N_Bos"))))

E2016_inc = rbindlist(lapply(stateTLs, function(s) fread(paste0("~/research/acs/untracked_data/2012-2016/Tracts-BlockGroups/data/tab4/sumfile/prod/2012thru2016/group2/e20165", s, "0015000.txt"), na.strings = c(".", " ", "NA", "N/A"), colClasses = c(rep("NULL", 2), "character", rep("NULL", 2), "character", rep("NULL", 110), "numeric", rep("NULL", 49)), col.names = c("STUSAB", "LOGRECNO", "median_income"))))


M2016 = rbindlist(lapply(stateTLs, function(s) fread(paste0("~/research/acs/untracked_data/2012-2016/Tracts-BlockGroups/data/tab4/sumfile/prod/2012thru2016/group2/m20165", s, "0010000.txt"), na.strings = c(".", " ", "NA", "N/A"), colClasses = c(rep("NULL", 2), "character", rep("NULL", 2), "character", rep("NULL", 42), "numeric", rep("NULL", 165)), col.names = c("STUSAB", "LOGRECNO", "N_Bos_ME90"))))

M2016_inc = rbindlist(lapply(stateTLs, function(s) fread(paste0("~/research/acs/untracked_data/2012-2016/Tracts-BlockGroups/data/tab4/sumfile/prod/2012thru2016/group2/m20165", s, "0015000.txt"), na.strings = c(".", " ", "NA", "N/A"), colClasses = c(rep("NULL", 2), "character", rep("NULL", 2), "character", rep("NULL", 110), "numeric", rep("NULL", 49)), col.names = c("STUSAB", "LOGRECNO", "median_income_ME90"))))


E2016 = merge(E2016, E2016_inc, all.x=TRUE, all.y=FALSE)
E2016 = merge(E2016, M2016)
E2016 = merge(E2016, M2016_inc, all.x=TRUE, all.y=FALSE)
E2016[,  STUSAB:=toupper(STUSAB)]
E2016 = merge(E2016, G2016, all.x=TRUE, all.y=FALSE)
E2016 = E2016[SUMLEVEL==140]

saveRDS(E2016, file="~/research/acs/data/summary_file_tracts_bosnians_median_income_2016.Rds")

