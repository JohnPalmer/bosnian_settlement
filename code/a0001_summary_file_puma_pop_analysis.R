# !diagnostics off
rm(list=ls())

library(data.table)
library(tidyverse)


E2009 = as.tibble(readRDS(file="~/research/acs/data/summary_file_pumas_bosnians_median_income.Rds"))

E2016 = as.tibble(readRDS(file="~/research/acs/data/summary_file_pumas_bosnians_median_income_2016.Rds")) 

