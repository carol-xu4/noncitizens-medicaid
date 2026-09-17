## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory 
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/noncitzens medicaid")

# ACS data -----------------------------------------------------------------
ddi_acs = read_ipums_ddi("data/input/usa_00033.xml")
acs = read_ipums_micro(ddi_acs)

acs = acs %>% rename_with(tolower)

acs = acs %>%
  select(
    year, serial, hhwt, pernum, perwt, citizen, bpl, hinscaid, gq)

# households only -- take out group quarters
acs = acs %>%
    filter(gq %in% c(1, 2, 5))

# recode citizens/noncitizens binary, using BPL first then CITIZEN
acs = acs %>%
  mutate(citizen2 = case_when(
    bpl <= 120           ~ 0,   # native-born -> citizen 
    citizen %in% c(1, 2) ~ 0,   # foreign-born, naturalized or born abroad of US parents -> citizen
    citizen == 3         ~ 1,   # foreign-born, not a citizen
    TRUE                 ~ NA_real_  ))

# save RDS
write_rds(acs, "data/output/acs.rds")
