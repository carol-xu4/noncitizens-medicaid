## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory 
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/noncitzens medicaid")

# CPS ASEC data --------------------------------------------------------------
ddi_cps = read_ipums_ddi("data/input/cps_00008.xml")  
cps = read_ipums_micro(ddi_cps)

cps = cps %>% rename_with(tolower)

cps = cps %>%
  select(
    year, serial, asecwth, pernum, asecwt, citizen, bpl, himcaidnw, gq)

#  remove GQ
cps = cps %>%
  filter(gq == 1)

# recode citizen to binary
# 1 = Born in U.S., 2 = Born in U.S. outlying, 3 = Born abroad of American
# parents, 4 = Naturalized citizen -> citizen; 5 = Not a citizen -> noncitizen;
# 9 = NIU -> NA
cps = cps %>%
  mutate(citizen2 = case_when(
    citizen %in% c(1, 2, 3, 4) ~ 0,
    citizen == 5                ~ 1,
    citizen == 9                ~ NA_real_))

# recode medicaid
cps = cps %>%
  mutate(medicaid = himcaidnw == 2)

# save RDS
write_rds(cps, "data/output/cps_asec.rds")
