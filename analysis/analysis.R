## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory 
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/noncitzens medicaid")

# CPS ASEC  data -------------------------------------------------------------
cps = readRDS("data/output/cps_asec.rds")

# total households
total_households = cps %>%
  filter(pernum == 1) %>%         # 1 row per hh
  group_by(year) %>%
  summarize(
    n = n(),
    households_wt  = sum(asecwth),
    .groups = "drop")

print(total_households)

write_csv(total_households, "results/cps_total_households.csv")

# total households with a noncitizen
noncitizen_households = cps %>%
    group_by(year, serial) %>%
    summarize(
    asecwth        = first(asecwth),
    any_noncitizen = any(citizen2 == 1, na.rm = TRUE),
    .groups = "drop") %>%
  filter(any_noncitizen) %>%
  group_by(year) %>%
  summarize(
    n = n(),
    households_wt = sum(asecwth),
    .groups = "drop")

print(noncitizen_households)

write_csv(noncitizen_households, "results/cps_noncitizen_households.csv")

# total noncitizens
noncitizens = cps %>%
    filter(!is.na(citizen2)) %>%
    group_by(year, citizen2) %>%
    summarize(n = n(),
        population = sum(asecwt), .groups = "drop") %>%
    print(n = Inf)

write_csv(noncitizens, "results/cps_noncitizens.csv")

# total noncitizens on medicaid
noncitizens_medicaid = cps %>%
  filter(!is.na(citizen2), citizen2 == 1) %>%
  group_by(year, caidnw) %>%
  summarize(n = n(),
    population = sum(asecwt), .groups = "drop") %>%
  print(n = Inf)

write_csv(noncitizens_medicaid, "results/cps_noncitizens_medicaid.csv")

# households with >=1 noncitizen and >=1 person on medicaid
household_flags = cps %>%
  group_by(year, serial) %>%
  summarize(
    asecwth        = first(asecwth),
    any_noncitizen = any(citizen2 == 1, na.rm = TRUE),
    any_medicaid   = any(caidnw == 2, na.rm = TRUE),
    .groups = "drop")

noncitizen_medicaid_households = household_flags %>%
  filter(any_noncitizen, any_medicaid) %>%
  group_by(year) %>%
  summarize(
    n = n(),
    households_wt = sum(asecwth),
    .groups = "drop")

print(noncitizen_medicaid_households)

write_csv(noncitizen_medicaid_households, "results/cps_noncitizen_medicaid_households.csv")

# households with at least one noncitizen who is on medicaid
household_flags = household_flags %>%
  left_join(
    cps %>%
      group_by(year, serial) %>%
      summarize(
        any_noncitizen_medicaid = any(citizen2 == 1 & caidnw == 2, na.rm = TRUE),
        .groups = "drop"),
    by = c("year", "serial")
  )

noncitizen_on_medicaid_households = household_flags %>%
  filter(any_noncitizen_medicaid) %>%
  group_by(year) %>%
  summarize(
    n = n(),
    households_wt = sum(asecwth),
    .groups = "drop")

print(noncitizen_on_medicaid_households)

write_csv(noncitizen_on_medicaid_households, "results/cps_noncitizen_on_medicaid_households.csv")

# total people on medicaid
cps %>% group_by(year, caidnw) %>%
    summarize(n = n(),
    population = sum(asecwt), .groups = "drop") %>%
    print(n = Inf)

# people on medicaid who are in a household with >=1 noncitizen
medicaid_people_in_noncitizen_hh = cps %>%
  left_join(
    household_flags %>% select(year, serial, any_noncitizen),
    by = c("year", "serial")
  ) %>%
  filter(caidnw == 2, any_noncitizen) %>%
  group_by(year) %>%
  summarize(
    n = n(),
    population = sum(asecwt),
    .groups = "drop")

print(medicaid_people_in_noncitizen_hh)

write_csv(medicaid_people_in_noncitizen_hh, "results/cps_medicaid_people_in_noncitizen_hh.csv")

# smell check: people on medicaid who are in households where everyone is a citizen
medicaid_people_in_citizen_hh = cps %>%
  left_join(
    household_flags %>% select(year, serial, any_noncitizen),
    by = c("year", "serial")
  ) %>%
  filter(caidnw == 2, !any_noncitizen) %>%
  group_by(year) %>%
  summarize(
    n = n(),
    population = sum(asecwt),
    .groups = "drop")

print(medicaid_people_in_citizen_hh)

write_csv(medicaid_people_in_citizen_hh, "results/cps_medicaid_people_in_citizen_hh.csv")