# Purpose: compile data for Anping's remote sensing analysis that uses year 1 ANPP
# data

# Script started May 8, 2023

# Author: Martin Holdrege


# dependencies ------------------------------------------------------------

# change path to dropbox in that script as needed
# creates 'path' object
source("Scripts/dropbox_path.R")
library(tidyverse)


# read in data ------------------------------------------------------------


# * precip ------------------------------------------------------------------

# annual ppt
ann1 <- read_csv(file.path(path, "IDE/data_processed/anpp_clean_trt_ppt_no-perc_365-0days_2023-02-06.csv"), 
                 show_col_types = FALSE)

# mean annual ppt
map1 <- read_csv(file.path(path, "IDE/data_processed/climate/climate_mean_annual_by_site.csv"),
         show_col_types = FALSE)

# * biomass -----------------------------------------------------------------

anpp1 <- read_csv(file.path(path, "IDE/data_processed/anpp_ppt_2023-05-01.csv"),
                  show_col_types = FALSE)


# * sites -----------------------------------------------------------------

sites1 <- read_csv(file.path(path, "IDE MS_Single year extreme/Data/msyr1_sites.csv"),
                   show_col_types = FALSE)


# process  ----------------------------------------------------------------

# * MAP -------------------------------------------------------------------
# here just interested in using the mswep data
map2 <- map1 %>% 
  filter(data_source == "mswep") %>% 
  select(site_code, MAP)


# * annual ppt ------------------------------------------------------------

ann2 <- ann1 %>% 
  # selecting only the sites in Kate's year 1 ms
  filter(site_code %in% sites1$site_code) %>% 
  group_by(site_code, year, trt) %>% 
  filter(biomass_date == max(biomass_date)) %>% 
  select(site_code, year, trt, ppt_mswep) %>% 
  distinct() %>% 
  pivot_wider(values_from = 'ppt_mswep',
              names_from = 'trt',
              names_prefix = "ppt_") %>% 
  left_join(map2, by = 'site_code') %>% 
  mutate(extreme_drought = ppt_Control < MAP) %>% 
  select(site_code, year, extreme_drought) %>% 
  group_by(site_code, year) %>% 
  # b/ can have multiple n_treat_days (b/ multiple start dates) in rare
  # cases, summarize here
  summarize(extreme_drought = median(extreme_drought),
            .groups = 'drop')

# *biomass ----------------------------------------------------------------

# long term average biomass in control plots
ctrl1 <- anpp1 %>% 
  filter(trt == 'Control',
         !is.na(mass)) %>% 
  group_by(site_code) %>% 
  summarize(n = length(unique(year)),
            mass = mean(mass)) %>% 
  # only want sites with at least 4 available years of control data 
  filter(n >= 4) %>% 
  rename(multi_yr_mean_ctrl_anpp =  mass) %>% 
  select(-n)

# biomass in drought plots in year 1
drt1 <- anpp1 %>% 
  filter(site_code %in% sites1$site_code,
         trt == 'Drought',
         # selecting year 1 data, as per Kate's manuscript
         n_treat_days >= 113 & n_treat_days <= 657) %>% 
  group_by(site_code) %>% 
  # getting first year's data within that n_treat_days window
  filter(year == min(year)) %>% 
  group_by(site_code, year) %>%  # need year for later joining
  summarize(mean_drt_anpp = mean(mass),
            .groups = 'drop')

sum(is.na(ann2[, c("site_code", "year", "extreme_drought")]))
ann2 %>% 
  filter(is.na(extreme_drought))

# combining datasets ------------------------------------------------------

comb1 <- drt1 %>% 
  inner_join(ctrl1, by = join_by("site_code")) %>% 
  left_join(ann2, by = c("site_code", "year")) %>% 
  select(-year)

# each site should only appear once
stopifnot(!duplicated(comb1$site_code),
          !is.na(comb1))
 

# write output ------------------------------------------------------------

write_csv(comb1, file.path(path, "IDE/data_processed/anpp_extremeness_for_Anping_2023-05-10.csv"))


