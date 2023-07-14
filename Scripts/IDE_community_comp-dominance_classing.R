## ------------------------------------------ ##
# IDE - Dominant Species Classification
## ------------------------------------------ ##
# Contributors: Meghan Hayden

## Purpose:
# Categorize species as dominant, co-dominant, or subdominant, based on relative cover at 
# the site level in the pre-treatment year (Year 0).

## Set-up -----------

# Clear environment
rm(list = ls())

# Set WD
user <- "Meghan"
if (user == "Meghan"){
  setwd("~/Library/CloudStorage/OneDrive-UCB-O365/UCB/Desktop/lterwg-droughtnet")
}

# Load libraries
library(data.table)
library(ggplot2)
library(dplyr)
library(broom.mixed)
library(lme4)
library(tidyr)


# Load data
## Here, load in the pre-processed cover data for all years
cover <- fread("cover_ppt_2023-05-10.csv") # from DropBox on 7/6/23

## Calculate -----------

# First, average relative cover in year 0 drought plots for each spp for each site
# Then sort by relative cover and assign rank
# Then do cumulative sums
# Then assign cut-off at 50% and mark that rank as number of co-doms in site

# At the plot level
#cover_yr0 <- cover[n_treat_years == 0,] # 
#cover_yr0 <- cover_yr0[relative_absolute == "absolute", ] # because the relative cover sites were messing it up
#cover_yr0_sum <- cover_yr0[,totplotcover.yr.live := sum(max_cover, na.rm= T), by=.(site_code, block, plot, subplot)]
#cover_yr0_rel <- cover_yr0_sum[,relative_sp_cover.yr.live := max_cover/totplotcover.yr.live]
#cover_yr0_rel <- cover_yr0_sum[,spp_mean_rel := mean(relative_sp_cover.yr.live, na.rm = T), by =. (site_code, Taxon)]
#cover_yr0_rel <- cover_yr0_rel %>%
#  group_by(site_code) %>%
#  distinct(Taxon, .keep_all = TRUE) %>%
#  mutate(ranks = dense_rank(-spp_mean_rel)) %>% # assigns ties to same rank
#  group_by(site_code) %>%
#  arrange(ranks) %>%
#  mutate(cumsum = cumsum(spp_mean_rel))
# find minimum number of species that is over 50%
#site_no_doms <- cover_yr0_rel %>%
#  group_by(site_code) %>%
#  filter(cumsum >= 0.50) %>% # minimum value greater than 0.5
#  group_by(site_code) %>%
#  slice(which.min(cumsum))
#site_no_doms <- site_no_doms[, c(2,35)]


# At the site level - HOW I'M DOING IT NOW**
cover_yr0_sum_site <- cover_yr0[,totsitecover.yr.live := sum(max_cover, na.rm= T), by=.(site_code)]
cover_yr0_sum_site <- cover_yr0_sum_site[,totspcover.yr.live := sum(max_cover, na.rm= T), by=.(Taxon, site_code)]
cover_yr0_rel_site <- cover_yr0_sum_site[,relative_sp_cover.yr.live := totspcover.yr.live/totsitecover.yr.live]
cover_yr0_rel_site <- cover_yr0_rel_site %>%
  group_by(site_code) %>%
  distinct(Taxon, .keep_all = TRUE) %>%
  mutate(ranks = dense_rank(-relative_sp_cover.yr.live)) %>% # assigns ties to same rank
  group_by(site_code) %>%
  arrange(ranks) %>%
  mutate(cumsum = cumsum(relative_sp_cover.yr.live))
# find minimum number of species that is over 50%
site_no_doms <- cover_yr0_rel_site %>%
  group_by(site_code) %>%
  filter(cumsum >= 0.50) %>% # minimum value greater than 0.5
  group_by(site_code) %>%
  slice(which.min(cumsum))
site_no_doms <- site_no_doms[, c(2,35)]
# sites have between 1-7co-dominant species
# mean(drought) - mean(control)/ sd(control) - site level mean
# great by treatment dom. and non-dom. 
# column for treatment and column for control 

# merge number of site dominants back into the full data
cover_rel_ranks <- right_join(site_no_doms, cover_yr0_rel_site, by = "site_code")
codom_spp_site <- cover_rel_ranks %>%
  mutate(dominance = if_else(ranks.y > ranks.x, "SD", if_else(ranks.y == 1, "D", "CD")), .keep = "all") #%>%

# write.csv(codom_spp_site, "codominant_spp_each_site.csv")

codom_spp_list <- codom_spp_site[, c("site_code", "Taxon","relative_sp_cover.yr.live","dominance")]
#write.csv(codom_spp_list, "codominant_spp_list_yr0.csv")
