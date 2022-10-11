library(tidyverse)
library(plyr)


#read in cover data
full_cover <- read.csv("full_cover_test.csv")

full_cover_v2 <- full_cover %>%
  
 pivot_longer(cols = c(local_provenance, local_lifeform, local_lifespan, functional_group, N_fixer, ps_path), 
        names_to = "traits", values_to = "trait_values") %>%
  mutate(trait_values = case_when(
    nchar(trait_values) == 0 | is.na(trait_values) ~ "NULL",
    trait_values %in% c("ANNUAL GRASS","Grass") ~ "GRASS",
    trait_values %in% c("Forb","NON-LEGUMINOUS FORB", "PERENNIAL FORB") ~ "FORB",
    trait_values %in% c("Graminoid", "SEDGE") ~ "GRAMINOID",
    trait_values == "HERBS" ~ "HERB",
    trait_values == "INDETERMINATE" ~ "NULL",
    trait_values == "Introduced" ~ "INT",
    trait_values == "LEGUME FORB" ~ "LEGUME",
    trait_values %in% c("n","Native") ~ "NAT",
    trait_values %in% c("Shrubs", "SHURB") ~ "SHRUB",
    trait_values == "SUB-SHRUB" ~ "SUBSHRUB",
    trait_values %in% c("unknown", "UNKNOWN") ~ "UNK",
    trait_values == "C3-C4 Intermediate" ~ "C3-C4 INTERMEDIATE",
    Taxon == "UNKNOWN SP.6(OKLAH.US)" & traits == "functional_group" ~ "GRASS",
    TRUE ~ trait_values
  )) %>%
  unique() %>%
 group_by(site_name, site_code, block, plot, subplot, year, first_treatment_year, first_treatment_date, cover_date, n_treat_days,
          n_treat_years, trt, Family, Taxon, live, max_cover, traits) %>%
  mutate(obs_count = n()) %>%
  ungroup() %>%
  mutate(drop_me = ifelse(test = trait_values %in% c("UNKNOWN","NULL") & obs_count > 1,
         yes = "drop me",
         no = "keep")) %>%
  filter(drop_me == "keep") %>%
  select(-obs_count, -drop_me) %>%
  group_by(site_name, site_code, block, plot, subplot, year, first_treatment_year, first_treatment_date, cover_date, n_treat_days,
           n_treat_years, trt, Family, Taxon, live, max_cover, traits) %>%
  mutate(obs_count = n()) # should have only 1
  
full_cover_v2 %>% as.data.frame() %>% filter(obs_count > 1) %>% select(traits) %>% unique()

unique(full_cover_v2$obs_count)
glimpse(full_cover_v2)








#treatment_info <- read.csv("C:/Users/ohler/Downloads/full_biomass_test.csv")
#treatment_info <- treatment_info[, c("site_code", "year", "n_treat_days", "block", "plot", "subplot")]
#treatment_info <- unique(treatment_info)

#full_biomass <- read.csv("C:/Users/ohler/Dropbox/IDE MS_Single year extreme/Data/full_biomass_test.csv")
#details <- full_biomass[, c("site_code", block, plot, subplot, year, )]

#read in precip data
ppt.1 <- read.csv("anpp_clean_trt_ppt_no-perc_365-0days_2022-10-07.csv")

#reduce column names to minimum
ppt.1$ppt.1 <- ppt.1$ppt#change precip column names in lag files to reflect lags
ppt.1 <- ddply(ppt.1, c("site_code", "year", "trt"), 
        function(x)data.frame(
         ppt.1 = mean(x$ppt)
        ))


#read in precip data
ppt.2 <- read.csv("anpp_clean_trt_ppt_no-perc_730-365days_2022-10-07.csv")

#reduce column names to minimum
ppt.2$ppt.2 <- ppt.2$ppt#change precip column names in lag files to reflect lags
ppt.2 <- ddply(ppt.2, c("site_code", "year", "trt"), 
        function(x)data.frame(
         ppt.2 = mean(x$ppt)
        ))

#read in precip data
ppt.3 <- read.csv("anpp_clean_trt_ppt_no-perc_1095-730days_2022-10-07.csv")

#reduce column names to minimum
ppt.3$ppt.3 <- ppt.3$ppt#change precip column names in lag files to reflect lags
ppt.3 <- ddply(ppt.3, c("site_code", "year", "trt"), 
        function(x)data.frame(
         ppt.3 = mean(x$ppt)
        ))

#read in precip data
ppt.4 <- read.csv("anpp_clean_trt_ppt_no-perc_1460-1095days_2022-10-07.csv")

#reduce column names to minimum
ppt.4$ppt.4 <- ppt.4$ppt#change precip column names in lag files to reflect lags
ppt.4 <- ddply(ppt.4, c("site_code", "year", "trt"), 
        function(x)data.frame(
         ppt.4 = mean(x$ppt)
        ))

#merge all the precip-lag years
full_ppt <- merge(ppt.1, ppt.2, by = c("site_code", "year", "trt"), all.x = TRUE)%>%
 unique()%>%
 merge(ppt.3, by = c("site_code", "year", "trt"), all.x = TRUE)%>%
 unique()%>%
 merge(ppt.4, by = c("site_code", "year", "trt"), all.x = TRUE)%>%
 unique()




cover_ppt_all_x <- merge(full_cover, full_ppt, by = c("site_code", "year", "trt"), all.x = TRUE)%>%
 subset(live == 1)

cover_ppt_no_x <- merge(full_cover, full_ppt, by = c("site_code", "year", "trt"), all.x = FALSE)%>%
 subset(live == 1)

cover_ppt_left_join <- left_join(full_cover, full_ppt, by = c("site_code", "year", "trt"))






#read worldclim data
worldclim <- read.csv("worldclim_map.csv")

cover_ppt_map <- merge(cover_ppt, worldclim, by = "site_code", all.x = TRUE)


#specify n_trt_years with n_treat_days
cover_ppt_map <- cover_ppt_map[-c(12)]#remove old column which isn't trustworthy
cover_ppt_map$n_treat_days <- as.numeric(cover_ppt_map$n_treat_days)
cover_ppt_map$n_treat_years <- ifelse(cover_ppt_map$n_treat_days <= 50, 0, 
                   ifelse(cover_ppt_map$n_treat_days > 50 & cover_ppt_map$n_treat_days < 415, 1, 
                       ifelse(cover_ppt_map$n_treat_days >= 415 & cover_ppt_map$n_treat_days < 780, 2, 
                          ifelse(cover_ppt_map$n_treat_days >= 780 & cover_ppt_map$n_treat_days < 1145, 3, 
                              ifelse(cover_ppt_map$n_treat_days >= 1145 & cover_ppt_map$n_treat_days < 1510, 4, 
                                 ifelse(cover_ppt_map$n_treat_days >= 1510 & cover_ppt_map$n_treat_days < 1875, 5, 
                                     ifelse(cover_ppt_map$n_treat_days >= 1875 & cover_ppt_map$n_treat_days <2240, 6, NA )))))))

temp <- cover_ppt_map[, c("site_code", "n_treat_years")]
temp <- unique(temp)

hist(subset(temp, n_treat_years != 0)$n_treat_years)
























#######################################################
##############################################
###############################################
#read in precip data
#trt_ppt <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/data/precip/anpp_clean_trt_ppt_no-perc_365-0days_2022-10-07.csv")


#summarize precip data because of multple biomass dates?
#trt_ppt_summary <- trt_ppt[, c("site_code", "year", "trt", "ppt")]
#trt_ppt_summary <- ddply(trt_ppt_summary, c("site_code", "year", "trt"), 
#             function(x)data.frame(
#              ppt = mean(x$ppt)
#             ))

#merge cover and precipitation
#cover_ppt <- merge(full_cover, trt_ppt_summary, by = c("site_code", "year", "trt"), all.x = TRUE)

live_cover_ppt <- subset(cover_ppt, live == 1)




