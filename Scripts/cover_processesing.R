library(plyr)
library(tidyverse)

#read n_treat_years data
IDE_treatment_years<- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/IDE_treatment_years_11-17-2022.csv")

#read in cover data survey
cover_survey <- read.csv("C:/Users/ohler/Dropbox/IDE_data_May 2018/IDE Site Info/cover_survey_results.csv")

#read in cover data
full_cover <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/full_cover_11-17-2022.csv")%>%
              subset(live == 1)
full_cover$trt <- plyr::revalue(full_cover$trt, c("Control_Infrastructure"="Control"))


full_cover_v2 <- full_cover %>%
  # pivot longer to check duplicate values
 pivot_longer(cols = c(local_provenance, local_lifeform, local_lifespan, functional_group, N_fixer, ps_path), 
        names_to = "traits", values_to = "trait_values") %>%
  # consolidating trait values into fewer categories
  dplyr::mutate(trait_values = case_when(
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
    # fixing an error that someone made when entering the data
    Taxon == "UNKNOWN SP.6(OKLAH.US)" & traits == "functional_group" ~ "GRASS",
    TRUE ~ trait_values)) %>%
  # more fine-tuning
  dplyr::mutate(trait_values = case_when(
    Taxon %in% c("CUSCUTA PLANIFLORA") & traits == "local_lifeform" ~ "FORB",
    Taxon %in% c("PSORALIDIUM TENUIFLORUM") & traits == "local_lifeform" ~ "FORB",
    Taxon  == "UNKNOWN SP.6(OKLAH.US)" & traits == "local_lifeform" ~ "FORB",
    Taxon %in% c("MIMOSA NUTTALLII", "DALEA CANDIDA") & traits == "local_lifeform" ~ "LEGUME",
    Taxon %in% c("BRYOPHYTE SP.(LYGRAINT.NO)") & traits == "local_lifeform" ~ "BRYOPHYTE",
    trait_values == "HERB" & Family == "Fabaceae" ~ "LEGUME",
    trait_values == "HERB" & Family == "Poaceae" ~ "GRASS",
    trait_values == "HERB" & Family %in% c("Juncaceae", "Cyperaceae") ~ "GRAMINOID",
    trait_values == "HERB" & !Family %in% c("Fabaceae", "Poaceae", "Juncaceae","Cyperaceae") ~ "FORB",
    TRUE ~ trait_values)) %>%
  # dropping duplicates
  unique() %>%
  group_by(site_name, site_code, block, plot, subplot, year, first_treatment_year, first_treatment_date, cover_date, n_treat_days,
          n_treat_years, trt, Family, Taxon, live, max_cover, traits) %>%
  # counting how many different trait_values there are 
  dplyr::mutate(obs_count = n()) %>%
  ungroup() %>%
  # if trait_values is UNKNOWN or NULL and if obs_count is greater than 1, we drop that row
  dplyr::mutate(drop_me = ifelse(test = trait_values %in% c("UNKNOWN","NULL") & obs_count > 1,
         yes = "drop me",
         no = "keep")) %>%
  # filter to all the rows we want to keep
  filter(drop_me == "keep") %>%
  # we don't need obs_count and drop_me anymore
  dplyr::select(-obs_count, -drop_me) %>%
  # put it back to wide format
  pivot_wider(names_from = "traits", values_from = "trait_values") %>%
  # reorder columns
  dplyr::select(site_name, site_code, block, plot, subplot,              
        year, first_treatment_year, first_treatment_date, cover_date, n_treat_days,         
         n_treat_years, trt, Family, Taxon, live,           
         local_provenance, local_lifeform, local_lifespan, functional_group,     
         N_fixer, ps_path, max_cover)

# check to see it looks ok
glimpse(full_cover_v2)


#treatment_info <- read.csv("C:/Users/ohler/Downloads/full_biomass_test.csv")
#treatment_info <- treatment_info[, c("site_code", "year", "n_treat_days", "block", "plot", "subplot")]
#treatment_info <- unique(treatment_info)

#full_biomass <- read.csv("C:/Users/ohler/Dropbox/IDE MS_Single year extreme/Data/full_biomass_test.csv")
#details <- full_biomass[, c("site_code", block, plot, subplot, year, )]

#read in precip data
ppt.1 <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/data/precip/anpp_clean_trt_ppt_no-perc_365-0days_2023-01-02.csv")

#reduce column names to minimum
ppt.1$ppt.1 <- ppt.1$ppt#change precip column names in lag files to reflect lags
ppt.1 <- ddply(ppt.1, c("site_code", "year", "trt"), 
        function(x)data.frame(
         ppt.1 = x$ppt[x$biomass_date %in% max(x$biomass_date)]
        ))


#read in precip data
ppt.2 <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/data/precip/anpp_clean_trt_ppt_no-perc_730-365days_2023-01-02.csv")

#reduce column names to minimum
ppt.2$ppt.2 <- ppt.2$ppt#change precip column names in lag files to reflect lags
ppt.2 <- ddply(ppt.2, c("site_code", "year", "trt"), 
        function(x)data.frame(
         ppt.2 = x$ppt[x$biomass_date %in% max(x$biomass_date)]
        ))

#read in precip data
ppt.3 <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/data/precip/anpp_clean_trt_ppt_no-perc_1095-730days_2023-01-02.csv")

#reduce column names to minimum
ppt.3$ppt.3 <- ppt.3$ppt#change precip column names in lag files to reflect lags
ppt.3 <- ddply(ppt.3, c("site_code", "year", "trt"), 
        function(x)data.frame(
         ppt.3 = x$ppt[x$biomass_date %in% max(x$biomass_date)]
        ))

#read in precip data
ppt.4 <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/data/precip/anpp_clean_trt_ppt_no-perc_1460-1095days_2023-01-02.csv")

#reduce column names to minimum
ppt.4$ppt.4 <- ppt.4$ppt#change precip column names in lag files to reflect lags
ppt.4 <- ddply(ppt.4, c("site_code", "year", "trt"), 
        function(x)data.frame(
         ppt.4 = x$ppt[x$biomass_date %in% max(x$biomass_date)]
        ))

#merge all the precip-lag years
full_ppt <- merge(ppt.1, ppt.2, by = c("site_code", "year", "trt"), all.x = TRUE)%>%
 unique()%>%
 merge(ppt.3, by = c("site_code", "year", "trt"), all.x = TRUE)%>%
 unique()%>%
 merge(ppt.4, by = c("site_code", "year", "trt"), all.x = TRUE)%>%
 unique()


cover_ppt <- merge(full_cover_v2, full_ppt, by = c("site_code", "year", "trt"), all.x = TRUE)%>%
 subset(live == 1)


#read worldclim data
#worldclim <- read.csv("worldclim_map.csv")

#cover_ppt_map <- merge(cover_ppt, worldclim, by = "site_code", all.x = TRUE)


#specify n_trt_years with n_treat_days
cover_ppt <- cover_ppt[-c(12)]#remove old column which isn't trustworthy
cover_ppt$n_treat_days <- as.numeric(cover_ppt$n_treat_days)

#temp <- cover_ppt[,c("site_code", "n_treat_days", "year")]
#temp <- unique(temp)

#cover_ppt$n_treat_years <- ifelse(cover_ppt$n_treat_days > 50 & cover_ppt$n_treat_days < 415, "1", "NA")
#years <- unique(cover_ppt[,c("site_code", "year", "n_treat_years")])
#years$n_treat_years2 <- 

#cover_ppt <- cover_ppt%>%
#  mutate(n_treat_years = case_when(
#    n_treat_days <= 50 ~ 0,
#    n_treat_days > 50 & n_treat_days < 415 ~ 1,
#    n_treat_days >= 415 & n_treat_days < 780 ~ 2,
#    n_treat_days >= 780 & n_treat_days < 1145 ~ 3,
#    n_treat_days >= 1145 & n_treat_days < 1510 ~ 4,
#    n_treat_days >= 1510 & n_treat_days < 1875 ~ 5,
#    n_treat_days >= 1875 & n_treat_days < 2240 ~ 6
#  ))

#temp <- cover_ppt_map[, c("site_code", "n_treat_years")]
#temp <- unique(temp)

#hist(subset(temp, n_treat_years != 0)$n_treat_years)


cover_ppt_full <- left_join(cover_ppt, IDE_treatment_years, by = c("site_code", "year"))%>%
                    subset(trt == "Control"| trt == "Drought")%>%
                  left_join(cover_survey, by = "site_code")



write.csv(cover_ppt_full, "C:/Users/ohler/Dropbox/IDE/data_processed/cover_ppt_2023-01-02.csv")


length(unique(subset(cover_ppt_full, n_treat_years == 1)$site_code))
length(unique(subset(cover_ppt_full, n_treat_years == 2)$site_code))
length(unique(subset(cover_ppt_full, n_treat_years == 3)$site_code))
length(unique(subset(cover_ppt_full, n_treat_years == 4)$site_code))
