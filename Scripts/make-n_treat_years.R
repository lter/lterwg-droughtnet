###########This script is supposed to do a lot of same stuff as the original duration lag analysis, but using long-term mean anpp to calculate response ratios

library(tidyverse)
library(plyr)



data.anpp <- read.csv("C:/Users/ohler/Dropbox/IDE MS_Single year extreme/Data/full_biomass_01-29-2026.csv")%>%
              dplyr::select(site_code, year, n_treat_days)%>%
              unique()
data.anpp$n_treat_days <- as.numeric(data.anpp$n_treat_days)

data.cover <- read.csv("C:/Users/ohler/Dropbox/IDE/data_raw/full_cover_2026-01-29.csv")%>%
              dplyr::select(site_code, year, n_treat_days)%>%
              unique()%>%
              subset(n_treat_days != "NULL")#marcdrt.ar has some null values for unknown reasons
data.cover$n_treat_days <- as.numeric(data.cover$n_treat_days)

comb <- rbind(data.anpp, data.cover)%>%
  ddply(.(site_code, year), function(x)data.frame(
    n_treat_days = max(x$n_treat_days, na.rm = TRUE)
  ))%>%
  mutate(n_treat_days = ifelse(is.infinite(n_treat_days), NA, n_treat_days))

  


first_treat_year <- subset(comb, n_treat_days >= 200 & n_treat_days < 565  )%>%
                    ddply(.(site_code),function(x)data.frame(
                      first_year = min(x$year)
                    ))


#hist(first_treat_year$n_treat_days)

half_treat_year <- subset(comb, n_treat_days >= 30 & n_treat_days < 200  )%>%
                  dplyr::select(site_code, year)%>%
                  subset(site_code != "wytham.uk")
half_treat_year$n_treat_years_half <- "0.5"

pretreatment_year <- subset(comb, n_treat_days <30)%>%
                    ddply(.(site_code), function(x)data.frame(
                      year = max(x$year)
                    ))
pretreatment_year$n_treat_years_pre <- "0"


comb1 <- left_join(comb, first_treat_year, by = "site_code")%>%
        left_join(half_treat_year, by = c("site_code", "year"))%>%
        left_join(pretreatment_year, by = c("site_code", "year"))



comb1$n_treat_years <- (comb1$year - comb1$first_year)+1
  
#comb1$n_treat_years1 <- ifelse(comb1$n_treat_years_half == "0.5", yes= "0.5",
#                              ifelse(comb1$n_treat_years_pre == "0", "0",
#                                     print(as.factor(comb1$n_treat_years)))#)


comb1$n_treat_years1 <- case_when(
  comb1$n_treat_years_half == 0.5 ~ 0.5,
  comb1$n_treat_years_pre == 0 ~ 0,
  comb1$site_code == "buoya.no" & comb1$year == "2019" ~ 2, #this hard codes in a treatment year for this site but may need to be changed if n_treat_year definition changes
  comb1$site_code == "haver.no" & comb1$year == "2019" ~ 2, #this hard codes in a treatment year for this site but may need to be changed if n_treat_year definition changes
  comb1$site_code == "allmendb.ch" & comb1$year == "2017" ~ 2, #this hard codes in a treatment year for this site but may need to be changed if n_treat_year definition changes
  comb1$site_code == "allmendb.ch" & comb1$year == "2018" ~ 3, #this hard codes in a treatment year for this site but may need to be changed if n_treat_year definition changes
  comb1$site_code == "allmendb.ch" & comb1$year == "2019" ~ 4, #this hard codes in a treatment year for this site but may need to be changed if n_treat_year definition changes
  comb1$site_code == "allmendo.ch" & comb1$year == "2017" ~ 2, #this hard codes in a treatment year for this site but may need to be changed if n_treat_year definition changes
  comb1$site_code == "allmendo.ch" & comb1$year == "2018" ~ 3, #this hard codes in a treatment year for this site but may need to be changed if n_treat_year definition changes
  comb1$site_code == "allmendo.ch" & comb1$year == "2019" ~ 4, #this hard codes in a treatment year for this site but may need to be changed if n_treat_year definition changes
  comb1$site_code == "cdpt_drt.us" & comb1$year == "2019" ~ -1, #this hard codes in a treatment year for this site but may need to be changed if n_treat_year definition changes
  comb1$site_code == "eea.br" & comb1$year == "2020" ~ 2, #this hard codes in a treatment year for this site but may need to be changed if n_treat_year definition changes
  comb1$site_code == "lcnorth.cl" & comb1$year == "2019" ~ 2, #this hard codes in a treatment year for this site but may need to be changed if n_treat_year definition changes
  comb1$site_code == "lcsouth.cl" & comb1$year == "2019" ~ 2, #this hard codes in a treatment year for this site but may need to be changed if n_treat_year definition changes
  
  comb1$site_code == "qdtnorth.cl" & comb1$year == "2019" ~ 2, #this hard codes in a treatment year for this site but may need to be changed if n_treat_year definition changes
  comb1$site_code == "qdtsouth.cl" & comb1$year == "2019" ~ 2, #this hard codes in a treatment year for this site but may need to be changed if n_treat_year definition changes
  
  comb1$site_code == "torla.es" & comb1$year == "2017" ~ 2, #this hard codes in a treatment year for this site but may need to be changed if n_treat_year definition changes
  comb1$site_code == "torla.es" & comb1$year == "2018" ~ 3, #this hard codes in a treatment year for this site but may need to be changed if n_treat_year definition changes
  comb1$site_code == "torla.es" & comb1$year == "2019" ~ 4, #this hard codes in a treatment year for this site but may need to be changed if n_treat_year definition changes
  comb1$site_code == "sevforest.us" & comb1$year == "2011" ~ 4,
  TRUE ~ comb1$n_treat_years
)

combfin <- dplyr::select(comb1, site_code, year, n_treat_years1)%>%
          dplyr::rename(n_treat_years=n_treat_years1)

subset(combfin, is.na(n_treat_years) == TRUE)










write.csv(combfin, "C:/Users/ohler/Dropbox/IDE/data_processed/IDE_treatment_years_2026-01-29.csv")

