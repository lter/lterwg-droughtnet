###########This script is supposed to do a lot of same stuff as the original duration lag analysis, but using long-term mean anpp to calculate response ratios

library(tidyverse)
library(plyr)



data.anpp <- read.csv("C:/Users/ohler/Dropbox/IDE MS_Single year extreme/Data/full_biomass_05-01-2023.csv")%>%
              dplyr::select(site_code, year, n_treat_days)%>%
              unique()

data.cover <- read.csv("C:/Users/ohler/Dropbox/IDE/data_raw/full_cover_2023-05-01.csv")%>%
              dplyr::select(site_code, year, n_treat_days)%>%
              unique()%>%
              subset(n_treat_days != "NULL")#marcdrt.ar has some null values for unknown reasons
data.cover$n_treat_days <- as.numeric(data.cover$n_treat_days)

comb <- rbind(data.anpp, data.cover)%>%
  ddply(.(site_code, year), function(x)data.frame(
    n_treat_days = max(x$n_treat_days)
  ))

  


first_treat_year <- subset(comb, n_treat_days >= 200 & n_treat_days < 800  )%>%
                    ddply(.(site_code),function(x)data.frame(
                      first_year = min(x$year)
                    ))


#hist(first_treat_year$n_treat_days)

half_treat_year <- subset(comb, n_treat_days >= 30 & n_treat_days < 200  )%>%
                  dplyr::select(site_code, year)
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
  TRUE ~ comb1$n_treat_years
)

combfin <- dplyr::select(comb1, site_code, year, n_treat_years1)%>%
          dplyr::rename(n_treat_years=n_treat_years1)

subset(combfin, is.na(n_treat_years) == TRUE)






write.csv(combfin, "C:/Users/ohler/Dropbox/IDE/data_processed/IDE_treatment_years_2023-05-01.csv")
