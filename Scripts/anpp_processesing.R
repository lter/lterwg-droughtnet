#######################################################################
#This script takes the clean anpp data and merges it with multiple iterations of precipitation data to facilitate analyses based on precip lags

######################################################################
##########
library(tidyverse)
library(plyr)


anpp_clean <- read.csv("C:/Users/ohler/Dropbox/IDE MS_Single year extreme/Data/anpp_clean_10-12-2022.csv")



treatment_info <- read.csv("C:/Users/ohler/Downloads/full_biomass_test.csv")
treatment_info <- treatment_info[,c("site_code", "year", "n_treat_days", "block", "plot", "subplot", "trt")]
treatment_info <- unique(treatment_info)

#full_biomass <- read.csv("C:/Users/ohler/Dropbox/IDE MS_Single year extreme/Data/full_biomass_test.csv")
#details <- full_biomass[,c("site_code", block, plot, subplot, year, )]

#read in precip data
ppt.1 <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/Data/precip/anpp_clean_trt_ppt_no-perc_365-0days_2022-10-07.csv")

#reduce column names to minimum
ppt.1$ppt.1 <- ppt.1$ppt#change precip column names in lag files to reflect lags
ppt.1 <- ddply(ppt.1, c("site_code", "year", "trt"),
               function(x)data.frame(
                 ppt.1 = mean(x$ppt)
               ))


#ppt.1[,c("site_code", "year", "block", "plot", "subplot", "trt", "ppt.1")]


#read in precip data
ppt.2 <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/Data/precip/anpp_clean_trt_ppt_no-perc_730-365days_2022-10-07.csv")

#reduce column names to minimum
ppt.2$ppt.2 <- ppt.2$ppt#change precip column names in lag files to reflect lags
ppt.2 <- ddply(ppt.2, c("site_code", "year", "trt"),
               function(x)data.frame(
                 ppt.2 = mean(x$ppt)
               ))

#ppt.2[,c("site_code", "year", "trt", "ppt.2")]

#read in precip data
ppt.3 <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/Data/precip/anpp_clean_trt_ppt_no-perc_1095-730days_2022-10-07.csv")

#reduce column names to minimum
ppt.3$ppt.3 <- ppt.3$ppt#change precip column names in lag files to reflect lags
ppt.3 <- ddply(ppt.3, c("site_code", "year", "trt"),
               function(x)data.frame(
                 ppt.3 = mean(x$ppt)
               ))

#ppt.3[,c("site_code", "year", "trt", "ppt.3")]

#read in precip data
ppt.4 <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/Data/precip/anpp_clean_trt_ppt_no-perc_1460-1095days_2022-10-07.csv")

#reduce column names to minimum
ppt.4$ppt.4 <- ppt.4$ppt#change precip column names in lag files to reflect lags
ppt.4 <- ddply(ppt.4, c("site_code", "year", "trt"),
               function(x)data.frame(
                 ppt.4 = mean(x$ppt)
               ))
# ppt.4[,c("site_code", "year", "trt", "ppt.4")]


#merge all the precip-lag years
full_ppt <- merge(ppt.1, ppt.2, by = c("site_code", "year", "trt"), all.x = TRUE)%>%
  unique()%>%
  merge(ppt.3, by = c("site_code", "year", "trt"), all.x = TRUE)%>%
  unique()%>%
  merge(ppt.4, by = c("site_code", "year", "trt"), all.x = TRUE)%>%
  unique()




anpp_ppt <- anpp_clean%>%
        left_join(treatment_info, c("site_code", "year", "block", "plot", "subplot"))%>%
        left_join(full_ppt, c("site_code",  "year", "trt"))

data.all<-anpp_ppt %>%
  dplyr::group_by(site_code,block,plot,subplot,year,mass,mass_category,trt,ppt.1, ppt.2,ppt.3,ppt.4) %>%
  dplyr::summarize(n_treat_days = max(n_treat_days))

#read worldclim data
worldclim <- read.csv("C:/Users/ohler/Dropbox/IDE MS_Single year extreme/Data/precip/worldclim_map.csv")

anpp_ppt_map <- left_join(anpp_ppt, worldclim, by = "site_code")


#specify n_trt_years with n_treat_days
anpp_ppt_map$n_treat_years <- ifelse(anpp_ppt_map$n_treat_days <= 50, 0,
                                     ifelse(anpp_ppt_map$n_treat_days > 50 & anpp_ppt_map$n_treat_days < 415, 1,
                                            ifelse(anpp_ppt_map$n_treat_days >= 415 & anpp_ppt_map$n_treat_days < 780, 2,
                                                   ifelse(anpp_ppt_map$n_treat_days >= 780 & anpp_ppt_map$n_treat_days < 1145, 3,
                                                          ifelse(anpp_ppt_map$n_treat_days >= 1145 & anpp_ppt_map$n_treat_days < 1510, 4,
                                                                 ifelse(anpp_ppt_map$n_treat_days >= 1510 & anpp_ppt_map$n_treat_days < 1875, 5,
                                                                        ifelse(anpp_ppt_map$n_treat_days >= 1875 & anpp_ppt_map$n_treat_days <2240, 6, NA )))))))

temp <- anpp_ppt_map[,c("site_code", "n_treat_years")]
temp <- unique(temp)

hist(subset(temp,n_treat_years != 0)$n_treat_years)

#library(lmerTest)
#mod.1 <- lmer(mass~ppt.1 + (1|site_code:plot) + (1|site_code:year), data = anpp_ppt) 
#mod.2 <- lmer(mass~ppt.1 + ppt.2 + (1|site_code:plot) + (1|site_code:year), data = anpp_ppt) 
#mod.3 <- lmer(mass~ppt.1 + ppt.2 + ppt.3 + (1|site_code:plot) + (1|site_code:year), data = anpp_ppt) 
#mod.4 <- lmer(mass~ppt.1 + ppt.2 + ppt.3 + ppt.4 + (1|site_code:plot) + (1|site_code:year), data = anpp_ppt) 

#AIC(mod.1, mod.2, mod.3, mod.4)

