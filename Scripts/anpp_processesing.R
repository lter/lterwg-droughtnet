#######################################################################
#This script takes the clean anpp data and merges it with multiple iterations of precipitation data to facilitate analyses based on precip lags

######################################################################
##########
library(tidyverse)
library(plyr)


#read n_treat_years data
IDE_treatment_years<- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/IDE_treatment_years_2025-05-19.csv")

anpp_clean <- read.csv("C:/Users/ohler/Dropbox/IDE MS_Single year extreme/Data/anpp_clean_2025-05-19.csv")



treatment_info <- read.csv("C:/Users/ohler/Dropbox/IDE MS_Single year extreme/Data/full_biomass_07-18-2024.csv")
treatment_info <- treatment_info[,c("site_code", "year", "n_treat_days", "block", "plot", "subplot", "trt")]
treatment_info <- unique(treatment_info)
infrastructure_controls <- subset(treatment_info, trt == "Control_infrastructure")


treatment_info <- treatment_info%>%
  tidyr::unite(temp, c("site_code", "trt" ), remove = FALSE)%>%
              subset( temp != "ayora.es_Control_infrastructure")%>%#remove infrastructure controls from sites that have other controls
                subset( temp != "baddrt.de_Control_infrastructure")%>%
                subset( temp != "cedarsav.us_Control_infrastructure")%>%
                subset( temp != "cedartrait.us_Control_infrastructure")%>%
                subset( temp != "freiburg.de_Control_infrastructure")%>%
                subset( temp != "guaribas.br_Control_infrastructure")%>%
                subset( temp != "purdue.us_Control_infrastructure")%>%
                subset( temp != "sevforest.us_Control_infrastructure")%>%
                subset( temp != "wytham.uk_Control_infrastructure")%>%
                dplyr::select(-temp)
                


treatment_info$trt <- plyr::revalue(treatment_info$trt, c("Control_infrastructure"="Control"))

site_map <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/Site_Elev-Disturb.csv")%>%
  dplyr::select(site_code, precip, habitat.type, arid, ipcc_regions, continent)%>%
  dplyr::rename(map = precip)
#full_biomass <- read.csv("C:/Users/ohler/Dropbox/IDE MS_Single year extreme/Data/full_biomass_test.csv")
#details <- full_biomass[,c("site_code", block, plot, subplot, year, )]

#read in precip data
ppt.1 <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/Data/precip/anpp_clean_trt_ppt_no-perc_365-0days_2024-12-16.csv")

#reduce column names to minimum
ppt.1$ppt.1 <- ppt.1$ppt#change precip column names in lag files to reflect lags
ppt.1 <- ddply(ppt.1, c("site_code", "year", "trt"),
               function(x)data.frame(
                 ppt.1 = x$ppt[x$biomass_date %in% max(x$biomass_date)]
               ))


#read in precip data
ppt.2 <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/Data/precip/anpp_clean_trt_ppt_no-perc_730-365days_2025-05-19.csv")

#reduce column names to minimum
ppt.2$ppt.2 <- ppt.2$ppt#change precip column names in lag files to reflect lags
ppt.2 <- ddply(ppt.2, c("site_code", "year", "trt"),
               function(x)data.frame(
                 ppt.2 = x$ppt[x$biomass_date %in% max(x$biomass_date)]
               ))


#read in precip data
ppt.3 <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/Data/precip/anpp_clean_trt_ppt_no-perc_1095-730days_2025-05-19.csv")

#reduce column names to minimum
ppt.3$ppt.3 <- ppt.3$ppt#change precip column names in lag files to reflect lags
ppt.3 <- ddply(ppt.3, c("site_code", "year", "trt"),
               function(x)data.frame(
                 ppt.3 = x$ppt[x$biomass_date %in% max(x$biomass_date)]
               ))

#read in precip data
ppt.4 <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/Data/precip/anpp_clean_trt_ppt_no-perc_1460-1095days_2025-05-19.csv")

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




anpp_ppt <- anpp_clean%>%
        left_join(treatment_info, c("site_code", "year", "block", "plot", "subplot"))%>%
        left_join(full_ppt, c("site_code",  "year", "trt"))

#summarize treatment days and remove plots identified by site PIs as problematic
data.all<-anpp_ppt %>%
  dplyr::group_by(site_code,block,plot,subplot,year,mass,mass_category,trt,ppt.1, ppt.2,ppt.3,ppt.4) %>%
  subset(n_treat_days != 91 )%>% #only removes one problematic Wytham data entry that screws with n_treat_years
  dplyr::summarize(n_treat_days = max(n_treat_days))%>%
  unite(rep_year, c("site_code", "year", "block", "plot", "subplot"), remove = FALSE)%>%
          subset(rep_year != "passogavia.it_2021_5_10_A")%>% #plot may have issues. check github
          subset(rep_year != "naqu.cn_2021_2_15_A")%>%#plot may have issues. check github
          subset(rep_year != "sevblack.us_2019_1_106_A")%>%#plot may have issues. Tim Ohlert says high biomass is due to shrub and therefore not representative of ANPP
          subset(rep_year != "jenadrt.de_2016_1_1_A")%>%#Jena says they lost a biomass bag in 2016
subset(rep_year != "jenadrt.de_2016_1_2_A")%>%
subset(rep_year != "jenadrt.de_2016_1_3_A")%>%
subset(rep_year != "jenadrt.de_2016_1_4_A")%>%
subset(rep_year != "jenadrt.de_2016_1_5_A")%>%
subset(rep_year != "jenadrt.de_2016_1_6_A")%>%
subset(rep_year != "nyngan.au_2019_1_8_A")%>% #major outlier. Tim made decision to nix it
subset(rep_year != "bayrdrt.de_2021_1_1_A")%>%#recovery data from Bayreuth
subset(rep_year != "bayrdrt.de_2021_1_6_A")%>%
subset(rep_year != "bayrdrt.de_2021_2_2_A")%>%
subset(rep_year != "bayrdrt.de_2021_2_7_A")%>%
  subset(rep_year != "bayrdrt.de_2021_3_3_A")%>%
  subset(rep_year != "bayrdrt.de_2021_3_8_A")%>%
  subset(rep_year != "bayrdrt.de_2021_4_4_A")%>%
  subset(rep_year != "bayrdrt.de_2021_4_9_A")%>%
  subset(rep_year != "bayrdrt.de_2021_5_10_A")%>%
  subset(rep_year != "bayrdrt.de_2021_5_5_A")%>%
  subset(rep_year != "bayrdrt.de_2022_1_1_A")%>%
  subset(rep_year != "bayrdrt.de_2022_1_6_A")%>%
  subset(rep_year != "bayrdrt.de_2022_2_2_A")%>%
  subset(rep_year != "bayrdrt.de_2022_2_7_A")%>%
  subset(rep_year != "bayrdrt.de_2022_3_3_A")%>%
  subset(rep_year != "bayrdrt.de_2022_3_8_A")%>%
  subset(rep_year != "bayrdrt.de_2022_4_4_A")%>%
  subset(rep_year != "bayrdrt.de_2022_4_9_A")%>%
  subset(rep_year != "bayrdrt.de_2022_5_10_A")%>%
  subset(rep_year != "bayrdrt.de_2022_5_5_A")%>%
  subset(rep_year != "marcdrt.ar_2021_1_2_A")%>% #recovery data
  subset(rep_year != "marcdrt.ar_2021_1_6_A")%>%
  subset(rep_year != "marcdrt.ar_2021_1_8_A")%>%
  subset(rep_year != "marcdrt.ar_2021_1_9_A")%>%
  subset(rep_year != "marcdrt.ar_2021_1_10_A")%>%
  subset(rep_year != "marcdrt.ar_2021_1_11_A")%>%
  subset(rep_year != "marcdrt.ar_2021_1_13_A")%>%
  subset(rep_year != "marcdrt.ar_2021_1_16_A")%>%
            subset(site_code != "lcnorth.cl")%>% #Doesn't report ANPP
  subset(site_code != "lcsouth.cl")%>%#Doesn't report ANPP
  subset(site_code != "qdtnorth.cl")%>%#Doesn't report ANPP
  subset(site_code != "qdtsouth.cl")%>%#Doesn't report ANPP
  subset(site_code != "neudamm.na")%>%#Doesn't report ANPP, also no weather info
  subset(site_code != "ebro.es")%>%#ANPP outside range for biome
  subset(site_code != "ethadb.au")%>%#ANPP outside range for biome
  subset(site_code != "ethadn.au")%>%#ANPP outside range for biome
  subset(site_code != "plattev.us")%>%#PI said not to use this data due to anomalies
  unite(site_trt, c("site_code", "trt"), remove =FALSE)%>%
  subset(site_trt != "garraf.es_Drought")%>% #Did not follow protocols: Not using for drought plots, only using control plots for this site
  subset(site_trt != "brandjberg.dk_Drought")%>%#Did not follow protocols: Not using for drought plots, only using control plots for this site
  subset(site_trt != "swift.ca_Drought")%>% #did not follow IDE protocols (drought plots did not exclude water- see Jillian email)%>%
  subset(select=-c(rep_year, site_trt))
                               
data.all[data.all$site_code == "ukulingadrt.za" & data.all$year == "2021" & data.all$block == "3" & data.all$plot == "8", "mass"] <- 393.66 #updated value from site PI. See GitHub for details #ukulinga.za -- The correct biomass for 2021 block 3 plot 8 should be 393.66 g/m2


anpp_ppt.end <- data.all%>%
          left_join( IDE_treatment_years, by = c("site_code", "year"))%>%
  subset(trt == "Control"| trt == "Drought" )%>%
  left_join(site_map, by = "site_code")#%>%
  #left_join(infrastructure_controls, by = c("site_code", "year", "n_treat_days", "block", "plot", "subplot"))
         #| trt == "Control_Infrastructure")





write.csv(anpp_ppt.end, "C:/Users/ohler/Dropbox/IDE/data_processed/anpp_ppt_2025-05-19.csv")


