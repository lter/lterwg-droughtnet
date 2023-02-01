#######################################################################
#This script takes the clean anpp data and merges it with multiple iterations of precipitation data to facilitate analyses based on precip lags

######################################################################
##########
library(tidyverse)
library(plyr)


#read n_treat_years data
IDE_treatment_years<- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/IDE_treatment_years_11-17-2022.csv")

anpp_clean <- read.csv("C:/Users/ohler/Dropbox/IDE MS_Single year extreme/Data/anpp_clean_11-18-2022.csv")



treatment_info <- read.csv("C:/Users/ohler/Dropbox/IDE MS_Single year extreme/Data/full_biomass_11-18-2022.csv")
treatment_info <- treatment_info[,c("site_code", "year", "n_treat_days", "block", "plot", "subplot", "trt")]
treatment_info <- unique(treatment_info)

treatment_info$trt <- plyr::revalue(treatment_info$trt, c("Control_Infrastructure"="Control"))

site_map <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/Site_Elev-Disturb.csv")%>%
  dplyr::select(site_code, precip, habitat.type)%>%
  dplyr::rename(map = precip)
#full_biomass <- read.csv("C:/Users/ohler/Dropbox/IDE MS_Single year extreme/Data/full_biomass_test.csv")
#details <- full_biomass[,c("site_code", block, plot, subplot, year, )]

#read in precip data
ppt.1 <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/Data/precip/anpp_clean_trt_ppt_no-perc_365-0days_2023-01-02.csv")

#reduce column names to minimum
ppt.1$ppt.1 <- ppt.1$ppt#change precip column names in lag files to reflect lags
ppt.1 <- ddply(ppt.1, c("site_code", "year", "trt"),
               function(x)data.frame(
                 ppt.1 = x$ppt[x$biomass_date %in% max(x$biomass_date)]
               ))


#read in precip data
ppt.2 <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/Data/precip/anpp_clean_trt_ppt_no-perc_730-365days_2023-01-02.csv")

#reduce column names to minimum
ppt.2$ppt.2 <- ppt.2$ppt#change precip column names in lag files to reflect lags
ppt.2 <- ddply(ppt.2, c("site_code", "year", "trt"),
               function(x)data.frame(
                 ppt.2 = x$ppt[x$biomass_date %in% max(x$biomass_date)]
               ))


#read in precip data
ppt.3 <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/Data/precip/anpp_clean_trt_ppt_no-perc_1095-730days_2023-01-02.csv")

#reduce column names to minimum
ppt.3$ppt.3 <- ppt.3$ppt#change precip column names in lag files to reflect lags
ppt.3 <- ddply(ppt.3, c("site_code", "year", "trt"),
               function(x)data.frame(
                 ppt.3 = x$ppt[x$biomass_date %in% max(x$biomass_date)]
               ))

#read in precip data
ppt.4 <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/Data/precip/anpp_clean_trt_ppt_no-perc_1460-1095days_2023-01-02.csv")

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
  dplyr::summarize(n_treat_days = max(n_treat_days))%>%
  unite(rep_year, c("site_code", "year", "block", "plot", "subplot"), remove = FALSE)%>%
          subset(rep_year != "passogavia.it_2021_5_10_A")%>% #plot may have issues. check github
          subset(rep_year != "naqu.cn_2021_2_15_A")%>%#plot may have issues. check github
          subset(rep_year != "sevblack.us_2019_1_106_A")%>%#plot may have issues. Tim Ohlert says high biomass is due to shrub and therefore not representative of ANPP
          subset(rep_year != "jenadrt.de_2015_1_1_A")%>%
subset(rep_year != "jenadrt.de_2015_1_2_A")%>%
subset(rep_year != "jenadrt.de_2015_1_3_A")%>%
subset(rep_year != "jenadrt.de_2015_1_4_A")%>%
subset(rep_year != "jenadrt.de_2015_1_5_A")%>%
subset(rep_year != "jenadrt.de_2015_1_6_A")%>%
          subset(site_code != "lcnorth.cl")%>% #Doesn't report ANPP
  subset(site_code != "lcsouth.cl")%>%#Doesn't report ANPP
  subset(site_code != "qdtnorth.cl")%>%#Doesn't report ANPP
  subset(site_code != "qdtsouth.cl")%>%#Doesn't report ANPP
  subset(site_code != "neudamm.na")%>%#Doesn't report ANPP, also no weather info
  subset(site_code != "ebro.es")%>%#ANPP outside range for biome
  subset(site_code != "ethadb.au")%>%#ANPP outside range for biome
  subset(site_code != "ethadn.au")%>%#ANPP outside range for biome
  unite(site_trt, c("site_code", "trt"), remove =FALSE)%>%
  subset(site_trt != "garraf.es_Drought")%>% #Did not follow protocols: Not using for drought plots, only using control plots for this site
  subset(site_trt != "brandjberg.dk_Drought")%>%#Did not follow protocols: Not using for drought plots, only using control plots for this site
  subset(site_trt != "swift.ca_Drought")%>% #did not follow IDE protocols (drought plots did not exclude water- see Jillian email)%>%
  subset(select=-c(rep_year, site_trt))
                               
data.all[data.all$site_code == "ukulingadrt.za" & data.all$year == "2021" & data.all$block == "3" & data.all$plot == "8", "mass"] <- 393.66 #updated value from site PI. See GitHub for details #ukulinga.za -- The correct biomass for 2021 block 3 plot 8 should be 393.66 g/m2


anpp_ppt.end <- data.all%>%
          left_join( IDE_treatment_years, by = c("site_code", "year"))%>%
  subset(trt == "Control"| trt == "Drought" )%>%
  merge(site_map)
         #| trt == "Control_Infrastructure")





#write.csv(anpp_ppt.end, "C:/Users/ohler/Dropbox/IDE/data_processed/anpp_ppt_2023-01-02.csv")


