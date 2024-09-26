library(tidyverse)
library(plyr)
library(ggthemes)


#load IDE data
ide_biomass <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/anpp_ppt_2024-03-15.csv")



#load sevilleta data

sev_met <- read.csv("C:/Users/ohler/Downloads/Sevilleta_LTER_Hourly_Meteorological_Data_1995_1999.csv")%>%
  rbind(read.csv("C:/Users/ohler/Downloads/Sevilleta_LTER_Hourly_Meteorological_Data_2000_2004.csv"))%>%
  rbind(read.csv("C:/Users/ohler/Downloads/Sevilleta_LTER_Hourly_Meteorological_Data_2005_2009.csv"))%>%
  rbind(read.csv("C:/Users/ohler/Downloads/Sevilleta_LTER_Hourly_Meteorological_Data_2010_2014.csv"))%>%
  rbind(read.csv("C:/Users/ohler/Downloads/Sevilleta_LTER_Hourly_Meteorological_Data_2015_2019.csv"))%>%
  rbind(read.csv("C:/Users/ohler/Downloads/Sevilleta_LTER_Hourly_Meteorological_Data_2020_2022.csv"))%>%
  subset(StationID == "49")%>%
  ddply(.( Year), function(x)data.frame(
    Precipitation = sum(x$Precipitation, na.rm = TRUE)
  ))%>%
  dplyr::rename(year = "Year")


sev_biomass <- read.csv("C:/Users/ohler/Documents/EDGE/sev331_quadrat_plant_species_biomass_2024-05-16.csv")%>%
  subset(site == "core_black" & season == "fall")%>%
  ddply(.(site, year, web, plot, quad), function(x)data.frame(
    biomass = sum(x$biomass.BM)
  ))%>%
  ddply(.(site, year), function(x)data.frame(
    biomass = mean(x$biomass)
  ))

sev_ide <- ide_biomass%>%
            subset(site_code == "sevblack.us")%>%
            ddply(.(site_code, block, plot, subplot, year, n_treat_years, trt, ppt.1), function(x)data.frame(
              biomass = sum(x$mass)
            ))%>%
            ddply(.(site_code, year, n_treat_years, trt, ppt.1), function(x)data.frame(
              biomass = mean(x$biomass)
            ))%>%
            dplyr::rename(site = "site_code")%>%
            dplyr::rename(Precipitation = "ppt.1")




sev <- left_join(sev_biomass, sev_met, by = "year")
sev$n_treat_years <- NA
sev$trt <- NA


sev_full <- rbind(sev, sev_ide) %>%
  dplyr::mutate(n_treat_years = as.character(n_treat_years))%>%
  dplyr::mutate(n_treat_years = replace_na(n_treat_years, "historic"))
#revalue(as.factor(sev_full$n_treat_years), c("NA"="historic"))

ggplot(sev_full, aes(Precipitation, biomass))+
  geom_point(aes(color = trt, shape = n_treat_years), size = 3)+
  scale_shape_manual(values = c(48, 49, 50, 51, 52, 16) )+
  geom_smooth(method = "lm", se = FALSE)+
  theme_classic()





#load SGS data





#load konza data

