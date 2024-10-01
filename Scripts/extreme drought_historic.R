library(tidyverse)
library(plyr)
library(ggthemes)
library(lubridate)


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

sev_precip_perc <- quantile(sev_met$Precipitation, probs = c(0.025,0.975))


sev_biomass <- read.csv("C:/Users/ohler/Documents/EDGE/sev331_quadrat_plant_species_biomass_2024-05-16.csv")%>%
  subset(site == "core_black" & season == "fall")%>%
  ddply(.(site, year, web, plot, quad), function(x)data.frame(
    biomass = sum(x$biomass.BM)
  ))%>%
  ddply(.(site, year), function(x)data.frame(
    biomass = mean(x$biomass)
  ))
sev_biomass_perc <- quantile(sev_biomass$biomass, probs = c(0.025,0.975))


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

sev_full%>%
    subset(n_treat_years != "0")%>%
ggplot( aes(Precipitation, biomass))+
  geom_hline(aes(yintercept = sev_biomass_perc[1]), linetype = "dashed")+
  geom_hline(aes(yintercept = sev_biomass_perc[2]), linetype = "dashed")+
  geom_vline(aes(xintercept = sev_precip_perc[1]), linetype = "dashed")+
  geom_vline(aes(xintercept = sev_precip_perc[2]), linetype = "dashed")+
  geom_point(aes(color = trt,# shape = n_treat_years
                 ), size = 3)+
 # scale_shape_manual(values = c( 49, 50, 51, 52, 16) )+
  scale_color_manual(values = c("#179F00","#FF5E1F","#7F7F7F"))+
  geom_smooth(data = subset(sev_full, n_treat_years == "historic"),aes(Precipitation, biomass),method = "lm", se = FALSE, color = "black")+
  ylim(0,235)+
  geom_text(aes(label=ifelse(n_treat_years != "historic",n_treat_years,'')),hjust=0,vjust=0)+
  theme_classic()

#ggsave("C:/Users/ohler/Dropbox/Tim Work/DroughtNet/sev_extreme.pdf",
#       device = "pdf",
#       width = 6,
#        height = 4)



#load SGS data

sgs_biomass <- read.csv("C:/Users/ohler/Downloads/Summary1_SGS_ANPP.csv")%>%
              subset(Site == "SWALE")%>%
              group_by(Year, Transect)%>%
              dplyr::summarize(anpp = mean(anpp_sum))%>%
              group_by(Year)%>%
              dplyr::summarise(biomass = mean(anpp))%>%
              dplyr::rename(year = Year)
sgs_biomass_perc <- quantile(sgs_biomass$biomass, probs = c(0.025,0.975))


sgs_precip <- read.csv("C:/Users/ohler/Downloads/FULL_precip.csv")%>%
              subset(Site == "SGS")%>%
              subset(amb_drought != "drought")

sgs_precip$DATE <- mdy(sgs_precip$DATE)
sgs_precip$year <- year(sgs_precip$DATE)

sgs_precip <- sgs_precip%>%
              group_by(year, DATE)%>%
              dplyr::summarize(daily.tot = mean(daily.tot))%>%
              group_by(year)%>%
              dplyr::summarize(Precipitation = sum(daily.tot))
sgs_precip_perc <- quantile(sgs_precip$Precipitation, probs = c(0.025,0.975))

sgs_ambient <- left_join(sgs_biomass, sgs_precip, by = "year")
sgs_ambient$n_treat_years <- "historic"
sgs_ambient$trt <- "historic"


sgs_ide <- ide_biomass%>%
  subset(site_code == "sgsdrt.us")%>%
  ddply(.(site_code, block, plot, subplot, year, n_treat_years, trt, ppt.1), function(x)data.frame(
    biomass = sum(x$mass)
  ))%>%
  ddply(.(year, n_treat_years, trt, ppt.1), function(x)data.frame(
    biomass = mean(x$biomass)
  ))%>%
  dplyr::rename(Precipitation = "ppt.1")
sgs_ide$n_treat_years <- as.character(sgs_ide$n_treat_years)


sgs_full <- rbind(sgs_ambient, sgs_ide)# %>%
  #dplyr::mutate(n_treat_years = as.character(n_treat_years))

#sgs_full$n_treat_years <- ifelse()

sgs_full$n_treat_years <- plyr::revalue(sgs_full$n_treat_years, c("0.5" = "1","1"="2","2"="3","3"="4","4"="5"))


sgs_full%>%
  subset(n_treat_years != "-6" & n_treat_years != "-5" & n_treat_years != "-4" &
           n_treat_years != "-3" &
           n_treat_years != "-2" &
           n_treat_years != "-1" &
           n_treat_years != "0" )%>%
ggplot( aes(Precipitation, biomass))+
  geom_hline(aes(yintercept = sgs_biomass_perc[1]), linetype = "dashed")+
  geom_hline(aes(yintercept = sgs_biomass_perc[2]), linetype = "dashed")+
  geom_vline(aes(xintercept = sgs_precip_perc[1]), linetype = "dashed")+
  geom_vline(aes(xintercept = sgs_precip_perc[2]), linetype = "dashed")+
  geom_point(aes(color = trt#, shape = n_treat_years
                 ), size = 3)+
  #scale_shape_manual(values = c(49, 50, 51, 52, 53, 16) )+
  scale_color_manual(values = c("#179F00","#FF5E1F","#7F7F7F"))+
  geom_smooth(data = subset(sgs_full, n_treat_years == "historic"),method = "lm", se = FALSE, color = "black")+
  geom_text(aes(label=ifelse(n_treat_years != "historic",n_treat_years,'')),hjust=0,vjust=0)+
  theme_classic()

#ggsave("C:/Users/ohler/Dropbox/Tim Work/DroughtNet/sgs_extreme_swale.pdf",
#       device = "pdf",
#     width = 6,
#        height = 4)



#load konza data

