library(tidyverse)
library(plyr)
library(visreg)
library(cowplot)
library(segmented)

##CLIMATE DATA
clim <- read.csv("C:\\Users\\ohler\\Dropbox\\IDE\\data_processed\\climate\\climate_mean_annual_by_site_v2.csv")


#DROUGHTNET
reduced_npp <- read.csv("C:\\Users\\ohler\\Dropbox\\IDE\\data_processed\\anpp_ppt_2023-05-01.csv")

temp <- reduced_npp[,c("site_code", "year")]
temp <- unique(temp)
temp <- ddply(temp, c("site_code"),
              function(x)(
                n.year=length(x$year)
              ))
temp$n.year <- temp$V1
reduced_npp <- merge(reduced_npp, temp, by = "site_code", all.x=TRUE)

#only use plots that are not manipulated
control_biomass_DN <- reduced_npp %>% 
  subset( n_treat_days < 30 | trt == "Control")%>%
  subset(n.year > 4)%>%
  ddply( c("site_code", "year", "plot", "subplot"),
         function(x)data.frame(
           biomass = sum(x$mass)
         )) %>%
  #ddply( c("site_code", "year", "plot"),
  #       function(x)data.frame(
  #        biomass = mean(x$biomass)
  #      )) %>%
  ddply( c("site_code", "year"),
         function(x)data.frame(
           biomass = mean(x$biomass)
         )) %>%
  ddply( c("site_code"),
         function(x)data.frame(
           biomass = mean(x$biomass),
           n.year = length(x$year)
         )) %>%
  subset( site_code != "prades.es")%>%
  subset( site_code != "qdtnorth.cl")

control_biomass_DN <- control_biomass_DN[!is.na(control_biomass_DN$n.year),]
control_biomass_DN$network <- "DroughtNet"




#NUTNET
full.biomass <- read.csv("C:/Users/ohler/Dropbox/NutNet data/full-biomass_2023-04-26.csv")

control_biomass_NN <- full.biomass %>%
  subset(live != 0)%>%
  subset(year_trt == 0 | trt=="Control")%>%
  #block, plot, subplot
  ddply( c("site_code", "year", "plot", "subplot"),
         function(x)data.frame(
           biomass = sum(x$mass)
         )) %>%
  ddply( c("site_code", "year", "plot"),
         function(x)data.frame(
           biomass = mean(x$biomass)
         )) %>%
  ddply( c("site_code", "year"),
         function(x)data.frame(
           biomass = mean(x$biomass)
         )) %>%
  ddply( c("site_code"),
         function(x)data.frame(
           biomass = mean(x$biomass),
           n.year = length(x$year)
         )) %>%
  subset(n.year > 4)

control_biomass_NN$network <- "NutNet"

dn_nn <- rbind(control_biomass_DN,control_biomass_NN)%>%
        left_join(clim, by = "site_code")
dn_nn <- subset(dn_nn, biomass < 2000)
dn_nn <- subset(dn_nn, MAP != "NA")



##Correlation between climate variales
dn_nn%>%
  dplyr::select( ppt_max_event, ppt_mean_event, days_half_ppt, daily_ppt_d, n_wet_days, avg_dryspell_length, ppt_95th_percentile_size, MAP, cv_ppt_intra, cv_ppt_inter, yearly_ppt_d, seasonality_index, aridity_index, r_monthly_t_p)%>%
  pairs()



#MAP models
map <- lm(biomass~MAP, data=dn_nn)
map.2 <- lm(biomass~MAP+I(MAP^2), data=dn_nn)
d <- lm(biomass~yearly_ppt_d, data=dn_nn)
map.d <- lm(biomass~MAP * yearly_ppt_d, data=dn_nn)
map.d.2 <- lm(biomass~(MAP+I(MAP^2)) * yearly_ppt_d, data=dn_nn)


AIC(map,map.2,d,map.d, map.d.2)
summary(map)
summary(map.2)
summary(d)
summary(map.d)
summary(map.d.2)

visreg2d(map.d.2, xvar = "MAP", "yearly_ppt_d", plot.type = "gg")+
    geom_point(data=dn_nn, aes(MAP, yearly_ppt_d))




map <- lm(biomass~MAP, data=dn_nn)
map.2 <- lm(biomass~MAP+I(MAP^2), data=dn_nn)
s <- lm(biomass~r_monthly_t_p, data=dn_nn)
map.s <- lm(biomass~MAP * r_monthly_t_p, data=dn_nn)
map.s.2 <- lm(biomass~(MAP+I(MAP^2)) * r_monthly_t_p, data=dn_nn)


AIC(map,map.2,s,map.s, map.s.2)
summary(map)
summary(map.2)
summary(s)
summary(map.s)
summary(map.s.2)



map <- lm(biomass~MAP, data=dn_nn)
map.2 <- lm(biomass~MAP+I(MAP^2), data=dn_nn)
s <- lm(biomass~days_half_ppt, data=dn_nn)
map.s <- lm(biomass~MAP * days_half_ppt, data=dn_nn)
map.s.2 <- lm(biomass~(MAP+I(MAP^2)) * days_half_ppt, data=dn_nn)

AIC(map,map.2,s,map.s, map.s.2)
summary(map)
summary(map.2)
summary(s)
summary(map.s)
summary(map.s.2)

visreg2d(map.s.2, xvar = "MAP", "days_half_ppt", plot.type = "gg")+
  geom_point(data=dn_nn, aes(MAP, days_half_ppt))



map <- lm(biomass~MAP, data=dn_nn)
map.2 <- lm(biomass~MAP+I(MAP^2), data=dn_nn)
s <- lm(biomass~daily_ppt_d, data=dn_nn)
map.s <- lm(biomass~MAP * daily_ppt_d, data=dn_nn)
map.s.2 <- lm(biomass~(MAP+I(MAP^2)) * daily_ppt_d, data=dn_nn)

AIC(map,map.2,s,map.s, map.s.2)
summary(map)
summary(map.2)
summary(s)
summary(map.s)
summary(map.s.2)

visreg2d(map.s.2, xvar = "MAP", "daily_ppt_d", plot.type = "gg")+
  geom_point(data=dn_nn, aes(MAP, daily_ppt_d))
