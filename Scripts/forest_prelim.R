install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata","paletteer"))

library("ggplot2")
theme_set(theme_bw())
library("sf")
library("paletteer")
library("rnaturalearth")
library("rnaturalearthdata")
library(tidyverse)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

library(tidyverse)
library(plyr)
library(lmerTest)
library(nlme)
library(visreg)
library(MuMIn)
library(ggthemes)
library(ggeffects)
library(MASS)
library(emmeans)
library(cowplot)
library(rsq)


#read ANPP data
data.anpp <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/anpp_ppt_2023-11-03.csv")%>%
  subset(habitat.type == "Forest")#%>%

data.cover <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/cover_ppt_2024-02-12.csv")%>%
  subset(habitat.type == "Forest")#%>%


length(unique(data.anpp$site_code)) #13

length(unique(data.cover$site_code)) #3

#prop <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/community_comp/Prc_LifeHistory_Controls_Oct2023.csv")

Site_Elev.Disturb <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/Site_Elev-Disturb.csv")


forest <- data.anpp%>%
          left_join(Site_Elev.Disturb, by = "site_code")

forest%>%
dplyr::select(site_code, habitat, precip, temp)%>%
  unique()

####MAKE THE MAP
ggplot(data = world) +
  geom_sf()+
  xlab("Longitude") + ylab("Latitude") +
  geom_point(data = forest, aes(x = longitud, y = latitud), size = 4, color = "forestgreen")+
  theme_bw()

ggplot(forest, aes(x = temp, y = precip))+
  geom_point(color = "forestgreen", size = 4)+
  xlim(0,30)+
  ylim(0,3500)+
  xlab("Mean annual temperature")+
  ylab( "Mean annual precipitation")+
  theme_base()



##
forest_summary <- forest%>%
              dplyr::select(site_code, n_treat_years)%>%
                  unique()

forest_sites <- unique(forest_summary$site_code)

unique(forest_summary$site_code)

forest%>%
  subset(site_code == "wayqecha.pe" )%>%
ggplot(aes(x=as.factor(n_treat_years), y = mass, color = trt))+
  geom_point()+
  scale_color_manual(values = c("blue", "red"))+
  ggtitle("wayqecha.pe" )+
  theme_bw()

for(i in forest_sites){
  tempdf <-   forest%>%
     subset(site_code == i)
  
  
  ggplot(tempdf,aes(x=as.factor(n_treat_years), y = mass, color = trt))+
    geom_point()+
    scale_color_manual(values = c("blue", "red"))+
    ggtitle(i)+
    theme_bw()

}



ggplot(forest, aes())


forest%>%
  subset(n_treat_years > 0)%>%
  ddply(.(site_code, n_treat_years, trt), function(x)data.frame(
    mass = mean(x$mass),
    mass.se = sd(x$mass)/sqrt(length(x$site_code))
  ))%>%
ggplot(aes(n_treat_years, y=mass))+
  geom_pointrange(aes(ymin = mass-mass.se, ymax = mass+mass.se, color = trt))+
  facet_wrap(~site_code)+
  scale_color_manual(values = c("blue", "red"))


