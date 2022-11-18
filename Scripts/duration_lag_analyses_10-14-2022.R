library(tidyverse)
library(plyr)
library(lmerTest)



anpp_ppt_map <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/anpp_ppt_10-12-2022.csv")

Site_Elev.Disturb <- read.csv("C:/Users/ohler/Dropbox/IDE MS_Single year extreme/Data/Site_Elev-Disturb.csv")

anpp_ppt_map <- anpp_ppt_map%>%
  left_join( Site_Elev.Disturb, by = "site_code")%>%
  subset(habitat.type == "Grassland" | habitat.type == "Shrubland")%>%
  subset(ppt.1 >0 &ppt.2 >0 &ppt.3 >0 &ppt.4 >0 )#%>%
#ddply(.(ste_code, year, trt, ppt.1, ppt.2, ppt.3, ppt.4))


mod.1 <- lmer(mass~ppt.1 + (1|site_code), data=anpp_ppt_map)
mod.2 <- lmer(mass~ppt.1 + ppt.2 + (1|site_code), data=anpp_ppt_map)
mod.3 <- lmer(mass~ppt.1 + ppt.2 + ppt.3 + (1|site_code), data=anpp_ppt_map)                
mod.4 <- lmer(mass~ppt.1 + ppt.2 + ppt.3 + ppt.4 + (1|site_code), data=anpp_ppt_map)                
AIC(mod.1, mod.2, mod.3, mod.4)

summary(mod.1)
summary(mod.2)
summary(mod.3)
summary(mod.4)
visreg(mod.1)



#REMOVING FOREST SITES (Will leave this data to Rich Phillips)
data.noforest<-anpp_ppt_map %>%
  dplyr::filter(!habitat.type %in% c('Forest','Forest understory'))
length(unique(data.noforest$site_code)) #120 (16 sites removed)
setdiff(data.all$site_code,data.noforest$site_code)
# [1] "bamboo.cn"    "bivensarm.us" "cmss.us"      "elizwood.us"  "gigante.pa"   "horizon.cr"  
#[7] "hubbard.us"   "jilpanger.au" "kranz.de"     "p12.pa"       "p13.pa"       "prades.es"   
#[13] "sevforest.us" "sherman.pa"   "thompson.us"  "wayqecha.pe" 
#NOTE: Prades didn't follow IDE protocols, but also happens to be a forest site

#Removing sites here that DO NOT report ANPP or are outside of ANPP range for biome listed in Fahey & Knapp 2007:
data.anpp<-data.noforest[which(data.noforest$site_code!="lcnorth.cl" #Doesn't report ANPP
                               &data.noforest$site_code!="lcsouth.cl" #Doesn't report ANPP
                               &data.noforest$site_code!="qdtnorth.cl" #Doesn't report ANPP
                               &data.noforest$site_code!="qdtsouth.cl" #Doesn't report ANPP
                               &data.noforest$site_code!="neudamm.na" #Doesn't report ANPP, also no weather info
                               &data.noforest$site_code!="ebro.es" #ANPP outside range for biome
                               &data.noforest$site_code!="garraf.es" #Did not follow protocols: Not using for drought plots, only using control plots for this site
                               &data.noforest$site_code!="brandjberg.dk" #Did not follow protocols: Not using for drought plots, only using control plots for this site
                               &data.noforest$site_code!="ethadb.au" #ANPP outside range for biome
                               &data.noforest$site_code!="ethadn.au" #ANPP outside range for biome
                               &data.noforest$site_code!="swift.ca" #did not follow IDE protocols (drought plots did not exclude water- see Jillian email)
                               &data.noforest$site_code!="jenadrt.de" #did not have the correct data for their first treatment year because they lost a biomass bag and 
                               #requested their data be removed (see GitHub for details), will include them in "Drought shelters were not in place for 
                               #120-650 days (+/- one week)
),] 

length(unique(data.anpp$site_code)) #108

#Only using sites with >= 2 reps for drought and >=1 rep for control
#From Mendy (11-08-2021):
#Thanks for the reminder - I think that the site should have 
#at least 2 or more drought plots but could have 1 or more 
#control plots. Given we are trying to see a drought effect, 
#if the site has only one drought plot I would not trust the 
#estimate of the treatment effect.

#Counting the number of reps for each treatment and year
uniq.plot<- data.anpp %>% 
  dplyr::filter(trt %in% c("Drought","Control"))%>%
  dplyr::select(site_code,year,plot,trt)%>%
  dplyr::distinct(site_code,year,plot,trt)%>%
  dplyr::as_tibble()

Plot.trt.ct <- uniq.plot%>% dplyr::group_by(site_code,trt,year) %>% dplyr::summarise(Plot.count=n())

#Switching to wide format to see which sites do not have both treatments in a year
Plottrt_wide<-spread(Plot.trt.ct,trt,Plot.count)
Plottrt_wide[is.na(Plottrt_wide)] <- 0

#Remove sites and years that don't have both control and drought plots
#or that only have one rep of drought
#brokenh.au in 2019 (0 drought plots)
#chacra.ar in 2016 (1 control plot) #Keeping
#chilcasdrt.ar in 2016 and 2020 (0 drought plots)
#charleville.au in 2017 (1 control plot) #Keeping
#cobar.au in 2018 (1 control plot) #Keeping

Plottrt_wide1<-Plottrt_wide%>%
  dplyr::filter(Drought>=3 & Control>=3)%>%
  dplyr::as_tibble()

#Switch back to long format for merge
Plot.trt.ct2<-gather(Plottrt_wide1,trt,Plot.count,Drought:Control)

#Merge unique trt count back with data frame to filter
data.anpp1<-merge(data.anpp,Plot.trt.ct2,by=c("site_code","trt","year"))

setdiff(data.anpp$site_code,data.anpp1$site_code) #no sites eliminated here
length(unique(data.anpp1$site_code)) #108


controls <- data.anpp1%>%
  subset(trt == "Control" )%>%
  ddply(.(site_code, year),
        function(x)data.frame(
          mean.control = mean(x$mass)
        ))

data.anpp2 <- merge(data.anpp1, controls, by = c("site_code", "year"))%>%
  subset(trt == "Drought")


data.anpp2$anpp_response <- log(data.anpp2$mass/data.anpp2$mean.control)
data.anpp2$drtsev.1 <- ((data.anpp2$ppt.1-data.anpp2$precip)/data.anpp2$precip)
data.anpp2$drtsev.2 <- ((data.anpp2$ppt.2-data.anpp2$precip)/data.anpp2$precip)
data.anpp2$drtsev.3 <- ((data.anpp2$ppt.3-data.anpp2$precip)/data.anpp2$precip)
data.anpp2$drtsev.4 <- ((data.anpp2$ppt.4-data.anpp2$precip)/data.anpp2$precip)


mod.1 <- lmer(anpp_response~drtsev.1 + (1|site_code:plot), data=data.anpp2)
mod.2 <- lmer(anpp_response~drtsev.1 + drtsev.2 + (1|site_code:plot), data=data.anpp2)
mod.3 <- lmer(anpp_response~drtsev.1 + drtsev.2 + drtsev.3 + (1|site_code:plot), data=data.anpp2)                
mod.4 <- lmer(anpp_response~drtsev.1 + drtsev.2 + drtsev.3 + drtsev.4 + (1|site_code:plot), data=data.anpp2)                
AIC(mod.1, mod.2, mod.3, mod.4)

summary(mod.1)
r.squaredGLMM(mod.1)
summary(mod.2)
r.squaredGLMM(mod.2)
summary(mod.2.revised)
r.squaredGLMM(mod.2.revised)
summary(mod.3)
r.squaredGLMM(mod.3)
summary(mod.4)
r.squaredGLMM(mod.4)
visreg(mod.2)
visreg2d(mod.2, "drtsev.1", "drtsev.2")


mod.1 <- lmer(anpp_response~drtsev.1 + (1|site_code/year), data=subset(data.anpp2, n_treat_days>1145))
mod.2 <- lmer(anpp_response~drtsev.1 + drtsev.2 + (1|site_code/year), data=subset(data.anpp2, n_treat_days>1145))
mod.2.revised <- lmer(anpp_response~ drtsev.2 + (1|site_code/year), data=subset(data.anpp2, n_treat_days>1145))
mod.3 <- lmer(anpp_response~drtsev.1 + drtsev.2 + drtsev.3 + (1|site_code/year), data=subset(data.anpp2, n_treat_days>1145))                
mod.4 <- lmer(anpp_response~drtsev.1 + drtsev.2 + drtsev.3 + drtsev.4 + (1|site_code/year), data=subset(data.anpp2, n_treat_days>1145))                
AIC(mod.1, mod.2, mod.2.revised, mod.3, mod.4)

summary(mod.1)
r.squaredGLMM(mod.1)
summary(mod.2)
r.squaredGLMM(mod.2)
summary(mod.2.revised)
r.squaredGLMM(mod.2.revised)
summary(mod.3)
r.squaredGLMM(mod.3)
summary(mod.4)
r.squaredGLMM(mod.4)





data.anpp.summary <- data.anpp2%>%
  ddply(.(site_code, year, drtsev.1,drtsev.2,drtsev.3,drtsev.4),
        function(x)data.frame(
          anpp_response = mean(x$anpp_response)
        ))


#interactive
mod.1 <- lmer(anpp_response~drtsev.1 + (1|site_code), data=data.anpp.summary, REML = FALSE)
mod.2 <- lmer(anpp_response~drtsev.1 * drtsev.2 + (1|site_code), data=data.anpp.summary, REML = FALSE)
mod.3 <- lmer(anpp_response~drtsev.1 * drtsev.2 * drtsev.3 + (1|site_code), data=data.anpp.summary, REML = FALSE)
mod.4 <- lmer(anpp_response~drtsev.1 * drtsev.2 * drtsev.3 * drtsev.4 + (1|site_code), data=data.anpp.summary, REML = FALSE)  

#additive
mod.5 <- lmer(anpp_response~drtsev.1 + (1|site_code), data=data.anpp.summary, REML = FALSE)
mod.6 <- lmer(anpp_response~drtsev.1 + drtsev.2 + (1|site_code), data=data.anpp.summary, REML = FALSE)
mod.7 <- lmer(anpp_response~drtsev.1 + drtsev.2 + drtsev.3 + (1|site_code), data=data.anpp.summary, REML = FALSE)
mod.8 <- lmer(anpp_response~drtsev.1 + drtsev.2 + drtsev.3 + drtsev.4 + (1|site_code), data=data.anpp.summary, REML = FALSE) 
mod.9 <- lmer(anpp_response~drtsev.1 + drtsev.1:drtsev.2 + (1|site_code), data=data.anpp.summary, REML = FALSE)
AIC(mod.1, mod.2, mod.3, mod.4, mod.5, mod.6, mod.7, mod.8, mod.9)

summary(mod.1)
r.squaredGLMM(mod.1)
summary(mod.2)
r.squaredGLMM(mod.2)
summary(mod.3)
r.squaredGLMM(mod.3)
summary(mod.4)
r.squaredGLMM(mod.4)
summary(mod.9)
r.squaredGLMM(mod.9)
visreg(mod.1)
visreg(mod.2)
visreg2d(mod.2, "drtsev.1", "drtsev.2", plot.type="gg", col = c("red", "white", "forestgreen"))+
  geom_point(data = data.anpp.summary, aes(x=drtsev.1, y=drtsev.2))+
  xlab("Current year drought severity")+
  ylab("Previous year drought severity")

visreg2d(mod.9, "drtsev.1", "drtsev.2", plot.type="gg", col = c("red", "white", "forestgreen"))+
  geom_point(data = data.anpp.summary, aes(x=drtsev.1, y=drtsev.2))+
  xlab("Current year drought severity")+
  ylab("Previous year drought severity")









###Delete later
x <- subset(data.anpp2, n_treat_days > 90 & n_treat_days < 420)


y <- ddply(x, .(site_code, year),
           function(x)data.frame(
             anpp_response = mean(x$anpp_response)
           ))


