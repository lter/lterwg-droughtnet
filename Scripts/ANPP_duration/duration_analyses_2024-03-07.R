###########Timbo slice uses this script to generate stats and figures for the IDE ANPP duration MS!

library(tidyverse)
library(plyr)
library(lmerTest)
library(nlme)
library(visreg)
library(MuMIn)
library(ggthemes)
library(ggeffects)
library(MASS)
library(cowplot)
library(rsq)
library(emmeans)

#read ANPP data
data.anpp <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/anpp_ppt_2024-07-18.csv")%>% #anpp_ppt_2023-11-03.csv
  subset(habitat.type == "Grassland" | habitat.type == "Shrubland")%>%
  mutate(n_treat_years = ifelse(site_code == "allmendo.ch" & n_treat_days == 197, 1, n_treat_years))%>%
  mutate(n_treat_years = ifelse(site_code == "allmendb.ch" & n_treat_days == 197, 1, n_treat_years))%>%
  mutate(n_treat_years = ifelse(site_code == "torla.es" & n_treat_days == 195, 1, n_treat_years))

length(unique(data.anpp$site_code)) #114

prop <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/community_comp/Prc_LifeHistory_Controls_Oct2023.csv")

Site_Elev.Disturb <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/Site_Elev-Disturb.csv")

#create long-term average ANPP in controls
anpp.mean <- data.anpp%>%
  subset(trt == "Control")%>%
  ddply(.(site_code, year),function(x)data.frame(mass = mean(x$mass)))%>% #summarizes controls for each year
  ddply(.(site_code),function(x)data.frame(mean.mass = mean(x$mass), n_years = length(x$mass)))#summarizes controls across years


##Calculate which years are extreme vs nominal for each site in each year
extremeyrs <- subset(data.anpp, trt == "Control")%>%
  dplyr::select(site_code, n_treat_years, year, ppt.1, map)%>%
  unique() %>%
  mutate(ppt.minus.map=ppt.1-map,
         e.n=ifelse(n_treat_years <1, NA,
                    ifelse(ppt.minus.map>0, "nominal", "extreme"))) %>%
  dplyr::select(site_code, year, n_treat_years, e.n)

extremeyrs.prev <- extremeyrs%>%
  dplyr::select(site_code, year, e.n)%>%
  dplyr::rename(prev_e.n = e.n)
extremeyrs.prev$year <- extremeyrs.prev$year + 1


extremeyrs.prev2 <- extremeyrs%>%
  dplyr::select(site_code, year, e.n)%>%
  dplyr::rename(prev_e.n2 = e.n)
extremeyrs.prev2$year <- extremeyrs.prev$year + 1 #had some trouble with these addition things but I think it's fine now

extremeyrs.prev3 <- extremeyrs%>%
  dplyr::select(site_code, year, e.n)%>%
  dplyr::rename(prev_e.n3 = e.n)
extremeyrs.prev3$year <- extremeyrs.prev$year + 2

#Only using sites with >= 2 reps for drought and >=1 rep for control
#Counting the number of reps for each treatment and year
uniq.plot<- data.anpp %>% 
  dplyr::filter(trt %in% c("Drought","Control"))%>%
  dplyr::select(site_code,year,plot,trt)%>%
  dplyr::distinct(site_code,year,plot,trt)%>%
  dplyr::as_tibble()

Plot.trt.ct <- uniq.plot%>% dplyr::group_by(site_code,trt,year) %>% dplyr::summarise(Plot.count=dplyr::n())

#Switching to wide format to see which sites do not have both treatments in a year
Plottrt_wide<-tidyr::spread(Plot.trt.ct,trt,Plot.count)
Plottrt_wide[is.na(Plottrt_wide)] <- 0

#Remove sites and years that don't have both control and drought plots
#or that only have one rep of drought
Plottrt_wide1<-Plottrt_wide%>%
  dplyr::filter(Drought>=2 & Control>=1)%>%
  dplyr::as_tibble()

#Switch back to long format for merge
Plot.trt.ct2<-tidyr::gather(Plottrt_wide1,trt,Plot.count,Drought:Control)

#Merge unique trt count back with data frame to filter
data.anpp1<-merge(data.anpp,Plot.trt.ct2,by=c("site_code","trt","year"))

setdiff(data.anpp$site_code,data.anpp1$site_code) #"brandjberg.dk" "garraf.es"  "swift.ca" eliminated here
length(unique(data.anpp1$site_code)) #111

##How many treatment years does each site have of the first 3 years?
num.treat.years <- data.anpp1[,c("site_code", "n_treat_years")]%>%
  unique()%>%
  subset(n_treat_years>=1&n_treat_years<=4)

num.treat.years <- ddply(num.treat.years,.(site_code),
                         function(x)data.frame(
                           num.years = length(x$n_treat_years)
                         ))

#reduces dataset to focal sites
data.anpp2 <- merge(data.anpp1, anpp.mean, by = c("site_code"))%>%
  subset(trt == "Drought")%>%
  subset(n_years>=4)%>% #Does long-term control average include at least 4 years?
  left_join(num.treat.years, by = "site_code")%>%
  left_join(extremeyrs, by = c("site_code", "year", "n_treat_years"))%>%
  left_join(extremeyrs.prev, by = c("site_code", "year"))%>%
  left_join(extremeyrs.prev2, by = c("site_code", "year"))%>%
  left_join(extremeyrs.prev3, by = c("site_code", "year"))%>%
  subset(num.years == 4 | num.years == 3
  ) %>%#change here if using 4 years #reduces dataset to focal sites
  left_join(prop, by = c("site_code"))%>%
  subset(site_code != "stubai.at")%>%#stubai is not a year-round drought so shouldn't be compared against these other sites
  subset(site_code != "sclaudio.ar") #sclaudio missed Y3 sampling (2020)
  #subset(site_code == "allmendo.ch" & site_code == "allmendb.ch" & site_code == "torla.es") #allmendo, allmendb, and torla have first treatment dates in the 190s, sclaudio missed Y3 sampling (2020)


data.anpp2$Ann_Per <- ifelse(data.anpp2$PctAnnual > 60, "Annual", 
                             ifelse(data.anpp2$PctAnnual <= 60, "Perennial",
                                    "NA"))

data.anpp2$Ann_Per <- ifelse(is.na(data.anpp2$Ann_Per) == TRUE, "Perennial", data.anpp2$Ann_Per) #morient.ar, b=nyngan.au, riomayo.ar, stubai.at, and syferkuil.za don't have cover data, but based on biomass and site info data that they submitted we can say that they are all perennial grassland.


length(unique(data.anpp2$site_code)) #74

#mean(subset(data.anpp.summary, n_treat_years == 1)$n_treat_days)



##Create anpp_response and drought severity metrics
data.anpp2$anpp_response <- log(data.anpp2$mass/data.anpp2$mean.mass)
data.anpp2$percent.reduction <- 100 * ((data.anpp2$mass/data.anpp2$mean.mass)-1)
data.anpp2$drtsev.1 <- -((data.anpp2$ppt.1-data.anpp2$map)/data.anpp2$map)
data.anpp2$drtsev.2 <- -((data.anpp2$ppt.2-data.anpp2$map)/data.anpp2$map)
data.anpp2$drtsev.3 <- -((data.anpp2$ppt.3-data.anpp2$map)/data.anpp2$map)
data.anpp2$drtsev.4 <- -((data.anpp2$ppt.4-data.anpp2$map)/data.anpp2$map)




##Summarize responses by site and year
data.anpp.summary <- data.anpp2%>%
  ddply(.(site_code, year, drtsev.1,drtsev.2,drtsev.3,drtsev.4, n_treat_years, map, habitat.type, e.n, prev_e.n, prev_e.n2, prev_e.n3, PctAnnual, Ann_Per, ipcc_regions),
        function(x)data.frame(
          mean_mass = mean(x$mass),
          ppt.1 = mean(x$ppt.1),
          anpp_response = mean(x$anpp_response),
          anpp_response.error = qt(0.975, df=length(x$habitat.type)-1)*sd(x$anpp_response, na.rm = TRUE)/sqrt(length(x$habitat.type)-1),
          anpp_response.se = sd(x$anpp_response, na.rm = TRUE)/sqrt(length(x$site_code)),
          
          percent.reduction = mean(x$percent.reduction),
          percent.reduction.error = qt(0.975, df=length(x$habitat.type)-1)*sd(x$percent.reduction, na.rm = TRUE)/sqrt(length(x$habitat.type)-1),
          percent.reduction.se = sd(x$percent.reduction, na.rm = TRUE)/sqrt(length(x$site_code)),
          n_treat_days = mean(x$n_treat_days)
        ))%>%
  #subset(n_treat_years == 0 | n_treat_years == 1 | n_treat_years == 2 | n_treat_years == 3) #CHANGE HERE IF YOU"RE GOING UP TO 4 TREATMENT YEARS
  subset(n_treat_years >= 1 & n_treat_years<= 4) #CHANGE HERE IF YOU"RE GOING UP TO 4 TREATMENT YEARS

data.anpp.summary$type <- ifelse(data.anpp.summary$Ann_Per == "Annual" & is.na(data.anpp.summary$Ann_Per) == FALSE , data.anpp.summary$Ann_Per, data.anpp.summary$habitat.type)


data.anpp.summary$type <-   plyr::revalue(data.anpp.summary$type, c(Grassland = "Herbaceous.Perennial", Shrubland = "Woody.Perennial"))


#of sites by prevailing vegetation type
data.anpp.summary%>%
  dplyr::select(site_code, type, map)%>%
  unique()%>%
  write.csv("C:/Users/ohler/Downloads/IDE_duration_sites.csv")
#  group_by(type)%>%
#  tally()

#table site info

table_S6 <- data.anpp.summary%>%
  ddply(c("site_code", "type"), function(x)data.frame(
    avg_precip_reduction = mean(x$drtsev.1, na.rm = TRUE)
  ))%>%
  left_join(Site_Elev.Disturb, by = "site_code")%>%
  dplyr::mutate(target.reduction = as.numeric(drought_trt)/100)%>%
  dplyr::select(site_name, site_code, country, continent, type, latitud, longitud, precip, temp, target.reduction, avg_precip_reduction)
  
#write.csv(table_S6, "C:/Users/ohler/Dropbox/IDE_Duration_ms/site_table.csv" )
 # ddply(.(site_code, type), function(x) data.frame(
#    anpp_response = mean(x$anpp_response),
#  ))%>%
  

  


  tempdf <- data.anpp.summary%>%
    ddply(.(site_code, type), function(x) data.frame(
      anpp_response = mean(x$anpp_response),
      anpp_response.se = sd(x$anpp_response)/sqrt(length(x$n_treat_years))
    ))
  #as.numeric(drought_trt)/100)
  
  

tempsites <- data.anpp.summary%>%
  subset(type == "Annual" & site_code != "cobar.au")%>%
  subset(n_treat_years == 2 | n_treat_years == 3)
mean(subset(tempsites, n_treat_years == 2)$anpp_response)
mean(subset(tempsites, n_treat_years == 3)$anpp_response)

mean(subset(tempsites, n_treat_years == 2)$drtsev.1)
mean(subset(tempsites, n_treat_years == 3)$drtsev.1)


tempdf <-subset(data.anpp.summary, Ann_Per == "Perennial")
mod <- lme(anpp_response~1, random = ~1|ipcc_regions/site_code, method = "ML", data=tempdf)
summary(mod)

tempdf <-subset(data.anpp.summary, Ann_Per == "Perennial" & e.n == "extreme")
mod <- lme(anpp_response~1, random = ~1|ipcc_regions/site_code, method = "ML", data=tempdf)
summary(mod)

tempdf <-subset(data.anpp.summary, Ann_Per == "Perennial" & e.n == "nominal")
mod <- lme(anpp_response~1, random = ~1|ipcc_regions/site_code, method = "ML", data=tempdf)
summary(mod)


##Here we generate the stats with continuous drought severity
#Backward model selection - skipping backward in favor of forward since backward likes the maximal model for some ungodly reason
tempdf <-subset(data.anpp.summary, n_treat_years == 4& Ann_Per == "Perennial")

#lmFull <- lme(anpp_response~drtsev.1 * drtsev.2, random = ~1|ipcc_regions,method = "ML", data=tempdf)
lmFull <- lmer(anpp_response~drtsev.1 * drtsev.2 * drtsev.3 * drtsev.4 + (1|ipcc_regions), data=tempdf)


lmNull <- lme(anpp_response~1, random = ~1|ipcc_regions,method = "ML",  data = tempdf)
#lmNull <- lmer(anpp_response~1+(1|ipcc_regions),  data = tempdf)

AIC(lmFull, lmNull)
#Forward model selection
stepAIC(lmNull, scope = list(upper = lmFull,
                             lower = ~1),
        trace = F)


winning.mod <- lme(anpp_response ~ drtsev.1, random = ~1|ipcc_regions,method = "ML",data = tempdf)
summary(winning.mod)




r.squaredGLMM(winning.mod)


d<-dredge(lmFull)
sw(d)

history.df <- data.anpp.summary%>% #need details about extreme vs nominanl just for figure
  dplyr::select(site_code, n_treat_years, e.n)%>%
  pivot_wider(names_from = n_treat_years, values_from = e.n)%>%
  dplyr::rename(y1 = "1", y2 = "2", y3 = '3')




##add some covariates to try out in model selection
#sand, MAP, ln(aridity), CV of MAP--- %graminoids is tricky, skpping for now
sandsite <-read.csv("C:\\Users\\ohler\\Dropbox\\IDE MS_Single year extreme\\Data\\site_sand_from_soilgrid.csv")%>%
  dplyr::select(site_code, sand_mean)

ai<-read.csv("C:\\Users\\ohler\\Dropbox\\IDE MS_Single year extreme\\Data\\ai_pet_04-18-2022.csv")%>%
  dplyr::select(site_code, AI)

cv<-read.csv("C:\\Users\\ohler\\Dropbox\\IDE\\data_processed\\climate\\climate_mean_annual_by_site_v2.csv")
cv1<-cv%>%
  #dplyr::filter(data_source=="mswep")%>%
  dplyr::select(site_code,cv_ppt_inter)

graminoid_richness <-read.csv("C:\\Users\\ohler\\Dropbox\\IDE\\data_processed\\graminoids_and_richness.csv")

seasonality <- read.csv("C:\\Users\\ohler\\Dropbox\\IDE\\data_processed\\climate\\climate_mean_annual_by_site_v2.csv")%>%
  dplyr::select(site_code, seasonality_index, r_monthly_t_p)



##Create planeled figure for supplements (and some stats to go along with them)
#YEAR 1
mult_reg <- data.anpp.summary%>%
  subset(n_treat_years == "1"&Ann_Per == "Perennial")%>%
  left_join(sandsite, by = "site_code")%>%
  left_join(ai, by = "site_code")%>%
  left_join(cv1, by = "site_code")%>%
  left_join(graminoid_richness, by = "site_code")%>%
  left_join(seasonality, by = "site_code")
  #subset(richness >= 0)

library("PerformanceAnalytics")

mod <- lmer(anpp_response~drtsev.1 + (1|ipcc_regions), data = mult_reg)
summary(mod)


a <- data.anpp.summary%>%
  #left_join(history.df, by = "site_code")%>%
  subset(n_treat_years == "1"&Ann_Per == "Perennial")%>%
  ggplot(aes(map, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
 # geom_smooth(method = "lm", se = TRUE, color = "black")+
  xlab("MAP")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+#1E4D2B", "#C8C372"
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

mod <- lme(anpp_response~map, random = ~1|ipcc_regions, data = subset(data.anpp.summary, n_treat_years == "1"&Ann_Per == "Perennial"))
summary(mod)
r.squaredGLMM(mod)

b <- data.anpp.summary%>%
  left_join(sandsite, by = "site_code")%>%
  subset(n_treat_years == "1"&Ann_Per == "Perennial")%>%
  ggplot(aes(sand_mean, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  #geom_smooth(method = "lm", se = TRUE)+
  xlab("Average sand content")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(sandsite, by = "site_code")%>%
  dplyr::select(anpp_response, sand_mean, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "1"& Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~sand_mean,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)


c <- data.anpp.summary%>%
  left_join(ai, by = "site_code")%>%
  subset(n_treat_years == "1"&Ann_Per == "Perennial")%>%
  ggplot(aes(AI, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  xlim(0,2)+
#  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed")+ #marginal when controlling for multiple comparisons
  xlab("Aridity index")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(ai, by = "site_code")%>%
  dplyr::select(anpp_response, AI, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "1"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~AI,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)


d <- data.anpp.summary%>%
  left_join(cv1, by = "site_code")%>%
  subset(n_treat_years == "1"&Ann_Per == "Perennial")%>%
  ggplot(aes(cv_ppt_inter, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
#  geom_smooth(method = "lm", se = TRUE, color = "black")+
  xlab("Interannual precip CV")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(cv1, by = "site_code")%>%
  dplyr::select(anpp_response, cv_ppt_inter, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "1"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~cv_ppt_inter, random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)



e <- data.anpp.summary%>%
  left_join(graminoid_richness, by = "site_code")%>%
  subset(n_treat_years == "1"&Ann_Per == "Perennial")%>%
  ggplot(aes(percent_graminoid, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  #geom_smooth(method = "lm", se = TRUE)+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(graminoid_richness, by = "site_code")%>%
  dplyr::select(anpp_response, percent_graminoid, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "1"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~percent_graminoid,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)

f <- data.anpp.summary%>%
  left_join(graminoid_richness, by = "site_code")%>%
  subset(n_treat_years == "1"&Ann_Per == "Perennial")%>%
  ggplot(aes(richness, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  geom_smooth(method = "lm", se = TRUE, color = "black")+
  xlab("Richness")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(graminoid_richness, by = "site_code")%>%
  dplyr::select(anpp_response, richness, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "1"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~richness,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)


g <- data.anpp.summary%>%
  left_join(seasonality, by = "site_code")%>%
  subset(n_treat_years == "1"&Ann_Per == "Perennial")%>%
  ggplot(aes(seasonality_index, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
#  geom_smooth(method = "lm",  se = TRUE, color = "black")+
  xlab("Seasonality")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(seasonality, by = "site_code")%>%
  dplyr::select(anpp_response, seasonality_index, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "1"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~seasonality_index,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)

h <- data.anpp.summary%>%
  left_join(seasonality, by = "site_code")%>%
  subset(n_treat_years == "1"&Ann_Per == "Perennial")%>%
  ggplot(aes(drtsev.2, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
#  geom_smooth(method = "lm",  se = TRUE, color = "black")+
  xlab("Previous year's drought severity")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(seasonality, by = "site_code")%>%
  dplyr::select(anpp_response, drtsev.2, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "1"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~drtsev.2,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)


i <- data.anpp.summary%>%
  subset(n_treat_years == "1"&Ann_Per == "Perennial")%>%
  ggplot(aes(drtsev.1, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  geom_smooth(method = "lm",  se = TRUE, color = "black")+
  xlab("Drought severity")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  dplyr::select(anpp_response, drtsev.1, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "1"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~drtsev.1,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)


j <- data.anpp.summary%>%
  subset(n_treat_years == "1"&Ann_Per == "Perennial")%>%
  left_join(Site_Elev.Disturb, by = "site_code")%>%
  ggplot(aes(temp, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
 # geom_smooth(method = "lm",  se = TRUE, color = "black")+
  xlab("MAT")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  dplyr::select(anpp_response, drtsev.1, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "1"&Ann_Per == "Perennial")%>%
  left_join(Site_Elev.Disturb, by = c("site_code", "ipcc_regions"))
mod <- lme(anpp_response~temp,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)




plot_grid(i,h,a, c, j,d, b, e, f, labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I'), nrow = 2)

ggsave(
  "C:/Users/ohler/Dropbox/IDE/figures/anpp_duration/covariate_supplemental_y1.pdf",
  plot = last_plot(),
  device = "pdf",
  path = NULL,
  scale = 1,
  width = 15,
  height = 6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE
)


###
#YEAR 2
mult_reg <- data.anpp.summary%>%
  subset(n_treat_years == "2"&Ann_Per == "Perennial")%>%
  left_join(sandsite, by = "site_code")%>%
  left_join(ai, by = "site_code")%>%
  left_join(cv1, by = "site_code")%>%
  left_join(graminoid_richness, by = "site_code")%>%
  left_join(seasonality, by = "site_code")#%>%
  #subset(richness > 0)

dplyr::select(mult_reg,drtsev.1,drtsev.2,map,AI,cv_ppt_inter,seasonality_index,sand_mean,percent_graminoid,richness)%>%
  chart.Correlation( histogram=TRUE, pch=19)


mod <- lmer(anpp_response~drtsev.1+#drtsev.2+map+#AI+
              #cv_ppt_inter+
              #seasonality_index+sand_mean+percent_graminoid+richness + 
            (1|ipcc_regions), data = mult_reg)
summary(mod)



a <- data.anpp.summary%>%
  #left_join(history.df, by = "site_code")%>%
  subset(n_treat_years == "2"&Ann_Per == "Perennial")%>%
  ggplot(aes(map, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed")+ #not significant when controlling for multiple comparisons
  xlab("MAP")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+#1E4D2B", "#C8C372"
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

mod <- lme(anpp_response~map, random = ~1|ipcc_regions, data = subset(data.anpp.summary, n_treat_years == "2"&Ann_Per == "Perennial"))
summary(mod)
r.squaredGLMM(mod)

b <- data.anpp.summary%>%
  left_join(sandsite, by = "site_code")%>%
  subset(n_treat_years == "2"&Ann_Per == "Perennial")%>%
  ggplot(aes(sand_mean, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  #geom_smooth(method = "lm", se = TRUE)+
  xlab("Average sand content")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(sandsite, by = "site_code")%>%
  dplyr::select(anpp_response, sand_mean, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "2"& Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~sand_mean,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)


c <- data.anpp.summary%>%
  left_join(ai, by = "site_code")%>%
  subset(n_treat_years == "2"&Ann_Per == "Perennial")%>%
  ggplot(aes(AI, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  xlim(0,2)+
#  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed")+ #marginal when controlling for multiple comparisons
  xlab("Aridity index")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(ai, by = "site_code")%>%
  dplyr::select(anpp_response, AI, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "2"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~AI,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)


d <- data.anpp.summary%>%
  left_join(cv1, by = "site_code")%>%
  subset(n_treat_years == "2"&Ann_Per == "Perennial")%>%
  ggplot(aes(cv_ppt_inter, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
#  geom_smooth(method = "lm", se = TRUE, color = "black")+
  xlab("Interannual precip CV")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(cv1, by = "site_code")%>%
  dplyr::select(anpp_response, cv_ppt_inter, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "2"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~cv_ppt_inter, random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)



e <- data.anpp.summary%>%
  left_join(graminoid_richness, by = "site_code")%>%
  subset(n_treat_years == "2"&Ann_Per == "Perennial")%>%
  ggplot(aes(percent_graminoid, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  #geom_smooth(method = "lm", se = TRUE)+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(graminoid_richness, by = "site_code")%>%
  dplyr::select(anpp_response, percent_graminoid, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "2"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~percent_graminoid,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)

f <- data.anpp.summary%>%
  left_join(graminoid_richness, by = "site_code")%>%
  subset(n_treat_years == "2"&Ann_Per == "Perennial")%>%
  ggplot(aes(richness, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  #geom_smooth(method = "lm", se = TRUE)+
  xlab("Richness")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(graminoid_richness, by = "site_code")%>%
  dplyr::select(anpp_response, richness, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "2"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~richness,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)


g <- data.anpp.summary%>%
  left_join(seasonality, by = "site_code")%>%
  subset(n_treat_years == "2"&Ann_Per == "Perennial")%>%
  ggplot(aes(seasonality_index, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  geom_smooth(method = "lm",  se = TRUE, color = "black", linetype = "dashed")+
  xlab("Seasonality")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(seasonality, by = "site_code")%>%
  dplyr::select(anpp_response, seasonality_index, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "2"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~seasonality_index,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)

h <- data.anpp.summary%>%
  left_join(seasonality, by = "site_code")%>%
  subset(n_treat_years == "2"&Ann_Per == "Perennial")%>%
  ggplot(aes(drtsev.2, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
#  geom_smooth(method = "lm",  se = TRUE, color = "black")+
  xlab("Previous year's drought severity")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(seasonality, by = "site_code")%>%
  dplyr::select(anpp_response, drtsev.2, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "2"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~drtsev.2,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)


i <- data.anpp.summary%>%
  subset(n_treat_years == "2"&Ann_Per == "Perennial")%>%
  ggplot(aes(drtsev.1, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  geom_smooth(method = "lm",  se = TRUE, color = "black")+
  xlab("Drought severity")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  dplyr::select(anpp_response, drtsev.1, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "2"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~drtsev.1,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)



j <- data.anpp.summary%>%
  subset(n_treat_years == "2"&Ann_Per == "Perennial")%>%
  left_join(Site_Elev.Disturb, by = "site_code")%>%
  ggplot(aes(temp, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  #geom_smooth(method = "lm",  se = TRUE, color = "black")+
  xlab("MAT")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  dplyr::select(anpp_response, drtsev.1, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "2"&Ann_Per == "Perennial")%>%
  left_join(Site_Elev.Disturb, by = c("site_code", "ipcc_regions"))
mod <- lme(anpp_response~temp,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)


plot_grid(i,h,a, c, j,d, b, e, f, labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I'), nrow = 2)

ggsave(
  "C:/Users/ohler/Dropbox/IDE/figures/anpp_duration/covariate_supplemental_y2.pdf",
  plot = last_plot(),
  device = "pdf",
  path = NULL,
  scale = 1,
  width = 15,
  height = 6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE
)




#YEAR 3
mult_reg <- data.anpp.summary%>%
  subset(n_treat_years == "3"&Ann_Per == "Perennial")%>%
  left_join(sandsite, by = "site_code")%>%
  left_join(ai, by = "site_code")%>%
  left_join(cv1, by = "site_code")%>%
  left_join(graminoid_richness, by = "site_code")%>%
  left_join(seasonality, by = "site_code")

mod <- lmer(anpp_response~drtsev.1+#drtsev.2+map+#AI+
              #cv_ppt_inter+
              #seasonality_index+sand_mean+percent_graminoid+richness + 
              (1|ipcc_regions), data = mult_reg)
summary(mod)


mod <- lmer(anpp_response~drtsev.1+drtsev.2+map+AI+cv_ppt_inter+seasonality_index+sand_mean+percent_graminoid+richness + (1|ipcc_regions), data = mult_reg)
summary(mod)


a <- data.anpp.summary%>%
  #left_join(history.df, by = "site_code")%>%
  subset(n_treat_years == "3"&Ann_Per == "Perennial")%>%
  ggplot(aes(map, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  geom_smooth(method = "lm", se = TRUE, color = "black")+
  xlab("MAP")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+#1E4D2B", "#C8C372"
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

mod <- lme(anpp_response~map, random = ~1|ipcc_regions, data = subset(data.anpp.summary, n_treat_years == "3"&Ann_Per == "Perennial"))
summary(mod)
r.squaredGLMM(mod)

b <- data.anpp.summary%>%
  left_join(sandsite, by = "site_code")%>%
  subset(n_treat_years == "3"&Ann_Per == "Perennial")%>%
  ggplot(aes(sand_mean, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  #geom_smooth(method = "lm", se = TRUE)+
  xlab("Average sand content")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(sandsite, by = "site_code")%>%
  dplyr::select(anpp_response, sand_mean, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "3"& Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~sand_mean,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)


c <- data.anpp.summary%>%
  left_join(ai, by = "site_code")%>%
  subset(n_treat_years == "3"&Ann_Per == "Perennial")%>%
  ggplot(aes(AI, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  xlim(0,2)+
  geom_smooth(method = "lm", se = TRUE, color = "black")+ 
  xlab("Aridity index")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(ai, by = "site_code")%>%
  dplyr::select(anpp_response, AI, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "3"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~AI,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)


d <- data.anpp.summary%>%
  left_join(cv1, by = "site_code")%>%
  subset(n_treat_years == "3"&Ann_Per == "Perennial")%>%
  ggplot(aes(cv_ppt_inter, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  geom_smooth(method = "lm", se = TRUE, color = "black")+
  xlab("Interannual precip CV")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(cv1, by = "site_code")%>%
  dplyr::select(anpp_response, cv_ppt_inter, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "3"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~cv_ppt_inter, random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)



e <- data.anpp.summary%>%
  left_join(graminoid_richness, by = "site_code")%>%
  subset(n_treat_years == "3"&Ann_Per == "Perennial")%>%
  ggplot(aes(percent_graminoid, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  #geom_smooth(method = "lm", se = TRUE)+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(graminoid_richness, by = "site_code")%>%
  dplyr::select(anpp_response, percent_graminoid, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "3"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~percent_graminoid,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)

f <- data.anpp.summary%>%
  left_join(graminoid_richness, by = "site_code")%>%
  subset(n_treat_years == "3"&Ann_Per == "Perennial")%>%
  ggplot(aes(richness, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  #geom_smooth(method = "lm", se = TRUE)+
  xlab("Richness")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(graminoid_richness, by = "site_code")%>%
  dplyr::select(anpp_response, richness, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "3"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~richness,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)


g <- data.anpp.summary%>%
  left_join(seasonality, by = "site_code")%>%
  subset(n_treat_years == "3"&Ann_Per == "Perennial")%>%
  ggplot(aes(seasonality_index, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  geom_smooth(method = "lm",  se = TRUE, color = "black", linetype = "dashed")+
  xlab("Seasonality")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(seasonality, by = "site_code")%>%
  dplyr::select(anpp_response, seasonality_index, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "3"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~seasonality_index,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)

h <- data.anpp.summary%>%
  left_join(seasonality, by = "site_code")%>%
  subset(n_treat_years == "3"&Ann_Per == "Perennial")%>%
  ggplot(aes(drtsev.2, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  geom_smooth(method = "lm",  se = TRUE, color = "black")+
  xlab("Previous year's drought severity")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(seasonality, by = "site_code")%>%
  dplyr::select(anpp_response, drtsev.2, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "3"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~drtsev.2,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)


i <- data.anpp.summary%>%
  subset(n_treat_years == "3"&Ann_Per == "Perennial")%>%
  ggplot(aes(drtsev.1, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  geom_smooth(method = "lm",  se = TRUE, color = "black")+
  xlab("Drought severity")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  dplyr::select(anpp_response, drtsev.1, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "3"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~drtsev.1,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)


j <- data.anpp.summary%>%
  subset(n_treat_years == "3"&Ann_Per == "Perennial")%>%
  left_join(Site_Elev.Disturb, by = "site_code")%>%
  ggplot(aes(temp, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  #geom_smooth(method = "lm",  se = TRUE, color = "black")+
  xlab("MAT")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  dplyr::select(anpp_response, drtsev.1, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "3"&Ann_Per == "Perennial")%>%
  left_join(Site_Elev.Disturb, by = c("site_code", "ipcc_regions"))
mod <- lme(anpp_response~temp,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)


plot_grid(i,h,a, c, j,d, b, e, f, labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I'), nrow = 2)

ggsave(
  "C:/Users/ohler/Dropbox/IDE/figures/anpp_duration/covariate_supplemental_y3.pdf",
  plot = last_plot(),
  device = "pdf",
  path = NULL,
  scale = 1,
  width = 15,
  height = 6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE
)

# # 
#YEAR 4
mult_reg <- data.anpp.summary%>%
  subset(n_treat_years == "4"&Ann_Per == "Perennial")%>%
  left_join(sandsite, by = "site_code")%>%
  left_join(ai, by = "site_code")%>%
  left_join(cv1, by = "site_code")%>%
  left_join(graminoid_richness, by = "site_code")%>%
  left_join(seasonality, by = "site_code")


mod <- lmer(anpp_response~drtsev.1#+drtsev.2+map+AI+cv_ppt_inter+seasonality_index+sand_mean+percent_graminoid+richness 
            + (1|ipcc_regions), data = mult_reg)
summary(mod)


a <- data.anpp.summary%>%
  #left_join(history.df, by = "site_code")%>%
  subset(n_treat_years == "4"&Ann_Per == "Perennial")%>%
  ggplot(aes(map, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed")+
  xlab("MAP")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+#1E4D2B", "#C8C372"
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

mod <- lme(anpp_response~map, random = ~1|ipcc_regions, data = subset(data.anpp.summary, n_treat_years == "4"&Ann_Per == "Perennial"))
summary(mod)
r.squaredGLMM(mod)

b <- data.anpp.summary%>%
  left_join(sandsite, by = "site_code")%>%
  subset(n_treat_years == "4"&Ann_Per == "Perennial")%>%
  ggplot(aes(sand_mean, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  #geom_smooth(method = "lm", se = TRUE)+
  xlab("Average sand content")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(sandsite, by = "site_code")%>%
  dplyr::select(anpp_response, sand_mean, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "4"& Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~sand_mean,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)


c <- data.anpp.summary%>%
  left_join(ai, by = "site_code")%>%
  subset(n_treat_years == "4"&Ann_Per == "Perennial")%>%
  ggplot(aes(AI, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  xlim(0,2)+
#  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed")+ #marginal when controlling for multiple comparisons
  xlab("Aridity index")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(ai, by = "site_code")%>%
  dplyr::select(anpp_response, AI, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "4"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~AI,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)


d <- data.anpp.summary%>%
  left_join(cv1, by = "site_code")%>%
  subset(n_treat_years == "4"&Ann_Per == "Perennial")%>%
  ggplot(aes(cv_ppt_inter, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
#  geom_smooth(method = "lm", se = TRUE, color = "black")+
  xlab("Interannual precip CV")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(cv1, by = "site_code")%>%
  dplyr::select(anpp_response, cv_ppt_inter, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "4"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~cv_ppt_inter, random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)



e <- data.anpp.summary%>%
  left_join(graminoid_richness, by = "site_code")%>%
  subset(n_treat_years == "4"&Ann_Per == "Perennial")%>%
  ggplot(aes(percent_graminoid, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  #geom_smooth(method = "lm", se = TRUE)+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(graminoid_richness, by = "site_code")%>%
  dplyr::select(anpp_response, percent_graminoid, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "4"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~percent_graminoid,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)

f <- data.anpp.summary%>%
  left_join(graminoid_richness, by = "site_code")%>%
  subset(n_treat_years == "4"&Ann_Per == "Perennial")%>%
  ggplot(aes(richness, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  geom_smooth(method = "lm", se = TRUE, color = "black")+
  xlab("Richness")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(graminoid_richness, by = "site_code")%>%
  dplyr::select(anpp_response, richness, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "4"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~richness,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)


g <- data.anpp.summary%>%
  left_join(seasonality, by = "site_code")%>%
  subset(n_treat_years == "4"&Ann_Per == "Perennial")%>%
  ggplot(aes(seasonality_index, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
#  geom_smooth(method = "lm",  se = TRUE, color = "black")+
  xlab("Seasonality")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(seasonality, by = "site_code")%>%
  dplyr::select(anpp_response, seasonality_index, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "4"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~seasonality_index,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)

h <- data.anpp.summary%>%
   subset(n_treat_years == "4"&Ann_Per == "Perennial")%>%
  ggplot(aes(drtsev.2, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  geom_smooth(method = "lm",  se = TRUE, color = "black")+
  xlab("Previous year's drought severity")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
    dplyr::select(anpp_response, drtsev.2, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "4"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~drtsev.2,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)


i <- data.anpp.summary%>%
  subset(n_treat_years == "4"&Ann_Per == "Perennial")%>%
  ggplot(aes(drtsev.1, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  geom_smooth(method = "lm",  se = TRUE, color = "black")+
  xlab("Drought severity")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  dplyr::select(anpp_response, drtsev.1, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "4"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~drtsev.1,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)


j <- data.anpp.summary%>%
  subset(n_treat_years == "4"&Ann_Per == "Perennial")%>%
  left_join(Site_Elev.Disturb, by = "site_code")%>%
  ggplot(aes(temp, anpp_response))+
  geom_point(aes(color = e.n),alpha = 0.8, size = 3#, pch = 21
  )+
  #geom_smooth(method = "lm",  se = TRUE, color = "black")+
  xlab("MAT")+
  ylab("Productivity response")+
  scale_color_manual( values = c("#da7901" , "grey48" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none",text = element_text(size = 12))+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")

tempdf <- data.anpp.summary%>%
  dplyr::select(anpp_response, drtsev.1, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "4"&Ann_Per == "Perennial")%>%
  left_join(Site_Elev.Disturb, by = c("site_code", "ipcc_regions"))
mod <- lme(anpp_response~temp,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)



plot_grid(i,h,a, c, j,d, b, e, f, labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I'), nrow = 2)

ggsave(
  "C:/Users/ohler/Dropbox/IDE/figures/anpp_duration/covariate_supplemental_y4.pdf",
  plot = last_plot(),
  device = "pdf",
  path = NULL,
  scale = 1,
  width = 15,
  height = 6,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE
)



###



#Stitches plot for all years aggregated
data.anpp.summary%>%
  #subset( n_treat_years >= 1 & n_treat_years <= 4)%>% #this line probably isn't necessary
  ddply(.(site_code, type), function(x) data.frame(
    anpp_response = mean(x$anpp_response),
    anpp_response.se = sd(x$anpp_response)/sqrt(length(x$n_treat_years))
  ))%>%
  dplyr::mutate(site_code = fct_reorder(site_code, desc(anpp_response)))%>%
  ggplot(  aes(site_code, anpp_response, color = type))+
  geom_pointrange(aes(ymin = anpp_response-anpp_response.se, ymax = anpp_response+anpp_response.se))+
  scale_color_manual("Vegetation type", values = c("#04a3bd", "#247d3f","#f0be3d" ))+
  geom_hline(yintercept = 0,linetype="dashed")+
  ylim(-3.5,1)+#this removes error bars from granite mountain sites.
  ylab("Avg Productivity response")+
  xlab("")+
  
  coord_flip()+
  theme_base()


length(unique(data.anpp.summary$site_code))#norm.cn, teshio.jp, freiburg.de, ayora.es

ggsave(
  "C:/Users/ohler/Dropbox/IDE/figures/anpp_duration/fig1_sitches-se.pdf",
  plot = last_plot(),
  device = "pdf",
  path = NULL,
  scale = 1,
  width = 6,
  height = 10,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE
)



tempdf <- data.anpp.summary%>%
  ddply(.(site_code, type), function(x) data.frame(
    anpp_response = mean(x$anpp_response),
    anpp_response.se = sd(x$anpp_response)/sqrt(length(x$n_treat_years))
  ))
data.anpp.summary%>%
  ddply(c("site_code"), function(x)data.frame(
    avg_precip_reduction = mean(x$drtsev.1, na.rm = TRUE)
  ))%>%
  left_join(dplyr::select(tempdf, site_code, anpp_response))%>%
  left_join(dplyr::select(Site_Elev.Disturb, site_code, drought_trt))%>%
  dplyr::mutate(site_code = fct_reorder(site_code, desc(anpp_response)))%>%
  ggplot(  aes(site_code, avg_precip_reduction))+
  geom_bar(stat = "identity", fill = "dodgerblue", linewidth = 0.98)+
  geom_hline(yintercept = 0,linetype="dashed")+
  geom_point(aes(x = site_code, y=as.numeric(drought_trt)/100), shape = 124, size = 2)+
  ylim(0, 1)+
  ylab("Precip reduction over 3 years")+
  xlab("")+
  coord_flip()+
  theme_base()+ theme(legend.position = "none")

ggsave(
  "C:/Users/ohler/Dropbox/IDE/figures/anpp_duration/fig1_clim-by-site.pdf",
  plot = last_plot(),
  device = "pdf",
  path = NULL,
  scale = 1,
  width = 3.5,
  height = 10,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE
)



###
 
 data.anpp.summary%>%
   dplyr::select(site_code, n_treat_years, e.n)%>%
   pivot_wider(names_from = n_treat_years, values_from = e.n)%>%
   pivot_longer(cols = c("1","2","3","4"), names_to = "n_treat_years", values_to = "e.n")%>%
   left_join(dplyr::select(tempdf, site_code, anpp_response))%>%
   dplyr::mutate(site_code = fct_reorder(site_code, dplyr::desc(-anpp_response)))%>%
   ggplot(aes(n_treat_years, forcats::fct_rev(site_code), fill = e.n))+
   geom_tile(colour = "black")+
   scale_fill_manual(values = c( "#da7901" , "grey48"), na.value = "white")+ #burnt sienna "#E97451"
   ylab("")+
   xlab("Treatment year")+
   theme_bw()
 
 ggsave(
   "C:/Users/ohler/Dropbox/IDE/figures/anpp_duration/EN_by_year_site.pdf",
   plot = last_plot(),
   device = "pdf",
   path = NULL,
   scale = 1,
   width = 3.5,
   height = 7,
   units = c("in"),
   dpi = 600,
   limitsize = TRUE
 )
# 
# 



##################################
#data.anpp.summary$temp <- ifelse(data.anpp.summary$drtsev.1 > .50, "MoreDrought",
#                               "LessDrought")
data.anpp.year <- data.anpp.summary%>%
  # subset(Ann_Per == "Perennial")%>%
  ddply(.(n_treat_years, type
  ),
  function(x)data.frame(
    anpp_response = mean(x$anpp_response),
    anpp_response.error = qt(0.975, df=length(x$site_code)-1)*sd(x$anpp_response, na.rm = TRUE)/sqrt(length(x$site_code)-1),
    anpp_response.se = sd(x$anpp_response, na.rm = TRUE)/sqrt(length(x$site_code)),
    percent.reduction = mean(x$percent.reduction),
    percent.reduction.error = qt(0.975, df=length(x$site_code)-1)*sd(x$percent.reduction, na.rm = TRUE)/sqrt(length(x$site_code)-1),
    percent.reduction.se = sd(x$percent.reduction, na.rm = TRUE)/sqrt(length(x$site_code))
  ))





mod <- lme(anpp_response~type*as.factor(n_treat_years), random = ~1|ipcc_regions/site_code, data = data.anpp.summary)
summary(mod)
library(emmeans)
means <- emmeans(mod, ~n_treat_years*type)
emmeans(mod, list(pairwise ~ type), adjust = "tukey")
x <- emmeans(mod, list(pairwise ~ n_treat_years*type), adjust = "tukey")$`pairwise differences of n_treat_years, type`
write.csv(x, "C:/Users/ohler/Dropbox/IDE_Duration_ms/fig1c_table.csv")
confint(means)


ggplot(confint(means),aes(as.factor(n_treat_years), emmean, color = type
))+
  geom_pointrange(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(width = 0.5))+
  #ylim(-1.2, 0.3)+
  geom_hline(yintercept = 0,linetype="dashed")+
  xlab("Years of drought")+
  ylab("Productivity response")+
  scale_color_manual("Prevailing veg type", values = c("#04a3bd" , "#247d3f","#f0be3d"))+ #"#D9782D", "#1E4D2B", "#C8C372" 
  #coord_flip()+
  theme_base()+
  theme(axis.ticks.length=unit(-0.25, "cm"), legend.position = "none")


ggsave(
  "C:/Users/ohler/Dropbox/IDE/figures/anpp_duration/fig1_allsites.pdf",
  plot = last_plot(),
  device = "pdf",
  path = NULL,
  scale = 1,
  width = 4,
  height = 4,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE
)



data.anpp.year <- data.anpp.summary%>%
  subset(Ann_Per == "Perennial")%>%
  ddply(.(n_treat_years, e.n
  ),
  function(x)data.frame(
    anpp_response = mean(x$anpp_response),
    anpp_response.error = qt(0.975, df=length(x$site_code)-1)*sd(x$anpp_response, na.rm = TRUE)/sqrt(length(x$site_code)-1),
    anpp_response.se = sd(x$anpp_response, na.rm = TRUE)/sqrt(length(x$site_code)),
    percent.reduction = mean(x$percent.reduction),
    percent.reduction.error = qt(0.975, df=length(x$site_code)-1)*sd(x$percent.reduction, na.rm = TRUE)/sqrt(length(x$site_code)-1),
    percent.reduction.se = sd(x$percent.reduction, na.rm = TRUE)/sqrt(length(x$site_code))
  ))

#




tempdf <- subset(data.anpp.summary, Ann_Per == "Perennial")%>%
  left_join(sandsite, by = "site_code")%>%
  left_join(ai, by = "site_code")%>%
  left_join(cv1, by = "site_code")%>%
  left_join(graminoid_richness, by = "site_code")%>%
  left_join(seasonality, by = "site_code")%>%
  subset( Ann_Per == "Perennial")
lmFull <- lme(anpp_response~#drtsev.1 *
                as.factor(n_treat_years)#*
              # map*#sand_mean + 
              #cv_ppt_inter# * 
              #r_monthly_t_p #+ PctAnnual  
              #+ percent_graminoid + richness+ 
              , random = ~1|ipcc_regions/site_code, method = "ML"
              , data=tempdf)

lmNull <- lme(anpp_response~1,random = ~1|ipcc_regions/site_code,method = "ML",  data = data.anpp.summary)
#Forward model selection
stepAIC(lmNull, scope = list(upper = lmFull,
                             lower = ~1),
        trace = F)


mod <- lm(anpp_response ~ as.factor(n_treat_years), 
          data = tempdf)
summary(mod)


#Modelselection:rankingbyAICcusingML 
ms2<-dredge(lmFull, rank = "AIC") 
#(attr(ms2,"rank.call")) 
#Getthemodels(fittedbyREML,asintheglobalmodel) 
fmList<-get.models(ms2,1:5) #Becausethemodelsoriginatefrom'dredge(...,rank=AICc,REML=FALSE)', 
#thedefaultweightsin'model.avg'areMLbased: 
#summary(model.avg(fmList))
sw(ms2)
sw(subset(ms2,delta<=4))
am <- model.avg(ms2)
coef(am)



#########


subset(data.anpp.summary,n_treat_years >=1 & n_treat_years <= 4)%>%
  subset(Ann_Per == "Perennial")%>%
  ggplot( aes(drtsev.1, anpp_response))+
  facet_wrap(~n_treat_years, scales = 'free')+
  #scale_color_manual(values = c("#da7901", "grey48" ))+
  #scale_fill_manual(values = c("#da7901", "grey48" ))+
  #geom_point(aes( color = e.n, fill = e.n),alpha = 0.8,#pch = 21,
  #           size=3)+
  geom_point(aes( color = habitat.type),alpha = 0.8,#pch = 21,
             size=3)+
  scale_color_manual("Vegetation type", values = c("#247d3f","#f0be3d" ))+
  #scale_shape_manual(values = c(19, 21))+
  geom_smooth(aes(),method = "lm", color = "black")+
  #scale_linetype_manual(values = c("solid","dashed"))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlim(-.12,1)+
  ylim(-5.05,1.2)+
  xlab("Drought severity (percent reduction of MAP)")+
  ylab("Productivity response")+
  theme_base()+
  theme(legend.position = "none",axis.ticks.length=unit(-0.25, "cm"))


ggsave(
  "C:/Users/ohler/Dropbox/IDE/figures/anpp_duration/fig2_revised_v1.pdf",
  plot = last_plot(),
  device = "pdf",
  path = NULL,
  scale = 1,
  width = 8,
  height = 7,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE
)



##Alan's version
alan <- data.anpp.summary%>%
  subset(Ann_Per == "Perennial")%>%
  unite(historyroad, c("e.n", "prev_e.n", "prev_e.n2", "prev_e.n3"), sep = "::")%>%
  subset(historyroad == "extreme::extreme::extreme::extreme" | historyroad == "extreme::extreme::extreme::nominal" | historyroad == "extreme::extreme::extreme::NA" | historyroad == "extreme::extreme::nominal::nominal" | historyroad == "extreme::extreme::nominal::NA" | historyroad == "extreme::extreme::NA::NA" | historyroad == "extreme::nominal::nominal::nominal" | historyroad == "extreme::nominal::nominal::NA" | historyroad == "extreme::nominal::NA::NA" | historyroad == "extreme::NA::NA::NA")

alan$history <- ifelse(alan$historyroad == "extreme::extreme::extreme::extreme", "Four",
                       ifelse(alan$historyroad == "extreme::extreme::extreme::nominal" | alan$historyroad == "extreme::extreme::extreme::NA", "Three",
                              ifelse(alan$historyroad == "extreme::extreme::nominal::nominal" |alan$historyroad == "extreme::extreme::nominal::NA" |alan$historyroad == "extreme::extreme::NA::NA", "Two",
                                     "One")))


ggplot(alan, aes(x = factor(history,level=c("One", "Two", "Three", "Four")), anpp_response))+
  geom_point()+
  theme_base()

alan$drtsev.ave <- (alan$drtsev.1 + alan$drtsev.2 + alan$drtsev.3 + alan$drtsev.4)/4
x <- subset(alan, history == "Four")%>%
  left_join(Site_Elev.Disturb, by = c("site_code", "ipcc_regions"))%>%
  dplyr::select(site_code, drtsev.ave, ipcc_regions, map, ppt.1, anpp_response, latitud, longitud)
  
write.csv(x, "C:/Users/ohler/Dropbox/IDE/data_raw/fouryear_extreme_sites.csv")

#all nominal avg
mod <- lme(anpp_response~1, random = ~1|ipcc_regions/site_code, data = subset(data.anpp.summary, e.n == "nominal" & Ann_Per == "Perennial"))
nominal.avg <- summary(mod)[[20]][1]
nominal.se <- summary(mod)[[20]][2]

#all extreme avg
mod <- lme(anpp_response~1, random = ~1|ipcc_regions/site_code, data = subset(data.anpp.summary, e.n == "extreme" & Ann_Per == "Perennial"))
summary(mod)
extreme.avg <- summary(mod)[[20]][1]
extreme.se <- summary(mod)[[20]][2]

#all one avg
mod <- lme(anpp_response~1, random = ~1|ipcc_regions/site_code, data = subset(alan, history == "One"))
summary(mod)
one.avg <- summary(mod)[[20]][1]
one.se <- summary(mod)[[20]][2]

#all two avg
mod <- lme(anpp_response~1, random = ~1|ipcc_regions/site_code, data = subset(alan, history == "Two"))
summary(mod)
two.avg <- summary(mod)[[20]][1]
two.se <- summary(mod)[[20]][2]

#all three avg
mod <- lme(anpp_response~1, random = ~1|ipcc_regions/site_code, data = subset(alan, history == "Three"))
summary(mod)
three.avg <- summary(mod)[[20]][1]
three.se <- summary(mod)[[20]][2]

#all four avg
mod <- lme(anpp_response~1, random = ~1|ipcc_regions/site_code, data = subset(alan, history == "Four"))
summary(mod)
four.avg <- summary(mod)[[20]][1]
four.se <- summary(mod)[[20]][2]

length(subset(alan, history == "One")$site_code)
length(subset(alan, history == "Two")$site_code)
length(subset(alan, history == "Three")$site_code)
length(subset(alan, history == "Four")$site_code)



#year one nominal avg
mod <- lme(anpp_response~1, random = ~1|ipcc_regions/site_code, data = subset(data.anpp.summary, e.n == "nominal" & n_treat_years == 1 & Ann_Per == "Perennial"))
nominal.avg <- summary(mod)[[20]][1]
nominal.se <- summary(mod)[[20]][2]

#year one extreme avg
mod <- lme(anpp_response~1, random = ~1|ipcc_regions/site_code, data = subset(data.anpp.summary, e.n == "extreme" & n_treat_years == 1 & Ann_Per == "Perennial"))
summary(mod)
extreme.avg <- summary(mod)[[20]][1]
extreme.se <- summary(mod)[[20]][2]

alan$historycont <- as.numeric(revalue(alan$history, c("One"=1, "Two"=2, "Three"=3, "Four"=4)))
mod <- lme(anpp_response~history, random = ~1|ipcc_regions/site_code, method = "REML",data = alan)
summary(mod)
pairs(emmeans(mod, ~history))

mod <- lme(anpp_response~historycont, random = ~1|ipcc_regions/site_code, data = alan)
summary(mod)
r.squaredGLMM(mod)

x <- ggpredict(mod, c("historycont"))



data.frame(history = c(1, 2, 3, 4), avg = c(one.avg, two.avg, three.avg, four.avg), se = c(one.se, two.se, three.se, four.se))%>%
  ggplot( aes(x = history, avg))+
  geom_pointrange(aes(ymax = avg+se, ymin = avg-se), color = "#DA7901")+
#  geom_hline(yintercept = 0, linetype = "dashed")+
#  geom_hline(yintercept = nominal.avg, color = "blue")+
#  geom_hline(yintercept = nominal.avg+nominal.se, color = "blue", linetype = "dashed")+
#  geom_hline(yintercept = nominal.avg-nominal.se, color = "blue", linetype = "dashed")+
#  geom_hline(yintercept = extreme.avg, color = "red")+
#  geom_hline(yintercept = extreme.avg+extreme.se, color = "red", linetype = "dashed")+
#  geom_hline(yintercept = extreme.avg-extreme.se, color = "red", linetype = "dashed")+
  #geom_smooth(method = "lm", color = "black")+
  geom_ribbon(data=x,aes(x=x,y=predicted, ymin=predicted-std.error,ymax=predicted+std.error), alpha =.4, fill = "#DA7901")+
  geom_line(data=x,aes(x=x,y=predicted), alpha =1, color = "#DA7901")+
  xlab("# consecutive years extreme drought")+
  ylab("Productivity response")+
  ylim(-2,0)+
  theme_base()+
  theme(axis.ticks.length=unit(-0.25, "cm"))

100 * (exp(one.avg) - 1) #percent reduction
100 * (exp(two.avg) - 1) #percent reduction
100 * (exp(three.avg) - 1) #percent reduction
100 * (exp(four.avg) - 1) #percent reduction


ggsave(
  "C:/Users/ohler/Dropbox/IDE/figures/anpp_duration/fig3_panelB.pdf",
  plot = last_plot(),
  device = "pdf",
  path = NULL,
  scale = 1,
  width = 5,
  height = 4.5,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE
)



x <- subset(alan, history == "Four")%>%
      left_join(anpp.mean, by = "site_code")
x$carbon_loss <-  x$mean.mass - x$mean_mass



tempdf <- subset(data.anpp.summary, Ann_Per == "Perennial" & is.na(drtsev.1) == FALSE)
mod <- lme(anpp_response~as.factor(n_treat_years)*e.n, random = ~1|ipcc_regions/site_code, data = tempdf)
summary(mod)

summary <- emmeans(mod, ~ e.n | n_treat_years, var = "n_treat_years")
pairs(emmeans(mod, ~ e.n | n_treat_years, var = "n_treat_years"))


#data.frame(history = c(1, 2, 3, 4), avg = c(one.avg, two.avg, three.avg, four.avg), se = c(one.se, two.se, three.se, four.se))%>%
    ggplot(data.frame(summary), aes(n_treat_years, emmean,))+
    #geom_pointrange(aes(ymax = avg+se, ymin = avg-se), color = "#DA7901")+
  geom_pointrange(aes( color = e.n, fill = e.n,ymax = emmean+SE, ymin = emmean-SE))+
  scale_color_manual(values = c("#DA7901", "grey48" ))+ #normal orange is #da7901
  scale_fill_manual(values = c("#DA7901", "grey48" ))+
  geom_smooth(data = data.frame(summary),aes(n_treat_years, emmean, color = e.n, fill = e.n),method = "lm", alpha = 0.4)+
    #geom_ribbon(data=x,aes(x=x,y=predicted, ymin=predicted-std.error,ymax=predicted+std.error), alpha =.4, fill = "#ae6000")+
    #geom_line(data=x,aes(x=x,y=predicted), alpha =1, color = "#ae6000")+
    xlab("Years")+
    ylab("Productivity response")+
    ylim(-2,0)+
    theme_base()+
    theme(axis.ticks.length=unit(-0.25, "cm"),legend.position = "none")
  
  
ggsave(
  "C:/Users/ohler/Dropbox/IDE/figures/anpp_duration/fig3_panelA.pdf",
  plot = last_plot(),
  device = "pdf",
  path = NULL,
  scale = 1,  
  width = 5,
  height = 4.5,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE
)

tempdf <- subset(data.anpp.summary, Ann_Per == "Perennial" & is.na(drtsev.1) == FALSE)
mod <- lme(anpp_response~n_treat_years*e.n, random = ~1|ipcc_regions/site_code, data = tempdf)
summary(mod) #for interaction
x <- emtrends(mod, ~ e.n, var = "n_treat_years") #for main effects
test(x)
summary(pairs(emtrends(mod, ~e.n | n_treat_years, var="n_treat_years")))



tempdf <- subset(data.anpp.summary, Ann_Per == "Perennial" & is.na(drtsev.1) == FALSE & n_treat_years == 1)
mod <- lme(anpp_response~drtsev.1*e.n, random = ~1|ipcc_regions, data = tempdf)
summary(mod)
one.main <- test(emtrends(mod, ~e.n | drtsev.1, var="drtsev.1"))#each variable separately
one.contrast <- summary(pairs(emtrends(mod, ~e.n | drtsev.1, var="drtsev.1")))%>%
  dplyr::rename(e.n = "contrast", drtsev.1.trend = "estimate")#ExN contrast

tempdf <- subset(data.anpp.summary, Ann_Per == "Perennial" & is.na(drtsev.1) == FALSE & n_treat_years == 2)
mod <- lme(anpp_response~drtsev.1*e.n, random = ~1|ipcc_regions, data = tempdf)
summary(mod)
two.main <- test(emtrends(mod, ~e.n | drtsev.1, var="drtsev.1"))#each variable separately
two.contrast <- summary(pairs(emtrends(mod, ~e.n | drtsev.1, var="drtsev.1")))%>%
  dplyr::rename(e.n = "contrast", drtsev.1.trend = "estimate")#ExN contrast

tempdf <- subset(data.anpp.summary, Ann_Per == "Perennial" & is.na(drtsev.1) == FALSE & n_treat_years == 3)
mod <- lme(anpp_response~drtsev.1*e.n, random = ~1|ipcc_regions, data = tempdf)
summary(mod)
three.main <- test(emtrends(mod, ~e.n | drtsev.1, var="drtsev.1"))#each variable separately
three.contrast <- summary(pairs(emtrends(mod, ~e.n | drtsev.1, var="drtsev.1")))%>%
  dplyr::rename(e.n = "contrast", drtsev.1.trend = "estimate")#ExN contrast

tempdf <- subset(data.anpp.summary, Ann_Per == "Perennial" & is.na(drtsev.1) == FALSE & n_treat_years == 4)
mod <- lme(anpp_response~drtsev.1*e.n, random = ~1|ipcc_regions, data = tempdf)
summary(mod)
four.main <- test(emtrends(mod, ~e.n | drtsev.1, var="drtsev.1"))#each variable separately
four.contrast <- summary(pairs(emtrends(mod, ~e.n | drtsev.1, var="drtsev.1")))%>%
  dplyr::rename(e.n = "contrast", drtsev.1.trend = "estimate")#ExN contrast

x <- rbind(one.main, one.contrast)%>%
  rbind(two.main)%>%
  rbind(two.contrast)%>%
  rbind(three.main)%>%
  rbind(three.contrast)%>%
  rbind(four.main)%>%
  rbind(four.contrast)

write.csv(x, "C:/Users/ohler/Dropbox/IDE_Duration_ms/fig2_statstable.csv")

# 
# 
 
 nonconsecutive <- data.anpp.summary%>%
   subset(Ann_Per == "Perennial")%>%
   unite(historyroad, c("e.n", "prev_e.n", "prev_e.n2", "prev_e.n3"), sep = "::")%>%
   subset(historyroad == "extreme::extreme::nominal::extreme" | historyroad == "extreme::nominal::extreme::extreme" |historyroad == "extreme::nominal::extreme::nominal" | historyroad == "extreme::nominal::extreme::NA")
 
 nonconsecutive$two_or_three <- ifelse(nonconsecutive$historyroad == "extreme::extreme::nominal::extreme" | nonconsecutive$historyroad == "extreme::nominal::extreme::extreme", "Three_nonconsecutive",
                                       ifelse(
                                         nonconsecutive$historyroad == "extreme::nominal::extreme::nominal" | nonconsecutive$historyroad == "extreme::nominal::extreme::NA", "Two_nonconsecutive",
                                         NA
                                       ))
 mean(subset(nonconsecutive, two_or_three == "Three_nonconsecutive")$anpp_response)
 sd(subset(nonconsecutive, two_or_three == "Three_nonconsecutive")$anpp_response)/5
 mean(subset(nonconsecutive, two_or_three == "Two_nonconsecutive")$anpp_response)
 sd(subset(nonconsecutive, two_or_three == "Two_nonconsecutive")$anpp_response)/5
# 
# 
# 
# 
 
 tempdf <- data.frame(history = c(1, 2, 3, 4), avg = c(one.avg, two.avg, three.avg, four.avg), se = c(one.se, two.se, three.se, four.se))
 
 ggplot(data=tempdf, aes(x = history, avg))+
   geom_pointrange(data=tempdf,aes(ymax = avg+se, ymin = avg-se))+
   geom_hline(yintercept = 0)+
   geom_abline(slope = -0.23659773, intercept = -0.04291775)+
   geom_ribbon(data=x,aes(x=x, y= predicted,ymin=predicted-std.error,ymax=predicted+std.error), alpha =.25)+
   geom_hline(yintercept = nominal.avg, color = "blue")+
   geom_hline(yintercept = nominal.avg+nominal.se, color = "blue", linetype = "dashed")+
   geom_hline(yintercept = nominal.avg-nominal.se, color = "blue", linetype = "dashed")+
   geom_hline(yintercept = extreme.avg, color = "red")+
   geom_hline(yintercept = extreme.avg+extreme.se, color = "red", linetype = "dashed")+
   geom_hline(yintercept = extreme.avg-extreme.se, color = "red", linetype = "dashed")+
   xlab("Number of consecutive years extreme drought")+
   ylab("Productivity response")+
   theme_base()
# 
# 
# 
# ###Nico's version
# 
# 
# 
# alan%>%
#   ddply(.(n_treat_years, historyroad, history), function(x)data.frame(
#     anpp_response = mean(x$anpp_response),
#     anpp_response.se = sd(x$anpp_response)/sqrt(length(x$site_code)),
#     n = length(x$site_code)
#   ))%>%
# ggplot(aes(n_treat_years, anpp_response, color = history))+
#   geom_pointrange(aes(ymax = anpp_response+anpp_response.se, ymin = anpp_response-anpp_response.se),position=position_dodge(width=0.25))+
#   geom_hline(yintercept = 0)+
#   xlab("Treatment year")+
#   ylab("ANPP response")+
#   theme_base()+
#   guides(color=guide_legend(title="# consecitive extreme years"))
# 
# mod <- lm(anpp_response~as.factor(n_treat_years)*history, data = alan)  
# summary(mod)
# 

unique(data.anpp.summary$site_code)




##########Full data table
data_table <- data.anpp.summary%>%
  left_join(dplyr::select(Site_Elev.Disturb, site_code, temp), by = "site_code")%>%
  left_join(sandsite, by = "site_code")%>%
  left_join(ai, by = "site_code")%>%
  left_join(cv1, by = "site_code")%>%
  left_join(graminoid_richness, by = "site_code")%>%
  left_join(seasonality, by = "site_code")%>%
  dplyr::select(site_code, n_treat_years, anpp_response, type, e.n, drtsev.1,  map, temp,   sand_mean, AI, cv_ppt_inter, percent_graminoid, richness)

write.csv(data_table, "C:/Users/ohler/Dropbox/IDE_Duration_ms/data_table.csv")
