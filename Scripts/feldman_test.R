##IDE replicate findings from Feldman metrics

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

climate <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/climate/mswep_mean_annual_by_site.csv")


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
  subset(n_years>=5)%>% #Does long-term control average include at least 4 years?
  left_join(num.treat.years, by = "site_code")%>%
  left_join(extremeyrs, by = c("site_code", "year", "n_treat_years"))%>%
  left_join(extremeyrs.prev, by = c("site_code", "year"))%>%
  left_join(extremeyrs.prev2, by = c("site_code", "year"))%>%
  left_join(extremeyrs.prev3, by = c("site_code", "year"))%>%
  #subset(num.years == 4 | num.years == 3
  #) %>%#change here if using 4 years #reduces dataset to focal sites
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
  ddply(.(site_code, year, drtsev.1,drtsev.2,drtsev.3,drtsev.4, n_treat_years, map, habitat.type, e.n, prev_e.n, prev_e.n2, prev_e.n3, PctAnnual, Ann_Per, ipcc_regions, continent),
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
  subset(n_treat_years >= 1 & n_treat_years<= 4)%>% #CHANGE HERE IF YOU"RE GOING UP TO 4 TREATMENT YEARS
  left_join(climate, by = "site_code")

data.anpp.summary$type <- ifelse(data.anpp.summary$Ann_Per == "Annual" & is.na(data.anpp.summary$Ann_Per) == FALSE , data.anpp.summary$Ann_Per, data.anpp.summary$habitat.type)


data.anpp.summary$type <-   plyr::revalue(data.anpp.summary$type, c(Grassland = "Herbaceous.Perennial", Shrubland = "Woody.Perennial"))




###Correlations of climate metrics
library(PerformanceAnalytics)

mydata <- climate[, c(3, 6, 7, 9)]
chart.Correlation(mydata, histogram=TRUE, pch=19, method = "pearson")







####
y1 <- subset(data.anpp.summary, n_treat_years == "1")

#make principal component axes of climate variable
pca <- prcomp(~ MAP + n_wet_days + ppt_mean_event+ avg_dryspell_length, data = climate, scale = TRUE, rank = 1)
pca.axes <- predict(pca, newdata = y1)

mod.map <- lme(anpp_response~MAP, random = ~1|ipcc_regions, data = y1)

mod.wetdayfrequency <- lme(anpp_response~n_wet_days, random = ~1|ipcc_regions, data = y1)

mod.wetdayintensity <- lme(anpp_response~ppt_mean_event, random = ~1|ipcc_regions, data = y1)

mod.dryspelllength <- lme(anpp_response~avg_dryspell_length, random = ~1|ipcc_regions, data = y1)

mod.pca <- lme(anpp_response~PC1#+PC2+PC3+PC3
               , random = ~1|ipcc_regions, data = cbind(y1, pca.axes))

summary(mod.map)
summary(mod.wetdayfrequency)
summary(mod.wetdayintensity)
summary(mod.dryspelllength)
summary(mod.pca)
r.squaredGLMM(mod.map)

AIC(mod.map,mod.wetdayfrequency, mod.wetdayintensity, mod.dryspelllength, mod.pca )


ggplot(y1, aes(ppt_mean_event, anpp_response))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_base()



## year 2
y2 <- subset(data.anpp.summary, n_treat_years == "2")

mod.map <- lme(anpp_response~MAP, random = ~1|ipcc_regions, data = y2)

mod.wetdayfrequency <- lme(anpp_response~n_wet_days, random = ~1|ipcc_regions, data = y2)

mod.wetdayintensity <- lme(anpp_response~ppt_mean_event, random = ~1|ipcc_regions, data = y2)

mod.dryspelllength <- lme(anpp_response~avg_dryspell_length, random = ~1|ipcc_regions, data = y2)


summary(mod.map)
summary(mod.wetdayfrequency)
summary(mod.wetdayintensity)
summary(mod.dryspelllength)

AIC(mod.map,mod.wetdayfrequency, mod.wetdayintensity, mod.dryspelllength )

ggplot(y2, aes(avg_dryspell_length, anpp_response))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_base()


## year 3
y3 <- subset(data.anpp.summary, n_treat_years == "3")

mod.map <- lme(anpp_response~map, random = ~1|ipcc_regions, data = y3)

mod.wetdayfrequency <- lme(anpp_response~n_wet_days, random = ~1|ipcc_regions, data = y3)

mod.wetdayintensity <- lme(anpp_response~ppt_mean_event, random = ~1|ipcc_regions, data = y3)

mod.dryspelllength <- lme(anpp_response~avg_dryspell_length, random = ~1|ipcc_regions, data = y3)


summary(mod.map)
summary(mod.wetdayfrequency)
summary(mod.wetdayintensity)
summary(mod.dryspelllength)

AIC(mod.map,mod.wetdayfrequency, mod.wetdayintensity, mod.dryspelllength )

ggplot(y3, aes(avg_dryspell_length, anpp_response))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_base()



## year 4
y4 <- subset(data.anpp.summary, n_treat_years == "4")

mod.map <- lme(anpp_response~map, random = ~1|ipcc_regions, data = y4)

mod.wetdayfrequency <- lme(anpp_response~n_wet_days, random = ~1|ipcc_regions, data = y4)

mod.wetdayintensity <- lme(anpp_response~ppt_mean_event, random = ~1|ipcc_regions, data = y4)

mod.dryspelllength <- lme(anpp_response~avg_dryspell_length, random = ~1|ipcc_regions, data = y4)


summary(mod.map)
summary(mod.wetdayfrequency)
summary(mod.wetdayintensity)
summary(mod.dryspelllength)

AIC(mod.map,mod.wetdayfrequency, mod.wetdayintensity, mod.dryspelllength )

ggplot(y4, aes(ppt_mean_event, anpp_response))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_base()



##by continent
mod.map <- lm(anpp_response~MAP*continent, data = y1)

mod.wetdayfrequency <- lm(anpp_response~n_wet_days*continent, data = y1)

mod.wetdayintensity <- lm(anpp_response~ppt_mean_event*continent, data = y1)

mod.dryspelllength <- lm(anpp_response~avg_dryspell_length*continent, data = y1)

summary(mod.map)
summary(mod.wetdayfrequency)
summary(mod.wetdayintensity)
summary(mod.dryspelllength)
r.squaredGLMM(mod.map)

AIC(mod.map,mod.wetdayfrequency, mod.wetdayintensity, mod.dryspelllength)
