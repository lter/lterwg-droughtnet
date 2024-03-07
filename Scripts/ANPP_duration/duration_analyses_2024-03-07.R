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


#read ANPP data
data.anpp <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/anpp_ppt_2023-11-03.csv")%>%
  subset(habitat.type == "Grassland" | habitat.type == "Shrubland")#%>%

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
  subset(site_code != "allmendo.ch" & site_code != "allmendb.ch" & site_code != "sclaudio.ar" & site_code != "torla.es") #allmendo, allmendb, and torla have first treatment dates in the 190s, sclaudio missed Y3 sampling (2020)


data.anpp2$Ann_Per <- ifelse(data.anpp2$PctAnnual > 60, "Annual", 
                             ifelse(data.anpp2$PctAnnual <= 60, "Perennial",
                                    "NA"))

data.anpp2$Ann_Per <- ifelse(is.na(data.anpp2$Ann_Per) == TRUE, "Perennial", data.anpp2$Ann_Per) #morient.ar, b=nyngan.au, riomayo.ar, stubai.at, and syferkuil.za don't have cover data, but based on biomass and site info data that they submitted we can say that they are all perennial grassland.


length(unique(data.anpp2$site_code)) #67

subset(data.anpp2, PctAnnual != "NA")$site_code%>%
  unique()%>%
  length()


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
  dplyr::select(site_code, type)%>%
  unique()%>%
  group_by(type)%>%
  tally()


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


##Here we generate the stats and figure for Fig 2
#Backward model selection - skipping backward in favor of forward since backward likes the maximal model for some ungodly reason
tempdf <-subset(data.anpp.summary, n_treat_years == 3& Ann_Per == "Perennial")

lmFull <- lme(anpp_response~drtsev.1 * drtsev.2 * drtsev.3, random = ~1|ipcc_regions,method = "ML", data=tempdf)
#lmFull <- lmer(anpp_response~drtsev.1 * drtsev.2 * drtsev.3 + (1|ipcc_regions), data=tempdf)


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
#YEAR 3
a <- data.anpp.summary%>%
  #left_join(history.df, by = "site_code")%>%
  subset(n_treat_years == "3"&Ann_Per == "Perennial")%>%
  ggplot(aes(map, anpp_response))+
  geom_point(aes(color = type),alpha = 0.8, size = 3#, pch = 21
  )+
  geom_smooth(method = "lm", se = TRUE, color = "black")+
  xlab("MAP")+
  ylab("ANPP response")+
  scale_color_manual( values = c("#247d3f", "#f0be3d" ))+#1E4D2B", "#C8C372"
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none")

mod <- lme(anpp_response~map, random = ~1|ipcc_regions, data = subset(data.anpp.summary, n_treat_years == "3"&Ann_Per == "Perennial"))
summary(mod)
r.squaredGLMM(mod)

b <- data.anpp.summary%>%
  left_join(sandsite, by = "site_code")%>%
  subset(n_treat_years == "3"&Ann_Per == "Perennial")%>%
  ggplot(aes(sand_mean, anpp_response))+
  geom_point(aes(color = type),alpha = 0.8, size = 3#, pch = 21
  )+
  #geom_smooth(method = "lm", se = TRUE)+
  xlab("Average sand content")+
  ylab("ANPP response")+
  scale_color_manual( values = c("#247d3f", "#f0be3d" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none")

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
  geom_point(aes(color = type),alpha = 0.8, size = 3#, pch = 21
  )+
  xlim(0,2)+
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed")+ #marginal when controlling for multiple comparisons
  xlab("Aridity index")+
  ylab("ANPP response")+
  scale_color_manual( values = c("#247d3f", "#f0be3d" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none")

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
  geom_point(aes(color = type),alpha = 0.8, size = 3#, pch = 21
  )+
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed")+ #marginal when controlling for multiple comparisons
  xlab("Interannual precip CV")+
  ylab("ANPP response")+
  scale_color_manual( values = c("#247d3f", "#f0be3d" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none")

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
  geom_point(aes(color = type),alpha = 0.8, size = 3#, pch = 21
  )+
  #geom_smooth(method = "lm", se = TRUE)+
  ylab("ANPP response")+
  scale_color_manual( values = c("#247d3f", "#f0be3d" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none")

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
  geom_point(aes(color = type),alpha = 0.8, size = 3#, pch = 21
  )+
  #geom_smooth(method = "lm", se = TRUE)+
  xlab("Richness")+
  ylab("ANPP response")+
  scale_color_manual( values = c("#247d3f", "#f0be3d" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none")

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
  geom_point(aes(color = type),alpha = 0.8, size = 3#, pch = 21
  )+
  geom_smooth(method = "lm",  se = TRUE, color = "black", linetype = "dashed")+ #marginal
  xlab("Seasonality")+
  ylab("ANPP response")+
  scale_color_manual( values = c("#247d3f", "#f0be3d" ))+
  geom_hline(yintercept = 0, linetype = "dashed")+ 
  ylim(-3.5, 0.75)+
  theme_base()+
  theme(legend.position = "none")

tempdf <- data.anpp.summary%>%
  left_join(seasonality, by = "site_code")%>%
  dplyr::select(anpp_response, seasonality_index, n_treat_years, site_code, Ann_Per, ipcc_regions)%>%
  subset(n_treat_years == "3"&Ann_Per == "Perennial")%>%
  filter(complete.cases(.))
mod <- lme(anpp_response~seasonality_index,random = ~1|ipcc_regions, data = tempdf)
summary(mod)
r.squaredGLMM(mod)


plot_grid(a, c, d, g,b, e, f, labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G'))

ggsave(
  "C:/Users/ohler/Dropbox/IDE/figures/anpp_duration/covariate_supplemental.pdf",
  plot = last_plot(),
  device = "pdf",
  path = NULL,
  scale = 1,
  width = 10,
  height = 9,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE
)

# # 
#partial r-squared shit

full_y3 <-  data.anpp.summary%>%
  left_join(sandsite, by = "site_code")%>%
  left_join(ai, by = "site_code")%>%
  left_join(cv1, by = "site_code")%>%
  left_join(graminoid_richness, by = "site_code")%>%
  left_join(seasonality, by = "site_code")%>%
  subset(n_treat_years == "3")%>%
  dplyr::select(site_code, drtsev.1, drtsev.2, drtsev.3,map, anpp_response, sand_mean, AI, cv_ppt_inter, percent_graminoid, richness, seasonality_index, PctAnnual, Ann_Per, r_monthly_t_p, ipcc_regions)
#sandsite, AI, cv1, graminoid_richness

full_y2 <-  data.anpp.summary%>%
  left_join(sandsite, by = "site_code")%>%
  left_join(ai, by = "site_code")%>%
  left_join(cv1, by = "site_code")%>%
  left_join(graminoid_richness, by = "site_code")%>%
  left_join(seasonality, by = "site_code")%>%
  subset(n_treat_years == "2")%>%
  dplyr::select(site_code, drtsev.1, drtsev.2, map, anpp_response, sand_mean, AI, cv_ppt_inter, percent_graminoid, richness, r_monthly_t_p, PctAnnual, Ann_Per, ipcc_regions)
#sandsite, AI, cv1, graminoid_richness

#make sure none of the variables beng loaded are super correlated
full_y3%>%
  dplyr::select( drtsev.1,drtsev.2,map,AI,sand_mean,cv_ppt_inter,seasonality_index, PctAnnual, r_monthly_t_p)%>%
  pairs() #only AI is egregiously correlated with map (will remove AI from model)

library("PerformanceAnalytics")
full_y3%>%
  dplyr::select( drtsev.1,drtsev.2,map,AI,sand_mean,cv_ppt_inter,seasonality_index, PctAnnual, r_monthly_t_p)%>%
  chart.Correlation( histogram=TRUE, pch=19)


#mod <- lme(anpp_response ~  drtsev.1*drtsev.2 + drtsev.1:drtsev.2
#          + map+#sand_mean + 
#            cv_ppt_inter + r_monthly_t_p #+ PctAnnual  
#+ percent_graminoid + richness+ 
#          , random = ~1|ipcc_regions
#           , data = subset(full_y3, Ann_Per == "Perennial")) # the model
#summary(mod)


tempdf <- subset(full_y3, Ann_Per == "Perennial")
lmFull <- lme(anpp_response~drtsev.1 + 
                map+#sand_mean + 
                cv_ppt_inter + 
                r_monthly_t_p #+ PctAnnual  
              #             + percent_graminoid + richness
              , random = ~1|ipcc_regions, method = "ML"
              , data=tempdf)


lmNull <- lme(anpp_response~1,random = ~1|ipcc_regions,method = "ML",  data = tempdf)


#Forward model selection
stepAIC(lmNull, scope = list(upper = lmFull,
                             lower = ~1),
        trace = F)


mod <- lm(anpp_response ~ map + drtsev.1 + cv_ppt_inter , 
          data = tempdf)
summary(mod)




#library(glmulti)

#glmulti(anpp_response~drtsev.1 * drtsev.2 * drtsev.3 * map*#sand_mean + 
#         cv_ppt_inter * r_monthly_t_p,subset(full_y3, Ann_Per == "Perennial"),exclude=c(),name="glmulti.analysis", intercept=TRUE,marginality=FALSE, bunch=30,chunk=1,chunks=1,level=2, minsize=0,maxsize=-1,minK=0,maxK=-1, method="h",crit="aic",confsetsize=100,popsize=100, mutrate=10^-3,sexrate=0.1,imm=0.3,plotty=TRUE, report=TRUE,deltaM=0.05,deltaB=0.05,conseq=5, fitfunction="lme",resumefile="id",includeobjects=TRUE)

#Modelselection:rankingbyAICcusingML 
ms2<-dredge(lmFull, rank = "AIC") 
#(attr(ms2,"rank.call")) 
#Getthemodels(fittedbyREML,asintheglobalmodel) 
fmList<-get.models(ms2,1:16) #Becausethemodelsoriginatefrom'dredge(...,rank=AICc,REML=FALSE)', 
#thedefaultweightsin'model.avg'areMLbased: 
#summary(model.avg(fmList))
sw(ms2)
sw(subset(ms2,delta<=4))
am <- model.avg(subset(ms2,delta<=4))
coef(am)
summary(am)






####drought severity figure by year
# change facet labels
new_labs <- as_labeller(
  c(`1` = "Year 1",
    `2` = "Year 2",
    `3` = "Year 3"))

###simple model comparisons with AIC
tempdf <- subset(full_y3, Ann_Per == "Perennial")

drt <- lme(anpp_response~drtsev.1, random = ~1|ipcc_regions,data = tempdf)
map <- lme(anpp_response~map, random = ~1|ipcc_regions,data = tempdf)
AI <- lme(anpp_response~AI, random = ~1|ipcc_regions,data = tempdf)
cv <- lme(anpp_response~cv_ppt_inter, random = ~1|ipcc_regions,data = tempdf)
seasonality <- lme(anpp_response~seasonality_index, random = ~1|ipcc_regions,data = tempdf)

AIC(drt, map, AI, cv, seasonality)
summary(drt)
r.squaredGLMM(drt)




subset(data.anpp.summary,n_treat_years >=1 & n_treat_years <= 4)%>%
  subset(Ann_Per == "Perennial")%>%
  ggplot( aes(drtsev.1, anpp_response, color = e.n, fill = e.n))+
  facet_wrap(~n_treat_years)+
  scale_color_manual(values = c("#da7901", "grey" ))+
  scale_fill_manual(values = c("#da7901", "grey" ))+
  geom_point(aes(),alpha = 0.8,#pch = 21,
             size=3)+
  #scale_shape_manual(values = c(19, 21))+
  geom_smooth(aes(),method = "lm")+
  #scale_linetype_manual(values = c("solid","dashed"))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  #scale_color_manual( values = c("#866475","#789ac0" ))+ #need to change colors to extreme vs nominal"#E58601", "#46ACC8"
  xlab("Drought severity (percent reduction of MAP)")+
  ylab("ANPP response")+
  theme_base()+
  theme(legend.position = "none")



tempdf <- subset(data.anpp.summary, Ann_Per == "Perennial" & is.na(drtsev.1) == FALSE)
mod <- lme(anpp_response~drtsev.1*n_treat_years*e.n, random = ~1|ipcc_regions/site_code, data = tempdf)
summary(mod)

ggsave(
  "C:/Users/ohler/Dropbox/IDE/figures/anpp_duration/fig3.pdf",
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


subset(data.anpp.summary,n_treat_years >=1 & n_treat_years <= 4)%>%
  subset(Ann_Per == "Perennial")%>%
  ggplot( aes(drtsev.1, anpp_response))+
  facet_wrap(~n_treat_years)+
  geom_point(aes(color = type),alpha = 0.8,#pch = 21,
             size=3)+
  geom_smooth(method = "lm", color = "black")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  scale_color_manual( values = c("#1E4D2B", "#C8C372"))+
  xlab("Drought severity (percent reduction of MAP)")+
  ylab("ANPP response")+
  theme_base()+
  theme(legend.position = "none")









#inset separated by extreme and nominal
mod <- lme(anpp_response~drtsev.1, random = ~1|ipcc_regions, data = subset(data.anpp.summary,n_treat_years ==1 & Ann_Per == "Perennial" & e.n == "extreme"))
summary(mod) 
r.squaredGLMM(mod)#R-squaredm 
slopey1 <- summary(mod)$coefficients$fixed[[2]]
se1 <- summary(mod)$tTable[,2]

mod <- lme(anpp_response~drtsev.1, random = ~1|ipcc_regions,data = subset(data.anpp.summary,n_treat_years ==1 & Ann_Per == "Perennial" & e.n == "nominal"))
summary(mod)
r.squaredGLMM(mod)#R-squaredm
slopey2 <- summary(mod)$coefficients$fixed[[2]]
se2 <- summary(mod)$tTable[,2]


mod <- lme(anpp_response~drtsev.1, random = ~1|ipcc_regions,data = subset(data.anpp.summary,n_treat_years ==2 & Ann_Per == "Perennial" & e.n == "extreme"))
summary(mod)
r.squaredGLMM(mod)#R-squaredm
slopey3 <- summary(mod)$coefficients$fixed[[2]]
se3 <- summary(mod)$tTable[,2]


mod <- lme(anpp_response~drtsev.1, random = ~1|ipcc_regions,data = subset(data.anpp.summary,n_treat_years ==2 & Ann_Per == "Perennial" & e.n == "nominal"))
summary(mod)
r.squaredGLMM(mod)#R-squaredm
slopey4 <- summary(mod)$coefficients$fixed[[2]]
se4 <- summary(mod)$tTable[,2]


mod <- lme(anpp_response~drtsev.1, random = ~1|ipcc_regions,data = subset(data.anpp.summary,n_treat_years ==3 & Ann_Per == "Perennial" & e.n == "extreme"))
summary(mod)
r.squaredGLMM(mod)#R-squaredm 
slopey5 <- summary(mod)$coefficients$fixed[[2]]
se5 <- summary(mod)$tTable[,2]

mod <- lme(anpp_response~drtsev.1, random = ~1|ipcc_regions,data = subset(data.anpp.summary,n_treat_years ==3 & Ann_Per == "Perennial" & e.n == "nominal"))
summary(mod)
r.squaredGLMM(mod)#R-squaredm
slopey6 <- summary(mod)$coefficients$fixed[[2]]
se6 <- summary(mod)$tTable[,2]

#INSET
data.frame(n_treat_years = c("1e", "1n", "2e", "2n", "3e","3n"), slope = c(slopey1, slopey2, slopey3, slopey4, slopey5, slopey6), se = c(se1, se2, se3, se4, se5, se6))%>%
  ggplot(aes(n_treat_years, slope))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_pointrange(aes(ymin = slope-se, ymax = slope+se))+
  xlab("Years of drought")+
  theme_base()


#stats
mod <- lme(anpp_response~drtsev.1*yrxextreme, random = ~1|ipcc_regions/site_code,data = subset(data.anpp.summary, Ann_Per == "Perennial")%>%
             unite("yrxextreme",c("n_treat_years", "e.n"), sep = "::", remove = FALSE))
summary(mod)

pairs(emtrends(mod, ~as.factor(yrxextreme), var="drtsev.1"))


mod <- lme(anpp_response~drtsev.1*n_treat_years*e.n, random = ~1|ipcc_regions/site_code,data = subset(data.anpp.summary, Ann_Per == "Perennial"))
summary(mod)






###
mod <- lme(anpp_response~drtsev.1, random = ~1|ipcc_regions, data = subset(data.anpp.summary,n_treat_years ==1 & Ann_Per == "Perennial"))
summary(mod) 
r.squaredGLMM(mod)#R-squaredm 0.08
slopey1 <- summary(mod)$coefficients$fixed[[2]]
se1 <- summary(mod)$tTable[,2]

mod <- lme(anpp_response~drtsev.1, random = ~1|ipcc_regions,data = subset(data.anpp.summary,n_treat_years ==2 & Ann_Per == "Perennial"))
summary(mod)
r.squaredGLMM(mod)#R-squaredm 0.06
slopey2 <- summary(mod)$coefficients$fixed[[2]]
se2 <- summary(mod)$tTable[,2]


mod <- lme(anpp_response~drtsev.1, random = ~1|ipcc_regions,data = subset(data.anpp.summary,n_treat_years ==3 & Ann_Per == "Perennial"))
summary(mod)
r.squaredGLMM(mod)#R-squaredm 0.09
slopey3 <- summary(mod)$coefficients$fixed[[2]]
se3 <- summary(mod)$tTable[,2]

#INSET
data.frame(n_treat_years = c("1", "2", "3"), slope = c(slopey1, slopey2, slopey3), se = c(se1, se2, se3))%>%
  ggplot(aes(n_treat_years, slope))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_pointrange(aes(ymin = slope-se, ymax = slope+se))+
  xlab("Years of drought")+
  theme_base()


ggsave(
  "C:/Users/ohler/Dropbox/IDE/figures/anpp_duration/fig3_inset.pdf",
  plot = last_plot(),
  device = "pdf",
  path = NULL,
  scale = 1,
  width = 2,
  height = 2.75,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE
)


mod <- lme(anpp_response~drtsev.1*n_treat_years, random = ~1|ipcc_regions/site_code, data = subset(data.anpp.summary, n_treat_years >=1 & n_treat_years <= 3 & Ann_Per == "Perennial")%>%
             mutate_at(vars(n_treat_years), factor)
)
summary(mod)
pairs(emtrends(mod, ~as.factor(n_treat_years), var="drtsev.1"))


mod <- lme(anpp_response~drtsev.1*n_treat_years, random = ~1|ipcc_regions/site_code, data = subset(data.anpp.summary, n_treat_years >=1 & n_treat_years <= 3 & Ann_Per == "Perennial"))
summary(mod)


mod <- lme(anpp_response~drtsev.1*n_treat_days, random = ~1|ipcc_regions/site_code, data = subset(data.anpp.summary, n_treat_years >=1 & n_treat_years <= 3 & Ann_Per == "Perennial"))
summary(mod)

mod <- lme(anpp_response~drtsev.1*n_treat_days, random = ~1|ipcc_regions/site_code, data = subset(data.anpp.summary, n_treat_years >=0.5 & n_treat_years <= 3 & Ann_Per == "Perennial"))
summary(mod)


#extract and analyze residuals
tempdf <- subset(data.anpp.summary, n_treat_years >=0.5 & n_treat_years <= 3 & Ann_Per == "Perennial")
mod <- lme(anpp_response~drtsev.1, random = ~1|ipcc_regions/site_code, data = tempdf)
summary(mod)

mod.resid <- residuals(mod)
tempdf$mod.resid <- mod.resid
mod.resid <- lm(mod.resid~n_treat_days,data=tempdf)
summary(mod.resid)
visreg(mod.resid, ylab = "model residuals")





#####Stitches plot for year 3 only

data.anpp.summary%>%
  subset( n_treat_years == 3)%>%
  dplyr::mutate(site_code = fct_reorder(site_code, desc(anpp_response)))%>%
  ggplot(  aes(site_code, anpp_response, color = type))+
  geom_pointrange(aes(ymin = anpp_response-anpp_response.se, ymax = anpp_response+anpp_response.se, fill = cut(drtsev.1, 6)))+
  scale_color_manual("Prevailing veg type", values = c("#D9782D", "#1E4D2B", "#C8C372" ))+
  geom_hline(yintercept = 0,linetype="dashed")+
  ylim(-5.8, 5.8)+#this removes error bars from hoide.de and chilcas.ar. The values at those sites are nuts so I don't know what to do about it
  ylab("ANPP response")+
  xlab("")+
  
  coord_flip()+
  theme_base()


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
  scale_color_manual("Vegetation type", values = c("#D9782D", "#1E4D2B", "#C8C372" ))+
  geom_hline(yintercept = 0,linetype="dashed")+
  ylim(-5.8, 5.8)+#this removes error bars from hoide.de and chilcas.ar. The values at those sites are nuts so I don't know what to do about it
  ylab("Avg ANPP response")+
  xlab("")+
  
  coord_flip()+
  theme_base()


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
  geom_bar(stat = "identity", fill = "dodgerblue")+
  geom_hline(yintercept = 0,linetype="dashed")+
  geom_hpline(aes(x = site_code, y=as.numeric(drought_trt)/100))+
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
  width = 4,
  height = 10,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE
)

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



data.anpp.year%>%
  # subset(e.n != "NA")%>%
  subset(type != "Annual")%>%
  
  ggplot(aes(as.factor(n_treat_years), anpp_response, color = type #e.n
  ))+
  geom_pointrange(aes(ymin = anpp_response-anpp_response.error, ymax = anpp_response+anpp_response.error), position = position_dodge(width = 0.5))+
  #ylim(-1.2, 0.3)+
  geom_hline(yintercept = 0,linetype="dashed")+
  xlab("Years of drought")+
  ylab("ANPP response")+
  scale_color_manual("Prevailing veg type", values = c( "#1E4D2B", "#C8C372" ))+ #"#D9782D","#1E4D2B", "#C8C372""#1e3e22", "#a3680f" "#00b844","#f2c300"
  #coord_flip()+
  theme_base()+
  theme(axis.ticks.length=unit(-0.25, "cm"))

ggsave(
  "C:/Users/ohler/Dropbox/IDE/figures/anpp_duration/fig1_response-by-year-type-confidence.pdf",
  plot = last_plot(),
  device = "pdf",
  path = NULL,
  scale = 1,
  width = 6,
  height = 3,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE
)
#inset annuals
data.anpp.year%>%
  subset(type == "Annual")%>%
  
  ggplot(aes(as.factor(n_treat_years), anpp_response))+
  geom_pointrange(aes(ymin = anpp_response-anpp_response.error, ymax = anpp_response+anpp_response.error), position = position_dodge(width = 0.5), color = "#D9782D")+ # "#df0000"
  #ylim(-1.2, 0.3)+
  geom_hline(yintercept = 0,linetype="dashed")+
  xlab("")+
  ylab("")+
  #  scale_color_manual("Prevailing veg type", values = c("#00b844","#f2c300" ))+ #"#D9782D","#1E4D2B", "#C8C372""#1e3e22", "#a3680f" 
  #coord_flip()+
  theme_base()+
  theme(axis.ticks.length=unit(-0.25, "cm"))

ggsave(
  "C:/Users/ohler/Dropbox/IDE/figures/anpp_duration/fig1_response-by-year-type-confidence_annualsinset.pdf",
  plot = last_plot(),
  device = "pdf",
  path = NULL,
  scale = 1,
  width = 3,
  height = 3,
  units = c("in"),
  dpi = 600,
  limitsize = TRUE
)




mod <- lme(anpp_response~type*as.factor(n_treat_years), random = ~1|ipcc_regions/site_code, data = data.anpp.summary)
summary(mod)
library(emmeans)
means <- emmeans(mod, ~n_treat_years*type)
emmeans(mod, list(pairwise ~ type), adjust = "tukey")
emmeans(mod, list(pairwise ~ n_treat_years*type), adjust = "tukey")
confint(means)


ggplot(confint(means),aes(as.factor(n_treat_years), emmean, color = type
))+
  geom_pointrange(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(width = 0.5))+
  #ylim(-1.2, 0.3)+
  geom_hline(yintercept = 0,linetype="dashed")+
  xlab("Years of drought")+
  ylab("ANPP response")+
  scale_color_manual("Prevailing veg type", values = c("#D9782D", "#1E4D2B", "#C8C372" ))+
  #coord_flip()+
  theme_base()+
  theme(axis.ticks.length=unit(-0.25, "cm"))






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



data.anpp.year%>%
  # subset(e.n != "NA")%>%
  #   subset(Ann_Per != "Perennial")%>%
  
  ggplot(aes(as.factor(n_treat_years), anpp_response, color = e.n
  ))+
  geom_pointrange(aes(ymin = anpp_response-anpp_response.se, ymax = anpp_response+anpp_response.se), position = position_dodge(width = 0.5))+
  #ylim(-1.2, 0.3)+
  geom_hline(yintercept = 0,linetype="dashed")+
  xlab("Years of drought")+
  ylab("ANPP response")+
  scale_color_manual("Extreme or nominal", values = c("firebrick2", "dodgerblue" ))+
  #coord_flip()+
  theme_base()+
  theme(axis.ticks.length=unit(-0.25, "cm"))




# mod <- lmer(anpp_response~e.n*as.factor(n_treat_years)+(1|site_code), data = subset(data.anpp.summary,Ann_Per == "Perennial"))
#summary(mod)
#library(emmeans)
#emmeans(mod, list(pairwise ~ e.n), adjust = "tukey")
#emmeans(mod, list(pairwise ~ n_treat_years*e.n), adjust = "tukey")

#ggsave(
#  "C:/Users/ohler/Dropbox/IDE/figures/anpp_duration/fig1_allsites.pdf",
#  plot = last_plot(),
#  device = "pdf",
#  path = NULL,
#  scale = 1,
#width = 3,
#  width = 5,
#  height = 3,
#  units = c("in"),
#  dpi = 600,
#  limitsize = TRUE
#)

mod <- lmer(anpp_response~as.factor(n_treat_years)*map + (1|site_code), data.anpp.summary)
summary(mod)
anova(mod)

mod <- lme(anpp_response~as.factor(n_treat_years) + (1|ipccsite_code), subset(data.anpp.summary, Ann_Per == "Perennial"))
summary(mod)
anova(mod)



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



#########categorize 3-year e/n

threeyearsum <- data.anpp.summary%>%
  subset(Ann_Per == "Perennial")%>%
  ddply(.(site_code), function(x)data.frame(
    threeyearanpp = mean(x$anpp_response)
  ))

threeyearhistory <- data.anpp.summary%>%
  subset(Ann_Per == "Perennial")%>%
  dplyr::select(site_code, n_treat_years, e.n)%>%
  pivot_wider(names_from = n_treat_years, values_from = e.n)%>%
  tidyr::unite(col = "history",c('1','2','3'), sep = "::")

threeyearsummary <- left_join(threeyearsum, threeyearhistory, by = "site_code")


ggplot(threeyearsummary, aes(x = history, y = threeyearanpp))+
  geom_point()+
  theme_base()+
  theme(axis.text.x = element_text(angle=90))

threeyearsummary%>%
  ddply(.(history), function(x)data.frame(
    threeyearanpp = mean(x$threeyearanpp),
    se = sd(x$threeyearanpp)/length(x$history)
  ))%>%
  ggplot(aes(history, threeyearanpp))+
  geom_pointrange(aes(ymax = threeyearanpp+se, ymin = threeyearanpp-se))+
  geom_hline(yintercept = 0)+
  theme_base()+
  theme(axis.text.x = element_text(angle=90))
##bunch categories






##Alan's version
alan <- data.anpp.summary%>%
  subset(Ann_Per == "Perennial")%>%
  unite(historyroad, c("e.n", "prev_e.n", "prev_e.n2", "prev_e.n3"), sep = "::")%>%
  subset(historyroad == "extreme::extreme::extreme::extreme" | historyroad == "extreme::extreme::extreme::nominal" | historyroad == "extreme::extreme::extreme::NA" | historyroad == "extreme::extreme::nominal::nominal" | historyroad == "extreme::extreme::nominal::NA" | historyroad == "extreme::extreme::NA::NA" | historyroad == "extreme::nominal::nominal::nominal" | historyroad == "extreme::nominal::nominal::NA" | historyroad == "extreme::nominal::NA::NA" | historyroad == "extreme::NA::NA::NA")

alan$history <- ifelse(alan$historyroad == "extreme::extreme::extreme::extreme", "Four",
                       ifelse(alan$historyroad == "extreme::extreme::extreme::nominal" | alan$historyroad == "extreme::extreme::extreme::NA", "Three",
                              ifelse(alan$historyroad == "extreme::extreme::nomnal::nominal" |alan$historyroad == "extreme::extreme::nominal::NA" |alan$historyroad == "extreme::extreme::NA::NA", "Two",
                                     "One")))


ggplot(alan, aes(x = factor(history,level=c("One", "Two", "Three", "Four")), anpp_response))+
  geom_point()+
  theme_base()



#all nominal avg
mod <- lme(anpp_response~1, random = ~1|ipcc_regions/site_code, data = subset(data.anpp.summary, e.n == "nominal"))
nominal.avg <- summary(mod)[[20]][1]
nominal.se <- summary(mod)[[20]][2]

#all extreme avg
mod <- lme(anpp_response~1, random = ~1|ipcc_regions/site_code, data = subset(data.anpp.summary, e.n == "extreme"))
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





#year one nominal avg
mod <- lme(anpp_response~1, random = ~1|ipcc_regions/site_code, data = subset(data.anpp.summary, e.n == "nominal" & n_treat_years == 1 & Ann_Per == "Perennial"))
nominal.avg <- summary(mod)[[20]][1]
nominal.se <- summary(mod)[[20]][2]

#year one extreme avg
mod <- lme(anpp_response~1, random = ~1|ipcc_regions/site_code, data = subset(data.anpp.summary, e.n == "extreme" & n_treat_years == 1 & Ann_Per == "Perennial"))
summary(mod)
extreme.avg <- summary(mod)[[20]][1]
extreme.se <- summary(mod)[[20]][2]

data.frame(history = c("One", "Two", "Three", "Four"), avg = c(one.avg, two.avg, three.avg, four.avg), se = c(one.se, two.se, three.se, four.se))%>%
  ggplot( aes(x = factor(history,level=c("One", "Two", "Three", "Four")), avg))+
  geom_pointrange(aes(ymax = avg+se, ymin = avg-se))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = nominal.avg, color = "blue")+
  geom_hline(yintercept = nominal.avg+nominal.se, color = "blue", linetype = "dashed")+
  geom_hline(yintercept = nominal.avg-nominal.se, color = "blue", linetype = "dashed")+
  geom_hline(yintercept = extreme.avg, color = "red")+
  geom_hline(yintercept = extreme.avg+extreme.se, color = "red", linetype = "dashed")+
  geom_hline(yintercept = extreme.avg-extreme.se, color = "red", linetype = "dashed")+
  xlab("# consecutive years extreme drought")+
  ylab("ANPP response")+
  theme_base()


ggsave(
  "C:/Users/ohler/Dropbox/IDE/figures/anpp_duration/fig3_consecutive-years.pdf",
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



alan$historycont <- as.numeric(revalue(alan$history, c("One"=1, "Two"=2, "Three"=3, "Four"=4)))
mod <- lme(anpp_response~history, random = ~1|ipcc_regions/site_code, method = "REML",data = alan)
summary(mod)
pairs(emmeans(mod, ~history))

mod <- lme(anpp_response~historycont, random = ~1|ipcc_regions/site_code, data = alan)
summary(mod)
r.squaredGLMM(mod)

x <- ggpredict(mod, c("historycont"))

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
  ylab("ANPP response")+
  theme_base()



##Sites to ask for 4 years
oneyears <- subset(data.anpp.summary, n_treat_years == 1)%>%
  dplyr::select(site_code, year, n_treat_years)


threeyears <- subset(data.anpp.summary, n_treat_years == 3)%>%
  dplyr::select(site_code, year, n_treat_years)
fouryears <- subset(data.anpp.summary, n_treat_years == 4)%>%
  dplyr::select(site_code, year, n_treat_years)

temp <- left_join(threeyears, oneyears, by = "site_code")%>%
  subset(is.na(year.y) == TRUE)


###

data.anpp.summary%>%
  dplyr::select(site_code, n_treat_years, e.n)%>%
  pivot_wider(names_from = n_treat_years, values_from = e.n)%>%
  pivot_longer(cols = c("1","2","3","4"), names_to = "n_treat_years", values_to = "e.n")%>%
  
  ggplot(aes(n_treat_years, forcats::fct_rev(site_code), fill = e.n))+
  geom_tile(colour = "black")+
  scale_fill_manual(values = c( "#da7901" , "grey"))+ #burnt sienna "#E97451"
  ylab("")+
  xlab("Treatment year")+
  theme_bw()


