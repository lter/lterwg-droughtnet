###########This script is supposed to do a lot of same stuff as the original duration lag analysis, but using long-term mean anpp to calculate response ratios

library(tidyverse)
library(plyr)
library(lmerTest)
library(nlme)
library(visreg)
library(MuMIn)
library(ggthemes)
library(ggeffects)



anpp_ppt_map <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/anpp_ppt_2023-01-02.csv")

Site_Elev.Disturb <- read.csv("C:/Users/ohler/Dropbox/IDE MS_Single year extreme/Data/Site_Elev-Disturb.csv")

data.anpp <- anpp_ppt_map%>%
  left_join( Site_Elev.Disturb, by = "site_code")%>%
  subset(habitat.type == "Grassland" | habitat.type == "Shrubland")#%>%
#subset(ppt.1 ==0 |ppt.2 ==0 |ppt.3 ==0 |ppt.4 ==0 )

anpp.mean <- anpp_ppt_map%>%
  #left_join( Site_Elev.Disturb, by = "site_code")%>%
  #subset(habitat.type == "Grassland" | habitat.type == "Shrubland")%>%
  subset(trt == "Control")%>%
  ddply(.(site_code, year),function(x)data.frame(mass = mean(x$mass)))%>% #summarizes controls for each year
  ddply(.(site_code),function(x)data.frame(mean.mass = mean(x$mass), n_years = length(x$mass)))#summarizes controls across years



length(unique(data.anpp$site_code)) #110

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
  dplyr::filter(Drought>=2 & Control>=1)%>%
  dplyr::as_tibble()

#Switch back to long format for merge
Plot.trt.ct2<-gather(Plottrt_wide1,trt,Plot.count,Drought:Control)

#Merge unique trt count back with data frame to filter
data.anpp1<-merge(data.anpp,Plot.trt.ct2,by=c("site_code","trt","year"))

setdiff(data.anpp$site_code,data.anpp1$site_code) #no sites eliminated here
length(unique(data.anpp1$site_code)) #1o7

#controls <- data.anpp1%>%
#  subset(trt == "Control" )%>%
#  ddply(.(site_code, year),
#        function(x)data.frame(
#          mean.control = mean(x$mass)
#        ))
num.treat.years <- data.anpp1[,c("site_code", "n_treat_years")]%>%
  unique()%>%
  subset(n_treat_years>=1&n_treat_years<=4)

num.treat.years <- ddply(num.treat.years,.(site_code),
                         function(x)data.frame(
                           num.years = length(x$n_treat_years)
                         ))

data.anpp2 <- data.anpp1%>%
  left_join(num.treat.years, by = "site_code")%>%
  subset(num.years == 4)

data.anpp2$rue <- data.anpp2$mass/data.anpp2$ppt.1

tempdf <-subset(data.anpp2, n_treat_years ==1)
mod <- lmer(rue~trt + (1|site_code), data = tempdf)
summary(mod)
visreg(mod)

tempdf <-subset(data.anpp2, n_treat_years ==2)
mod <- lmer(rue~trt + (1|site_code), data = tempdf)
summary(mod)
visreg(mod)



tempdf <-subset(data.anpp2, n_treat_years >=1 &n_treat_years <=4)
mod <- lmer(rue~trt*n_treat_days + (1|site_code), data = tempdf)
summary(mod)
visreg(mod)



site_forplot <- ddply(tempdf, .(site_code, trt,n_treat_days),function(x)data.frame(rue = mean(x$rue)))
ggplot(tempdf, aes(n_treat_days, rue, color = trt))+
  geom_point(data=site_forplot, aes(n_treat_days, rue))+
  geom_smooth(method = "lm")+
  scale_color_manual(values = c("#0099f6","#df0000"))+
  theme_base()


#visreg(mod, "n_treat_days", by="trt", overlay=TRUE)


#############################################
###########################################
####################################


data.anpp2$anpp_response <- log(data.anpp2$mass/data.anpp2$mean.mass)
data.anpp2$drtsev.1 <- ((data.anpp2$ppt.1-data.anpp2$precip)/data.anpp2$precip)
data.anpp2$drtsev.2 <- ((data.anpp2$ppt.2-data.anpp2$precip)/data.anpp2$precip)
data.anpp2$drtsev.3 <- ((data.anpp2$ppt.3-data.anpp2$precip)/data.anpp2$precip)
data.anpp2$drtsev.4 <- ((data.anpp2$ppt.4-data.anpp2$precip)/data.anpp2$precip)





data.anpp.summary <- data.anpp2%>%
  ddply(.(site_code, year, drtsev.1,drtsev.2,drtsev.3,drtsev.4, n_treat_years),
        function(x)data.frame(
          anpp_response = mean(x$anpp_response),
          anpp_response.error = qt(0.975, df=length(x$habitat.type)-1)*sd(x$anpp_response, na.rm = TRUE)/sqrt(length(x$habitat.type)-1),
          n_treat_days = mean(x$n_treat_days)
        ))%>%
  subset(n_treat_years >= 1 & n_treat_years<= 4)

length(unique(data.anpp.summary$site_code))