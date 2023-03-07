library(tidyverse)
library(plyr)
library(lmerTest)
library(nlme)
library(visreg)
library(MuMIn)
library(ggthemes)
library(ggeffects)
library(ggpmisc)



anpp_ppt_map <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/anpp_ppt_2023-02-06.csv")

Site_Elev.Disturb <- read.csv("C:/Users/ohler/Dropbox/IDE MS_Single year extreme/Data/Site_Elev-Disturb.csv")

data.anpp <- anpp_ppt_map%>%
  #left_join( Site_Elev.Disturb, by = "site_code")#%>%
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


wide.df <- data.anpp1%>%
  subset(n_treat_years >=1 & n_treat_years <=2)%>%
  ddply(.(site_code, n_treat_years, trt),
        function(x)data.frame(
          mass = mean(x$mass),
          ppt.1 = max(x$ppt.1)
        ))%>%
  pivot_wider(names_from = c(n_treat_years), values_from = c("mass", "ppt.1"))




wide.df$pue <- (wide.df$mass_2-wide.df$mass_1)/(wide.df$ppt.1_2-wide.df$ppt.1_1)





#

mod <-lmer(pue~trt + (1|site_code), data = wide.df)
summary(mod)
visreg(mod)

ggplot(wide.df, aes(trt, pue))+
  geom_boxplot()+
  geom_point()+
  theme_base()+
  ylim(-10,20)
#

















#controls <- data.anpp1%>%
#  subset(trt == "Control" )%>%
#  ddply(.(site_code, year),
#        function(x)data.frame(
#          mean.control = mean(x$mass),
#          ppt.1_control = max(x$ppt.1)
#        ))

#data.anpp2 <- merge(data.anpp1, controls, by = c("site_code", "year"))%>%
#  subset(trt == "Drought")%>%
  


#data.anpp2$anpp_response <- log(data.anpp2$mass/data.anpp2$mean.control)
#data.anpp2$drtsev.1 <- ((data.anpp2$ppt.1-data.anpp2$precip)/data.anpp2$precip)
#data.anpp2$drtsev.2 <- ((data.anpp2$ppt.2-data.anpp2$precip)/data.anpp2$precip)
#data.anpp2$drtsev.3 <- ((data.anpp2$ppt.3-data.anpp2$precip)/data.anpp2$precip)
#data.anpp2$drtsev.4 <- ((data.anpp2$ppt.4-data.anpp2$precip)/data.anpp2$precip)






data.anpp2 <- data.anpp1%>%
  ddply(.(site_code, year, trt, n_treat_years, map),
        function(x)data.frame(
        mass = mean(x$mass),
        ppt = max(x$ppt.1)))%>%
  subset(n_treat_years >= 1 & n_treat_years<= 3)%>%
    pivot_wider(names_from = "trt", values_from = c("mass", "ppt"))%>%
  left_join(EN.df, by = c("site_code", "year"))

data.anpp2$PR <- ((data.anpp2$mass_Control-data.anpp2$mass_Drought)/data.anpp2$mass_Control)/((data.anpp2$ppt_Control-data.anpp2$ppt_Drought)/data.anpp2$ppt_Control)

data.anpp2%>%
#  subset(n_treat_years == 3)%>%
  subset(site_code != "hyide.de" & site_code != "stubai.at")%>%
 # subset(e.n == "extreme")%>%
ggplot(aes(map, PR))+
  facet_wrap(~n_treat_years)+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_base()



#create long-term average ANPP in controls
anpp.mean <- data.anpp%>%
  subset(trt == "Control")%>%
  ddply(.(site_code, year),function(x)data.frame(mass = mean(x$mass)))%>% #summarizes controls for each year
  ddply(.(site_code),function(x)data.frame(mean.mass = mean(x$mass), n_years = length(x$mass)))#summarizes controls across years


data.anpp3 <- data.anpp2%>%
              left_join(anpp.mean, by = c("site_code"))


data.anpp3$RM <- ((data.anpp3$mass_Control-data.anpp3$mass_Drought)/data.anpp3$mean.mass)/((data.anpp3$ppt_Control-data.anpp3$ppt_Drought)/data.anpp3$map)


 data.anpp3%>%
    subset(n_treat_years == 3)%>%
   subset(n_years > 3)%>%
  #subset(site_code != "hyide.de" & site_code != "stubai.at")%>%
  #subset(e.n == "extreme")%>%
  ggplot(aes(map, RM))+
  facet_wrap(~n_treat_years)+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_base()

tempdf <-  data.anpp3%>%
  subset(n_treat_years == 3)%>%
  subset(n_years > 3)
mod <- lm(RM~map, tempdf)
summary(mod)




en.df <- data.anpp3 %>%
  dplyr::select(site_code, n_treat_years, e.n)%>%
  pivot_wider(names_from = "n_treat_years", values_from = "e.n")


en.df$history <- ifelse(en.df[,2] == "extreme" & en.df[,3] == "extreme", "extreme.extreme.extreme",
                        ifelse(en.df[,2] == "nominal" & en.df[,3] == "extreme", "nominal.extreme.extreme",
                               ifelse(en.df[,2] == "extreme" & en.df[,3] == "nominal", "extreme.nominal.extreme",
                                      "nominal.nominal.extreme"
                               )))



data.anpp4 <- left_join(data.anpp3, en.df, by = "site_code")


data.anpp4%>%
#  subset(history == "extreme.extreme.extreme")%>%
  subset(n_years > 3)%>%
  subset(site_code != "hyide.de" & site_code != "stubai.at")%>%
  #subset(e.n == "extreme")%>%
  ggplot(aes(map, RM))+
  facet_wrap(~n_treat_years)+
  geom_point()+
  geom_smooth(method = "lm")+
  stat_poly_eq(formula = y~x)+
  theme_base()

data.anpp4%>%
  #  subset(history == "extreme.extreme.extreme")%>%
  subset(n_years > 3)%>%
  subset(site_code != "hyide.de" & site_code != "stubai.at")%>%
  #subset(e.n == "extreme")%>%
  ggplot(aes(map, PR))+
  facet_wrap(~n_treat_years)+
  geom_point()+
  geom_smooth(method = "lm")+
  stat_poly_eq(formula = y~x)+
  theme_base()



data.anpp4%>%
  subset(history == "extreme.extreme.extreme")%>%
  subset(n_years > 3)%>%
  #subset(site_code != "hyide.de" & site_code != "stubai.at")%>%
  #subset(e.n == "extreme")%>%
  ggplot(aes(map, RM))+
  facet_wrap(~n_treat_years)+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_base()

data.anpp4%>%
  subset(history == "extreme.extreme.extreme")%>%
  subset(n_years > 3)%>%
  #subset(site_code != "hyide.de" & site_code != "stubai.at")%>%
  #subset(e.n == "extreme")%>%
  ggplot(aes(as.factor(n_treat_years), RM))+
  geom_boxplot()+
  theme_base()

tempdf <- data.anpp4%>%
  subset(history == "extreme.extreme.extreme")%>%
  subset(n_years > 3)
mod <- lm(RM~as.factor(n_treat_years), tempdf)
summary(mod)



data.anpp4%>%
  subset(history == "extreme.extreme.extreme")%>%
  subset(n_years > 3)%>%
  #subset(site_code != "hyide.de" & site_code != "stubai.at")%>%
  #subset(e.n == "extreme")%>%
  ggplot(aes(as.factor(n_treat_years), PR))+
  geom_boxplot()+
  theme_base()

tempdf <- data.anpp4%>%
  subset(history == "extreme.extreme.extreme")%>%
  subset(n_years > 3)
mod <- lm(PR~as.factor(n_treat_years), tempdf)
summary(mod)
