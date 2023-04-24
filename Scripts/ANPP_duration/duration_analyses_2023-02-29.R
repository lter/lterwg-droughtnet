###########Uses long term mean anpp to calculate response ratios. A direct follow-up to the 2023 IDE Todos Santos meeting

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

#read ANPP data
data.anpp <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/anpp_ppt_2023-03-16.csv")%>%
  subset(habitat.type == "Grassland" | habitat.type == "Shrubland")#%>%

length(unique(data.anpp$site_code)) #112

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
         e.n=ifelse(ppt.minus.map>0, "nominal", "extreme")) %>%
  dplyr::select(site_code, year, n_treat_years, e.n)


#Only using sites with >= 2 reps for drought and >=1 rep for control
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
Plottrt_wide1<-Plottrt_wide%>%
  dplyr::filter(Drought>=2 & Control>=1)%>%
  dplyr::as_tibble()

#Switch back to long format for merge
Plot.trt.ct2<-gather(Plottrt_wide1,trt,Plot.count,Drought:Control)

#Merge unique trt count back with data frame to filter
data.anpp1<-merge(data.anpp,Plot.trt.ct2,by=c("site_code","trt","year"))

setdiff(data.anpp$site_code,data.anpp1$site_code) #"brandjberg.dk" "garraf.es"  "swift.ca" eliminated here
length(unique(data.anpp1$site_code)) #1o9

##How many treatment years does each site have of the first 4 years?
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
  subset(num.years == 4 | num.years == 3
  ) #change here if using 4 years #reduces dataset to focal sites

length(unique(data.anpp2$site_code)) #67


##Create anpp_response and drought severity metrics
data.anpp2$anpp_response <- log(data.anpp2$mass/data.anpp2$mean.mass)
data.anpp2$drtsev.1 <- -((data.anpp2$ppt.1-data.anpp2$map)/data.anpp2$map)
data.anpp2$drtsev.2 <- -((data.anpp2$ppt.2-data.anpp2$map)/data.anpp2$map)
data.anpp2$drtsev.3 <- -((data.anpp2$ppt.3-data.anpp2$map)/data.anpp2$map)
data.anpp2$drtsev.4 <- -((data.anpp2$ppt.4-data.anpp2$map)/data.anpp2$map)




##Summarize responses by site and year
data.anpp.summary <- data.anpp2%>%
  ddply(.(site_code, year, drtsev.1,drtsev.2,drtsev.3,drtsev.4, n_treat_years, map, habitat.type, e.n),
        function(x)data.frame(
          mean_mass = mean(x$mass),
          ppt.1 = mean(x$ppt.1),
          anpp_response = mean(x$anpp_response),
          anpp_response.error = qt(0.975, df=length(x$habitat.type)-1)*sd(x$anpp_response, na.rm = TRUE)/sqrt(length(x$habitat.type)-1),
          n_treat_days = mean(x$n_treat_days)
        ))%>%
  subset(n_treat_years >= 1 & n_treat_years<= 4) #CHANGE HERE IF YOU"RE GOING UP TO 4 TREATMENT YEARS



##add some covariates to try out in model selection
#sand, MAP, ln(aridity), CV of MAP--- %graminoids is tricky, skpping for now
sandsite <-read.csv("C:\\Users\\ohler\\Dropbox\\IDE MS_Single year extreme\\Data\\site_sand_from_soilgrid.csv")%>%
  dplyr::select(site_code, sand_mean)

ai<-read.csv("C:\\Users\\ohler\\Dropbox\\IDE MS_Single year extreme\\Data\\ai_pet_04-18-2022.csv")%>%
      dplyr::select(site_code, AI)

cv<-read.csv("C:\\Users\\ohler\\Dropbox\\IDE\\data_processed\\climate\\climate_mean_annual_by_site.csv")
cv1<-cv%>%
  dplyr::filter(data_source=="mswep")%>%
  dplyr::select(site_code,cv_ppt_inter)

##MODEL SELECTION WITH YEAR 3 ONLY
#Backward model selection - skipping backward in favor of forward since backward likes the maximal model for some ungodly reason
lmFull <- lm(anpp_response~drtsev.1 * drtsev.2 * drtsev.3 * map #drtsev.4  #+ sand_mean + AI + cv_ppt_inter
             , data=subset(data.anpp.summary, n_treat_years == 3))


lmNull <- lm(anpp_response~1,  data = subset(data.anpp.summary, n_treat_years == 3),  na.action=na.exclude)


#Forward model selection
stepAIC(lmNull, scope = list(upper = lmFull,
                             lower = ~1),
        trace = F)



tempdf <-subset(data.anpp.summary, n_treat_years == 3)
winning.mod <- lm(anpp_response ~ drtsev.1 + drtsev.2 + drtsev.3 + 
                    map + drtsev.1:drtsev.2 + drtsev.2:drtsev.3 + drtsev.2:map
                  , data = tempdf)
summary(winning.mod)


history.df <- data.anpp.summary%>%
  dplyr::select(site_code, n_treat_years, e.n)%>%
  pivot_wider(names_from = n_treat_years, values_from = e.n)%>%
  dplyr::rename(y1 = "1", y2 = "2", y3 = '3')

data.anpp.summary%>%
  left_join(history.df, by = "site_code")%>%
  subset(n_treat_years == 3 & y2 != "NA")%>%
ggplot(aes(drtsev.1, anpp_response, color = y2))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  xlab("Drought severity")+
  ylab("Treatment ANPP / Avg ANPP")+
  theme_base()



##MAP interactions with current year and precious year precipitation

tempdf <- subset(data.anpp.summary, n_treat_years == 3)
current_year.mod <- lm(anpp_response ~ drtsev.1 + drtsev.1:map, data = tempdf)
summary(current_year.mod)

current_year.mod <- lm(anpp_response ~ drtsev.1 *map, data = tempdf)
summary(current_year.mod)

previous_year.mod <- lm(anpp_response ~ drtsev.2 + drtsev.2:map, data = tempdf)
summary(previous_year.mod)

previous_year.mod <- lm(anpp_response ~ drtsev.2 *map, data = tempdf)
summary(previous_year.mod)

visreg2d(current_year.mod, xvar = "map", yvar = "drtsev.1")
visreg(current_year.mod)
visreg(previous_year.mod)
visreg2d(previous_year.mod, xvar = "map", yvar = "drtsev.2")

map.mod <- lm(anpp_response ~  map, data = tempdf)
summary(map.mod)
visreg(map.mod)


tempdf$map.cat <- ifelse(tempdf$map > 400, "High_MAP", "Low_MAP")
tempdf%>%
  subset(anpp_response > -4)%>%
ggplot(aes(drtsev.2, anpp_response, color = map.cat))+
  geom_point()+
  geom_smooth(method = "lm")

####drought severity figure by year
subset(data.anpp.summary,n_treat_years >=1 & n_treat_years <= 3)%>%
  ggplot( aes(drtsev.1, anpp_response))+
  facet_wrap(~n_treat_years)+
  geom_point()+
  geom_smooth(method = "lm")+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  xlab("Drought severity")+
  ylab("Treatment ANPP / Avg ANPP")+
  theme_bw()


mod <- lm(anpp_response~drtsev.1, data = subset(data.anpp.summary,n_treat_years ==1))
mod1 <- lm(anpp_response~poly(drtsev.1,2), data = subset(data.anpp.summary,n_treat_years ==1))
mod2 <- lm(anpp_response~poly(drtsev.1,3), data = subset(data.anpp.summary,n_treat_years ==1))
mod3 <- lm(anpp_response~poly(drtsev.1,4), data = subset(data.anpp.summary,n_treat_years ==1))
AIC(mod, mod1, mod2, mod3)#mod best
summary(mod) #R-squared 0.10
slopey1 <- coef(mod)[[2]]

mod <- lm(anpp_response~drtsev.1, data = subset(data.anpp.summary,n_treat_years ==2))
mod1 <- lm(anpp_response~poly(drtsev.1,2), data = subset(data.anpp.summary,n_treat_years ==2))
mod2 <- lm(anpp_response~poly(drtsev.1,3), data = subset(data.anpp.summary,n_treat_years ==2))
mod3 <- lm(anpp_response~poly(drtsev.1,4), data = subset(data.anpp.summary,n_treat_years ==2))
AIC(mod, mod1, mod2, mod3)#mod1 best
summary(mod1)#R-squared 0.17
slopey2 <- coef(mod)[[2]]
ggplot(subset(data.anpp.summary,n_treat_years ==2), aes(drtsev.1, anpp_response))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~poly(x,2))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  xlab("Drought severity")+
  ylab("Treatment ANPP / Avg ANPP")+
  theme_bw()



mod <- lm(anpp_response~drtsev.1, data = subset(data.anpp.summary,n_treat_years ==3))
mod1 <- lm(anpp_response~poly(drtsev.1,2), data = subset(data.anpp.summary,n_treat_years ==3))
mod2 <- lm(anpp_response~poly(drtsev.1,3), data = subset(data.anpp.summary,n_treat_years ==3))
mod3 <- lm(anpp_response~poly(drtsev.1,4), data = subset(data.anpp.summary,n_treat_years ==3))
AIC(mod, mod1, mod2, mod3)#mod2 best
summary(mod2)#R-squared 0.25
slopey3 <- coef(mod)[[2]]
ggplot(subset(data.anpp.summary,n_treat_years ==3), aes(drtsev.1, anpp_response))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~poly(x,3))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  xlab("Drought severity")+
  ylab("Treatment ANPP / Avg ANPP")+
  theme_bw()


##only if using 4 treatment years
#mod <- lm(anpp_response~drtsev.1, data = subset(data.anpp.summary,n_treat_years ==4))
#summary(mod)
#slopey4 <- coef(mod)[[2]]
##

#potential inset
data.frame(n_treat_years = c(1, 2, 3), slope = c(slopey1, slopey2, slopey3))%>%
  ggplot(aes(n_treat_years, slope))+
  geom_point( size = 5)+
  geom_line()+
 # ylim(-3,0)+
  xlab("Treatment year")+
  theme_base()

mod <- lmer(anpp_response~drtsev.1*as.factor(n_treat_years)+ (1|site_code), data = subset(data.anpp.summary, n_treat_years >=1 & n_treat_years <= 3))
summary(mod)
pairs(emtrends(mod, ~as.factor(n_treat_years), var="drtsev.1"))


#####Can I remake stitches plots by year and plot drought severity and response by year?
ordered.df <- subset(data.anpp.summary,n_treat_years ==1)%>%
  
  rbind(subset(data.anpp.summary,n_treat_years ==2))%>%
  rbind(subset(data.anpp.summary,n_treat_years ==3))%>%
  rbind(subset(data.anpp.summary,n_treat_years ==4))


ggplot(ordered.df,  aes(site_code, anpp_response))+
  facet_wrap(~n_treat_years)+
  geom_pointrange(aes(ymin = anpp_response-anpp_response.error, ymax = anpp_response+anpp_response.error))+
  geom_hline(yintercept = 0,linetype="dashed")+
  ylim(c(-6.3,5))+
  ylab("Treatment ANPP / Avg ANPP")+
  xlab("")+
  coord_flip()+
  theme_bw()

data.anpp.summary%>%
  subset( n_treat_years == 3)%>%
  dplyr::mutate(site_code = fct_reorder(site_code, desc(anpp_response)))%>%
  ggplot(  aes(site_code, anpp_response))+
  geom_pointrange(aes(ymin = anpp_response-anpp_response.error, ymax = anpp_response+anpp_response.error, fill = cut(drtsev.1, 6)))+
  scale_fill_brewer(palette = "Reds", direction = -1
                    , drop = FALSE)+
  geom_hline(yintercept = 0,linetype="dashed")+
  ylim(c(-6.3,5))+ #this removes error bars from hoide.de and chilcas.ar. The values at those sites are nuts so I don't know what to do about it
  ylab("Treatment ANPP / Avg ANPP")+
  xlab("")+
  coord_flip()+
  theme_bw()


##################################
data.anpp.year <- data.anpp.summary%>%
  ddply(.(n_treat_years),
        function(x)data.frame(
          anpp_response = mean(x$anpp_response),
          anpp_response.error = qt(0.975, df=length(x$site_code)-1)*sd(x$anpp_response, na.rm = TRUE)/sqrt(length(x$site_code)-1)
        ))


ggplot(data.anpp.year, aes(fct_rev(as.factor(n_treat_years)), anpp_response))+
  geom_pointrange(aes(ymin = anpp_response-anpp_response.error, ymax = anpp_response+anpp_response.error))+
  ylim(-1.1, 0)+
  geom_hline(yintercept = 0)+
  xlab("Treatment year")+
  ylab("Treatment ANPP / Avg ANPP")+
  coord_flip()+
  theme_base()



mod <- lme(anpp_response~as.factor(n_treat_years), random = ~1|site_code, data = data.anpp.summary,)
summary(mod)


ggplot(data.anpp.year, aes(as.factor(n_treat_years), anpp_response, color = history))+
  #  facet_wrap(~e.n)+
  geom_pointrange(data=data.anpp.year,aes(ymin = anpp_response-anpp_response.error, ymax = anpp_response+anpp_response.error),position=position_dodge(width=.25))+
  #  ylim(-1, 0)+
  geom_hline(yintercept = 0)+
  theme_base()


en.df <- data.anpp.summary %>%
  dplyr::select(site_code, n_treat_years, e.n)%>%
  pivot_wider(names_from = "n_treat_years", values_from = "e.n")

en.df$history <- ifelse(en.df[,2] == "extreme" & en.df[,3] == "extreme", "extreme.extreme.extreme",
                        ifelse(en.df[,2] == "nominal" & en.df[,3] == "extreme", "nominal.extreme.extreme",
                               ifelse(en.df[,2] == "extreme" & en.df[,3] == "nominal", "extreme.nominal.extreme",
                                      "nominal.nominal.extreme"
                               )))

data.anpp.summary1 <- left_join(data.anpp.summary, en.df, by = "site_code")

data.anpp.year <- data.anpp.summary1%>%
  ddply(.(n_treat_years, history),
        function(x)data.frame(
          anpp_response = mean(x$anpp_response),
          anpp_response.error = qt(0.975, df=length(x$site_code)-1)*sd(x$anpp_response, na.rm = TRUE)/sqrt(length(x$site_code)-1),
          n = length(x$site_code),
          anpp_response.se = sd(x$anpp_response)/sqrt(length(x$site_code))
        ))%>%
  subset(history != "NA")


ggplot(subset(data.anpp.year, history == "extreme.extreme.extreme"), aes(as.factor(n_treat_years), anpp_response))+
  #facet_wrap(~history)+
  geom_pointrange(aes(ymin = anpp_response-anpp_response.se, ymax = anpp_response+anpp_response.se),position=position_dodge(width=.25))+
  #  ylim(-1, 0)+
  geom_hline(yintercept = 0)+
  theme_base()

temp.df <- data.anpp.summary1%>%
  subset( history == "extreme.extreme.extreme")
temp.df$n_treat_years <- as.factor(temp.df$n_treat_years)
mod <- lm(anpp_response~n_treat_years,data = temp.df )
summary(mod)
a <- aov(mod)
TukeyHSD(a, "n_treat_years")


temp.df <- data.anpp.summary1%>%
  subset( history == "extreme.extreme.extreme")
temp.df$n_treat_years <- as.factor(temp.df$n_treat_years)
mod <- lme(anpp_response~n_treat_days, random = ~1|site_code, data = temp.df)
summary(mod)
mod <- lme(anpp_response~n_treat_years, random = ~1|site_code, data = temp.df)
summary(mod)
ggplot(temp.df, aes(n_treat_days, anpp_response))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_base()



tempdf <- data.anpp.summary1%>%
  subset(n_treat_years == 3)%>%
  dplyr::rename("two"="2")%>%
  subset(two != "NA")
mod <- lm(anpp_response~drtsev.1*two, data = tempdf)
summary(mod)  











temp <- subset(data.anpp1, n_treat_days >1209 & n_treat_days < 1574)#1574
length(unique(temp$site_code))
