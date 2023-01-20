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


data.anpp2 <- merge(data.anpp1, anpp.mean, by = c("site_code"))%>%
  subset(trt == "Drought")%>%
  subset(n_years>=4)%>%
  left_join(num.treat.years, by = "site_code")%>%
  subset(num.years == 4 | num.years == 3)


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
  subset(n_treat_years >= 1 & n_treat_years<= 3)

length(unique(data.anpp.summary$site_code))



big.mod <- lmer(anpp_response~n_treat_days*drtsev.1 + (1|site_code), data = data.anpp.summary)
summary(big.mod)
#visreg(big.mod)


x <- ggpredict(big.mod,"n_treat_days")
plot(x)+
  #  ylim(0,15)+
  ylab("ANPP response (LRR)")+
  xlab("Treatment days")+
  geom_point(data = data.anpp.summary, aes(n_treat_days, anpp_response))+
  theme_base()

x <- ggpredict(big.mod,"drtsev.1")
plot(x)+
  #  ylim(0,15)+
  ylab("ANPP response (LRR)")+
  xlab("Drought severity")+
  geom_point(data = data.anpp.summary, aes(drtsev.1, anpp_response))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  theme_base()

ggplot(data.anpp.summary, aes(drtsev.1, anpp_response))+
  geom_smooth(method = "lm", formula= exp(y)~x)



##this bit tries testing the model residuals but it doesn't show anything
temp.mod <- lmer(anpp_response~drtsev.1 + (1|site_code), data = data.anpp.summary)
resid <- residuals(temp.mod)
data.anpp.summary$resid <- resid
mod.resid <- lm(resid~n_treat_days,data=data.anpp.summary)
mod.resid.2 <- lm(resid~n_treat_days+I(n_treat_days^2),data=data.anpp.summary)
AIC(mod.resid,mod.resid.2)
summary(mod.resid)
visreg(mod.resid)
#can delete above section showing that model residuals aren't explanatory


##############try model selection
library(MASS)
library(nlme)
#Backward model selection - skipping backward in favor of forward since backward likes the maximal model for some ungodly reason
lmFull <- lme(anpp_response~drtsev.1 * drtsev.2 * drtsev.3 , random = ~1|site_code, data=data.anpp.summary, method = "ML", na.action = na.exclude, correlation = corAR1())


stepAIC(lmFull, scope = list(upper = lmFull,
                            lower = ~1),
        trace = F)

lmNull <- lme(anpp_response~1, random = ~ 1 |site_code, data = data.anpp.summary, method = "ML",  na.action=na.exclude, correlation = corAR1())


#Forward model selection
stepAIC(lmNull, scope = list(upper = lmFull,
                             lower = ~1),
        trace = F)

winning.mod <- lme(anpp_response ~ drtsev.1 + drtsev.2 + drtsev.3 + drtsev.1:drtsev.2 +      drtsev.2:drtsev.3 + drtsev.1:drtsev.3, random = ~ 1 |site_code, data = data.anpp.summary, method = "ML",  na.action=na.exclude, correlation = corAR1())
summary(winning.mod)

winning.mod.lmer <- lmer(anpp_response ~ drtsev.1 + drtsev.2 + drtsev.3 + drtsev.1:drtsev.2 +      drtsev.2:drtsev.3 + drtsev.1:drtsev.3 +(1|site_code),data = data.anpp.summary)
summary(winning.mod.lmer)

visreg2d(winning.mod.lmer, "drtsev.1", "drtsev.2", plot.type="gg", col = c("red", "white", "forestgreen"))+
  geom_point(data = data.anpp.summary, aes(x=drtsev.1, y=drtsev.2))+
  xlab("Current year drought severity")+
  ylab("Previous year drought severity")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme_base()  




#Backward model selection - skipping backward in favor of forward since backward likes the maximal model for some ungodly reason
lmFull <- lm(anpp_response~drtsev.1 * drtsev.2 * drtsev.3 , random = ~1|site_code, data=subset(data.anpp.summary, n_treat_years == 3))


stepAIC(lmFull, scope = list(upper = lmFull,
                             lower = ~1),
        trace = F)

lmNull <- lme(anpp_response~1,  data = subset(data.anpp.summary, n_treat_years == 3), method = "ML",  na.action=na.exclude, correlation = corAR1())


#Forward model selection
stepAIC(lmNull, scope = list(upper = lmFull,
                             lower = ~1),
        trace = F)

stepAIC(lmFull, scope = list(upper = lmFull,
                             lower = ~1),direction = "both",
        trace = F)

tempdf <-subset(data.anpp.summary, n_treat_years == 3)
winning.mod <- lm(anpp_response ~ drtsev.1 + drtsev.2 + drtsev.3 + drtsev.1:drtsev.2 +      drtsev.1:drtsev.3 + drtsev.2:drtsev.3, data = tempdf)
summary(winning.mod)

#winning.mod.lmer <- lmer(anpp_response ~ drtsev.1 + drtsev.2 + drtsev.3 + drtsev.1:drtsev.2 +      drtsev.2:drtsev.3 + drtsev.1:drtsev.3 +(1|site_code),data = data.anpp.summary)
#summary(winning.mod.lmer)

visreg2d(winning.mod, "drtsev.1", "drtsev.2", plot.type="gg", col = c("red", "white", "forestgreen"))+
  #geom_point(data = data.anpp.summary, aes(x=drtsev.1, y=drtsev.2))+
  xlab("Current year drought severity")+
  ylab("Previous year drought severity")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme_base()  



####drought severity figure by year
subset(data.anpp.summary,n_treat_years >=1 & n_treat_years <= 4)%>%
  ggplot( aes(drtsev.1, anpp_response))+
  facet_wrap(~n_treat_years)+
  geom_point()+
  geom_smooth(method = "lm")+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  xlab("Drought severity")+
  theme_bw()


mod <- lm(anpp_response~drtsev.1, data = subset(data.anpp.summary,n_treat_years ==1))
summary(mod)
mod <- lm(anpp_response~drtsev.1, data = subset(data.anpp.summary,n_treat_years ==2))
summary(mod)
mod <- lm(anpp_response~drtsev.1, data = subset(data.anpp.summary,n_treat_years ==3))
summary(mod)
mod <- lm(anpp_response~drtsev.1, data = subset(data.anpp.summary,n_treat_years ==4))
summary(mod)

mod <- lmer(anpp_response~drtsev.1*as.factor(n_treat_years)+ (1|site_code), data = subset(data.anpp.summary, n_treat_years >=1 & n_treat_years <= 4))
summary(mod)


temp <- subset(data.anpp2,n_treat_years >=1 & n_treat_years <= 4)
mod <- lmer(anpp_response~drtsev.1 * as.factor(n_treat_years) + (1|site_code), data = temp)
summary(mod)
r.squaredGLMM(mod)
visreg(mod)


#####Can I remake stitches plots by year and plot drought severity and response by year?
ordered.df <- subset(data.anpp.summary,n_treat_years ==1)%>%
  dplyr::mutate(site_code = fct_reorder(site_code, desc(anpp_response)))%>%
  rbind(subset(data.anpp.summary,n_treat_years ==2))%>%
  rbind(subset(data.anpp.summary,n_treat_years ==3))%>%
  rbind(subset(data.anpp.summary,n_treat_years ==4))

ggplot(ordered.df,  aes(site_code, anpp_response))+
  facet_wrap(~n_treat_years)+
  geom_pointrange(aes(ymin = anpp_response-anpp_response.error, ymax = anpp_response+anpp_response.error))+
  geom_hline(yintercept = 0,linetype="dashed")+
  ylim(c(-5,3))+
  ylab("anpp_response")+
  xlab("")+
  coord_flip()+
  theme_bw()

library(ggcorrplot)                                  

p.mat <-  data.anpp.summary%>%
  dplyr::select( -c("year", "anpp_response.error", "drtsev.1", "drtsev.2", 'drtsev.3', "drtsev.4", "n_treat_days"))%>%
  pivot_wider(names_from = "n_treat_years", values_from = c("anpp_response"))%>%
  dplyr::select(-site_code)%>%
  drop_na()%>%
  cor_pmat()

data.anpp.summary%>%
  dplyr::select( -c("year", "anpp_response.error", "drtsev.1", "drtsev.2", 'drtsev.3', "drtsev.4", "n_treat_days"))%>%
  pivot_wider(names_from = "n_treat_years", values_from = c("anpp_response"))%>%
  dplyr::select(-site_code)%>%
  drop_na()%>%
  cor()%>%
  ggcorrplot(type = "lower", lab = TRUE, p.mat = p.mat)




################
##average drought severity and average anpp response over 4 years

anpp.avg <- data.anpp2%>%
        ddply(.(site_code, n_treat_years, drtsev.1, mean.mass),function(x)data.frame(
          mass = mean(x$mass)
        ))%>%
        subset(n_treat_years >= 1 & n_treat_years <=4)%>%
        ddply(.(site_code, mean.mass),function(x)data.frame(
          avg_mass = (sum(x$mass)/4),
          avg.drtsev = mean(x$drtsev.1)
        ))

anpp.avg$lrr_avg <- log(anpp.avg$avg_mass/anpp.avg$mean.mass)

mod <- lm(lrr_avg~avg.drtsev, data = anpp.avg)
summary(mod)

ggplot(anpp.avg, aes(avg.drtsev, lrr_avg))+
  geom_smooth(method = "lm")+
  geom_point()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  theme_base()
