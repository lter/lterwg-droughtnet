library(tidyverse)
library(plyr)
library(lmerTest)
library(nlme)
library(visreg)
library(MuMIn)
library(ggthemes)



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






data.anpp.summary <- data.anpp2%>%
  ddply(.(site_code, year, drtsev.1,drtsev.2,drtsev.3,drtsev.4, n_treat_years),
        function(x)data.frame(
          anpp_response = mean(x$anpp_response),
          anpp_response.error = qt(0.975, df=length(x$habitat.type)-1)*sd(x$anpp_response, na.rm = TRUE)/sqrt(length(x$habitat.type)-1)
        ))%>%
  subset(n_treat_years >= 1 & n_treat_years<= 4)
        

##All models to compare
#interactive
#mod.1 <- lmer(anpp_response~drtsev.1 + (1|site_code), data=data.anpp.summary, REML = FALSE)
#mod.2 <- lmer(anpp_response~drtsev.1 * drtsev.2 + (1|site_code), data=data.anpp.summary, REML = FALSE)
#mod.3 <- lmer(anpp_response~drtsev.1 * drtsev.2 * drtsev.3 + (1|site_code), data=data.anpp.summary, REML = FALSE)
#mod.4 <- lmer(anpp_response~drtsev.1 * drtsev.2 * drtsev.3 * drtsev.4 + (1|site_code), data=data.anpp.summary, REML = FALSE)  

#additive
#mod.5 <- lmer(anpp_response~drtsev.1 + (1|site_code), data=data.anpp.summary, REML = FALSE)
#mod.6 <- lmer(anpp_response~drtsev.1 + drtsev.2 + (1|site_code), data=data.anpp.summary, REML = FALSE)
#mod.7 <- lmer(anpp_response~drtsev.1 + drtsev.2 + drtsev.3 + (1|site_code), data=data.anpp.summary, REML = FALSE)
#mod.8 <- lmer(anpp_response~drtsev.1 + drtsev.2 + drtsev.3 + drtsev.4 + (1|site_code), data=data.anpp.summary, REML = FALSE) 
#mod.9 <- lmer(anpp_response~drtsev.1 + drtsev.1:drtsev.2 + (1|site_code), data=data.anpp.summary, #REML = FALSE)
#AIC(mod.1, mod.2, mod.3, mod.4, mod.5, mod.6, mod.7, mod.8, mod.9)

#summary(mod.1)
#r.squaredGLMM(mod.1)
#summary(mod.2)
#r.squaredGLMM(mod.2)
#summary(mod.3)
#r.squaredGLMM(mod.3)
#summary(mod.4)
#r.squaredGLMM(mod.4)
#summary(mod.9)
#r.squaredGLMM(mod.9)


##############try model selection
library(MASS)
library(nlme)
#Backward model selection - skipping backward in favor of forward since backward likes the maximal model for some ungodly reason
lmFull <- lme(anpp_response~drtsev.1 * drtsev.2 * drtsev.3 * drtsev.4, random = ~1|site_code, data=data.anpp.summary, method = "ML", na.action = na.exclude, correlation = corAR1())

#stepAIC(lmFull, scope = list(upper = mod.4,
#                            lower = ~1),
#        trace = F)

lmNull <- lme(anpp_response~1, random = ~ 1 |site_code, data = data.anpp.summary, method = "ML",  na.action=na.exclude, correlation = corAR1())


#Forward model selection
stepAIC(lmNull, scope = list(upper = lmFull,
                             lower = ~1),
        trace = F)

winning.mod <- lme(anpp_response ~ drtsev.1 + drtsev.2 + drtsev.1:drtsev.2, random = ~ 1 |site_code, data = data.anpp.summary, method = "ML",  na.action=na.exclude, correlation = corAR1())
summary(winning.mod)

winning.mod.lmer <- lmer(anpp_response ~ drtsev.1 + drtsev.2 + drtsev.1:drtsev.2 +(1|site_code),data = data.anpp.summary)
summary(winning.mod.lmer)

visreg2d(winning.mod.lmer, "drtsev.1", "drtsev.2", plot.type="gg", col = c("red", "white", "forestgreen"))+
  geom_point(data = data.anpp.summary, aes(x=drtsev.1, y=drtsev.2, color = as.factor(n_treat_years)))+
  xlab("Current year drought severity")+
  ylab("Previous year drought severity")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme_base()  
  

#####Can I remake stitches plots by year and plot drought severity and response by year?
subset(data.anpp.summary,n_treat_years ==1)%>%
dplyr::mutate(site_code = fct_reorder(site_code, desc(anpp_response))) %>%
ggplot(  aes(site_code, anpp_response))+
  geom_pointrange(aes(ymin = anpp_response-anpp_response.error, ymax = anpp_response+anpp_response.error))+
  geom_hline(yintercept = 0,linetype="dashed")+
  ylim(c(-5,3))+
  ylab("anpp_response")+
  xlab("")+
  coord_flip()+
  theme_bw()

subset(data.anpp.summary,n_treat_years ==2)%>%
  dplyr::mutate(site_code = fct_reorder(site_code, desc(anpp_response))) %>%
  ggplot(  aes(site_code, anpp_response))+
  geom_pointrange(aes(ymin = anpp_response-anpp_response.error, ymax = anpp_response+anpp_response.error))+
  geom_hline(yintercept = 0,linetype="dashed")+
  ylim(c(-5,3))+
  ylab("anpp_response")+
  xlab("")+
  coord_flip()+
  theme_bw()


subset(data.anpp.summary,n_treat_years ==3)%>%
  dplyr::mutate(site_code = fct_reorder(site_code, desc(anpp_response))) %>%
  ggplot(  aes(site_code, anpp_response))+
  geom_pointrange(aes(ymin = anpp_response-anpp_response.error, ymax = anpp_response+anpp_response.error))+
  geom_hline(yintercept = 0,linetype="dashed")+
  ylim(c(-5,3))+
  ylab("anpp_response")+
  xlab("")+
  coord_flip()+
  theme_bw()

subset(data.anpp.summary,n_treat_years ==4)%>%
  dplyr::mutate(site_code = fct_reorder(site_code, desc(anpp_response))) %>%
  ggplot(  aes(site_code, anpp_response))+
  geom_pointrange(aes(ymin = anpp_response-anpp_response.error, ymax = anpp_response+anpp_response.error))+
  geom_hline(yintercept = 0,linetype="dashed")+
  ylim(c(-5,3))+
  ylab("anpp_response")+
  xlab("")+
  coord_flip()+
  theme_bw()




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
    dplyr::select( -c("year", "anpp_response.error", "drtsev.1", "drtsev.2", 'drtsev.3', "drtsev.4"))%>%
    pivot_wider(names_from = "n_treat_years", values_from = c("anpp_response"))%>%
    dplyr::select(-site_code)%>%
    drop_na()%>%
    cor_pmat()
  
  data.anpp.summary%>%
  dplyr::select( -c("year", "anpp_response.error", "drtsev.1", "drtsev.2", 'drtsev.3', "drtsev.4"))%>%
  pivot_wider(names_from = "n_treat_years", values_from = c("anpp_response"))%>%
  dplyr::select(-site_code)%>%
  drop_na()%>%
  cor()%>%
  ggcorrplot(type = "lower", lab = TRUE, p.mat = p.mat)

 
###Make grand means by year-ecosystem-nominalvsextreme
  
data.anpp2$is.extreme <- ifelse(data.anpp2$ppt.1 < data.anpp2$precip, "yes", "no")

grand.means <- data.anpp2%>%
    ddply(.(site_code, year, drtsev.1,drtsev.2,drtsev.3,drtsev.4, n_treat_years, habitat.type,is.extreme ),
          function(x)data.frame(
            anpp_response = mean(x$anpp_response),
            anpp_response.error = qt(0.975, df=length(x$habitat.type)-1)*sd(x$anpp_response, na.rm = TRUE)/sqrt(length(x$habitat.type)-1)
          ))%>%
    subset(n_treat_years >= 1 & n_treat_years<= 4)%>%
  ddply(.(n_treat_years, habitat.type),#here is were you add is.extreme if you want that
        function(x)data.frame(
          anpp_response = mean(x$anpp_response),
          anpp_response.error = qt(0.975, df=length(x$habitat.type)-1)*sd(x$anpp_response, na.rm = TRUE)/sqrt(length(x$habitat.type)-1)
        ))


ggplot(grand.means,  aes(habitat.type, anpp_response))+
  facet_wrap(~n_treat_years)+
  geom_pointrange(aes(ymin = anpp_response-anpp_response.error, ymax = anpp_response+anpp_response.error))+
  geom_hline(yintercept = 0,linetype="dashed")+
  ylim(c(-1.1,0.4))+
  ylab("anpp_response")+
  xlab("")+
  coord_flip()+
  theme_bw()
  
  
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

mod <- lmer(anpp_response~drtsev.1*n_treat_years+ (1|site_code), data = subset(data.anpp.summary, n_treat_years >=1 & n_treat_years <= 4))
summary(mod)

mod <- lmer(anpp_response~drtsev.1 + (1|site_code), data = subset(data.anpp2,n_treat_years ==1))
summary(mod)
r.squaredGLMM(mod)
mod <- lmer(anpp_response~drtsev.1 + (1|site_code), data = subset(data.anpp2,n_treat_years ==2))
summary(mod)
r.squaredGLMM(mod)
mod <- lmer(anpp_response~drtsev.1 + (1|site_code), data = subset(data.anpp2,n_treat_years ==3))
summary(mod)
r.squaredGLMM(mod)
mod <- lmer(anpp_response~drtsev.1 + (1|site_code), data = subset(data.anpp2,n_treat_years ==4))
summary(mod)
r.squaredGLMM(mod)

temp <- subset(data.anpp2,n_treat_years >=1 & n_treat_years <= 4)
mod <- lmer(anpp_response~drtsev.1 * n_treat_years + (1|site_code), data = temp)
summary(mod)
r.squaredGLMM(mod)
visreg(mod)

###Some numbers on the data
length(unique(subset(data.anpp.summary, n_treat_years == 1)$site_code))
length(unique(subset(data.anpp.summary, n_treat_years == 2)$site_code))
length(unique(subset(data.anpp.summary, n_treat_years == 3)$site_code))
length(unique(subset(data.anpp.summary, n_treat_years == 4)$site_code))





##############################
####Try making lag based on previous year's ANPP response (ecological drought)


data.anpp3 <- data.anpp2[,c("site_code", "year", "block", "plot", "subplot", "n_treat_years", "habitat.type", "anpp_response", "drtsev.1", "drtsev.2", "drtsev.3", "drtsev.4")]

data.anpp.dummy <- data.anpp3[,c("site_code", "year", "block", "plot", "subplot", "anpp_response")]
data.anpp.dummy <- dplyr::rename(data.anpp.dummy, anpp_response_prevyear = anpp_response)

data.anpp.dummy$year <- data.anpp.dummy$year - 1 #force lag year for merging

data.anpp4 <- left_join(data.anpp3, data.anpp.dummy, by = c("site_code", "year", "block", "plot", "subplot"))

anpp.summary.eco <- data.anpp4%>%
  ddply(.(site_code, year, n_treat_years, habitat.type),
        function(x)data.frame(
          anpp_response = mean(x$anpp_response),
          anpp_response_prevyear = mean(x$anpp_response_prevyear),
          drtsev.1 = mean(x$drtsev.1),
          drtsev.2 = mean(x$drtsev.2),
          drtsev.3 = mean(x$drtsev.3),
          drtsev.4 = mean(x$drtsev.4)
        ))


#all models to compare ecological drought
#interactive
temp.df <- subset(anpp.summary.eco, n_treat_years == 2 & 
                    is.na(anpp_response_prevyear) != TRUE)
#mod.1 <- lmer(anpp_response~anpp_response_prevyear + (1|site_code), data=temp.df, REML = FALSE)
#mod.2 <- lmer(anpp_response~drtsev.1 + anpp_response_prevyear + (1|site_code), data=temp.df, REML = FALSE)
#mod.3 <- lmer(anpp_response~drtsev.1 * anpp_response_prevyear + (1|site_code), data=temp.df, REML = FALSE)

mod.1 <- lm(anpp_response~anpp_response_prevyear, data=temp.df)
mod.2 <- lm(anpp_response~drtsev.1 + anpp_response_prevyear, data=temp.df)
mod.3 <- lm(anpp_response~drtsev.1 * anpp_response_prevyear, data=temp.df)
mod.4 <- lm(anpp_response~drtsev.1, data=temp.df)
mod.5 <- lm(anpp_response~drtsev.1 * drtsev.2, data=temp.df)


AIC(mod.1, mod.2, mod.3, mod.4, mod.5)

summary(mod.1)
r.squaredGLMM(mod.1)
summary(mod.2)
r.squaredGLMM(mod.2)
summary(mod.3)
r.squaredGLMM(mod.3)

visreg2d(mod.3, xvar = "drtsev.1", yvar = "anpp_response_prevyear", plot.type ="gg")+
  geom_point(data = temp.df, aes(drtsev.1, anpp_response_prevyear))


ggplot(temp.df, aes(anpp_response_prevyear,anpp_response))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("ANPP response previous year")+
  ylab("ANPP response")+
  theme_base()



#FORWARD MODEL SELECTION
lmFull <- lme(anpp_response~anpp_response_prevyear*drtsev.1 * drtsev.2 * drtsev.3 * drtsev.4, random = ~1|site_code, data=temp.df, method = "ML", na.action = na.exclude, correlation = corAR1())

#stepAIC(lmFull, scope = list(upper = mod.4,
#                            lower = ~1),
#        trace = F)

lmNull <- lme(anpp_response~1, random = ~ 1 |site_code, data = temp.df, method = "ML",  na.action=na.exclude, correlation = corAR1())


#Forward model selection
stepAIC(lmNull, scope = list(upper = lmFull,
                             lower = ~1),
        trace = F)

winning.mod <- lme(anpp_response ~ drtsev.1 + anpp_response_prevyear + drtsev.2 +drtsev.1:anpp_response_prevyear + drtsev.1:drtsev.2, random = ~ 1 |site_code, data = temp.df, method = "ML",  na.action=na.exclude, correlation = corAR1())
summary(winning.mod)







####Do some stuff with the RM index from Bondaruk 2022

anpp.RM <- data.anpp%>%
          ddply(.(site_code, trt, year, n_treat_years, habitat.type),function(x)data.frame(
            mass = mean(x$mass),
            ppt.1 = mean(x$ppt.1),
            #ppt.2 = mean(x$ppt.2),
            #ppt.3 = mean(x$ppt.3),
            #ppt.4 = mean(x$ppt.4),
            MAP = mean(x$precip)
          ))%>%
          left_join(anpp.mean, by = "site_code")%>%
          pivot_wider(names_from = "trt", values_from = c("mass", "ppt.1"))%>%
          subset(n_years >= 4)%>% #this reduces to sites where mean ANPP was calculated over at least 4 years
          subset(n_treat_years >= 1 &n_treat_years <=4)

anpp.RM$RM <- ((anpp.RM$mass_Control-anpp.RM$mass_Drought) / anpp.RM$mean.mass)/((anpp.RM$ppt.1_Control-anpp.RM$ppt.1_Drought) / anpp.RM$MAP)


ggplot(anpp.RM, aes(as.factor(n_treat_years), RM))+
  #geom_beeswarm()+\
  geom_boxplot()+
  geom_point()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  #ylim(-5, 5)+
  theme_base()
  
  
ggplot(anpp.RM, aes(MAP, RM, color = habitat.type))+
  facet_wrap(~n_treat_years)+
  geom_point()+
  ylim(-5,5)+
  theme_base()
  

mod <- lmer(RM~as.factor(n_treat_years)+(1|site_code), data = anpp.RM)
summary(mod)




#####################################
#####Pull slopes and p-values for each site over time

#start with data.anpp2

site_vector <- unique(data.anpp2$site_code)

site.regressions_master <- {}

for(i in 1:length(site_vector)) {
  temp.df <- data.anpp2%>%
          subset( site_code == site_vector[i])%>%
          subset(n_treat_days > 90 & n_treat_days <2000)
          
  new.dat <- data.frame(site_code = unique(temp.df$site_code))
  if(length(unique(temp.df$n_treat_years))>=3){
    #temp.mod <- lmer(anpp_response~n_treat_days + (1|plot), data = temp.df)
    temp.mod <- lm(anpp_response~n_treat_days, data = temp.df)
    summary(temp.mod)
    
    new.dat$slope <- coef(summary(temp.mod))["n_treat_days", "Estimate"]
    new.dat$pval <- coef(summary(temp.mod))["n_treat_days", "Pr(>|t|)"]
  } else {
    new.dat$slope <- NA
    new.dat$pval <- NA
  }

  site.regressions_master <- rbind(site.regressions_master, new.dat )
  rm(temp.df, temp.mod, new.dat)
}

site.regressions_master <- site.regressions_master%>%drop_na()
length(subset(site.regressions_master, pval >0.05)$site_code)
length(subset(site.regressions_master, pval <0.05 & slope > 0)$site_code)
length(subset(site.regressions_master, pval <0.05 & slope < 0)$site_code)


site.regressions_summary <- site.regressions_master%>%
                            ddply(.(),function(x)data.frame(
                              not_significant = length(x)
                            ))



