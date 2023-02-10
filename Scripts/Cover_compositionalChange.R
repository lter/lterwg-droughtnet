###Code to for how different control-treatment communities are with drought
###cody by Meghan Avolio, Feb 8, 2023

library(tidyverse)
library(codyn)
library(lmer)
library(lmerTest)

theme_set(theme_bw(12))

setwd("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\data_processed")

dat<-read.csv("cover_ppt_2023-02-06.csv") %>% 
  mutate(replicate=paste(block, plot, subplot, sep="::"))

#dropping datasets without pretreatment
drop_no_pretrt<-dat %>% 
  select(site_code, n_treat_years) %>% 
  unique() %>% 
  filter(n_treat_years==0) %>% 
  select(-n_treat_years)

dat2<-dat %>% 
  right_join(drop_no_pretrt) %>% 
  mutate(rep=paste(site_code, replicate, sep=";")) %>% 
  filter(n_treat_years!=0.5)

#dropping sites with less than 5 species in the controls
##just kidding, we are no longer doing this
# dat3<-dat2 %>% 
#   filter(n_treat_years==0)
# 
# rich<-community_structure(df = dat3, abundance.var = "max_cover", replicate.var = 'rep') %>% 
#   separate(rep, into=c("site_code", "replicate", "trt"), sep=";")
# 
# ggplot(data=rich, aes(x=trt, y=richness))+
#   geom_point()+
#   geom_hline(yintercept = 2)+
#   facet_wrap(~site_code, scales="free")

###looping through site
sc<-unique(dat2$site_code)

diff_metrics<-data.frame()

for (i in 1:length(sc)){
  
  subset<-dat2%>%
    filter(site_code==sc[i])
  
  nyrs<-subset %>% 
    select(year, n_treat_years) %>% 
    unique()
  
  pretrt<-unique(filter(subset, n_treat_years==0)$year)
  scode<-unique(subset$site_code)
  
  diff_mult<-multivariate_difference(df=subset, time.var="year", species.var = "Taxon", abundance.var = "max_cover", replicate.var="replicate", treatment.var="trt", reference.treatment = "Control") %>% 
    select(-trt, -trt2)
  
  diff_ranks<-RAC_difference(df=subset, time.var="year", species.var = "Taxon", abundance.var = "max_cover", replicate.var="replicate", treatment.var="trt", reference.treatment = "Control") %>% 
    mutate(site_code=scode) %>% 
    select(-replicate2, -trt, -trt2) %>% 
    group_by(site_code, year, replicate) %>% 
    summarize_all(mean) %>% 
    select(-replicate) %>% 
    group_by(site_code, year) %>% 
    summarize_all(mean)
  
  diffmetrics<-diff_ranks %>% 
    left_join(diff_mult) %>% 
    mutate(pretrtyear=pretrt) %>% 
    left_join(nyrs)
  
  
  diff_metrics<-diff_metrics %>% 
    bind_rows(diffmetrics)
}


#write.csv(diff_metrics, "C:\\Users\\mavolio2\\Dropbox\\IDE_DiffMeasures.csv", row.names=F)

#getting drought severity
drt<-dat2 %>% 
  filter(trt=="Drought") %>% 
  select(site_code, n_treat_years, trt, year, map, ppt.1, ppt.2, ppt.3, ppt.4) %>% 
  unique() %>% 
  mutate(drtseverity=(ppt.1-map)/map) %>% 
  select(-ppt.1, -ppt.2, -ppt.3, -ppt.4)

#write.csv(drt, "C:\\Users\\mavolio2\\Dropbox\\IDE_DroughtSeverity.csv", row.names=F)


#looking at data
difflong<-diff_metrics %>% 
  select(-abs_dispersion_diff, -trt_greater_disp) %>% 
  pivot_longer(names_to = 'measure', values_to = 'value', richness_diff:composition_diff) %>% 
  filter(n_treat_years==1|n_treat_years==2|n_treat_years==3) %>% 
  left_join(drt)

ggplot(data=difflong, aes(x=drtseverity, y=value))+
  geom_point()+
  geom_smooth(method='lm')+
  facet_grid(measure~n_treat_years, scales="free")


ggplot(data=subset(difflong, n_treat_years==1|n_treat_years==2|n_treat_years==3), aes(x=n_treat_years, y=value, color=drtseverity.1))+
  geom_jitter()+
  geom_smooth(method='lm')+
  facet_wrap(~measure, scales="free")

#doing statistical tests of this
rd<-lmer(value~n_treat_years+(1|site_code), data=subset(difflong, measure=="richness_diff"))
summary(rd)
#not sig
rd<-lmer(value~drtseverity+(1|site_code), data=subset(difflong, measure=="richness_diff"))
summary(rd)

ggplot(data=subset(difflong, measure=="richness_diff"), aes(x=drtseverity, y=value))+
         geom_point()+
  ylab("Richness Diff")+
  geom_hline(yintercept=0)+
  geom_smooth(method="lm", color="black")+
  xlab("Drought Severity")


ed<-lmer(value~n_treat_years+(1|site_code), data=subset(difflong, measure=="evenness_diff"))
summary(ed)
#no diff
ed<-lmer(value~drtseverity+(1|site_code), data=subset(difflong, measure=="evenness_diff"))
summary(ed)
#no diff

ra<-lmer(value~n_treat_years+(1|site_code), data=subset(difflong, measure=="rank_diff"))
summary(rd)
#rank differs by year
ra<-lmer(value~drtseverity+(1|site_code), data=subset(difflong, measure=="rank_diff"))
summary(ra)
#not sig

ggplot(data=subset(difflong, measure=="rank_diff"), aes(x=n_treat_years, y=value))+
  geom_point()+
  ylab("Rank Diff")+
  #geom_hline(yintercept=0)+
  geom_smooth(method="lm", color="black")+
  xlab("Years of Drought")

# spd<-lmer(value~n_treat_years+(1|site_code), data=subset(difflong, measure=="species_diff"))
# summary(spd)
# #not sig
# spd<-lmer(value~drtseverity+(1|site_code), data=subset(difflong, measure=="species_diff"))
# summary(ra)
# #no sig


cd<-lmer(value~n_treat_years+(1|site_code), data=subset(difflong, measure=="composition_diff"))
summary(cd)
#comp differs by year
cd<-lmer(value~drtseverity+(1|site_code), data=subset(difflong, measure=="composition_diff"))
summary(cd)
#comp differs by drt severity
cd1<-lm(value~drtseverity, data=subset(difflong, measure=="composition_diff"&n_treat_years==1))
summary(cd1)
cd2<-lm(value~drtseverity, data=subset(difflong, measure=="composition_diff"&n_treat_years==2))
summary(cd2)
cd3<-lm(value~drtseverity, data=subset(difflong, measure=="composition_diff"&n_treat_years==3))
summary(cd3)#only sig.

ggplot(data=subset(difflong, measure=="composition_diff"), aes(x=drtseverity, y=value))+
  geom_point()+
  geom_smooth(method = "lm", color="black")+
  ylab("Composition Diff")+
  xlab("Drought Severity")

ggplot(data=subset(difflong, measure=="composition_diff"), aes(x=n_treat_years, y=value))+
  geom_point()+
  geom_smooth(method = "lm", color="black")+
  ylab("Composition Diff")+
  xlab("Years of Drought")


###looping through site for changes
sc<-unique(dat2$site_code)

diff_metrics<-data.frame()

for (i in 1:length(sc)){
  
  subset<-dat2%>%
    filter(site_code==sc[i])
  
  pretrt<-unique(filter(subset, n_treat_years==0)$year)
  scode<-unique(subset$site_code)
  
  diff_mult<-multivariate_difference(df=subset, time.var="year", species.var = "Taxon", abundance.var = "max_cover", replicate.var="replicate", treatment.var="trt", reference.treatment = "Control") %>% 
    select(-trt, -trt2)
  
  diff_ranks<-RAC_difference(df=subset, time.var="year", species.var = "Taxon", abundance.var = "max_cover", replicate.var="replicate", treatment.var="trt", reference.treatment = "Control") %>% 
    mutate(site_code=scode) %>% 
    select(-replicate2, -trt, -trt2) %>% 
    group_by(site_code, year, replicate) %>% 
    summarize_all(mean) %>% 
    select(-replicate) %>% 
    group_by(site_code, year) %>% 
    summarize_all(mean)
  
  diffmetrics<-diff_ranks %>% 
    left_join(diff_mult) %>% 
    mutate(pretrtyear=pretrt)
  
  
  diff_metrics<-diff_metrics %>% 
    bind_rows(diffmetrics)
}



