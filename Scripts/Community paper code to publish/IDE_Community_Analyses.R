###Code to for how different control-treatment communities are with drought
###cody by Meghan Avolio, Feb 8, 2023
###March 21, 2023. Working to include all communities for the difference analysis.
###May 4th, 2023, updating analyses with updated data. Are looking at change
###update Oct 11, 2023, updating datasets
#####update Nov 28, 2023 updating dataset and deleting code i'm no longer using\
###Feb 2024 updating to include new dominance analayses
####Dec 2024, new data?

library(tidyverse)
library(codyn)
library(lme4)
library(lmerTest)
library(vegan)
library(data.table)
library(gridExtra)
library(emmeans)
library(car)

theme_set(theme_bw(12))

setwd("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\data_processed")


# reading in and getting data ---------------------------------------------

dat<-read.csv("cover_ppt_2024-12-16.csv") %>% 
  mutate(replicate=paste(block, plot, subplot, sep="_"))

#dropping datasets without pretreatment data
drop_no_pretrt<-dat %>% 
  select(site_code, n_treat_years) %>% 
  unique() %>% 
  filter(n_treat_years==0) %>% 
  select(-n_treat_years)

dat2<-dat %>% 
  right_join(drop_no_pretrt) %>% 
  mutate(rep=paste(site_code, replicate, sep=";")) %>% 
  filter(n_treat_years!=0.5&n_treat_years!=-1)

#having problems with cdpt_drt.us, sherman.pa, eea.br because they only have year 0 data. 
dat3<-dat2 %>% 
  filter(site_code!="sherman.pa"&site_code!="eea.br")

sites<-dat3 %>%
select(site_code) %>%
  unique()
# write.csv(sites, "community_comp\\sitelistMarhc2024.csv")


#summarizing the data
drt1yr<-dat3 %>%
  filter(n_treat_years==1) %>%
  select(site_code) %>%
  unique() %>%
  mutate(p=1)
drt2yr<-dat3 %>%
  filter(n_treat_years==2) %>%
  select(site_code) %>%
  unique() %>%
  mutate(p2=1)
drt3yr<-dat3 %>%
  filter(n_treat_years==3) %>%
  select(site_code) %>%
  unique()
drt4yr<-dat3 %>%
  filter(n_treat_years==4) %>%
  select(site_code) %>%
  unique()

# #five sites are not repeated over years, and only have one year of data.
# oneyr<-dat2 %>%
#   select(site_code, n_treat_years) %>%
#   unique() %>%
#   group_by(site_code) %>%
#   mutate(max=max(n_treat_years)) %>%
#   filter(max==1)


# Calculating drought severity and reading in other site metrics -------------------------------------------

#getting drought severity
drt<-dat3 %>% 
  filter(trt=="Drought") %>% 
  select(site_code, n_treat_years, trt, year, map, ppt.1, ppt.2, ppt.3, ppt.4) %>%   unique() %>% 
  mutate(drtseverity=(ppt.1-map)/map) %>% 
  select(-ppt.1, -ppt.2, -ppt.3, -ppt.4) %>% 
  filter(n_treat_years<5)

site_types<-read.csv("community_comp\\Prc_LifeHistory_Controls_Dec24.csv")

precipcv<-read.csv("climate\\climate_mean_annual_by_site_v3.csv")

#site richness
rich_0_site<-dat3 %>% 
  filter(n_treat_years==0) %>% 
  group_by(site_code, Taxon) %>% 
  summarize(mcov=mean(max_cover))

siterichness<-community_structure(df=rich_0_site, abundance.var = 'mcov', replicate.var = 'site_code') 


# Calculating DCi to identify dominant species ----------------------------

## 1) Need to add in zeros
sc<-unique(dat3$site_code)

datfilled<-data.frame()

for (i in 1:length(sc)){
  
  subset<-dat3%>%
    filter(site_code==sc[i])
  
  addzero<-subset %>% 
    select(site_code, year, n_treat_years, trt, replicate, Taxon, max_cover)%>%
    pivot_wider(names_from = "Taxon", values_from = "max_cover", values_fill=0)
  
  datfill<-addzero %>% 
    pivot_longer(6:ncol(addzero), names_to = "Taxon", values_to = "max_cover")
  
  datfilled<-datfilled%>%
    bind_rows(datfill)
}

###get relative cover of each species
totcov<-datfilled %>%
  ungroup() %>% 
  filter(n_treat_years==0) %>% 
  group_by(site_code, replicate) %>% 
  summarise(totcov=sum(max_cover))

relave<-datfilled%>%
  filter(n_treat_years==0) %>% 
  left_join(totcov) %>% 
  mutate(relcov=max_cover/totcov) %>% 
  group_by(site_code, Taxon) %>% 
  summarize(mean=mean(relcov)) %>% 
  filter(mean!=0)

#to get relative frequency, determine number of control plots
nplots<-datfilled%>%
  filter(n_treat_years==0) %>% 
  select(site_code, replicate)%>%
  unique()%>%
  group_by(site_code)%>%
  summarize(totplots=length(replicate))

#to get relative frequency, determine number of control plots a species is found in, merge in total number of plots and calculate relative frequency. By taking this from all dat there are no options for zero and thus all using n to calcualte length will work.
relfreq<-datfilled%>%
  filter(max_cover!=0, n_treat_years==0) %>% 
  select(site_code, Taxon, replicate)%>%
  unique()%>%
  group_by(site_code, Taxon)%>%
  summarize(nplots=length(replicate))%>%
  left_join(nplots)%>%
  mutate(freq=nplots/totplots)

#calculate DCi
DCi<-relave%>%
  left_join(relfreq)%>%
  mutate(DCi=(mean+freq)/2)%>%
  group_by(site_code) %>% 
  mutate(max=max(DCi)) %>% 
  filter(DCi==max)


# Getting measures of community composition change ----------------------------------------------

###looping through site for changes with pre-treatment as a reference year

sc<-unique(datfilled$site_code)

deltaracs<-data.frame()
deltadom<-data.frame()

for (i in 1:length(sc)){

  subset<-datfilled%>%
    filter(site_code==sc[i])
  
  pretrt<-unique(filter(subset, n_treat_years==0)$year)
  scode<-unique(subset$site_code)
  
  treats<-subset %>% 
    select(replicate, site_code, trt) %>% 
    unique()
  
  nyear<-subset %>% 
    select(year, n_treat_years) %>% 
    unique() %>% 
    rename(year2=year)

  change_ranks<-RAC_change(df=subset, time.var="year", species.var = "Taxon", abundance.var = "max_cover", replicate.var="replicate", reference.time = as.integer(pretrt)) %>% 
    left_join(treats) %>% 
    left_join(nyear) %>% 
    select(-year) %>% 
    rename(year=year2)
  
    deltaracs<-deltaracs %>% 
    bind_rows(change_ranks)
  
  domsp<-filter(DCi, site_code==sc[i])
  domsp<-domsp$Taxon
    
  subset2<-subset %>% 
    filter(Taxon %in% domsp)
  
  dom_abund<-abundance_change(df=subset2, time.var='year', species.var = 'Taxon', abundance.var = 'max_cover', replicate.var = 'replicate', reference.time = as.integer(pretrt)) %>% 
    left_join(treats) %>% 
    left_join(nyear) %>% 
    select(-year) %>% 
    rename(year=year2)
  
  deltadom<-deltadom %>%   
    bind_rows(dom_abund)
  
  
}

# doing stats on change ---------------------------------------------------

#Are C-T rates of change different from one-another and do they differ over time
#Export data to SAS to run models

deltarac4yrs<-deltaracs %>% 
  left_join(deltadom) %>% 
  filter(n_treat_years<5& n_treat_years>0) 

uniquereps<-deltarac4yrs %>%
  select(site_code, replicate) %>%
  unique() %>%
  group_by(site_code) %>%
  mutate(rep=rank(replicate))

# test<-uniquereps %>%
#   group_by(site_code, rep) %>%
#   summarize(n=length(rep))

deltarac4yrs2<-deltarac3yrs %>%
  left_join(uniquereps) %>%
  mutate(rep2=paste(site_code, rep, sep=''))

#write.csv(deltarac3yrs2, "CommunityData_DrtbyTime_forSAS_withdom2_Dec24.csv", row.names=F)

#adjust P-values for treat effect
#these values are from SAS
pvalsTRT=data.frame(measure=c('richness_change', 'evenness_change', 'rank_change', 'gains', 'losses', 'deltaabund'), pvalue=c(0.00001, 0.357,  0.214, 0.011, 0.00001, 0.0066)) %>%
  mutate(padj=paste("p = " , round(p.adjust(pvalue, method="BH"), 3)))

pvalsTRTYR=data.frame(measure=c('rich', 'even', 'rank', 'gain', 'loss', 'abund'), pvalue=c(0.8805, 0.299, 0.0726, 0.834, 0.982, 0.0396)) %>%
  mutate(padj=p.adjust(pvalue, method="BH"))

pvalsYR=data.frame(measure=c('rich', 'even', 'rank', 'gain', 'loss', 'abund'), pvalue=c(0.013, 0.001, 0.001, 0.0001, 0.0001, 0.00001)) %>% 
  mutate(padj=p.adjust(pvalue, method="BH"))

### Figure 1: Magnitude of effects over time ----
#Overall change averaged over all years
MeanCI<-deltarac4yrs %>%
  select(site_code, replicate, year, trt, n_treat_years, richness_change, evenness_change, rank_change, gains, losses, change) %>% 
  pivot_longer(richness_change:change, names_to = "measure", values_to = "value") %>% 
  group_by(trt, measure) %>% 
  summarize(mean=mean(value, na.rm=T), n=length(value), sd=sd(value, na.rm=T)) %>% 
  mutate(se=sd/sqrt(n), CI=se*1.96) %>% 
  mutate(n_treat_years=5)

# Change for each year of the experiment
deltaracs_long_by_year <-deltarac4yrs %>%
  select(site_code, replicate, year, trt, n_treat_years, richness_change, evenness_change, rank_change, gains, losses, change) %>% 
  pivot_longer(richness_change:change, names_to = "measure", values_to = "value") %>% 
  group_by(n_treat_years, trt, measure) %>% 
  summarise(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE),
            n = length(value)) %>% 
  mutate(se=sd/sqrt(n), CI=se*1.96) %>% 
  bind_rows(MeanCI)

deltaracs_long_by_year$measure <- factor(deltaracs_long_by_year$measure,
                                         levels = c("gains",
                                                    "losses",
                                                    "richness_change",
                                                    "evenness_change",
                                                    "rank_change", 
                                                    'change'))
labs=c(gains="Sp. Gains", losses='Sp. Losses', richness_change="Richness Chg.", evenness_change='Evenness Chg.', rank_change= 'Reordering', change="Dom. Abund. Chg.")

Fig2 <- ggplot(data = deltaracs_long_by_year, aes(x=as.factor(n_treat_years), y=mean, color = trt))+
  scale_color_manual(values=c("darkgreen","darkorange")) + 
  facet_wrap(~measure, scales = "free_y", nrow = 2, labeller = labeller(measure=labs)) +
  geom_point(aes(color = trt), size = 3, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, color = trt),
                width = 0, size = 1, position = position_dodge(width = 0.3)) +
  labs(y = "Change from pre-treatment", x = "Years of treatment", color = "Treatment")+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position = "top")+
  geom_vline(xintercept = 4.5)+
  scale_x_discrete(labels=c('1', '2', '3', '4', 'Overall'))

Fig2

#ggsave("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\Papers\\Community-comp_change\\Fig2.jpg", plot=Fig2, units = "in", width=6.5, height=5)


# Control treatment differences statistics for drt severity --------

#doing responses. Because all outputs are bound between 0 and 1. We are just doing T-C. negative value means drought had lower values than control. positive values means drought had higher values than control

#Does the severity of the drought affect the magnitude of drought responsiveness
RRRac<-deltaracs %>% 
  pivot_longer(names_to="measure", values_to = "value", richness_change:losses) %>% 
  group_by(site_code, year, n_treat_years, trt, measure) %>% 
  summarise(value=mean(value)) %>% 
  pivot_wider(names_from = "trt", values_from = "value") %>% 
  mutate(RR=(Drought-Control)) %>% 
  select(-Drought, -Control, n_treat_years)

RRall<-RRRac%>% 
  left_join(drt) %>% 
  filter(n_treat_years<5)

length(unique(RRall$site_code))

###
##dom change by site. 
sitedomchange<-deltadom %>% 
  group_by(site_code) %>% 
  summarise(deltaabund=mean(change, rm.na=T))

RR2<-RRall %>%
  left_join(site_types) %>% 
  #na.omit() %>% 
  left_join(precipcv, by="site_code") %>% 
  left_join(siterichness) %>% 
  left_join(sitedomchange)

length(unique(RR2$site_code))

write.csv(RR2, 'communityData_DrtSeverity_forSAS_withDom_dec24.csv', row.names=F)
str(RR2)

####doing this is SAS now

#####not doing this b/c nothing is significant in SAS so why bother
# pvalsDrtSev=data.frame(measure=c('rich', 'even', 'rank', 'gain', 'loss'), pvalue=c(0.06577, 0.09861, 0.4597, 0.4289, 0.04373)) %>% 
#   mutate(padj=p.adjust(pvalue, method="BH"))


# Multiple regression analysis --------------------------------------------


###looking at regional drivers. take the average change measure for each site averaging overall years
#get control treatment difference and join site descriptor variables

RRRac_average<-deltaracs %>% 
  pivot_longer(names_to="measure", values_to = "value", richness_change:losses) %>% 
  filter(n_treat_years<5& n_treat_years>0) %>% 
  group_by(site_code, year, n_treat_years, trt, measure) %>% 
   summarise(value=mean(value, na.rm=T)) %>% 
  group_by(site_code, trt, measure) %>% 
  summarise(value=mean(value, na.rm=T)) %>% 
  pivot_wider(names_from = "trt", values_from = "value") %>% 
  mutate(RR=(Drought-Control)) %>% 
  left_join(site_types) %>% 
  left_join(precipcv, by="site_code") %>% 
  left_join(sitedomchange) %>% 
  left_join(siterichness) %>% 
  dplyr::select(site_code, measure, RR, PctAnnual, PctGrass, MAP, cv_ppt_inter, deltaabund, richness)

length(unique(RRRac_average$site_code))


#load importance package here to not interfere with other code
#library(relaimpo)
#library(MASS)

#stepwise multiple regression models
mrich2<-stepAIC(lm(RR~MAP+cv_ppt_inter+PctAnnual+PctGrass+richness+deltaabund, data=subset(RRRac_average, measure=="richness_change")))
summary(mrich2)
calc.relimp(mrich2)

meven2<-stepAIC(lm(RR~MAP+cv_ppt_inter+PctAnnual+PctGrass+richness+deltaabund, data=subset(RRRac_average, measure=="evenness_change")))
summary(meven2)

mrank2<-stepAIC(lm(RR~MAP+cv_ppt_inter+PctAnnual+PctGrass+richness+deltaabund, data=subset(RRRac_average, measure=="rank_change")))
summary(mrank2)
calc.relimp(mrank2)

mgain2<-stepAIC(lm(RR~MAP+cv_ppt_inter+PctAnnual+PctGrass+richness+deltaabund, data=subset(RRRac_average, measure=="gains")))
summary(mgain2)


mloss2<-stepAIC(lm(RR~MAP+cv_ppt_inter+PctAnnual+PctGrass+richness+deltaabund, data=subset(RRRac_average, measure=="losses")))
summary(mloss2)
calc.relimp(mloss2)


### Figure 3 of MAP and richness and losses ---------------------------------


toplotlm<-RRRac_average %>% 
  filter(measure %in% c("losses", "richness_change")) %>% 
  rename(DomSpChg=deltaabund) %>%
  pivot_longer(cols=c('MAP', 'DomSpChg'), names_to = 'ind', values_to = "indval") 

plotlm<-ggplot(data=subset(toplotlm, ind=="MAP"), aes(x=indval, y=RR))+
  geom_point()+
  geom_smooth(method="lm", color="black", alpha=0.1)+
  ylab("Drought-Control Differences")+
  xlab("MAP (mm)")+
  geom_hline(yintercept = 0)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_wrap(~measure, scales='free', labeller = labeller(measure=labs))

ggsave("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\Papers\\Community-comp_change\\multipleregresson.jpg", plot=plotlm, units="in", width=3, height=4.5)


# Analysis of trait and abundance based losses ----------------------------

##Dropping rare categories and 
datCat<-dat3%>% # 
  mutate(rep=paste(site_code, replicate, sep=";")) %>% 
  filter(max_cover!=0&Family!="NULL") %>% 
  # standardizing lifeform and lifespan information
  mutate(local_lifeform = ifelse(local_lifeform %in% c("GRAMINOID","Grass", 'GRASS'),"Grass", ifelse(local_lifeform%in% c("VINE","TREE","WOODY","SHRUB","SUBSHRUB"),"Woody",ifelse(local_lifeform %in% c("BRYOPHYTE","MOSS","CLUBMOSS","LICHEN","FUNGI"),"NONVASCULAR", ifelse(local_lifeform %in% c("FORB", "LEGUME"), "Forb", local_lifeform)))),
         local_lifespan = ifelse(local_lifespan %in% c("BIENNIAL","PERENNIAL"), "Perennial",ifelse(local_lifespan == "UNK","NULL",local_lifespan))) %>% # local_lifeform consolidation
  filter(local_lifeform != "FERN") %>%
  filter(local_lifeform != "SUCCULENT") %>%
  filter(local_lifeform != "CACTUS"&local_lifeform!='NONVASCULAR')%>% 
  filter(local_lifeform != "NULL",
         local_lifespan != "NULL",
         local_lifespan!="INDETERMINATE", 
         ps_path != "NULL",
         ps_path!='C3-C4 INTERMEDIATE') %>%
  mutate(N.fixer = ifelse(N.fixer == 0, "Non-N-fixer",
                          ifelse(N.fixer == 1, "N-fixer",NA)))

length(unique(datCat$site_code)) 

#pretreat cover
pretrt_cover<-dat3 %>% 
  filter(n_treat_years==0) %>% 
  select(site_code, trt, block, plot, subplot, Taxon, max_cover) %>% 
  rename(pretrt=max_cover)

###determining loss, gain or persistence, we are only looking at losses.
###I need to do this on a dataset by dataset period because some sites are missing years.
datblip2<-datCat %>% 
  select(site_code, trt, block, plot, subplot, Taxon, local_lifeform, local_lifespan, N.fixer, ps_path, n_treat_years, max_cover) %>% 
  filter(n_treat_years<5& n_treat_years>-1) %>% 
  pivot_wider(names_from = n_treat_years, names_prefix = "y", values_from = max_cover, values_fill = 0) %>% 
  mutate(outcome=ifelse(y0>0 & y1>0 & y2>0 & y3>0& y4>0, "persist", 
                        ifelse(y0>0&y1==0&y2==0&y3==0&y4==0|y0>0&y1>0&y2==0&y3==0&y4==0|y0>0&y1>0&y2>0&y3==0&y4==0, "loss", 
                        ifelse(y0==0&y1>0&y2>0&y3>0|y0==0&y1==0&y2>0&y3>0, "gain","blip")))) %>% 
  mutate(loss=ifelse(outcome=='loss', 1, 0),
         gain=ifelse(outcome=="gain", 1, 0),
         persist=ifelse(outcome=='persist', 1, 0)) %>% 
  left_join(pretrt_cover) %>% 
  filter(!is.na(pretrt))

length(unique(datblip2$site_code))

# ###thinking about groupings
# #c3/c4 with functional type
# group1<-datblip2 %>% 
#   group_by(local_lifeform, N.fixer, ps_path, local_lifespan) %>% 
#   summarise(n=length(pretrt))
# 
# #n-fixer lifeform
# group2<-datblip2 %>% 
#   filter(outcome=='gain') %>% 
#   group_by(local_lifeform, N.fixer, ps_path, local_lifespan) %>% 
#   summarise(n=length(pretrt))
# 
# group3<-datblip2 %>% 
#   group_by(local_lifeform, local_lifespan) %>% 
#   summarize(n=length(pretrt))


#Using binomial logistic regression to look for the effect of pre-treatment abundance, drought treatment and functional trait

####LOSSES
#lifeform
loss_lifeform <- glmer(loss ~ trt*local_lifeform*pretrt + (1|site_code), family = binomial(), data = datblip2)
Anova(loss_lifeform)

plotlifeform<-datblip2 %>% 
  mutate(loss2=ifelse(local_lifeform=="Forb"&loss==0, 0.04, ifelse(local_lifeform=='Grass'&loss==0, 0.02, ifelse(local_lifeform=="Grass"&loss==1, 0.98, ifelse(local_lifeform=="Woody"&loss==1, 0.96, loss)))))

#lifespan
loss_lifespan <- glmer(loss ~ trt*local_lifespan*pretrt + (1|site_code), family = binomial(), data = subset(datblip2, local_lifeform!="Woody"))
Anova(loss_lifespan)

plotlifespan<-datblip2 %>% 
  filter(local_lifeform!='Woody') %>% 
  mutate(loss2=ifelse(local_lifespan=="ANNUAL"&loss==0, 0.02, ifelse(local_lifespan=='Perennial'&loss==1, 0.98, loss)))

#nfixation
loss_Nfix <- glmer(loss ~ trt*N.fixer*pretrt + (1|site_code), family = binomial(), data = subset(datblip2, local_lifeform=="Forb"))
Anova(loss_Nfix)

plotnfix<-datblip2 %>% 
  mutate(loss2=ifelse(N.fixer=="Non-N-fixer"&loss==0, 0.02, ifelse(N.fixer=='N-fixer'&loss==1, 0.98, loss)))

#photosynthetic pathway

loss_ps <- glmer(loss ~ trt*ps_path*pretrt + (1|site_code), family = binomial(), data = subset(datblip2, local_lifeform=="Grass"))
Anova(loss_ps)

plotps<-datblip2 %>% 
  mutate(loss2=ifelse(ps_path=="C3"&loss==0, 0.02, ifelse(ps_path=='C4'&loss==1, 0.98, loss)))

####looking at braod fucntional categories. The model for all categories didn't converge, so I am focusing on annuals only.
datablip3<-datblip2 %>% 
  mutate(cat=ifelse(local_lifeform=='Forb'&local_lifespan=='ANNUAL', 'Ann. Forb',
                    ifelse(local_lifeform=='Forb'&local_lifespan=='Perennial'&N.fixer=='N-fixer', 'N-fix. Forb',
                           ifelse(local_lifeform=='Forb'&local_lifespan=='Perennial'&N.fixer=='Non-N-fixer', 'Non-N-fix. Forb',
                                  ifelse(local_lifeform=='Grass'&local_lifespan=='ANNUAL', 'Ann. Grass',
                                         ifelse(local_lifeform=='Grass'&local_lifespan=='Perennial'&ps_path=='C3', 'C3 Grass',
                                                ifelse(local_lifeform=='Grass'&local_lifespan=='Perennial'&ps_path=='C4', 'C4 Grass',
                                                       ifelse(local_lifeform=='Woody', 'Woody', 'ZZZ'))))))))
# 
# loss_cat <- glmer(loss ~ trt*cat*pretrt + (1|site_code), family = binomial(), data = datablip3)
# summary(loss_cat)
# Anova(loss_cat)

loss_Acat <- glmer(loss ~ trt*cat*pretrt + (1|site_code), family = binomial(), data = subset(datablip3, local_lifespan=='ANNUAL'&local_lifeform!='Woody'))
summary(loss_Acat)
Anova(loss_Acat)

plotcat<-datablip3 %>% 
  mutate(loss2=ifelse(cat=="Ann. Forb"&loss==0, 0.02, ifelse(cat=='Ann. Grass'&loss==1, 0.98, loss)))

# ###checking pvalues
# 
# pvals=data.frame(measure=c('form', 'span', 'N', 'PS', 'AC'), pvalue=c(0.176715, 0.957090, 0.098945, 0.001589, 0.510575)) %>% 
#   mutate(padj=p.adjust(pvalue, method="BH"))

###Figure 4 for loss proababilities-------------
LF.Loss<-
  ggplot(data=plotlifeform, aes(x=pretrt, y=loss2, color=local_lifeform, linetype=trt))+
  geom_point()+
  scale_color_manual(name='Lifeform', values=c('darkseagreen2','palegreen4', 'darkgreen'))+
  geom_smooth(aes(y=loss), method = 'glm', method.args=list(family='binomial'), alpha = 0.1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Pre-treatment Abundance")+
  ylab("Loss Probability")+
  #guides(linetype=guide_legend(override.aes = list(color='black')))+
  scale_linetype_manual(name='Treatment', values=c(1,4), guide='none')
LF.Loss

LS.loss<-
  ggplot(data=subset(plotlifespan, local_lifeform!='Woody'), aes(x=pretrt, y=loss2, color=local_lifespan, linetype=trt))+
  geom_point()+
  scale_color_manual(name='Lifespan', values=c('skyblue1', 'skyblue4'),labels=c('Annual', 'Perennial'))+
  geom_smooth(aes(y=loss), method = 'glm', method.args=list(family='binomial'), alpha=0.1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Pre-treatment Abundance")+
  ylab("Loss Probability")+
  #guides(linetype=guide_legend(override.aes = list(color='black')))+
  scale_linetype_manual(name='Treatment', values=c(1,4), guide='none')
LS.loss

# NFix.Loss<-
#   ggplot(data=subset(plotnfix, local_lifeform!='Grass'), aes(x=pretrt, y=loss2, color=N.fixer, linetype=trt))+
#   geom_point()+
#   scale_color_manual(name='N Fixer', values=c('orange', 'orange4'))+
#   geom_smooth(aes(y=loss), method = 'glm', method.args=list(family='binomial'), alpha=0.1)+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#   xlab("Pre-treatment Abundance")+
#   ylab("Loss Probability")+
#   guides(linetype=guide_legend(override.aes = list(color='black')))+
#   scale_linetype_manual(name='Treatment', values=c(1,4))
# NFix.Loss
# 
# ps.Loss<-
#   ggplot(data=subset(plotps, local_lifeform=="Grass"), aes(x=pretrt, y=loss2, color=ps_path, linetype=trt))+
#   geom_point()+
#   scale_color_manual(name='PS Pathway', values=c('darkorchid1', 'darkorchid4'))+
#   geom_smooth(aes(y=loss), method = 'glm', method.args=list(family='binomial'), alpha=0.1)+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#   xlab("Pre-treatment Abundance")+
#   ylab("Loss Probability")+
#   #guides(linetype=guide_legend(override.aes = list(color='black')))+
#   scale_linetype_manual(name='Treatment', values=c(1,4))
# ps.Loss

Acat.loss<-
  ggplot(data=subset(plotcat, local_lifespan=='ANNUAL'&local_lifeform!='Woody'), aes(x=pretrt, y=loss2, color=cat, linetype=trt))+
  geom_point()+
  scale_color_manual(name='Lifeform', values=c('black','darkgray'))+
  geom_smooth(aes(y=loss), method = 'glm', method.args=list(family='binomial'), alpha = 0.1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Pre-treatment Abundance")+
  ylab("Loss Probability")+
  guides(linetype=guide_legend(override.aes = list(color='black')))+
  scale_linetype_manual(name='Treatment', values=c(1,4))
Acat.loss

###put all figs together
lossfigs<-grid.arrange(LF.Loss, LS.loss, Acat.loss, ncol=2)


ggsave("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\Papers\\Community-comp_change\\lossfig.jpg", plot=lossfigs, units="in", width=9, height=10)

# Appendix Figures 2 and 3 ---------------------------
###pairs funtion to make panels
panel.cor <- function(x, y, cex.cor = 0.05, method = "pearson", ...) {
  options(warn = -1)                   # Turn of warnings (e.g. tied ranks)
  usr <- par("usr"); on.exit(par(usr)) # Saves current "usr" and resets on exit
  par(usr = c(0, 1, 0, 1))             # Set plot size to 1 x 1
  r <- cor(x, y, method = method, use = "pair")               # correlation coef
  p <- cor.test(x, y, method = method)$p.val                  # p-value
  n <- sum(complete.cases(x, y))                              # How many data pairs
  txt <- format(r, digits = 3)                                # Format r-value
  txt1 <- format(p, digits = 3)                                 # Format p-value
  txt2 <- paste0("r = ", txt, '\n', "p = ", txt1, '\n', 'n = ', n) # Make panel text
  text(0.5, 0.5, txt2, cex = cex.cor, ...)                      # Place panel text
  options(warn = 0)                                             # Reset warning
}

pairsplot<-RRRac_average %>% 
  rename(PrecipCV=cv_ppt_inter, SiteRichness=richness, DomSpChange=deltaabund) %>% 
  dplyr::select(site_code, MAP, PrecipCV, PctAnnual, PctGrass, SiteRichness, DomSpChange) %>% 
  unique()

pair<-pairs(pairsplot[,2:7], lower.panel = panel.cor, cex.cor=1.5)
pair

comchangeplot<-RRRac_average %>% 
  dplyr::select(site_code, measure, RR, deltaabund) %>% 
  pivot_wider(names_from = measure, values_from = RR, values_fill = NA) %>% 
  rename(DomSpChange=deltaabund, SpGains=gains, SpLosses=losses, RichnessChg=richness_change, EvennessChg=evenness_change, ReOrdering=rank_change) %>% 
  dplyr::select(SpGains, SpLosses, RichnessChg, EvennessChg, ReOrdering, DomSpChange)
pair2<-pairs(comchangeplot[,2:7], lower.panel = panel.cor, cex.cor=1.5)

#this export isn't working. Probably b/c it isn't a ggolot. UGH.
# ggsave("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\Papers\\Community-comp_change\\pairs.jpg", plot=pair, units="in", width=3, height=3)
# 
# ggsave("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\Papers\\Community-comp_change\\pairs_measures.jpg", plot=pair2, units="in", width=3, height=3)

###histogram for talk
hist<-RRRac_average %>% 
  select(site_code, PctAnnual, PctGrass, MAP, cv_ppt_inter, deltaabund, richness) %>% 
  unique() %>% 
  pivot_longer(PctAnnual:richness, names_to = 'attribute', values_to = 'value') %>% 
  mutate(attribute2=factor(attribute, levels=c('PctAnnual', 'PctGrass', 'deltaabund', 'richness', 'MAP', 'cv_ppt_inter')))

labs1=c(cv_ppt_inter="Inter. Precip. Vari.", deltaabund='Dom Sp. Abund. Chg.', MAP='Mean Annual Precip.', richness="Site Richness", PctAnnual='Pct. Annual Cover', PctGrass= 'Pct. Grass Cover')

theme_set(theme_bw(24))
ggplot(dat=hist, aes(x=value, fill=attribute2))+
  geom_histogram(bins=10)+
  scale_fill_manual(values=c( 'darkgreen',  'green3', 'lightgreen', 'gray','cornflowerblue','blue'))+
  facet_wrap(~attribute2, scales='free', labeller = labeller(attribute2=labs1), ncol=6)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'none')+
  xlab("")+
  ylab("")

drtsevhist<-RR2 %>% 
  ungroup() %>% 
  select(site_code, drtseverity) %>% 
  unique()
ggplot(dat=drtsevhist, aes(x=drtseverity))+
  geom_histogram(bins=15)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Drought Severity")+
  ylab("")


# Figure 1: Map of sites + covariate distribution -------------------------
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(paletteer)
# Load world map 
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

latlong<-read.csv("Site_Elev-Disturb.csv")
sites<-drt1yr %>% 
  left_join(latlong)

# Map sites in each continent
map <- ggplot() + 
  geom_sf(data = world, fill = "antiquewhite") + 
  #geom_sf(data = oz_states, colour = "black", fill = NA) + 
  geom_point(data = sites, mapping = aes(x = longitud, y = latitud),
             pch = 21, 
             color = "black", 
             size = 2, 
             fill='darkgray') + 
  scale_fill_paletteer_d(`"dutchmasters::milkmaid"`) +
  geom_jitter(position = "jitter") +
  theme_bw(base_size = 16) +
  labs(x = "Latitude", y = "Longitude") + 
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) + 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "aliceblue")) + 
  coord_sf(ylim = c(-80, 80), expand = FALSE)#+
#ggtitle('IDE Sites (n = 83)')
map

sitemap<-dat3 %>% 
  select(site_code, map) %>% 
  unique()

# Make histograms of covariates
hist1 <- ggplot() +
  geom_histogram(data = sitemap, binwidth = 10, mapping = aes(x = map/10), fill = "darkblue", color = "antiquewhite") +
  theme_bw(base_size = 12) +
  ylim(0, 20) +
  labs(x = "MAP (cm)", y = "Site Count")
hist1

hist2 <- ggplot() +
  geom_histogram(data = siterichness, binwidth = 5, mapping = aes(x = richness), fill = "darkgreen", color = "antiquewhite") +
  theme_bw(base_size = 12) +
  ylim(0, 20) +
  labs(x = "Richness", y = "")
hist2

hist3 <- ggplot() +
  geom_histogram(data = site_types, binwidth = 10, mapping = aes(x = PctAnnual), fill = "brown", color = "antiquewhite") +
  theme_bw(base_size = 12) +
  ylim(0, 20) +
  labs(x = "Annuals (percent)", y = "")
hist3

# Combine into one figure
Fig1<- grid.arrange(map, hist1, hist2, hist3, nrow = 2, 
                    layout_matrix = rbind(c(1,1, 1), c(1,1,1), c(2,3,4)))

Fig1

ggsave("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\Papers\\Community-comp_change\\MAP.jpeg", plot=Fig1, units="in", width=5.5, height=4.5)



# Appendix Figure 1 - stitcher plots ----------------------------------

RRRich<- as.data.table(RRall)[measure == "richness_change", ]
RRLoss <- as.data.table(RRall)[measure == "losses", ]
RRgains <- as.data.table(RRall)[measure == "gains", ]
habitat <- unique(dat[, c(2,31)])

# Separately for losses, gains and richness change

# Losses
stitches_loss <- RRLoss %>%
  group_by(site_code) %>%
  summarise(mean = mean(RR, na.rm = TRUE), sd = sd(RR, na.rm = TRUE),
            n = length(RR)) %>%
  mutate(se=sd/sqrt(n), CI=se*1.96) %>%
  ungroup() %>%
  arrange(mean)
stitches_loss$Sig<-with(stitches_loss, mean + CI <= 0 | mean - CI >= 0)
stitches_loss <- merge(stitches_loss, drt, by = "site_code")
stitches_loss <- merge(stitches_loss, habitat, by = "site_code")
site_order <- reorder(stitches_loss$site_code, -(stitches_loss$mean))
site_list_order <- levels(site_order)

Fig.stitch.loss <-ggplot(stitches_loss, aes(x = mean,y=reorder(site_code, -mean))) +
  geom_vline(xintercept=0,size=0.5,lty=2)+
  geom_segment(aes(x=mean-CI,y=reorder(site_code, -mean),
                   xend=mean+CI,yend=reorder(site_code, -mean), color = habitat.type,
                   linetype = reorder(Sig, mean)),linewidth = 1, size=0.25)+
  geom_point(aes(color = habitat.type), size = 2) +
  scale_color_manual(name="IDE Sites (n=83)",
                     values=c("mediumseagreen","rosybrown4"))+
  labs(x = "Species Losses (Drought - Control)", y="",
       linetype = "Significant")+
  theme_bw(base_size = 12)

Fig.stitch.loss

# Gains
stitches_gains <- RRgains %>%
  group_by(site_code) %>%
  summarise(mean = mean(RR, na.rm = TRUE), sd = sd(RR, na.rm = TRUE),
            n = length(RR)) %>%
  mutate(se=sd/sqrt(n), CI=se*1.96) %>%
  ungroup() %>%
  arrange(mean)
stitches_gains$Sig<-with(stitches_gains, mean + CI <= 0 | mean - CI >= 0)
stitches_gains <- merge(stitches_gains, drt, by = "site_code")
stitches_gains <- merge(stitches_gains, habitat, by = "site_code")

Fig.stitch.gains <-ggplot(stitches_gains, aes(x = mean,y=reorder(site_code, -mean))) +
  geom_vline(xintercept=0,size=0.5,lty=2)+
  geom_segment(aes(x=mean-CI,y=reorder(site_code, -mean),
                   xend=mean+CI,yend=reorder(site_code, -mean), color = habitat.type,
                   linetype = reorder(Sig, mean)),linewidth = 1, size=0.25)+
  geom_point(aes(color = habitat.type), size = 2) +
  scale_color_manual(name="IDE Sites (n=77)",
                     values=c("mediumseagreen","rosybrown4"))+
  labs(x = "Species gainses (Drought - Control)", y="",
       linetype = "Significant")+
  theme_bw(base_size = 12)

Fig.stitch.gains

# Richness
stitches_Rich <- RRRich %>%
  group_by(site_code) %>%
  summarise(mean = mean(RR, na.rm = TRUE), sd = sd(RR, na.rm = TRUE),
            n = length(RR)) %>%
  mutate(se=sd/sqrt(n), CI=se*1.96) %>%
  ungroup() %>%
  arrange(mean)
stitches_Rich$Sig<-with(stitches_Rich, mean + CI <= 0 | mean - CI >= 0)
stitches_Rich <- merge(stitches_Rich, drt, by = "site_code")
stitches_Rich <- merge(stitches_Rich, habitat, by = "site_code")

Fig.stitch.Rich <-ggplot(stitches_Rich, aes(x = mean,y=reorder(site_code, -mean))) +
  geom_vline(xintercept=0,size=0.5,lty=2)+
  geom_segment(aes(x=mean-CI,y=reorder(site_code, -mean),
                   xend=mean+CI,yend=reorder(site_code, -mean), color = habitat.type,
                   linetype = reorder(Sig, mean)),linewidth = 1, size=0.25)+
  geom_point(aes(color = habitat.type), size = 2) +
  scale_color_manual(name="IDE Sites (n=77)",
                     values=c("mediumseagreen","rosybrown4"))+
  labs(x = "Species Riches (Drought - Control)", y="",
       linetype = "Significant")+
  theme_bw(base_size = 12)

Fig.stitch.Rich

# One figure with shared y-axis!

allwide<-RR2 %>%
  pivot_wider(names_from="measure", values_from = "RR") %>%
  select(-dispersion_change)

stitches_Rich$metric <- "Richness Change"
stitches_gains$metric <- "Species Gains"
stitches_loss$metric <- "Species Losses"
stitches_all <- rbind(stitches_Rich, stitches_gains)
stitches_all <- rbind(stitches_all, stitches_loss)
stitches_sites <- merge(stitches_all, stitches_Rich, by = "site_code")
stitches_sites <- stitches_sites[, c(1:16)]
stitches_sites <- stitches_sites %>%
  group_by(site_code) %>%
  summarise(mean_sev = mean(drtseverity.x), across()) %>%
  ungroup() %>%
  group_by(metric.x) %>%
  distinct(site_code, .keep_all = TRUE)

stitches_sites$Sig.x[is.na(stitches_sites$Sig.x)] <- "FALSE"

SuppFig2 <-ggplot(stitches_sites, aes(x = mean.x, y=reorder(site_code, -mean.y))) +
  facet_grid(~metric.x) +
  geom_vline(xintercept=0,size=0.5,lty=2)+
  geom_segment(aes(x=mean.x-CI.x,y=reorder(site_code, -mean.y),
                   xend=mean.x+CI.x,yend=reorder(site_code, -mean.y), color = habitat.type.x,
                   linetype = reorder(Sig.x, mean.x)),linewidth = 1, size=0.25)+
  geom_point(aes(color = habitat.type.x), size = 2) +
  scale_color_manual(name="IDE Sites (n=83)",
                     values=c("mediumseagreen","rosybrown4"))+
  labs(x = "Difference (Drought - Control)", y="",
       linetype = "Significant")+
  theme_bw(base_size = 12)

SuppFig2

##mean changes over time for both treatments
meanchange<-richchange %>% 
  group_by(trt) %>% 
  summarise(m=mean(RC), s=sd(RC), n=length(RC)) %>% 
  mutate(se=s/sqrt(n))

####getting numbers for poster
trt<-dat3 %>% 
  select(rep, trt) %>% 
  unique()

rich<-community_structure(df = dat3, abundance.var ="max_cover", replicate.var = "rep", time.var = "n_treat_years") %>% 
  select(-Evar) 

#figure out why there are NAs here
rich2<-rich %>% 
  separate(rep, into=c("site_code", "plot"), sep=";",remove=F) %>% 
  group_by(site_code) %>% 
  filter(n_treat_years<4) %>% 
  mutate(max=max(n_treat_years)) %>% 
  filter(n_treat_years==0|n_treat_years==max) %>% 
  pivot_wider(names_from=n_treat_years, names_prefix = "y", values_from = richness) %>% 
  mutate(sp_change=ifelse(is.na(y1)&is.na(y2), y3-y0, ifelse(is.na(y1)&is.na(y3), y2-y0, y1-y0))) %>% 
  left_join(trt) %>% 
  group_by(site_code, trt) %>% 
  summarise(mean=mean(sp_change, rm.na=T)) %>% 
  filter(trt!="Control") %>% 
  drop_na()

mean(rich2$mean)
se<-sd(rich2$mean)/sqrt(76)


#stuff for an appendix
# Calculate plot richness in year 0
rich_0 <- dat3 %>%
  filter(n_treat_years == 0) %>% 
  mutate(rep=paste(site_code, replicate, sep='::'))

plotrichness<-community_structure(df=rich_0, abundance.var = 'max_cover', replicate.var = 'rep') %>% 
  separate(rep, into=c('site_code', 'replicate'), sep='::') %>% 
  group_by(site_code) %>% 
  summarize(mrich=mean(richness))
# Table for trajectory analyses -------------------------------------------

#table of trajectory analyes
yr1<-read.csv('C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\data_processed\\community_comp\\trajectory\\March 2024\\pre_vs_year_1_post_trajectory_summary.csv') %>% 
  mutate(yr=1)
yr2<-read.csv('C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\data_processed\\community_comp\\trajectory\\March 2024\\pre_vs_year_2_post_trajectory_summary.csv') %>% 
  mutate(yr=2)
yr3<-read.csv('C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\data_processed\\community_comp\\trajectory\\March 2024\\pre_vs_year_3_post_trajectory_summary.csv') %>% 
  mutate(yr=3)

fortable<-yr1 %>% 
  bind_rows(yr2, yr3) %>% 
  group_by(site) %>% 
  mutate(max=max(yr)) %>% 
  filter(yr==max) %>% 
  ungroup() %>% 
  mutate(padj=p.adjust(P_Value, method="BH")) %>% 
  dplyr::select(site, analysis_period, diff, Z_Score, padj, analysis_period) 

missing<-sites %>% 
  left_join(fortable, join_by('site_code'=="site"))

write.csv(fortable, 'C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\data_processed\\community_comp\\trajectory\\March 2024\\table for paper.csv', row.names = F)



