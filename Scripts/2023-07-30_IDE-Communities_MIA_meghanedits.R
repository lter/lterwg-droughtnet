###Code to for how different control-treatment communities are with drought
###code by Meghan Avolio, Feb 8, 2023
###code by Maggie Anderson, May 03, 2023
###March 21, 2023. Working to include all communities for the difference analysis
###July/October 2023 working on code about trait responsiveness

################################################################################
# PART 1: setup
################################################################################

# clear workspace
rm(list = ls()) 

# detach pesky packages
detach(package:plyr,unload=TRUE)

# Load libraries 
library(tidyverse) # data wrangling
#library(codyn) # diversity indices
library(lme4) # linear models
library(lmerTest)
library(vegan) # diveristy indices
#library(chisq.posthoc.test) # post-hoc test
#library(plotly) # interactive plots
#library(Hmisc) # confidence intervals
#library(ggpubr) # arranging plots
library(emmeans) # extracting stats
library(car)
library(gridExtra)

theme_set(theme_bw(16)) # set theme

################################################################################
# PART 2: load in data
################################################################################
#meghan's path
setwd('C://Users/mavolio2//Dropbox//IDE (1)//data_processed')

# cover data
dat<-read.csv("cover_ppt_2023-10-12.csv") %>% 
  mutate(replicate=paste(block, plot, subplot, sep="::"))

################################################################################
# PART 5: prep data for trait gain/loss analyses
################################################################################
drop_no_pretrt<-dat %>% 
  select(site_code, n_treat_years) %>% 
  unique() %>% 
  filter(n_treat_years==0) %>% 
  select(-n_treat_years)

dat2<-dat %>% 
  right_join(drop_no_pretrt) %>% 
  mutate(rep=paste(site_code, replicate, sep=";")) %>% 
  filter(n_treat_years!=0.5&n_treat_years!=-1&n_treat_years<4&n_treat_years>-1) %>% 
  filter(site_code!="cdpt_drt.us"&site_code!="eea.br")

length(unique(dat2$site_code))

datblip<-dat2%>% # prep dataframe for looping
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

length(unique(datblip$site_code)) 



################################################################################
# PART 6: count species gains and losses
################################################################################
#pretreat cover
pretrt_cover<-dat2 %>% 
  filter(n_treat_years==0) %>% 
  group_by(site_code, trt, block, plot, subplot, Taxon) %>% 
  summarise(pretrt=mean(max_cover))


datblip2<-datblip %>% 
  select(site_code, trt, block, plot, subplot, Taxon, local_lifeform, local_lifespan, N.fixer, ps_path, n_treat_years, max_cover) %>% 
  pivot_wider(names_from = n_treat_years, names_prefix = "y", values_from = max_cover, values_fill = 0) %>% 
  mutate(outcome=ifelse(y0>0 & y1>0 & y2>0 & y3>0, "persist", 
                 ifelse(y0>0&y1==0&y2==0&y3==0|y0>0&y1>0&y2==0&y3==0, "loss", 
                 ifelse(y0==0&y1>0&y2>0&y3>0|y0==0&y1==0&y2>0&y3>0, "gain","blip")))) %>% 
  mutate(loss=ifelse(outcome=='loss', 1, 0),
         gain=ifelse(outcome=="gain", 1, 0),
         persist=ifelse(outcome=='persist', 1, 0)) %>% 
  left_join(pretrt_cover)

length(unique(datblip2$site_code))
numloss<-sum(datblip2$loss)
numgain<-sum(datblip2$gain)
numpersist<-sum(datblip2$persist)

table(datblip2$ps_path)
table(datblip2$N.fixer)
table(datblip2$local_lifespan)
table(datblip2$local_lifeform)


#now trying to to the binomial logistic regression with looking for the effect of pre-treatment abundance, drought treatment and functional trait

####LOSSES

loss_lifeform <- glmer(loss ~ local_lifeform*trt*pretrt + (1|site_code), family = binomial(), data = datblip2)
summary(loss_lifeform)
Anova(loss_lifeform)

plotlifeform<-datblip2 %>% 
  mutate(loss2=ifelse(local_lifeform=="Forb"&loss==0, 0.04, ifelse(local_lifeform=='Grass'&loss==0, 0.02, ifelse(local_lifeform=="Grass"&loss==1, 0.98, ifelse(local_lifeform=="Woody"&loss==1, 0.96, loss)))))

LF.Loss<-
ggplot(data=plotlifeform, aes(x=pretrt, y=loss2, color=local_lifeform, linetype=trt))+
  geom_point()+
  scale_color_manual(name='Lifeform', values=c('deeppink', 'forestgreen', 'brown'))+
  geom_smooth(aes(y=loss), method = 'glm', method.args=list(family='binomial'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Pre-treatment Abundance")+
  ylab("Loss Probability")+
  annotate('text', y=0.75, x=50, label='Trt*FG NS')+
  annotate('text', y=0.68, x=50, label='Trt*A NS')+
  annotate('text', y=0.60, x=50, label='Trt*FG*A p = <0.001')

loss_lifespan <- glmer(loss ~ trt*local_lifespan*pretrt + (1|site_code), family = binomial(), data = datblip2)
summary(loss_lifespan)
Anova(loss_lifespan)

plotlifespan<-datblip2 %>% 
  mutate(loss2=ifelse(local_lifespan=="ANNUAL"&loss==0, 0.02, ifelse(local_lifespan=='Perennial'&loss==1, 0.98, loss)))

LS.loss<-
  ggplot(data=plotlifespan, aes(x=pretrt, y=loss2, color=local_lifespan, linetype=trt))+
  geom_point()+
  scale_color_manual(name='Lifespan', values=c('purple', 'forestgreen'),labels=c('Annual', 'Perennial'))+
  geom_smooth(aes(y=loss), method = 'glm', method.args=list(family='binomial'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Pre-treatment Abundance")+
  ylab("Loss Probability")+
  annotate('text', y=0.75, x=50, label='Trt*FG p = 0.002')+
  annotate('text', y=0.68, x=50, label='Trt*A NS')+
  annotate('text', y=0.60, x=50, label='Trt*FG*A p = 0.002')


loss_Nfix <- glmer(loss ~ trt*N.fixer*pretrt + (1|site_code), family = binomial(), data = datblip2)
summary(loss_Nfix)
Anova(loss_Nfix)

plotnfix<-datblip2 %>% 
  mutate(loss2=ifelse(N.fixer=="Non-N-fixer"&loss==0, 0.02, ifelse(N.fixer=='N-fixer'&loss==1, 0.98, loss)))

NFix.Loss<-
ggplot(data=plotnfix, aes(x=pretrt, y=loss2, color=N.fixer, linetype=trt))+
  geom_point()+
  scale_color_manual(name='N Fixer', values=c('orange', 'forestgreen'))+
  geom_smooth(aes(y=loss), method = 'glm', method.args=list(family='binomial'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Pre-treatment Abundance")+
  ylab("Loss Probability")+
  annotate('text', y=0.75, x=50, label='Trt*FG NS')+
  annotate('text', y=0.68, x=50, label='Trt*A NS')+
  annotate('text', y=0.60, x=50, label='Trt*FG*A NS')


loss_ps <- glmer(loss ~ trt*ps_path*pretrt + (1|site_code), family = binomial(), data = datblip2)
summary(loss_ps)
Anova(loss_ps)

plotps<-datblip2 %>% 
  mutate(loss2=ifelse(ps_path=="C3"&loss==0, 0.02, ifelse(ps_path=='C4'&loss==1, 0.98, loss)))

ps.Loss<-
  ggplot(data=plotps, aes(x=pretrt, y=loss2, color=ps_path, linetype=trt))+
  geom_point()+
  scale_color_manual(name='PS Pathway', values=c('limegreen', 'darkgreen'))+
  geom_smooth(aes(y=loss), method = 'glm', method.args=list(family='binomial'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Pre-treatment Abundance")+
  ylab("Loss Probability")+
  annotate('text', y=0.75, x=50, label='Trt*FG NS')+
  annotate('text', y=0.68, x=50, label='Trt*A NS')+
  annotate('text', y=0.60, x=50, label='Trt*FG*A NS')

###put all figs together
grid.arrange(LF.Loss, LS.loss, NFix.Loss, ps.Loss, ncol=2)


#####PERSIST

persist_lifeform <- glmer(persist ~ local_lifeform*trt*pretrt + (1|site_code), family = binomial(), data = datblip2)
summary(persist_lifeform)
Anova(persist_lifeform)

plotlifeformP<-datblip2 %>% 
  mutate(persist2=ifelse(local_lifeform=="Forb"&persist==0, 0.04, ifelse(local_lifeform=='Grass'&persist==0, 0.02, ifelse(local_lifeform=="Grass"&persist==1, 0.98, ifelse(local_lifeform=="Woody"&persist==1, 0.96, persist)))))

LF.Persist<-
  ggplot(data=plotlifeformP, aes(x=pretrt, y=persist2, color=local_lifeform, linetype=trt))+
  geom_point()+
  scale_color_manual(name='Lifeform', values=c('deeppink', 'forestgreen', 'brown'))+
  geom_smooth(aes(y=persist), method = 'glm', method.args=list(family='binomial'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Pre-treatment Abundance")+
  ylab("Persist Probability")+
  annotate('text', y=0.75, x=20, label='Trt*FG NS')+
  annotate('text', y=0.68, x=20, label='Trt*A NS')+
  annotate('text', y=0.60, x=20, label='Trt*FG*A NS')

persist_lifespan <- glmer(persist ~ local_lifespan*trt*pretrt + (1|site_code), family = binomial(), data = datblip2)
summary(loss_lifeform)
Anova(persist_lifespan)

plotlifespanP<-datblip2 %>% 
  mutate(persist2=ifelse(local_lifespan=="ANNUAL"&persist==0, 0.02, ifelse(local_lifespan=="Perennial"&persist==1, 0.98, persist)))

LS.Persist<-
  ggplot(data=plotlifespanP, aes(x=pretrt, y=persist2, color=local_lifespan, linetype=trt))+
  geom_point()+
  scale_color_manual(name='Lifespan', values=c('purple', 'forestgreen'),labels=c('Annual', 'Perennial'))+
  geom_smooth(aes(y=persist), method = 'glm', method.args=list(family='binomial'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Pre-treatment Abundance")+
  ylab("Persist Probability")+
  annotate('text', y=0.75, x=20, label='Trt*FG p = <0.001')+
  annotate('text', y=0.68, x=20, label='Trt*A p < 0.001')+
  annotate('text', y=0.60, x=20, label='Trt*FG*A NS')

persist_Nfix <- glmer(persist ~ trt*N.fixer*pretrt + (1|site_code), family = binomial(), data = datblip2)
summary(loss_Nfix)
Anova(persist_Nfix)

plotnfixP<-datblip2 %>% 
  mutate(persist2=ifelse(N.fixer=="Non-N-fixer"&persist==0, 0.02, ifelse(N.fixer=='N-fixer'&persist==1, 0.98, persist)))

NFix.Persist<-
  ggplot(data=plotnfixP, aes(x=pretrt, y=persist2, color=N.fixer, linetype=trt))+
  geom_point()+
  scale_color_manual(name='N Fixer', values=c('orange', 'forestgreen'))+
  geom_smooth(aes(y=persist), method = 'glm', method.args=list(family='binomial'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Pre-treatment Abundance")+
  ylab("Persist Probability")+
  annotate('text', y=0.75, x=20, label='Trt*FG NS')+
  annotate('text', y=0.68, x=20, label='Trt*A NS')+
  annotate('text', y=0.60, x=20, label='Trt*FG*A NS')

persist_ps <- glmer(persist ~ trt*ps_path*pretrt + (1|site_code), family = binomial(), data = datblip2)
summary(loss_ps)
Anova(persist_ps)

plotpsP<-datblip2 %>% 
  mutate(persist2=ifelse(ps_path=="C3"&persist==0, 0.02, ifelse(ps_path=='C4'&persist==1, 0.98, persist)))

ps.Persist<-
  ggplot(data=plotpsP, aes(x=pretrt, y=persist2, color=ps_path, linetype=trt))+
  geom_point()+
  scale_color_manual(name='PS Pathway', values=c('limegreen', 'darkgreen'))+
  geom_smooth(aes(y=persist), method = 'glm', method.args=list(family='binomial'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Pre-treatment Abundance")+
  ylab("Persist Probability")+
  annotate('text', y=0.75, x=20, label='Trt*FG NS')+
  annotate('text', y=0.68, x=20, label='Trt*A NS')+
  annotate('text', y=0.60, x=20, label='Trt*FG*A NS')

grid.arrange(LF.Persist, LS.Persist, NFix.Persist, ps.Persist, ncol=2)




#####Gains

gain_lifeform <- glmer(gain ~ local_lifeform*trt + (1|site_code), family = binomial(), data = datblip2)
Anova(gain_lifeform)

lf<- data.frame(summary(emmeans(gain_lifeform, pairwise ~ trt*local_lifeform, type = 'response')$emmeans)) %>% 
  mutate(trait = "Lifeform")%>% 
  rename(TraitCat=local_lifeform)

gain_lifespan <- glmer(gain ~ local_lifespan*trt + (1|site_code), family = binomial(), data = datblip2)
summary(gain_lifespan)
Anova(gain_lifespan)

ls<- data.frame(summary(emmeans(gain_lifespan, pairwise ~ trt*local_lifespan, type = 'response')$emmeans)) %>% 
  mutate(trait = "Lifespan")%>% 
  rename(TraitCat=local_lifespan) %>% 
  mutate(TraitCat=ifelse(TraitCat=="ANNUAL", "Annual", 'Perennial'))

gain_Nfix <- glmer(gain ~ trt*N.fixer + (1|site_code), family = binomial(), data = datblip2)
summary(gain_Nfix)
Anova(gain_Nfix)

nfix<- data.frame(summary(emmeans(gain_Nfix, pairwise ~ trt*N.fixer, type = 'response')$emmeans)) %>% 
  mutate(trait = "N_Fixer")%>% 
  rename(TraitCat=N.fixer)

gain_ps <- glmer(gain ~ trt*ps_path + (1|site_code), family = binomial(), data = datblip2)
summary(gain_ps)
Anova(gain_ps)

ps<- data.frame(summary(emmeans(gain_ps, pairwise ~ trt*ps_path, type = 'response')$emmeans)) %>% 
  mutate(trait = "Photo_Path")%>% 
  rename(TraitCat=ps_path)


#plotting gains
pvals<-data.frame(trait=c('Lifeform', 'Lifespan', 'N_Fixer', 'Photo_Path'), pval=c(0.23097, 0.04163, 0.787599, 0.811343)) %>% 
  mutate(padj=paste("p = ", p.adjust(pval, method="BH")))

toplot<-lf %>% 
  bind_rows(ls, nfix, ps) %>%
  left_join(pvals)

labs=c(Lifeform="Growth Form", Lifespan='Lifespan', N_Fixer="N Fixation", Photo_Path='Photosynthetic Pathway')

gainfig<-
ggplot(data=toplot, aes(x=TraitCat, y=prob, color=trt, label=padj))+
  geom_point(size=3, position = position_dodge(width = 0.4))+
  scale_color_manual(values=c("darkgreen", "darkorange"), name="")+
  geom_errorbar(aes(ymin=prob-SE, ymax=prob+SE), width=0.2, position = position_dodge(width = 0.4))+
  geom_text(aes(x=Inf, y=Inf), hjust=1.05, vjust=1.2, color="black")+
  facet_wrap(~trait, scales = 'free', labeller = labeller(trait=labs))+
  ylab("Probability of Gains")+
  xlab("Trait")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "top")

#ggsave("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\Papers\\Community-comp_change\\probloss.jpg", plot=loss, units = "in", width=6.5, height=5)


lf<- data.frame(summary(emmeans(loss_lifeform, pairwise ~ trt*local_lifeform, type = 'response')$emmeans)) %>% 
  mutate(trait = "Lifeform") %>% 
  rename(TraitCat=local_lifeform)
