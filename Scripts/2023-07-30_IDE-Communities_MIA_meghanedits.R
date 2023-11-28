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

theme_set(theme_bw(12)) # set theme

################################################################################
# PART 2: load in data
################################################################################
#meghan's path
setwd('C://Users/mavolio2//Dropbox//IDE (1)//data_processed')

# cover data
dat<-read.csv("cover_ppt_2023-10-25.csv") %>% 
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

numpersist/11946
numloss/11946
numgain/11946


###thinking about groupings
#c3/c4 with functional type
group1<-datblip2 %>% 
  group_by(local_lifeform, ps_path) %>% 
  summarise(n=length(pretrt))

#n-fixer lifeform
group2<-datblip2 %>% 
  filter(outcome=='gain') %>% 
  group_by(local_lifeform, N.fixer, trt) %>% 
  summarise(n=length(pretrt))

group3<-datblip2 %>% 
  group_by(local_lifeform, local_lifespan) %>% 
  summarize(n=length(pretrt))


#now trying to to the binomial logistic regression with looking for the effect of pre-treatment abundance, drought treatment and functional trait

####LOSSES

loss_lifeform <- glmer(loss ~ trt*local_lifeform*pretrt + (1|site_code), family = binomial(), data = datblip2)
Anova(loss_lifeform)

plotlifeform<-datblip2 %>% 
  mutate(loss2=ifelse(local_lifeform=="Forb"&loss==0, 0.04, ifelse(local_lifeform=='Grass'&loss==0, 0.02, ifelse(local_lifeform=="Grass"&loss==1, 0.98, ifelse(local_lifeform=="Woody"&loss==1, 0.96, loss)))))

LF.Loss<-
ggplot(data=plotlifeform, aes(x=pretrt, y=loss2, color=local_lifeform, linetype=trt))+
  geom_point()+
  scale_color_manual(name='Lifeform', values=c('darkseagreen2','palegreen4', 'darkgreen'))+
  geom_smooth(aes(y=loss), method = 'glm', method.args=list(family='binomial'), alpha = 0.1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Pre-treatment Abundance")+
  ylab("Loss Probability")+
  guides(linetype=guide_legend(override.aes = list(color='black')))+
  scale_linetype_manual(name='Treatment', values=c(1,4))
LF.Loss

loss_lifespan <- glmer(loss ~ trt*local_lifespan*pretrt + (1|site_code), family = binomial(), data = subset(datblip2, local_lifeform!="Woody"))
Anova(loss_lifespan)

plotlifespan<-datblip2 %>% 
  filter(local_lifeform!='Woody') %>% 
  mutate(loss2=ifelse(local_lifespan=="ANNUAL"&loss==0, 0.02, ifelse(local_lifespan=='Perennial'&loss==1, 0.98, loss)))

LS.loss<-
  ggplot(data=subset(plotlifespan, local_lifeform!='Woody'), aes(x=pretrt, y=loss2, color=local_lifespan, linetype=trt))+
  geom_point()+
  scale_color_manual(name='Lifespan', values=c('skyblue1', 'skyblue4'),labels=c('Annual', 'Perennial'))+
  geom_smooth(aes(y=loss), method = 'glm', method.args=list(family='binomial'), alpha=0.1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Pre-treatment Abundance")+
  ylab("Loss Probability")+
  guides(linetype=guide_legend(override.aes = list(color='black')))+
  scale_linetype_manual(name='Treatment', values=c(1,4))
LS.loss

loss_Nfix <- glmer(loss ~ trt*N.fixer*pretrt + (1|site_code), family = binomial(), data = subset(datblip2, local_lifeform!="Grass"))
Anova(loss_Nfix)

plotnfix<-datblip2 %>% 
  mutate(loss2=ifelse(N.fixer=="Non-N-fixer"&loss==0, 0.02, ifelse(N.fixer=='N-fixer'&loss==1, 0.98, loss)))

NFix.Loss<-
ggplot(data=subset(plotnfix, local_lifeform!='Grass'), aes(x=pretrt, y=loss2, color=N.fixer, linetype=trt))+
  geom_point()+
  scale_color_manual(name='N Fixer', values=c('orange', 'orange4'))+
  geom_smooth(aes(y=loss), method = 'glm', method.args=list(family='binomial'), alpha=0.1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Pre-treatment Abundance")+
  ylab("Loss Probability")+
  guides(linetype=guide_legend(override.aes = list(color='black')))+
  scale_linetype_manual(name='Treatment', values=c(1,4))
NFix.Loss

loss_ps <- glmer(loss ~ trt*ps_path*pretrt + (1|site_code), family = binomial(), data = subset(datblip2, local_lifeform=="Grass"))
Anova(loss_ps)

plotps<-datblip2 %>% 
  mutate(loss2=ifelse(ps_path=="C3"&loss==0, 0.02, ifelse(ps_path=='C4'&loss==1, 0.98, loss)))

ps.Loss<-
  ggplot(data=subset(plotps, local_lifeform=="Grass"), aes(x=pretrt, y=loss2, color=ps_path, linetype=trt))+
  geom_point()+
  scale_color_manual(name='PS Pathway', values=c('darkorchid1', 'darkorchid4'))+
  geom_smooth(aes(y=loss), method = 'glm', method.args=list(family='binomial'), alpha=0.1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Pre-treatment Abundance")+
  ylab("Loss Probability")+
  guides(linetype=guide_legend(override.aes = list(color='black')))+
  scale_linetype_manual(name='Treatment', values=c(1,4))
ps.Loss

###put all figs together
lossfigs<-grid.arrange(LF.Loss, LS.loss, NFix.Loss, ps.Loss, ncol=2)


ggsave("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\Papers\\Community-comp_change\\lossfig.jpg", plot=lossfigs, units="in", width=9, height=8)


# #####PERSIST
# 
# persist_lifeform <- glmer(persist ~ trt*local_lifeform*pretrt + (1|site_code), family = binomial(), data = datblip2)
# Anova(persist_lifeform)
# 
# plotlifeformP<-datblip2 %>% 
#   mutate(persist2=ifelse(local_lifeform=="Forb"&persist==0, 0.04, ifelse(local_lifeform=='Grass'&persist==0, 0.02, ifelse(local_lifeform=="Grass"&persist==1, 0.98, ifelse(local_lifeform=="Woody"&persist==1, 0.96, persist)))))
# 
# LF.Persist<-
#   ggplot(data=plotlifeformP, aes(x=pretrt, y=persist2, color=local_lifeform, linetype=trt))+
#   geom_point()+
#   scale_color_manual(name='Lifeform', values=c('deeppink', 'forestgreen', 'brown'))+
#   geom_smooth(aes(y=persist), method = 'glm', method.args=list(family='binomial'), alpha=0.1)+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#   xlab("Pre-treatment Abundance")+
#   ylab("Persist Probability")+
#   guides(linetype=guide_legend(override.aes = list(color='black')))+
#   scale_linetype_manual(name='Treatment', values=c(1,4))
# 
# persist_lifespan <- glmer(persist ~ trt*local_lifespan*pretrt + (1|site_code), family = binomial(), data = subset(datblip2, local_lifeform!="Woody"))
# Anova(persist_lifespan)
# 
# plotlifespanP<-datblip2 %>% 
#   mutate(persist2=ifelse(local_lifespan=="ANNUAL"&persist==0, 0.02, ifelse(local_lifespan=="Perennial"&persist==1, 0.98, persist)))
# 
# LS.Persist<-
#   ggplot(data=subset(plotlifespanP, local_lifeform!='Woody'), aes(x=pretrt, y=persist2, color=local_lifespan, linetype=trt))+
#   geom_point()+
#   scale_color_manual(name='Lifespan', values=c('purple', 'forestgreen'),labels=c('Annual', 'Perennial'))+
#   geom_smooth(aes(y=persist), method = 'glm', method.args=list(family='binomial'), alpha=0.1)+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#   xlab("Pre-treatment Abundance")+
#   ylab("Persist Probability")+
#   guides(linetype=guide_legend(override.aes = list(color='black')))+
#   scale_linetype_manual(name='Treatment', values=c(1,4))
# LS.Persist
# 
# persist_Nfix <- glmer(persist ~ trt*N.fixer*pretrt + (1|site_code), family = binomial(), data = subset(datblip2, local_lifeform!="Grass"))
# summary(loss_Nfix)
# Anova(persist_Nfix)
# 
# plotnfixP<-datblip2 %>% 
#   mutate(persist2=ifelse(N.fixer=="Non-N-fixer"&persist==0, 0.02, ifelse(N.fixer=='N-fixer'&persist==1, 0.98, persist)))
# 
# NFix.Persist<-
#   ggplot(data=subset(plotnfixP, local_lifeform!='Grass'), aes(x=pretrt, y=persist2, color=N.fixer, linetype=trt))+
#   geom_point()+
#   scale_color_manual(name='N Fixer', values=c('orange', 'forestgreen'))+
#   geom_smooth(aes(y=persist), method = 'glm', method.args=list(family='binomial'), alpha=0.1)+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#   xlab("Pre-treatment Abundance")+
#   ylab("Persist Probability")+
#   guides(linetype=guide_legend(override.aes = list(color='black')))+
#   scale_linetype_manual(name='Treatment', values=c(1,4))
# 
# persist_ps <- glmer(persist ~ trt*ps_path*pretrt + (1|site_code), family = binomial(), data = filter(datblip2, local_lifeform=='Grass'))
# summary(loss_ps)
# Anova(persist_ps)
# 
# plotpsP<-datblip2 %>% 
#   mutate(persist2=ifelse(ps_path=="C3"&persist==0, 0.02, ifelse(ps_path=='C4'&persist==1, 0.98, persist)))
# 
# ps.Persist<-
#   ggplot(data=subset(plotpsP, local_lifeform=='Grass'), aes(x=pretrt, y=persist2, color=ps_path, linetype=trt))+
#   geom_point()+
#   scale_color_manual(name='PS Pathway', values=c('limegreen', 'darkgreen'))+
#   geom_smooth(aes(y=persist), method = 'glm', method.args=list(family='binomial'), alpha=0.1)+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#   xlab("Pre-treatment Abundance")+
#   ylab("Persist Probability")+
#   guides(linetype=guide_legend(override.aes = list(color='black')))+
#   scale_linetype_manual(name='Treatment', values=c(1,4))
# 
# grid.arrange(LF.Persist, LS.Persist, NFix.Persist, ps.Persist, ncol=2)
# 
# 
# 
# 
# #####Gains
# 
# gain_lifeform <- glmer(gain ~ trt*local_lifeform + (1|site_code), family = binomial(), data = datblip2)
# Anova(gain_lifeform)
# 
# lf<- data.frame(summary(emmeans(gain_lifeform, pairwise ~ trt*local_lifeform, type = 'response')$emmeans)) %>% 
#   mutate(trait = "Lifeform")%>% 
#   rename(TraitCat=local_lifeform)
# 
# gain_lifespan <- glmer(gain ~ trt*local_lifespan + (1|site_code), family = binomial(), data = subset(datblip2, local_lifeform!='Woody'))
# summary(gain_lifespan)
# Anova(gain_lifespan)
# 
# ls<- data.frame(summary(emmeans(gain_lifespan, pairwise ~ trt*local_lifespan, type = 'response')$emmeans)) %>% 
#   mutate(trait = "Lifespan")%>% 
#   rename(TraitCat=local_lifespan) %>% 
#   mutate(TraitCat=ifelse(TraitCat=="ANNUAL", "Annual", 'Perennial'))
# 
# gain_Nfix <- glmer(gain ~ trt*N.fixer + (1|site_code), family = binomial(), data = subset(datblip2, local_lifeform!='Grass'))
# summary(gain_Nfix)
# Anova(gain_Nfix)
# 
# gain_Nfix <- glmer(gain ~ trt*N.fixer + (1|site_code), family = binomial(), data = datblip2)
# summary(gain_Nfix)
# Anova(gain_Nfix)
# 
# nfix<- data.frame(summary(emmeans(gain_Nfix, pairwise ~ trt*N.fixer, type = 'response')$emmeans)) %>% 
#   mutate(trait = "N_Fixer")%>% 
#   rename(TraitCat=N.fixer)
# 
# gain_ps <- glmer(gain ~ trt*ps_path + (1|site_code), family = binomial(), data = subset(datblip2, local_lifeform=='Grass'))
# summary(gain_ps)
# Anova(gain_ps)
# 
# ps<- data.frame(summary(emmeans(gain_ps, pairwise ~ trt*ps_path, type = 'response')$emmeans)) %>% 
#   mutate(trait = "Photo_Path")%>% 
#   rename(TraitCat=ps_path)
# 
# 
# #plotting gains
# pvals<-data.frame(trait=c('Lifeform', 'Lifespan', 'N_Fixer', 'Photo_Path'), pval=c(0.23097, 0.04163, 0.787599, 0.811343)) %>% 
#   mutate(padj=paste("p = ", p.adjust(pval, method="BH")))
# 
# toplot<-lf %>% 
#   bind_rows(ls, nfix, ps) #%>%
#   #left_join(pvals)
# 
# labs=c(Lifeform="Growth Form", Lifespan='Lifespan', N_Fixer="N Fixation", Photo_Path='Photosynthetic Pathway')
# 
# gainfig<-
# ggplot(data=toplot, aes(x=TraitCat, y=prob, color=trt))+
#   geom_point(size=3, position = position_dodge(width = 0.4))+
#   scale_color_manual(values=c("darkgreen", "darkorange"), name="")+
#   geom_errorbar(aes(ymin=prob-SE, ymax=prob+SE), width=0.2, position = position_dodge(width = 0.4))+
#   #geom_text(aes(x=Inf, y=Inf), hjust=1.05, vjust=1.2, color="black")+
#   facet_wrap(~trait, scales = 'free', labeller = labeller(trait=labs))+
#   ylab("Probability of Gains")+
#   xlab("Trait")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "top")
# gainfig
# #ggsave("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\Papers\\Community-comp_change\\probloss.jpg", plot=loss, units = "in", width=6.5, height=5)
# 
# 
# ####losses without pretreatment data
# 
# ls_lifeform <- glmer(loss ~ local_lifeform*trt + (1|site_code), family = binomial(), data = datblip2)
# Anova(ls_lifeform)
# 
# l_lf<- data.frame(summary(emmeans(ls_lifeform, pairwise ~ trt*local_lifeform, type = 'response')$emmeans)) %>% 
#   mutate(trait = "Lifeform")%>% 
#   rename(TraitCat=local_lifeform)
# 
# ls_lifespan <- glmer(loss ~ local_lifespan*trt + (1|site_code), family = binomial(), data = datblip2)
# Anova(ls_lifespan)
# 
# l_ls<- data.frame(summary(emmeans(ls_lifespan, pairwise ~ trt*local_lifespan, type = 'response')$emmeans)) %>% 
#   mutate(trait = "Lifespan")%>% 
#   rename(TraitCat=local_lifespan) %>% 
#   mutate(TraitCat=ifelse(TraitCat=="ANNUAL", "Annual", 'Perennial'))
# 
# ls_Nfix <- glmer(loss ~ trt*N.fixer + (1|site_code), family = binomial(), data = datblip2)
# Anova(ls_Nfix)
# 
# l_nfix<- data.frame(summary(emmeans(ls_Nfix, pairwise ~ trt*N.fixer, type = 'response')$emmeans)) %>% 
#   mutate(trait = "N_Fixer")%>% 
#   rename(TraitCat=N.fixer)
# 
# ls_ps <- glmer(loss ~ trt*ps_path + (1|site_code), family = binomial(), data = datblip2)
# Anova(ls_ps)
# 
# l_ps<- data.frame(summary(emmeans(ls_ps, pairwise ~ trt*ps_path, type = 'response')$emmeans)) %>% 
#   mutate(trait = "Photo_Path")%>% 
#   rename(TraitCat=ps_path)
# 
# 
# #plotting losses
# l_pvals<-data.frame(trait=c('Lifeform', 'Lifespan', 'N_Fixer', 'Photo_Path'), pval=c(0.8337, 0.0003422, 0.8611, 0.5033)) %>% 
#   mutate(padj=paste("p = ", p.adjust(pval, method="BH")))
# 
# ls.toplot<-l_lf %>% 
#   bind_rows(l_ls, l_nfix, l_ps) %>%
#   left_join(l_pvals)
# 
# labs=c(Lifeform="Growth Form", Lifespan='Lifespan', N_Fixer="N Fixation", Photo_Path='Photosynthetic Pathway')
# 
# lossfig_nopretrt<-
#   ggplot(data=ls.toplot, aes(x=TraitCat, y=prob, color=trt, label=padj))+
#   geom_point(size=3, position = position_dodge(width = 0.4))+
#   scale_color_manual(values=c("darkgreen", "darkorange"), name="")+
#   geom_errorbar(aes(ymin=prob-SE, ymax=prob+SE), width=0.2, position = position_dodge(width = 0.4))+
#   geom_text(aes(x=Inf, y=Inf), hjust=1.05, vjust=1.2, color="black")+
#   facet_wrap(~trait, scales = 'free', labeller = labeller(trait=labs))+
#   ylab("Probability of Gains")+
#   xlab("Trait")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "top")
# lossfig_nopretrt
