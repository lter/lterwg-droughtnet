###Code to for how different control-treatment communities are with drought
###cody by Meghan Avolio, Feb 8, 2023
###March 21, 2023. Working to include all communities for the difference analysis.
###May 4th, 2023, updating analyses with updated data. Are looking at change

library(tidyverse)
library(codyn)
library(lme4)
library(lmerTest)
library(vegan)
library(data.table)
library(sjPlot)

#library(plyr)

theme_set(theme_bw(16))

# Set WD
user <- "MTH"
if (user == "MTH"){
  setwd("~/Library/CloudStorage/OneDrive-UCB-O365/UCB/Desktop/lterwg-droughtnet")
} else{setwd("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\data_processed")}

setwd("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\data_processed")

dat<-read.csv("cover_ppt_2023-05-10.csv") %>% 
  mutate(replicate=paste(block, plot, subplot, sep="::"))

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

# # Getting measures of differences -----------------------------------------
# 
# 
# ###looping through site to get at differences
# datall<-dat%>% 
#   mutate(rep=paste(site_code, replicate, sep=";")) %>% 
#   filter(n_treat_years!=0.5&n_treat_years!=-1) %>% 
#   filter(n_treat_years==1)
# 
# sc<-unique(datall$site_code)
# 
# diff_metrics<-data.frame()
# 
# for (i in 1:length(sc)){
#   
#   subset<-datall%>%
#     filter(site_code==sc[i])
#   
#   nyrs<-subset %>% 
#     select(year, n_treat_years) %>% 
#     unique()
#   
#   scode<-unique(subset$site_code)
#   
#   diff_mult<-multivariate_difference(df=subset, time.var="year", species.var = "Taxon", abundance.var = "max_cover", replicate.var="replicate", treatment.var="trt", reference.treatment = "Control") %>% 
#     select(-trt, -trt2)
#   
#   diff_ranks<-RAC_difference(df=subset, time.var="year", species.var = "Taxon", abundance.var = "max_cover", replicate.var="replicate", treatment.var="trt", reference.treatment = "Control") %>% 
#     mutate(site_code=scode) %>% 
#     select(-replicate2, -trt, -trt2) %>% 
#     group_by(site_code, year, replicate) %>% 
#     summarize_all(mean) %>% 
#     select(-replicate) %>% 
#     group_by(site_code, year) %>% 
#     summarize_all(mean)
#   
#   diffmetrics<-diff_ranks %>% 
#     left_join(diff_mult) %>% 
#     left_join(nyrs)
#   
#   
#   diff_metrics<-diff_metrics %>% 
#     bind_rows(diffmetrics)
# }
# 
# 
# #write.csv(diff_metrics, "C:\\Users\\mavolio2\\Dropbox\\IDE_DiffMeasures_Yr1.csv", row.names=F)

# Calculating drought severity -------------------------------------------


#getting drought severity
drt<-dat %>% 
  filter(trt=="Drought") %>% 
  select(site_code, n_treat_years, trt, year, map, ppt.1, ppt.2, ppt.3, ppt.4) %>% 
  unique() %>% 
  mutate(drtseverity=(ppt.1-map)/map) %>% 
  select(-ppt.1, -ppt.2, -ppt.3, -ppt.4)

site_types<-read.csv("community_comp\\Prc_LifeHistory_July2023.csv")

continent<-read.csv("Site_Elev-Disturb.csv") %>% 
  select(site_code, continent)


#write.csv(drt, "C:\\Users\\mavolio2\\Dropbox\\IDE_DroughtSeverity.csv", row.names=F)


# #looking at data
# difflong<-diff_metrics %>% 
#   select(-abs_dispersion_diff, -trt_greater_disp) %>% 
#   pivot_longer(names_to = 'measure', values_to = 'value', richness_diff:composition_diff) %>% 
#   left_join(drt) %>% 
#   left_join(site_types)
# 
# unique(diff_metrics$site_code)
# 
# ggplot(data=difflong, aes(x=drtseverity, y=value))+
#   geom_point()+
#   geom_smooth(method='lm')+
#   facet_grid(measure~n_treat_years, scales="free")
# 
# 
# 
# # Statistical analyses of difference --------------------------------------
# #############doing statistical tests of this
# rich<-difflong %>% 
#   filter(measure=="richness_diff")
# t.test(rich$value, mu=0, alternative = "two.sided")
# #this is sig.
# even<-difflong %>% 
#   filter(measure=="evenness_diff")
# t.test(even$value, mu=0, alternative = "two.sided")
# #this is NOT sig.
# rank<-difflong %>% 
#   filter(measure=="rank_diff")
# t.test(rank$value, mu=0, alternative = "two.sided")
# #this is sig.
# sp<-difflong %>% 
#   filter(measure=="species_diff")
# t.test(sp$value, mu=0, alternative = "two.sided")
# #this is sig.
# 
# comp<-difflong %>% 
#   filter(measure=="composition_diff")
# t.test(comp$value, mu=0, alternative = "two.sided")
# #this is sig.
# 
# rd<-lm(value~drtseverity+map+PctGrass+PctAnnual, data=subset(difflong, measure=="richness_diff"))
# summary(rd)
# 
# ggplot(data=subset(difflong, measure=="richness_diff"), aes(x=map, y=value))+
#   geom_point()+
#   ylab("Richness Diff")+
#   geom_hline(yintercept=0)+
#   geom_smooth(method="lm", color="black")+
#   xlab("MAP")+
#   annotate("text", x=-Inf, y=-Inf, vjust=-1, hjust=-1 ,label="p < 0.01")
# 
# ed<-lm(value~drtseverity+map+PctGrass+PctAnnual, data=subset(difflong, measure=="evenness_diff"))
# summary(ed)
# #pct grass
# 
# ggplot(data=subset(difflong, measure=="evenness_diff"), aes(x=PctGrass, y=value))+
#   geom_point()+
#   ylab("Evenness Diff")+
#   geom_hline(yintercept=0)+
#   geom_smooth(method="lm", color="black")+
#   xlab("% Grass")+
#   annotate("text", x=-Inf, y=-Inf, vjust=-1, hjust=-1 ,label="p < 0.01")
# 
# ra<-lm(value~drtseverity+map+PctGrass+PctAnnual, data=subset(difflong, measure=="rank_diff"))
# summary(ra)
# #not sig
# 
# spd<-lm(value~drtseverity+map+PctGrass+PctAnnual, data=subset(difflong, measure=="species_diff"))
# summary(spd)
# #sig
# 
# 
# cd<-lm(value~drtseverity+map+PctGrass+PctAnnual, data=subset(difflong, measure=="composition_diff"))
# summary(cd)
# 
# ggplot(data=subset(difflong, measure=="composition_diff"), aes(x=PctAnnual, y=value))+
#   geom_point()+
#   geom_smooth(method = "lm", color="black")+
#   ylab("Composition Diff")+
#   xlab("% Annual")
# 
# # Permanova ---------------------------------------------------------------
# 
# site_vec <- unique(datall$site_code)
# 
# permanova_out_master <- {}
# 
# for(i in 1:length(site_vec)) {
#   ide_temp <- datall %>% 
#     filter(site_code==site_vec[i]) %>% 
#     select(site_code, trt, Taxon, max_cover, replicate) %>% 
#     pivot_wider(names_from = Taxon, values_from = max_cover, values_fill = 0)
# 
#   permanova_temp <- adonis2(ide_temp[4:ncol(ide_temp)] ~ trt, data=ide_temp, permutations=99)
#       
#   perm_out_temp <- data.frame(
#         site_code = site_vec[i],
#         perm_Pvalue =  permanova_temp$'Pr(>F)'[1]
#       )
#       
#       permanova_out_master <- rbind(permanova_out_master, perm_out_temp)
#       
#     }
#   
# 


# Getting measures of change ----------------------------------------------
#dropping datasets without pretreatment
drop_no_pretrt<-dat %>% 
  select(site_code, n_treat_years) %>% 
  unique() %>% 
  filter(n_treat_years==0) %>% 
  select(-n_treat_years)

dat2<-dat %>% 
  right_join(drop_no_pretrt) %>% 
  mutate(rep=paste(site_code, replicate, sep=";")) %>% 
  filter(n_treat_years!=0.5&n_treat_years!=-1)

#summarazing the data
drt1yr<-dat2 %>% 
  filter(n_treat_years==1) %>% 
  select(site_code) %>% 
  unique()
drt2yr<-dat2 %>% 
  filter(n_treat_years==2) %>% 
  select(site_code) %>% 
  unique()
drt3yr<-dat2 %>% 
  filter(n_treat_years==3) %>% 
  select(site_code) %>% 
  unique()

###looping through site for changes with pretreatmetn as a reference year

#having problems with cdpt_drt.us and eea.br they only have year 0 data. 
dat3<-dat2 %>% 
  filter(site_code!="cdpt_drt.us"&site_code!="eea.br")

sc<-unique(dat3$site_code)

deltamult<-data.frame()
deltaracs<-data.frame()

for (i in 1:length(sc)){
  
  subset<-dat3%>%
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
  
  change_mult<-multivariate_change(df=subset, time.var="year", species.var = "Taxon", abundance.var = "max_cover", replicate.var="replicate", treatment.var="trt", reference.time =  as.integer(pretrt)) %>% 
    select(-year) %>% 
    left_join(nyear) %>% 
    rename(year=year2) %>% 
    mutate(site_code=scode)
  
  deltamult<-deltamult %>% 
    bind_rows(change_mult)
  
  change_ranks<-RAC_change(df=subset, time.var="year", species.var = "Taxon", abundance.var = "max_cover", replicate.var="replicate", reference.time = as.integer(pretrt)) %>% 
    left_join(treats) %>% 
    left_join(nyear) %>% 
    select(-year) %>% 
    rename(year=year2)
  
    deltaracs<-deltaracs %>% 
    bind_rows(change_ranks)
}

###calcluaing bp change
  
bp_yr0<-dat3 %>% 
    group_by(site_code, year, n_treat_years, trt, replicate) %>% 
    summarise(max=max(max_cover), sum=sum(max_cover)) %>%
    mutate(bp=max/sum) %>% 
    ungroup() %>% 
    filter(n_treat_years==0) %>% 
    select(-max, -sum, -n_treat_years, -year) %>% 
    rename(bp0=bp)

bp_change<-dat3 %>% 
    group_by(site_code, year, n_treat_years, trt, replicate) %>% 
    summarise(max=max(max_cover), sum=sum(max_cover)) %>%
    mutate(bp=max/sum) %>% 
    filter(n_treat_years!=0) %>%
    left_join(bp_yr0) %>% 
    mutate(value=bp-bp0) %>% 
    mutate(measure="Dominance") %>% 
  select(-sum, -max,-bp, -bp0)

##looking at data a bit
years<-deltamult %>% 
  select(site_code, year, n_treat_years) %>% 
  unique()

#doing responses. Because all outputs are bound between 0 and 1. We are just doing T-C. negative value means drought had lower values than control. positive values means drought had higher values than control
RRMult<-deltamult %>% 
  pivot_longer(names_to="measure", values_to = "value", composition_change:dispersion_change) %>%
  pivot_wider(names_from = "trt", values_from = "value") %>% 
  mutate(RR=(Drought-Control)) %>% 
  select(-Drought, -Control, n_treat_years)

RRRac<-deltaracs %>% 
  pivot_longer(names_to="measure", values_to = "value", richness_change:losses) %>% 
  bind_rows(bp_change) %>% 
  group_by(site_code, year, n_treat_years, trt, measure) %>% 
  summarise(value=mean(value)) %>% 
  pivot_wider(names_from = "trt", values_from = "value") %>% 
  mutate(RR=(Drought-Control)) %>% 
  select(-Drought, -Control, n_treat_years)

RRall<-RRRac%>% 
  bind_rows(RRMult) %>% 
  left_join(drt) %>% 
  filter(n_treat_years<4)

# chgRacMeans<-chgRaclong%>% 
#   group_by(site_code, year, trt, measure) %>% 
#   summarize(mean=mean(abs(value))) %>% 
#   pivot_wider(names_from="trt", values_from = "mean")

# ChgCntlSD<-chgRaclong %>% 
#   filter(trt=="Control") %>% 
#   group_by(site_code, year, measure) %>% 
#   summarize(cntSD=sd(value))


# #boxplot
# ggplot(data = subset(RRall, measure!="dispersion_change"), aes(x=measure, y=RR))+
#   geom_boxplot(aes(group=measure))+
#   scale_x_discrete(limits=c("richness_change", "evenness_change", "Dominance", 'rank_change', 'gains', 'losses', 'composition_change'), labels=c("Richness", "Evenness", "Dominance", "Ranks", "Gains", 'Losses', "Composition"))+
#   geom_hline(yintercept = 0)+
#   annotate("text", x=1, y=.5, label="*", size=8, color="red")+
#   annotate("text", x=5, y=.6, label="*", size=8, color="red")+
#   annotate("text", x=6, y=.5, label="*", size=8,color="red")+
#   annotate("text", x=7, y=.5, label="*", size=8,color="red")+
#   xlab("Measure of Community Change")+
#   ylab("Drought-Control Difference")

meanCIdiff<-RRall %>% 
  group_by(measure) %>% 
  summarize(mean=mean(RR, na.rm=T), n=length(RR), sd=sd(RR, na.rm=T)) %>% 
  mutate(se=sd/sqrt(n), CI=se*1.96)
  
##Top panel. Mean with CI on the differences between treatment and control changes.
CTdiff_measures<-ggplot(data = subset(meanCIdiff, measure!="dispersion_change"|measure!="Dominance"), aes(x=measure, y=mean))+
  geom_point(size=3)+
  scale_x_discrete(limits=c("richness_change", "evenness_change", 'rank_change', 'gains', 'losses', 'composition_change'), labels=c("Richness", "Evenness", "Ranks", "Gains", 'Losses', "Composition"))+
  geom_errorbar(aes(ymin=mean-CI, ymax=mean+CI), width=0.5)+
  geom_hline(yintercept = 0)+
  annotate("text", x=1, y=-0.03, label="*", size=8, color="red")+
  annotate("text", x=4, y=0.001, label="*", size=8, color="red")+
  annotate("text", x=5, y=.07, label="*", size=8,color="red")+
  annotate("text", x=6, y=.06, label="*", size=8,color="red")+
  xlab("Measure of Community Change")+
  ylab("Control-Treatment Differences")

ggsave("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\Papers\\Community-comp_change\\CTdiff.jpg", plot=CTdiff_measures, units = "in", width=6, height=3.5)

meanCIcomp<-deltamult %>%
  group_by(trt) %>% 
  summarize(mean=mean(composition_change, na.rm=T), n=length(composition_change), sd=sd(composition_change, na.rm=T)) %>% 
  mutate(se=sd/sqrt(n), CI=se*1.96, measure="Composition_change")

MeanCI<-deltaracs %>%
  pivot_longer(richness_change:losses, names_to = "measure", values_to = "value") %>% 
  group_by(trt, measure) %>% 
  summarize(mean=mean(value, na.rm=T), n=length(value), sd=sd(value, na.rm=T)) %>% 
  mutate(se=sd/sqrt(n), CI=se*1.96) %>% 
  bind_rows(meanCIcomp) %>% 
  filter(measure!="dispersion_change"&measure!="Dominance"&measure!=
           "Composition_change") %>% 
  mutate(measure2=factor(measure, levels=c("gains", "losses", 'richness_change', 'evenness_change', 'rank_change'))) %>% 
  left_join(pvalsTRT)


labs=c(gains="Sp. Gains", losses='Sp. Losses', richness_change="Richness Chg.", evenness_change='Evenness Chg.', rank_change= 'Rank Chg.')
#figure of CT differences
CTCompare<-
ggplot(data=MeanCI, aes(x=trt, y=mean, color=trt, label=padj))+
  geom_point(size=4)+
  scale_color_manual(values=c("darkgreen", "darkorange"), name="")+
  geom_errorbar(aes(ymin=mean-CI, ymax=mean+CI, width=0.2))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x =element_blank(), legend.position = c(0.8,0.3))+
  facet_wrap(~measure2, scales='free', labeller = labeller(measure2=labs), ncol=3)+
  ylab("Change from Pre-Treatment")+
  xlab("")+
  geom_text(aes(x=Inf, y=Inf), hjust=1.05, vjust=1.2, color="black")
CTCompare

ggsave("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\Papers\\Community-comp_change\\CTchange.jpg", plot=CTCompare, units = "in", width=6.5, height=5)

# rich<-
# ggplot(data = subset(MeanCI, measure=="richness_change"), aes(x=trt, y=mean, color=trt))+
#   geom_point(size=4)+
#   scale_color_manual(values=c("darkgreen", "darkorange"))+
#   geom_errorbar(aes(ymin=mean-CI, ymax=mean+CI, width=0.2))+
#   geom_hline(yintercept = 0)+
#   scale_y_continuous(limits=c(-1, 1))+
#   ggtitle("Richness Change")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position ='none', axis.title = element_blank())
# 
# ggplot(data = subset(MeanCI, measure=="gains"), aes(x=trt, y=mean, color=trt))+
#   geom_point(size=4)+
#   scale_color_manual(values=c("darkgreen", "darkorange"))+
#   geom_errorbar(aes(ymin=mean-CI, ymax=mean+CI, width=0.2))+
#   scale_y_continuous(limits=c(0, 1))+
#   ggtitle("Species Gains")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position ='none', axis.title = element_blank())
# 
# ggplot(data = subset(MeanCI, measure=="losses"), aes(x=trt, y=mean, color=trt))+
#   geom_point(size=4)+
#   scale_color_manual(values=c("darkgreen", "darkorange"))+
#   geom_errorbar(aes(ymin=mean-CI, ymax=mean+CI, width=0.2))+
#   scale_y_continuous(limits=c(0, 1))+
#   ggtitle("Species Losses")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position ='none', axis.title = element_blank())
# 
# ggplot(data = subset(MeanCI, measure=="richness_change"), aes(x=trt, y=mean, color=trt))+
#   geom_point(size=4)+
#   scale_color_manual(values=c("darkgreen", "darkorange"))+
#   geom_errorbar(aes(ymin=mean-CI, ymax=mean+CI, width=0.2))+
#   geom_hline(yintercept = 0)+
#   scale_y_continuous(limits=c(-1, 1))+
#   ggtitle("Richness Change")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position ='none', axis.title = element_blank())



# doing stats on change ---------------------------------------------------

#1) Are there differences from zero?

RRallstats<-RRall %>% 
  group_by(measure) %>% 
  summarize(pval=t.test(RR, mu=0, alternative="two.sided")$p.value) %>% 
  mutate(padj=p.adjust(pval, method="BH"))


#1A) are C-T rates of change different from one-another and do they differ over time

deltarac3yrs<-deltaracs %>% 
  filter(n_treat_years<4)

mrich<-lmer(richness_change~trt*n_treat_years + (1|site_code/replicate), data=deltarac3yrs)
anova(mrich)

meven<-lmer(evenness_change~trt*n_treat_years + (1|site_code/replicate), data=deltarac3yrs)
anova(meven)

mrank<-lmer(rank_change~trt*n_treat_years + (1|site_code/replicate), data=deltarac3yrs)
anova(mrank)

mgain<-lmer(gains~trt*n_treat_years + (1|site_code/replicate), data=deltarac3yrs)
anova(mgain)

mloss<-lmer(losses~trt*n_treat_years + (1|site_code/replicate), data=deltarac3yrs)
anova(mloss)

ggplot(data=deltarac3yrs, aes(x=n_treat_years, y=rank_change, color=trt))+
  geom_point()+
  geom_smooth(method = "lm")

#adjust P-values for treat effect
pvalsTRT=data.frame(measure=c('richness_change', 'evenness_change', 'rank_change', 'gains', 'losses'), pvalue=c(0.005, 0.732, 0.248, 0.049, 0.009)) %>%
  mutate(padj=paste("p = " , round(p.adjust(pvalue, method="BH"), 3)))

pvalsTRTYR=data.frame(measure=c('rich', 'even', 'rank', 'gain', 'loss'), pvalue=c(0.654, 0.374, 0.022, 0.988, 0.444)) %>% 
  mutate(padj=p.adjust(pvalue, method="BH"))

###looking at changes over time.
##there are no differences among the years at all

mrich<-lmer(RR~as.factor(n_treat_years)+(1|site_code), data=rich)
summary(mrich)
anova(mrich)
#no effect of year
meven<-lmer(RR~as.factor(n_treat_years)+(1|site_code), data=even)
anova(meven)
#no effect of year
mdom<-lmer(RR~as.factor(n_treat_years)+(1|site_code), data=dom)
anova(mdom)
#no effect year

mrank<-lmer(RR~as.factor(n_treat_years)+(1|site_code), data=rank)
anova(mrank)
summary(mrank)

ggplot(data=rank, aes(x=n_treat_years, y=RR))+
  geom_point()+
  geom_smooth(method = "lm", color="black")+
  ylab("Rank Drought\nControl Difference")+
  xlab("Year of Drought")
#sig effect of year - but so minor

mgain<-lmer(RR~as.factor(n_treat_years)+(1|site_code), data=gain)
anova(mgain)
#no effect of year
mloss<-lmer(RR~as.factor(n_treat_years)+(1|site_code), data=loss)
anova(mloss)
#no effect of year
mcomp<-lmer(RR~as.factor(n_treat_years)+(1|site_code), data=comp)
anova(mcomp)
# #no effect of year
# mdisp<-lmer(RR~as.factor(n_treat_years)+(1|site_code), data=disp)
# summary(mdisp)
# #no effect of year




###
RR2<-RRall %>% 
  left_join(site_types) %>% 
  left_join(continent) %>% 
  na.omit() %>% 
  mutate(MAP=(as.numeric(map)/1000),
         PAnn=(as.numeric(PctAnnual)/100),
         PGras=(as.numeric(PctGrass)/100))

str(RR2)

###looking at local and regional drivers

mrich2<-lmer(RR~drtseverity+MAP+PAnn+PGras+(1|site_code), data=subset(RR2, measure=="richness_change"))
summary(mrich2)
anova(mrich2)
#nothing


meven2<-lmer(RR~drtseverity+MAP+PAnn+PGras+(1|site_code), data=subset(RR2, measure=="evenness_change"))
summary(meven2)
anova(meven2)
#no effect of anything

mdom2<-lmer(RR~drtseverity+MAP+PAnn+PGras+(1|site_code), data=subset(RR2, measure=="Dominance"))
summary(mdom2)
anova(mdom2)
#no effect of anything

mrank2<-lmer(RR~drtseverity+MAP+PAnn+PGras+(1|site_code), data=subset(RR2, measure=="rank_change"))
anova(mrank2)
#nothing

mgain2<-lmer(RR~drtseverity+MAP+PAnn+PGras+(1|site_code), data=subset(RR2, measure=="gains"))
summary(mgain2)
anova(mgain2)
#nothing

mloss2<-lmer(RR~drtseverity+MAP+PAnn+PGras+(1|site_code), data=subset(RR2, measure=="losses"))
summary(mloss2)
anova(mloss2)
#sig effect map

ggplot(data=subset(RR2, measure="losses"), aes(x=MAP*1000, y=RR))+
  geom_point()+
  geom_smooth(method="lm", color="black")+
  ylab("Losses Drought\nControl Differences")+
  xlab("MAP")

mcomp2<-lmer(RR~drtseverity+MAP+PAnn+PGras+(1|site_code), data=subset(RR2, measure=="composition_change"))
anova(mcomp2)
#noting
# 
# mdisp<-lmer(RR~drtseverity+map+PctAnnual+PctGrass+(1|site_code), data=subset(RR2, measure=="dispersion_change"))
# summary(mdisp)
# #big effect percent grass

# ggplot(data=subset(RR2, measure=="dispersion_change"), aes(x=PctGrass, y=RR))+
#   geom_point()+
#   geom_smooth(method="lm", color="black")+
#   ylab("Dispersion Response")

###pairs
allwide<-RRall %>% 
  #select(-cntSD) %>% 
   pivot_wider(names_from="measure", values_from = "RR") #%>% 
  #select(-dominance, -dispersion_change, -rank_change)



panel.cor <- function(x, y, cex.cor = 0.8, method = "pearson", ...) {
  options(warn = -1)                   # Turn of warnings (e.g. tied ranks)
  usr <- par("usr"); on.exit(par(usr)) # Saves current "usr" and resets on exit
  par(usr = c(0, 1, 0, 1))             # Set plot size to 1 x 1
  r <- cor(x, y, method = method, use = "pair")               # correlation coef
  p <- cor.test(x, y, method = method)$p.val                  # p-value
  n <- sum(complete.cases(x, y))                              # How many data pairs
  txt <- format(r, digits = 3)                                # Format r-value
  txt1 <- format(p, digits = 3)                                 # Format p-value
  txt2 <- paste0("r= ", txt, '\n', "p= ", txt1, '\n', 'n= ', n) # Make panel text
  text(0.5, 0.5, txt2, cex = cex.cor, ...)                      # Place panel text
  options(warn = 0)                                             # Reset warning
}

pairs(allwide[,7:11], lower.panel = panel.cor, cex.cor=2)

#### Figures -------

### Figure 1: Map of sites + covariate distribution ----
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(paletteer)
library(gridExtra)

# Load world map 
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# Calculate richness in year 0
rich_0 <- dat %>%
  filter(n_treat_years == 0) %>%
  group_by(site_code) %>%
  distinct(Taxon) %>%
  summarise(rich = n())
RR2_rich <- merge(RR2, rich_0, by = "site_code")
RR2_rich <- RR2_rich %>%
  group_by(site_code) %>%
  distinct(trt, .keep_all = TRUE)

latlong<-read.csv("Site_Elev-Disturb.csv")
sites<-drt1yr %>% 
  left_join(latlong)


# Map sites in each continent
map <- ggplot() + 
  geom_sf(data = world, fill = "antiquewhite") + 
  #geom_sf(data = oz_states, colour = "black", fill = NA) + 
  geom_point(data = sites, mapping = aes(x = longitud, y = latitud, fill = continent),
             pch = 21, 
             color = "black", 
             size = 2) + 
  scale_fill_paletteer_d(`"dutchmasters::milkmaid"`) +
  geom_jitter(position = "jitter") +
  theme_bw(base_size = 16) +
  labs(x = "Latitude", y = "Longitude", fill = "IDE Sites (n = 77)") + 
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) + 
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue")) + 
  coord_sf(ylim = c(-80, 80), expand = FALSE)
map

#make for ESA
esamap <- ggplot() + 
  geom_sf(data = world, fill = "antiquewhite") + 
  #geom_sf(data = oz_states, colour = "black", fill = NA) + 
  geom_point(data = sites, mapping = aes(x = longitud, y = latitud, fill=continent),
             pch = 21, 
             color = "black",
             size = 2) + 
  scale_fill_paletteer_d(`"dutchmasters::milkmaid"`) +
  geom_jitter(position = "jitter") +
  theme_bw(base_size = 16) +
  labs(x = "Latitude", y = "Longitude") + 
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) + 
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"), legend.position = "none") + 
  coord_sf(ylim = c(-80, 80), expand = FALSE)


# Make histograms of covariates
hist1 <- ggplot() +
  geom_histogram(data = RR2_rich, binwidth = 100, mapping = aes(x = map), fill = "darkblue", color = "antiquewhite") +
  theme_bw(base_size = 12) +
  ylim(0, 20) +
  labs(x = "MAP (mm)", y = "Site Count")
hist1

hist2 <- ggplot() +
  geom_histogram(data = RR2_rich, binwidth = 5, mapping = aes(x = rich), fill = "darkgreen", color = "antiquewhite") +
  theme_bw(base_size = 12) +
  ylim(0, 20) +
  labs(x = "Species Richness", y = "")
hist2

hist3 <- ggplot() +
  geom_histogram(data = RR2_rich, binwidth = 10, mapping = aes(x = PctGrass), fill = "brown", color = "antiquewhite") +
  theme_bw(base_size = 12) +
  ylim(0, 20) +
  labs(x = "% Grass", y = "")
hist3

# Combine into one figure
Fig1<- grid.arrange(esamap, hist1, hist2, hist3, nrow = 3, 
             layout_matrix = rbind(c(1,1,1), c(1,1,1), c(2,3,4)))

Fig1

ggsave("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\Papers\\Community-comp_change\\MAP.jpeg", plot=Fig1, units="in", width=7, height=4.5)

### Figure 2: Difference in change for drought - control plots ----
# 2A: Difference in change (Drought - Control)
# 2B: Change displayed for Drought and Control separately

# 2A: Differences in change 
Fig2A <- ggplot(data = subset(meanCIdiff, measure!=c("dispersion_change")), 
       aes(x=measure, y=mean))+
  geom_point(size = 2)+
  scale_x_discrete(limits=c("richness_change", "evenness_change", 'gains', 'losses', 'rank_change', 'composition_change'), 
                   labels=c("Richness", "Evenness", "Gains", 'Losses', "Ranks", "Composition"))+
  geom_errorbar(aes(ymin=mean-CI, ymax=mean+CI), width = 0, size = 0.5)+
  geom_hline(yintercept = 0)+
  annotate("text", x=1, y=.065, label="*", size=8, color="red")+
  annotate("text", x=3, y=.065, label="*", size=8, color="red")+
  annotate("text", x=4, y=.065, label="*", size=8,color="red")+
  annotate("text", x=5, y=.065, label="*", size=8,color="red")+
  annotate("text", x=6, y=.065, label="*", size=8,color="red")+
  xlab("Measure of Community Change")+
  ylab("Drought - Control Differences") + theme_bw(base_size = 15) +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank())

Fig2A

# 2B: Broken out into drought and control change

# Combine RACs and composition change 
delta_long <- merge(deltaracs, deltamult, by = c("n_treat_years", "trt", "site_code", "year"))
# Pivot and get summary stats
deltaracs_long <-delta_long %>% 
  pivot_longer(names_to="measure", values_to = "value", richness_change:composition_change) %>% 
  group_by(trt, measure) %>% 
  summarise(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE),
            n = length(value)) %>% 
  mutate(se=sd/sqrt(n), CI=se*1.96)


# Re-order and re-label
new_labs <- as_labeller(
  c(`richness_change` = "Richness",
    `evenness_change` = "Evenness",
    `rank_change` = "Ranks",
    `gains` = "Gains",
    `losses` = "Losses",
    `composition_change` = "Composition")
)

deltaracs_long$measure <- factor(deltaracs_long$measure, # Reordering group factor levels
                                 levels = c("richness_change",
                                            "evenness_change",
                                            "gains",
                                            "losses",
                                            "rank_change",
                                            "composition_change"))

Fig2B <- ggplot(data = subset(deltaracs_long, !is.na(measure)), aes(x=trt, y=mean))+
  scale_color_manual(values=c("mediumseagreen","wheat4")) + 
  facet_wrap(~measure, labeller = new_labs, scales = "free_y",
             nrow = 1) +
  geom_point(aes(color = trt), size = 3)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, color = trt),
                width = 0, size = 1) +
  labs(y = "Change from pre-treatment (mean +/- SE)",
       color = "Treatment") +
  theme_bw(base_size = 15) +
  theme(axis.title.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

Fig2B 

### Supplemental Figure 1: Magnitude of effects over time ----
# Pivot and get summary stats
deltaracs_long_by_year <-delta_long %>% 
  filter(n_treat_years < 4) %>%
  pivot_longer(names_to="measure", values_to = "value", richness_change:composition_change) %>% 
  group_by(n_treat_years, trt, measure) %>% 
  summarise(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE),
            n = length(value)) %>% 
  mutate(se=sd/sqrt(n), CI=se*1.96)
# Re-order
deltaracs_long_by_year$measure <- factor(deltaracs_long_by_year$measure, # Reordering group factor levels
                                 levels = c("richness_change",
                                            "evenness_change",
                                            "gains",
                                            "losses",
                                            "rank_change",
                                            "composition_change"))

SuppFig1 <- ggplot(data = subset(deltaracs_long_by_year, !is.na(measure)), aes(x=as.factor(n_treat_years), y=mean, 
                                                                               color = trt))+
  scale_color_manual(values=c("mediumseagreen","wheat4")) + 
  facet_wrap(~measure, labeller = new_labs, scales = "free_y",
             nrow = 3) +
  geom_point(aes(color = trt), size = 3, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, color = trt),
                width = 0, size = 1, position = position_dodge(width = 0.3)) +
  labs(y = "Change from pre-treatment (mean +/- SE)", x = "Years of treatment",
       color = "Treatment") +
  theme_bw(base_size = 15)

SuppFig1

### Supplemental Figure 2: stitches plots of all sites ----

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
  scale_color_manual(name="IDE Sites (n=77)",
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
  scale_color_manual(name="IDE Sites (n=77)",
                     values=c("mediumseagreen","rosybrown4"))+
  labs(x = "Difference (Drought - Control)", y="",
       linetype = "Significant")+
  theme_bw(base_size = 12) 

SuppFig2


####getting numbers for poster
trt<-dat2 %>% 
  select(rep, trt) %>% 
  unique()

rich<-community_structure(df = dat2, abundance.var ="max_cover", replicate.var = "rep", time.var = "n_treat_years") %>% 
  select(-Evar) %>% 
  filter(n_treat_years==0|n_treat_years==3)

rich2<-pivot_wider(rich, names_from=n_treat_years, names_prefix = "y", values_from = richness) %>% 
  mutate(sp_change=y3-y0) %>% 
  na.omit() %>% 
  left_join(trt) %>% 
  separate(rep, into=c("site_code", "plot"), sep=";") %>% 
  group_by(site_code, trt) %>% 
  summarise(mean=mean(sp_change)) %>% 
  filter(trt!="Control")

mean(rich2$mean)
se<-sd(rich2$mean)/sqrt(55)
