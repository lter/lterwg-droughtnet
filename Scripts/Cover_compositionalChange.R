###Code to for how different control-treatment communities are with drought
###cody by Meghan Avolio, Feb 8, 2023
###March 21, 2023. Working to include all communities for the difference analysis.
###May 4th, 2023, updating analyses with updated data. Are looking at change

library(tidyverse)
library(codyn)
library(lme4)
library(lmerTest)
library(vegan)
library(sjPlot)
#library(plyr)

theme_set(theme_bw(20))

setwd("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\data_processed")
setwd("E:\\Dropbox\\IDE (1)\\data_processed")

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

#doing responses. Because all outputs are bound between 0 and 1. We are just doing T-C. negative value means drought had lower values than control. postitive values means drought had higher values than control
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
  na.omit() %>% 
  group_by(measure) %>% 
  summarize(mean=mean(RR), n=length(RR), sd=sd(RR)) %>% 
  mutate(se=sd/sqrt(n), CI=se*1.96)
  
##Top panel. Mean with CI on the differences between treatment and control changes.
ggplot(data = subset(meanCIdiff, measure!="dispersion_change"), aes(x=measure, y=mean))+
  geom_point()+
  scale_x_discrete(limits=c("richness_change", "evenness_change", "Dominance", 'rank_change', 'gains', 'losses', 'composition_change'), labels=c("Richness", "Evenness", "Dominance", "Ranks", "Gains", 'Losses', "Composition"))+
  geom_errorbar(aes(ymin=mean-CI, ymax=mean+CI))+
  geom_hline(yintercept = 0)+
  annotate("text", x=1, y=-0.03, label="*", size=8, color="red")+
  annotate("text", x=5, y=0.001, label="*", size=8, color="red")+
  annotate("text", x=6, y=.07, label="*", size=8,color="red")+
  annotate("text", x=7, y=.06, label="*", size=8,color="red")+
  xlab("Measure of Community Change")+
  ylab("Control-Treatment Differences")

meanCIcomp<-deltamult %>%
  na.omit() %>% 
  group_by(trt) %>% 
  summarize(mean=mean(composition_change), n=length(composition_change), sd=sd(composition_change)) %>% 
  mutate(se=sd/sqrt(n), CI=se*1.96, measure="Composition_change")

MeanCI<-deltaracs %>%
  na.omit() %>% 
  pivot_longer(richness_change:losses, names_to = "measure", values_to = "value") %>% 
  group_by(measure, trt) %>% 
  summarize(mean=mean(value), n=length(value), sd=sd(value)) %>% 
  mutate(se=sd/sqrt(n), CI=se*1.96) %>% 
  bind_rows(meanCIcomp)

ggplot(data = subset(meanCI, measure!="dispersion_change"), aes(x=measure, y=mean))+
  geom_point()+
  scale_x_discrete(limits=c("richness_change", "evenness_change", "Dominance", 'rank_change', 'gains', 'losses', 'composition_change'), labels=c("Richness", "Evenness", "Dominance", "Ranks", "Gains", 'Losses', "Composition"))+
  geom_errorbar(aes(ymin=mean-CI, ymax=mean+CI))+
  geom_hline(yintercept = 0)+
  annotate("text", x=1, y=-0.03, label="*", size=8, color="red")+
  annotate("text", x=5, y=0.001, label="*", size=8, color="red")+
  annotate("text", x=6, y=.07, label="*", size=8,color="red")+
  annotate("text", x=7, y=.06, label="*", size=8,color="red")+
  xlab("Measure of Community Change")+
  ylab("Control-Treatment Differences")

# doing stats on change ---------------------------------------------------

#1) Are there differences from zero?

rich<-RRall %>%
  filter(measure=="richness_change")
t.test(rich$RR, mu=0, alternative = "two.sided")
#this is NOT sig.
even<-RRall %>%
  filter(measure=="evenness_change")
t.test(even$RR, mu=0, alternative = "two.sided")
#this is sig.
dom<-RRall %>%
  filter(measure=="Dominance")
t.test(dom$RR, mu=0, alternative = "two.sided")
#this is sig.
rank<-RRall %>%
  filter(measure=="rank_change")
t.test(rank$RR, mu=0, alternative = "two.sided")
#this is NOT sig.
gain<-RRall %>%
  filter(measure=="gains")
t.test(gain$RR, mu=0, alternative = "two.sided")
#this is sig.
loss<-RRall %>%
  filter(measure=="losses")
t.test(loss$RR, mu=0, alternative = "two.sided")
#this is sig.
comp<-RRall %>%
  filter(measure=="composition_change")
t.test(comp$RR, mu=0, alternative = "two.sided")
#this is sig.

#I think i should drop dispersion
# disp<-RRall %>% 
#   filter(measure=="dispersion_change")
# t.test(disp$RR, mu=0, alternative = "two.sided")
# #this is NOT sig.

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
allwide<-GlassD %>% 
  select(-cntSD) %>% 
   pivot_wider(names_from="measure", values_from = "RR") %>% 
  select(-dominance, -dispersion_change, -rank_change)



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
