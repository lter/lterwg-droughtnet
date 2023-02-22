###Code to for how different control-treatment communities are with drought
###cody by Meghan Avolio, Feb 8, 2023

library(tidyverse)
library(codyn)
library(lme4)
library(lmerTest)

theme_set(theme_bw(20))

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
  filter(n_treat_years!=0.5&n_treat_years!=-1)

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

# Getting measures of differences -----------------------------------------


###looping through site to get at differences
datall<-dat%>% 
  mutate(rep=paste(site_code, replicate, sep=";")) %>% 
  filter(n_treat_years!=0.5&n_treat_years!=-1)

sc<-unique(datall$site_code)

diff_metrics<-data.frame()

for (i in 1:length(sc)){
  
  subset<-datall%>%
    filter(site_code==sc[i])
  
  nyrs<-subset %>% 
    select(year, n_treat_years) %>% 
    unique()
  
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
    left_join(nyrs)
  
  
  diff_metrics<-diff_metrics %>% 
    bind_rows(diffmetrics)
}


#write.csv(diff_metrics, "C:\\Users\\mavolio2\\Dropbox\\IDE_DiffMeasures.csv", row.names=F)

# Calculating drought severtity -------------------------------------------


#getting drought severity
drt<-dat %>% 
  filter(trt=="Drought") %>% 
  select(site_code, n_treat_years, trt, year, map, ppt.1, ppt.2, ppt.3, ppt.4) %>% 
  unique() %>% 
  mutate(drtseverity=(ppt.1-map)/map) %>% 
  select(-ppt.1, -ppt.2, -ppt.3, -ppt.4)

site_types<-read.csv("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\data_processed\\Prc_LifeHistory_2023-02-09.csv")

continent<-read.csv("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\data_processed\\Site_Elev-Disturb.csv") %>% 
  select(site_code, continent)

subset<-continent %>% 
  right_join(drop_no_pretrt)

#write.csv(drt, "C:\\Users\\mavolio2\\Dropbox\\IDE_DroughtSeverity.csv", row.names=F)


#looking at data
difflong<-diff_metrics %>% 
  select(-abs_dispersion_diff, -trt_greater_disp) %>% 
  pivot_longer(names_to = 'measure', values_to = 'value', richness_diff:composition_diff) %>% 
  filter(n_treat_years==1|n_treat_years==2|n_treat_years==3) %>% 
  left_join(drt) %>% 
  left_join(site_types)

unique(diff_metrics$site_code)

ggplot(data=difflong, aes(x=drtseverity, y=value))+
  geom_point()+
  geom_smooth(method='lm')+
  facet_grid(measure~n_treat_years, scales="free")

ggplot(data=subset(difflong, n_treat_years==1|n_treat_years==2|n_treat_years==3), aes(x=n_treat_years, y=value, color=drtseverity.1))+
  geom_jitter()+
  geom_smooth(method='lm')+
  facet_wrap(~measure, scales="free")



# Statistical analyses of difference --------------------------------------
#############doing statistical tests of this
rich<-difflong %>% 
  filter(measure=="richness_diff")
t.test(rich$value, mu=0, alternative = "two.sided")
#this is sig.
even<-difflong %>% 
  filter(measure=="evenness_diff")
t.test(even$value, mu=0, alternative = "two.sided")
#this is NOT sig.

boxplot(comp$value)

rank<-difflong %>% 
  filter(measure=="rank_diff")
t.test(rank$value, mu=0, alternative = "two.sided")
#this is NOT sig.
sp<-difflong %>% 
  filter(measure=="species_diff")
t.test(sp$value, mu=0, alternative = "two.sided")
#this is sig.

comp<-difflong %>% 
  filter(measure=="composition_diff")
t.test(comp$value, mu=0, alternative = "two.sided")
#this is sig.
disp<-GlassD %>% 
  filter(measure=="dispersion_change")
t.test(disp$RR, mu=0, alternative = "two.sided")
#this is NOT sig.




rd<-lmer(value~as.factor(n_treat_years)+(1|site_code), data=subset(difflong, measure=="richness_diff"))
summary(rd)
#Sig intercept.



rd<-lmer(value~drtseverity+map+PctGrass+PctAnnual+(1|site_code), data=subset(difflong, measure=="richness_diff"))
summary(rd)

ggplot(data=subset(difflong, measure=="richness_diff"), aes(x=drtseverity, y=value))+
         geom_point()+
  ylab("Richness Diff")+
  geom_hline(yintercept=0)+
  geom_smooth(method="lm", color="black")+
  xlab("Drought Severity")+
  annotate("text", x=-Inf, y=-Inf, vjust=-1, hjust=-1 ,label="p = 0.06")
ggplot(data=subset(difflong, measure=="richness_diff"), aes(x=map, y=value))+
  geom_point()+
  ylab("Richness Diff")+
  geom_hline(yintercept=0)+
  geom_smooth(method="lm", color="black")+
  xlab("MAP")+
  annotate("text", x=-Inf, y=-Inf, vjust=-1, hjust=-1 ,label="p = 0.01")
ggplot(data=subset(difflong, measure=="richness_diff"), aes(x=PctAnnual, y=value))+
  geom_point()+
  ylab("Richness Diff")+
  geom_hline(yintercept=0)+
  geom_smooth(method="lm", color="black")+
  annotate("text", x=-Inf, y=-Inf, vjust=-1, hjust=-1 ,label="p = 0.002")


ed<-lmer(value~as.factor(n_treat_years)+(1|site_code), data=subset(difflong, measure=="evenness_diff"))
summary(ed)
#no diff
ed<-lmer(value~drtseverity+map+PctGrass+PctAnnual+(1|site_code), data=subset(difflong, measure=="evenness_diff"))
summary(ed)
#no diff

ra<-lmer(value~as.factor(n_treat_years)+(1|site_code), data=subset(difflong, measure=="rank_diff"))
summary(rd)
#rank differs by year
ra<-lmer(value~drtseverity+map+PctGrass+PctAnnual+(1|site_code), data=subset(difflong, measure=="rank_diff"))
summary(ra)
#not sig

ggplot(data=subset(difflong, measure=="rank_diff"), aes(x=n_treat_years, y=value))+
  geom_point()+
  ylab("Rank Diff")+
  #geom_hline(yintercept=0)+
  geom_smooth(method="lm", color="black")+
  xlab("Years of Drought")

spd<-lmer(value~as.factor(n_treat_years)+(1|site_code), data=subset(difflong, measure=="species_diff"))
summary(spd)

ggplot(data=subset(difflong, measure=="species_diff"), aes(x=n_treat_years, y=value))+
  geom_point()+
  ylab("Species Diff")+
  #geom_hline(yintercept=0)+
  geom_smooth(method="lm", color="black")+
  xlab("Years of Drought")

spd<-lmer(value~drtseverity+map+PctGrass+PctAnnual+(1|site_code), data=subset(difflong, measure=="species_diff"))
summary(spd)
#sig
ggplot(data=subset(difflong, measure=="species_diff"), aes(x=drtseverity, y=value))+
  geom_point()+
  ylab("Species Diff")+
  #geom_hline(yintercept=0)+
  geom_smooth(method="lm", color="black")+
  xlab("Drought Severity")


cd<-lmer(value~n_treat_years+(1|site_code), data=subset(difflong, measure=="composition_diff"))
summary(cd)
#comp differs by year
cd<-lmer(value~drtseverity+map+PctGrass+PctAnnual+(1|site_code), data=subset(difflong, measure=="composition_diff"))
summary(cd)

ggplot(data=subset(difflong, measure=="composition_diff"), aes(x=map, y=value))+
  geom_point()+
  geom_smooth(method = "lm", color="black")+
  ylab("Composition Diff")+
  xlab("MAP")
ggplot(data=subset(difflong, measure=="composition_diff"), aes(x=PctAnnual, y=value))+
  geom_point()+
  geom_smooth(method = "lm", color="black")+
  ylab("Composition Diff")

ggplot(data=subset(difflong, measure=="composition_diff"), aes(x=n_treat_years, y=value))+
  geom_point()+
  geom_smooth(method = "lm", color="black")+
  ylab("Composition Diff")+
  xlab("Years of Drought")


# Getting measures of change ----------------------------------------------


###looping through site for changes with pretreatmetn as a reference year

#having problems with eea.br, dropping this. not sure why
dat3<-dat2 %>% 
  filter(site_code!="eea.br")

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


##looking at data a bit
years<-deltamult %>% 
  select(site_code, year, n_treat_years) %>% 
  unique()

dom<-read.csv("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\data_processed\\BP_index_change.csv") %>% 
  select(-X, -plot) %>% 
  rename(replicate=rep, value=BP_index_change) %>% 
  mutate(measure="dominance") %>% 
  left_join(years)

RRMult<-deltamult %>% 
  pivot_longer(names_to="measure", values_to = "value", composition_change:dispersion_change) %>%
  pivot_wider(names_from = "trt", values_from = "value") %>% 
  mutate(RR=(Drought-Control)/Control) %>% 
  select(-Drought, -Control, -n_treat_years)

chgRaclong<-deltaracs %>% 
  pivot_longer(names_to="measure", values_to = "value", richness_change:losses) %>% 
  bind_rows(dom)

chgRacMeans<-chgRaclong%>% 
  group_by(site_code, year, trt, measure) %>% 
  summarize(mean=mean(abs(value))) %>% 
  pivot_wider(names_from="trt", values_from = "mean")

ChgCntlSD<-chgRaclong %>% 
  filter(trt=="Control") %>% 
  group_by(site_code, year, measure) %>% 
  summarize(cntSD=sd(value))

GlassD<-chgRacMeans %>% 
  left_join(ChgCntlSD) %>% 
  mutate(RR=(Drought-Control)/cntSD) %>% 
  filter(RR!=Inf&RR!=-Inf) %>% 
  select(-Drought, -Control) %>% 
  bind_rows(RRMult) %>% 
  left_join(drt) %>% 
  filter(n_treat_years<4)


ggplot(data = subset(GlassD, -20<RR&RR<20), aes(x=measure, y=RR))+
  geom_boxplot(aes(group=measure))+
  scale_x_discrete(limits=c("richness_change", "evenness_change", "dominance", 'rank_change', 'gains', 'losses', 'composition_change', 'dispersion_change'), labels=c("Richness", "Evenness", "Dominance", "Ranks", "Gains", 'Losses', "Composition", "Dispersion"))+
  geom_hline(yintercept = 0)+
  annotate("text", x=2, y=10, label="*", size=8, color="red")+
  annotate("text", x=5, y=12, label="*", size=8, color="red")+
  annotate("text", x=6, y=16, label="*", size=8,color="red")+
  annotate("text", x=7, y=8, label="*", size=8,color="red")+
  xlab("Measure of Community Change")+
  ylab("Glass's D")
  


# doing stats on change ---------------------------------------------------

##1) Are there differences from zero?

rich<-GlassD %>% 
  filter(measure=="richness_change")
t.test(rich$RR, mu=0, alternative = "two.sided")
#this is NOT sig.
even<-GlassD %>% 
  filter(measure=="evenness_change")
t.test(even$RR, mu=0, alternative = "two.sided")
#this is sig.
dom<-GlassD %>% 
  filter(measure=="dominance")
t.test(dom$RR, mu=0, alternative = "two.sided")
#this is not sig.
rank<-GlassD %>% 
  filter(measure=="rank_change")
t.test(rank$RR, mu=0, alternative = "two.sided")
#this is NOT sig.
gain<-GlassD %>% 
  filter(measure=="gains")
t.test(gain$RR, mu=0, alternative = "two.sided")
#this is sig.
loss<-GlassD %>% 
  filter(measure=="losses")
t.test(loss$RR, mu=0, alternative = "two.sided")
#this is sig.
comp<-GlassD %>% 
  filter(measure=="composition_change")
t.test(comp$RR, mu=0, alternative = "two.sided")
#this is sig.
disp<-GlassD %>% 
  filter(measure=="dispersion_change")
t.test(disp$RR, mu=0, alternative = "two.sided")
#this is NOT sig.

###looking at changes over time.
##there are no differences among the years at all

mrich<-lmer(RR~as.factor(n_treat_years)+(1|site_code), data=rich)
summary(mrich)
#no effect of year
meven<-lmer(RR~as.factor(n_treat_years)+(1|site_code), data=even)
summary(meven)
#no effect of year
mdom<-lmer(RR~as.factor(n_treat_years)+(1|site_code), data=dom)
summary(mdom)
#Sig effect of year
ggplot(data=dom, aes(x=n_treat_years, y=RR))+
  geom_point()+
  geom_smooth(method = "lm", color="black")+
  ylab("Dominance Glass's D")+
  xlab("Year of Drought")

mrank<-lmer(RR~as.factor(n_treat_years)+(1|site_code), data=rank)
summary(mrank)
#no effect of year
mgain<-lmer(RR~as.factor(n_treat_years)+(1|site_code), data=gain)
summary(mgain)
#no effect of year
mloss<-lmer(RR~as.factor(n_treat_years)+(1|site_code), data=loss)
summary(mloss)
#no effect of year
mcomp<-lmer(RR~as.factor(n_treat_years)+(1|site_code), data=comp)
summary(mcomp)
#no effect of year
mdisp<-lmer(RR~as.factor(n_treat_years)+(1|site_code), data=disp)
summary(mdisp)
#no effect of year



RR2<-GlassD %>% 
  left_join(site_types) %>% 
  left_join(continent) 




###looking at local and regional drivers
mrich<-lmer(RR~drtseverity+map+PctAnnual+PctGrass+(1|site_code), data=subset(RR2, measure=="richness_change"))
summary(mrich)
#effect of percent annual

ggplot(data=subset(RR2, measure=="richness_change"), aes(x=PctAnnual, y=RR))+
  geom_point()+
  ylab("Richness Glass's D")+
  geom_smooth(method="lm", color="black")


meven<-lmer(RR~drtseverity+map+PctAnnual+PctGrass+(1|site_code), data=subset(RR2, measure=="evenness_change"))
summary(meven)
#no effect of anything
mrank<-lmer(RR~drtseverity+map+PctAnnual+PctGrass+(1|site_code), data=subset(RR2, measure=="rank_change"))
summary(mrank)
#nothing
mgain<-lmer(RR~drtseverity+map+PctAnnual+PctGrass+(1|site_code), data=subset(RR2, measure=="gains"))
summary(mgain)
#what does sig intercept mean? mild relationship with MAP

ggplot(data=subset(RR2, measure=="gains"), aes(x=map, y=RR))+
  geom_point()+
  ylab("Gains Glass's D")+
  xlab("MAP")+
  geom_smooth(method = "lm", color="black")

mloss<-lmer(RR~drtseverity+map+PctAnnual+PctGrass+(1|site_code), data=subset(RR2, measure=="losses"))
summary(mloss)
#nothing is significant
mcomp<-lmer(RR~drtseverity+map+PctAnnual+PctGrass+(1|site_code), data=subset(RR2, measure=="composition_change"))
summary(mcomp)
#percent annaul mild manner
ggplot(data=subset(RR2, measure=="composition_change"), aes(x=PctAnnual, y=RR))+
  geom_point()+
  geom_smooth(method="lm", color="black")+
  ylab("Composition Response")

mdisp<-lmer(RR~drtseverity+map+PctAnnual+PctGrass+(1|site_code), data=subset(RR2, measure=="dispersion_change"))
summary(mdisp)
#big effect percent grass

ggplot(data=subset(RR2, measure=="dispersion_change"), aes(x=PctGrass, y=RR))+
  geom_point()+
  geom_smooth(method="lm", color="black")+
  ylab("Dispersion Response")

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
