###Code to for how different control-treatment communities are with drought
###cody by Meghan Avolio, Feb 8, 2023
###March 21, 2023. Working to include all communities for the difference analysis.
###May 4th, 2023, updating analyses with updated data. Are looking at change
###update Oct 11, 2023, updating datasets
#####update Nov 28, 2023 updating dataset and deleting code i'm no longer using\
###Feb 2024 updating to include new dominance analayses

library(tidyverse)
library(codyn)
library(lme4)
library(lmerTest)
library(vegan)
library(data.table)
library(gridExtra)

theme_set(theme_bw(12))

setwd("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\data_processed")

dat<-read.csv("cover_ppt_2023-11-27.csv") %>% 
  mutate(replicate=paste(block, plot, subplot, sep="_"))

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

#having problems with cdpt_drt.us and eea.br because they only have year 0 data. 
dat3<-dat2 %>% 
  filter(site_code!="cdpt_drt.us"&site_code!="eea.br")

sites<-dat3 %>%
select(site_code) %>%
  unique()
# write.csv(sites, "community_comp\\sitelistMarhc2024.csv")

# Calculating drought severity and site metrics -------------------------------------------

#getting drought severity
drt<-dat2 %>% 
  filter(trt=="Drought") %>% 
  select(site_code, n_treat_years, trt, year, map, ppt.1, ppt.2, ppt.3, ppt.4) %>%   unique() %>% 
  mutate(drtseverity=(ppt.1-map)/map) %>% 
  select(-ppt.1, -ppt.2, -ppt.3, -ppt.4) %>% 
  filter(n_treat_years<4)

site_types<-read.csv("community_comp\\Prc_LifeHistory_Controls_Oct2023.csv")

precipcv<-read.csv("climate\\climate_mean_annual_by_site_v3.csv")

#site richness
rich_0_site<-dat3 %>% 
  filter(n_treat_years==0) %>% 
  group_by(site_code, Taxon) %>% 
  summarize(mcov=mean(max_cover))

siterichness<-community_structure(df=rich_0_site, abundance.var = 'mcov', replicate.var = 'site_code') 

#meghan doing DCI

## Need to add in zeros
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

# sc<-unique(dat3$site_code)
# mdom_spp<-data.frame()
# 
# for (i in 1:length(sc)){
#   
#   subset<-unique.ras%>%
#     filter(site_code==sc[i]) %>%
#     mutate(ranks = dense_rank(-dci)) %>%
#     filter(ranks == 1)
#   dom_spp <- rbind(dom_spp, subset)
# }



# # calculating dominant species
# cover <- as.data.table(dat3)[,totplotcover.yr.live := sum(max_cover, na.rm= T), by=.(site_code, year, block, plot, subplot)]
# cover[,relative_sp_cover.yr.live := max_cover/totplotcover.yr.live]
# #sum(is.na(cover$relative_sp_cover.yr.live))
# cover[, totsitecover.yr := sum(totplotcover.yr.live, na.rm= T), by=.(site_code, year)]
# cover[, tot_maxcover_site.yr  := sum(max_cover, na.rm= T), by=.(Taxon, site_code, year)]
# 
# cover[is.na(tot_maxcover_site.yr),tot_maxcover_site.yr := 0]
# cover[is.na(totsitecover.yr),totsitecover.yr  := 0] #not necessary
# 
# cover[, relative_abundance_spp_site.yr  :=  tot_maxcover_site.yr/totsitecover.yr, by=.(Taxon, site_code, year)]
# 
# #create a variable for just year 0 
# cover[, relative_abundance_spp_site.yr0 := min(relative_abundance_spp_site.yr[n_treat_years==0]), by=.(Taxon, site_code)]
# cover[is.infinite(relative_abundance_spp_site.yr0),relative_abundance_spp_site.yr0 := NA]
# 
# cover[, tot.num.plots := length(unique(replicate[n_treat_years == 0])), by =.(site_code)]
# cover[, tot.num.plots.with.spp := length(unique(replicate[n_treat_years == 0 & max_cover>0])), by =.(site_code, Taxon)]
# 
# cover[, rel_freq.space :=  tot.num.plots.with.spp/tot.num.plots]
# cover[is.na(rel_freq.space),rel_freq.space  := 0] #32520 NAs
# 
# unique.ras = unique(cover[, .(site_code, Taxon, relative_abundance_spp_site.yr0, rel_freq.space)])
# 
# # calculate DCI
# # relative abundance + relative frequency / 2
# # 0.6 was dominant 
# unique.ras[, dci := (relative_abundance_spp_site.yr0 + rel_freq.space)/2]
# hist(unique.ras$dci)
# sc<-unique(dat3$site_code)
# dom_spp<-data.frame()
# 
# for (i in 1:length(sc)){
#   
#   subset<-unique.ras%>%
#     filter(site_code==sc[i]) %>%
#     mutate(ranks = dense_rank(-dci)) %>%
#     filter(ranks == 1)
#   dom_spp <- rbind(dom_spp, subset)
# }

#write.csv(dom_spp, "IDE_dominant_spp_by_DCI_yr0.csv")

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

# Getting measures of change ----------------------------------------------


#summarizing the data
# drt1yr<-dat2 %>% 
#   filter(n_treat_years==1) %>% 
#   select(site_code) %>% 
#   unique() %>% 
#   mutate(p=1)
# drt2yr<-dat2 %>% 
#   filter(n_treat_years==2) %>% 
#   select(site_code) %>% 
#   unique() %>% 
#   mutate(p2=1)
# drt3yr<-dat2 %>% 
#   filter(n_treat_years==3) %>% 
#   select(site_code) %>% 
#   unique()
# 
# #five sites are not repeated over years, and only have one year of data.
# oneyr<-dat2 %>% 
#   select(site_code, n_treat_years) %>% 
#   unique() %>% 
#   group_by(site_code) %>% 
#   mutate(max=max(n_treat_years)) %>% 
#   filter(max==1)



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

#1) are C-T rates of change different from one-another and do they differ over time

deltarac3yrs<-deltaracs %>% 
  left_join(deltadom) %>% 
  filter(n_treat_years<4& n_treat_years>0) 

uniquereps<-deltarac3yrs %>%
  select(site_code, replicate) %>%
  unique() %>%
  group_by(site_code) %>%
  mutate(rep=rank(replicate))

# test<-uniquereps %>%
#   group_by(site_code, rep) %>%
#   summarize(n=length(rep))


deltarac3yrs2<-deltarac3yrs %>%
  left_join(uniquereps) %>%
  mutate(rep2=paste(site_code, rep, sep=''))

#write.csv(deltarac3yrs2, "CommunityData_DrtbyTime_forSAS_withdom.csv", row.names=F)

#####I am no longer doing these stats in R.

# length(unique(deltarac3yrs$site_code))
# 
# mrich<-lmer(richness_change~trt*n_treat_years + (1|site_code/replicate), data=deltarac3yrs)
# anova(mrich)
# 
# meven<-lmer(evenness_change~trt*n_treat_years + (1|site_code/replicate), data=deltarac3yrs)
# anova(meven)
# 
# mrank<-lmer(rank_change~trt*n_treat_years + (1|site_code/replicate), data=deltarac3yrs)
# anova(mrank)
# 
# mgain<-lmer(gains~trt*n_treat_years + (1|site_code/replicate), data=deltarac3yrs)
# anova(mgain)
# 
# mloss<-lmer(losses~trt*n_treat_years + (1|site_code/replicate), data=deltarac3yrs)
# anova(mloss)
# 
# ggplot(data=deltarac3yrs, aes(x=n_treat_years, y=rank_change, color=trt))+
#   geom_point()+
#   geom_smooth(method = "lm")

#adjust P-values for treat effect
#these values are now from SAS
pvalsTRT=data.frame(measure=c('richness_change', 'evenness_change', 'rank_change', 'gains', 'losses', 'deltaabund'), pvalue=c(0.00001, 0.357,  0.214, 0.011, 0.00001, 0.0008)) %>%
  mutate(padj=paste("p = " , round(p.adjust(pvalue, method="BH"), 3)))

pvalsTRTYR=data.frame(measure=c('rich', 'even', 'rank', 'gain', 'loss', 'abund'), pvalue=c(0.8805, 0.299, 0.0726, 0.834, 0.982, 0.007)) %>%
  mutate(padj=p.adjust(pvalue, method="BH"))

pvalsYR=data.frame(measure=c('rich', 'even', 'rank', 'gain', 'loss', 'abund'), pvalue=c(0.013, 0.001, 0.001, 0.0001, 0.0001, 0.0005)) %>% 
  mutate(padj=p.adjust(pvalue, method="BH"))

MeanCI<-deltarac3yrs %>%
  select(site_code, replicate, year, trt, n_treat_years, richness_change, evenness_change, rank_change, gains, losses, change) %>% 
  pivot_longer(richness_change:change, names_to = "measure", values_to = "value") %>% 
  group_by(trt, measure) %>% 
  summarize(mean=mean(value, na.rm=T), n=length(value), sd=sd(value, na.rm=T)) %>% 
  mutate(se=sd/sqrt(n), CI=se*1.96) %>% 
  mutate(n_treat_years=4)


### Figure 1: Magnitude of effects over time ----
# Pivot and get summary stats
deltaracs_long_by_year <-deltarac3yrs %>%
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
  geom_vline(xintercept = 3.5)+
  scale_x_discrete(labels=c('1', '2', '3', 'Overall'))

Fig2

#ggsave("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\Papers\\Community-comp_change\\Fig2.jpg", plot=Fig2, units = "in", width=6.5, height=5)


#doing responses. Because all outputs are bound between 0 and 1. We are just doing T-C. negative value means drought had lower values than control. positive values means drought had higher values than control

#2) does the severity of the drought affect the magnitude of drought responsiveness
RRRac<-deltaracs %>% 
  pivot_longer(names_to="measure", values_to = "value", richness_change:losses) %>% 
  group_by(site_code, year, n_treat_years, trt, measure) %>% 
  summarise(value=mean(value)) %>% 
  pivot_wider(names_from = "trt", values_from = "value") %>% 
  mutate(RR=(Drought-Control)) %>% 
  select(-Drought, -Control, n_treat_years)

RRall<-RRRac%>% 
  left_join(drt) %>% 
  filter(n_treat_years<4)

length(unique(RRall$site_code))

###
##dom change by site. 4 sites get dropped here bc the dom sp was only found in the pretreatment year
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

write.csv(RR2, 'communityData_DrtSeverity_forSAS_withDom.csv', row.names=F)
str(RR2)

####doing this is SAS now
###looking at drought severity
# 
# drt.rich<-lmer(RR~drtseverity+(1|site_code), data=subset(RR2, measure=="richness_change"))
# summary(drt.rich)
# anova(drt.rich)
# 
# drt.even<-lmer(RR~drtseverity+(1|site_code), data=subset(RR2, measure=="evenness_change"))
# summary(drt.even)
# anova(drt.even)
# 
# drt.rank<-lmer(RR~drtseverity+(1|site_code), data=subset(RR2, measure=="rank_change"))
# summary(drt.rank)
# anova(drt.rank)
# 
# drt.gain<-lmer(RR~drtseverity+(1|site_code), data=subset(RR2, measure=="gains"))
# summary(drt.gain)
# anova(drt.gain)
# 
# drt.loss<-lmer(RR~drtseverity+(1|site_code), data=subset(RR2, measure=="losses"))
# summary(drt.loss)
# anova(drt.loss)
# 
#####not doing this b/c nothing is significant in SAS so why bother
# pvalsDrtSev=data.frame(measure=c('rich', 'even', 'rank', 'gain', 'loss'), pvalue=c(0.06577, 0.09861, 0.4597, 0.4289, 0.04373)) %>% 
#   mutate(padj=p.adjust(pvalue, method="BH"))

###looking at regional drivers. take the average change measure for each site averaging overall years
RRRac_average<-deltaracs %>% 
  pivot_longer(names_to="measure", values_to = "value", richness_change:losses) %>% 
  filter(n_treat_years<4& n_treat_years>0) %>% 
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



# p.loss.map<-ggplot(data=subset(RRRac_average, measure=="losses"), aes(x=MAP, y=RR))+
#   geom_point()+
#   geom_smooth(method="lm", color="black", alpha=0.1)+
#   #ggtitle('Species Losses')+
#   ylab("Drought-Control\nSp. Loss Differences")+
#   xlab("MAP")+
#   geom_hline(yintercept = 0)+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# p.loss.map
# 
# p.loss.ann<-ggplot(data=subset(RRRac_average, measure=="losses"), aes(x=PctAnnual, y=RR))+
#   geom_point()+
#   geom_smooth(method="lm", color="black", alpha=0.2)+
#   #ggtitle('Species Losses')+
#   ylab("Drought-Control\nSp. Loss Differences")+
#   xlab("% Annuals")+
#   geom_hline(yintercept = 0)+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# p.loss.ann

ggsave("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\Papers\\Community-comp_change\\multipleregresson.jpg", plot=plotlm, units="in", width=3, height=4.5)

# Pairs plots of values in multiple regressions ---------------------------
###pairs
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

write.csv(fortable, 'C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\data_processed\\community_comp\\trajectory\\March 2024\\table for paper.csv', row.names = F)

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

#make for ESA
# esamap <- ggplot() + 
#   geom_sf(data = world, fill = "antiquewhite") + 
#   #geom_sf(data = oz_states, colour = "black", fill = NA) + 
#   geom_point(data = sites, mapping = aes(x = longitud, y = latitud, fill=continent),
#              pch = 21, 
#              color = "black",
#              size = 2) + 
#   scale_fill_paletteer_d(`"dutchmasters::milkmaid"`) +
#   geom_jitter(position = "jitter") +
#   theme_bw(base_size = 16) +
#   labs(x = "Latitude", y = "Longitude") + 
#   annotation_north_arrow(location = "bl", which_north = "true", 
#                          pad_x = unit(0.25, "in"), pad_y = unit(0.5, "in"),
#                          style = north_arrow_fancy_orienteering) + 
#   theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
#         panel.background = element_rect(fill = "aliceblue"), legend.position = "none") + 
#   coord_sf(ylim = c(-80, 80), expand = FALSE)

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


#stuff for an appendix
# Calculate plot richness in year 0
rich_0 <- dat3 %>%
  filter(n_treat_years == 0) %>% 
  mutate(rep=paste(site_code, replicate, sep='::'))

plotrichness<-community_structure(df=rich_0, abundance.var = 'max_cover', replicate.var = 'rep') %>% 
  separate(rep, into=c('site_code', 'replicate'), sep='::') %>% 
  group_by(site_code) %>% 
  summarize(mrich=mean(richness))
