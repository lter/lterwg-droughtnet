#Get RR with IDE_community_analyses
#Get data.anpp.summary with Duration_analyses_2024-03-07

#read in drougth data
nominal<-read.csv('C:\\Users\\ohler\\Dropbox\\IDE\\data_processed\\IDE_duration_sites_years.csv')

rr3<-RR2 %>% 
  left_join(nominal) %>% 
  group_by(n_treat_years, measure, e.n) %>% 
  dplyr::summarise(mrr=mean(RR), se=(sd(RR))/sqrt(length(RR))) %>% 
  na.omit() %>% 
  filter(measure!='rank_change'&measure!='evenness_change')

labs<-c('richness_change'='Richness Change', 'gains'='Gains', 'losses'='Losses')

fig<-ggplot(data=rr3, aes(x=n_treat_years, y=mrr, color=e.n))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=mrr-se, ymax=mrr+se), width=0.25)+
  facet_wrap(~measure, labeller=labeller(measure=labs))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_hline(yintercept = 0)+
  ylab("Drought-Control Differences")+
  xlab('Year of Drought Treatment')+
  scale_color_manual(name="Drought type", values=c('orange2', 'gray'), labels=c('Extreme', 'Nominal'))

ggsave('C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\papers\\supplemental_fig_RR.pdf',fig, width=8, height=5, units='in' )


###stats

#anpp<-read.csv('C:\\Users\\ohler\\Dropbox\\IDE_Duration_ms\\data_table.csv')%>%
#        dplyr::select(Site.code, Vegetation.type, Number.of.treatment.years, Productivity.response, Drought.severity)


comp <- left_join(RR2, data.anpp.summary, by = c("site_code", "n_treat_years"))%>%
        subset(type == "Herbaceous.Perennial"|type == "Woody.Perennial")
comp$n_treat_years <- as.character(comp$n_treat_years)

unique(comp$site_code) #down to 49 sites



##Categorcal by year
mod <- lme(RR~e.n*n_treat_years, random = ~1|ipcc_regions/site_code, data = subset(comp, measure=="gains"))
summary(mod)
pairs(emmeans(mod, ~e.n | n_treat_years, var="n_treat_years"))


mod <- lme(RR~e.n*n_treat_years, random = ~1|ipcc_regions/site_code, data = subset(comp, measure=="losses"))
summary(mod)
pairs(emmeans(mod, ~e.n | n_treat_years, var="n_treat_years"))

mod <- lme(RR~e.n*n_treat_years, random = ~1|ipcc_regions/site_code, data = subset(comp, measure=="richness_change"))
summary(mod)
pairs(emmeans(mod, ~e.n | n_treat_years, var="n_treat_years"))




##Regressed against drought severity
mod <- lme(RR~drtsev.1, random = ~1|ipcc_regions, data = subset(comp, measure == "losses" & n_treat_years == "1"))
summary(mod)

mod <- lme(RR~drtsev.1, random = ~1|ipcc_regions, data = subset(comp, measure == "losses" & n_treat_years == "2"))
summary(mod)

mod <- lme(RR~drtsev.1, random = ~1|ipcc_regions, data = subset(comp, measure == "losses" & n_treat_years == "3"))
summary(mod)

mod <- lme(RR~drtsev.1, random = ~1|ipcc_regions, data = subset(comp, measure == "losses" & n_treat_years == "4"))
summary(mod)












###losses to anpp response
mod <- lme(anpp_response~RR, random = ~1|ipcc_regions, data = subset(comp, measure == "losses" & n_treat_years == "1"))
summary(mod)

mod <- lme(anpp_response~RR, random = ~1|ipcc_regions, data = subset(comp, measure == "losses" & n_treat_years == "2"))
summary(mod)

mod <- lme(anpp_response~RR, random = ~1|ipcc_regions, data = subset(comp, measure == "losses" & n_treat_years == "3"))
summary(mod)

mod <- lme(anpp_response~RR, random = ~1|ipcc_regions, data = subset(comp, measure == "losses" & n_treat_years == "4"))
summary(mod)
