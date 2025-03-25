#Get RR with IDE_community_analyses

#read in drougth data
nominal<-read.csv('C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\data_processed\\IDE_duration_sites_years.csv')

rr3<-RR2 %>% 
  left_join(nominal) %>% 
  group_by(n_treat_years, measure, e.n) %>% 
  summarise(mrr=mean(RR), se=(sd(RR))/sqrt(length(RR))) %>% 
  na.omit()

ggplot(data=rr3, aes(x=n_treat_years, y=mrr, color=e.n))+
  geom_point()+
  geom_errorbar(aes(ymin=mrr-se, ymax=mrr+se), width=0.25)+
  facet_wrap(~measure)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_hline(yintercept = 0)+
  ylab("Drought-Control Differences")+
  xlab('Year of Drought Treatment')+
  scale_color_manual(name="Drought type", values=c('darkred', 'pink2'))
