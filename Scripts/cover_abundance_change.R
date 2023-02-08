###Code to figure out much cover changed for each year of drought.
###cody by Meghan Avolio, Feb 8, 2023

library(tidyverse)
library(codyn)

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
  right_join(drop_no_pretrt)

###looping through site
sc<-unique(dat2$site_code)

deltabund<-data.frame()

for (i in 1:length(sc)){
  
  subset<-dat2%>%
    filter(site_code==sc[i])
  
  pretrt<-unique(filter(subset, n_treat_years==0)$year)
  scode<-unique(subset$site_code)
  
                
delta_abund<-abundance_change(df=subset, time.var="year", species.var = "Taxon", abundance.var = "max_cover", replicate.var="replicate", reference.time = as.numeric(pretrt)) %>% 
  mutate(site_code=scode)

deltabund<-deltabund %>% 
  bind_rows(delta_abund)
}

write.csv(deltabund, "C:\\Users\\mavolio2\\Dropbox\\IDE_cover_change.csv", row.names=F)
