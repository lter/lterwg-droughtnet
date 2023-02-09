###Code to for each site what % grass/forb and annual/perennial species
###cody by Meghan Avolio, Feb 8, 2023

library(tidyverse)
#library(codyn)

setwd("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\data_processed")

dat<-read.csv("cover_ppt_2023-02-06.csv") %>% 
  mutate(replicate=paste(block, plot, subplot, sep="::"),
         lifeform2=ifelse(local_lifeform=="GRAMINOID", "GRASS", local_lifeform))

#dropping datasets without pretreatment
drop_no_pretrt<-dat %>% 
  select(site_code, n_treat_years) %>% 
  unique() %>% 
  filter(n_treat_years==0) %>% 
  select(-n_treat_years)

dat2<-dat %>% 
  right_join(drop_no_pretrt) %>% 
  filter(trt=="Drought")

totalcover<-dat2 %>% 
  filter(n_treat_years==0) %>% 
  group_by(site_code, replicate) %>% 
  summarize(tot=sum(max_cover))

pctLifeForm<-dat2 %>% 
  filter(n_treat_years==0) %>% 
  group_by(site_code, replicate, lifeform2) %>% 
  summarize(sum=sum(max_cover)) %>% 
  pivot_wider(names_from = lifeform2, values_from = sum, values_fill = 0) %>% 
  pivot_longer(FORB:CACTUS, names_to = "lifeform2", values_to="sum") %>% 
  filter(lifeform2=="GRASS") %>% 
  left_join(totalcover) %>% 
  mutate(PGrass=(sum/tot)*100) %>% 
  group_by(site_code) %>% 
  summarize(PctGrass=mean(PGrass))

pctLifehistory<-dat2 %>% 
  filter(n_treat_years==0) %>% 
  group_by(site_code, replicate, local_lifespan) %>% 
  summarize(sum=sum(max_cover)) %>% 
  pivot_wider(names_from = local_lifespan, values_from = sum, values_fill = 0) %>% 
  pivot_longer(ANNUAL:NULL, names_to = "local_lifespan", values_to="sum") %>% 
  filter(local_lifespan=="ANNUAL") %>% 
  left_join(totalcover) %>% 
  mutate(PAnn=(sum/tot)*100) %>% 
  group_by(site_code) %>% 
  summarize(PctAnnual=mean(PAnn)) %>% 
  left_join(pctLifeForm)

