###Code to for each site what % grass/forb and annual/perennial species
###code by Meghan Avolio, Feb 8, 2023
###Updataing files Oct 11, 2023

library(tidyverse)
#library(codyn)

setwd("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\data_processed")
setwd("E:Dropbox\\IDE (1)\\data_processed")

dat<-read.csv("cover_ppt_2023-10-12.csv") %>% 
  mutate(replicate=paste(block, plot, subplot, sep="::"),
         lifeform2=ifelse(local_lifeform=="GRAMINOID"|local_lifeform=="Grass", "GRASS", local_lifeform))


# Calculating by plots in year 0 only -------------------------------------

#dropping datasets without pretreatment
drop_no_pretrt<-dat %>%
  select(site_code, n_treat_years) %>%
  unique() %>%
  filter(n_treat_years==0) %>%
  select(-n_treat_years)

#doing this on pretreatment data
dat2<-dat %>%
  filter(n_treat_years==0)

totalcover<-dat2 %>% 
  group_by(site_code, replicate) %>% 
  summarize(tot=sum(max_cover))

pctLifeForm<-dat2 %>% 
  group_by(site_code, replicate, lifeform2) %>% 
  summarize(sum=sum(max_cover)) %>% 
  pivot_wider(names_from = lifeform2, values_from = sum, values_fill = 0) %>% 
  pivot_longer(FORB:MOSS, names_to = "lifeform2", values_to="sum") %>% 
  filter(lifeform2=="GRASS") %>% 
  left_join(totalcover) %>% 
  mutate(PGrass=(sum/tot)*100) %>% 
  group_by(site_code) %>% 
  summarize(PctGrass=mean(PGrass))

test<-dat2 %>% 
  filter(lifeform2=="Grass") %>% 
  select(site_code, Taxon, local_lifeform, local_lifespan, local_provenance, N.fixer, ps_path, functional_group) %>% 
  unique()

pctLifehistory<-dat2 %>% 
  group_by(site_code, replicate, local_lifespan) %>% 
  summarize(sum=sum(max_cover)) %>% 
  filter(local_lifespan!="NULL") %>% 
  pivot_wider(names_from = local_lifespan, values_from = sum, values_fill = 0) %>% 
  pivot_longer(ANNUAL:UNK, names_to = "local_lifespan", values_to="sum") %>% 
  filter(local_lifespan=="ANNUAL") %>% 
  left_join(totalcover) %>% 
  mutate(PAnn=(sum/tot)*100) %>% 
  group_by(site_code) %>% 
  summarize(PctAnnual=mean(PAnn)) %>% 
  full_join(pctLifeForm)

write.csv(pctLifehistory, "community_comp\\Prc_LifeHistory_Oct2023.csv", row.names=F)


# Calculating for controls over all years ---------------------------------

#dropping treated plots
controls<-dat %>%
  select(site_code, trt) %>%
  unique() %>%
  filter(trt=='Control') %>%
  select(-trt)

#doing this on contols data
datControl<-dat %>%
  filter(trt=='Control')

totalcover_control<-datControl %>% 
  group_by(site_code, replicate) %>% 
  summarize(tot=sum(max_cover))

pctLifeForm_contorl<-datControl %>% 
  group_by(site_code, replicate, lifeform2) %>% 
  summarize(sum=sum(max_cover)) %>% 
  pivot_wider(names_from = lifeform2, values_from = sum, values_fill = 0) %>% 
  pivot_longer(FORB:MOSS, names_to = "lifeform2", values_to="sum") %>% 
  filter(lifeform2=="GRASS") %>% 
  left_join(totalcover_control) %>% 
  mutate(PGrass=(sum/tot)*100) %>% 
  group_by(site_code) %>% 
  summarize(PctGrass=mean(PGrass))

pctLifehistory_control<-datControl %>% 
  group_by(site_code, replicate, local_lifespan) %>% 
  summarize(sum=sum(max_cover)) %>% 
  filter(local_lifespan!="NULL") %>% 
  pivot_wider(names_from = local_lifespan, values_from = sum, values_fill = 0) %>%   pivot_longer(ANNUAL:UNK, names_to = "local_lifespan", values_to="sum") %>% 
  filter(local_lifespan=="ANNUAL") %>% 
  left_join(totalcover_control) %>% 
  mutate(PAnn=(sum/tot)*100) %>% 
  group_by(site_code) %>% 
  summarize(PctAnnual=mean(PAnn)) %>% 
  full_join(pctLifeForm_contorl)

write.csv(pctLifehistory_control, "community_comp\\Prc_LifeHistory_Controls_Oct2023.csv", row.names=F)



combine<-pctLifehistory_control %>% 
  left_join(pctLifehistory)


ggplot(data=combine, aes(x=CPctAnnual, y=PctAnnual))+
  geom_point()


ggplot(data=pctLifehistory_control, aes(x=CPctAnnual))+
  geom_histogram()
