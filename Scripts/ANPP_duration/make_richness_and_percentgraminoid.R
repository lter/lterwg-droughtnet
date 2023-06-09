##just gonna make my own proportion graminoid and richness here
library(tidyverse)
library(plyr)


cover_ppt <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/cover_ppt_2023-05-10.csv")%>%
      dplyr::select(site_code, block, plot, subplot, Taxon, functional_group, n_treat_years, max_cover)%>%
      subset(n_treat_years == "0")

cover_ppt$functional_group_revised <- ifelse(cover_ppt$functional_group == "GRASS", "GRAMINOID", cover_ppt$functional_group)

all_sum <- cover_ppt%>%
  group_by(site_code, block, plot, subplot)%>%
  dplyr::summarise(all_cover = sum(max_cover))
  

gram_sum <- cover_ppt%>%
    group_by(site_code, block, plot, subplot, functional_group_revised)%>%
    dplyr::summarise(gram_cover = sum(max_cover))%>%
    subset(functional_group_revised == "GRAMINOID")


temp <- left_join(all_sum, gram_sum, by = c("site_code", "block", "plot", "subplot")) 

temp$gram_cover <- ifelse(is.na(temp$gram_cover)==TRUE, 0, temp$gram_cover)
temp$percent_graminoid <- temp$gram_cover/temp$all_cover

perc_gram <- temp%>%
              group_by(site_code)%>%
              dplyr::summarise(percent_graminoid = mean(percent_graminoid))

######now make same df for species richness

ave.richness <- cover_ppt%>%
    dplyr::select(site_code, block, plot, subplot, Taxon)%>%
    group_by(site_code, block, plot, subplot)%>%
    dplyr::summarise(richness = length(Taxon))%>%
    group_by(site_code)%>%
    dplyr::summarise(richness = mean(richness))



graminoid_richness <- left_join(perc_gram, ave.richness, by = "site_code")  


write.csv(graminoid_richness, "C:/Users/ohler/Dropbox/IDE/data_processed/graminoids_and_richness.csv")
