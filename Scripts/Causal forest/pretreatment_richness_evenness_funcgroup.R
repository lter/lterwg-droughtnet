##just gonna make my own proportion graminoid and richness here
library(tidyverse)
library(plyr)
library(codyn)


cover_ppt <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/cover_ppt_2026-03-27.csv")%>%
  dplyr::select(site_code, block, plot, subplot, Taxon, functional_group, ps_path, n_treat_years, max_cover)%>%
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


#########same but for c3 vs c4
c4_sum <- cover_ppt%>%
  group_by(site_code, block, plot, subplot, ps_path)%>%
  dplyr::summarise(c4_cover = sum(max_cover))%>%
  subset(ps_path == "C4")

temp <- left_join(all_sum, c4_sum, by = c("site_code", "block", "plot", "subplot")) 

temp$c4_cover <- ifelse(is.na(temp$c4_cover)==TRUE, 0, temp$c4_cover)
temp$percent_c4 <- temp$c4_cover/temp$all_cover

perc_c4 <- temp%>%
  group_by(site_code)%>%
  dplyr::summarise(percent_c4 = mean(percent_c4))


######now make same df for species richness

#ave.richness <- cover_ppt%>%
#  dplyr::select(site_code, block, plot, subplot, Taxon)%>%
#  group_by(site_code, block, plot, subplot)%>%
#  dplyr::summarise(richness = length(Taxon))%>%
#  group_by(site_code)%>%
#  dplyr::summarise(richness = mean(richness))

ave.evenness.richness <- cover_ppt%>%
  dplyr::select(site_code, block, plot, subplot, Taxon, max_cover)%>%
  unite("site::rep",c("site_code", "block", "plot", "subplot"), sep = "::")%>%
  community_structure(abundance.var = "max_cover", replicate.var = "site::rep", metric = "Evar")%>%
  separate("site::rep", c("site_code", "block", "plot", "subplot"), sep = "::")%>%
  group_by(site_code)%>%
  dplyr::summarise(ave.richness = mean(richness, na.rm = TRUE), ave.evenness = mean(Evar, na.rm = TRUE) )






pretreatment_community_info <- left_join(perc_gram,perc_c4, by = "site_code")%>%
                      left_join( ave.evenness.richness, by = "site_code")





write.csv(pretreatment_community_info, "C:/Users/ohler/Dropbox/IDE/data_processed/pretreatment_community_info.csv")
