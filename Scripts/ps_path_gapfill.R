library(tidyverse)

dat <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/cover_ppt_2023-05-01.csv")%>%
  select(Family, Taxon, ps_path)%>%
  unique()


dat1 <- subset(dat, is.na(ps_path) == TRUE)%>%
  separate(Taxon, into = c("Genus", "Species"), remove = FALSE, sep = " ")


dat1$ps_path <- ifelse(dat1$Genus == "BINERTIA" | dat1$Genus == "TIDESTROMIA" | dat1$Genus == "PECTIS" | dat1$Genus == "EUPLOCA" | dat1$Genus == "BULBOSTYLIS" | dat1$Genus == "CYPERUS" | dat1$Genus == "FIMBRISTYLIS" | dat1$Genus == "CHAMAESYCE" | dat1$Genus == "ALLIONIA" | dat1$Genus == "CALLIGONUM" | dat1$Genus == "PORTULACA" | dat1$Genus == "EUPHORBIA", "C4",
                       ifelse(dat1$Genus == "BELAPHARIS" | dat1$Genus == "AERVA" | dat1$Genus == "ALTERNANTHERA" | dat1$Genus == "ATRIPLEX" | dat1$Genus == "SUAEDA" | dat1$Genus == "TECTICORNIA" | dat1$Genus == "FLAVERIA" | dat1$Genus == "POLYCARPOREA" | dat1$Genus == "ELEOCHARS" | dat1$Genus == "RHYNCHOSPORA" | dat1$Genus == "EUPHORBIA" | dat1$Genus == "MOLLUGO" | dat1$Genus == "BOERHAVIA" | dat1$Genus == "BASSIA" | dat1$Family == "Poaceae", "UNK",
                       "C3"))





Whole Genus
Binertia #whole
Tidestromia#whole
Pectis#whole
Euploca#whole
Bulbostylis#whol3
Cyperus#whole
Fimbristylis#whole
Chamaesyce#whole
Allionia#whole
Calligonum#whole
Portulaca#whole

Incomplete Genus
Belapharis
Aerva
Alternanthera
Atriplex
Suaeda
Tecticornia
Flaveria
Polycarporea
Eleochars
Rhynchospora
Euphorbia
Mollugo
Boerhavia
Bassia
Poaceae



