###Make litter data by plot


library(tidyverse)

litter <- read.csv("C:/Users/ohler/Dropbox/IDE MS_Single year extreme/Data/full_biomass_10-16-2023.csv")%>%
          subset(mass_category == "LITTER")%>%
          dplyr::select(site_code, block, plot, subplot, year, biomass_date, mass, mass_category)

length(unique(litter$site_code))

write.csv(litter, "C:/Users/ohler/Dropbox/IDE/data_processed/IDE_litter_2024-02-14.csv")

