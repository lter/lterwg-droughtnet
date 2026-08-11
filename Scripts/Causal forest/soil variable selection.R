library(tidyverse)




sand.df <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/site_sand_soc_from_soilgrid_2026-03-26.csv")


soil <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/IDE_soil_2024-12-16.csv")%>%
  group_by(site_code)%>%
  dplyr::summarize(ph = mean(ph, na.rm = TRUE), no3 = mean(as.numeric(no3), na.rm = TRUE), p = mean(as.numeric(p), na.rm = TRUE), k = mean(k, na.rm = TRUE), zn = mean(as.numeric(zn), na.rm = TRUE), fe = mean(fe, na.rm = TRUE), mn = mean(mn, na.rm = TRUE), cu = mean(cu, na.rm = TRUE),  sand = mean(as.numeric(sand), na.rm = TRUE),silt = mean(silt, na.rm = TRUE), clay = mean(clay, na.rm = TRUE), c = mean(c, na.rm = TRUE), n = mean(n, na.rm = TRUE), c_n = mean(c_n, na.rm = TRUE), organicmatter = mean(organicmatter, na.rm = TRUE))


df <- sand.df%>%
      left_join(soil, by = "site_code")


mod <- lm(n~n_0_5cm, df)
summary(mod)

mod <- lm(n~n_0_15cm, df)
summary(mod)

mod <- lm(n~n_0_30cm, df)
summary(mod)

mod <- lm(n~n_0_60cm, df)
summary(mod)

mod <- lm(n~n_0_60cm_weighted, df)
summary(mod)




mod <- lm(sand~sand_0_5cm, df)
summary(mod)

mod <- lm(sand~sand_0_15cm, df)
summary(mod)

mod <- lm(sand~sand_0_30cm, df)
summary(mod)

mod <- lm(sand~sand_0_60cm, df)
summary(mod)

mod <- lm(sand~sand_0_60cm_weighted, df)
summary(mod)







mod <- lm(c~soc_0_5cm, df)
summary(mod)

mod <- lm(c~soc_0_15cm, df)
summary(mod)

mod <- lm(c~soc_0_30cm, df)
summary(mod)

mod <- lm(c~soc_0_60cm, df)
summary(mod)

mod <- lm(c~soc_0_60cm_weighted, df)
summary(mod)
