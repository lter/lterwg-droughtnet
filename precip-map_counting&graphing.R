ide.precip <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/anpp_ppt_map_2023-01-11.csv")

library(formattable)
library(tidyr)
library(dplyr)

# calculate ppt reduction from map and percent ppt reduction from map
ide.precip$ppt.map <- ide.precip$ppt.1 - ide.precip$site_map
ide.precip$percent_ppt_red <- (ide.precip$ppt.map / ide.precip$site_map)*100

# select just control plots - not interested in drought plots for this...
ide.precip.ctrls <- subset(ide.precip, ide.precip$trt == "Control")

str(ide.precip.ctrls)
ide.precip.ctrls$ppt.map <- round(ide.precip.ctrls$ppt.map, 1)
ide.precip.ctrls$percent_ppt_red <- round(ide.precip.ctrls$percent_ppt_red, 1)
ide.precip.ctrls$mass <- round(ide.precip.ctrls$mass, 2)
ide.precip.ctrls$ppt.1 <- round(ide.precip.ctrls$ppt.1, 1)
ide.precip.ctrls$ppt.2 <- round(ide.precip.ctrls$ppt.2, 1)
ide.precip.ctrls$ppt.3 <- round(ide.precip.ctrls$ppt.3, 1)
ide.precip.ctrls$ppt.4 <- round(ide.precip.ctrls$ppt.4, 1)

# get a single value per site per year - not interested in plot-level values for this...
ide.precip.ctrls.siteavgs <- aggregate(x = ide.precip.ctrls[c("site_map", "ppt.1", "ppt.map", "percent_ppt_red", "mass")],
                                       by = ide.precip.ctrls[c("site_code", "n_treat_years")],
                                       FUN = mean, na.rm = TRUE)

# select values just for trt years 1, 2, 3, and 4 for now...
ide.precip.ctrls.siteavgs <- subset(ide.precip.ctrls.siteavgs, 
                                    ide.precip.ctrls.siteavgs$n_treat_years == "1" | ide.precip.ctrls.siteavgs$n_treat_years == "2" 
                                    | ide.precip.ctrls.siteavgs$n_treat_years == "3" | ide.precip.ctrls.siteavgs$n_treat_years == "4")
# pivot to wide format
ide.precip.ctrls.siteavgs.wide <- ide.precip.ctrls.siteavgs %>% 
  pivot_wider(names_from = n_treat_years, values_from = c(ppt.1, ppt.map, percent_ppt_red, mass))

# create subset of just sites with 4 consecutive years of extreme drought (ppt-map<0)
ide.4year.edrt.sites <- subset(ide.precip.ctrls.siteavgs.wide, 
                               ide.precip.ctrls.siteavgs.wide$ppt.map_1 < 0 & 
                                 ide.precip.ctrls.siteavgs.wide$ppt.map_2 < 0 &
                                 ide.precip.ctrls.siteavgs.wide$ppt.map_3 < 0 &
                                 ide.precip.ctrls.siteavgs.wide$ppt.map_4 < 0)
#View(ide.4year.edrt.sites)
nrow(ide.4year.edrt.sites)
ide.4year.edrt.sites.df <- dplyr::select(ide.4year.edrt.sites, site_code)

# create subset of just sites with 3 consecutive years of extreme drought (ppt-map<0)
ide.3year.edrt.sites <- subset(ide.precip.ctrls.siteavgs.wide, 
                               ide.precip.ctrls.siteavgs.wide$ppt.map_1 < 0 & 
                                 ide.precip.ctrls.siteavgs.wide$ppt.map_2 < 0 &
                                 ide.precip.ctrls.siteavgs.wide$ppt.map_3 < 0  )#| for now, I'm only using the sites where the first three years were extreme
                                # ide.precip.ctrls.siteavgs.wide$ppt.map_2 < 0 & 
                                # ide.precip.ctrls.siteavgs.wide$ppt.map_3 < 0 &
                                # ide.precip.ctrls.siteavgs.wide$ppt.map_4 < 0)
#View(ide.3year.edrt.sites)
nrow(ide.3year.edrt.sites)
ide.3year.edrt.sites.df <- dplyr::select(ide.3year.edrt.sites, site_code)

# create subset of just sites with 2 consecutive years of extreme drought (ppt-map<0)
ide.2year.edrt.sites <- subset(ide.precip.ctrls.siteavgs.wide, 
                               ide.precip.ctrls.siteavgs.wide$ppt.map_1 < 0 & ide.precip.ctrls.siteavgs.wide$ppt.map_2 < 0 |
                                 ide.precip.ctrls.siteavgs.wide$ppt.map_2 < 0 & ide.precip.ctrls.siteavgs.wide$ppt.map_3 < 0 | 
                                 ide.precip.ctrls.siteavgs.wide$ppt.map_3 < 0 & ide.precip.ctrls.siteavgs.wide$ppt.map_4 < 0)
#View(ide.2year.edrt.sites)
nrow(ide.2year.edrt.sites)

# create subset of columns for table-ing (just table-ing ppt-map in year 1-4 for now...)
vars.mm <- c('site_code', 'site_map', 'ppt.map_1', 'ppt.map_2', 'ppt.map_3', 'ppt.map_4')

ide.precip.ctrls.siteavgs.mm <- ide.precip.ctrls.siteavgs.wide[vars.mm]
ide.precip.ctrls.siteavgs.mm
ide.precip.ctrls.siteavgs.mm <- ide.precip.ctrls.siteavgs.mm %>% arrange(site_code)

yr.formatter <- formatter("span", style = x ~ style(color = ifelse(x > 0, "green", ifelse(x < 0, "red", "grey"))))

table.allsites <- formattable(ide.precip.ctrls.siteavgs.mm, 
                              align =c("l", "c", "c", "c", "c", "c"), 
                              list('ppt.map_1' = yr.formatter,'ppt.map_2' = yr.formatter,
                                   'ppt.map_3' = yr.formatter,'ppt.map_4' = yr.formatter))
table.allsites

ide.4year.edrt.sites.mm <- ide.4year.edrt.sites[vars.mm]
ide.4year.edrt.sites.mm

table.4year.edrt.sites <- formattable(ide.4year.edrt.sites.mm, 
                                      align =c("l", "c", "c", "c", "c", "c"), 
                                      list('ppt.map_1' = yr.formatter,'ppt.map_2' = yr.formatter,
                                           'ppt.map_3' = yr.formatter,'ppt.map_4' = yr.formatter))
table.4year.edrt.sites

ide.3year.edrt.sites.mm <- ide.3year.edrt.sites[vars.mm]
ide.3year.edrt.sites.mm

table.3year.edrt.sites <- formattable(ide.3year.edrt.sites.mm, 
                                      align =c("l", "c", "c", "c", "c", "c"), 
                                      list('ppt.map_1' = yr.formatter,'ppt.map_2' = yr.formatter,
                                           'ppt.map_3' = yr.formatter,'ppt.map_4' = yr.formatter))
table.3year.edrt.sites

ide.2year.edrt.sites.mm <- ide.2year.edrt.sites[vars.mm]
ide.2year.edrt.sites.mm

table.2year.edrt.sites <- formattable(ide.2year.edrt.sites.mm, 
                                      align =c("l", "c", "c", "c", "c", "c"), 
                                      list('ppt.map_1' = yr.formatter,'ppt.map_2' = yr.formatter,
                                           'ppt.map_3' = yr.formatter,'ppt.map_4' = yr.formatter))
table.2year.edrt.sites
