ide.precip <- read.csv("~/Desktop/anpp_ppt_2023-01-11.csv")

library(formattable)
library(tidyr)

# select just control plots - not interested in drought plots for this...
ide.precip.ctrls <- subset(ide.precip, ide.precip$trt == "Control")

str(ide.precip.ctrls)
ide.precip.ctrls$ppt.map <- as.numeric(ide.precip.ctrls$ppt.map)
ide.precip.ctrls$percent_reduction <- as.numeric(ide.precip.ctrls$percent_reduction)

# get a single value per site per year - not interested in plot-level values for this...
ide.precip.ctrls.siteavgs <- aggregate(x = ide.precip.ctrls[c("site_map", "ppt.1", "ppt.map", "percent_reduction", "mass")],
                                       by = ide.precip.ctrls[c("site_code", "n_treat_years")],
                                       FUN = mean, na.rm = TRUE)

# select values just for trt years 1, 2, 3, and 4...
ide.precip.ctrls.siteavgs <- subset(ide.precip.ctrls.siteavgs, 
                                    ide.precip.ctrls.siteavgs$n_treat_years == "1" | ide.precip.ctrls.siteavgs$n_treat_years == "2" 
                                    | ide.precip.ctrls.siteavgs$n_treat_years == "3" | ide.precip.ctrls.siteavgs$n_treat_years == "4")
# pivot to wide format
ide.precip.ctrls.siteavgs.wide <- ide.precip.ctrls.siteavgs %>% 
  pivot_wider(names_from = n_treat_years, values_from = c(ppt.1, ppt.map, percent_reduction, mass))

# create subset of just sites with 4 years of extreme drought (ppt-map<0)
ide.4year.edrt.sites <- subset(ide.precip.ctrls.siteavgs.wide, 
                               ide.precip.ctrls.siteavgs.wide$ppt.map_1 < 0 & 
                                 ide.precip.ctrls.siteavgs.wide$ppt.map_2 < 0 &
                                 ide.precip.ctrls.siteavgs.wide$ppt.map_3 < 0 &
                                 ide.precip.ctrls.siteavgs.wide$ppt.map_4 < 0)
View(ide.4year.edrt.sites)

# create subset of columns for table-ing (just table-ing ppt-map in year 1-4 for now...)
vars.mm <- c('site_code', 'site_map', 'ppt.map_1', 'ppt.map_2', 'ppt.map_3', 'ppt.map_4')

ide.precip.ctrls.siteavgs.mm <- ide.precip.ctrls.siteavgs.wide[vars.mm]
ide.precip.ctrls.siteavgs.mm

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
