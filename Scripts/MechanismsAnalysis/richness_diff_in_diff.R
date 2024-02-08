library(tidyverse)

setwd("~/Dropbox/data_processed/")
cover <- fread("Cover_ppt_henry.csv")

did_df <- cover %>%
  select(c(site_code, year, newplotid, trt, is.PretreatmentYr, sr_plot)) %>%
  drop_na() %>%
  distinct() %>%
  mutate(exp = !is.PretreatmentYr,
         treat = ifelse(trt == "Control", 0, 1)) %>%
  select(-is.PretreatmentYr)

show.plot = function(dat, site, label="", show.means=TRUE, show.errors=TRUE) {
  library(ggplot2)
  
  gdat = dat %>%
    dplyr::filter(site_code == site) %>%
    dplyr::group_by(trt,year,exp,treat) %>%
    dplyr::summarize(y = mean(sr_plot), y_sd = sd(sr_plot)) %>%
    dplyr::mutate(year = as.numeric(year))
  
  first_treatment_yr <- min(gdat$year[gdat$exp == 1]) - 0.5
  treatment_length <- max(gdat$year) - min(gdat$year)  
  
  gg = ggplot(gdat, aes(y=y,x=year, color= trt)) +
    geom_line() + 
    geom_vline(xintercept=first_treatment_yr) +
    scale_x_continuous(limits = c(min(gdat$year), max(gdat$year)),
                       breaks = seq(min(gdat$year), max(gdat$year), 1)) +
    theme_bw()
  #annotate("text",x=T/4, y = 0.9*max(gdat$y), label=label)
  
  if (show.means) {
    y.pre.tr <<- mean(dplyr::filter(gdat,treat==1, exp==0)$y) %>% round(1)
    y.exp.tr <<- mean(dplyr::filter(gdat,treat==1, exp==1)$y) %>% round(1)
    y.pre.co <<- mean(dplyr::filter(gdat,treat==0, exp==0)$y) %>% round(1)
    y.exp.co <<- mean(dplyr::filter(gdat,treat==0, exp==1)$y) %>% round(1)
    gg = gg + 
      annotate("label", x=min(gdat$year), y=y.pre.tr,label=y.pre.tr) +
      annotate("label", x=min(gdat$year), y=y.pre.co,label=y.pre.co) +
      annotate("label", x=max(gdat$year), y=y.exp.tr,label=y.exp.tr) +
      annotate("label", x=max(gdat$year), y=y.exp.co,label=y.exp.co)
  }
  
  if (show.errors) {
    gg = gg +
      geom_ribbon(aes(ymin=y-y_sd, ymax=y+y_sd), alpha = 0.1, group = gdat$trt,
                  linetype = "dotted")
  }
  
  gg
}

unique(data$site_code[data$n_treat_years <= -1])
show.plot(did_df, "cdpt_drt.us")
show.plot(did_df, "ethadb.au")
show.plot(did_df, "ethadn.au")
show.plot(did_df, "sgsdrt.us")
show.plot(did_df, "yarradrt.au")
