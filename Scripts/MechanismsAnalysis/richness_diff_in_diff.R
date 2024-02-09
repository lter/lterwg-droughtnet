#Close graphics and clear local memory
graphics.off()
rm(list=ls())

library(tidyverse)
library(data.table)
library(fixest)
library(viridis)
require(ggplot2)
require(knitr)
require(stargazer)

# setwd("~/Dropbox/data_processed/")
# cover <- fread("Cover_ppt_henry.csv")
setwd("~/Dropbox/DroughtMechanisms/")
cover <- fread("processsed_cover_Feb92024.csv")

## Filter data table to live cover and drop species with max_cover of 0.
cover = cover[live==1,]
cover = cover[max_cover > 0,]

##################################################################################################
### Making a unique plot id and year as factor ####################################################
##################################################################################################
# class(cover$plot) # covert to a character
anpp$plot <- as.character(anpp$plot)
cover$year <- as.character(anpp$year)
class(anpp$block) # covert to a character
anpp$block <- as.character(anpp$block)

#in general: my.dt[,newplotid:=as.factor(paste(site.id.col, plot.id.col, sep="_"))]
anpp[,newplotid:=as.factor(paste(site_code, block, plot, subplot, sep="_"))]

class(anpp$year) 
anpp$year <- as.character(anpp$year)

#######################################################################################
### parallel trends plots
#######################################################################################

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

############################################################################
#### Separate out types of drought  ###########################################################
#####################################################################################

#nominal vs extreme 
drought_type_df <- cover %>%
  filter(trt == "Control") %>%
  select(c(site_code, year, ppt.1, map)) %>%
  distinct() %>%
  mutate(drought.type = ifelse(ppt.1 > map, "nominal", "extreme")) %>%
  select(-c(ppt.1, map))

## create a single row for newplotid and year (vs a row per Taxon!)
cover <- cover %>%
  left_join(drought_type_df)

################################################################################
### Prep Data for analysis ####################################################
# remove Taxon rows that are NA!!!
################################################################################
cover$year <- as.character(cover$year)
cover <- na.omit(cover)

#to subset columns and also remove duplicate rows from the cover file so that there is one observation per plot and year 
# and the data isn't artificially replicated 
length(unique(cover$newplotid))
# [1] 538 
length(unique(paste0(cover$newplotid, cover$year)))
#[1] 2316

coversummary = unique(cover[, .(habitat.type, site_code, year, trt, newplotid,  n_treat_years, drought.type, #site and year features 
                                sr_INT, sr_NAT, sr_UNK, sr_INT.site, 
                                AnnualGrassCover, PerenGrassCover, WoodyCover, PerForbCover, AnnualForbCover, # combined cover groups
                                AnnualPercentcover.yr, PerenPercentcover.yr, GrassPercentcover.yr, ForbPercentcover.yr,
                                INTcover, Native_cover.yr, sr_annual_forb, sr_per_forb, sr_annual_grass,
                                sr_per_grass, sr_woody, sr_grass, sr_forbs, sr_legume, sr_nonvascular,
                                sr_annual_INT, sr_per_INT, sr_annual_NAT, sr_per_NAT)]) # , na.rm = T
nrow(coversummary)
cover = coversummary

##################################################################################################
### Make some dummy variables  #############################################################
##################################################################################################
# Make a variable that identifies if it is a pre-treatment year or not 
cover[,is.PretreatmentYr := (n_treat_years <= 0),]

#make this also a variable that is a 0 or 1 to indicate 1 (TRUE) if it is a treated year or not for processing the full dataset
# to only sites that also have pre-treatment years. 
cover[,is.TreatedYear := (is.PretreatmentYr != TRUE),]

# Create dummy variable for before or not 
cover$before <- ifelse(cover$is.PretreatmentYr == "TRUE", 1, 0)

# Create dummy variable for after *EXPOSED* or not  
cover$exp <- ifelse(cover$is.PretreatmentYr == "TRUE", 0, 1)

# Create dummy variable for trt or not 
cover$treat <- ifelse(cover$trt == "Drought", 1, 0)

#create variable if treated and after 
cover$treat_exp = cover$exp*cover$treat

## filter to sites with at least one pre-treatment year

#filter to only sites that have pre-treatment data
#Find min value of treatment years; 0 = pre-treatment data
cover[,min.trt.yr:=min(n_treat_years), by=.(site_code)]
cover[,max.trt.yr:=max(n_treat_years), by=.(site_code)]
cover = cover[min.trt.yr <= 0, ] 

#five sites are not repeated over years, and only have one year of data - from Meghan Avolio 
# oneyr<-dat2 %>% 
#   select(site_code, n_treat_years) %>% 
#   unique() %>% 
#   group_by(site_code) %>% 
#   mutate(max=max(n_treat_years)) %>% 
#   filter(max==1)

# filter to grassland and shrubland 
cover = cover[habitat.type != "Forest" ,]
cover = cover[habitat.type != "Forest understory" ,]

############################################################################
#### ESTiMATION ###########################################################
#####################################################################################
#dataset for extreme droughts only??? 


reg <- lm(AnnualGrassCover ~ treat + exp + treat_exp + treat_exp:drought.type, data = cover)
reg <- lm(AnnualGrassCover ~ treat + exp + treat_exp , data = cover)
summary(reg)

reg <- lm(sr_annual_INT ~ treat + exp + treat_exp  + treat_exp:drought.type, data = cover)
summary(reg)

reg <- lm(sr_annual_NAT ~ treat + exp + treat_exp  + treat_exp:drought.type, data = cover)
summary(reg)

reg <- lm(sr_woody ~ treat + exp + treat_exp  + treat_exp:drought.type, data = cover)
summary(reg)

reg <- lm( PerenGrassCover ~ treat + exp + treat_exp  + treat_exp:drought.type, data = cover)
summary(reg)

reg <- lm(PerForbCover~ treat + exp + treat_exp  + treat_exp:drought.type, data = cover)
summary(reg)



reg <- lm(GrassPercentcover.yr ~ treat + exp + treat_exp + treat_exp:max.trt.yr, data = cover)

# 
# reg <- lm(mass ~ treat + exp + treat_exp:habitat.type, data = anpp)
# reg <- lm(mass ~ treat + exp + treat_exp + treat_exp:max.trt.yr, data = anpp)
# reg <- lm(mass ~ treat + exp + treat_exp:site_code, data = anpp)
# # reg <- lm(mass ~ treat:site_code + exp:site_code + treat_exp:site_code, data = anpp)
stargazer( reg, 
           #type = "html",
           type = "text",
           summary = TRUE,
           dep.var.labels = ("Cover var"),
           column.labels = c("Estimate"),
           covariate.labels = c("Intercept (B0)", 
                                "Treatment group (B1)", 
                                "Post-treatment (B2)", 
                                "Diff in Diff (B3)"),
           omit.stat = "all", 
           ci=TRUE, ci.level=0.95, 
           digits = 0, 
           intercept.bottom = FALSE)

grass.cover = cover[habitat.type == "Grassland", ] 

reg <- lm( sr_NAT ~ treat + exp + treat_exp, data = grass.cover)
summary(reg)

reg <- lm(sr_NAT ~ treat + exp + treat_exp:site_code, data = grass.cover)
# reg <- lm(mass ~ treat:site_code + exp:site_code + treat_exp:site_code, data = grass.anpp)

stargazer( reg, 
           #type = "html",
           type = "text",
           summary = TRUE,
           dep.var.labels = ("ANPP"),
           column.labels = c("estimate"),
           covariate.labels = c("Intercept (B0)", 
                                "Treatment group (B1)", 
                                "Post-treatment (B2)", 
                                "Diff in Diff (B3)"),
           omit.stat = "all", 
           ci=TRUE, ci.level=0.95, 
           digits = 0, 
           intercept.bottom = FALSE)

shrub/cov = cover[habitat.type == "Shrubland", ] 
# reg <- lm(mass ~ treat + exp + treat_exp, data = shrub.anpp)
# reg <- lm(mass ~ treat + exp + treat_exp + treat_exp:max.trt.yr, data = shrub.anpp)



