## Laura Dee ANPP
## DroughtNet: Explore ANPP 
#Feb 7 2024

#File paths to IDE data. 
# •IDE dropbox
# •Data and metadata
# •ANPP data
# •IDE/data_processed/anpp_ppt_datecode.csv
# •Cover data
# •IDE/data_processed/Cover_ppt_datecode.csv
# •Site info
# •IDE/data_processed/Site_Elev-Disturb.csv

#Close graphics and clear local memory
graphics.off()
rm(list=ls())

#packages
library(data.table)
library(fixest)
library(viridis)
require(ggplot2)
require(knitr)
require(stargazer)

setwd("~/Dropbox/data_processed/")
cover <- fread("Cover_ppt_2023-11-27.csv")
anpp <- fread("anpp_ppt_2023-11-03.csv")
site <- fread("Site_Elev-Disturb.csv")
climate <- fread("climate/climate_mean_annual_by_site_v3.csv")

head(cover)
cover$X =  NULL
cover$V1 =  NULL
# ppt.1  - rainfall 365 days before treatment, plot level 
# ppt.2 - rainfall 365 days before ppt.1
# ppt.3 - rainfall 365 days before ppt.2 
# map site report mean annual precip
head(anpp)
anpp$X = NULL
anpp$V1 = NULL
## Need to double check; to make max_cover NOT a character
cover$max_cover <- as.numeric(cover$max_cover)

## Are species that are absent recorded?
summary(cover$max_cover)
summary(cover$max_cover == "0")

## Filter data table to live cover and drop species with max_cover of 0.
cover = cover[live==1,]
cover = cover[max_cover > 0,]

# Explore ANPP data 
table(anpp$n_treat_years)
table(anpp$trt)
table(anpp$mass_category)
summary(anpp$mass)
hist(anpp$mass)
# check out subplot 
table(anpp$subplot) 
View(anpp[which(anpp$subplot == "B" ),])  #sand.us, llara.au , rhijn.nl 
View(anpp[which(anpp$subplot == "C" ),]) #sand.us, llara.au , rhijn.nl 
View(anpp[which(anpp$subplot == "D" ),]) #sand.us, llara.au , rhijn.nl 
View(anpp[which(anpp$subplot == "M" ),]) #cedartraits.us
View(anpp[which(anpp$subplot == "N" ),]) #cedartraits.us
View(anpp[which(anpp$subplot == "S" ),])#cedartraits.us

#*** HENRY TO CHECK UNQIUE SUBPLOTS 

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

###############################################################################################
## Create year to year change variable #####################################################
#############################################################################################
anpp[order(year), changemass := mass-shift(mass), by =.(newplotid)]

##################################################################################################
### Make some dummy variables  #############################################################
##################################################################################################
# Make a variable that identifies if it is a pre-treatment year or not 
anpp[,is.PretreatmentYr := (n_treat_years <= 0),]

#make this also a variable that is a 0 or 1 to indicate 1 (TRUE) if it is a treated year or not for processing the full dataset
# to only sites that also have pre-treatment years. 
anpp[,is.TreatedYear := (is.PretreatmentYr != TRUE),]

# Create dummy variable for before or not 
anpp$before <- ifelse(anpp$is.PretreatmentYr == "TRUE", 1, 0)

# Create dummy variable for after *EXPOSED* or not  
anpp$exp <- ifelse(anpp$is.PretreatmentYr == "TRUE", 0, 1)

# Create dummy variable for trt or not 
anpp$treat <- ifelse(anpp$trt == "Drought", 1, 0)

#create variable if treated and after 
anpp$treat_exp = anpp$exp*anpp$treat

###############################################################################
## Parallel Trends Plot ###########################################################
#####################################################################################
## From: https://skranz.github.io/r/2021/10/20/ParallelTrendsPlot.html

# show.plot = function(dat,label="", show.means=TRUE) {
#   library(ggplot2)
#   gdat = dat %>%
#     group_by(group, t,exp,treat) %>%
#     summarize(y = mean(y))
# 
# gg = ggplot(gdat, aes(y=y,x=t, color= group)) +
#   geom_line() + 
#   geom_vline(xintercept=T/2) +
#   theme_bw() +
#   annotate("text",x=T/4, y = 0.9*max(gdat$y), label=label)
# 
# if (show.means) {
#   y.pre.tr <<- mean(filter(gdat,treat==1, exp==0)$y) %>% round(1)
#   y.exp.tr <<- mean(filter(gdat,treat==1, exp==1)$y) %>% round(1)
#   y.pre.co <<- mean(filter(gdat,treat==0, exp==0)$y) %>% round(1)
#   y.exp.co <<- mean(filter(gdat,treat==0, exp==1)$y) %>% round(1)
#   gg = gg + 
#     annotate("label", x=T/4, y=y.pre.tr+15,label=y.pre.tr) +
#     annotate("label", x=T/4, y=y.pre.co-15,label=y.pre.co) +
#     annotate("label", x=T*0.75, y=y.exp.tr+15,label=y.exp.tr) +
#     annotate("label", x=T*0.75, y=y.exp.co-15,label=y.exp.co)
# }
# gg
# }  
# show.plot(dat)
library(tidyr)
first_treatment_yr <- min(gdat$year[gdat$exp == 1])

sg = anpp[site_code == "sgsdrt.us",]

show.plot = function(dat, site, label="", show.means=TRUE) {
   library(ggplot2)
   
  gdat = dat %>%
    dplyr::filter(site_code == site) %>%
    dplyr::group_by(trt,year,exp,treat) %>%
    dplyr::summarize(y = mean(mass)) %>%
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
      annotate("label", x=min(gdat$year) + treatment_length/4, y=y.pre.tr,label=y.pre.tr) +
      annotate("label", x=min(gdat$year) + treatment_length/4, y=y.pre.co,label=y.pre.co) +
      annotate("label", x=max(gdat$year) - treatment_length/4, y=y.exp.tr,label=y.exp.tr) +
      annotate("label", x=max(gdat$year) - treatment_length/4, y=y.exp.co,label=y.exp.co)
  }
  gg
} 

show.plot(anpp)
show.plot(anpp, "sgsdrt.us") # No pretreatment we should filter to sites with at least one full PT year. 
table(sg$n_treat_years, sg$year)

unique(anpp$site_code[anpp$n_treat_years <= -1])
show.plot(anpp, "cdpt_drt.us")
show.plot(anpp, "kranz.de")
show.plot(anpp, "morient.ar")
show.plot(anpp, "sgsdrt.us")
show.plot(anpp, "yarradrt.au")

##*** ADD ERROR BARS TO THE FIGURE ****


#####################################################################################
## Diff-in-Diff #################################################################
#####################################################################################
#filter to only sites that have pre-treatment data
#Find min value of treatment years; 0 = pre-treatment data
anpp[,min.trt.yr:=min(n_treat_years), by=.(site_code)]
anpp[,max.trt.yr:=max(n_treat_years), by=.(site_code)]
anpp = anpp[min.trt.yr <= 0, ] 
anpp = anpp[habitat.type != "Forest" ,]
anpp = anpp[habitat.type != "Forest understory" ,]

reg <- lm(mass ~ treat + exp + treat_exp, data = anpp)
reg <- lm(mass ~ treat + exp + treat_exp:habitat.type, data = anpp)
reg <- lm(mass ~ treat + exp + treat_exp + treat_exp:max.trt.yr, data = anpp)
reg <- lm(mass ~ treat + exp + treat_exp:site_code, data = anpp)
# reg <- lm(mass ~ treat:site_code + exp:site_code + treat_exp:site_code, data = anpp)
stargazer( reg, 
         #type = "html",
         type = "text",
         summary = TRUE,
           dep.var.labels = ("ANPP"),
           column.labels = c("Estimate"),
           covariate.labels = c("Intercept (B0)", 
                                "Treatment group (B1)", 
                                "Post-treatment (B2)", 
                                "Diff in Diff (B3)"),
           omit.stat = "all", 
          ci=TRUE, ci.level=0.95, 
           digits = 0, 
           intercept.bottom = FALSE)

grass.anpp = anpp[habitat.type == "Grassland", ] 

reg <- lm(mass ~ treat + exp + treat_exp, data = grass.anpp)
reg <- lm(mass ~ treat + exp + treat_exp:site_code, data = grass.anpp)
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

shrub.anpp = anpp[habitat.type == "Shrubland", ] 
reg <- lm(mass ~ treat + exp + treat_exp, data = shrub.anpp)
