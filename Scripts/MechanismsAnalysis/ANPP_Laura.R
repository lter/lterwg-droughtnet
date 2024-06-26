## Laura Dee ANPP
## DroughtNet: Explore ANPP 
#Feb 7 2024

##******### To do - Feb 7 2024 - create a filter to remove sites with no pretreatment year, 
##*# and a certain number of years of data?
##*#**# create error bars on the parallel trends figures 
##****  Create the nominal and extreme drought variables and drought severity 

#File paths to IDE data:
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
require(tidyr)
require(ddplyr)

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
# hist(anpp$mass)
# check out subplot 
table(anpp$subplot) 
# View(anpp[which(anpp$subplot == "B" ),]) #sand.us, llara.au , rhijn.nl 
# View(anpp[which(anpp$subplot == "C" ),]) #sand.us, llara.au , rhijn.nl 
# View(anpp[which(anpp$subplot == "D" ),]) #sand.us, llara.au , rhijn.nl 
# View(anpp[which(anpp$subplot == "M" ),]) #cedartraits.us
# View(anpp[which(anpp$subplot == "N" ),]) #cedartraits.us
# View(anpp[which(anpp$subplot == "S" ),]) #cedartraits.us

#*** HENRY TO CHECK UNQIUE SUBPLOTS *****

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

####################################################################################
### Plot variation in ANPP by plot from year to year by site ########################
######################################################################################
# anpp = anpp[changemass != NA,]
anpp1 = anpp[!is.na(changemass)]
anpp1$treat_exp <- as.character(anpp1$treat_exp)
Fig <- ggplot(data = anpp1, aes(x = changemass, fill = treat_exp)) +  facet_wrap(~site_code) + theme_bw() +
  geom_vline(xintercept=c(0,0), color = "blue", linetype="dashed") +
  geom_histogram(bins = 100) +
  labs(x = "Plot-level change in anpp year to year") +  theme_bw() +
  theme(axis.title.y= element_text(size=14)) + theme(axis.title.x= element_text(size=12)) +
  theme(axis.text.y = element_text(size = 14)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(size=14)) 
Fig

#spot check
yarradrt.au = anpp[site_code == "yarradrt.au"]
yarradrt.au  = yarradrt.au[!is.na(changemass)]
table( yarradrt.au$year, yarradrt.au$changemass)
yarradrt.au$treat_exp <- as.character(yarradrt.au$treat_exp)
Fig <- ggplot(data = yarradrt.au, aes(x = changemass, fill = treat_exp)) +   theme_bw() +
  geom_vline(xintercept=c(0,0), color = "blue", linetype="dashed") +
  geom_histogram(bins = 100) +
  labs(x = "Plot-level change in anpp year to year") +  theme_bw() +
  theme(axis.title.y= element_text(size=14)) + theme(axis.title.x= element_text(size=12)) +
  theme(axis.text.y = element_text(size = 14)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(size=14)) 
Fig
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
## and this website for the standard deviations on the plots: https://typethepipe.com/vizs-and-tips/ggplot-geom_ribbon-shadow-confidence-interval/

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

#first_treatment_yr <- min(gdat$year[gdat$exp == 1])

show.plot = function(dat, site, label="", show.means=TRUE) {
   library(ggplot2)
   
  gdat = dat %>%
    dplyr::filter(site_code == site) %>%
    dplyr::group_by(trt,year,exp,treat) %>%
    dplyr::summarize(y = mean(mass), y_sd = sd(mass)) %>%
    dplyr::mutate(year = as.numeric(year))
  
  first_treatment_yr <- min(gdat$year[gdat$exp == 1]) - 0.5
  treatment_length <- max(gdat$year) - min(gdat$year)  

  gg = ggplot(gdat, aes(y=y,x=year, color= trt)) +
    geom_line() + 
    geom_ribbon(aes(ymin=y-y_sd, ymax=y+y_sd), alpha = 0.1, group = gdat$trt,
                linetype = "dotted") +
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

show.plot(anpp, "sgsdrt.us")  
table(sg$n_treat_years, sg$year)

#***** we should filter to sites with at least one full PT year, probably earlier.. ###

unique(anpp$site_code[anpp$n_treat_years <= -1])
show.plot(anpp, "cdpt_drt.us")
show.plot(anpp, "kranz.de")
show.plot(anpp, "morient.ar")
show.plot(anpp, "sgsdrt.us")
show.plot(anpp, "yarradrt.au")

############################################################################
#### Separate out types of drought  ###########################################################
#####################################################################################

#nominal vs extreme  as a categorical variable 
drought_type_df <- anpp %>%
  filter(trt == "Control") %>%
  select(c(site_code, year, ppt.1, map)) %>%
  distinct() %>%
  mutate(drought.type = ifelse(ppt.1 > map, "nominal", "extreme")) %>%
  select(-c(ppt.1, map))

## create a single row for newplotid and year (vs a row per Taxon!)
anpp <- anpp %>%
  left_join(drought_type_df)

# Create extreme or not as a dummy variable 
extreme_df <- anpp %>%
  filter(trt == "Control") %>%
  select(c(site_code, year, ppt.1, map)) %>%
  distinct() %>%
  mutate(extreme = ifelse(ppt.1 > map, "0", "1")) %>%
  select(-c(ppt.1, map))

## create a single row for newplotid and year (vs a row per Taxon!)
anpp <- anpp %>%
  left_join(extreme_df)

#check it worked
head(anpp)

#####################################################################################
## Diff-in-Diff ####################################################################
#####################################################################################
#filter to only sites that have pre-treatment data
#Find min value of treatment years; 0 = pre-treatment data
anpp[,min.trt.yr:=min(n_treat_years), by=.(site_code)]
anpp[,max.trt.yr:=max(n_treat_years), by=.(site_code)]
anpp = anpp[min.trt.yr <= 0, ] 
anpp = anpp[habitat.type != "Forest" ,]
anpp = anpp[habitat.type != "Forest understory" ,]

#five sites are not repeated over years, and only have one year of data - from Meghan Avolio 
# oneyr<-dat2 %>% 
#   select(site_code, n_treat_years) %>% 
#   unique() %>% 
#   group_by(site_code) %>% 
#   mutate(max=max(n_treat_years)) %>% 
#   filter(max==1)

reg <- lm(mass ~ treat + exp + treat_exp*extreme, data = anpp)
summary(reg)
reg <- lm(mass ~ treat + exp + treat_exp + treat_exp:habitat.type:extreme, data = anpp)
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
reg <- lm(mass ~ treat + exp + treat_exp*extreme, data = anpp) 
summary(reg)
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
reg <- lm(mass ~ treat + exp + treat_exp*extreme, data = anpp)

summary(reg)

reg <- lm(mass ~ treat + exp + treat_exp + treat_exp:max.trt.yr:extreme, data = shrub.anpp)
summary(reg)

######################################################################
### Drought Severity and Drought Classifications ########################
########################################################################
# **** Need to get this code form TIM to classify as nominal, extreme, and to reproduce their continuous severity metrics
#getting drought severity - from Meghan Avolio 
# drt<-dat2 %>% 
#   filter(trt=="Drought") %>% 
#   select(site_code, n_treat_years, trt, year, map, ppt.1, ppt.2, ppt.3, ppt.4) %>%   unique() %>% 
#   mutate(drtseverity=(ppt.1-map)/map) %>% 
#   select(-ppt.1, -ppt.2, -ppt.3, -ppt.4) %>% 
#   filter(n_treat_years<4)

###***** TO DO ******
###CHECK FOR DUPLICATES ### 
# looks ok after filtering out forests (prades.es) and making a unique ID that includes block (for Boulder)
dups = anpp[,.N, by=c("site_code", "plot", "year")][N>1,]
# Empty data.table (0 rows and 4 cols): site_code,plot,year,N
table(anpp[,.N, by=c("site_code", "plot", "year")][,N])

bldrdrt.us = anpp[site_code == "bldrdrt.us"]
View(bldrdrt.us) ## this has a block!! so it is unique 

prades.es = anpp[site_code == "prades.es"]  # filter this one out 
View(prades.es) # filter this one out 

anpp[,.N, by=c("site_code", "newplotid", "year")][N>1,]
# Empty data.table (0 rows and 4 cols): site_code,newplotid,year,N

anpp[,.N, by=c("site_code", "newplotid", "n_treat_years")][N>1,]
# Empty data.table (0 rows and 4 cols): site_code,newplotid,year_trt,N

anpp[,.N, by=c("site_code", "plot", "n_treat_years")][N>1,]
#Empty data.table (0 rows and 4 cols): site_code,plot,year_trt,N

################################################################################
## OUTPUT AND WRITE OUT #######################################################
################################################################################
## see which sites and years & write out the site and year list as a table: 
tab =  table(anpp$site_code, anpp$year)
write.csv(tab, "~/Dropbox/dnDatasetDescript-PlotsSiteYearList.csv")
