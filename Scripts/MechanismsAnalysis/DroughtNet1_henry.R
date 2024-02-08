## DroughtNet v1
#Laura 
#Feb 5 2024

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
require(tidyverse)

setwd("~/Dropbox/data_processed/")
cover <- fread("Cover_ppt_2023-11-27.csv")
anpp <- fread("anpp_ppt_2023-11-03.csv")
site <- fread("Site_Elev-Disturb.csv")
climate <- fread("climate/climate_mean_annual_by_site_v3.csv")

#Henry files 
# cover <- fread("C:/Users/henry/Downloads/Cover_ppt_2023-11-27.csv")
# anpp <- fread("C:/Users/henry/Downloads/anpp_ppt_2023-11-03.csv")
# site <- fread("C:/Users/henry/Downloads/Site_Elev-Disturb.csv")
# climate <- fread("C:/Users/henry/Downloads/climate_mean_annual_by_site_v3.csv")
#   
head(cover)
cover$X =  NULL
cover$V1 =  NULL
# ppt.1  - rainfall 365 days before treatment, plot level 
# ppt.2 - rainfall 365 days before ppt.1
# ppt.3 - rainfall 365 days before ppt.2 
# map site report mean annual precip
head(anpp)
anpp$X = NULL

## Need to double check; to make max_cover NOT a character
cover$max_cover <- as.numeric(cover$max_cover)

## Are species that are absent recorded?
summary(cover$max_cover)
summary(cover$max_cover == "0")

## Filter data table to live cover and drop species with max_cover of 0.
cover = cover[live==1,]
cover = cover[max_cover > 0,]

#Are there really some sites that have 7 years of pretreat data? Which?
table(cover$n_treat_years)
# View(cover[which(cover$n_treat_years== "-6" ),]) # sgsdrt.us

############################################################################################################
### Native vs Non-Native Variables #########################################################################
#############################################################################################################
table(cover$local_provenance)
# INT    NAT native   NULL    UNK 
# 6194  41146  9    910   3838 

# covert native to NAT 
cover[local_provenance =="native", local_provenance:="NAT"]

#convert NULL to UNK to combine  #these NULLs are stored as a string in the data.table so can run this. Checked with:
#cover[is.null(local_provenance),]
cover[local_provenance=="NULL", local_provenance:="UNK"] 

# all are all of the unknowns are a single site or across sites? Check with this line:
# View(cover[which(cover$local_provenance == "UNK"),]) # the species of unknown origins are across different sites.

###################################################################################################
#### Use to filter pre-treatment years or treated years ############################################
#################################################################################################### 
# Make a variable that identifies if it is a pre-treatment year or not 
cover[,is.PretreatmentYr := (n_treat_years <= 0),]

#make this also a variable that is a 0 or 1 to indicate 1 (TRUE) if it is a treated year or not for processing the full dataset
# to only sites that also have pre-treatment years. 
cover[,is.TreatedYear := (is.PretreatmentYr != TRUE),]

##################################################################################################
### Making a unique plot id and year as factor ####################################################
##################################################################################################
# class(cover$plot) # covert to a character
cover$plot <- as.character(cover$plot)
cover$year <- as.character(cover$year)
class(cover$block) # covert to a character
cover$block <- as.character(cover$block)

#in general: my.dt[,newplotid:=as.factor(paste(site.id.col, plot.id.col, sep="_"))]
cover[,newplotid:=as.factor(paste(site_code, block, plot, subplot, sep="_"))]

### FIRST DEAL WITH SUBPLOTS ### 
#looks like there are plots with different subplots.. 
table(cover$subplot)

# A     B     C     D     M     N     S 
# 49576   275   326   298   545   534   543 
# View(cover[which(cover$subplot == "B" ),]) #rhijn.nl and  llara.au 
# View(cover[which(cover$subplot == "C" ),]) #rhijn.nl and  llara.au 
# View(cover[which(cover$subplot == "D" ),]) #rhijn.nl and  llara.au  
# View(cover[which(cover$subplot == "M" ),]) #cedartrait.us
# View(cover[which(cover$subplot == "N" ),]) #cedartrait.us
# View(cover[which(cover$subplot == "S" ),]) #cedartrait.us

## check out block situation
table(cover$block) 

############################################################################################################
### Prep data and species list  #########################################################################
#############################################################################################################
## First, only the species PRESENT in a plot are recorded in the cover data, so species that are present at a site but not a plot 
# are *not* listed (i.e.  with max_cover = 0). We need to fix that before computing average relative abundance at a site.

# First, we compute list of all species at a site over the time period in the data:
sp.at.site.yr = unique(cover[,.(site_code, year, Taxon)])

# Want to create and merge one record per (Taxon, site_code, plot, year), using merge to flag records that weren't present 
# in original data.
subplot.yr.combos = unique(cover[,.(site_code, year, newplotid)])
expanded.spp.recs = merge(subplot.yr.combos, sp.at.site.yr, by = c("site_code", "year"), all.x=T, allow.cartesian = T)
cover = merge(cover, expanded.spp.recs, by=c("site_code", "year", "newplotid", "Taxon"), all.y=T)

# need to update n_treat_years, trt, live, local_provenance for the records that got added
#cover[,live:=1]
#cover[,trt:=min(trt[!is.na(trt)], na.rm=T), by=.(plot, site_code, year)]
#cover[,local_provenance:=min(local_provenance[!is.na(local_provenance)]),by=.(site_code, Taxon)]
#cover[,n_treat_years:=min(n_treat_years[!is.na(n_treat_years)], na.rm=T), by=.(plot, site_code, year)]

##########################################################################################################
##### Compute TOTAL & TOTAL LIVE RELATIVE COVER PER PLOT MEASURES ########################################
##########################################################################################################
# make a total cover in a plot, site, year. This includes live cover only.
cover[,totplotcover.yr.live := sum(max_cover, na.rm= T), by=.(newplotid, year)]

#Make a relative cover for each species in each plot and year
# based on TOTAL cover (including only live cover as we already filtered to the data table to live above).
cover[,relative_sp_cover.plot := max_cover/totplotcover.yr.live]
#***to check, run: sum(is.na(cover$relative_sp_cover.yr.live))

# calculate site-level relative abundance 2 ways:
# 1) pre-treatment relative abundance for each species, aggregated across all pre-treatment years
# 2) post-treatment relative abundance for each species, separated by year and treatment group

# calculate site-level relative abundance #1
total_cover_site_pre <- cover %>%
  filter(n_treat_years <= 0) %>%
  aggregate(max_cover ~ site_code, data = ., FUN = sum) %>%
  rename(total_cover = max_cover)
sp_cover_site_pre <- cover %>%
  filter(n_treat_years <= 0) %>%
  aggregate(max_cover ~ site_code + Taxon, data = ., FUN = sum) %>%
  left_join(total_cover_site_pre) %>%
  mutate(relative_sp_cover_site_pre = max_cover / total_cover) %>%
  select(-c(max_cover, total_cover))

# calculate site-level relative abundance #2
total_cover_site_post <- cover %>%
  filter(n_treat_years > 0) %>%
  aggregate(max_cover ~ site_code + year + trt, data = ., FUN = sum) %>%
  rename(total_cover = max_cover)
sp_cover_site_post <- cover %>%
  filter(n_treat_years > 0) %>%
  aggregate(max_cover ~ site_code + year + trt + Taxon, data = ., FUN = sum) %>%
  left_join(total_cover_site_post) %>%
  mutate(relative_sp_cover_site_treatment = max_cover / total_cover) %>%
  select(-c(max_cover, total_cover))


# add site-level relative abundances to main dataframe
cover <- cover %>%
  left_join(sp_cover_site_pre) %>%
  left_join(sp_cover_site_post)
cover$relative_sp_cover_site_treatment[is.na(cover$relative_sp_cover_site_treatment)] <- cover$relative_sp_cover_site_pre[is.na(cover$relative_sp_cover_site_treatment)]


#########################################################################################
# Laura's old code for calculating site-level relative abundance
# # make a site-level relative abundance for each species and year. This requires
# #first summing the total cover per site and year and the total cover of each species in all plots at site 
# cover[, totsitecover.yr := sum(max_cover, na.rm= T), by=.(site_code, year)]
# cover[, tot_maxcover_site.yr  := sum(max_cover, na.rm= T), by=.(Taxon, site_code, year)]
# #then divide these two numbers: 
# cover[, relative_sp_cover_site  :=  tot_maxcover_site.yr/totsitecover.yr, by=.(Taxon, site_code, year)]

# #create a variable for just year 0 
# cover[, relative_abundance_spp_site.yr0 := min(relative_abundance_spp_site.yr[n_treat_years==0]), by=.(Taxon, site_code)]
# cover[is.infinite(relative_abundance_spp_site.yr0),relative_abundance_spp_site.yr0 := NA]
# 
# # if the species isn't present at a site in year_trt == 0, give the species a relative abundance of 0 in that year:
# 
# ### Next step --create a relative frequency in year 0 variable #####
# #total # of plots within a site, for pre-treatment year:
# # again we use the pre-treatment year because we calculate the metrics at the site level and want to avoid classifying species post treatment
# cover[, tot.num.plots := length(unique(plot[n_treat_years == 0])), by =.(site_code)]  #this will work because no records of max_cover = 0.
# 
# #number of plots within a site, in the pre-treatment year, that a species occurred in:
# cover[, tot.num.plots.with.spp := length(unique(plot[n_treat_years== 0 & max_cover>0])), by =.(site_code, Taxon)]
# 
# ##Compute Relative Frequency in year 0.
# ## Relative frequency = number of plots at a site in year 0 a species occurred / total number of plots at a site in year 0" 
# # If a site has no records for plots in a pre-treatment year (year_trt==0), rel_freq.space will be NA.
# # That's fine -- these sites will be filtered out later
# cover[, rel_freq.space :=  tot.num.plots.with.spp/tot.num.plots]
# cover[is.na(rel_freq.space),rel_freq.space  := 0]

##### Compute a convenience column that says whether a species was present in a plot in a site in the pre-treatment year
cover[,present_year0:=max_cover[n_treat_years==0]>0, by=.(Taxon,site_code,plot)]

#################################################################################################################################
## Run this Code   if you want to Filter data to only species present in year 0 and save that dataset  #####
#################################################################################################################################
cover_present_year0 = cover[present_year0 == TRUE,]
# write.csv(cover_present_year0, "cover_present_year0.csv")

##############################################################################    
## Compute Species Richness  ######################################################     
##############################################################################    
# Species Richness
cover[, sr_plot := length(unique(Taxon[max_cover>0])), by = .(newplotid, year)]
cover[, sr_site := length(unique(Taxon[max_cover>0])), by = .(site_code, year)]

#create plot level change variable 
cover[order(year), changesr_plot := sr_plot-shift(sr_plot), by =.(newplotid)]

#create plot level lagged variable 
cover[order(year), laggedsr_plot := shift(sr_plot), by =.(newplotid)]

# Site-level richness ALL years
cover[, SR.site.allyears := length(unique(Taxon[max_cover>0])), by = .(site_code)]

####################################################################################
### Other plot level community change metrics - HENRY TO DO **** #################
####################################################################################
# Evenness - HENRY

# REORDERING***
#---------------------------------------------------------------------------------------#
metrics_df <- cover %>%
  select(site_code, year, block, plot) %>%
  distinct() %>%
  mutate(evenness = NA,
         reordering = NA)

for (i in 1:nrow(metrics_df)) {
  filtered_dat <- cover %>%
    filter(site_code == metrics_df$site_code[i]
           & year == metrics_df$year[i]
           & block == metrics_df$block[i]
           & plot == metrics_df$plot[i])
}

####################################################################################################
###  Changes in types of species by plot: Number of species ########################################
################################################################################################

###INVASIVE VS NATIVE ########

# Compute native, non-native, and unknown origin species richness by plot, site, year. Note filter to max_cover > 0 to 
# consider only species that were actually present (filter to max_cover>0)
cover[, sr_INT := length(unique(Taxon[local_provenance=="INT" & max_cover>0])), by = .(plot, site_code, year)]
cover[, sr_NAT := length(unique(Taxon[local_provenance=="NAT" & max_cover>0])), by = .(plot, site_code, year)]
cover[, sr_UNK := length(unique(Taxon[local_provenance=="UNK" & max_cover>0])), by = .(plot, site_code, year)]

## to make a plot of number of introduced species over time at a site 
cover[, sr_INT.site := length(unique(Taxon[local_provenance == "INT" & max_cover>0])), by = .(site_code, year)]
cover[, sr_NAT.site := length(unique(Taxon[local_provenance == "NAT" & max_cover>0])), by = .(site_code, year)]

#**** Identify which species are not present in year 0 .... **** 
#compute the SR of species that were not present in year 0 for each plot and year but are present in a given year in that plot
cover[, sr_NA := length(unique(Taxon[present_year0 == FALSE & max_cover>0])), by = .(newplotid, year)]
printNA <- table(cover$sr_NA, cover$site_code)
write.csv(printNA, "~/Dropbox/printNAbysite_droughtnet.csv")
cover.NA.unique = unique(cover[, .(site_code, year,  plot,  trt,  sr_NA)])

##### Lifespan, lifeform, functional group cleaning #########

# table(cover$local_lifespan)
# ANNUAL      BIENNIAL INDETERMINATE          NULL     PERENNIAL           UNK 
# 11574           381          1666          1303         37081            66 

#Create an other group of other.. 
# covert native to NAT 
cover[local_lifespan =="NULL", local_lifespan:="OTHER"]
cover[local_lifespan =="UNK", local_lifespan:="OTHER"]
cover[local_lifespan =="BIENNIAL", local_lifespan:="OTHER"]
cover[local_lifespan =="INDETERMINATE", local_lifespan:="OTHER"]

##local life form cleaning ### 
# #table(cover$local_lifeform)
# 
# BRYOPHYTE    CACTUS  CLUBMOSS      FERN      FORB     FUNGI GRAMINOID     Grass     GRASS    LEGUME 
# 755       422         4         7     25563         1      3372         4     13942      1771 
# LICHEN      MOSS      NULL     SHRUB  SUBSHRUB SUCCULENT      TREE      VINE     WOODY 
# 176       328       950      2635      1503       106       214       204       114

#fix grass and consolidate groups 
cover[local_lifeform =="Grass", local_lifeform := "GRASS"]
cover[local_lifeform =="GRAMINOID", local_lifeform := "GRASS"]
cover[local_lifeform == "CLUBMOSS", local_lifeform := "NONVASCULAR"]
cover[local_lifeform == "FUNGI", local_lifeform := "NONVASCULAR"]
cover[local_lifeform == "MOSS", local_lifeform := "NONVASCULAR"]
cover[local_lifeform == "LICHEN", local_lifeform := "NONVASCULAR"]
cover[local_lifeform == "BRYOPHYTE", local_lifeform := "NONVASCULAR"]
cover[local_lifeform == "VINE" , local_lifeform := "WOODY"]
cover[local_lifeform =="TREE", local_lifeform := "WOODY"]
cover[local_lifeform == "SHRUB", local_lifeform := "WOODY"]
cover[local_lifeform =="SUBSHRUB",local_lifeform := "WOODY"]

## Combine functional groups ## 

table(cover$functional_group)
#combine Null and unk
cover[functional_group == "UNK", functional_group := "NULL"]
cover[functional_group == "GRAMINOID", functional_group := "GRASS"]

#condense woody species and trees all to woody:
cover[functional_group] = ifelse("VINE", "WOODY")
cover[functional_group] = ifelse("TREE", "WOODY")
cover[functional_group] = ifelse("SHRUB", "WOODY")
cover[functional_group] = ifelse("SUBSHRUB", "WOODY")
cover[functional_group == "LICHEN", functional_group := "NONVASCULAR"]
cover[functional_group== "BRYOPHYTE", functional_group:= "NONVASCULAR"]

### now make some variables about the SR of groups/combinations of these groups ##

##invasive - annual or perennial
cover[, sr_annual_INT := length(unique(Taxon[local_lifespan == "ANNUAL" & local_provenance == "INT"])), by = .(newplotid, year)]
cover[, sr_per_INT := length(unique(Taxon[local_lifespan == "PERENNIAL" & local_provenance == "INT"])), by = .(newplotid, year)]

# Native -- annual or perennial
cover[, sr_annual_NAT := length(unique(Taxon[local_lifespan == "ANNUAL" & local_provenance == "NAT"])), by = .(newplotid, year)]
cover[, sr_per_NAT := length(unique(Taxon[local_lifespan == "PERENNIAL" & local_provenance == "NAT"])), by = .(newplotid, year)]

## Annual/perennial forb or grass or woody 
cover[, sr_annual_forb := length(unique(Taxon[local_lifespan == "ANNUAL" & functional_group  == "FORB"])), by = .(newplotid, year)]
cover[, sr_per_forb := length(unique(Taxon[local_lifespan == "PERENNIAL" & functional_group  == "FORB"])), by = .(newplotid, year)]
cover[, sr_annual_grass := length(unique(Taxon[local_lifespan == "ANNUAL" & functional_group  == "GRASS"])), by = .(newplotid, year)]
cover[, sr_per_grass := length(unique(Taxon[local_lifespan == "PERENNIAL" & functional_group == "GRASS"])), by = .(newplotid, year)]

## woody
cover[, sr_woody := length(unique(Taxon[functional_group == "WOODY"])), by = .(newplotid, year)]


####################################################################################################
###  Changes in types of species by plot:  % Cover ########################################
################################################################################################

###INVASIVE VS NATIVE ########


###INVASIVE VS NATIVE ########








###################################################################################################################################################
####### Make Categorical Variables to Label Spp as Dominant, Subordinant, and Rare   - based on the relative abundance Quantiles per Site #########################################
#############################################################################################################################################

#**Note to self ****
#0 quartile = 0 quantile = 0 percentile
# 1 quartile = 0.25 quantile = 25 percentile
# 2 quartile = .5 quantile = 50 percentile (median)
# 3 quartile = .75 quantile = 75 percentile
# 4 quartile = 1 quantile = 100 percentile

unique.ras = unique(cover_present_year0[, .(site_code, Taxon, relative_abundance_spp_site.yr0)])

unique.ras[,RAquant0.6:=quantile(relative_abundance_spp_site.yr0, probs=0.6), by=site_code]
unique.ras[,RAquant0.95:=quantile(relative_abundance_spp_site.yr0, probs=0.95), by=site_code]
unique.ras[,RAsite_group := ifelse(relative_abundance_spp_site.yr0<RAquant0.6,"Rare",
                                   ifelse(relative_abundance_spp_site.yr0<RAquant0.95, "Subordinate","Dominant"))]

#whats the breakdown of species classified in each group overall 
table(unique.ras$RAsite_group)
#whats the breakdown of species classified in each group by site
cut.off1 <- table(unique.ras$site_code, unique.ras$RAsite_group)
write.csv(cut.off1, "cutoff1_species.csv") 

#re-merge the quantiles and classifications into the cover_present_year0 dataset
unique.ras[,relative_abundance_spp_site.yr0:=NULL] # drop before re-merge
cover_present_year0 = merge(cover_present_year0, unique.ras, by=c("site_code", "Taxon"))


