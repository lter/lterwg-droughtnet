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
View(cover[which(cover$n_treat_years== "-6" ),]) # sgsdrt.us

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
View(cover[which(cover$local_provenance == "UNK"),]) # the species of unknown origins are across different sites.

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
View(cover[which(cover$subplot == "B" ),]) #rhijn.nl and  llara.au 
View(cover[which(cover$subplot == "C" ),]) #rhijn.nl and  llara.au 
View(cover[which(cover$subplot == "D" ),]) #rhijn.nl and  llara.au  
View(cover[which(cover$subplot == "M" ),]) #cedartrait.us
View(cover[which(cover$subplot == "N" ),]) #cedartrait.us
View(cover[which(cover$subplot == "S" ),]) #cedartrait.us

## check out block situation
table(cover$block) 

############################################################  
## Compute Species Richness and Evenness ##################  
############################################################  
# Species Richness
cover[, sr_plot := length(unique(Taxon[max_cover>0])), by = .(plot, site_code, year)]
cover[, sr_site := length(unique(Taxon[max_cover>0])), by = .(site_code, year)]

ggplot(data = cover, aes(x=year, y=sr_site)) +
  +     geom_line(aes(group=site_code))

# Site-level richness ALL years
cover[, SR.site.allyears := length(unique(cover[,.(site_code, Taxon)]))]

# Evenness - HENRY


# REORDERING***


# changes in types of species by plot
local_lifespan

############################################################################################################
### Prep data and species list  #########################################################################
#############################################################################################################
## First, only the species PRESENT in a plot are recorded in the cover data, so species that are present at a site but not a plot 
# are *not* listed (i.e.  with max_cover = 0). We need to fix that before computing average relative abundance at a site.

# First, we compute list of all species at a site over the time period in the data:
sp.at.site = unique(cover[,.(site_code, Taxon)])

# Want to create and merge one record per (Taxon, site_code, plot, year), using merge to flag records that weren't present 
# in original data.
site.plot.year.combos = unique(cover[,.(site_code, plot, year)])
expanded.spp.recs = merge(site.plot.year.combos, sp.at.site, by=c("site_code"), allow.cartesian = T)
cover = merge(cover, expanded.spp.recs, by=c("site_code", "plot", "year", "Taxon"), all.y=T)

# need to update n_treat_years, trt, live, local_provenance for the records that got added
cover[,live:=1]
cover[,trt:=min(trt[!is.na(trt)], na.rm=T), by=.(plot, site_code, year)]
cover[,local_provenance:=min(local_provenance[!is.na(local_provenance)]),by=.(site_code, Taxon)]
cover[,n_treat_years:=min(n_treat_years[!is.na(n_treat_years)], na.rm=T), by=.(plot, site_code, year)]

##########################################################################################################
##### Compute TOTAL LIVE RELATIVE COVER PER PLOT MEASURES ########################################
##########################################################################################################
# make a total cover in a plot, site, year. This includes live cover only.
cover[,totplotcover.yr.live := sum(max_cover, na.rm= T), by=.(plot, site_code, year)]

#Make a relative cover for each species in each plot and year
# based on TOTAL cover (including only live cover as we already filtered to the data table to live above).
cover[,relative_sp_cover.yr.live := max_cover/totplotcover.yr.live]
#***to check, run: sum(is.na(cover$relative_sp_cover.yr.live))


# now compute #s we care about, including max_cover,sr_INT, sr_NAT, sr_UNK, totplotcover.yr.live, relative_sp_cover.yr.live, 
# tot.num.plots, tot.num.plots.with.spp, rel_freq.space
cover[is.na(max_cover), max_cover:=0] # if NA, species wasn't there in that plot and year, so cover should be zero

# Compute native, non-native, and unknown origin species richness by plot, site, year. Note filter to max_cover > 0 to 
# consider only species that were actually present.
cover[, sr_INT := length(unique(Taxon[local_provenance=="INT" & max_cover>0])), by = .(plot, site_code, year)]
cover[, sr_NAT := length(unique(Taxon[local_provenance=="NAT" & max_cover>0])), by = .(plot, site_code, year)]
cover[, sr_UNK := length(unique(Taxon[local_provenance=="UNK" & max_cover>0])), by = .(plot, site_code, year)]

## to make a plot of number of introduced species over time at a site 
cover[, sr_INT.site := length(unique(Taxon[local_provenance == "INT" & max_cover>0])), by = .(site_code, year)]


#####
##### Compute a convenience column that says whether a species was present in a plot in a site in the pre-treatment year
cover[,present_year0:=max_cover[year_trt==0]>0, by=.(Taxon,site_code,plot)]

#compute the SR of species that were not present in year 0 for each plot and year but are present in a given year in that plot
cover[, sr_NA := length(unique(Taxon[present_year0 == FALSE & max_cover>0])), by = .(plot, site_code, year)]

#*** ultimately cut btwn these lines to clean code*****
printNA <- table(cover$sr_NA, cover$site_code)
# summary(cover$sr_NA.test)
write.csv(printNA, "printNAbysite.csv")

cover.NA.unique = unique(cover[, .(site_code, year,  site_name,  plot,  year_trt , trt,  sr_NA)])


##########################################################################################################
##### Compute TOTAL & TOTAL LIVE RELATIVE COVER PER PLOT MEASURES ########################################
##########################################################################################################
# make a total cover in a plot, site, year. This includes live cover only.
cover[,totplotcover.yr.live := sum(max_cover, na.rm= T), by=.(plot, site_code, year)]

#Make anrelative cover for each species in each plot and year
# based on TOTAL cover (including only live cover as we already filtered to the data table to live above).
cover[,relative_sp_cover.yr.live := max_cover/totplotcover.yr.live]
#***to check, run: sum(is.na(cover$relative_sp_cover.yr.live))

# make a site-level relative abundance for each species and year. This requires
#first summing the total cover per site and year and the total cover of each species in all plots at site 
cover[, totsitecover.yr := sum(totplotcover.yr.live, na.rm= T), by=.(site_code, year)]
cover[, tot_maxcover_site.yr  := sum(max_cover, na.rm= T), by=.(Taxon, site_code, year)]

#**need to set to 0 if cover$tot_maxcover_site.yr or totsitecover.yris NA....
cover[is.na(tot_maxcover_site.yr),tot_maxcover_site.yr := 0]
cover[is.na(totsitecover.yr),totsitecover.yr  := 0] #not necessary

#then divide these two numbers: 
cover[, relative_abundance_spp_site.yr  :=  tot_maxcover_site.yr/totsitecover.yr, by=.(Taxon, site_code, year)]

#create a variable for just year 0 
cover[, relative_abundance_spp_site.yr0 := min(relative_abundance_spp_site.yr[year_trt==0]), by=.(Taxon, site_code)]
cover[is.infinite(relative_abundance_spp_site.yr0),relative_abundance_spp_site.yr0 := NA]

# if the species isn't present at a site in year_trt == 0, give the species a relative abundance of 0 in that year:

### Next step --create a relative frequency in year 0 variable #####
#total # of plots within a site, for pre-treatment year:
# again we use the pre-treatment year because we calculate the metrics at the site level and want to avoid classifying species post treatment
cover[, tot.num.plots := length(unique(plot[n_treat_years == 0])), by =.(site_code)]  #this will work because no records of max_cover = 0.

#number of plots within a site, in the pre-treatment year, that a species occurred in:
cover[, tot.num.plots.with.spp := length(unique(plot[n_treat_years== 0 & max_cover>0])), by =.(site_code, Taxon)]

##Compute Relative Frequency in year 0.
## Relative frequency = number of plots at a site in year 0 a species occurred / total number of plots at a site in year 0" 
# If a site has no records for plots in a pre-treatment year (year_trt==0), rel_freq.space will be NA.
# That's fine -- these sites will be filtered out later
cover[, rel_freq.space :=  tot.num.plots.with.spp/tot.num.plots]
cover[is.na(rel_freq.space),rel_freq.space  := 0]

#################################################################################################################################
## Run this Code (and all following code)  if you want to Filter data to only species present in year 0 and save that dataset  #####
#################################################################################################################################
cover_present_year0 = cover[present_year0 == TRUE,]
# write.csv(cover_present_year0, "cover_present_year0May142021.csv")

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


