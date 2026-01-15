### ### ### ### ### ### ### ### ### ### ### ### ### 
### Mediation Project : Prep data for analysis ####
# Filtering as of Nov 7 2025 ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### 
#Tim is branching off the work of Laura and Sarah Elizabeth. The main difference is to NOT subset out a bunch of sites


#Close graphics and clear local memory
graphics.off()
rm(list=ls())

### Load Packages
library(data.table)
library(fixest)
library(viridis)
require(ggplot2)
require(knitr)
require(stargazer)
require(tidyr)
require(dplyr)

### Set working directory ### 
setwd("C:/Users/ohler/Dropbox/DroughtMechanisms/")
#path_out <- "~/Dropbox/DroughtMechanisms/data_processed/"

# Load processed cover and anpp data from IDE:
# IDE/data_processed/Cover_ppt_<datecode>.csv 
# raw files are the following and processed in the code on this github
# cover <- fread("data_og/cover_ppt_2024-12-19.csv")
# anpp <- fread("data_og/anpp_ppt_2024-12-16.csv")

cover <- fread("C:/Users/ohler/Dropbox/DroughtMechanisms/data_processed/processed_cover_Nov2025.csv")

length(unique(cover$site_code))


#anpp <- fread("C:/Users/ohler/Dropbox/DroughtMechanisms/data_processed/2_processed_anpp_Nov2025.csv") ## where is the version with tims stuff?
#anpp$V1 = NULL
cover$V1 = NULL
#site level climate and other covariates
# sitecovars <- fread("data_og/Site_Elev-Disturb.csv") # Site characteristics
# Climate data
# climate <- fread("data_og/climate_mean_annual_by_site_v3.csv")
#climate <- fread("C:/Users/ohler/Dropbox/DroughtMechanisms/data_og/Compiled_climate_data_IDE.csv")
#climate$V1 = NULL

nrow(cover) # 116386
#nrow(anpp) #5591 
#nrow(climate) #24520

########################################################################################
### Site Inclusion Criteria: #############################################################
########################################################################################
### Filter the data to:
# only Grassland and Shrubland - done 
# sites with both treatment and control data - done 
# sites that have pre-treatment data  - needs to be checked
# sites with at least X years of data - to decide!******
# Create dataframes for sites with treatments with different max extreme years 

##################################################################################################
### Making a unique plot id and year as factor ####################################################
##################################################################################################
# the unique replicate is site, year, plot, subplot 
#class(cover$plot) # covert to a character
#anpp$plot <- as.character(anpp$plot)
#anpp$subplot <- as.character(anpp$subplot)
#anpp$year <- as.character(anpp$year)
#anpp$block <- as.character(anpp$block)

#in general: my.dt[,newplotid:=as.factor(paste(site.id.col, plot.id.col, sep="_"))]
#anpp[,newplotid:=as.factor(paste(site_code, block, plot, subplot, sep="_"))]
#length(unique(anpp$newplotid)) 
#1287 without filter and 739 with filter
# uhhh not its 1316...

###CHECK FOR DUPLICATES ### 
# looks ok after filtering out forests (prades.es) and making a unique ID that includes block (for Boulder)
#dups = anpp[, .N, by=c("site_code", "plot", "year")][N>1,]
#print(dups)  # Empty data.table (0 rows and 4 cols): site_code,plot,year,N
#table(anpp[,.N, by=c("site_code", "plot", "year")][,N])

#anpp[,.N, by=c("site_code", "newplotid", "year")][N>1,]
# should be -- Empty data.table (0 rows and 4 cols): site_code,newplotid,year,N

#anpp[,.N, by=c("site_code", "newplotid", "n_treat_years")][N>1,]
# should be Empty data.table (0 rows and 4 cols): site_code,newplotid,year_trt,N

#anpp[,.N, by=c("site_code", "plot", "n_treat_years")][N>1,]
#should be Empty data.table (0 rows and 4 cols): site_code,plot,year_trt,N

###############################################################################################
## Create and Apply Filters  ###############################################################
#############################################################################################
# filter to sites with both control and treatment plots
#anpp[,n_trt_status:=length(unique(trt)), by=.(site_code)]
#anpp = anpp[n_trt_status==2,]

# filter to grassland and shrublands
#anpp = anpp[habitat.type != "Forest" ,]
#anpp = anpp[habitat.type != "Forest understory" ,]

#filter to only sites that have pre-treatment data
#Find min value of treatment years; 0 = pre-treatment data
#anpp[,min.trt.yr:= min(n_treat_years), by=.(site_code)]
#anpp[,max.trt.yr:= max(n_treat_years), by=.(site_code)]
#anpp = anpp[min.trt.yr <= 0, ] 

#length(unique(anpp$newplotid)) #733
#nrow(anpp) #3714

### to filter to a certain number of years or obs needed:
#from Meghan Avolio: five sites are not repeated over years, and only have one year of data - 
#oneyr <- anpp %>%
#  dplyr::select(site_code, n_treat_years) %>%
#  unique() %>%
#  group_by(site_code) %>%
#  mutate(max=max(n_treat_years)) %>%
#  filter(max==1)

#length(unique(oneyr$site_code)) #6 sites but #cdpt_drt.us has -1, 0, 0.5 
#print(oneyr$site_code) 

##################################################################################################
### Make some dummy variables for diff-in-diff  #############################################################
##################################################################################################
# Make a variable that identifies if it is a pre-treatment year or not 

##** IS THIS RIGHT or  WRONG?
#anpp[,is.PretreatmentYr := (n_treat_years <= 0),]
# anpp.pt = anpp[is.PretreatmentYr == 1, ] 

#make this also a variable that is a 0 or 1 to indicate 1 (TRUE) if it is a treated year or not for processing the full dataset
# to only sites that also have pre-treatment years. 
#anpp[,is.TreatedYear := (is.PretreatmentYr != TRUE),]

# Create dummy variable for before or not 
#anpp$before <- ifelse(anpp$is.PretreatmentYr == "TRUE", 1, 0)

# Create dummy variable for after *EXPOSED* or not  
#anpp$exp <- ifelse(anpp$is.PretreatmentYr == "TRUE", 0, 1)

# Create dummy variable for trt or not 
#anpp$treat <- ifelse(anpp$trt == "Drought", 1, 0)

#create variable if treated and after 
#anpp$treat_exp = anpp$exp*anpp$treat

###############################################################################################
## Process and Subset cover to the unique plot-level entries #############################
#############################################################################################
#drought.type, extreme

#filter out sites without cover survey
table(cover$cover_survey) 
# no   yes 
# 9297 29522 
#cover = cover[cover_survey == "yes",]
#nrow(cover) #29522

##**this needs to be update to the ones we will want in the final dataset
coversummary = unique(cover[, .(habitat.type, site_code, plot, year, trt, newplotid, n_treat_years, #site and year features 
                                n_treat_days, Domcover, sr_plot, Rarecover, Subordcover,
                                PerForbCover, AnnualGrassCover, PerenGrassCover,
                                C3Cover, C4Cover, CAMCover, AnnualForbCover, WoodyPercentcover.yr,
                                GrassPercentcover.yr, ForbPercentcover.yr, PerenPercentcover.yr,
                                INTcover, Native_cover.yr, AnnualPercentcover.yr)])

# sr_INT, sr_NAT, sr_UNK, sr_INT.site, 
# AnnualGrassCover, PerenGrassCover, WoodyCover, PerForbCover, AnnualForbCover, # combined cover groups
# AnnualPercentcover.yr, PerenPercentcover.yr, GrassPercentcover.yr, ForbPercentcover.yr,
# INTcover, Native_cover.yr, sr_annual_forb, sr_per_forb, sr_annual_grass,
# sr_per_grass, sr_woody, sr_grass, sr_forbs, sr_legume, sr_nonvascular,
# sr_annual_INT, sr_per_INT, sr_annual_NAT, sr_per_NAT)]) # , na.rm = T
nrow(cover) #29522
nrow(coversummary) #3558 
#check if worked 
cover = coversummary 
nrow(cover) #9329 
table(cover$site_code)


cover1 <- subset(cover, n_treat_years <= 0)%>%
          group_by(site_code, n_treat_years)%>%
          dplyr::summarize(Domcover = mean(Domcover), Rarecover = mean(Rarecover), Subordcover = mean(Subordcover),  PerForbCover= mean(PerForbCover),AnnualGrassCover = mean(AnnualGrassCover), PerenGrassCover  = mean(PerenGrassCover), C3Cover= mean(C3Cover), C4Cover= mean(C4Cover),       CAMCover    = mean(CAMCover), AnnualForbCover = mean(AnnualForbCover), WoodyPercentcover = mean(WoodyPercentcover.yr), GrassPercentcover = mean(GrassPercentcover.yr), ForbPercentcover = mean(ForbPercentcover.yr),PerenPercentcover = mean(PerenPercentcover.yr), INTcover = mean(INTcover),  Native_cover = mean(Native_cover.yr), AnnualPercentcover = mean(AnnualPercentcover.yr) )%>%
        group_by(site_code)%>%
  dplyr::summarize(Domcover = mean(Domcover), Rarecover = mean(Rarecover), Subordcover = mean(Subordcover),  PerForbCover= mean(PerForbCover),AnnualGrassCover = mean(AnnualGrassCover), PerenGrassCover  = mean(PerenGrassCover), C3Cover= mean(C3Cover), C4Cover= mean(C4Cover),       CAMCover    = mean(CAMCover), AnnualForbCover = mean(AnnualForbCover), WoodyPercentcover = mean(WoodyPercentcover), GrassPercentcover = mean(GrassPercentcover), ForbPercentcover = mean(ForbPercentcover),PerenPercentcover = mean(PerenPercentcover), INTcover = mean(INTcover),  Native_cover = mean(Native_cover), AnnualPercentcover = mean(AnnualPercentcover) )


write.csv(cover1, "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/data/comm_moderators.csv")











###############################################################################################
## Merge in composition variables and mediators #############################
#############################################################################################
cover$year <- as.character(cover$year)
anpp$year <- as.character(anpp$year)
data = merge(anpp, cover, by = c("newplotid", "site_code", "trt", "year"), allow.cartesian = TRUE)
nrow(data) #now this is 2417? this all needs to be checked.

# extreme_years_anyorder is a site by year variable so for each treatment year it is the number or prior extreme years in any order.
# max_extreme_years_out_of_four is a site attribute so each site only gets a single value.
# So for a given value in each column, there will always be more sites in extreme_years_anyorder 
#since max_extreme_years_out_of_four is max(extreme_years_anyorder) by site
# There are only 8 sites with max_extreme_years_out_of_four >= 4 since that requires four years of treatments and each of them extreme

## data checks
table(data$live) # should all be 1
table(data$habitat.type.y) #include grassland and shrubland remove forests

# Filter to: sites with cover data, sites with pre-treatment data
# data = data[cover_survey == "yes",]
# table(data$cover_survey) # need to remove the sites without cover_survey == yes I assume

# Check its with control and treatment
table(data$n_trt_status) # this should be 2

# certain number of years too?

# check the sites in here have a pre-treatment year

nrow(data) #this ends up not being many observations in the end!

###############################################################################################
## Merge in climate variables and moderators #############################
#############################################################################################

############################################################################
#### Separate out types of drought: Extreme and Nominal  ###########################################################
#####################################################################################
#Nominal vs extreme  as a categorical variable based on Smith et al (2024) PNAS
drought_type_df <- anpp %>%
  filter(trt == "Control") %>%
  dplyr::select(c(site_code, year, ppt.1, map)) %>%
  distinct() %>%
  mutate(drought.type = ifelse(ppt.1 > map, "nominal", "extreme")) %>%
  dplyr::select(-c(ppt.1, map))

## create a single row for newplotid and year (vs a row per Taxon!)
anpp <- anpp %>%
  left_join(drought_type_df)

# Create extreme or not as a dummy variable 
extreme_df <- anpp %>%
  filter(trt == "Control") %>%
  dplyr::select(c(site_code, year, ppt.1, map)) %>%
  distinct() %>%
  mutate(extreme = ifelse(ppt.1 > map, "0", "1")) %>%
  dplyr::select(-c(ppt.1, map))

## create a single row for newplotid and year (vs a row per Taxon!)
anpp <- anpp %>%
  left_join(extreme_df)

#check it worked
head(anpp)

#****CAN I NOW CUT THIS ***?? 
#Count # of drought.type years 
# anpp = anpp[, n_extremes:= n(drought.type == "extreme"), .by=(site_code)]
# anpp$extreme <- as.integer(anpp$extreme)
# anpp = anpp[, n_extremes3 := .N, by = c("extreme", "site_code")]

###############################################################################################
## Create different extreme treatment definitions and datasubsets ##################################
#############################################################################################
# ppt.1  - rainfall 365 days before treatment, plot level 
# ppt.2 - rainfall 365 days before ppt.1
# ppt.3 - rainfall 365 days before ppt.2 
# map site report mean annual precip

##### View treatment categories Tim made #####
# column 'percent_extreme' = is the proportion of experiment years that were extreme (actually a decimal). Control plots always have '0' 
# but they are probably actually a little different than drought treatment plots that only received nominal drought which will also have a value of 0.
# column 'consecutive_extreme' is the number of consecutive extreme years leading up to the sampling date. Controls are marked 'Control' in this column.
table(anpp$consecutive_extreme)
table(anpp$extreme_years_anyorder) ## accumulating 
table(anpp$max_extreme_years_out_of_four) # will need to be updated each data update

##*** I DONT THINK THIS WORKED BECAUSE ALL THE SAME LENGTH!*****
# anpp = anpp[percent_extreme <= .25, ] 
# anpp.extreme.2 = anpp[extreme_years_anyorder <= 2,] 
# length(unique(anpp.extreme.2$site_code)) #69 
# anpp.extreme.1 = anpp[extreme_years_anyorder <= 1,] 
# length(unique(anpp.extreme.1$site_code))
# 
# anpp.extreme.2b = anpp[max_extreme_years_out_of_four <= 4,] 
# length(unique(anpp.extreme.2b$site_code)) #48 w/ 2; 64 with >=3, 69 <= 4

# most conservative - extremes >3 years
# four <- "4"
# at_least_3 <- c("3", "4")
# at_least_2 <- c("2", "3", "4")
# at_least_1 <- c("1", "2", "3", "4")
# anpp3 <- subset(anpp,extreme_years_anyorder %in% at_least_3 )
# anpp2 <- subset(anpp,extreme_years_anyorder %in% at_least_2 )
# anpp1 <- subset(anpp,extreme_years_anyorder %in% at_least_1 )
# anpp4 <- subset(anpp,extreme_years_anyorder %in% four)

# ## max extreme years *any order*
# # most conservative - extremes >3 years but sample size probably need the 2
# #* to do power analysis
# four <- "4"
# at_least_3 <- c("3", "4")
# at_least_2 <- c("2", "3", "4")
# at_least_1 <- c("1", "2", "3", "4")
# anpp3 <- subset(anpp,max_extreme_years_out_of_four %in% at_least_3 )
# anpp2 <- subset(anpp,max_extreme_years_out_of_four %in% at_least_2 )
# anpp1 <- subset(anpp, max_extreme_years_out_of_four %in% at_least_1 )
# anpp4 <- subset(anpp, max_extreme_years_out_of_four %in% four)

# nrow(anpp4) #624 
# nrow(anpp3) #1456 
# nrow(anpp2) #2289
# nrow(anpp1) #3159

###############################################################################################
## Compute Continue Drought Severity  ######################################################
#############################################################################################
## Drought Severity as a continuous measure instead of the binary control/nominal

#*** GET FROM TIM *****



###############################################################################################
### Write Out Prepped Data for Analyses  ######################################################
#############################################################################################
# Data output directory
path_out <- "~/Dropbox/DroughtMechanisms/data_processed/"
# write out processed data with current month & year
current_date <- format(Sys.Date(), "%b%Y")
write.csv(data, file.path(path_out, paste0('PreppedForDataAnalysis_', current_date, '.csv')))

#decide if we want to write out seperate datasets for each extreme sequence and cutoff
# or do this in the analysis file (and make their own files for analysis)

## See which sites and years & write out the site and year list as a table: 
tab =  table(data$site_code, data$year)
write.csv(tab, "Site_Year_ObservationsTable.csv")