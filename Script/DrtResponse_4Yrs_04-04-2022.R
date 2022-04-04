#==========================#
#  IDE MANUSCRIPT ANALYSIS #
#         & FIGURES        #
#         APRIL 2022       #
#        1-4YRS DROUGHT    #
#==========================#
#CODE WHERE WE MERGE GRASSLANDS AND SHRUBLANDS FOR LMER ANALYSIS AND FIG 3
#THIS CODE RELIES ON CODE FROM THE FOLLOWING R SCRIPTS (run these before running script below):
#R_package_load.R (LOADING PACKAGES)
source("C:\\Users\\Kwilks86\\Dropbox\\IDE MS_Single year extreme\\Code\\R_package_load.R")

#anpp_data_cleaning_Lau.R (CLEANING FULL BIOMASS DATA FROM SQL)
source("C:\\Users\\Kwilks86\\Dropbox\\IDE MS_Single year extreme\\Code\\anpp_data_cleaning_Lau.R")

#FunctionalGroup_Proportions.R (CLEANING FULL BIOMASS DATA & CALCULATING AVE GRAMINOID PROP)
#source("C:\\Users\\Kwilks86\\Dropbox\\IDE MS_Single year extreme\\Code\\FunctionalGroup_Proportions.R")

#Read in site_sand_from_soilgrid.csv for sand data

#file.choose()
#LOAD PACKAGES

# Load data exported from SQL for full biomass merged with survey results 
# All sites kept in merge, even for sites with no survey
full_biomass<-read.csv("C:\\Users\\Kwilks86\\Dropbox\\IDE_3-4Yrs_Drt\\Data\\Full_Biomass-SurveyResults_03-28-2022.csv")
site_code<-as.data.frame(unique(full_biomass$site_code))
#write.csv(site_code,"C:\\Users\\katew\\Dropbox\\IDE MS_Single year extreme\\Data\\sitecode-check.csv",row.names=FALSE)
#setdiff(full_biomass$site_code,anpp_data.frame$site_code)

# Data input --------------------------------------------------------------
# Load site information
site_info<-read.csv("C:\\Users\\Kwilks86\\Dropbox\\IDE MS_Single year extreme\\Data\\Site_Elev-Disturb.csv")
unique(site_info$habitat.type)

# Load precipitation data
ppt_data<-read.csv("C:\\Users\\Kwilks86\\Dropbox\\IDE MS_Single year extreme\\Data\\precip\\precip_by_trmt_year_with_percentiles_2021-05-12.csv")
head(ppt_data)
latlon <- site_info[,c('site_code','longitud','latitud')]

###remove duplicate sites for now
latlon <- latlon[!duplicated(latlon$site_code),] #0.05 subtracted from latitude for lygra sites because they were placed in a fjord with no world clim data

#Get WorldClim MAP (Mean Annual Precip)------------------------------------
  
for(i in c('01','02','03','04','05','06','07','08','09','10','11','12')){
tmp = raster(paste("C:\\Users\\Kwilks86\\Dropbox\\IDE MS_Single year extreme\\Data\\precip\\wc2.1_30s_prec/wc2.1_30s_prec_",i,'.tif',sep=''))
assign(paste('p',i,sep=''),tmp)
}

latlon$Jan <- raster::extract(p01,latlon[,2:3])
latlon$Feb <- raster::extract(p02,latlon[,2:3])
latlon$Mar <- raster::extract(p03,latlon[,2:3])
latlon$Apr <- raster::extract(p04,latlon[,2:3])
latlon$May <- raster::extract(p05,latlon[,2:3])
latlon$Jun <- raster::extract(p06,latlon[,2:3])
latlon$Jul <- raster::extract(p07,latlon[,2:3])
latlon$Aug <- raster::extract(p08,latlon[,2:3])
latlon$Sep <- raster::extract(p09,latlon[,2:3])
latlon$Oct <- raster::extract(p10,latlon[,2:3])
latlon$Nov <- raster::extract(p11,latlon[,2:3])
latlon$Dec <- raster::extract(p12,latlon[,2:3])

latlon$wc_map <- rowSums(latlon[,4:15])

site_info<-merge(site_info,latlon[,c("site_code","wc_map")])
#Create a data.frame specific for the Magnitude-Duration paper
length(site_info$site_code) #136

# Merge anpp post-processed data and full biomass only for columns needed
# Also remove duplicates for plot and year, but keeping max n_treat_days for sites with more than one sampling date in a year
# These multiple sampling dates already added in anpp cleaning code
full_biomass$n_treat_days<-as.numeric(as.character(full_biomass$n_treat_days))
length(unique(full_biomass$site_code)) #134 sites (one site is NPKD- paikenpkd)

#MAYBE NEED TO FIGURE THIS OUT
#Take max biomass date by year since some sites may have collected biomass across 
#Two sampling periods (e.g. bayreuth)
#full_biomass1<-full_biomass%>%group_by("site_code","year")
  
  
#ALL SITES DATAFRAME (n=134)-----------------------------------------------------
#Creating a data.all object to preserve all sites for Figure 1 (B. and C.)
data.all<-merge(anpp_data.frame,full_biomass[,c("site_code","block","plot","subplot","biomass_date","year","n_treat_days","trt")],all=FALSE)
length(unique(data.all$site_code)) #134
setdiff(full_biomass$site_code,data.all$site_code) #Looking for differences in site_codes between two dataframes

#Removing duplicates
data.all<-data.all %>%
  dplyr::group_by(site_code,block,plot,subplot,year,mass,mass_category,trt) %>%
  dplyr::summarize(n_treat_days = max(n_treat_days))
data.all<-merge(data.all,site_info[,c("site_code","habitat.type","precip","temp","arid","wc_map")])
head(site_info)
unique(data.all$habitat.type)
data.all$trt[data.all$trt=="Control_Infrastructure"]<-"Control"
length(unique(data.all$site_code)) #134
#write.csv(data.all,"C:\\Users\\katew\\Dropbox\\IDE MS_Single year extreme\\Data\\anpp_trt_06-22-2021.csv",row.names=FALSE)

#REMOVING FOREST SITES (Will leave this data to Heidi)
data.noforest<-data.all %>%
  dplyr::filter(!habitat.type %in% c('Forest','Forest understory'))
length(unique(data.noforest$site_code)) #118 (16 sites removed)
setdiff(data.all$site_code,data.noforest$site_code)
#Prades didn't follow protocols, so shouldn't be counted with Forests
# [1] "bamboo.cn"    "bivensarm.us" "cmss.us"      "elizwood.us"  "gigante.pa"   "horizon.cr"  
#[7] "hubbard.us"   "jilpanger.au" "kranz.de"     "p12.pa"       "p13.pa"       "prades.es"   
#[13] "sevforest.us" "sherman.pa"   "thompson.us"  "wayqecha.pe" 
#*NOTE: Prades didn't follow droughtnet protocols, but also happens to be a forest site
#write.csv(data.all,"C:\\Users\\katew\\Dropbox\\IDE MS_Single year extreme\\Data\\anpp_clean_trt_07-23-2020.csv",row.names=FALSE)

#Removing sites here that DO NOT report ANPP or are outside of ANPP range for biome listed in Fahey & Knapp 2007:
data.anpp<-data.noforest[which(data.noforest$site_code!="lcnorth.cl" #Doesn't report ANPP
                               &data.noforest$site_code!="lcsouth.cl" #Doesn't report ANPP
                               &data.noforest$site_code!="qdtnorth.cl" #Doesn't report ANPP
                               &data.noforest$site_code!="qdtsouth.cl" #Doesn't report ANPP
                               &data.noforest$site_code!="neudamm.na" #Doesn't report ANPP, also no weather info
                               &data.noforest$site_code!="ebro.es" #ANPP outside range for biome
                               &data.noforest$site_code!="garraf.es" #Did not follow protocols: Not using for drought plots, only using control plots for this site
                               &data.noforest$site_code!="brandjberg.dk" #Did not follow protocols: Not using for drought plots, only using control plots for this site
                               &data.noforest$site_code!="ethadb.au" #ANPP outside range for biome
                               &data.noforest$site_code!="ethadn.au"
                               &data.noforest$site_code!="swift.ca" #did not follow IDE protocols (drought plots did not exclude water- see Jillian email)
                               &data.noforest$site_code!="plattev.us"),] #Cannot confirm low biomass in one plot (filed under not following protocols)

length(unique(data.anpp$site_code)) #106
#setdiff(full_biomass$site_code,data.noforest$site_code)
setdiff(full_biomass$site_code,data.anpp$site_code) #28 Excluded (4 of these 30 didn't follow protocols)

#When year is continuous


#Only using sites with >= 2 reps for drought and >=1 rep for control
#Sites with 
#Making an exception for charleville.au since they have two drought trts and one control
#From Mendy (11-08-2021):
#Thanks for the reminder - I think that the site should have 
#at least 2 or more drought plots but could have 1 or more 
#control plots. Given we are trying to see a drought effect, 
#if the site has only one drought plot I would not trust the 
#estimate of the treatment effect.
uniq.plot<- data.anpp %>% 
  dplyr::filter(trt %in% c("Drought","Control"))%>%
  dplyr::select(site_code,year,plot,trt)%>%
  dplyr::distinct(site_code,year,plot,trt)%>%
  dplyr::as_tibble()

Plot.trt.ct <- uniq.plot%>% dplyr::group_by(site_code,trt,year) %>% dplyr::summarise(Plot.count=n())

#Making to wide format to see which sites do not have both treatments in a year
Plottrt_wide<-spread(Plot.trt.ct,trt,Plot.count)
Plottrt_wide[is.na(Plottrt_wide)] <- 0

#Remove sites and years that don't have both control and drought plots
#OR that only have one rep of drought
#brokenh.au in 2018 and 2019 (0 drought plots)
#chacra.ar in 2016 (1 control plot) #Keeping
#chilcasdrt.ar in 2016 and 2020 (0 drought plots)
#charleville.au in 2017 (1 control plot) #Keeping
#charleville.au in 2019 (1 drought plot)
#cobar.au in 2018 (1 control plot) #keeping

Plottrt_wide1<-Plottrt_wide%>%
  dplyr::filter(Drought>=2 & Control>=1)%>%
  dplyr::as_tibble()

#Switch back to long format for merge
Plot.trt.ct2<-gather(Plottrt_wide1,trt,Plot.count,Drought:Control)

#Merge unique trt count back with data frame to filter
data.anpp1<-merge(data.anpp,Plot.trt.ct2,by=c("site_code","trt","year"))

#Year one will now just filter the lower end since many sites had 2nd year 
#caught in 113-657
#Early cutoff is within a week of 120 days
data.anpp2 <- data.anpp1%>%
  dplyr::group_by(site_code)%>%
  dplyr::filter(n_treat_days >= 113 & n_treat_days <= 657)%>%
  dplyr::as_tibble()

#Now checking to see which sites have different n_treat_days 
#for different plots in the same year and making a different column to change those to be the within the same trt year
uniq.trtdays<- data.anpp2 %>% 
  dplyr::select(site_code,year,n_treat_days,plot,trt)%>%
  dplyr::distinct(site_code,year,n_treat_days)%>%
  dplyr::as_tibble()

#Calculate difference in days across site_code to filter out multiple
#observations within the 113-657 day time frame
uniq.trtdays2<-uniq.trtdays%>%
  dplyr::group_by(site_code) %>%
  dplyr::mutate(day_diff = n_treat_days - lag(n_treat_days, default = n_treat_days[1]))%>%
  dplyr::as_tibble()

#Australian sites have early and late season sampling that differs for some plots
#by ~100 days, but keeping these- currently not filtered by code below
#The filter below takes care of most sites with multiple years within 113-657,
#except syferkuil.za 
uniq.trtdays3<-uniq.trtdays2%>%
  dplyr::filter(day_diff<100)%>%
  dplyr::as_tibble()

#Remove second instance of syferkuil
uniq.trtdays4<-uniq.trtdays3%>%
  dplyr::filter(!(site_code=='syferkuil.za' & year=="2017"))%>%
  dplyr::as_tibble()

#Check distinct site codes and years
#check2<-uniq.trtdays4%>% dplyr::distinct(site_code,year) #96 observations, which is our final site count

#Merge back with anpp data frame
datayr1<-merge(data.anpp2,uniq.trtdays4,by=c("site_code","year","n_treat_days")) %>% 
  dplyr::as_tibble()

datayr1$n_treat_years<-1 #Label this as year one

datayr1b<-datayr1%>%
  dplyr::select(-c(day_diff))%>%
  dplyr::as_tibble()

#Merge back with dataframe that has other years
data.anpp3 <- data.anpp1%>%
  dplyr::group_by(site_code)%>%
  dplyr::filter(n_treat_days > 657)%>%
  dplyr::as_tibble()

#Select out unique values
uniq.trtdays5<- data.anpp3 %>% 
  dplyr::select(site_code,year,plot,trt)%>%
  dplyr::distinct(site_code,year)%>%
  dplyr::as_tibble()

#Counting number of years
uniq.trtyrs <- uniq.trtdays5%>%                        
  dplyr::group_by(site_code) %>%
  dplyr::mutate(n_treat_years = (row_number()+1)) %>% #adding one since year 1 in diff dataframe
  dplyr::as_tibble()

#Merge back with anpp data frame
datayr2.4<-merge(data.anpp3,uniq.trtyrs,by=c("site_code","year")) %>% 
  dplyr::as_tibble()

#Rbind year 1 and years 2-4 together
datayr1.4<-rbind(datayr1b,datayr2.4)

#Now subset out drought and control to calculate all the metrics
yr_drt_anpp.all<-datayr1.4 %>%
  dplyr::filter(trt=="Drought")%>%
  dplyr::group_by(site_code,year,plot,habitat.type,n_treat_years,wc_map)%>%
  dplyr::summarize(mass=mean(mass))
length(unique(yr_drt_anpp.all$site_code)) #99

control_means.all<-datayr1.4%>%
  dplyr::filter(trt=="Control")%>% 
  dplyr::group_by(site_code,year,habitat.type,n_treat_years,wc_map)%>%
  dplyr::summarize(mean_control_mass=mean(mass))%>%
  as_tibble()
length(unique(control_means.all$site_code)) #99

data.allb<-data.frame(merge(yr_drt_anpp.all,control_means.all[,c("site_code","year","mean_control_mass","habitat.type","n_treat_years","wc_map")]))
length(unique(data.allb$site_code)) #99 sites
unique(data.allb$habitat.type)

as.data.frame(unique(data.allb$site_code))

#Count number of sites within each year setting
sites.year<-data.allb %>%                    
  dplyr::group_by(n_treat_years) %>%          
  dplyr::summarise(Unique = n_distinct(site_code)) 
sites.year

#For new look at data with ANPP averaged by treatment
condrt_anpp.all<-datayr1.4%>%
  dplyr::filter(trt %in% c("Control","Drought"))%>% 
  dplyr::group_by(site_code,year,trt,habitat.type,n_treat_years,wc_map)%>%
  dplyr::summarize(mass=mean(mass))%>%
  dplyr::as_tibble()
length(unique(condrt_anpp.all$site_code)) #99

#AVERAGE ANPP by drought and control plots

#Concatenate site_code with wc_map
datayr1.4$site_codeMAP <- paste(datayr1.4$site_code, datayr1.4$wc_map)

#Mean ANPP with CIs
condrt_anpp.all2<-datayr1.4%>%
  dplyr::filter(trt %in% c("Control","Drought"))%>% 
  dplyr::group_by(site_code,site_codeMAP,trt,habitat.type,n_treat_years,wc_map)%>%  
  dplyr::summarize(ave.mass=mean(mass), n = n(),sd = sd(mass,na.rm=T), se = sd/sqrt(n),
                   LowerCI = ave.mass - qt(1 - (0.05 / 2), n - 1) * se,
                   UpperCI = ave.mass + qt(1 - (0.05 / 2), n - 1) * se)%>%
  as_tibble()

#Mean no CIs
condrt_anpp.all3<-condrt_anpp.all %>%
  dplyr::filter(trt %in% c("Control","Drought"))%>% 
  dplyr::group_by(site_code,site_codeMAP,trt,habitat.type,n_treat_years,wc_map)%>%
  dplyr::summarize(ave.mass=mean(mass))%>%
  dplyr::as_tibble()
length(unique(condrt_anpp.all2$site_codeMAP)) #99
str(condrt_anpp.all2)

site_codes<-as.data.frame(unique(condrt_anpp.all2$site_code))

#condrt_anpp.all2$n_treat_years<-as.factor(condrt_anpp.all2$n_treat_years)

#write.csv(site_codes,"C:\\Users\\Kwilks86\\Dropbox\\IDE_3-4Yrs_Drt\\Data\\site_codes100.csv",row.names=FALSE)

file.choose()
setwd("C:\\Users\\Kwilks86\\Dropbox\\IDE Meeting_April2022\\Figures\\ANPP_check")

#ALL 99 sites
uniq_siteMAP = unique(condrt_anpp.all2$site_codeMAP)

for (i in uniq_siteMAP) {
  
  temp_plot = ggplot(data= subset(condrt_anpp.all2, site_codeMAP == i)) + 
    #geom_errorbar(aes(x=mean_DS3,y=n_treat_years,xmin=LowerCI, xmax=UpperCI), width=.1) +
    geom_point(size=4, aes(x=n_treat_years,y=ave.mass,fill=trt,shape=trt)) +
    geom_errorbar(aes(x=n_treat_years,y=ave.mass,ymin = LowerCI, ymax = UpperCI,color=trt),width=0.05) + 
    geom_line(aes(x=n_treat_years,y=ave.mass,color=trt))+
    scale_fill_manual(name="Treatment",labels = c('Control','Drought'),values=c("darkorange3","black"))+
    scale_color_manual(name="Treatment",labels = c('Control','Drought'),values=c("darkorange3","black"))+
    scale_shape_manual(name="Treatment",labels = c('Control','Drought'),values=c(21,24))+
    labs(x = "Drought Years", y="Average ANPP")+
    #geom_vline(xintercept=0)+
    ggtitle(i)+
    theme(legend.position="top",axis.line.x = element_line(colour ="black", size = 0.5),
          axis.line.y = element_line(colour ="black", size = 0.5),
          axis.text.x = element_text(color="black",size=12,family="Calibri"),
          axis.text.y = element_text(color="black",size=12,family="Calibri"),
          axis.title.x = element_text(color="black",size=12,family="Calibri"),
          axis.title.y = element_text(color="black",size=12,family="Calibri"),
          legend.text = element_text(color="black",size=12,family="Calibri"),
          legend.title = element_text(color="black",size=12,family="Calibri"),
          plot.background = element_blank())+ scale_x_continuous(breaks = condrt_anpp.all2$n_treat_years)
  ggsave(temp_plot, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}


#TEST OUT AR1 CORRELATION IN NLME PACKAGE==================
#DROUGHT RESPONSE VS YEAR AS FACTOR
#Calculating drought response (DS3)
data.allb$DS3<-log(data.allb$mass/data.allb$mean_control_mass)
data.allb$year<-as.factor(as.character((data.allb$year)))
data.allb$n_treat_years<-as.factor(as.character((data.allb$n_treat_years)))

#Remove year 5 for sites since so few
data.allc<-data.allb%>%dplyr::filter(!n_treat_years %in% c("5"))

#Remove outlier year for hyide
data.allc<-data.allc%>%dplyr::filter(!(site_code %in% c('hyide.de')& n_treat_years %in% c(3)))%>%
  dplyr::as_tibble() 

#Regular model with year as a factor, wc_map as continuous, and site as a random effects
Model1<-lme(DS3~n_treat_years,random=~1|site_code,data=data.allc)
summary(Model1) #AIC= 2477.426
r.squaredGLMM(Model1)
anova(Model1,type="marginal")
emout1<-emmeans(Model1,~n_treat_years)
pairs(emout1) #pairwise comparisons between years

#Model with AR1 correlation structure and year as a factor, wc_map as continuous, and site as a random effects
Model2<-lme(DS3~n_treat_years,random=~1|site_code, correlation= corAR1(form=~1|site_code),data=data.allc)
summary(Model2) #AIC=2473.775
anova(Model2)
r.squaredGLMM(Model2)
emout2<-emmeans(Model2,~n_treat_years)
pairs(emout2)

#N_treat_days (continuous) as explanatory variable with random slopes for each site===================
#Changing bfl and syferkuil n_treat_days to highest number so nothing is lost in 
#the merge of drought with control data

#Filter out 0 days of drought and pre-treatment
#Also, one outlier site (stubai) with ~3000 trt days
data.anpp1b<-data.anpp1%>%dplyr::filter(n_treat_days>0 & n_treat_days<2973)%>% 
  dplyr::as_tibble()

data.anpp1b <- within(data.anpp1b, n_treat_days[n_treat_days %in% c(344,345,346,347,348) & site_code == 'bfl.us'] <- 349)
data.anpp1b <- within(data.anpp1b, n_treat_days[n_treat_days %in% c(186,188,194,196,207) & site_code == 'syferkuil.za'] <- 211)
data.anpp1b <- within(data.anpp1b, n_treat_days[n_treat_days %in% c(566,573,578,581,588) & site_code == 'syferkuil.za'] <- 589)
data.anpp1b <- within(data.anpp1b, n_treat_days[n_treat_days %in% c(950,953,963,973,986) & site_code == 'syferkuil.za'] <- 988)
data.anpp1b <- within(data.anpp1b, n_treat_days[n_treat_days %in% c(1334,1335,1336,1337) & site_code == 'syferkuil.za'] <- 1339)
data.anpp1b<- within(data.anpp1b, n_treat_days[n_treat_days %in% c(1696,1698,1701,1712) & site_code == 'syferkuil.za'] <- 1716)
data.anpp1b<- within(data.anpp1b, n_treat_days[n_treat_days %in% c(73) & site_code == 'haver.no'] <- 90)


Yr.ct <- data.anpp1b%>%      
  dplyr::distinct(site_code,year)%>%
  dplyr::group_by(site_code) %>%
  dplyr::mutate(n_treat_years = (row_number())) %>% 
  dplyr::group_by(n_treat_years) %>%          
  dplyr::summarise(Unique = n_distinct(site_code))%>% 
  dplyr::as_tibble()

sites.52 <- data.anpp1b%>%      
  dplyr::distinct(site_code,year)%>%
  dplyr::group_by(site_code) %>%
  dplyr::mutate(n_treat_years = (row_number())) %>% 
  dplyr::filter(n_treat_years=="4")%>%
  dplyr::as_tibble()

yr_drt_anppallb<-data.anpp1b%>%
  dplyr::filter(trt=="Drought")%>%
  dplyr::group_by(site_code,year,n_treat_days,plot,habitat.type,wc_map)%>%
  dplyr::summarize(mass=mean(mass))
length(unique(yr_drt_anppallb$site_code)) #99

control_meansallb<-data.anpp1b%>%
  dplyr::filter(trt=="Control")%>% 
  dplyr::group_by(site_code,year,n_treat_days,habitat.type,wc_map)%>%
  dplyr::summarize(mean_control_mass=mean(mass))
length(unique(control_meansallb$site_code)) #99

data.alld<-data.frame(merge(yr_drt_anppallb,control_meansallb[,c("site_code","year","mean_control_mass","habitat.type","n_treat_days","wc_map")]))
length(unique(data.alld$site_code)) #99 sites
anti_join(yr_drt_anppallb,data.alld)

data.alld$DS3<-log(data.alld$mass/data.alld$mean_control_mass)
str(data.alld)
data.alld$site_code<-as.factor(data.alld$site_code)

#Remove outlier year (2019) for hyide (WAY OUT THERE)
data.alle<-data.alld%>%dplyr::filter(!(site_code %in% c('hyide.de')& n_treat_days %in% c(1224)))%>%
  dplyr::as_tibble()  
#Starting on the left side of the bar, the formula for a random intercept, by itself, 
#is simply "1". The formula for a random regression coefficient for a variable x, without 
#the corresponding random intercept, is "0 + x".  Random intercepts are included by default, 
#so "x" and "1 + x" are equivalent specifications of both a random slope and a random intercept.

#Model 1 did not originally converge
#Model failed to converge with max|grad| = 27.9177 (tol = 0.002, component 1)
#Model is nearly unidentifiable: very large eigenvalue
#- Rescale variables?
#  Model is nearly unidentifiable: large eigenvalue ratio
#- Rescale variables?
#Trying to get the model to converge using:

#different algorithms:
#lmer(...,  control = lmerControl(optimizer="bobyqa")) #Didn't work
#lmer(...,  control = lmerControl(optimizer="Nelder_Mead")) #Didn't work

#running the algorithm with higher number of iterations: 
#control=lmerControl(optCtrl=list(maxfun=2000)) #Didn't work

#restarting the model from parameter estimate reached before the algorithm gave up: 
#ss <- getME(re.lm1,c("theta","fixef"))
#re.lm1b <- update(re.lm1,start=ss)#Didn't work

#Scale continuous variables

data.alle$n_treat_days<- scale(data.alle$n_treat_days)
summary(data.alle)

re.lm1 <- lmer(DS3 ~ n_treat_days + (1+n_treat_days|site_code),data = data.alle) 
#Scaling worked, no longer get non-convergence
summary(re.lm1)
r.squaredGLMM(re.lm1)


re.lm2 <- lmer(DS3 ~ n_treat_days + (1|site_code), data = data.alle) 
summary(re.lm2)
r.squaredGLMM(re.lm2)

AICc(re.lm1) #3237.523
AICc(re.lm2) #3386.85 (Delta AIC= 149.33)

#Build these models with treatment in the model
#Remove year 5 for sites since so few
datayr1.4b<-datayr1.4%>%dplyr::filter(!n_treat_years %in% c("5"))%>%
  dplyr::filter(!(site_code %in% c('hyide.de')& n_treat_years %in% c(3)))%>%
  dplyr::as_tibble() 

datayr1.4b$n_treat_years<-as.factor(datayr1.4b$n_treat_years)

#AR1
#Regular model with year and trt as factors, and site as a random effects
Model1trt<-lme(mass~trt*n_treat_years,random=~1|site_code,data=datayr1.4b)
summary(Model1trt) #AIC= 33192.44
r.squaredGLMM(Model1trt)
anova(Model1,type="marginal")
emout1trt<-emmeans(Model1trt,~trt|n_treat_years)
pairs(emout1trt) #pairwise comparisons between years

#Model with AR1 correlation structure and year as a factor, wc_map as continuous, and site as a random effects
Model2trt<-lme(mass~trt*n_treat_years,random=~1|site_code, correlation= corAR1(form=~1|site_code),data=datayr1.4b)
summary(Model2trt) #AIC=33098.88 
r.squaredGLMM(Model2trt)
anova(Model2trt)
emout2trt<-emmeans(Model2trt,~trt|n_treat_years)
pairs(emout2trt)

#Random slope models
data.anpp1b
data.anpp1c<-data.anpp1b%>%dplyr::filter(!(site_code %in% c('hyide.de')& n_treat_days %in% c(1224)))%>%
  dplyr::as_tibble() 
data.anpp1c$n_treat_days<- scale(data.anpp1c$n_treat_days)

re.lm1trt <- lmer(mass ~ trt*n_treat_days + (1+n_treat_days|site_code),data = data.anpp1c) 
#Scaling worked, no longer get non-convergence
summary(re.lm1trt)
r.squaredGLMM(re.lm1trt)
plot(re.lm1)

re.lm2trt <- lmer(mass ~ trt*n_treat_days + (1|site_code), data = data.anpp1c) 
summary(re.lm2trt)

AICc(re.lm1trt) #44587.04
AICc(re.lm2trt) #44695.39

data.all.mean<-data.alld%>%
  dplyr::group_by(site_code,n_treat_days,habitat.type)%>%
  dplyr::summarise(meanDS3=mean(DS3))%>%
  dplyr::as_tibble()
str(data.all.mean)
unique(data.all.mean$site_code) #105

Fig_days <- ggplot(data.all.mean,aes(x=n_treat_days,y=meanDS3,shape=habitat.type,color=habitat.type))+
  geom_hline(yintercept=0,lty=2,color="black",size=1) +
  #geom_vline(xintercept=0,lty=1,color="gray30",size=1)+
  #geom_vline(xintercept=-0.05, colour="orange",size=1) + 
  geom_text(aes(label=site_code)) +
  #geom_vline(data=filter(data.ppt, habitat.type=="Shrubland"), aes(xintercept=-0.11), colour="purple",size=1)+ 
  #geom_text(data=filter(data.ppt, habitat.type=="Shrubland"),aes(x=-0.20, label="-11%", y=-1.6), colour="purple", angle=0, text=element_text(size=4)) +
  #geom_hline(yintercept=-0.69,lty=2,color="red2",size=1.5)+
  #geom_hline(yintercept=-1.39,lty=2,color="red2",size=1.5)+
  #geom_abline(intercept=-0.15837, slope=0.35, size=2,color="blue")+
  #geom_ribbon(aes(ymin=LowerCI.line,ymax=UpperCI.line), fill="pink", alpha=.5)+
  geom_point(size=5,stroke = 1) + 
  scale_color_manual(name="Ecosystem Type",labels = c('Grassland','Shrubland'),values=c("mediumseagreen","wheat3"))+
  scale_shape_manual(name="Ecosystem Type",labels = c('Grassland','Shrubland'),values=c(19,17))+ 
  geom_smooth(aes(group=1),method = "lm", formula=y~x, fullrange=TRUE,se=TRUE,col="black",fill = "gray60")+
  #geom_errorbarh(aes(xmin = drtLowerCI, xmax = drtUpperCI),color="black",height = 0.05)+
  #geom_errorbar(aes(ymin = DS3LowerCI, ymax = DS3UpperCI),color="black",width=0.05) + 
  #scale_shape_manual(name="1 in 100 year Drought",labels = c('Extr'='Yes','NotExtreme'='No'),values=c(21,24))+
  #facet_wrap(.~habitat.type) +
  #geom_text(aes(label=site_code))+
  #geom_point(aes(x=2000,y=39.27),size=3,col="steelblue")+geom_errorbar(aes(x= 2000,ymin=37.19, ymax=41.35), width=20,col="steelblue",size=1)+ 
  #scale_y_continuous(expression(paste("Drought RR ( log ( ",ANPP[D]," / ",ANPP[C]," ))" ,sep=" ")), sec.axis = sec_axis(~ . * 1.0, name = "Mean Drought RR"))+
  #, sec.axis = sec_axis(~ . * 1.0, name = "Mean ANPP % Reduction"))+
  labs(y = "Drought Response", x="Treatment Days")+
  theme_tufte(ticks=T,base_size = 22)+ geom_rangeframe()+
  theme(text=element_text(size=16,  family="Calibri",color="black"),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        #axis.line.y.right = element_line(color="steelblue", size = 1),
        #axis.ticks.y.right = element_line(color = "steelblue"),
        #axis.title.y.right = element_text(colour = "steelblue"),
        #axis.text.y.right = element_text(colour = "steelblue"),
        legend.position = "top") 
#scale_fill_gradient2(low='darkorange',high='deepskyblue3') + 
#labs(fill = "Ambient precip \nrelative to MAP")
Fig_days

jpeg(Fig_days, filename = "C:\\Users\\Kwilks86\\Dropbox\\IDE_3-4Yrs_Drt\\Fig_days.jpeg", width=8,height=8,units="in",res=600)  
Fig_days
dev.off()

write.csv(data.anpp1b,"C:\\Users\\Kwilks86\\Dropbox\\IDE_3-4Yrs_Drt\\data_trtdays.csv",row.names=FALSE)


#SITES WITH ONLY ALL 4 YEARS of DATA (n=31)
data.allc

#Count how many years each site has
sites.year2<-data.allc %>%                    
  dplyr::group_by(site_code) %>%          
  dplyr::summarise(Unique = n_distinct(n_treat_years))%>%
  dplyr::filter(Unique=="4")%>%
  dplyr::as_tibble()
sites.year2

data.31<-merge(data.allc,sites.year2,by="site_code")

#Regular model with year as a factor, wc_map as continuous, and site as a random effects
Model31<-lme(DS3~n_treat_years,random=~1|site_code,data=data.31)
summary(Model31) #AIC=922.495
anova(Model31,type="marginal")
r.squaredGLMM(Model31)
emout31<-emmeans(Model31,~n_treat_years)
pairs(emout31) #pairwise comparisons between years

#Model with AR1 correlation structure and year as a factor, wc_map as continuous, and site as a random effects
Model2.31<-lme(DS3~n_treat_years,random=~1|site_code, correlation= corAR1(form=~1|site_code),data=data.31)
summary(Model2.31) #AIC= 924.4326
anova(Model2.31)
r.squaredGLMM(Model2.31)
emout2.31<-emmeans(Model2.31,~n_treat_years)
pairs(emout2.31)

#Random slope models
data.alle$n_treat_days<- scale(data.alle$n_treat_days)
summary(data.alle)

data.52b<-merge(data.alle,sites.52,by="site_code")

re.lm1.52 <- lmer(DS3 ~ n_treat_days + (1+n_treat_days|site_code),data = data.52b)

#Scaling worked, no longer get non-convergence
summary(re.lm1.52)
r.squaredGLMM(re.lm1.52)
anova(re.lm1.52)

re.lm2.52<- lmer(DS3 ~ n_treat_days + (1|site_code), data = data.52b) 
summary(re.lm2.52)
anova(re.lm2.31)
AICc(re.lm1.52) #2243.366
AICc(re.lm2.52) #2362.881

data.52c<-merge(data.52b,data.alle, by=c("site_code","habitat.type","DS3","plot","n_treat_days"))

data.52.mean<-data.52c%>%
  dplyr::group_by(site_code,n_treat_days,habitat.type)%>%
  dplyr::summarise(meanDS3=mean(DS3))%>%
  dplyr::as_tibble()

Fig_days52 <- ggplot(data.52.mean,aes(x=n_treat_days,y=meanDS3,shape=habitat.type,color=habitat.type))+
  geom_hline(yintercept=0,lty=2,color="black",size=1) +
  #geom_vline(xintercept=0,lty=1,color="gray30",size=1)+
  #geom_vline(xintercept=-0.05, colour="orange",size=1) + 
  #geom_text(aes(label=site_code)) +
  #geom_vline(data=filter(data.ppt, habitat.type=="Shrubland"), aes(xintercept=-0.11), colour="purple",size=1)+ 
  #geom_text(data=filter(data.ppt, habitat.type=="Shrubland"),aes(x=-0.20, label="-11%", y=-1.6), colour="purple", angle=0, text=element_text(size=4)) +
  #geom_hline(yintercept=-0.69,lty=2,color="red2",size=1.5)+
  #geom_hline(yintercept=-1.39,lty=2,color="red2",size=1.5)+
  #geom_abline(intercept=-0.15837, slope=0.35, size=2,color="blue")+
  #geom_ribbon(aes(ymin=LowerCI.line,ymax=UpperCI.line), fill="pink", alpha=.5)+
  geom_point(size=5,stroke = 1) + 
  scale_color_manual(name="Ecosystem Type",labels = c('Grassland','Shrubland'),values=c("mediumseagreen","wheat3"))+
  scale_shape_manual(name="Ecosystem Type",labels = c('Grassland','Shrubland'),values=c(19,17))+ 
  geom_smooth(aes(group=1),method = "lm", formula=y~x, fullrange=TRUE,se=TRUE,col="black",fill = "gray60")+
  #geom_errorbarh(aes(xmin = drtLowerCI, xmax = drtUpperCI),color="black",height = 0.05)+
  #geom_errorbar(aes(ymin = DS3LowerCI, ymax = DS3UpperCI),color="black",width=0.05) + 
  #scale_shape_manual(name="1 in 100 year Drought",labels = c('Extr'='Yes','NotExtreme'='No'),values=c(21,24))+
  #facet_wrap(.~habitat.type) +
  #geom_text(aes(label=site_code))+
  #geom_point(aes(x=2000,y=39.27),size=3,col="steelblue")+geom_errorbar(aes(x= 2000,ymin=37.19, ymax=41.35), width=20,col="steelblue",size=1)+ 
  #scale_y_continuous(expression(paste("Drought RR ( log ( ",ANPP[D]," / ",ANPP[C]," ))" ,sep=" ")), sec.axis = sec_axis(~ . * 1.0, name = "Mean Drought RR"))+
  #, sec.axis = sec_axis(~ . * 1.0, name = "Mean ANPP % Reduction"))+
  labs(y = "Drought Response", x="Treatment Days")+
  theme_tufte(ticks=T,base_size = 22)+ geom_rangeframe()+
  theme(text=element_text(size=16,  family="Calibri",color="black"),
        axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1),
        #axis.line.y.right = element_line(color="steelblue", size = 1),
        #axis.ticks.y.right = element_line(color = "steelblue"),
        #axis.title.y.right = element_text(colour = "steelblue"),
        #axis.text.y.right = element_text(colour = "steelblue"),
        legend.position = "top") 
#scale_fill_gradient2(low='darkorange',high='deepskyblue3') + 
#labs(fill = "Ambient precip \nrelative to MAP")
Fig_days52

jpeg(Fig_days52, filename = "C:\\Users\\Kwilks86\\Dropbox\\IDE_3-4Yrs_Drt\\Fig_days-52sites.jpeg", width=8,height=8,units="in",res=600)  
Fig_days52
dev.off()