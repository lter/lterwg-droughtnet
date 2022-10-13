

#multiyear anpp analysis for droughtnet##

#this script looks at whether the response of ANPP to drought
#changes throught time across sites that meet certain criteria, namely:

# 1. There were at least four plots
# 2. There were four consecutive years of drought
# 3. Non-forest sites/other problem sites removed
# 4. And in a subset of sites, sites that had all four years of consecutive 'extreme' drought

# written by Andrew Felton 10/10/22-10/13/22
#please not this script was originally part of an R project that
#has data files locally (in a 'data' folder) and local 'figures' folder
#to save the figures in

#github:
#lter/lterwg-droughtnet

#setup-----

rm(list=ls())

# Import packages
pkgs <- c("tidyverse", 'rstudioapi')
lapply(pkgs, library, character.only = TRUE) # load them

# Set wd
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

#get site-level map data (I had all files locally...)
habitat <- read.csv('data/Site_Elev-Disturb.csv')
site_map <- habitat %>%
  select(site_code,precip)
rm(habitat)

#import anpp data
anpp_data <- read.csv('data/anpp_ppt_10-12-2022.csv')
head(anpp_data,1)
anpp_data <- anpp_data %>% 
  filter(mass_category == 'anpp') %>%
  filter(trt %in% c('Drought','Control','Control_Infrastructure')) %>%
  select(site_code, block, plot, subplot, year,mass,mass_category,trt,ppt.1)
unique(anpp_data$trt)

#fix label and check
anpp_data$trt <- gsub("Control_Infrastructure","Control",anpp_data$trt)
unique(anpp_data$trt)

anpp_data <- left_join(anpp_data,site_map,by=c('site_code'))

#calculate drought response and severity

#get drought severity
drought_severity_control <- anpp_data %>%
  select(site_code,year,trt,ppt.1,precip) %>%
  filter(trt == 'Control') %>%
  rename('control_precip' = 'ppt.1' )%>%
  select(site_code,year,control_precip,precip)

drought_severity_drought <- anpp_data %>%
  select(site_code,year,trt,ppt.1) %>%
  filter(trt == 'Drought') %>%
  rename('drought_precip' = 'ppt.1') %>%
  select(site_code,year,drought_precip)

#join and remove duplicates
drought_severity <- left_join(drought_severity_drought,drought_severity_control,
                              by=c('site_code','year'))

rm(drought_severity_control,drought_severity_drought)

drought_severity$drought_severity <- 
  ((drought_severity$drought_precip - drought_severity$precip)/drought_severity$precip)

drought_severity <- drought_severity[!duplicated(drought_severity), ]

#get log response ratio for each year
head(anpp_data,1)
control_anpp_mean <- anpp_data %>%
  filter(trt == 'Control') %>%
  group_by(site_code,year) %>%
  summarise(control_mean = mean(mass)) 

drought_anpp <- anpp_data %>%
  select(site_code,year,block,plot,subplot,trt,mass) %>%
  filter(trt == 'Drought') %>%
  rename('drought_anpp' = 'mass') %>%
  select(site_code,year,block,plot,subplot,drought_anpp)

#join and remove duplicates
drought_control_anpp <- left_join(drought_anpp,control_anpp_mean,
                                  by = c('site_code','year'))
rm(control_anpp_mean,drought_anpp)
drought_control_anpp <- drought_control_anpp[!duplicated(drought_control_anpp),]

drought_control_anpp <- aggregate(drought_anpp ~ site_code + year + block +
                                    plot + drought_anpp + control_mean,
                                  mean,data = drought_control_anpp)

drought_control_anpp$lrr_drought <- 
  log(drought_control_anpp$drought_anpp/drought_control_anpp$control_mean)

drought_severity <- aggregate(drought_severity ~ site_code + year, mean, data=drought_severity)

drought_control_anpp_precip <- left_join(drought_control_anpp,drought_severity,
                                         by=c('site_code','year'))
head(drought_control_anpp_precip,1)

#check hoide
# anpp_hoide <- anpp_data %>%
#   filter(site_code == 'hoide.de')
# 
# ggplot(anpp_hoide,aes(year,mass,color=trt)) +
#   #geom_point()
#   #geom_boxplot()
#   geom_point(position=position_dodge(width=0.5))
# 
# #allmend
# anpp_allmend <- anpp_data %>%
#   filter(site_code == 'allmendb.ch')
# ggplot(anpp_allmend,aes(year,mass,color=trt)) +
#   #geom_point()
#   #geom_boxplot()
#   geom_point(position=position_dodge(width=0.5))
# 
# #jenadrt.de
# anpp_jena <- anpp_data %>%
#   filter(site_code == 'jenadrt.de')
# ggplot(anpp_data,aes(year,mass,color=trt)) +
#   #geom_point()
# #geom_boxplot()
# geom_point(position=position_dodge(width=0.5))

#plot(lrr_drought ~ drought_severity,data=drought_control_anpp_precip)

unique(drought_control_anpp_precip$site_code)

# hoide <- anpp_data %>%
#   filter(site_code == "hoide.de")
# head(hoide,1)
# 
# ggplot(hoide,aes(year,mass,color=trt)) +
#   geom_point()

#filter to sites that have at least three plot replicates
no_plots <- aggregate(plot ~ year + site_code,length,data=drought_control_anpp_precip)
no_plots <- no_plots %>%
  filter(plot > 3) %>%
  rename('number_of_plots' = 'plot')

drought_control_anpp_precip <- left_join(drought_control_anpp_precip,no_plots,
                                         by=c('site_code','year'))

head(drought_control_anpp_precip,1)

#filter to sites that had four consecutive years of data

#step 1
drought_control_anpp_precip <- drought_control_anpp_precip %>%
  group_by(site_code) %>%
  mutate(year_2 = year - min(year) + 1) %>%
  filter(year_2 < 5) 

drought_control_anpp_precip <- na.omit(drought_control_anpp_precip)

#step 2
year_length <- aggregate(lrr_drought ~ site_code + year,mean,data=drought_control_anpp_precip)
year_length <- aggregate(lrr_drought ~ site_code,length,data=year_length)
year_length <- year_length %>% rename('no_years' = 'lrr_drought')
drought_control_anpp_precip <- left_join(drought_control_anpp_precip,year_length,by=c('site_code'))
drought_control_anpp_precip <- drought_control_anpp_precip %>%
  filter(no_years > 3)

#make sure they were consecutive
max_min <- function(x){
  
  max_min_result <- (max(x) + 1) - min(x)
  return(max_min_result)
  
}

consec_years <- aggregate(year ~ site_code,max_min,data=drought_control_anpp_precip)
consec_years <- consec_years %>%
  rename('consecutive_years' = 'year')
drought_control_anpp_precip <- left_join(drought_control_anpp_precip,consec_years,
                                         by=c('site_code'))
rm(consec_years)
drought_control_anpp_precip <- drought_control_anpp_precip %>%
  filter(consecutive_years > 3)

#import habitat and filter out forests
habitat <- read.csv('data/Site_Elev-Disturb.csv')
head(habitat,1)
unique(habitat$habitat)
habitat <- habitat %>%
  select(site_code,habitat) %>%
  filter(!habitat %in% c('forest','Forest understory','seasonal tropical forest',
                         'mixed temperate forest','Meditarranean forest','Forest',
                         'Temperate deciduous forest',"tropical cloud forest",
                         "Pine savanna","pinion juniper woodland","Deciduous forest",
                         "bamboo forest","temerate forest (deciduous)",
                         "Pine/Oak Savanna"))


#merge with main dataset
drought_control_anpp_precip <- left_join(drought_control_anpp_precip,habitat,
                                         by=c('site_code'))

#kranze.de doesn't have a habitat type

unique(drought_control_anpp_precip$habitat)

#remove other sites
#Removing sites here that DO NOT report ANPP or are outside of ANPP range for biome listed in Fahey & Knapp 2007:
drought_control_anpp_precip <- drought_control_anpp_precip[which(drought_control_anpp_precip$site_code!="lcnorth.cl" #Doesn't report ANPP
                                                                 &drought_control_anpp_precip$site_code!="lcsouth.cl" #Doesn't report ANPP
                                                                 &drought_control_anpp_precip$site_code!="qdtnorth.cl" #Doesn't report ANPP
                                                                 &drought_control_anpp_precip$site_code!="qdtsouth.cl" #Doesn't report ANPP
                                                                 &drought_control_anpp_precip$site_code!="neudamm.na" #Doesn't report ANPP, also no weather info
                                                                 &drought_control_anpp_precip$site_code!="ebro.es" #ANPP outside range for biome
                                                                 &drought_control_anpp_precip$site_code!="garraf.es" #Did not follow protocols: Not using for drought plots, only using control plots for this site
                                                                 &drought_control_anpp_precip$site_code!="brandjberg.dk" #Did not follow protocols: Not using for drought plots, only using control plots for this site
                                                                 &drought_control_anpp_precip$site_code!="ethadb.au" #ANPP outside range for biome
                                                                 &drought_control_anpp_precip$site_code!="ethadn.au" #ANPP outside range for biome
                                                                 &drought_control_anpp_precip$site_code!="swift.ca" #did not follow IDE protocols (drought plots did not exclude water- see Jillian email)
                                                                 &drought_control_anpp_precip$site_code!="jenadrt.de" #did not have the correct data for their first treatment year because they lost a biomass bag and
                                                                 
                                                                 #requested their data be removed (see GitHub for details), will include them in "Drought shelters were not in place for
                                                                 #120-650 days (+/- one week)
),] 

#how many sites?
length(unique(drought_control_anpp_precip$site_code))
#36 as of 10/13/2022

#plots -----

#plot(lrr_drought ~ year_2,data=drought_control_anpp_precip)

theme_chunk <- theme(
  axis.text.x = element_text(color = 'black', size = 7),
  axis.text.y = element_text(color = 'black', size = 7),
  axis.title = element_text(color = 'black', size = 10),
  axis.ticks = element_line(color = 'black'),
  legend.key = element_blank(),
  legend.title = element_blank(),
  legend.text = element_text(size = 5),
  legend.position = 'top',
  strip.background = element_rect(fill = "white"),
  strip.text = element_text(size = 15),
  panel.background = element_rect(fill = NA),
  panel.border = element_blank(),
  axis.line.x = element_line(colour = "black"),
  axis.line.y = element_line(colour = "black"))

# quantile(drought_control_anpp_precip$lrr_drought,probs=0.01)
# #filter out extreme high hyide site
# drought_control_anpp_precip_filter <- drought_control_anpp_precip %>%
#   filter(!site_code == 'hyide.de')

#broken up by year
year_facet_plot <- ggplot(drought_control_anpp_precip,aes(drought_severity,lrr_drought)) +
  geom_hline(yintercept = 0,color='red') +
  facet_wrap(~year_2) +
  geom_smooth(method='lm',se=F) +
  stat_summary(fun='mean',geom='point') +
  #geom_point(pch=1) +
  ylab('ANPP response (lrr)') +
  xlab('Drought severity') + theme_chunk

png(height = 1700,width=2000,res=300,'Figures/year_facet_plot.png')

year_facet_plot

dev.off()

#interaction plot
interaction_plot <- ggplot(drought_control_anpp_precip,aes(drought_severity,lrr_drought,color=as.factor(year_2))) +
  #geom_hline(yintercept = 0,color='red') +
  geom_smooth(method='lm',se=F) +
  #geom_point(alpha=0.1) +
  ylab('ANPP response (lrr)') +
  xlab('Drought severity') + theme_chunk

png(height = 1700,width=2000,res=300,'Figures/interaction_plot.png')

interaction_plot

dev.off()

length(unique(drought_control_anpp_precip$site_code))
unique(drought_control_anpp_precip$habitat)

drought_control_anpp_precip_mean <- aggregate(lrr_drought ~ site_code + year_2,mean,
                                              data=drought_control_anpp_precip)

#stitch plot 
#library(tidytext)
head(drought_control_anpp_precip_mean,1)
length(unique(drought_control_anpp_precip_mean$site_code))

#create a reference column for ordering (order sites by first year lrr)
drought_control_anpp_precip_mean_order <- drought_control_anpp_precip_mean %>%
  filter(year_2 == 1) %>%
  select(site_code,lrr_drought) %>%
  rename('lrr_drought_year_1' = 'lrr_drought')

drought_control_anpp_precip_mean <- 
  left_join(drought_control_anpp_precip_mean,drought_control_anpp_precip_mean_order,
            by='site_code')

stitch_plot <- 
  ggplot(drought_control_anpp_precip_mean,
         aes(x=reorder(site_code,-lrr_drought_year_1), y=lrr_drought)) +
  geom_point() +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~year_2) +
  geom_hline(yintercept = 0,color='red') +
  ylab('ANPP response (lrr)') +
  xlab('') + 
  # theme(
  #   axis.text.x = element_text(color = 'black', size = 5),
  #   #angle=25,hjust=1),
  #   axis.text.y = element_text(color = 'black', size = 5),
  #   axis.title = element_text(color = 'black', size = 10),
  #   axis.ticks = element_line(color = 'black'),
  #   legend.key = element_blank(),
  #   legend.title = element_blank(),
  #   legend.text = element_text(size = 5),
  #   #legend.position = c(0.82, 0.7),
  #   legend.position = 'top',
#   strip.background = element_rect(fill = "white"),
#   strip.text = element_text(size = 15),
#   panel.background = element_rect(fill = NA),
#   #panel.border = element_blank(),
#   #make the borders clear in prep for just have two axes
#   axis.line.x = element_line(colour = "black"),
#   axis.line.y = element_line(colour = "black"))

plot(stitch_plot)

png(height = 2000,width=2000,res=300,'Figures/stitch_plot.png')
stitch_plot
dev.off()


facet_plot <- 
  ggplot(drought_control_anpp_precip,
         aes(year_2,lrr_drought,
         )) +
  geom_point(size=1,pch=1) +
  stat_summary(fun='mean',geom='line') +
  facet_wrap(~site_code) +
  geom_hline(yintercept = 0,color='red') +
  ylab('ANPP response (lrr)') +
  xlab('') + 
  theme(
    axis.text.x = element_text(color = 'black', size = 10),
    axis.text.y = element_text(color = 'black', size = 10),
    axis.title = element_text(color = 'black', size = 15),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 5),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


png(height = 4000,width=4000,res=300,'Figures/duration_all_sites.png')
facet_plot
dev.off()

# #look at models
# library(nlme) 
# 
# multi_year_drought_allsites <- aggregate(lrr_drought ~ plot + year_2 + site_code + 
#                                            drought_severity,
#                                          mean,data=drought_control_anpp_precip)
# 
# severity_lme <- lme(lrr_drought ~ drought_severity*year_2,random = ~1|site_code,
#                     data = drought_control_anpp_precip)
# summary(severity_lme)
# plot(severity_lme)
# 
# plot(lrr_drought ~ year_2,data=drought_control_anpp_precip)
# 
# #subset to just year_1
# multi_year_drought_allsites_year_1 <- multi_year_drought_allsites %>%
#   filter(year_2=='1')
# 
# severity_year_1_lme <- lme(lrr_drought ~ drought_severity,random = ~1|site_code,
#                     data = multi_year_drought_allsites_year_1)
# summary(severity_year_1_lme)
# plot(lrr_drought ~ drought_severity,multi_year_drought_allsites_year_1)
# 
# #
# #

#most extreme year in each year
head(drought_control_anpp_precip,1)
drought_control_anpp_precip_extremes <-
  drought_control_anpp_precip_extremes %>%
  group_by(site_code)




#now filter to sites that had extreme years in every year ------

#filter sites that had a extreme drought in each year
ambient_precip <- read.csv('data/precip_by_trmt_year_with_percentiles_365-0days_2022-09-11.csv')
head(ambient_precip,1)
ambient_precip$drought_extemity <- 
  (ambient_precip$ppt_ambient - ambient_precip$site_map)/ambient_precip$site_map

ambient_precip <- ambient_precip %>%
  select(site_code,year,drought_extemity)

drought_control_anpp_precip <- left_join(drought_control_anpp_precip,ambient_precip,
                                         by=c('year','site_code'))

# #filter on two conditions: extreme and year 2
# extreme_year_2 <- drought_control_anpp_precip %>%
#   filter(year_2 == '2' & drought_extemity < 0.01) %>%
#   select(site_code,plot,lrr_drought,year_2)
# 
# #filter to year 1
# year_1 <- drought_control_anpp_precip %>%
#   filter(year_2 == '1') %>%
#   select(site_code,plot,drought_severity)
# 
# extreme_year_2_year_1 <- merge(extreme_year_2,year_1,by=c('site_code','plot'))
# 
# plot(lrr_drought ~ drought_severity,data=extreme_year_2_year_1)

#filter to just extreme years
just_extremes <- drought_control_anpp_precip %>%
  filter(drought_extemity < 0.01)

just_extremes <- just_extremes %>%
  group_by(site_code) %>%
  mutate(consecutive_extremes = year - min(year) + 1) 

#how many site now?
number_extremes <-aggregate(lrr_drought~year+site_code,mean,data=just_extremes)
number_extremes <-aggregate(lrr_drought~site_code,length,data=number_extremes)
number_extremes <- number_extremes %>%
  filter(lrr_drought > 3) %>%
  rename('extreme_drought_years' = 'lrr_drought')

just_extremes <- merge(just_extremes,number_extremes,by=c('site_code'))
length(unique(just_extremes$site_code))

year_facet_plot_only_extremes <- ggplot(just_extremes,aes(drought_severity,lrr_drought)) +
  geom_hline(yintercept = 0,color='red') +
  facet_wrap(~year_2) +
  geom_smooth(method='lm',se=F) +
  stat_summary(fun='mean',geom='point') +
  ggtitle('All four years are extremes') +
  ylab('ANPP response (lrr)') +
  xlab('Drought severity') + theme_chunk

png(height = 1700,width=2000,res=300,'Figures/year_facet_plot_only_extremes.png')

year_facet_plot_only_extremes

dev.off()

just_extremes_mean <- aggregate(lrr_drought ~ site_code + year_2,mean,data=just_extremes)

drought_year_extreme <- ggplot(just_extremes_mean,aes(year_2,lrr_drought)) +
  geom_hline(yintercept = 0,color='red') +
  geom_smooth(method='lm',se=T) +
  geom_point(pch=19,size=5) +
  ylab('ANPP response (lrr)') +
  xlab('Year') + theme_chunk

png(height = 1700,width=2000,res=300,'Figures/drought_year_extreme.png')

drought_year_extreme

dev.off()

just_extremes_plot_mean <- 
  aggregate(lrr_drought ~ site_code + plot + year_2,mean,data=just_extremes)

# duration <- lme(lrr_drought ~ year_2,random = ~1|site_code,data=just_extremes)
# summary(duration)
# plot(duration)

drought_year_extreme_site <- ggplot(just_extremes_mean,aes(year_2,lrr_drought,color=site_code)) +
  geom_hline(yintercept = 0,color='red') +
  geom_line(size=1) +
  geom_point(pch=19,size=2) +
  ylab('ANPP response (lrr)') +
  xlab('Year') + theme_chunk

png(height = 1700,width=2000,res=300,'Figures/drought_year_extreme_site.png')

drought_year_extreme_site

dev.off()

just_extremes <- just_extremes %>% mutate(lagged = lag(year_2, default = 1))

head(drought_year_extreme_site,1)

#look at weird values/sites ------

# #range in each year
# boxplot_year <- ggplot(drought_control_anpp_precip,aes(x=as.factor(year_2), y=lrr_drought)) +
#   geom_boxplot() +
#     ylab('ANPP response (lrr)') +
#     xlab('Year') + theme_chunk
# 
# png(height = 1700,width=2000,res=300,'Figures/drought_year_boxplot.png')
# 
# boxplot_year
# 
# dev.off()
# 
# #facet drought response by year for each site
# boxplot_year <- ggplot(drought_control_anpp_precip,aes(x=year_2, y=lrr_drought)) +
#   geom_point(size=0.5) +
#   facet_wrap(~site_code) +
#   ylab('ANPP response (lrr)') +
#   xlab('Year') + theme_chunk
# 
# #year 1
# drought_control_anpp_precip_year_1 <- drought_control_anpp_precip %>%
#   filter(year_2 == '1')
# 
# hist(drought_control_anpp_precip_year_1$lrr_drought,
#      main = 'Year 1',xlab = 'ANPP response')
# 
# 
# 
# #year 2
# drought_control_anpp_precip_year_2 <- drought_control_anpp_precip %>%
#   filter(year_2 == '2')
# 
# hist(drought_control_anpp_precip_year_2$lrr_drought,
#      main = 'Year 2',xlab = 'ANPP response',add=T,col='red')
# 
# 
# #ways to subset
# # have at least three treatments and three controls - done
# # make sure all have four years of data - done
# # tag sites that had extreme years in each - done
# 
# #(ambient - map/map)
# #ambient is the precip in the 365 days prior. If the equation is less than 0.01
# #then it was deemed = extreme
# 
# 
# head(drought_control_anpp_precip,1)

