library(tidyverse)
library(plyr)


anpp_ppt <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/anpp_ppt_12-05-2022.csv")
#IDE_treatment_years <- read.csv("C:/Users/ohler/Downloads/IDE_treatment_years_11-17-2022.csv")%>%
#  subset(n_treat_years != "NA")%>%
#                        unique()

#anpp_ppt <- left_join(anpp_ppt.1, IDE_treatment_years, by = c("site_code", "year"))

Site_Elev.Disturb <- read.csv("C:/Users/ohler/Dropbox/IDE MS_Single year extreme/Data/Site_Elev-Disturb.csv")

anpp_ppt_map <- anpp_ppt%>%
  left_join( Site_Elev.Disturb, by = "site_code")%>%
  subset(habitat.type == "Grassland" | habitat.type == "Shrubland")%>%
  subset(ppt.1 >0 &ppt.2 >0 &ppt.3 >0 &ppt.4 >0 )#%>%
anpp_ppt_map$extreme.nominal <- ifelse(anpp_ppt_map$ppt.1 <= anpp_ppt_map$precip, "extreme", "nominal")
EN.df <- subset(anpp_ppt_map, trt == "Control")
EN.df <- EN.df[,c("site_code", "n_treat_years", "extreme.nominal")]
data.anpp <- anpp_ppt_map
#REMOVING FOREST SITES (Will leave this data to Rich Phillips)
#data.noforest<-anpp_ppt %>%
#  dplyr::filter(!habitat.type %in% c('Forest','Forest understory'))
#length(unique(data.noforest$site_code)) #120 (16 sites removed)
#setdiff(data.all$site_code,data.noforest$site_code)
# [1] "bamboo.cn"    "bivensarm.us" "cmss.us"      "elizwood.us"  "gigante.pa"   "horizon.cr"  
#[7] "hubbard.us"   "jilpanger.au" "kranz.de"     "p12.pa"       "p13.pa"       "prades.es"   
#[13] "sevforest.us" "sherman.pa"   "thompson.us"  "wayqecha.pe" 
#NOTE: Prades didn't follow IDE protocols, but also happens to be a forest site

#Removing sites here that DO NOT report ANPP or are outside of ANPP range for biome listed in Fahey & Knapp 2007:
#data.anpp<- anpp_ppt%>%
#            subset(site_code != "lcnorth.cl")%>%
#  subset(site_code != "lcsouth.cl")%>%#Doesn't report ANPP
#  subset(site_code != "qdtnorth.cl")%>%#Doesn't report ANPP
#  subset(site_code != "qdtsouth.cl")%>%#Doesn't report ANPP
#  subset(site_code != "neudamm.na")%>%#Doesn't report ANPP, also no weather info
#  subset(site_code != "ebro.es")%>%#ANPP outside range for biome
#  subset(site_code != "garraf.es")%>%#Did not follow protocols: Not using for drought plots, only using control plots for this site
#  subset(site_code != "brandjberg.dk")%>%#Did not follow protocols: Not using for drought plots, only using control plots for this site
#  subset(site_code != "ethadb.au")%>%#ANPP outside range for biome
#  subset(site_code != "ethadn.au")%>%#ANPP outside range for biome
#  subset(site_code != "swift.ca")%>%#did not follow IDE protocols (drought plots did not exclude water- see Jillian email)
#  subset(site_code != "jenadrt.de")#did not have the correct data for their first treatment year because they lost a biomass bag and 
  
  
#  data.noforest[which(data.noforest$site_code!="lcnorth.cl" #Doesn't report ANPP
#                               &data.noforest$site_code!="lcsouth.cl" #Doesn't report ANPP
#                               &data.noforest$site_code!="qdtnorth.cl" #Doesn't report ANPP
#                               &data.noforest$site_code!="qdtsouth.cl" #Doesn't report ANPP
#                               &data.noforest$site_code!="neudamm.na" #Doesn't report ANPP, also no weather info
#                               &data.noforest$site_code!="ebro.es" #ANPP outside range for biome
#                               &data.noforest$site_code!="garraf.es" #Did not follow protocols: Not using for drought plots, only using control plots for this site
#                               &data.noforest$site_code!="brandjberg.dk" #Did not follow protocols: Not using for drought plots, only using control plots for this site
#                               &data.noforest$site_code!="ethadb.au" #ANPP outside range for biome
#                               &data.noforest$site_code!="ethadn.au" #ANPP outside range for biome
#                               &data.noforest$site_code!="swift.ca" #did not follow IDE protocols (drought plots did not exclude water- see Jillian email)
#                               &data.noforest$site_code!="jenadrt.de" #did not have the correct data for their first treatment year because they lost a biomass bag and 
                               #requested their data be removed (see GitHub for details), will include them in "Drought shelters were not in place for 
                               #120-650 days (+/- one week)
#),] 

length(unique(data.anpp$site_code)) #108


#Counting the number of reps for each treatment and year
uniq.plot<- data.anpp %>% 
  dplyr::filter(trt %in% c("Drought","Control"))%>%
  dplyr::select(site_code,year,plot,trt)%>%
  dplyr::distinct(site_code,year,plot,trt)%>%
  dplyr::as_tibble()

Plot.trt.ct <- uniq.plot%>% dplyr::group_by(site_code,trt,year) %>% dplyr::summarise(Plot.count=n())

#Switching to wide format to see which sites do not have both treatments in a year
Plottrt_wide<-spread(Plot.trt.ct,trt,Plot.count)
Plottrt_wide[is.na(Plottrt_wide)] <- 0

#Remove sites and years that don't have both control and drought plots
#or that only have one rep of drought
#brokenh.au in 2019 (0 drought plots)
#chacra.ar in 2016 (1 control plot) #Keeping
#chilcasdrt.ar in 2016 and 2020 (0 drought plots)
#charleville.au in 2017 (1 control plot) #Keeping
#cobar.au in 2018 (1 control plot) #Keeping

Plottrt_wide1<-Plottrt_wide%>%
  dplyr::filter(Drought>=3 & Control>=3)%>%
  dplyr::as_tibble()




#Switch back to long format for merge
Plot.trt.ct2<-gather(Plottrt_wide1,trt,Plot.count,Drought:Control)

#Merge unique trt count back with data frame to filter
data.anpp1<-merge(data.anpp,Plot.trt.ct2,by=c("site_code","trt","year"))

setdiff(data.anpp$site_code,data.anpp1$site_code) #no sites eliminated here
length(unique(data.anpp1$site_code)) #100


controls <- data.anpp1%>%
  subset(trt == "Control" )%>%
  ddply(.(site_code, year),
        function(x)data.frame(
          mean.control = mean(x$mass)
        ))

data.anpp2 <- merge(data.anpp1, controls, by = c("site_code", "year"))%>%
  subset(trt == "Drought")#%>%
              #left_join(Site_Elev.Disturb, by = "site_code")



data.anpp2$anpp_response <- log(data.anpp2$mass/data.anpp2$mean.control)
data.anpp2$drtsev.1 <- ((data.anpp2$ppt.1-data.anpp2$precip)/data.anpp2$precip)
#data.anpp2$extreme.nominal <- ifelse(data.anpp2$ppt.1 <= data.anpp2$precip, "extreme", "nominal")
data.anpp2 <- dplyr::select(data.anpp2, -c(extreme.nominal))
data.anpp2 <- left_join(data.anpp2, EN.df, by = c("site_code", "n_treat_years"))

data.summ <- ddply(data.anpp2, .(site_code, n_treat_years, habitat.type, drtsev.1, extreme.nominal, precip),
                        function(x)data.frame(
                          anpp_response = mean(x$anpp_response, na.rm = TRUE),
                          anpp_response.error = qt(0.975, df=length(x$site_code)-1)*sd(x$anpp_response, na.rm = TRUE)/sqrt(length(x$site_code)-1)
                        ))%>%
  subset(habitat.type == "Grassland" | habitat.type == "Shrubland")



subset(data.summ, n_treat_years == 2)%>%
dplyr::mutate(site_code = fct_reorder(site_code, desc(anpp_response))) %>%
ggplot( aes(site_code, anpp_response, color = habitat.type))+
  geom_pointrange(aes(ymin = anpp_response-anpp_response.error, ymax = anpp_response+anpp_response.error))+
  geom_hline(yintercept = 0,linetype="dashed")+
  ylim(c(-6,6))+
  ylab("anpp_response")+
  xlab("")+
  coord_flip()+
  theme_bw()
        


###Need to calculate which are nominal and which are extreme to separate responses in figures
ddply(data.summ, .(habitat.type, n_treat_years, extreme.nominal),
      function(x)data.frame(
        anpp_response = mean(x$anpp_response, na.rm = TRUE),
        anpp_response.error = qt(0.975, df=length(x$habitat.type)-1)*sd(x$anpp_response, na.rm = TRUE)/sqrt(length(x$habitat.type)-1)
      ))%>%
subset(n_treat_years == 2)%>%
  
  ggplot( aes(interaction(habitat.type,extreme.nominal), anpp_response, color = habitat.type, shape = extreme.nominal))+
  geom_pointrange(aes(ymin = anpp_response-anpp_response.error, ymax = anpp_response+anpp_response.error))+
  geom_hline(yintercept = 0,linetype="dashed")+
  ylim(c(-1.25,.5))+
  ylab("anpp_response")+
  xlab("")+
  coord_flip()+
  theme_bw()

temp <- data.summ%>%
                       subset(n_treat_years == 2)%>%
                        subset(habitat.type == "Shrubland" & extreme.nominal == "nominal")
length(unique(temp$site_code))

temp <- data.summ%>%
                subset(n_treat_years == 2)%>%
                subset(habitat.type == "Shrubland" & extreme.nominal == "extreme")         
length(unique(temp$site_code))

temp <- data.summ%>%
                subset(n_treat_years == 2)%>%
                subset(habitat.type == "Grassland" & extreme.nominal == "nominal")
length(unique(temp$site_code))

temp <- data.summ%>%
                subset(n_treat_years == 2)%>%
                subset(habitat.type == "Grassland" & extreme.nominal == "extreme")
length(unique(temp$site_code))

#####Remake drought severity figure
subset(data.summ, n_treat_years == 2)%>%
ggplot( aes(drtsev.1, anpp_response))+
  geom_point(aes(color = habitat.type))+
  geom_smooth(method = "lm")+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  xlab("Drought severity")+
  theme_bw()

mod <- lm(anpp_response~drtsev.1, data = subset(data.summ, n_treat_years == 2))
summary(mod)
mod <- lmer(anpp_response~drtsev.1 + (1|site_code), data = subset(data.anpp2, n_treat_years == 2))
summary(mod)
##mean responses Y2
response_mean <- subset(data.summ, n_treat_years == 2)%>%
          ddply( .(habitat.type, extreme.nominal),
       function(x)data.frame(
         anpp_response = mean(x$anpp_response)
       ))

response_mean$pct_change <- (100 * ((exp(response_mean$anpp_response)-1)))


#######################Test the covariates
subset(data.summ, n_treat_years == 2)%>%
ggplot( aes(precip, anpp_response))+
  geom_point(aes(color = habitat.type))+
  geom_smooth(method = "lm")+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  xlab("MAP")+
  theme_bw()
