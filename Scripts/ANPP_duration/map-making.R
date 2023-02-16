install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata","paletteer"))
                 
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("paletteer")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)



##Put in some site info. Using same workflow as main duration script
data.anpp <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/anpp_ppt_2023-02-06.csv")%>%
  subset(habitat.type == "Grassland" | habitat.type == "Shrubland")#%>%

Site_Elev.Disturb <- read.csv("C:/Users/ohler/Dropbox/IDE MS_Single year extreme/Data/Site_Elev-Disturb.csv")

anpp.mean <- data.anpp%>%
  #left_join( Site_Elev.Disturb, by = "site_code")%>%
  #subset(habitat.type == "Grassland" | habitat.type == "Shrubland")%>%
  subset(trt == "Control")%>%
  ddply(.(site_code, year),function(x)data.frame(mass = mean(x$mass)))%>% #summarizes controls for each year
  ddply(.(site_code),function(x)data.frame(mean.mass = mean(x$mass), n_years = length(x$mass)))#summarizes controls across years

length(unique(data.anpp$site_code)) #112
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


Plottrt_wide1<-Plottrt_wide%>%
  dplyr::filter(Drought>=2 & Control>=1)%>%
  dplyr::as_tibble()

#Switch back to long format for merge
Plot.trt.ct2<-gather(Plottrt_wide1,trt,Plot.count,Drought:Control)

#Merge unique trt count back with data frame to filter
data.anpp1<-merge(data.anpp,Plot.trt.ct2,by=c("site_code","trt","year"))

setdiff(data.anpp$site_code,data.anpp1$site_code) #no sites eliminated here
length(unique(data.anpp1$site_code)) #1o9

#controls <- data.anpp1%>%
#  subset(trt == "Control" )%>%
#  ddply(.(site_code, year),
#        function(x)data.frame(
#          mean.control = mean(x$mass)
#        ))
num.treat.years <- data.anpp1[,c("site_code", "n_treat_years")]%>%
  unique()%>%
  subset(n_treat_years>=1&n_treat_years<=4)

num.treat.years <- ddply(num.treat.years,.(site_code),
                         function(x)data.frame(
                           num.years = length(x$n_treat_years)
                         ))


data.anpp2 <- merge(data.anpp1, anpp.mean, by = c("site_code"))%>%
  subset(trt == "Drought")%>%
  subset(n_years>=4)%>%
  left_join(num.treat.years, by = "site_code")%>%
  subset(num.years == 4 | num.years == 3
  ) #change here if using 4 years


data.anpp.summary <- data.anpp2%>%
  ddply(.(site_code, year, n_treat_years, map),
        function(x)data.frame(
          mean_mass = mean(x$mass),
          ppt.1 = mean(x$ppt.1),
          anpp_response = mean(x$anpp_response),
          anpp_response.error = qt(0.975, df=length(x$habitat.type)-1)*sd(x$anpp_response, na.rm = TRUE)/sqrt(length(x$habitat.type)-1),
          n_treat_days = mean(x$n_treat_days)
        ))%>%
  subset(n_treat_years >= 1 & n_treat_years<= 3)%>% #CHANGE HERE IF YOU"RE GOING UP TO 4 TREATMENT YEARS
  left_join(Site_Elev.Disturb, by = "site_code")%>%
  dplyr::select(site_code, latitud, longitud, map)%>%
  unique()
  
length(unique(data.anpp.summary$site_code))



####MAKE THE MAP
ggplot(data = world) +
  geom_sf()+
  xlab("Longitude") + ylab("Latitude") +
  geom_point(data = data.anpp.summary, aes(x = longitud, y = latitud, fill = map), size = 4, pch = 21, alpha = 0.7)+
  paletteer::scale_fill_paletteer_c("viridis::plasma", direction = -1)





