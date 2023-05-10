library(tidyverse)
library(plyr)
library(visreg)
library(cowplot)
library(segmented)


d.fun <- function (var, k=0){
  var<-var[!is.na(var)]
  if(length(var)<3){return(NA)}
  if(sd(var)==0){return(0)}
  if (min(var)<0){var <- var + abs(min(var)) + 0.01*(max(var)-min(var))}
  if (length(var)<2){return(NA)}else{
    var <- var + k
    aux <- numeric(length(var)-1)
    for (i in 1:(length(var)-1)){
      aux [i] <- abs(log(var[i+1]/var[i]))
    }
  }
  return(mean(aux, na.rm=T))}

#climate
CRU <- read.csv("C:/Users/ohler/Dropbox/Tim Work/DroughtNet/NN_DN-CRU-1961-2019.csv")
CRU <- ddply(CRU, .(site_code, year),
             function(x)data.frame(
               ppt = sum(x$ppt)
             ))
CRU <- subset(CRU, year >= "1961" & year <= "2010")

climate_frame <- ddply(CRU, .(site_code),
                       function(x)data.frame(
                         d.var = d.fun(x$ppt),
                         MAP = mean(x$ppt),
                         cv = sd(x$ppt)/mean(x$ppt),
                         sd = sd(x$ppt)
                       ))



#DROUGHTNET

reduced_npp <- read.csv("C:\\Users\\ohler\\Dropbox\\IDE\\data_processed\\anpp_ppt_2023-05-01.csv")

temp <- reduced_npp[,c("site_code", "year")]
temp <- unique(temp)
temp <- ddply(temp, c("site_code"),
              function(x)(
                n.year=length(x$year)
              ))
temp$n.year <- temp$V1
reduced_npp <- merge(reduced_npp, temp, by = "site_code", all.x=TRUE)

#only use plots that are not manipulated
control_biomass_DN <- reduced_npp %>% 
  subset( n_treat_days < 30 | trt == "Control")%>%
  subset(n.year > 4)%>%
  ddply( c("site_code", "year", "plot", "subplot"),
         function(x)data.frame(
           biomass = sum(x$mass)
         )) %>%
  #ddply( c("site_code", "year", "plot"),
  #       function(x)data.frame(
  #        biomass = mean(x$biomass)
  #      )) %>%
  ddply( c("site_code", "year"),
         function(x)data.frame(
           biomass = mean(x$biomass)
         )) %>%
  ddply( c("site_code"),
         function(x)data.frame(
           biomass = mean(x$biomass),
           n.year = length(x$year)
         )) %>%
  merge(climate_frame, by = "site_code",all.x=TRUE)%>%
  #subset(MAP < 1500)%>%
  subset( site_code != "prades.es")%>%
  subset( site_code != "qdtnorth.cl")

control_biomass_DN <- control_biomass_DN[!is.na(control_biomass_DN$n.year),]
control_biomass_DN$network <- "DroughtNet"




#NUTNET


full.biomass <- read.csv("C:/Users/ohler/Dropbox/NutNet data/full-biomass_2023-04-26.csv")


control_biomass_NN <- full.biomass %>%
  subset(live != 0)%>%
  subset(year_trt == 0 | trt=="Control")%>%
  #block, plot, subplot
  ddply( c("site_code", "year", "plot", "subplot"),
         function(x)data.frame(
           biomass = sum(x$mass)
         )) %>%
  ddply( c("site_code", "year", "plot"),
         function(x)data.frame(
           biomass = mean(x$biomass)
         )) %>%
  ddply( c("site_code", "year"),
         function(x)data.frame(
           biomass = mean(x$biomass)
         )) %>%
  ddply( c("site_code"),
         function(x)data.frame(
           biomass = mean(x$biomass),
           n.year = length(x$year)
         )) %>%
  subset(n.year > 4)%>%
  merge(climate_frame, by = "site_code",all.x=TRUE)#%>%
#subset(MAP < 1500)

control_biomass_NN$network <- "NutNet"

dn_nn <- rbind(control_biomass_DN,control_biomass_NN)
dn_nn <- subset(dn_nn, biomass < 2000)
dn_nn <- subset(dn_nn, MAP != "NA")


##
#MAP models
map <- lm(biomass~MAP, data=dn_nn)
map.2 <- lm(biomass~MAP+I(MAP^2), data=dn_nn)
d <- lm(biomass~d.var, data=dn_nn)
map.d <- lm(biomass~MAP * d.var, data=dn_nn)
map.d.2 <- lm(biomass~(MAP+I(MAP^2)) * d.var, data=dn_nn)


AIC(map,map.2,d,map.d, map.d.2)
summary(map)
summary(map.2)
summary(d)
summary(map.d)
summary(map.d.2)


visreg(map)
visreg(map.2)
visreg2d(map.d, xvar="MAP",yvar="d.var", plot.type="gg", ylab="Variability", main="ANPP")+
  geom_point(data=dn_nn,aes(x=MAP,y=d.var))

hist(dn_nn$MAP)
ggplot(dn_nn, aes(x=MAP, y=biomass))+
  geom_smooth(method="lm",formula= (y ~ x + I(x^2)),se=T,col="black",size=2,fill = "dodgerblue", alpha=.2)+
  geom_point(aes(size=d.var, colour=network),shape=19)+
  xlim(c(0,3000))+
  ylab("ANPP (g/m2)")+
  theme_cowplot()


ggplot(dn_nn, aes(x=MAP, y=biomass))+
  geom_smooth(method="lm",formula= (y ~ x),se=T,col="black",size=2,fill = "dodgerblue", alpha=.2)+
  geom_point(aes(size=d.var, colour=network),shape=19)+
  xlim(c(0,1200))+
  ylab("ANPP (g/m2)")+
  theme_cowplot()


map.resid <- residuals(map.2)
#map.resid <- resid(map.2)
#x <- c(unname(map.resid))
dn_nn$map.resid <- map.resid
mod.resid <- lm(map.resid~d.var,data=dn_nn)
mod.resid.2 <- lm(map.resid~d.var+I(d.var^2),data=dn_nn)
AIC(mod.resid,mod.resid.2)
summary(mod.resid)
visreg(mod.resid)


pscore.test(mod.resid)
seg <- segmented(mod.resid, seg.Z=~d.var)

plot(seg)  
points(dn_nn$d.var, dn_nn$map.resid)
ylim(-400,900)

ggplot(lm(biomass~MAP, data=dn_nn)) + 
  geom_point(aes(x=dn_nn$d.var, y=.resid),color = "springgreen4")+
  geom_smooth(aes(x=dn_nn$d.var, y=.resid),method="lm",formula= (y ~ x + I(x^2)),se=T,col="black",size=2,fill = "darkseagreen3")+
  ylab("MAP model residuals")+
  xlab("D (variability)")+
  #geom_smooth(aes(x=dn_nn$d.var, y=.resid),method="lm",se=T,col="black",size=2)+
  theme_bw()

plot(dn_nn$d.var, map.resid, xlab="variability (D)",ylab="MAP regression residuals")
abline(0,0)




##MAP variability correlation
d_map<- lm(d.var~MAP, data=dn_nn)
d_map.2<- lm(d.var~MAP+I(MAP^2), data=dn_nn)
d_map.3<- lm(d.var~MAP+I(MAP^2)+I(MAP^3), data=dn_nn)
AIC(d_map,d_map.2,d_map.3)
summary(d_map.3)

visreg(d_map.3, ylab="variability (D)")
ggplot(dn_nn, aes(x=MAP, y=d.var))+
  geom_smooth(method="lm",formula= (y ~ x + I(x^2) + I(x^3)),se=T,col="black",size=2,fill = "dodgerblue", alpha=.2)+
  geom_point(aes(colour=network),shape=19, size=3)+
  xlim(c(0,2500))+
  ylab("Variability")+
  theme_cowplot()


##CV - D check
cv_d <- lm(d.var~cv, data = dn_nn)
summary(cv_d)
visreg(cv_d, ylab = "D")




##############
#Does variability matter in arid sites?


arid_sites <- subset(dn_nn, MAP<500)
mid_sites <- subset(dn_nn, MAP>400 & MAP<700)
mesic_sites <- subset(dn_nn, MAP>700 &MAP<1000)
super_mesic_sites <- subset(dn_nn, MAP>1000)

map <- lm(biomass~MAP, data=arid_sites)
map.2 <- lm(biomass~MAP+I(MAP^2), data=arid_sites)
d <- lm(biomass~d.var, data=arid_sites)
map.d <- lm(biomass~MAP * d.var, data=arid_sites)


ggplot(arid_sites, aes(d.var, biomass))+
  geom_smooth(method="lm",formula= (y ~ x),se=T,col="black",size=2,fill = "dodgerblue", alpha=.2)+
  geom_point(aes(colour=network),shape=19, size=3)+
  #xlim(c(0,2500))+
  ylab("ANPP (g/m2)")+
  xlab("variability")+
  ggtitle("1000+ mm")+
  theme_cowplot()



#map <- lm(biomass~MAP, data=dn_nn)
#map.2 <- lm(biomass~MAP+I(MAP^2), data=dn_nn)
#d <- lm(biomass~d.var, data=dn_nn)
#map.d <- lm(biomass~MAP * d.var, data=dn_nn)


AIC(map,map.2,d,map.d)
summary(map)
summary(map.2)
summary(d)
summary(map.d)

visreg(d)
visreg2d(map.d, xvar="MAP",yvar = "d.var")



ggplot(aes(d.var,biomass), data=arid_sites) + 
  geom_point(aes(size=MAP))+
  geom_smooth(method="lm",formula= (y ~ x),se=T,col="black",size=2)+
  ylab("biomass")+
  xlab("D (variability)")+
  theme_bw()




# have to provide estimates for breakpoints.
# after looking a the data, 
my.seg <- segmented(map, 
                    seg.Z = ~ MAP,
                    psi = list(MAP = c(400, 700)))

# When not providing estimates for the breakpoints "psi = NA" can be used.
# The number of breakpoints that will show up is not defined
#my.seg <- segmented(my.lm, 
#                    seg.Z = ~ DistanceMeters, 
#                    psi = NA)

# display the summary
summary(my.seg)

my.fitted <- fitted(my.seg)


###################
##Annual precipitation


#DroughtNet
annual_control_DN <- reduced_npp %>% 
  subset( n_treat_days < 30 | trt == "Control")%>%
  subset(ANPP == "Yes")%>%
  ddply( c("site_code", "year", "plot", "subplot", "ppt"),
         function(x)data.frame(
           biomass = sum(x$mass)
         )) %>%
  ddply( c("site_code", "year", "plot", "ppt"),
         function(x)data.frame(
           biomass = mean(x$biomass)
         )) %>%
  ddply( c("site_code", "year", "ppt"),
         function(x)data.frame(
           biomass = mean(x$biomass)
         )) %>%
  merge(climate_frame, by = "site_code",all=TRUE)%>%
  subset(MAP < 1200)

annual_control_DN <- annual_control_DN[!is.na(annual_control_DN$ppt),]




#NutNet
weather_annual <- read.csv("C:/Users/ohler/Dropbox/NutNet data/climate/WeatherStation/Weather_annual_20191110.csv")


annual_control_NN <- full.biomass %>%
  subset(live != 0)%>%
  subset(year_trt == 0 | trt=="Control")%>%
  #block, plot, subplot
  ddply( c("site_code", "year", "plot", "subplot"),
         function(x)data.frame(
           biomass = sum(x$mass)
         )) %>%
  ddply( c("site_code", "year", "plot"),
         function(x)data.frame(
           biomass = mean(x$biomass)
         )) %>%
  ddply( c("site_code", "year"),
         function(x)data.frame(
           biomass = mean(x$biomass)
         ))%>%
  merge(climate_frame, by = "site_code",all.x=TRUE)%>%
  merge(weather_annual, by = c("site_code","year"))%>%
  subset(MAP < 1500)

annual_control_NN <- annual_control_NN[!is.na(annual_control_NN$d.var),]




dn_nn_annual <- rbind(annual_control_DN,annual_control_NN)



ppt <- lmer(biomass~ppt+(1|site_code/year/plot), data=dn_nn_annual)
ppt.2 <- lmer(biomass~ppt+I(ppt^2)+(1|site_code),data=dn_nn_annual)
d <- lmer(biomass~d.var+(1|site_code), data=dn_nn_annual)
ppt.d <- lmer(biomass~ppt * d.var+(1|site_code), data=dn_nn_annual)


AIC(ppt,ppt.2,d,ppt.d)
summary(ppt)
summary(d)
summary(ppt.d)
visreg(ppt, ylab = "ANPP", xlab="Annual Precipitation")
visreg(d, ylab = "ANPP", xlab="variability (D)")
visreg2d(ppt.d,xvar="ppt",yvar="d.var")















