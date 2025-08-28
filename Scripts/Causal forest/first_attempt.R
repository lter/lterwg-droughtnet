##try causal forest IDE


library(grf)
library(tidyverse)
library(plyr)
library(lmerTest)
library(nlme)
library(visreg)
library(MuMIn)
library(ggthemes)
library(ggeffects)
library(MASS)
library(cowplot)
#library(rsq)
library(emmeans)
library(kernelshap)   #  General SHAP
library(shapviz)      #  SHAP plots

#read ANPP data
data.anpp <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/anpp_ppt_2025-05-19.csv")%>% #anpp_ppt_2023-11-03.csv
  subset(habitat.type == "Grassland" | habitat.type == "Shrubland")%>%
  mutate(n_treat_years = ifelse(site_code == "allmendo.ch" & n_treat_days == 197, 1, n_treat_years))%>%
  mutate(n_treat_years = ifelse(site_code == "allmendb.ch" & n_treat_days == 197, 1, n_treat_years))%>%
  mutate(n_treat_years = ifelse(site_code == "torla.es" & n_treat_days == 195, 1, n_treat_years))

length(unique(data.anpp$site_code)) #114

prop <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/community_comp/Prc_LifeHistory_Controls_Oct2023.csv")

Site_Elev.Disturb <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/Site_Elev-Disturb.csv")%>%
  dplyr::select(site_code, latitud, longitud )

#create long-term average ANPP in controls
anpp.mean <- data.anpp%>%
  subset(trt == "Control")%>%
  ddply(.(site_code, year),function(x)data.frame(mass = mean(x$mass)))%>% #summarizes controls for each year
  ddply(.(site_code),function(x)data.frame(mean.mass = mean(x$mass), n_years = length(x$mass)))#summarizes controls across years


##Calculate which years are extreme vs nominal for each site in each year
extremeyrs <- subset(data.anpp, trt == "Control")%>%
  dplyr::select(site_code, n_treat_years, year, ppt.1, map)%>%
  unique() %>%
  mutate(ppt.minus.map=ppt.1-map,
         e.n=ifelse(n_treat_years <1, NA,
                    ifelse(ppt.minus.map>0, "nominal", "extreme"))) %>%
  dplyr::select(site_code, year, n_treat_years, e.n)

extremeyrs.prev <- extremeyrs%>%
  dplyr::select(site_code, year, e.n)%>%
  dplyr::rename(prev_e.n = e.n)
extremeyrs.prev$year <- extremeyrs.prev$year + 1


extremeyrs.prev2 <- extremeyrs%>%
  dplyr::select(site_code, year, e.n)%>%
  dplyr::rename(prev_e.n2 = e.n)
extremeyrs.prev2$year <- extremeyrs.prev$year + 1 #had some trouble with these addition things but I think it's fine now

extremeyrs.prev3 <- extremeyrs%>%
  dplyr::select(site_code, year, e.n)%>%
  dplyr::rename(prev_e.n3 = e.n)
extremeyrs.prev3$year <- extremeyrs.prev$year + 2

#Only using sites with >= 2 reps for drought and >=1 rep for control
#Counting the number of reps for each treatment and year
uniq.plot<- data.anpp %>% 
  dplyr::filter(trt %in% c("Drought","Control"))%>%
  dplyr::select(site_code,year,plot,trt)%>%
  dplyr::distinct(site_code,year,plot,trt)%>%
  dplyr::as_tibble()

Plot.trt.ct <- uniq.plot%>% dplyr::group_by(site_code,trt,year) %>% dplyr::summarise(Plot.count=dplyr::n())

#Switching to wide format to see which sites do not have both treatments in a year
Plottrt_wide<-tidyr::spread(Plot.trt.ct,trt,Plot.count)
Plottrt_wide[is.na(Plottrt_wide)] <- 0

#Remove sites and years that don't have both control and drought plots
#or that only have one rep of drought
Plottrt_wide1<-Plottrt_wide%>%
  dplyr::filter(Drought>=2 & Control>=1)%>%
  dplyr::as_tibble()

#Switch back to long format for merge
Plot.trt.ct2<-tidyr::gather(Plottrt_wide1,trt,Plot.count,Drought:Control)

#Merge unique trt count back with data frame to filter
data.anpp1<-merge(data.anpp,Plot.trt.ct2,by=c("site_code","trt","year"))%>%
  subset(habitat.type == "Grassland")%>%
  left_join(prop, by = c("site_code"))%>%
  subset(site_code != "stubai.at")%>%#stubai is not a year-round drought so shouldn't be compared against these other sites
  subset(site_code != "sclaudio.ar") #sclaudio missed Y3 sampling (2020)
#subset(site_code == "allmendo.ch" & site_code == "allmendb.ch" & site_code == "torla.es") #allmendo, allmendb, and torla have first treatment dates in the 190s, sclaudio missed Y3 sampling (2020)

setdiff(data.anpp$site_code,data.anpp1$site_code) #"brandjberg.dk" "garraf.es"  "swift.ca" eliminated here
length(unique(data.anpp1$site_code)) #111

##How many treatment years does each site have of the first 3 years?
num.treat.years <- data.anpp1[,c("site_code", "n_treat_years")]%>%
  unique()%>%
  subset(n_treat_years>=1&n_treat_years<=4)

num.treat.years <- ddply(num.treat.years,.(site_code),
                         function(x)data.frame(
                           num.years = length(x$n_treat_years)
                         ))


##Create anpp_response and drought severity metrics
data.anpp1$drtsev.1 <- -((data.anpp1$ppt.1-data.anpp1$map)/data.anpp1$map)
data.anpp1$drtsev.2 <- -((data.anpp1$ppt.2-data.anpp1$map)/data.anpp1$map)
data.anpp1$drtsev.3 <- -((data.anpp1$ppt.3-data.anpp1$map)/data.anpp1$map)
data.anpp1$drtsev.4 <- -((data.anpp1$ppt.4-data.anpp1$map)/data.anpp1$map)


##add some covariates to try out in model selection
#sand, MAP, ln(aridity), CV of MAP--- %graminoids is tricky, skpping for now
sandsite <-read.csv("C:\\Users\\ohler\\Dropbox\\IDE MS_Single year extreme\\Data\\site_sand_from_soilgrid.csv")%>%
  dplyr::select(site_code, sand_mean)

ai<-read.csv("C:\\Users\\ohler\\Dropbox\\IDE MS_Single year extreme\\Data\\ai_pet_04-18-2022.csv")%>%
  dplyr::select(site_code, AI)

cv<-read.csv("C:\\Users\\ohler\\Dropbox\\IDE\\data_processed\\climate\\climate_mean_annual_by_site_v3.csv")
cv1<-cv%>%
  #dplyr::filter(data_source=="mswep")%>%
  dplyr::select(site_code,cv_ppt_inter)

graminoid_richness <-read.csv("C:\\Users\\ohler\\Dropbox\\IDE\\data_processed\\graminoids_and_richness.csv")

seasonality <- read.csv("C:\\Users\\ohler\\Dropbox\\IDE\\data_processed\\climate\\climate_mean_annual_by_site_v2.csv")%>%
  dplyr::select(site_code, seasonality_index, r_monthly_t_p)



##Create planeled figure for supplements (and some stats to go along with them)
mult_reg <- data.anpp1%>%
  left_join(sandsite, by = "site_code")%>%
  left_join(ai, by = "site_code")%>%
  left_join(cv1, by = "site_code")%>%
  left_join(graminoid_richness, by = "site_code")%>%
  left_join(seasonality, by = "site_code")%>%
  left_join(Site_Elev.Disturb, by = "site_code")%>%
  mutate(trt_num = ifelse(trt=="Control", 0, 1))%>%
  group_by(latitud, longitud, trt_num, ppt.1, ppt.2, ppt.3, ppt.4, n_treat_days, n_treat_years, map, arid, PctAnnual, PctGrass, sand_mean, AI, cv_ppt_inter, richness, seasonality_index, r_monthly_t_p)%>%
  dplyr::summarise(mass = mean(mass))


#data.anpp1 <- data.anpp1%>%
#              mutate(trt_num = ifelse(trt=="Control", 0, 1))%>%
#              group_by(site_code, trt_num, ppt.1, ppt.2, ppt.3, ppt.4, n_treat_days, n_treat_years, map, arid, PctAnnual, PctGrass, sand_mean, AI, cv_ppt_inter, richness, seasonality_index, r_monthly_t_p)%>%
#              dplyr::summarise(mass = mean(mass))

X <- mult_reg%>%
  ungroup()%>%
  dplyr::select(ppt.1, ppt.2, ppt.3, ppt.4, n_treat_days, n_treat_years, map,  PctAnnual, PctGrass, sand_mean, AI, cv_ppt_inter, richness, seasonality_index, r_monthly_t_p)

tau.forest <- causal_forest(X, Y = mult_reg$mass, W = mult_reg$trt_num, num.trees = 4000)
#

tau.hat.oob <- predict(tau.forest)
hist(tau.hat.oob$predictions)


tau.hat <- predict(tau.forest, estimate.variance = TRUE)
plot(mult_reg$trt_num, tau.hat$predictions, ylim = range(tau.hat$predictions, 0, 2), xlab = "x", ylab = "tau", type = "l")
lines(mult_reg$trt_num, pmax(0, mult_reg$trt_num), col = 2, lty = 2)


average_treatment_effect(tau.forest, target.sample = "all")



average_treatment_effect(tau.forest, target.sample = "treated")




####starting some other workflow below

# As outcomes we'll look at the number of correct answers.
Y <- mult_reg$mass
W <- mult_reg$trt_num
X <- X



rf <- regression_forest(X, W, num.trees = 5000)
p.hat <- predict(rf)$predictions

hist(p.hat)



Y.forest <- regression_forest(X, Y) #this function doesn't know the treatment but that's the whole point
Y.hat <- predict(Y.forest)$predictions

varimp.Y <- variable_importance(Y.forest)

# Keep the top 10 variables for CATE estimation
keep <- colnames(X)[order(varimp.Y, decreasing = TRUE)[1:10]]
keep
#[1] "map"               "AI"                "sand_mean"         "arid"             
#[5] "seasonality_index" "ppt.1"             "PctAnnual"         "PctGrass"         
#[9] "ppt.3"             "ppt.2"  



X.cf <- X[, keep]
W.hat <- 0.5

# Set aside the first half of the data for training and the second for evaluation.
# (Note that the results may change depending on which samples we hold out for training/evaluation)
train <- 1:(nrow(X.cf) / 2)

train.forest <- causal_forest(X.cf[train, ], Y[train], W[train], Y.hat = Y.hat[train], W.hat = W.hat)
tau.hat.eval <- predict(train.forest, X.cf[-train, ])$predictions

eval.forest <- causal_forest(X.cf[-train, ], Y[-train], W[-train], Y.hat = Y.hat[-train], W.hat = W.hat)



average_treatment_effect(eval.forest)
#estimate   std.err 
#-19.93438  10.14605 


varimp <- variable_importance(eval.forest)
ranked.vars <- order(varimp, decreasing = TRUE)
colnames(X.cf)[ranked.vars[1:5]]
#[1] "PctAnnual" "ppt.1"     "PctGrass"  "sand_mean" "ppt.3"   

rate.cate <- rank_average_treatment_effect(eval.forest, list(cate = -1 *tau.hat.eval))
rate.age <- rank_average_treatment_effect(eval.forest, list(map = X[-train, "map"]))

#par(mfrow = c(1, 2))
plot(rate.cate, ylab = "Number of correct answers", main = "TOC: By most negative CATEs")
plot(rate.age, ylab = "Number of correct answers", main = "TOC: By decreasing map")

#xvars <- c("ppt.1", "ppt.2", "ppt.3", "ppt.4", "n_treat_days", "n_treat_years", "map", "arid", "PctAnnual", "PctGrass", "sand_mean", "AI", "cv_ppt_inter", "richness", "seasonality_index", "r_monthly_t_p")
imp <- sort(setNames(variable_importance(eval.forest), keep))
#par(mai = c(0.7, 2, 0.2, 0.2))
barplot(imp, horiz = TRUE, las = 1, col = "orange")

pred_fun <- function(object, newdata, ...) {
  predict(object, newdata, ...)$predictions
}
library(hstats)
pdps <- lapply(colnames(X.cf[-train, ]), function(v) plot(partial_dep(eval.forest, v=v, X = X.cf[-train, ], pred_fun = pred_fun
                                                                      )))
library(patchwork)
wrap_plots(pdps, guides = "collect", ncol = 5) &
#  ylim(c(-0.11, -0.06)) &
  ylab("Treatment effect")

#H <- hstats(eval.forest, X = X, pred_fun = pred_fun, verbose = FALSE)
#plot(H)
#partial_dep(eval.forest, v = "map", X = X, pred_fun = pred_fun) |> 
#  plot()

#partial_dep(eval.forest, v = colnames(X.cf[-train, ]), X = X.cf[-train, ])


# Explaining one CATE
#kernelshap(eval.forest, X = X.cf[-train, ], #bg_X = X, 
#           pred_fun = pred_fun) |> 
#  shapviz() |> 
#  sv_waterfall() +
#  xlab("Prediction")

# Explaining all CATEs globally
system.time(  # 13 min
  ks <- kernelshap(eval.forest, X = X.cf[-train, ], pred_fun = pred_fun)  
)
shap_values <- shapviz(ks)
sv_importance(shap_values)
sv_importance(shap_values, kind = "bee")
sv_dependence(shap_values, v = xvars) +
  plot_layout(ncol = 3) &
  ylim(c(-0.04, 0.03))





#######################################below doesn't work all the way through
Y <- mult_reg$mass
Y.forest <- regression_forest(X,Y)#but how does it know the treatment designation
varimp.Y <- variable_importance(Y.forest)
colnames(X)[order(varimp.Y, decreasing = TRUE)[1:10]]
ranked.vars <- order(varimp.Y, decreasing = TRUE)



keep <- colnames(X)[order(varimp.Y, decreasing = TRUE)[1:10]]
X.cf <- X[, keep]
train <- 1:(nrow(X.cf) / 2)

W = mult_reg$trt_num
eval.forest <- causal_forest(X.cf[-train, ], Y[-train], W[-train], Y.hat = Y.hat[-train], W.hat = W.hat)
rate.map <- rank_average_treatment_effect(Y.forest, list(map = X[-train, "map"]))


plot(rate.map, ylab = "Number of correct answers", main = "By map?")













#reduces dataset to focal sites
data.anpp2$Ann_Per <- ifelse(data.anpp2$PctAnnual > 60, "Annual", 
                             ifelse(data.anpp2$PctAnnual <= 60, "Perennial",
                                    "NA"))

data.anpp2$Ann_Per <- ifelse(is.na(data.anpp2$Ann_Per) == TRUE, "Perennial", data.anpp2$Ann_Per) #morient.ar, b=nyngan.au, riomayo.ar, stubai.at, and syferkuil.za don't have cover data, but based on biomass and site info data that they submitted we can say that they are all perennial grassland.


length(unique(data.anpp2$site_code)) #74

#mean(subset(data.anpp.summary, n_treat_years == 1)$n_treat_days)






