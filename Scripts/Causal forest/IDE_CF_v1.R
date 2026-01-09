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
library(ggbeeswarm)
set.seed(77)

#read ANPP data
data.anpp <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/anpp_ppt_2025-10-20.csv")%>% 
  subset(habitat.type == "Grassland" | habitat.type == "Shrubland" | habitat.type == "Forest understory" | habitat.type == "")%>%
  mutate(n_treat_years = ifelse(site_code == "allmendo.ch" & n_treat_days == 197, 1, n_treat_years))%>%
  mutate(n_treat_years = ifelse(site_code == "allmendb.ch" & n_treat_days == 197, 1, n_treat_years))%>%
  mutate(n_treat_years = ifelse(site_code == "torla.es" & n_treat_days == 195, 1, n_treat_years))%>%
  subset(n_treat_years >=1 & n_treat_years <= 4)

length(unique(data.anpp$site_code)) #121

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
  subset(habitat.type == "Grassland" | habitat.type == "Shrubland" | habitat.type == "Forest understory" | habitat.type == "")%>%
  left_join(prop, by = c("site_code"))#%>%
#  subset(site_code != "stubai.at")%>%#stubai is not a year-round drought so shouldn't be compared against these other sites
#  subset(site_code != "sclaudio.ar") #sclaudio missed Y3 sampling (2020)
#subset(site_code == "allmendo.ch" & site_code == "allmendb.ch" & site_code == "torla.es") #allmendo, allmendb, and torla have first treatment dates in the 190s, sclaudio missed Y3 sampling (2020)

setdiff(data.anpp$site_code,data.anpp1$site_code) #"brandjberg.dk" "garraf.es"  "swift.ca" eliminated here
length(unique(data.anpp1$site_code)) #118

##How many treatment years does each site have of the first 3 years?
num.treat.years <- data.anpp1[,c("site_code", "n_treat_years")]%>%
  unique()%>%
  subset(n_treat_years>=1&n_treat_years<=4)

num.treat.years <- ddply(num.treat.years,.(site_code),
                         function(x)data.frame(
                           num.years = length(x$n_treat_years)
                         ))


##Create anpp_response and drought severity metrics
data.anpp1$relprecip.1 <- -((data.anpp1$ppt.1-data.anpp1$map)/data.anpp1$map)
data.anpp1$relprecip.2 <- -((data.anpp1$ppt.2-data.anpp1$map)/data.anpp1$map)
data.anpp1$relprecip.3 <- -((data.anpp1$ppt.3-data.anpp1$map)/data.anpp1$map)
data.anpp1$relprecip.4 <- -((data.anpp1$ppt.4-data.anpp1$map)/data.anpp1$map)

length(unique(data.anpp1$site_code))#number of sites
#number of sites with soil N

############################
#####Look at overall treatment effect in a way that shows there's tons of variability in response
te <- data.anpp1%>%
  group_by(site_code, trt, n_treat_years)%>%
  dplyr::summarize(ANPP = mean(mass))

ggplot(te, aes(trt, ANPP))+
  geom_violin()+
  geom_beeswarm(alpha = 0.1)+
  geom_point(data = te %>% group_by(trt)%>%dplyr::summarize(mean.anpp = mean(ANPP)), aes(trt,mean.anpp), size = 2.5, color = "red", pch = 21)+
  theme_base()

###############################
###Precip lags with causal forest
drt.sev <- data.anpp1%>%
  ungroup()%>%
  subset(trt == "Drought")%>%
  dplyr::select(site_code, n_treat_years, relprecip.1, relprecip.2, relprecip.3, relprecip.4)%>%
  group_by(site_code, n_treat_years)%>%
  dplyr::summarize(drtsev.1 = mean(relprecip.1),
                   drtsev.2 = mean(relprecip.2),
                   drtsev.3 = mean(relprecip.3),
                   drtsev.4 = mean(relprecip.4))


# As outcomes we'll look at the number of correct answers.
Y <- te$ANPP
W <- te%>%
  ungroup()%>%
  mutate(trt_num = ifelse(trt=="Control", 0, 1))%>%
  pull(trt_num)
#W <- dist.df$relprecip.1
#X <- dist.df%>%
#  ungroup()%>%
#  dplyr::select(relprecip.1,relprecip.2,relprecip.3,relprecip.4)
X <- te%>%
  ungroup()%>%
  left_join(drt.sev, by = c("site_code", "n_treat_years"))%>%
  dplyr::select(drtsev.1,drtsev.2,drtsev.3,drtsev.4)

rf <- regression_forest(X, W, num.trees = 5000)
p.hat <- predict(rf)$predictions

hist(p.hat)

Y.forest <- regression_forest(X, Y) #this function doesn't know the treatment but that's the whole point
Y.hat <- predict(Y.forest)$predictions

varimp.Y <- variable_importance(Y.forest)

# Keep the top 10 variables for CATE estimation
keep <- colnames(X)[order(varimp.Y, decreasing = TRUE)[1:4]]
keep
#[1] "drtsev.2" "drtsev.1" "drtsev.3" "drtsev.4"

X.cf <- X[, keep]
W.hat <- 0.5

# Set aside the first half of the data for training and the second for evaluation.
# (Note that the results may change depending on which samples we hold out for training/evaluation)
train <- sample(1:nrow(X.cf), size = floor(0.5 * nrow(X.cf)))#random sample of data to train instead of jut the first half of the dataset

train.forest <- causal_forest(X.cf[train, ], Y[train], W[train], Y.hat = Y.hat[train], W.hat = W.hat)
tau.hat.eval <- predict(train.forest, X.cf[-train, ])$predictions

eval.forest <- causal_forest(X.cf[-train, ], Y[-train], W[-train], Y.hat = Y.hat[-train], W.hat = W.hat)

average_treatment_effect(eval.forest)
# estimate   std.err 
#-12.38185  17.11393  

varimp <- variable_importance(eval.forest)
ranked.vars <- order(varimp, decreasing = TRUE)
colnames(X.cf)[ranked.vars[1:4]]
#[1] "drtsev.1" "drtsev.2" "drtsev.3" "drtsev.4"

rate.cate <- rank_average_treatment_effect(eval.forest, list(cate = -1 *tau.hat.eval))
#rate.age <- rank_average_treatment_effect(eval.forest, list(map = X[-train, "map"]))

plot(rate.cate, ylab = "Number of correct answers", main = "TOC: By most negative CATEs")
#plot(rate.age, ylab = "Number of correct answers", main = "TOC: By decreasing map")

#xvars <- c("ppt.1", "ppt.2", "ppt.3", "ppt.4", "n_treat_days", "n_treat_years", "map", "arid", "PctAnnual", "PctGrass", "sand_mean", "AI", "cv_ppt_inter", "richness", "seasonality_index", "r_monthly_t_p")
imp <- sort(setNames(variable_importance(eval.forest), keep))
#par(mai = c(0.7, 2, 0.2, 0.2))
barplot(imp, horiz = TRUE, las = 1, col = "orange")
ggplot(rownames_to_column(data.frame(imp))%>%dplyr::mutate(rowname = dplyr::recode(rowname, drtsev.1 = "Drought severity 1",drtsev.2 = "Drought severity 2",drtsev.3 = "Drought severity 3",drtsev.4 = "Drought severity 4")),aes(rowname,imp))+
  geom_bar(stat="identity")+
  coord_flip()+
  ylab("Variable importance")+
  xlab("")+
  theme_base()

ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/lag_variable-importance.pdf",
        plot = last_plot(),
        device = "pdf",
        path = NULL,
        scale = 1,
        width = 4,
        height = 3,
        units = c("in"),
        dpi = 600,
        limitsize = TRUE
)



pred_fun <- function(object, newdata, ...) {
  predict(object, newdata, ...)$predictions
}
library(hstats)
pdps <- lapply(colnames(X.cf[-train, ]), function(v) plot(partial_dep(eval.forest, v=v, X = X.cf[-train, ], pred_fun = pred_fun
)))
library(patchwork)


wrap_plots(list(pdps[[2]],pdps[[1]],pdps[[3]],pdps[[4]]), guides = "collect", ncol = 4) &
  #  ylim(c(-0.11, -0.06)) &
  ylab("Treatment effect")

ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/lag_treatmenteffects_predictions.pdf",
        plot = last_plot(),
        device = "pdf",
        path = NULL,
        scale = 1,
        width = 12,
        height = 3,
        units = c("in"),
        dpi = 600,
        limitsize = TRUE
)

H <- hstats(eval.forest, X = X, pred_fun = pred_fun, verbose = FALSE)
plot(H)
#partial_dep(eval.forest, v = "map", X = X, pred_fun = pred_fun) |> 
# plot()

partial_dep(eval.forest, v = colnames(X.cf[-train, ]), X = X.cf[-train, ])

# Explaining one CATE
kernelshap(eval.forest, X = X.cf[-train, ], bg_X = X, 
           pred_fun = pred_fun) |> 
  shapviz() |> 
  sv_waterfall() +
  xlab("Prediction")


# Explaining all CATEs globally
system.time(  # 13 min
  ks <- kernelshap(eval.forest, X = X.cf[-train, ], pred_fun = pred_fun)  
)
shap_values <- shapviz(ks)
sv_importance(shap_values)
sv_importance(shap_values, kind = "bee")
#sv_dependence(shap_values, v = xvars) +
#  plot_layout(ncol = 3) &
#  ylim(c(-0.04, 0.03))


#######################################################################
###Causal forest for moderators
##FIRST JUST THE TOP RATED MODERATORS
# As outcomes we'll look at the number of correct answers.

##Top-rankd moderators from survey
#Seasonality 
#Soil N 
#Mean annual precipitation 
#Richness (pre-treatment) 
#Functional group composition (pre-treatment) 
#Species composition (pre-treatment) 
#Interannual precipitation variability 
#Soil texture 
#Aridity 


#First step, merge in moderators
climate <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/climate/climate_mean_annual_by_site_v3.csv")

prop <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/community_comp/Prc_LifeHistory_Controls_Oct2023.csv")

site_richness <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/site_ave_richness.csv")
#soil variables for soil variation
soil <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/IDE_soil_2024-12-16.csv")%>%
  group_by(site_code)%>%
  dplyr::summarize(ph = mean(ph, na.rm = TRUE), no3 = mean(no3, na.rm = TRUE), p = mean(p, na.rm = TRUE), k = mean(k, na.rm = TRUE), zn = mean(zn, na.rm = TRUE), fe = mean(fe, na.rm = TRUE), mn = mean(mn, na.rm = TRUE), cu = mean(cu, na.rm = TRUE), sand = mean(sand, na.rm = TRUE), silt = mean(silt, na.rm = TRUE), clay = mean(clay, na.rm = TRUE), c = mean(c, na.rm = TRUE), n = mean(n, na.rm = TRUE), c_n = mean(c_n, na.rm = TRUE))
length(unique(subset(soil, n>0)$site_code))#number of site with N data


sand.df <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/climate/site_sand_from_soilgrid.csv")
length(unique(subset(sand.df, sand_mean>0)$site_code))#number of site with N data

comm <- read.csv("C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/data/comm_moderators.csv")#%>%
  #subset(n_treat_years.x == 0)%>%
  #group_by(site_code)%>%
  #dplyr::summarize(Domcover = mean(Domcover), Rarecover = mean(Rarecover), Subordcover = mean(Subordcover), PerForbCover = mean(PerForbCover), AnnualGrassCover = mean(AnnualGrassCover), PerenGrassCover = mean(PerenGrassCover), C3Cover = mean(C3Cover), C4Cover = mean(C4Cover), CAMCover = mean(CAMCover), AnnualForbCover = mean(AnnualForbCover), WoodyPercentcover = mean(WoodyPercentcover.yr), GrassPercentcover = mean(GrassPercentcover.yr), ForbPercentcover = mean(ForbPercentcover.yr), INTcover = mean(INTcover), Native_cover = mean(Native_cover.yr), AnnualPercentcover = mean(AnnualPercentcover.yr))
length(unique(comm$site_code))

te1 <- te%>%
  left_join(climate, by = "site_code")%>%
  left_join(prop, by = "site_code")%>%
  left_join(soil, by = "site_code")%>%
  left_join(site_richness, by = "site_code")%>%
  left_join(sand.df, by = "site_code")%>%
  left_join(comm, by = "site_code")

length(unique(subset(te1, mean_sr >0)$site_code))
length(unique(subset(te1, PerenGrassCover >-1)$site_code))
length(unique(subset(te1, ph >-1)$site_code))
length(unique(subset(te1, p >-1)$site_code))
length(unique(subset(te1, k >-1)$site_code))
length(unique(subset(te1, zn >-1)$site_code))



#seasonality_index, n, MAP, mean_sr, cv_ppt_inter, sand_mean, aridity_index, PerenGrassCover,#Functional group composition (pre-treatment) 
#Domcover

Y <- te1$ANPP
W <- te1%>%
  ungroup()%>%
  mutate(trt_num = ifelse(trt=="Control", 0, 1))%>%
  pull(trt_num)
#W <- dist.anpp$relprecip.1
#X <- dist.anpp%>%
#  ungroup()%>%
#  dplyr::select(relprecip.1,relprecip.2,relprecip.3,relprecip.4)
X <- te1%>%
  ungroup()%>%
  dplyr::select(seasonality_index, n, MAP, mean_sr, cv_ppt_inter, sand_mean, aridity_index, 
                PerenGrassCover,#Functional group composition (pre-treatment) 
                Domcover#Species composition (pre-treatment) 
  )#put just the moderators you're testing here



rf <- regression_forest(X, W, num.trees = 5000)
p.hat <- predict(rf)$predictions

hist(p.hat)

Y.forest <- regression_forest(X, Y) #this function doesn't know the treatment but that's the whole point
Y.hat <- predict(Y.forest)$predictions

varimp.Y <- variable_importance(Y.forest)

# Keep the top 10 variables for CATE estimation
keep <- colnames(X)[order(varimp.Y, decreasing = TRUE)[1:9]]
keep
#[1] "MAP"               "aridity_index"     "sand_mean"         "mean_sr"           "cv_ppt_inter"     [6] "seasonality_index" "n"                 "PerenGrassCover"   "Domcover"         

X.cf <- X[, keep]
W.hat <- 0.5

# Set aside the first half of the data for training and the second for evaluation.
# (Note that the results may change depending on which samples we hold out for training/evaluation)
train <- sample(1:nrow(X.cf), size = floor(0.5 * nrow(X.cf)))#random sample of data to train instead of jut the first half of the dataset

train.forest <- causal_forest(X.cf[train, ], Y[train], W[train], Y.hat = Y.hat[train], W.hat = W.hat)
tau.hat.eval <- predict(train.forest, X.cf[-train, ])$predictions

eval.forest <- causal_forest(X.cf[-train, ], Y[-train], W[-train], Y.hat = Y.hat[-train], W.hat = W.hat)

average_treatment_effect(eval.forest)
#estimate   std.err 
#-21.93045  11.27195  

varimp <- variable_importance(eval.forest)
ranked.vars <- order(varimp, decreasing = TRUE)
colnames(X.cf)[ranked.vars[1:9]]
#[1] "sand_mean"         "mean_sr"           "cv_ppt_inter"      "aridity_index"    
#[5] "MAP"               "seasonality_index" "n"                 "PerenGrassCover"  
#[9] "Domcover"        

rate.cate <- rank_average_treatment_effect(eval.forest, list(cate = -1 *tau.hat.eval))
#rate.age <- rank_average_treatment_effect(eval.forest, list(map = X[-train, "map"]))

plot(rate.cate, ylab = "Number of correct answers", main = "TOC: By most negative CATEs")
#plot(rate.age, ylab = "Number of correct answers", main = "TOC: By decreasing map")

#xvars <- c("ppt.1", "ppt.2", "ppt.3", "ppt.4", "n_treat_days", "n_treat_years", "map", "arid", "PctAnnual", "PctGrass", "sand_mean", "AI", "cv_ppt_inter", "richness", "seasonality_index", "r_monthly_t_p")
imp <- sort(setNames(variable_importance(eval.forest), keep))
#par(mai = c(0.7, 2, 0.2, 0.2))
barplot(imp, horiz = TRUE, las = 1, col = "orange")
ggplot(rownames_to_column(data.frame(imp))#%>%dplyr::mutate(rowname = dplyr::recode(rowname, proportion.nestedness = "Proportion nestedness",PctGrass = "% grass",PctAnnual = "% annual",MAT = "Mean annual temperature",map = "Mean annual precipitation",gamma_rich = "Gamma richness",cv_ppt_inter = "Interannual precipitaiton variability",bp_dominance = "Berger-Parker Dominance",aridity_index = "Aridity",anpp = "Site ANPP"))
       ,aes(fct_reorder(rowname,imp),imp))+
  geom_bar(stat="identity")+
  coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  theme_base()

ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/moderators_variable-importance.pdf",
        plot = last_plot(),
        device = "pdf",
        path = NULL,
        scale = 1,
        width = 6,
        height = 3,
        units = c("in"),
        dpi = 600,
        limitsize = TRUE
)


pred_fun <- function(object, newdata, ...) {
  predict(object, newdata, ...)$predictions
}
library(hstats)
pdps <- lapply(colnames(X.cf[-train, ]), function(v) plot(partial_dep(eval.forest, v=v, X = X.cf[-train, ], pred_fun = pred_fun
)))
library(patchwork)
wrap_plots(pdps, guides = "collect", ncol = 3) &
  #  ylim(c(-0.11, -0.06)) &
  ylab("Treatment effect")


ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/moderator_treatmenteffects_predictions.pdf",
        plot = last_plot(),
        device = "pdf",
        path = NULL,
        scale = 1,
        width = 14,
        height = 6,
        units = c("in"),
        dpi = 600,
        limitsize = TRUE
)


#H <- hstats(eval.forest, X = X, pred_fun = pred_fun, verbose = FALSE)
#plot(H)
#partial_dep(eval.forest, v = "map", X = X, pred_fun = pred_fun) |> 
# plot()

partial_dep(eval.forest, v = colnames(X.cf[-train, ]), X = X.cf[-train, ])

# Explaining one CATE
kernelshap(eval.forest, X = X.cf[-train, ], bg_X = X, 
           pred_fun = pred_fun) |> 
  shapviz() |> 
  sv_waterfall() +
  xlab("Prediction")

# Explaining all CATEs globally
system.time(  # 13 min
  ks <- kernelshap(eval.forest, X = X.cf[-train, ], pred_fun = pred_fun)  
)
shap_values <- shapviz(ks)
sv_importance(shap_values)
sv_importance(shap_values, kind = "bee")
#sv_dependence(shap_values, v = xvars) +
#  plot_layout(ncol = 3) &
#  ylim(c(-0.04, 0.03))



######################################
#######Moderators: kitchen sink approach

Y <- te1$ANPP
W <- te1%>%
  ungroup()%>%
  mutate(trt_num = ifelse(trt=="Control", 0, 1))%>%
  pull(trt_num)
#W <- dist.anpp$relprecip.1
#X <- dist.anpp%>%
#  ungroup()%>%
#  dplyr::select(relprecip.1,relprecip.2,relprecip.3,relprecip.4)
X <- te1%>%
  ungroup()%>%
  dplyr::select(ppt_max_event, ppt_mean_event, days_half_ppt, daily_ppt_d, n_wet_days, avg_dryspell_length, ppt_95th_percentile_size, MAP, cv_ppt_intra, cv_ppt_inter, yearly_ppt_d, seasonality_index, aridity_index, r_monthly_t_p, MAT, #PctAnnual, PctGrass,
                ph, no3, p, k, zn, fe, mn, cu, sand, silt, clay, c, n, c_n, mean_sr, sand_mean, sand0_5, sand5_15, sand15_30, sand30_60, sand60_100, Domcover, Rarecover, Subordcover, PerForbCover, AnnualGrassCover, PerenGrassCover, C3Cover, C4Cover, CAMCover, AnnualForbCover, WoodyPercentcover, GrassPercentcover, ForbPercentcover, INTcover, Native_cover, AnnualPercentcover) #put just the moderators you're testing here




rf <- regression_forest(X, W, num.trees = 50000)
p.hat <- predict(rf)$predictions

hist(p.hat)

Y.forest <- regression_forest(X, Y) #this function doesn't know the treatment but that's the whole point
Y.hat <- predict(Y.forest)$predictions

varimp.Y <- variable_importance(Y.forest)

# Keep the top 10 variables for CATE estimation
keep <- colnames(X)[order(varimp.Y, decreasing = TRUE)[1:10]]
keep
#[1] "MAP"                      "aridity_index"            "avg_dryspell_length"    #[4] "daily_ppt_d"              "ppt_max_event"            "sand_mean"              #[7] "cv_ppt_inter"             "ppt_95th_percentile_size" "mean_sr"                #[10] "sand0_5"       

X.cf <- X[, keep]
W.hat <- 0.5

# Set aside the first half of the data for training and the second for evaluation.
# (Note that the results may change depending on which samples we hold out for training/evaluation)
train <- sample(1:nrow(X.cf), size = floor(0.5 * nrow(X.cf)))#random sample of data to train instead of jut the first half of the dataset

train.forest <- causal_forest(X.cf[train, ], Y[train], W[train], Y.hat = Y.hat[train], W.hat = W.hat)
tau.hat.eval <- predict(train.forest, X.cf[-train, ])$predictions

eval.forest <- causal_forest(X.cf[-train, ], Y[-train], W[-train], Y.hat = Y.hat[-train], W.hat = W.hat)

average_treatment_effect(eval.forest)
#estimate   std.err 
#-39.90510  11.74852  

varimp <- variable_importance(eval.forest)
ranked.vars <- order(varimp, decreasing = TRUE)
colnames(X.cf)[ranked.vars[1:10]]
#[1] "sand_mean"                "sand0_5"                  "mean_sr"                #[4] "ppt_max_event"            "MAP"                      "aridity_index"           #[7] "cv_ppt_inter"             "ppt_95th_percentile_size" "daily_ppt_d"             #[10] "avg_dryspell_length"      

rate.cate <- rank_average_treatment_effect(eval.forest, list(cate = -1 *tau.hat.eval))
#rate.age <- rank_average_treatment_effect(eval.forest, list(map = X[-train, "map"]))

plot(rate.cate, ylab = "Number of correct answers", main = "TOC: By most negative CATEs")
#plot(rate.age, ylab = "Number of correct answers", main = "TOC: By decreasing map")

#xvars <- c("ppt.1", "ppt.2", "ppt.3", "ppt.4", "n_treat_days", "n_treat_years", "map", "arid", "PctAnnual", "PctGrass", "sand_mean", "AI", "cv_ppt_inter", "richness", "seasonality_index", "r_monthly_t_p")
imp <- sort(setNames(variable_importance(eval.forest), keep))
#par(mai = c(0.7, 2, 0.2, 0.2))
barplot(imp, horiz = TRUE, las = 1, col = "orange")
ggplot(rownames_to_column(data.frame(imp))#%>%dplyr::mutate(rowname = dplyr::recode(rowname, proportion.nestedness = "Proportion nestedness",PctGrass = "% grass",PctAnnual = "% annual",MAT = "Mean annual temperature",map = "Mean annual precipitation",gamma_rich = "Gamma richness",cv_ppt_inter = "Interannual precipitaiton variability",bp_dominance = "Berger-Parker Dominance",aridity_index = "Aridity",anpp = "Site ANPP"))
       ,aes(fct_reorder(rowname,imp),imp))+
  geom_bar(stat="identity")+
  coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  theme_base()

ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/moderators_variable-importance_kitchensink.pdf",
        plot = last_plot(),
        device = "pdf",
        path = NULL,
        scale = 1,
        width = 6,
        height = 3,
        units = c("in"),
        dpi = 600,
        limitsize = TRUE
)




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


ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/moderator_treatmenteffects_predictions_kitchensink.pdf",
        plot = last_plot(),
        device = "pdf",
        path = NULL,
        scale = 1,
        width = 14,
        height = 6,
        units = c("in"),
        dpi = 600,
        limitsize = TRUE
)


#H <- hstats(eval.forest, X = X, pred_fun = pred_fun, verbose = FALSE)
#plot(H)
#partial_dep(eval.forest, v = "map", X = X, pred_fun = pred_fun) |> 
# plot()

partial_dep(eval.forest, v = colnames(X.cf[-train, ]), X = X.cf[-train, ])

# Explaining one CATE
kernelshap(eval.forest, X = X.cf[-train, ], bg_X = X, 
           pred_fun = pred_fun) |> 
  shapviz() |> 
  sv_waterfall() +
  xlab("Prediction")

# Explaining all CATEs globally
system.time(  # 13 min
  ks <- kernelshap(eval.forest, X = X.cf[-train, ], pred_fun = pred_fun)  
)
shap_values <- shapviz(ks)
sv_importance(shap_values)
sv_importance(shap_values, kind = "bee")
#sv_dependence(shap_values, v = xvars) +
#  plot_layout(ncol = 3) &
#  ylim(c(-0.04, 0.03))





