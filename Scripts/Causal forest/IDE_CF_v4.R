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
library(ggplotify)
library(emmeans)
library(kernelshap)   #  General SHAP
library(shapviz)      #  SHAP plots
library(ggbeeswarm)
library(hstats)
library(patchwork)


#read ANPP data
data.anpp <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/anpp_ppt_2026-03-27.csv")%>% 
  subset(habitat.type == "Grassland" | habitat.type == "Shrubland" | habitat.type == "Forest understory")%>%
  mutate(n_treat_years = ifelse(site_code == "allmendo.ch" & n_treat_days == 197, 1, n_treat_years))%>%
  mutate(n_treat_years = ifelse(site_code == "allmendb.ch" & n_treat_days == 197, 1, n_treat_years))%>%
  mutate(n_treat_years = ifelse(site_code == "torla.es" & n_treat_days == 195, 1, n_treat_years))%>%
  subset(n_treat_years >=1 & n_treat_years <= 4)

length(unique(data.anpp$site_code)) #121

#prop <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/community_comp/Prc_LifeHistory_Controls_Oct2023.csv")

Site_Elev.Disturb <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/Site_Elev-Disturb.csv")%>%
  dplyr::select(site_code, latitud, longitud )

#create long-term average ANPP in controls
anpp.mean <- data.anpp%>%
  subset(trt == "Control")%>%
  ddply(.(site_code, year),function(x)data.frame(mass = mean(x$mass)))%>% #summarizes controls for each year
  ddply(.(site_code),function(x)data.frame(mean.mass = mean(x$mass), n_years = length(x$mass)))#summarizes controls across years


##Calculate which years are extreme vs nominal for each site in each year
#extremeyrs <- subset(data.anpp, trt == "Control")%>%
#  dplyr::select(site_code, n_treat_years, year, ppt.1, map)%>%
#  unique() %>%
#  mutate(ppt.minus.map=ppt.1-map,
#         e.n=ifelse(n_treat_years <1, NA,
#                    ifelse(ppt.minus.map>0, "nominal", "extreme"))) %>%
#  dplyr::select(site_code, year, n_treat_years, e.n)

#extremeyrs.prev <- extremeyrs%>%
#  dplyr::select(site_code, year, e.n)%>%
#  dplyr::rename(prev_e.n = e.n)
#extremeyrs.prev$year <- extremeyrs.prev$year + 1


#extremeyrs.prev2 <- extremeyrs%>%
#  dplyr::select(site_code, year, e.n)%>%
#  dplyr::rename(prev_e.n2 = e.n)
#extremeyrs.prev2$year <- extremeyrs.prev$year + 1 #had some trouble with these addition things but I think it's fine now

#extremeyrs.prev3 <- extremeyrs%>%
#  dplyr::select(site_code, year, e.n)%>%
#  dplyr::rename(prev_e.n3 = e.n)
#extremeyrs.prev3$year <- extremeyrs.prev$year + 2

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
data.anpp1<-merge(data.anpp,Plot.trt.ct2,by=c("site_code","trt","year"))#%>%
#  subset(habitat.type == "Grassland" | habitat.type == "Shrubland" | habitat.type == "Forest understory" | habitat.type == "")%>%
#  left_join(prop, by = c("site_code"))

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
#data.anpp1$relprecip.1 <- -((data.anpp1$ppt.1-data.anpp1$map)/data.anpp1$map)
#data.anpp1$relprecip.2 <- -((data.anpp1$ppt.2-data.anpp1$map)/data.anpp1$map)
#data.anpp1$relprecip.3 <- -((data.anpp1$ppt.3-data.anpp1$map)/data.anpp1$map)
#data.anpp1$relprecip.4 <- -((data.anpp1$ppt.4-data.anpp1$map)/data.anpp1$map)

data.anpp1%>%
  left_join(read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/Site_Elev-Disturb.csv"), by = "site_code")%>%
  dplyr::select(site_name,site_code)%>%
  unique()%>%
  write.csv("C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/site_list.csv")

############################
#####Look at overall treatment effect in a way that shows there's tons of variability in response
te <- data.anpp1%>%
  group_by(site_code, trt, n_treat_years)%>%
  dplyr::summarize(ANPP = mean(mass))

ggplot(te, aes(trt, ANPP))+
  geom_violin()+
  geom_beeswarm(alpha = 0.1)+
  geom_point(data = te %>% group_by(trt)%>%dplyr::summarize(mean.anpp = mean(ANPP)), aes(trt,mean.anpp), size = 2.5, color = "red", pch = 21)+
  xlab("")+
  ylab("ANPP (g/m2)")+
  theme_base()

ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/main_effect.pdf",
        plot = get_last_plot(),
        device = "pdf",
        path = NULL,
        scale = 1,
        width = 4,
        height = 3,
        units = c("in"),
        dpi = 600,
        limitsize = TRUE
)

###############################
#######################################################################
###Causal forest for moderators
##FIRST JUST THE TOP 5 RATED MODERATORS

##Top 5-rankd moderators from survey
#map
#Seasonality
#       Richness
#species composition(pre-treatment) 
#soil texture

#First step, merge in moderators
climate <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/climate/climate_mean_annual_by_site_v4.csv")

#prop <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/community_comp/Prc_LifeHistory_Controls_Oct2023.csv")

#site_richness <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/pretreatment_community_info.csv") # pretreatment_community_info.csv

soil <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/IDE_soil_2024-12-16.csv")%>%
  group_by(site_code)%>%
  dplyr::summarize(ph = mean(ph, na.rm = TRUE), no3 = mean(as.numeric(no3), na.rm = TRUE), p = mean(as.numeric(p), na.rm = TRUE), k = mean(k, na.rm = TRUE), zn = mean(as.numeric(zn), na.rm = TRUE), fe = mean(fe, na.rm = TRUE), mn = mean(mn, na.rm = TRUE), cu = mean(cu, na.rm = TRUE),  silt = mean(silt, na.rm = TRUE), clay = mean(clay, na.rm = TRUE), c = mean(c, na.rm = TRUE), n = mean(n, na.rm = TRUE), c_n = mean(c_n, na.rm = TRUE), organicmatter = mean(organicmatter, na.rm = TRUE))


sand.df <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/site_sand_soc_from_soilgrid_2026-03-26.csv")

comm <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/pretreatment_community_info.csv")

length(unique(comm$site_code))

te1 <- te%>%
  left_join(climate, by = "site_code")%>%
  #left_join(prop, by = "site_code")%>%
  left_join(soil, by = "site_code")%>%
  #left_join(site_richness, by = "site_code")%>%
  left_join(sand.df, by = "site_code")%>%
  left_join(comm, by = "site_code")#%>%
  #left_join(drt.sev, by = c("site_code", "n_treat_years"))

#length(unique(subset(te1, mean_sr >0)$site_code))
#length(unique(subset(te1, PerenGrassCover >-1)$site_code))
#length(unique(subset(te1, Domcover >-1)$site_code))
#length(unique(subset(te1, n>-1)$site_code))#number of site with N data

#moderator variables below
#seasonality_index, n, MAP, mean_sr, cv_ppt_inter, sand_mean, aridity_index, PerenGrassCover,#Functional group composition (pre-treatment) 
#Domcover

Y <- te1$ANPP
W <- te1%>%
  ungroup()%>%
  mutate(trt_num = ifelse(trt=="Control", 0, 1))%>%
  pull(trt_num)
X <- te1%>%
  ungroup()%>%
  dplyr::select(MAP,seasonality_index,ave.richness,ave.evenness#Species composition (pre-treatment) 
                ,sand_0_60cm_weighted
  )#put just the moderators you're testing here




eval.forest <- causal_forest(X, Y, W, clusters = as.factor(te1$site_code),
                             num.trees = 2000)
tau.hat.eval <- predict(eval.forest, X)$predictions

average_treatment_effect(eval.forest)
#  estimate    std.err 
#-27.745905   5.352455 

varimp <- variable_importance(eval.forest)
ranked.vars <- order(varimp, decreasing = TRUE)
colnames(X)[ranked.vars[1:5]]
#"sand_0_60cm_weighted" "seasonality_index" "ave.richness"         "ave.evenness"  "MAP"               

rate <- rank_average_treatment_effect(eval.forest,
                                      predict(eval.forest, X)$predictions)
as.ggplot(~plot(rate))
paste("AUTOC:", round(rate$estimate, 2), "+/", round(1.96 * rate$std.err, 2))


ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/moderator_TOC.pdf",
        plot = get_last_plot(),
        device = "pdf",
        path = NULL,
        scale = 1,
        width = 4,
        height = 3,
        units = c("in"),
        dpi = 600,
        limitsize = TRUE
)


imp <- sort(setNames(variable_importance(eval.forest), colnames(X)))

ggplot(rownames_to_column(data.frame(imp))%>%dplyr::mutate(rowname = dplyr::recode(rowname, MAP = "Mean annual precipitation", sand_mean = "Mean sand content", seasonality_index = "Seasonality", mean_sr = "Species richness", Domcover = "Cover of dominant species"))
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
pdps <- lapply(colnames(X), function(v) plot(partial_dep(eval.forest, v=v, X = X, pred_fun = pred_fun
)))

wrap_plots(pdps, guides = "collect", ncol = 3) &
  ylim(c(-36,-23)) &
  ylab("Treatment effect")&
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))


ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/moderator_treatmenteffects_predictions.pdf",
        plot = last_plot(),
        device = "pdf",
        path = NULL,
        scale = 1,
        width = 8,
        height = 6,
        units = c("in"),
        dpi = 600,
        limitsize = TRUE
)


# Explaining all CATEs globally
ks <- kernelshap(eval.forest, X = X, pred_fun = pred_fun)  
shap_values <- shapviz(ks)
sv_importance(shap_values)&
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/moderator_treatmenteffects_shap.pdf",
        plot = last_plot(),
        device = "pdf",
        path = NULL,
        scale = 1,
        width = 5,
        height = 4,
        units = c("in"),
        dpi = 600,
        limitsize = TRUE
)


sv_dependence(shap_values, v = names(X)[1:5], color_var = NULL, jitter_width = 0.01) +
  plot_layout(ncol = 3)# &
  #ylim(c(-20, 6))

H <- hstats(eval.forest, X = X, pred_fun = pred_fun, verbose = FALSE)
plot(H)
sv_importance(shap_values, kind = "bee")




#plot_list <- lapply(names(X)[1:5], shap.plot.dependence, data_long = prep)





######################################
#######Moderators: top 10

#Top ten moderators from survey
#map
#Seasonality
#       Richness
#species composition(pre-treatment) 
#soil texture
#aridity
#Interannual precipitation variability 
#Functional group composition (pre-treatment) 
#Soil N 
#Water holding capacity (organic matter)


Y <- te1$ANPP
W <- te1%>%
  ungroup()%>%
  mutate(trt_num = ifelse(trt=="Control", 0, 1))%>%
  pull(trt_num)
X <- te1%>%
  ungroup()%>%
  dplyr::select( MAP,seasonality_index,ave.richness,ave.evenness,sand_0_60cm_weighted,aridity_index,cv_ppt_inter,percent_graminoid,n, ,soc_0_60cm_weighted)#drtsev.1) #put just the moderators you're testing here

eval.forest <- causal_forest(X, Y, W, clusters = as.factor(te1$site_code),
                             num.trees = 2000)

average_treatment_effect(eval.forest)
#  estimate    std.err 
#-27.745905   5.352455 

varimp <- variable_importance(eval.forest)
ranked.vars <- order(varimp, decreasing = TRUE)
colnames(X)[ranked.vars[1:10]]
#"percent_graminoid"    "sand_0_60cm_weighted" "seasonality_index"    "ave.richness"  "ave.evenness"         "n"  "soc_0_60cm_weighted"  "cv_ppt_inter" "MAP"       "aridity_index"       

rate <- rank_average_treatment_effect(eval.forest,
                                      predict(eval.forest, X)$predictions)
plot(rate)
paste("AUTOC:", round(rate$estimate, 2), "+/", round(1.96 * rate$std.err, 2))
test_calibration(eval.forest) #A coefficient of 1 for mean.forest.prediction suggests that the mean forest prediction is correct and a coefficient of 1 for differential.forest.prediction suggests that the forest has captured heterogeneity in the underlying signal.

ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/moderator_TOC_kitchensink.pdf",
        plot = get_last_plot(),
        device = "pdf",
        path = NULL,
        scale = 1,
        width = 4,
        height = 3,
        units = c("in"),
        dpi = 600,
        limitsize = TRUE
)


imp <- sort(setNames(variable_importance(eval.forest), colnames(X)))
ggplot(rownames_to_column(data.frame(imp))%>%dplyr::mutate(rowname = dplyr::recode(rowname, MAP = "Mean annual precipitation", sand_mean = "Mean sand content", seasonality_index = "Seasonality", mean_sr = "Species richness", Domcover = "Cover of dominant species", #organicmatter = "Organic matter", 
                                                                                   PerenGrassCover = "Cover of perennial grass", aridity_index = "Aridity", cv_ppt_inter = "CV of interannual precipitation", n = "Soil nitrogen"))
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
        width = 5.5,
        height = 4,
        units = c("in"),
        dpi = 600,
        limitsize = TRUE
)


pred_fun <- function(object, newdata, ...) {
  predict(object, newdata, ...)$predictions
}
pdps <- lapply(colnames(X), function(v) plot(partial_dep(eval.forest, v=v, X = X, pred_fun = pred_fun
)))
wrap_plots(pdps, guides = "collect", ncol = 5) &
  ylim(c(-33,-22)) &
  ylab("Treatment effect of drought on ANPP (g/m2)")&
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))


ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/moderator_treatmenteffects_predictions_kitchensink.pdf",
        plot = last_plot(),
        device = "pdf",
        path = NULL,
        scale = 1,
        width = 13,
        height = 6,
        units = c("in"),
        dpi = 600,
        limitsize = TRUE
)

# Explaining all CATEs globally
ks <- kernelshap(eval.forest, X = X, pred_fun = pred_fun)  
shap_values <- shapviz(ks)
sv_importance(shap_values)&
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/moderator_treatmenteffects_shap_kitchensink.pdf",
        plot = last_plot(),
        device = "pdf",
        path = NULL,
        scale = 1,
        width = 4,
        height = 3.5,
        units = c("in"),
        dpi = 600,
        limitsize = TRUE
)



sv_dependence(shap_values, v = names(X)[1:10], color_var = NULL, jitter_width = 0.001) +
  plot_layout(ncol = 5) &
  ylim(c(-6, 6))&
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))&
  theme(strip.text = element_blank())


ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/moderator_treatmenteffects_shappredictions_kitchensink.pdf",
        plot = last_plot(),
        device = "pdf",
        path = NULL,
        scale = 1,
        width = 13,
        height = 6,
        units = c("in"),
        dpi = 600,
        limitsize = TRUE
)



#visualize overall interaction strength (left) - how much prediction variability comes from interactions with that varible
H <- hstats(eval.forest, X = X, pred_fun = pred_fun, verbose = FALSE)
plot(H)&
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))&
  theme(strip.text = element_blank())


partial_dep(eval.forest, v = "percent_graminoid", X = X, BY = "soc_0_60cm_weighted", by_size = 4L, pred_fun = pred_fun) |> 
  plot()&
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))&
  theme(strip.text = element_blank())


pd <- partial_dep(
  eval.forest,
  v = c("percent_graminoid", "soc_0_60cm_weighted"),
  X = X,
  pred_fun = pred_fun,
  grid_size = 250   # increase for smoother surface
)

pd_df <- pd$data
ggplot(pd_df, aes(
  x = percent_graminoid,
  y = soc_0_60cm_weighted,
  fill = y   # or yhat depending on output
)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(
    x = "Percent Graminoid",
    y = "SOC (0–60 cm)",
    fill = "Predicted"
  ) +
  theme(
    panel.background = element_rect(fill = "white", colour = "grey50")
  )

sv_importance(shap_values, kind = "bee")


#############################################
###########mixed effects regressions

te2 <- te1%>%
  dplyr::select(site_code, trt, n_treat_years, ANPP)%>%
  pivot_wider(names_from = trt, values_from = ANPP)%>%
  mutate(trt_minus_con = Drought-Control)


te3 <- te1%>%
  subset(trt=="Drought")%>%
  left_join(te2, by = c("site_code", "n_treat_years"))


vars <- c("MAP","seasonality_index","ave.richness","ave.evenness","sand_0_60cm_weighted","aridity_index","cv_ppt_inter","percent_graminoid","n","soc_0_60cm_weighted")

plots <- lapply(vars, function(v) {
  
  # mixed-effects model
  f <- as.formula(paste("trt_minus_con ~", v))
  
  m <- lme(
    fixed = f,
    random = ~1 | site_code,
    data = te3,
    na.action = na.omit,
    method = "REML"
  )
  
  # extract p-value for fixed effect
  pval <- summary(m)$tTable[2, "p-value"]
  sig  <- pval < 0.1  #0.05
  
  p_label <- paste0("p = ", format.pval(pval, digits = 2))
  
  # base plot
  p <- ggplot(te3, aes(x = .data[[v]], y = trt_minus_con)) +
    geom_point() +
    annotate(
      "text",
      x = Inf, y = Inf,
      label = p_label,
      hjust = 1.1, vjust = 1.5,
      size = 3.5
    ) +
    ylab("Treatment effect on ANPP") +
    xlab(v) +
    theme_base()
  
  # add regression line ONLY if significant
  if (sig) {
    
    # new data for smooth line
    newdat <- data.frame(
      x = seq(
        min(te3[[v]], na.rm = TRUE),
        max(te3[[v]], na.rm = TRUE),
        length.out = 100
      )
    )
    names(newdat) <- v
    
    # fixed-effect predictions (no random effects)
    newdat$pred <- predict(m, newdata = newdat, level = 0)
    
    p <- p +
      geom_line(
        data = newdat,
        aes(x = .data[[v]], y = pred),
        linewidth = 1
      )
  }
  
  p
})


wrap_plots(plots, ncol = 5)


ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/moderator_mixed_regressions.pdf",
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
