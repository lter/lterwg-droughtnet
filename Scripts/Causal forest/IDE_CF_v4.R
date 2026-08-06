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
library(mgcv)
library(ranger)

set.seed(100)

#read ANPP data
data.anpp <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/anpp_ppt_2026-03-27.csv")%>% 
  subset(habitat.type == "Grassland" | habitat.type == "Shrubland" | habitat.type == "Forest understory")%>%
  mutate(n_treat_years = ifelse(site_code == "allmendo.ch" & n_treat_days == 197, 1, n_treat_years))%>%
  mutate(n_treat_years = ifelse(site_code == "allmendb.ch" & n_treat_days == 197, 1, n_treat_years))%>%
  mutate(n_treat_years = ifelse(site_code == "torla.es" & n_treat_days == 195, 1, n_treat_years))%>%
  subset(n_treat_years >=1 & n_treat_years <= 4
  )

length(unique(data.anpp$site_code)) #121

Site_Elev.Disturb <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/Site_Elev-Disturb.csv")%>%
  dplyr::select(site_code, latitud, longitud )

#create long-term average ANPP in controls
anpp.mean <- data.anpp%>%
  subset(trt == "Control")%>%
  ddply(.(site_code, year),function(x)data.frame(mass = mean(x$mass)))%>% #summarizes controls for each year
  ddply(.(site_code),function(x)data.frame(mean.mass = mean(x$mass), n_years = length(x$mass)))#summarizes controls across years

prop <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/community_comp/Prc_LifeHistory_Controls_Oct2023.csv")

site_richness <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/site_ave_richness.csv")



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
data.anpp1<-merge(data.anpp,Plot.trt.ct2,by=c("site_code","trt","year"))

length(unique(data.anpp1$site_code)) #118
length(unique(subset(data.anpp1, habitat.type == "Grassland")$site_code)) #84
length(unique(subset(data.anpp1, habitat.type == "Shrubland")$site_code)) #28
length(unique(subset(data.anpp1, habitat.type == "Forest understory")$site_code)) #6

##How many treatment years does each site have of the first 3 years?
num.treat.years <- data.anpp1[,c("site_code", "n_treat_years")]%>%
  unique()%>%
  subset(n_treat_years>=1&n_treat_years<=4)

num.treat.years <- ddply(num.treat.years,.(site_code),
                         function(x)data.frame(
                           num.years = length(x$n_treat_years)
                         ))

data.anpp1%>%
  left_join(read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/Site_Elev-Disturb.csv"), by = "site_code")%>%
  dplyr::select(site_name,site_code)%>%
  unique()%>%
  write.csv("C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/site_list.csv")

# ----------------------------------
# Master variable naming key
# ----------------------------------
var_key <- c(
  MAP = "Mean annual precipitation (MAP)",
  sand_0_60cm_weighted = "Soil texture",
  percent_graminoid = "Functional group composition",
  seasonality_index = "Seasonality",
  ave.evenness = "Species composition",
  n = "Soil nutrients",
  ave.richness = "Species richness",
  cv_ppt_inter = "Interannual precipitation variability",
  aridity_index = "Aridity",
  soc_0_60cm_weighted = "Soil organic matter"
)

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

te_figure <- te%>%
  dplyr::select(site_code, trt, n_treat_years, ANPP)%>%
  pivot_wider(names_from = trt, values_from = ANPP)%>%
  mutate(trt_minus_con = Drought-Control)%>%
  group_by(site_code)%>%
  dplyr::summarize(trt_minus_con = mean(trt_minus_con))

ggplot(te_figure, aes(trt_minus_con))+
  geom_histogram(bins = 15, color = "black")+
  xlab("Average ANPP response (treatment-control)[g/m2]")+
  ylab("Count of sites")+
  geom_vline(xintercept = -27.745905, color = "red", linetype = "dashed")+
  theme_base()

ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/treatment_effect_histogram.pdf",
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



#Y <- te1$ANPP
#W <- te1%>%
#  ungroup()%>%
#  mutate(trt_num = ifelse(trt=="Control", 0, 1))%>%
#  pull(trt_num)
#X <- te1%>%
#  ungroup()%>%
#  dplyr::select(MAP,seasonality_index,ave.richness,ave.evenness#Species composition (pre-treatment) 
#                ,sand_0_60cm_weighted
#  )#put just the moderators you're testing here

length(unique(subset(te1, MAP > 0)$site_code))
length(unique(subset(te1, seasonality_index > 0)$site_code))
length(unique(subset(te1, ave.richness > -1)$site_code))
length(unique(subset(te1, ave.evenness > -1)$site_code))
length(unique(subset(te1, sand_0_60cm_weighted > -1)$site_code))
length(unique(subset(te1, percent_graminoid > -1)$site_code))
length(unique(subset(te1, n > -1)$site_code))
length(unique(subset(te1, cv_ppt_inter > -1)$site_code))
length(unique(subset(te1, aridity_index > -1)$site_code))
length(unique(subset(te1, soc_0_60cm_weighted > -1)$site_code))


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
  dplyr::select( MAP,seasonality_index,ave.richness,ave.evenness,sand_0_60cm_weighted,aridity_index,cv_ppt_inter,percent_graminoid,n, 
                 #n_0_15cm,
                 soc_0_60cm_weighted)#drtsev.1) #put just the moderators you're testing here

# Compute per-row weights: each site gets total weight of 1
site_counts <- te1 %>%
  add_count(site_code, name = "site_n") %>%
  mutate(w = 1 / site_n) %>%
  pull(w)

eval.forest <- causal_forest(X, Y, W,
                             clusters      = as.factor(te1$site_code),
                             sample.weights = site_counts,
                             num.trees     = 2000)#change to 10,000 for publication 2000

average_treatment_effect(eval.forest)
#  estimate    std.err 
#-25.175261   5.146604  


varimp <- variable_importance(eval.forest)
ranked.vars <- order(varimp, decreasing = TRUE)
colnames(X)[ranked.vars[1:10]]
# "sand_0_60cm_weighted" "seasonality_index"    "soc_0_60cm_weighted"  "percent_graminoid"   "ave.evenness"         "n"                    "ave.richness"         "MAP"                 "cv_ppt_inter"         "aridity_index"        

rate <- rank_average_treatment_effect(eval.forest,
                                      predict(eval.forest, X)$predictions)
plot(rate)
paste("AUTOC:", round(rate$estimate, 2), "+/", round(1.96 * rate$std.err, 2))
test_calibration(eval.forest) #A coefficient of 1 for mean.forest.prediction suggests that the mean forest prediction is correct and a coefficient of 1 for differential.forest.prediction suggests that the forest has captured heterogeneity in the underlying signal.

ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/moderator_TOC_top.pdf",
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

varimp_df <- tibble(
  variable = names(imp),
  value = as.numeric(imp)
) %>%
  mutate(
    moderator = recode(variable, !!!var_key)
  ) %>%
  filter(!is.na(moderator))

p_varimp <- ggplot(varimp_df, aes(x = reorder(moderator, value), y = value)) +
  geom_col(fill = "grey40") +
  coord_flip() +
  labs(x = NULL, y = "Split-based variable importance") +
  theme_base()


pred_fun <- function(object, newdata, ...) {
  predict(object, newdata, ...)$predictions
}
pdps <- lapply(colnames(X), function(v) plot(partial_dep(eval.forest, v=v, X = X, pred_fun = pred_fun
)))
wrap_plots(pdps, guides = "collect", ncol = 5) &
  ylim(c(-33,-22)) &
  ylab("Treatment effect of drought on ANPP (g/m2)")&
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))


ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/moderator_treatmenteffects_predictions_top.pdf",
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

# ── Partial dependence plots with SE CI ──────────────────────────────────────
# Strategy: vary focal variable across its observed range; fix all other
# covariates at their column means.  predict(..., estimate.variance = TRUE)
# returns the IJ variance for each individual CATE, so SE = sqrt(var) is exact.
pdp_with_ci <- function(forest, focal_var, X, n_grid = 50) {
  
  grid_vals <- seq(
    min(X[[focal_var]], na.rm = TRUE),
    max(X[[focal_var]], na.rm = TRUE),
    length.out = n_grid
  )
  
  # Use colMeans() instead of summarise(across(...))
  X_means <- as.data.frame(as.list(colMeans(X, na.rm = TRUE)))
  
  newdata <- X_means[rep(1L, n_grid), ]
  newdata[[focal_var]] <- grid_vals
  
  preds <- predict(forest, newdata = newdata, estimate.variance = TRUE)
  
  tibble(
    x        = grid_vals,
    estimate = preds$predictions,
    se       = sqrt(preds$variance.estimates),
    lower    = estimate - se,
    upper    = estimate + se,
    variable = focal_var
  )
}

# Run for every moderator and bind into one data frame
pdp_data <- purrr::map_dfr(colnames(X), pdp_with_ci,
                           forest = eval.forest, X = X)

# Attach human-readable labels
pdp_data <- pdp_data %>%
  mutate(label = recode(variable, !!!var_key))

# One plot per moderator
pdp_plots <- pdp_data %>%
  group_by(variable) %>%
  group_split() %>%
  purrr::map(function(df) {
    ggplot(df, aes(x = x, y = estimate)) +
      geom_ribbon(aes(ymin = lower, ymax = upper),
                  fill = "grey70", alpha = 0.4) +
      geom_line(linewidth = 0.7, colour = "grey20") +
      geom_hline(yintercept = 0, linetype = "dashed",
                 colour = "grey50", linewidth = 0.4) +
      labs(
        x     = unique(df$label),
        y     = "Treatment effect of drought\non ANPP (g m\u207b\u00b2)",
        title = NULL
      ) +
      coord_cartesian(ylim = c(-35, 0)) +
      theme(
        panel.background = element_rect(fill = "white", colour = "grey50"),
        axis.title       = element_text(size = 8),
        axis.text        = element_text(size = 7)
      )
  })

wrap_plots(pdp_plots, ncol = 5)

ggsave(
  "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/moderator_treatmenteffects_predictions_top1.pdf",
  plot   = last_plot(),
  device = "pdf",
  width  = 13, height = 6, units = "in", dpi = 600
)



pdp_with_spread <- function(forest, focal_var, X, n_grid = 50) {
  
  grid_vals <- seq(min(X[[focal_var]], na.rm = TRUE),
                   max(X[[focal_var]], na.rm = TRUE),
                   length.out = n_grid)
  
  purrr::map_dfr(grid_vals, function(g) {
    # Take the real data, overwrite ONLY the focal column with g
    nd <- X
    nd[[focal_var]] <- g
    cate <- predict(forest, newdata = nd)$predictions   # one CATE per site-row
    
    tibble(
      x        = g,
      estimate = mean(cate),                    # marginal PDP line
      lower    = quantile(cate, 0.10, names = FALSE),
      upper    = quantile(cate, 0.90, names = FALSE),
      q25      = quantile(cate, 0.25, names = FALSE),
      q75      = quantile(cate, 0.75, names = FALSE),
      variable = focal_var
    )
  })
}

pdp_spread_data <- purrr::map_dfr(colnames(X), pdp_with_spread,
                                  forest = eval.forest, X = X) %>%
  mutate(label = recode(variable, !!!var_key))


pdp_spread_plots <- pdp_spread_data %>%
  group_by(variable) %>% group_split() %>%
  purrr::map(function(df) {
    ggplot(df, aes(x = x, y = estimate)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey95", alpha = 0.5) +
      geom_ribbon(aes(ymin = q25,   ymax = q75),   fill = "grey75", alpha = 0.5) +
      geom_line(linewidth = 0.7, colour = "grey15") +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
      labs(x = unique(df$label),
           y = "Treatment effect of drought\non ANPP (g m\u207b\u00b2)") +
      theme(panel.background = element_rect(fill = "white", colour = "grey50"),
            axis.title = element_text(size = 8), axis.text = element_text(size = 7))
  })

wrap_plots(pdp_spread_plots, ncol = 5)


ggsave(
  "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/moderator_treatmenteffects_predictions_topquantiles.pdf",
  plot   = last_plot(),
  device = "pdf",
  width  = 13, height = 6, units = "in", dpi = 600
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

ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/moderator_treatmenteffects_shappredictions_top.pdf",
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
plot(H, fill = "black")&
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))&
  theme(strip.text = element_blank())

h2 <- H[["h2_pairwise"]]

vals <- h2$num[, 1] / h2$denom[, 1]
names(vals) <- rownames(h2$num)

vars <- colnames(X)
#vars <- H$varnames

p <- length(vars)
M <- matrix(0, p, p, dimnames = list(vars, vars))
diag(M) <- NA


for (nm in names(vals)) {
  parts <- strsplit(nm, ":")[[1]]
  
  if (length(parts) == 1) {
    # main effects (optional)
    M[parts, parts] <- vals[nm]
  } else {
    v1 <- parts[1]
    v2 <- parts[2]
    M[v1, v2] <- vals[nm]
    M[v2, v1] <- vals[nm]
  }
}

df <- as.data.frame(M) |>
  rownames_to_column("var1") |>
  pivot_longer(-var1, names_to = "var2", values_to = "interaction") |>
  mutate(
    var1 = factor(var1, levels = vars),
    var2 = factor(var2, levels = vars)
  ) |>
  filter(
    as.integer(var1) > as.integer(var2),   # keep one triangle
    !is.na(interaction)                     # remove diagonal
  )



ggplot(df, aes(x = var2, y = var1, fill = interaction)) +
  geom_tile(color = "black", linewidth = 0.4) +
  coord_fixed() +
  scale_fill_gradient(
    low = "white",
    high = "black"
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Interaction\nstrength"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )



ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/interaction_h2_heatmap.pdf",
        plot = last_plot(),
        device = "pdf",
        path = NULL,
        scale = 1,
        width = 6,
        height = 4,
        units = c("in"),
        dpi = 600,
        limitsize = TRUE
)



keep <- !is.na(X$soc_0_60cm_weighted)
X_clean <- X[keep, ]
by_var <- as.numeric(X_clean$soc_0_60cm_weighted > 30)

pd <- partial_dep(
  eval.forest,
  v = "MAP",
  X = X_clean,
  BY = by_var,
  by_size = 2L,
  pred_fun = pred_fun
)

pd$data$Group <- factor(pd$data$Group, levels = c(0, 1), labels = c("Low SOC (0-30)", "High SOC (30+)"))

plot(pd) &
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) &
  theme(strip.text = element_text())


ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/interaction_example.pdf",
        plot = last_plot(),
        device = "pdf",
        path = NULL,
        scale = 1,
        width = 6,
        height = 4,
        units = c("in"),
        dpi = 600,
        limitsize = TRUE
)



by_var <- ifelse(is.na(X$percent_graminoid), NA, as.numeric(X$percent_graminoid > 0.3))
table(by_var, useNA = "always")  # confirm you get 0s, 1s, and NAs

pd <- partial_dep(
  eval.forest,
  v = "seasonality_index",
  X = X,
  BY = by_var,
  by_size = 2L,
  pred_fun = pred_fun
) 



keep <- !is.na(X$percent_graminoid)
X_clean <- X[keep, ]
by_var <- as.numeric(X_clean$percent_graminoid > 0.3)

pd <- partial_dep(
  eval.forest,
  v = "seasonality_index",
  X = X_clean,
  BY = by_var,
  by_size = 2L,
  pred_fun = pred_fun
)

pd$data$Group <- factor(pd$data$Group, levels = c(0, 1), labels = c("Low graminoid (0-0.3)", "High graminoid (0.3-1)"))

plot(pd) &
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) &
  theme(strip.text = element_text())


#partial_dep(eval.forest, v = "seasonality_index", X = X, BY = "percent_graminoid", by_size = 2L, 
#            by_breaks =  c(0.0000,0.2, 0.9743), pred_fun = pred_fun) |> 
#  plot()&
#  theme(panel.background = element_rect(fill = "white", colour = "grey50"))&
#  theme(strip.text = element_blank())

ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/interaction_example2.pdf",
        plot = last_plot(),
        device = "pdf",
        path = NULL,
        scale = 1,
        width = 6,
        height = 4,
        units = c("in"),
        dpi = 600,
        limitsize = TRUE
)



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

#sv_importance(shap_values, kind = "bee")





keep <- !is.na(X$ave.evenness)
X_clean <- X[keep, ]
by_var <- as.numeric(X_clean$ave.evenness > 0.12)

pd <- partial_dep(
  eval.forest,
  v = "MAP",
  X = X_clean,
  BY = by_var,
  by_size = 2L,
  pred_fun = pred_fun
)

pd$data$Group <- factor(pd$data$Group, levels = c(0, 1), labels = c("Low evenness (0-0.3)", "High evenness (0.3-1)"))

plot(pd) &
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) &
  theme(strip.text = element_text())


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
  
  # enforce complete cases for THIS variable
  dat_v <- te3 %>%
    dplyr::select(trt_minus_con, site_code, all_of(v)) %>%
    tidyr::drop_na()
  
  # null model (ML)
  m0 <- lme(
    fixed  = trt_minus_con ~ 1,
    random = ~1 | site_code,
    data   = dat_v,
    method = "ML"
  )
  
  # forward selection up to quartic
  m_sel_ml <- stepAIC(
    m0,
    scope = list(
      lower = ~1,
      upper = as.formula(
        paste0("~ poly(", v, ", 4, raw = TRUE)")
      )
    ),
    direction = "forward",
    trace = FALSE
  )
  
  # refit with REML for inference
  m_sel <- update(m_sel_ml, method = "REML")
  sm <- summary(m_sel)
  
  
  # extract overall p-value for the fixed effect block
  tt <- sm$tTable
  has_term <- nrow(tt) > 1
  
  pval <- if (has_term) {
    max(tt[-1, "p-value"])   # conservative: weakest polynomial term
  } else {
    NA
  }
  
  sig <- !is.na(pval) && pval < 0.1
  p_label <- if (has_term) {
    paste0("p = ", format.pval(pval, digits = 2))
  } else {
    "null"
  }
  
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
    xlab(var_key[[v]]) +
    theme_base()
  
  # add fitted curve if any non-null model selected
  if (has_term) {
    
    newdat <- data.frame(
      x = seq(
        min(te3[[v]], na.rm = TRUE),
        max(te3[[v]], na.rm = TRUE),
        length.out = 100
      )
    )
    names(newdat) <- v
    
    newdat$pred <- predict(m_sel, newdata = newdat, level = 0)
    
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



###GAMS for comparison




plots <- lapply(vars, function(v) {
  
  #----------------------------------
  # 0. Variable-specific data
  #----------------------------------
  dat <- te3 %>%
    dplyr::select(trt_minus_con, site_code, all_of(v)) %>%
    dplyr::filter(!is.na(.data[[v]]))
  
  dat$site_code <- factor(dat$site_code)
  
  # skip if too few unique values to form a smooth
  if (dplyr::n_distinct(dat[[v]]) < 4) {
    message(paste("Skipping", v, "- too few unique values"))
    return(NULL)
  }
  
  #----------------------------------
  # 1. Fit GAM
  #----------------------------------
  f <- as.formula(
    paste0("trt_minus_con ~ s(", v, ", k = 5) + s(site_code, bs = 're')")
  )
  
  m <- gam(
    formula = f,
    data = dat,
    method = "REML"
  )
  
  sm <- summary(m)
  
  #----------------------------------
  # 2. Check that the smooth actually exists
  #----------------------------------
  smooth_name <- paste0("s(", v, ")")
  
  if (!smooth_name %in% rownames(sm$s.table)) {
    message(paste("Smooth dropped for", v))
    return(NULL)
  }
  
  pval <- sm$s.table[smooth_name, "p-value"]
  sig  <- pval < 0.1
  p_label <- paste0("p = ", format.pval(pval, digits = 2))
  
  #----------------------------------
  # 3. Base plot
  #----------------------------------
  p <- ggplot(dat, aes(x = .data[[v]], y = trt_minus_con)) +
    geom_point(alpha = 0.7) +
    annotate(
      "text",
      x = Inf, y = Inf,
      label = p_label,
      hjust = 1.1, vjust = 1.5,
      size = 3.5
    ) +
    ylab("Treatment effect on ANPP") +
    xlab(var_key[[v]]) +
    theme_base()
  
  #----------------------------------
  # 4. Add smooth only if significant
  #----------------------------------
  if (sig) {
    
    newdat <- data.frame(
      site_code = dat$site_code[1],
      x = seq(
        min(dat[[v]], na.rm = TRUE),
        max(dat[[v]], na.rm = TRUE),
        length.out = 200
      )
    )
    names(newdat)[2] <- v
    
    smooth_terms <- predict(
      m,
      newdata = newdat,
      type = "terms"
    )
    
    newdat$pred <- smooth_terms[, smooth_name] +
      coef(m)["(Intercept)"]
    
    p <- p +
      geom_line(
        data = newdat,
        aes(x = .data[[v]], y = pred),
        linewidth = 1
      )
  }
  
  p
})


plots <- Filter(Negate(is.null), plots)
wrap_plots(plots, ncol = 5)

ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/moderator_GAM.pdf",
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





# S-learner and T-learner versions of top 10 moderator analyses 
# using the ranger package. Set some of the tuning params
# to what causal_forest uses:
# 2000 trees
# subsampling with half the data per tree
# # vars to try per split: min(ceiling(sqrt(ncol(X)) + 20), ncol(X))

mtry_val <- min(ceiling(sqrt(ncol(X)) + 20), ncol(X))

# s-learner
sl.data = cbind(Y, W, X)
sl.predvars = colnames(sl.data)[-1]
sl.form = as.formula(paste0("Y ~ ", paste(sl.predvars, collapse=" + ")))
sl.mod = ranger(sl.form,
                data=sl.data, 
                num.trees=2000, #change to 10,000 for publication
                mtry=mtry_val,
                replace=F,
                sample.fraction=0.5)

# Function to predict treatment effects. One model but predict twice:
# once with treatment = 1, once with treatment = 0, and subtract.

sl_te_pred = function(mod, newX) {
  tmp.data = cbind(W=rep(1, nrow(newX)), newX)
  treated_preds = predict(mod, data=tmp.data)$predictions
  tmp.data[,"W"] = 0
  ctrl_preds = predict(mod, data=tmp.data)$predictions
  return(treated_preds-ctrl_preds)
}

# ATE - sign same, but magnitudes very different than CF
mean(sl_te_pred(sl.mod, X))

pdps.s.learner <- lapply(colnames(X), function(v) plot(partial_dep(sl.mod, v=v, X = X, pred_fun = sl_te_pred)))

wrap_plots(pdps.s.learner, guides = "collect", ncol = 5) &
  ylim(c(-22,9)) &
  ylab("Treatment effect")&
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))


ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/moderator_s-learner.pdf",
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


# t-learner
tl.data = cbind(Y, X)
tl.predvars = colnames(tl.data)[-1]
tl.form = as.formula(paste0("Y ~ ", paste(tl.predvars, collapse=" + ")))
tl.mod.trt = ranger(tl.form,
                    data=tl.data[W==1,], 
                    num.trees=2000, #change to 10,000 for publication
                    mtry=mtry_val,
                    replace=F,
                    sample.fraction=0.5)
tl.mod.ctrl = ranger(tl.form,
                     data=tl.data[W==0,], 
                     num.trees=2000, #change to 10,000 for publication
                     mtry=mtry_val,
                     replace=F,
                     sample.fraction=0.5)

tl.mod = list(tmod = tl.mod.trt, 
              cmod = tl.mod.ctrl)

tl_te_pred = function(mod, newX) {
  treated_preds = predict(mod$tmod, data=newX)$predictions
  ctrl_preds = predict(mod$cmod, data=newX)$predictions
  return(treated_preds-ctrl_preds)
}

# ATE - sign same (and closer to CF estimate than S-learner estimate is)
mean(tl_te_pred(tl.mod, X))


pdps.t.learner <- lapply(colnames(X), function(v) plot(partial_dep(tl.mod, v=v, X = X, pred_fun = tl_te_pred)))

wrap_plots(pdps.t.learner, guides = "collect", ncol = 5) &
  ylim(c(-190,90)) &
  ylab("Treatment effect")&
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))


ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/moderator_t-learner.pdf",
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


# ── Bootstrapped confidence bands for S- and T-learner PDPs ──────────────────
# The ranger learners don't provide IJ variance like grf does, so uncertainty
# is obtained by refitting each learner on bootstrap resamples, recomputing the
# PDP curve (focal var varied, all others fixed at column means), and taking the
# mean and 2.5/97.5 percentiles across bootstraps at each grid point.
# NOTE: these bands are bootstrap-based, NOT infinitesimal jackknife like the
# causal forest bands. State this distinction in the figure caption.

n_boot_learner <- 50   # fast check; increase (e.g. 200) for publication

# Generic bootstrap PDP for a learner. refit_fun(idx) returns a fitted model
# (or list of models); te_pred_fun(model, newX) returns treatment-effect preds.
boot_pdp_learner <- function(refit_fun, te_pred_fun, X, focal_vars,
                             n_boot = 50, n_grid = 50) {
  
  X_means <- as.data.frame(as.list(colMeans(X, na.rm = TRUE)))
  
  purrr::map_dfr(1:n_boot, function(b) {
    idx   <- sample(seq_len(nrow(X)), replace = TRUE)
    mod_b <- refit_fun(idx)
    
    purrr::map_dfr(focal_vars, function(v) {
      grid_vals <- seq(min(X[[v]], na.rm = TRUE),
                       max(X[[v]], na.rm = TRUE), length.out = n_grid)
      nd <- X_means[rep(1L, n_grid), ]
      nd[[v]] <- grid_vals
      tibble(variable = v, x = grid_vals,
             te = te_pred_fun(mod_b, nd), boot = b)
    })
  }) %>%
    group_by(variable, x) %>%
    dplyr::summarize(
      estimate = mean(te),
      lower    = quantile(te, 0.025, names = FALSE),
      upper    = quantile(te, 0.975, names = FALSE),
      .groups  = "drop"
    ) %>%
    mutate(label = recode(variable, !!!var_key))
}

# Refit closures reusing the exact ranger settings from above
sl_refit <- function(idx) {
  d <- cbind(Y = Y[idx], W = W[idx], X[idx, ])
  ranger(sl.form, data = d, num.trees = 2000, mtry = mtry_val,
         replace = FALSE, sample.fraction = 0.5)
}

tl_refit <- function(idx) {
  d  <- cbind(Y = Y[idx], X[idx, ])
  wb <- W[idx]
  list(
    tmod = ranger(tl.form, data = d[wb == 1, ], num.trees = 2000,
                  mtry = mtry_val, replace = FALSE, sample.fraction = 0.5),
    cmod = ranger(tl.form, data = d[wb == 0, ], num.trees = 2000,
                  mtry = mtry_val, replace = FALSE, sample.fraction = 0.5)
  )
}

# Bootstrap bands for ALL 10 moderators
sl_ci <- boot_pdp_learner(sl_refit, sl_te_pred, X, colnames(X),
                          n_boot = n_boot_learner)
tl_ci <- boot_pdp_learner(tl_refit, tl_te_pred, X, colnames(X),
                          n_boot = n_boot_learner)

# Helper to build the banded standalone panels
make_learner_plots <- function(ci_df, ylims) {
  ci_df %>%
    mutate(variable = factor(variable, levels = colnames(X))) %>%
    group_by(variable) %>% group_split() %>%
    purrr::map(function(df) {
      ggplot(df, aes(x = x, y = estimate)) +
        geom_ribbon(aes(ymin = lower, ymax = upper),
                    fill = "grey70", alpha = 0.4) +
        geom_line(linewidth = 0.7, colour = "grey20") +
        geom_hline(yintercept = 0, linetype = "dashed",
                   colour = "grey50", linewidth = 0.4) +
        labs(x = unique(df$label), y = "Treatment effect") +
        coord_cartesian(ylim = ylims) +
        theme(panel.background = element_rect(fill = "white", colour = "grey50"),
              axis.title = element_text(size = 8),
              axis.text  = element_text(size = 7))
    })
}

# S-learner PDPs with bootstrap CI
wrap_plots(make_learner_plots(sl_ci, c(-22, 9)), ncol = 5)

ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/moderator_s-learner_CI.pdf",
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

# T-learner PDPs with bootstrap CI
wrap_plots(make_learner_plots(tl_ci, c(-190, 90)), ncol = 5)

ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/moderator_t-learner_CI.pdf",
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



# Compare across CF, S-learner, T-learner


#####
###Combine a few partial dependency plots for a figure
focal_vars <- c(
  "sand_0_60cm_weighted",
  "ave.richness",
  "seasonality_index"
)

var_labels <- c(
  sand_0_60cm_weighted = "Sand content (0–60 cm)",
  ave.richness        = "Species richness",
  seasonality_index   = "Seasonality"
)

get_pd_df <- function(mod, v, X, pred_fun, model_name) {
  
  pd <- partial_dep(
    mod,
    v = v,
    X = X,
    pred_fun = pred_fun,
    grid_size = 100
  )
  
  df <- pd$data
  
  # identify prediction column robustly
  pred_col <- setdiff(colnames(df), v)
  
  if (length(pred_col) != 1) {
    stop("Could not uniquely identify prediction column in partial_dep output")
  }
  
  df |>
    dplyr::rename(
      x = !!v,
      y = !!pred_col
    ) |>
    dplyr::mutate(
      variable = v,
      model = model_name
    )
}


cf_pd <- purrr::map_dfr(
  focal_vars,
  get_pd_df,
  mod = eval.forest,
  X = X,
  pred_fun = pred_fun,
  model_name = "Causal forest"
)

sl_pd <- purrr::map_dfr(
  focal_vars,
  get_pd_df,
  mod = sl.mod,
  X = X,
  pred_fun = sl_te_pred,
  model_name = "RF S-learner"
)

tl_pd <- purrr::map_dfr(
  focal_vars,
  get_pd_df,
  mod = tl.mod,
  X = X,
  pred_fun = tl_te_pred,
  model_name = "RF T-learner"
)

#lme_pd <- purrr::map_dfr(focal_vars, function(v) {

#  f <- as.formula(paste("trt_minus_con ~", v))

#  m <- lme(
#    fixed = f,
#    random = ~ 1 | site_code,
#    data = te3,
#    method = "REML",
#    na.action = na.omit
#  )

#  xseq <- seq(
#    min(te3[[v]], na.rm = TRUE),
#    max(te3[[v]], na.rm = TRUE),
#    length.out = 100
#  )

#  newdat <- data.frame(xseq)
#  names(newdat) <- v

#  newdat$y <- predict(m, newdata = newdat, level = 0)

#  newdat |>
#    dplyr::rename(x = !!v) |>
#    dplyr::mutate(
#      variable = v,
#     model = "Linear model"
#    )
#})

#gam_pd <- purrr::map_dfr(focal_vars, function(v) {

#  dat <- te3 |>
#    dplyr::select(trt_minus_con, site_code, all_of(v)) |>
#    dplyr::filter(!is.na(.data[[v]]))

#  dat$site_code <- factor(dat$site_code)

# skip variables with too few unique values
#  if (dplyr::n_distinct(dat[[v]]) < 4) {
#    message("Skipping ", v, " (too few unique values)")
#    return(NULL)
#  }

#  m <- gam(
#    as.formula(paste0(
#      "trt_minus_con ~ s(", v, ", k = 5) + s(site_code, bs = 're')"
#    )),
#    data = dat,
#    method = "REML"
#  )

# prediction grid
#  xseq <- seq(
#    min(dat[[v]], na.rm = TRUE),
#    max(dat[[v]], na.rm = TRUE),
#    length.out = 200
#  )

#  newdat <- data.frame(
#    site_code = dat$site_code[1],  # hold RE constant
#    xval = xseq
#  )
#  names(newdat)[2] <- v

# ✅ marginal prediction (always defined)
#  newdat$y <- predict(m, newdata = newdat, type = "response")

#  newdat |>
#    dplyr::rename(x = !!v) |>
#    dplyr::mutate(
#      variable = v,
#      model    = "GAM"
#    )
#})


pd_all <- dplyr::bind_rows(
  cf_pd,
  sl_pd,
  tl_pd#,
  #  lme_pd,
  #  gam_pd
) |>
  dplyr::mutate(
    variable = factor(variable, levels = focal_vars),
    model = factor(
      model,
      levels = c(
        "Causal forest",
        "RF S-learner",
        "RF T-learner"#,
        #        "Linear model",
        #        "GAM"
      )
    )
  )

ggplot(pd_all, aes(x = x, y = y)) +
  geom_line(linewidth = 0.9) +
  facet_grid(
    rows   = vars(variable),
    cols   = vars(model),
    scales = "free",
    #labeller = labeller(variable = as.labeller(var_key))
  ) +
  ylab("Treatment effect of drought on ANPP") +
  xlab(NULL) +
  ylim(-50,0)+ #RF T-learner species richness goes way lower than this limit
  theme_base() +
  theme(
    panel.background = element_rect(fill = "white", colour = "grey50"),
    strip.background = element_blank(),
    strip.text.y = element_text(angle = 0, size = 10),
    strip.text.x = element_text(size = 10)
  )





plot_one_variable <- function(dat, var, lab) {
  
  p <- ggplot(
    dat |> dplyr::filter(variable == var),
    aes(x = x, y = y)
  ) +
    geom_line(linewidth = 0.9) +
    facet_grid(
      . ~ model,
      scales = "free_y"
    ) +
    xlab(lab) +
    ylim(-50,0)+#RF T-learner species richness goes way lower than this limit
    theme_base() +
    theme(
      panel.background = element_rect(fill = "white", colour = "grey50"),
      strip.background = element_blank(),
      strip.text.x = element_text(size = 10)
    )
  
  # ✅ add y-axis label only once (middle row)
  if (var == focal_vars[2]) {
    p <- p + ylab("Treatment effect of drought on ANPP")
  } else {
    p <- p + ylab(NULL)
  }
  
  p
}

p_sand <- plot_one_variable(
  pd_all,
  "sand_0_60cm_weighted",
  "Sand content (0–60 cm)"
)

p_rich <- plot_one_variable(
  pd_all,
  "ave.richness",
  "Species richness"
)

p_seas <- plot_one_variable(
  pd_all,
  "seasonality_index",
  "Seasonality"
)



(p_sand / p_rich / p_seas) +
  plot_layout(heights = c(1, 1, 1))


ggsave( "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/partial_dependiencies_comparison.pdf",
        plot = last_plot(),
        device = "pdf",
        path = NULL,
        scale = 1,
        width = 6,
        height = 7,
        units = c("in"),
        dpi = 600,
        limitsize = TRUE
)


##################################
########real kitchen sink

######################################
#######Moderators: kitchen sink approach
te1 <- te%>%
  left_join(climate, by = "site_code")%>%
  left_join(prop, by = "site_code")%>%
  left_join(soil, by = "site_code")%>%
  left_join(site_richness, by = "site_code")%>%
  left_join(sand.df, by = "site_code")%>%
  left_join(comm, by = "site_code")



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
  dplyr::select(ppt_max_event, ppt_mean_event, days_half_ppt, daily_ppt_d, n_wet_days, avg_dryspell_length, ppt_95th_percentile_size, MAP, cv_ppt_intra, cv_ppt_inter, yearly_ppt_d, seasonality_index, aridity_index, r_monthly_t_p, MAT, PctAnnual, PctGrass,
                ph, no3, p, k, zn, fe, mn, cu, silt, clay, c, n, c_n, mean_sr, sand_0_5cm, sand_0_15cm, sand_0_30cm, sand_0_60cm, sand_0_60cm_weighted, soc_0_5cm, soc_0_15cm, soc_0_30cm, soc_0_60cm, soc_0_60cm_weighted, n_0_5cm, n_0_15cm, n_0_30cm, n_0_60cm, n_0_60cm_weighted, percent_graminoid, percent_c4, ave.richness, ave.evenness) #put just the moderators you're testing here




# Compute per-row weights: each site gets total weight of 1
site_counts <- te1 %>%
  add_count(site_code, name = "site_n") %>%
  mutate(w = 1 / site_n) %>%
  pull(w)

eval.forest <- causal_forest(X, Y, W,
                             clusters      = as.factor(te1$site_code),
                             sample.weights = site_counts,
                             num.trees     = 2000)#change to 10,000 for publication 2000

average_treatment_effect(eval.forest)
#  estimate    std.err 
#-25.175261   5.146604  



varimp <- variable_importance(eval.forest)
ranked.vars <- order(varimp, decreasing = TRUE)
top10_vars <- colnames(X)[ranked.vars[1:10]]
colnames(X)[ranked.vars[1:10]]
#[1] "PctGrass"          "PctAnnual"         "cv_ppt_intra"      "sand_0_15cm"       "sand_0_5cm"        "r_monthly_t_p"     "mean_sr"           "seasonality_index" "daily_ppt_d"       "ave.evenness"        

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

varimp_df <- tibble(
  variable = names(imp),
  value = as.numeric(imp)
) %>%
  mutate(
    moderator = recode(variable, !!!var_key)
  ) %>%
  filter(!is.na(moderator))

p_varimp <- ggplot(varimp_df, aes(x = reorder(moderator, value), y = value)) +
  geom_col(fill = "grey40") +
  coord_flip() +
  labs(x = NULL, y = "Split-based variable importance") +
  theme_base()


pred_fun <- function(object, newdata, ...) {
  predict(object, newdata, ...)$predictions
}
pdps <- lapply(top10_vars, function(v) plot(partial_dep(eval.forest, v=v, X = X, pred_fun = pred_fun
)))
wrap_plots(pdps, guides = "collect", ncol = 5) &
  ylim(c(-28,-20)) &
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


# ── Partial dependence plots with SE CI ──────────────────────────────────────
# Strategy: vary focal variable across its observed range; fix all other
# covariates at their column means.  predict(..., estimate.variance = TRUE)
# returns the IJ variance for each individual CATE, so SE = sqrt(var) is exact.
pdp_with_ci <- function(forest, focal_var, X, n_grid = 50) {
  
  grid_vals <- seq(
    min(X[[focal_var]], na.rm = TRUE),
    max(X[[focal_var]], na.rm = TRUE),
    length.out = n_grid
  )
  
  # Use colMeans() instead of summarise(across(...))
  X_means <- as.data.frame(as.list(colMeans(X, na.rm = TRUE)))
  
  newdata <- X_means[rep(1L, n_grid), ]
  newdata[[focal_var]] <- grid_vals
  
  preds <- predict(forest, newdata = newdata, estimate.variance = TRUE)
  
  tibble(
    x        = grid_vals,
    estimate = preds$predictions,
    se       = sqrt(preds$variance.estimates),
    lower    = estimate - se,
    upper    = estimate + se,
    variable = focal_var
  )
}

# Run for every moderator and bind into one data frame
pdp_data <- purrr::map_dfr(top10_vars, pdp_with_ci,
                           forest = eval.forest, X = X)

# Attach human-readable labels
pdp_data <- pdp_data %>%
  mutate(label = recode(variable, !!!var_key))

# One plot per moderator
pdp_plots <- pdp_data %>%
  group_by(variable) %>%
  group_split() %>%
  purrr::map(function(df) {
    ggplot(df, aes(x = x, y = estimate)) +
      geom_ribbon(aes(ymin = lower, ymax = upper),
                  fill = "grey70", alpha = 0.4) +
      geom_line(linewidth = 0.7, colour = "grey20") +
      geom_hline(yintercept = 0, linetype = "dashed",
                 colour = "grey50", linewidth = 0.4) +
      labs(
        x     = unique(df$label),
        y     = "Treatment effect of drought\non ANPP (g m\u207b\u00b2)",
        title = NULL
      ) +
      coord_cartesian(ylim = c(-35, 0)) +
      theme(
        panel.background = element_rect(fill = "white", colour = "grey50"),
        axis.title       = element_text(size = 8),
        axis.text        = element_text(size = 7)
      )
  })

wrap_plots(pdp_plots, ncol = 5)

ggsave(
  "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/moderator_treatmenteffects_predictions_kitchensink1.pdf",
  plot   = last_plot(),
  device = "pdf",
  width  = 13, height = 6, units = "in", dpi = 600
)



pdp_with_spread <- function(forest, focal_var, X, n_grid = 50) {
  
  grid_vals <- seq(min(X[[focal_var]], na.rm = TRUE),
                   max(X[[focal_var]], na.rm = TRUE),
                   length.out = n_grid)
  
  purrr::map_dfr(grid_vals, function(g) {
    # Take the real data, overwrite ONLY the focal column with g
    nd <- X
    nd[[focal_var]] <- g
    cate <- predict(forest, newdata = nd)$predictions   # one CATE per site-row
    
    tibble(
      x        = g,
      estimate = mean(cate),                    # marginal PDP line
      lower    = quantile(cate, 0.10, names = FALSE),
      upper    = quantile(cate, 0.90, names = FALSE),
      q25      = quantile(cate, 0.25, names = FALSE),
      q75      = quantile(cate, 0.75, names = FALSE),
      variable = focal_var
    )
  })
}

pdp_spread_data <- purrr::map_dfr(top10_vars, pdp_with_spread,
                                  forest = eval.forest, X = X) %>%
  mutate(label = recode(variable, !!!var_key))


pdp_spread_plots <- pdp_spread_data %>%
  group_by(variable) %>% group_split() %>%
  purrr::map(function(df) {
    ggplot(df, aes(x = x, y = estimate)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey95", alpha = 0.5) +
      geom_ribbon(aes(ymin = q25,   ymax = q75),   fill = "grey75", alpha = 0.5) +
      geom_line(linewidth = 0.7, colour = "grey15") +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
      labs(x = unique(df$label),
           y = "Treatment effect of drought\non ANPP (g m\u207b\u00b2)") +
      theme(panel.background = element_rect(fill = "white", colour = "grey50"),
            axis.title = element_text(size = 8), axis.text = element_text(size = 7))
  })

wrap_plots(pdp_spread_plots, ncol = 5)

ggsave(
  "C:/Users/ohler/Dropbox/Tim+Laura/IDE causal forest/figures/moderator_treatmenteffects_predictions_kitchensinkquantiles.pdf",
  plot   = last_plot(),
  device = "pdf",
  width  = 13, height = 6, units = "in", dpi = 600
)
