


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





