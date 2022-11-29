## ------------------------------------------------------- ##
#     (NEW)  Exporting Trajectory Analysis Outputs
## ------------------------------------------------------- ##

## ------------------------------------------------------- ##
#         EXCLUDE PRE-TREATMENT YEARS VERSION 
## ------------------------------------------------------- ##

# Clear environment
rm(list = ls())

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, RRPP, njlyon0/helpR, vegan)

# Name our export folders
export_folder <- paste0("export_trajectories_post_treatment_", Sys.Date())
nms_folder <- file.path(export_folder, "NMS_plots")
trajectory_folder <- file.path(export_folder, "trajectory_plots")
summary_stats_folder <- file.path(export_folder, "summary_stats")

# Make our export folders
dir.create(path = file.path(export_folder), showWarnings = FALSE)
dir.create(path = file.path(nms_folder), showWarnings = FALSE)
dir.create(path = file.path(trajectory_folder), showWarnings = FALSE)
dir.create(path = file.path(summary_stats_folder), showWarnings = FALSE)

# Create empty lists to store our statistics in later
anova_list <- list() 
traj_list <- list()

# Read in our data
comp_raw <- read.csv(file.path("cover_ppt_11-18-2022.csv"))

# Standardize our data
all_comp <- comp_raw %>%
  dplyr::mutate(trt = dplyr::case_when(
    trt == "Control_Infrastructure" ~ "Control",
    TRUE ~ trt)) %>%
  # Filter out unwanted plot types
  dplyr::filter(trt %in% c("Control", "Drought")) %>%
  # Filter out pre-treatment years
  dplyr::filter(n_treat_years >= 1)

# Make a list of "bad" sites that will break the loop
bad_sites <- c("chacra.ar", "cdpt_drt.us", "cobar.au", "eea.br", "hoide.de", "hyide.de", 
               "indiana.us", "jrnchi.us", "morient.ar", "nnss.us", "octc.us", "qdtnorth.cl", 
               "sevblack.us", "sevblue.us", "sevmixed.us")

# For the rest of the sites that work, we...
for (a_site in setdiff(x = unique(all_comp$site_code), y = bad_sites)){
  message("Processing begun for site: ", a_site)
  
  comp <- all_comp %>%
    # Filter to one site
    dplyr::filter(site_code == a_site) %>%
    # Generate a single column for block + plot + subplot
    dplyr::mutate(block_plot_subplot = paste(block, plot, subplot, sep = "_")) %>%
    # Pare down to only needed columns
    dplyr::select(year, trt, block_plot_subplot, Taxon, max_cover) %>%
    # Remove spaces from Taxon column
    dplyr::mutate(Taxon = gsub(pattern = " ", replacement = "_",
                               x = Taxon)) %>%
    group_by(year, trt, block_plot_subplot, Taxon) %>% 
    
    summarize(max_cover = mean(max_cover, na.rm = TRUE)) %>%
    
    ungroup() %>%
    
    # Pivot to wide format 
    tidyr::pivot_wider(names_from = Taxon,
                       values_from = max_cover,
                       values_fill = 0)
  
  if(ncol(comp) <= 5){
    message("Insufficient species")
  } else {
    # First, make our community composition information into a separate matrix
    comm_mat <- comp %>%
      dplyr::select(-year, -trt, -block_plot_subplot) %>%
      as.matrix()
    
    # Next, define the RRPP dataframe
    comp_rdf <- RRPP::rrpp.data.frame("treatment" = comp$trt,
                                      "year" = comp$year,
                                      "plot" = comp$block_plot_subplot,
                                      "community" = comm_mat)
    # Fit the model
    mod_fit <- RRPP::lm.rrpp(community ~ treatment * year,
                             data = comp_rdf, # data = RRPP 'dataframe'
                             iter = 999, # 999 permutations
                             RRPP = TRUE # permute residuals
    )
    
    # Get the summary values (with F statistics)
    anova_for_site <- anova(mod_fit, effect.type = "F")
    anova_list[[a_site]] <- anova_for_site
    
    # Perform multidimensional scaling
    mds_obj <- vegan::metaMDS(comp[-c(1:3)], distance = "bray",
                              autotransform = F, expand = F,
                              k = 2, try = 100)
    if(mds_obj$stress <= 0.01){
      message("What we have here is a failURE to converge")
    } else {
      
      # Create a column for the explanatory variables of each row
      comp$nms_group <- paste(comp$trt, comp$year, sep = "_")
      
      # Identify correct number of colors / shapes
      col_length <- length(unique(comp$nms_group)) / 2
      ide_colors <- c(rep("mediumseagreen", times = col_length),
                      rep("rosybrown4",times = col_length))
      ide_lines <- c(rep(1, times = col_length),
                     rep(2, times = col_length))
      
      # Saving the nms plot as a png
      png(filename = file.path(nms_folder, paste0(a_site, "_nms.png")), width = 850, height = 850, units = "px")
      
      # Feed those vectors into the ordination function
      helpR::nms_ord(mod = mds_obj, groupcol = comp$nms_group,
                     shapes = rep(x = 21:25, times = 4),
                     colors = ide_colors,
                     lines = ide_lines,
                     title = paste0(a_site),
                     leg_pos = 'topright')
      
      dev.off()
    }
    
    traj_fit <- RRPP::trajectory.analysis(fit = mod_fit,
                                          # Groups are control vs. drought
                                          groups = comp_rdf$treatment,
                                          # Trajectory "points" are years
                                          traj.pts = comp_rdf$year)
    
    # Magnitude distance (MD)
    traj_summary_md <- summary(traj_fit, attribute = "MD")
    traj_list[["md"]][[a_site]] <- traj_summary_md
    
    
    # Shape Differences (SD)
    traj_summary_sd <- summary(traj_fit, attribute = "SD")
    traj_list[["sd"]][[a_site]] <- traj_summary_sd
    
    
    # Angles (TC)
    traj_summary_tc <- summary(traj_fit, attribute = "TC", angle.type = "deg")
    traj_list[["tc"]][[a_site]] <- traj_summary_tc
    
    # Saving the trajectory plot as a png
    png(filename = file.path(trajectory_folder, paste0(a_site, "_trajectory.png")), width = 850, height = 850, units = "px")
    
    # Create a plot object
    traj_plot <- plot(traj_fit, pch = 21, cex = 0.7, col = "gray", main = paste0(a_site))
    
    # Then add trajectories
    RRPP::add.trajectories(traj_plot, traj.pch = 21)
    dev.off()
  }
}
