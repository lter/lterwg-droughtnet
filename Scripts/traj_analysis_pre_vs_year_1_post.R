## ------------------------------------------------------- ##
#     (NEW)  Exporting Trajectory Analysis Outputs
## ------------------------------------------------------- ##

## ------------------------------------------------------- ##
#     PRE-TREATMENT VS YEAR 1 POST-TREATMENT VERSION 
## ------------------------------------------------------- ##

# Clear environment
rm(list = ls())

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, RRPP, njlyon0/helpR, vegan)

# Name our export folders
export_folder <- paste0("export_trajectories_pre_vs_year_1_post_", Sys.Date())
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
  # Keep only rows where n_treat_years is 0 or 1 
  dplyr::filter(n_treat_years == 0 | n_treat_years == 1) %>%
  # Group by site because we will be filtering the minimum value for each site later
  dplyr::group_by(site_code) %>%
  # Make a new column indicating the sign of n_treat_days
  dplyr::mutate(treat_days_sign = case_when(
    n_treat_days < 0 ~ "negative_treat_days",
    n_treat_days > 0 ~ "positive_treat_days",
    n_treat_days == 0 ~ "zero_treat_days",
    is.na(n_treat_days) ~ "NA_treat_days",
  )) %>%
  # If n_treat_years is 0 and n_treat_days is negative, calculate the distance between 0 and n_treat_days
  dplyr::mutate(dist_from_zero = ifelse(
    test = n_treat_years == 0 & treat_days_sign == "negative_treat_days",
    yes = 0 - n_treat_days,
    no = "keep_for_later"
  )) %>%
  # For each site, out of all the rows where n_treat_years is 0 and n_treat_days is negative, drop the ones where there are multiple pre-treatment years
  # Basically for each site, there is now only 1 pre-treatment year
  # However, the pre-treatment year being kept is the one where n_treat_days is closest to 0
  # So if a site has n_treat_days = -700 and n_treat_days = -600 and both rows are n_treat_years = 0, the row with n_treat_days = -600 is kept
  dplyr::mutate(drop_or_keep = ifelse(
    test = dist_from_zero == min(dist_from_zero) | dist_from_zero == "keep_for_later",
    yes = "keep",
    no = "drop"
  )) %>%
  # Ungroup
  dplyr::ungroup() %>%
  # Keep the appropriate rows
  dplyr::filter(drop_or_keep == "keep") %>%
  # Drop the temporary columns
  dplyr::select(-treat_days_sign, -dist_from_zero, -drop_or_keep)
  
# Make a list of "bad" sites that will break the loop
bad_sites <- c("allmendb.ch", "allmendo.ch", "antelope.us", "ayora.es", "bfl.us", "chacra.ar",
               "chilcasdrt.ar", "eea.br", "guaribas.br", "hoide.de", "indiana.us", "jenadrt.de",
               "kernb.ca", "lygraint.no", "lygraold.no", "oklah.us", "pineta.es", "purdue.us",
               "qdtnorth.cl", "qdtsouth.cl", "slp.us", "teshio.jp")

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
