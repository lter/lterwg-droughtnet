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
librarian::shelf(tidyverse, RRPP, supportR, vegan)

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
fit_list <- list()
trt_legend_list <- list()

# Read in our data
comp_raw <- read.csv(file.path("cover_ppt_2023-01-02.csv"))

# Filter our data to 1 pre-treatment year
comp_pre_treatment <- comp_raw %>%
  dplyr::mutate(trt = dplyr::case_when(
    trt == "Control_Infrastructure" ~ "Control",
    TRUE ~ trt)) %>%
  # Filter out unwanted plot types
  dplyr::filter(trt %in% c("Control", "Drought")) %>%
  # Group by site_code, trt, block, plot, subplot, year, Taxon
  dplyr::group_by(site_code, trt, block, plot, subplot, year, Taxon) %>%
  # Keep the rows where n_treat_years == 0 AND where the cover is max for the above grouping variables
  dplyr::filter(n_treat_years == 0 & max_cover == max(max_cover)) %>%
  # Ungroup
  dplyr::ungroup()

# Filter our data to year 1 post-treatment
comp_year_1 <- comp_raw %>%
  dplyr::mutate(trt = dplyr::case_when(
    trt == "Control_Infrastructure" ~ "Control",
    TRUE ~ trt)) %>%
  # Filter out unwanted plot types
  dplyr::filter(trt %in% c("Control", "Drought")) %>%
  # Keep only year 1 post-treatment
  dplyr::filter(n_treat_years == 1)

# Combine both together
all_comp <- bind_rows(comp_pre_treatment, comp_year_1)

# Make a list of "bad" sites that will break the loop
bad_sites <- c("antelope.us", "ayora.es", "bfl.us", "chacra.ar", "chilcasdrt.ar",
               "eea.br", "guaribas.br", "hoide.de", "hyide.de", "indiana.us", "jenadrt.de", 
               "oklah.us", "pineta.es", "slp.us", 
               "biddulph.ca", "cmss.us", "ethadb.au", "lcnorth.cl", "lcsouth.cl",
               "qdtnorth.cl", "qdtsouth.cl", "bayrdrt.de", "kinsella.ca", "validate.fr")

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
      supportR::nms_ord(mod = mds_obj, groupcol = comp$nms_group,
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
    fit_list[[a_site]] <- traj_fit
    trt_legend_list[[a_site]] <- comp_rdf$treatment
    
    
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
    traj_plot <- plot(traj_fit, pch = 3, cex = 0.7, col = "gray", main = paste0(a_site, " pre-treatment vs. year 1 post-treatment"))
    
    # Then add trajectories
    RRPP::add.trajectories(traj_plot, traj.pch = c(21, 22))
    legend("topright", levels(as.factor(comp_rdf$treatment)), pch =  c(21, 22), pt.bg = 1)
    
    dev.off()
    
  }
}

# Make a master pdf of all our trajectory plots
pdf(file.path(trajectory_folder, "traj_plots_pre_vs_year_1_post.pdf"))
par(mfrow = c(3,3))

for (a_site in names(fit_list)){

  # Create a plot object
  traj_plot <- plot(fit_list[[a_site]], pch = 3, cex = 0.4, col = "gray", main = paste0(a_site))
  
  # Then add trajectories
  RRPP::add.trajectories(traj_plot, traj.pch = c(21, 22))
  legend("topright", levels(as.factor(trt_legend_list[[a_site]])), pch =  c(21, 22), pt.bg = 1)
  
  # Add a title to our master pdf
  mtext("Trajectory plots for pre-treatment vs. year 1 post-treatment", outer=TRUE,  cex=0.7, line=-1.5)
  
}
dev.off()

# Extracting the anova statistics from each site and mapping every summary table into one dataframe
anova_df <- anova_list %>%
  purrr::map(.f = 1) %>%
  purrr::map(.f = ~as.data.frame(.)) %>%
  purrr::map(.f = ~rownames_to_column(.,"variable")) %>%
  purrr::imap(.f = ~mutate(.x, site = paste0(.y), .before = everything())) %>%
  purrr::map_dfr(.f = select, everything())

# Exporting the anova summary statistics csv
write.csv(anova_df, file = file.path(summary_stats_folder, "anova_summary.csv"), row.names = FALSE)

# Extracting the magnitude distance statistics from each site and mapping every summary table into one dataframe
mag_dist <- traj_list$md %>%
  purrr::map(.f = 3) %>%
  purrr::map(.f = ~as.data.frame(.)) %>%
  purrr::map(.f = ~rownames_to_column(.,"variable")) %>%
  purrr::map(.f = ~dplyr::rename(.,p_value = `Pr > d`))%>%
  purrr::imap(.f = ~mutate(.x, site = paste0(.y), .before = everything())) %>%
  purrr::map(.f = ~mutate(., r = NA, .after = variable)) %>%
  purrr::map(.f = ~mutate(., angle = NA, .after = r)) %>%
  purrr::imap(.f = ~mutate(., analysis_type = "Magnitude distance", .after = p_value)) %>% 
  purrr::map_dfr(.f = select, everything()) 

# Extracting the shape differences statistics from each site
shape_diff <- traj_list$sd %>%
  purrr::map(.f = 3)

# Create a placeholder data frame for the sites without any shape differences statistics
for (i in 1:length(shape_diff)){
  if (is.character(shape_diff[[i]])){
    shape_diff[[i]] <- data.frame(d = NA, `UCL (95%)` = NA, Z = NA, `Pr > d` = NA, row.names = "Control:Drought", check.names = FALSE)
  } else {
    next
  }
}

# Mapping every summary table into one dataframe
shape_diff_v2 <- shape_diff %>%
  purrr::map(.f = ~as.data.frame(.)) %>%
  purrr::map(.f = ~rownames_to_column(.,"variable")) %>%
  purrr::map(.f = ~dplyr::rename(., p_value = `Pr > d`)) %>% 
  purrr::imap(.f = ~mutate(.x, site = paste0(.y), .before = everything())) %>%
  purrr::map(.f = ~mutate(., r = NA, .after = variable)) %>%
  purrr::map(.f = ~mutate(., angle = NA, .after = r)) %>%
  purrr::map(.f = ~mutate(., analysis_type = "Shape differences", .after = p_value)) %>% 
  purrr::map_dfr(.f = select, everything()) 

# Extracting the angles statistics from each site and mapping every summary table into one dataframe
angles <- traj_list$tc %>%
  purrr::map(.f = 3)  %>%
  purrr::map(.f = ~as.data.frame(.)) %>%
  purrr::map(.f = ~rownames_to_column(.,"variable")) %>%
  purrr::map(.f = ~rename(.,p_value = `Pr > angle`)) %>%
  purrr::imap(.f = ~mutate(.x, site = paste0(.y), .before = everything())) %>%
  purrr::map(.f = ~mutate(., analysis_type = "Angles", .after = p_value)) %>%
  purrr::map(.f = ~mutate(., d = NA, .after = angle)) %>% 
  purrr::map_dfr(.f = select, everything()) 

# Combine the magnitude distances, shape differences, and angles together into one master dataframe
traj_df <- dplyr::bind_rows(mag_dist, shape_diff_v2, angles)

# Exporting the trajectory summary statistics csv
write.csv(traj_df, file = file.path(summary_stats_folder, "trajectory_summary.csv"), row.names = FALSE)
