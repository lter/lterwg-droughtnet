## ------------------------------------------------------- ##
#         Exporting Trajectory Analysis Outputs
## ------------------------------------------------------- ##

## ------------------------------------------------------- ##
#     PRE-TREATMENT VS YEAR 2 POST-TREATMENT VERSION 
## ------------------------------------------------------- ##

# Clear environment
rm(list = ls())

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, RRPP, supportR, vegan, NCEAS/scicomptools)

# Name our export folders
export_folder <- paste0("export_trajectories_pre_vs_year_2_post_", Sys.Date())
trajectory_folder <- file.path(export_folder, "pre_vs_year_2_post_trajectory_plots")
summary_stats_folder <- file.path(export_folder, "pre_vs_year_2_post_summary_stats")

# Make our export folders
dir.create(path = file.path(export_folder), showWarnings = FALSE)
dir.create(path = file.path(trajectory_folder), showWarnings = FALSE)
dir.create(path = file.path(summary_stats_folder), showWarnings = FALSE)

# Create empty lists to store our statistics in later
fit_list <- list()
trt_legend_list <- list()
stats_list <- list()

# Read in our data
comp_raw <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/cover_ppt_2024-12-16.csv")

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

# Filter our data to year 2 post-treatment
comp_year_2 <- comp_raw %>%
  dplyr::mutate(trt = dplyr::case_when(
    trt == "Control_Infrastructure" ~ "Control",
    TRUE ~ trt)) %>%
  # Filter out unwanted plot types
  dplyr::filter(trt %in% c("Control", "Drought")) %>%
  # Keep only year 2 post-treatment
  dplyr::filter(n_treat_years == 2)

# Sites that have year 2 post-treatment data but not year 0 data
bad_sites_A <- setdiff(x = unique(comp_year_2$site_code), y = unique(comp_pre_treatment$site_code))

# Sites that have year 0 data but not year 2 post-treatment data
bad_sites_B <- setdiff(x = unique(comp_pre_treatment$site_code), y = unique(comp_year_2$site_code))

# Combine both together
all_comp <- bind_rows(comp_pre_treatment, comp_year_2)

# Make sure its only 0 and 2
unique(all_comp$n_treat_years)

# Make a list of "bad" sites that will break the loop
bad_sites <- c(bad_sites_A, bad_sites_B, "brokenh.au", 
               #"cobar.au",
               "hyide.de"
               )

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
    
    dplyr::summarize(max_cover = mean(max_cover, na.rm = TRUE)) %>%
    
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
    
    traj_fit <- RRPP::trajectory.analysis(fit = mod_fit,
                                          # Groups are control vs. drought
                                          groups = comp_rdf$treatment,
                                          # Trajectory "points" are years
                                          traj.pts = comp_rdf$year)
    fit_list[[a_site]] <- traj_fit
    trt_legend_list[[a_site]] <- comp_rdf$treatment
    
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
pdf(file.path(trajectory_folder, "traj_plots_pre_vs_year_2_post.pdf"))
par(mfrow = c(3,3))

for (a_site in names(fit_list)){
  
  # Create a plot object
  traj_plot <- plot(fit_list[[a_site]], pch = 3, cex = 0.4, col = "gray", main = paste0(a_site))
  
  # Then add trajectories
  RRPP::add.trajectories(traj_plot, traj.pch = c(21, 22))
  legend("topright", levels(as.factor(trt_legend_list[[a_site]])), pch =  c(21, 22), pt.bg = 1)
  
  # Add a title to our master pdf
  mtext("Trajectory plots for pre-treatment vs. year 2 post-treatment", outer=TRUE,  cex=0.7, line=-1.5)
  
}
dev.off()

# Extract the trajectory analysis statistics for each site except ukulingadrt.za 
for (a_site in setdiff(x = names(fit_list), y = c("cerrillos.ar"#"ukulingadrt.za","brokenh.au", "cobar.au", "hyide.de", "biddulph.ca"
  ))){
  message("Processing begun for site: ", a_site)
  site_stats <- stat_extract(mod_fit = fit_list[[a_site]])
  stats_list[[a_site]] <- site_stats
}

for (a_bad_site in c("ukulingadrt.za")){
  # Using stat_extract() on ukulingadrt.za gives an error so I will extract its magnitude distance statistics manually
  traj_summary_md <- tibble::as_tibble(as.list(summary(fit_list[[a_bad_site]], attribute = "MD")$x$PD$obs)) %>%
    # Now bring in remaining summary values
    cbind(summary(fit_list[[a_bad_site]], attribute = "MD")$summary.table) %>%
    # Make a column identifying which metric this is
    dplyr::mutate(metric = "distance") %>%
    # Rename some of these columns
    dplyr::rename(diff = d,
                  UCL_95perc = `UCL (95%)`,
                  Z_Score = Z,
                  P_Value = `Pr > d`) %>%
    dplyr::mutate(significance = dplyr::case_when(
      P_Value >= 0.05 ~ paste0(metric, "-NS"),
      is.na(P_Value) ~ paste0(metric, "-NULL"),
      TRUE ~ paste0(metric, "-sig")),
      .before = dplyr::everything())
  
  rownames(traj_summary_md) <- NULL
  
  # Put the ukulingadrt.za magnitude distance statistics back into our list
  stats_list[[a_bad_site]] <- traj_summary_md
}

# Flatten the statistics for each site into one master dataframe
traj_df <- stats_list %>%
  purrr::imap(.f = ~mutate(.x, site = paste0(.y)#, .before = tidyselect::everything()
  )) %>% 
  purrr::map_dfr(.f = dplyr::select, dplyr::everything()
  ) %>%
  dplyr::mutate(analysis_period = "year 0 vs year 2"#, .before = everything()
                )%>%
  subset(metric == "distance")


# Exporting the trajectory summary statistics csv
write.csv(traj_df, file = "C:/Users/ohler/Dropbox/IDE/papers/Community-comp_change/pre_vs_year_2_post_trajectory_summary.csv", row.names = FALSE)
