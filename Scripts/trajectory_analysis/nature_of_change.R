## ------------------------------------------------------- ##
           # Vote Counting for Nature of Change
## ------------------------------------------------------- ##

# Purpose:
## Identify sites that share the same pattern of significance across the 3 trajectory analysis metrics
## Sites that share these characteristics can be said to have the same "nature of change"
## And may be experiencing similar ecological drivers

## -------------------------------------- ##
              # Housekeeping ----
## -------------------------------------- ##
# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, RRPP)

# Clear environment
rm(list = ls())

# Read in composition data
comp_raw <- read.csv(file.path("cover_ppt_2023-01-02.csv"))

# Do some preliminary wrangling
comp <- comp_raw %>%
  #Filter to only accepted treatments and years after treatment application
  dplyr::filter(n_treat_years >= 1 & trt %in% c("Control", "Drought")) %>%
  # Make some new columns
  dplyr::mutate(
    ## Concatenated block-plot-subplot column
    block_plot_subplot = paste(block, plot, subplot, sep = "_"),
    ## Taxon column without spaces
    Taxon = gsub(pattern = " ", replacement = "_", x = Taxon)) %>%
  # Get average cover within year / treatment / block-plot-subplot
  dplyr::group_by(site_code, n_treat_years, year, trt, block_plot_subplot, Taxon) %>% 
  dplyr::summarize(max_cover = mean(max_cover, na.rm = T)) %>%
  dplyr::ungroup()

# Check out what that yields
dplyr::glimpse(comp)

# Make sure filter steps worked as desired
unique(comp$trt)
range(comp$n_treat_years)

## -------------------------------------- ##
         # Trajectory Analysis ----
## -------------------------------------- ##

# Make a list to export outputs to
out_list <- list()

# Make a vector of "bad" sites that will break the loop
bad_sites <- c()
# bad_sites <- c("chacra.ar", "cdpt_drt.us", "cobar.au", "eea.br", "hyide.de", 
#                "indiana.us", "jrnchi.us", "morient.ar", "nnss.us", "octc.us", "qdtnorth.cl", 
#                "sevblack.us", "sevblue.us", "sevmixed.us",
#                "jilpanger.au", "kiskun.hu", "lcnorth.cl", "lcsouth.cl", "purdue.us",
#                "qdtsouth.cl", "sand.us", "teshio.jp")

# Loop across sites to get trajectory analysis results
# for(focal_site in setdiff(x = unique(comp$site_code), y = bad_sites)){
for(focal_site in "allmendb.ch"){
  
  # Print starting message
  message("Processing begun for '", focal_site, "'")
  
  # Do necessary wrangling
  sub_data <- comp %>%
    # Filter data to only this site
    dplyr::filter(site_code == focal_site) %>%
    # Pivot to wide format
    tidyr::pivot_wider(names_from = Taxon, 
                       values_from = max_cover, 
                       values_fill = 0)
  
  # Now make a matrix version of just the community composition
  sub_mat <- sub_data %>%
    # Drop group columns
    dplyr::select(-site_code:-block_plot_subplot) %>%
    # Make it a matrix
    as.matrix()
  
  # Make the special dataframe required by the RRPP package
  sub_rdf <- RRPP::rrpp.data.frame("treatment" = sub_data$trt,
                                   "year" = sub_data$year,
                                   "plot" = sub_data$block_plot_subplot,
                                   "community" = sub_mat)
  # Fit perMANOVA model
  sub_fit <- RRPP::lm.rrpp(community ~ treatment * year,
                           data = sub_rdf, iter = 999, RRPP = T)
  
  # Run trajectory analysis
  sub_traj <- RRPP::trajectory.analysis(fit = sub_fit,
                                        groups = sub_rdf$treatment,
                                        traj.pts = sub_rdf$year)
  
  # Strip out parts of this
  sub_dist <- summary(sub_traj, attribute = "MD")
  sub_shape <- summary(sub_traj, attribute = "SD")
  sub_angle <- summary(sub_traj, attribute = "TC", angle.type = "deg")
  
  # Wrangle the distance output
  sub_dist_v2 <- tibble::as_tibble(as.list(sub_dist$x$PD$obs)) %>%
    # Now bring in remaining summary values
    cbind(sub_dist$summary.table) %>%
    # Make a column identifying which metric this is
    dplyr::mutate(metric = "distance")
  
  ## Fix column names
  names(sub_dist_v2) <- c(paste0("dist_", names(sub_dist_v2)[1]),
                          paste0("dist_", names(sub_dist_v2)[2]),
                          "diff", "UCL_95perc", "Z_Score", "P_Value", "metric")
  
  # Wrangle shape output
  sub_shape_v2 <- sub_shape$summary.table %>%
    # Rename columns
    dplyr::rename(diff = d,
                  UCL_95perc = `UCL (95%)`,
                  Z_Score = Z,
                  P_Value = `Pr > d`) %>%
    # Make a metric column
    dplyr::mutate(metric = "shape")
  
  # Wrangle angle output
  sub_angle_v2 <- sub_angle$summary.table %>%
    # Rename columns
    dplyr::rename(angle_r = r,
                  diff = angle,
                  UCL_95perc = `UCL (95%)`,
                  Z_Score = Z,
                  P_Value = `Pr > angle`) %>%
    # Make a metric column
    dplyr::mutate(metric = "angle")
  
  # Combine these extracted objects
  sub_combo <- sub_dist_v2 %>%
    # Bind distance, shape, and angle together by column name
    dplyr::bind_rows(sub_shape_v2, sub_angle_v2) %>%
    # Reorder columns
    dplyr::select(metric, dplyr::starts_with("dist_"),
                  angle_r, diff, UCL_95perc,
                  Z_Score, P_Value) %>%
    # Make some new columns
    dplyr::mutate(
      ## Make a column for site
      site_code = focal_site,
      ## Identify whether each metric was significant
      significance = ifelse(P_Value < 0.05, 
                            yes = paste0(metric, "-sig"), 
                            no = paste0(metric, "-NS")),
      ## Move both columns all the way to the left
      .before = dplyr::everything())
    
  # Drop the row names
  rownames(sub_combo) <- NULL
  
  # Finally, let's create an output object to preserve
  sub_actual <- sub_combo %>%
    # Identify nature of change
    dplyr::mutate(change_nature = paste(significance, collapse = "__"),
                  .before = dplyr::everything())
  
  # Add this to an output list
  out_list[[focal_site]] <- sub_actual
  
  # Ending message
  message("'", focal_site, "' complete") }

## -------------------------------------- ##
                # Export ----
## -------------------------------------- ##

# Wrangle the output list into a flat dataframe
out_df <- out_list %>%
  # Unlist by row binding
  list_rbind() %>%
  # And sort by change nature and site
  dplyr::arrange(change_nature, site_code)
 
# Glimpse output
dplyr::glimpse(out_df)

# Export locally
write.csv(x = out_df, row.names = F, na = '',
          file = file.path("traj-analysis_nature-of-change.csv"))

# End ----
