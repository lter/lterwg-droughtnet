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
  dplyr::ungroup() %>%
  # Identify the number of treatment years for each site
  dplyr::group_by(site_code) %>%
  dplyr::mutate(year_ct = length(unique(year))) %>%
  dplyr::ungroup() %>%
  # Trajectory analysis can't be run / doesn't make sense for sites with only one year of data (no trajectory can exist with only one point per group)
  dplyr::filter(year_ct > 1)

# Check out what that yields
dplyr::glimpse(comp)

# Make sure filter steps worked as desired
unique(comp$trt)
range(comp$n_treat_years)
range(comp$year_ct)

# Note that we've dropped sites with only one year of data as they cannot have a trajectory
## Here is the set of all sites that were dropped between "comp_raw" and "comp"
setdiff(x = unique(comp_raw$site_code), y = unique(comp$site_code))

## -------------------------------------- ##
         # Trajectory Analysis ----
## -------------------------------------- ##

# Make a list to export outputs to
out_list <- list()

# Make a vector of "bad" sites that will break the loop
bad_sites <- c(
  # Error in `RRPP::trajectory.analysis`
  ## "Error: Not every trajectory point has replication (more than one observation)."
  "chacra.ar", "cobar.au",
  # Error in attempting to get angle results from trajectory analysis
  ## "Error in if (any(y <= 0)) y = y - min(y) + 1e-04 : missing value where TRUE/FALSE needed"
  "jorndrt.us")

# Loop across sites to get trajectory analysis results
for(focal_site in setdiff(x = unique(comp$site_code), y = bad_sites)){
# for(focal_site in "allmendb.ch"){
  
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
  
  # If there are only two points we'll need to skip shape 
  if(length(unique(sub_data$year)) == 2){
    # Make an empty dummy df in this case
    sub_shape_v2 <- data.frame("diff" = NA,
                               "UCL_95perc" = NA,
                               "Z_Score" = NA,
                               "P_Value" = NA,
                               "metric" = "shape")
    # Otherwise...
    } else {
  # Wrangle shape output
  sub_shape_v2 <- sub_shape$summary.table %>%
    # Rename columns
    dplyr::rename(diff = d,
                  UCL_95perc = `UCL (95%)`,
                  Z_Score = Z,
                  P_Value = `Pr > d`) %>%
    # Make a metric column
    dplyr::mutate(metric = "shape") }
  
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
      significance = ifelse(P_Value >= 0.05 | is.na(P_Value), 
                            yes = paste0(metric, "-NS"), 
                            no = paste0(metric, "-sig")),
      ## Move both columns all the way to the left
      .before = dplyr::everything())
    
  # Drop the row names
  rownames(sub_combo) <- NULL
  
  # Finally, let's create an output object to preserve
  sub_actual <- sub_combo %>%
    # Identify nature of change
    dplyr::mutate(change_nature = paste(significance, collapse = "__"),
                  .after = site_code)
  
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
  dplyr::arrange(change_nature, site_code) %>%
  # Make a simpler change nature column
  dplyr::mutate(change_simp = paste0("type ", as.numeric(as.factor(change_nature))),
                .before = dplyr::everything())
 
# Glimpse output
dplyr::glimpse(out_df)

# How many change types?
out_df %>%
  dplyr::group_by(change_simp, change_nature) %>%
  dplyr::summarize(site_ct = length(unique(site_code)))

# Export locally
write.csv(x = out_df, row.names = F, na = '',
          file = file.path("traj-analysis_nature-of-change.csv"))

# End ----
