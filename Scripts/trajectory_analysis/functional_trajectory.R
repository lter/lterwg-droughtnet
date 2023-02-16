## ------------------------------------------------------- ##
           # Functional Group Trajectory Analysis
## ------------------------------------------------------- ##

# Purpose:
## Perform trajectory analysis on non-species abundances
## Specifically, on functional group abundance & family abundance

## -------------------------------------- ##
            # Housekeeping ----
## -------------------------------------- ##
# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, RRPP, NCEAS/scicomptools, supportR)

# Clear environment
rm(list = ls())

# Read in raw trait info
taxa_raw <- read.csv(file.path("IDE_taxa_info_2023-02-06.csv"))

# Tidy it using the dedicated 'trait_tidy' script
source(file.path("Scripts", "trajectory_analysis", "trait_tidy.R"))

# Glimpse it
dplyr::glimpse(taxa_info)

## -------------------------------------- ##
          # Necessary Wrangling ----
## -------------------------------------- ##
# Read in composition data
comp_raw <- read.csv(file.path("cover_ppt_2023-01-02.csv"))

# Do some preliminary wrangling
comp_v2 <- comp_raw %>%
  # Filter to include only the *first* calendar year before treatment AND all post-treatment years
  dplyr::filter(n_treat_years >= 0) %>%
  # Now make n_treat_years a character
  dplyr::mutate(n_treat_years = as.character(n_treat_years)) %>%
  # Filter to only accepted treatments
  dplyr::filter(trt %in% c("Control", "Drought")) %>%
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
  dplyr::mutate(year_ct = length(unique(n_treat_years))) %>%
  dplyr::ungroup() %>%
  # Trajectory analysis can't be run / doesn't make sense for sites with only one year of data (no trajectory can exist with only one point per group)
  dplyr::filter(year_ct > 1) %>%
  # Attach the functional group information
  dplyr::left_join(y = taxa_info, by = c("Taxon", "site_code"))

# Check that out
dplyr::glimpse(comp_v2)

# Now need to identify which (if any) sites have unequal replicates between treatments
uneq_reps <- comp_v2 %>%
  # Count number of replicates of site-treatment combinations
  dplyr::group_by(site_code, trt) %>%
  dplyr::summarize(traj_reps = length(unique(n_treat_years))) %>%
  dplyr::ungroup() %>%
  # Pivot that new column to wide format
  tidyr::pivot_wider(names_from = trt, values_from = traj_reps) %>%
  # Filter to only cases where control and drought DON'T have the same rep number
  dplyr::filter(Control != Drought)

# Check what that leaves
uneq_reps

# Final wrangling to composition object
comp <- comp_v2 %>%
  # Drop sites with unequal replicates
  dplyr::filter(!site_code %in% c(uneq_reps$site_code)) %>%
  # Make a site + treatment column
  dplyr::mutate(site_trt = paste0(site_code, "_", trt),
                .before = dplyr::everything())
  
# Check out what that yields
dplyr::glimpse(comp)

# Make sure filter steps worked as desired
unique(comp$trt)
sort(unique(comp$n_treat_years))
range(comp$year_ct)

# Sites dropped for only 1 year of data
setdiff(x = unique(comp_raw$site_code), y = unique(comp_v2$site_code))

# Sites dropped for unequal drought vs. control year replicates
setdiff(x = unique(comp_v2$site_code), y = unique(comp$site_code))

## -------------------------------------- ##
   # Function Analysis - Provenance ----
## -------------------------------------- ##

# Wrangle composition object for this analysis
fxn_df <- comp %>%
  # Group by desired columns and summarize
  dplyr::group_by(site_trt, site_code, trt, n_treat_years, block_plot_subplot, provenance) %>% 
  dplyr::summarize(max_cover = mean(max_cover, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Filter out any unknown families
  dplyr::filter(!is.na(provenance)) %>%
  # Pivot to wide format
  tidyr::pivot_wider(names_from = provenance, 
                     values_from = max_cover, 
                     values_fill = 0) %>%
  # Count trajectory replicates
  dplyr::group_by(site_trt, site_code, trt, n_treat_years) %>%
  dplyr::mutate(traj_reps = dplyr::n()) %>%
  dplyr::ungroup() %>%
  # Identify minimum replicates per site/treatment
  dplyr::group_by(site_code) %>%
  dplyr::mutate(min_reps = min(traj_reps, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Drop sites with insufficient replicates
  dplyr::filter(min_reps > 1) %>%
  # Drop the replicate counting columns
  dplyr::select(-dplyr::ends_with("_reps")) %>%
  # Now that we've excluded some sites, we may have columns of all zeros so we need to drop them
  ## Can do this by pivoting long, dropping zeros, then pivoting wide again
  tidyr::pivot_longer(cols = -site_trt:-block_plot_subplot) %>%
  dplyr::filter(value > 0) %>%
  # Count whether there are more than 1 levels
  dplyr::group_by(site_code) %>%
  dplyr::mutate(level_ct = length(unique(name))) %>%
  dplyr::ungroup() %>%
  # Drop sites with only one level (i.e., not multivariate)
  dplyr::filter(level_ct > 1) %>%
  # Drop that column
  dplyr::select(-level_ct)

# See what we lost
setdiff(x = unique(comp$site_code), y = unique(fxn_df$site_code))

# Glimpse this
dplyr::glimpse(fxn_df)
## Now we can do the analytical workflow!

# Make a list to store this information
out_list <- list()

# Make a vector of "bad" sites that will break the loop
bad_sites <- c()

# Iterate across sites
for(focal_site in setdiff(x = unique(fxn_df$site_code), y = bad_sites)){
  # for(focal_site in "allmendb.ch"){
  
  # Print starting message
  message("Processing begun for '", focal_site, "'")
  
  # Do necessary wrangling
  sub_data <- fxn_df %>%
    # Filter data to only this site
    dplyr::filter(site_code == focal_site) %>%
    # Pivot to wide format
    tidyr::pivot_wider(names_from = name, 
                       values_from = value, 
                       values_fill = 0)
  
  # Now make a matrix version of just the community composition
  sub_mat <- sub_data %>%
    # Drop group columns
    dplyr::select(-site_trt:-block_plot_subplot) %>%
    # Make it a matrix
    as.matrix()
  
  # Make the special dataframe required by the RRPP package
  sub_rdf <- RRPP::rrpp.data.frame("treatment" = sub_data$trt,
                                   "year" = sub_data$n_treat_years,
                                   "plot" = sub_data$block_plot_subplot,
                                   "community" = sub_mat)
  
  # Fit perMANOVA model
  sub_fit <- RRPP::lm.rrpp(community ~ treatment * year,
                           data = sub_rdf, iter = 999, RRPP = T)
  
  # Run trajectory analysis
  sub_traj <- RRPP::trajectory.analysis(fit = sub_fit,
                                        groups = sub_rdf$treatment,
                                        traj.pts = sub_rdf$year)
  
  # Extract relevant summary information
  sub_metrics <- scicomptools::traj_extract(traj_mod = sub_traj, angle_type = "deg")
  
  # Finally, let's create an output object to preserve
  sub_actual <- sub_metrics %>%
    # Make a site column
    dplyr::mutate(site_code = focal_site, .before = dplyr::everything()) %>%
    # Identify nature of change
    dplyr::mutate(change_nature = paste(significance, collapse = "__"),
                  .after = site_code)
  
  # Add this to an output list
  out_list[[focal_site]] <- sub_actual
  
  # Ending message
  message("'", focal_site, "' complete") }

## -------------------------------------- ##
    # Function Export - Provenance ----
## -------------------------------------- ##

# Wrangle the output list into a flat dataframe
out_df <- out_list %>%
  # Unlist by row binding
  list_rbind() %>%
  # And sort by change nature and site
  dplyr::arrange(change_nature, site_code) %>%
  # Make a simpler change nature column
  dplyr::mutate(change_simp = paste0("type ", as.numeric(as.factor(change_nature))),
                .before = dplyr::everything()) %>%
  # Make a reminder column about which response this is
  dplyr::mutate(response = "provenance results", .before = dplyr::everything())

# Glimpse output
dplyr::glimpse(out_df)

# How many change types?
out_df %>%
  dplyr::group_by(change_simp, change_nature) %>%
  dplyr::summarize(site_ct = length(unique(site_code)))

# Export locally
write.csv(x = out_df, row.names = F, na = '',
          file = file.path("traj-analysis_provenance-results.csv"))

# Clean up environment
rm(list = setdiff(ls(), c("comp")))

## -------------------------------------- ##
    # Function Analysis - Lifeform ----
## -------------------------------------- ##

# Wrangle composition object for this analysis
fxn_df <- comp %>%
  # Group by desired columns and summarize
  dplyr::group_by(site_trt, site_code, trt, n_treat_years, block_plot_subplot, lifeform) %>% 
  dplyr::summarize(max_cover = mean(max_cover, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Filter out any unknown families
  dplyr::filter(!is.na(lifeform)) %>%
  # Pivot to wide format
  tidyr::pivot_wider(names_from = lifeform, 
                     values_from = max_cover, 
                     values_fill = 0) %>%
  # Count trajectory replicates
  dplyr::group_by(site_trt, site_code, trt, n_treat_years) %>%
  dplyr::mutate(traj_reps = dplyr::n()) %>%
  dplyr::ungroup() %>%
  # Identify minimum replicates per site/treatment
  dplyr::group_by(site_code) %>%
  dplyr::mutate(min_reps = min(traj_reps, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Drop sites with insufficient replicates
  dplyr::filter(min_reps > 1) %>%
  # Drop the replicate counting columns
  dplyr::select(-dplyr::ends_with("_reps")) %>%
  # Now that we've excluded some sites, we may have columns of all zeros so we need to drop them
  ## Can do this by pivoting long, dropping zeros, then pivoting wide again
  tidyr::pivot_longer(cols = -site_trt:-block_plot_subplot) %>%
  dplyr::filter(value > 0) %>%
  # Count whether there are more than 1 levels
  dplyr::group_by(site_code) %>%
  dplyr::mutate(level_ct = length(unique(name))) %>%
  dplyr::ungroup() %>%
  # Drop sites with only one level (i.e., not multivariate)
  dplyr::filter(level_ct > 1) %>%
  # Drop that column
  dplyr::select(-level_ct)

# See what we lost
setdiff(x = unique(comp$site_code), y = unique(fxn_df$site_code))

# Glimpse this
dplyr::glimpse(fxn_df)
## Now we can do the analytical workflow!

# Make a list to store this information
out_list <- list()

# Make a vector of "bad" sites that will break the loop
bad_sites <- c()

# Iterate across sites
for(focal_site in setdiff(x = unique(fxn_df$site_code), y = bad_sites)){
  # for(focal_site in "allmendb.ch"){
  
  # Print starting message
  message("Processing begun for '", focal_site, "'")
  
  # Do necessary wrangling
  sub_data <- fxn_df %>%
    # Filter data to only this site
    dplyr::filter(site_code == focal_site) %>%
    # Pivot to wide format
    tidyr::pivot_wider(names_from = name, 
                       values_from = value, 
                       values_fill = 0)
  
  # Now make a matrix version of just the community composition
  sub_mat <- sub_data %>%
    # Drop group columns
    dplyr::select(-site_trt:-block_plot_subplot) %>%
    # Make it a matrix
    as.matrix()
  
  # Make the special dataframe required by the RRPP package
  sub_rdf <- RRPP::rrpp.data.frame("treatment" = sub_data$trt,
                                   "year" = sub_data$n_treat_years,
                                   "plot" = sub_data$block_plot_subplot,
                                   "community" = sub_mat)
  
  # Fit perMANOVA model
  sub_fit <- RRPP::lm.rrpp(community ~ treatment * year,
                           data = sub_rdf, iter = 999, RRPP = T)
  
  # Run trajectory analysis
  sub_traj <- RRPP::trajectory.analysis(fit = sub_fit,
                                        groups = sub_rdf$treatment,
                                        traj.pts = sub_rdf$year)
  
  # Extract relevant summary information
  sub_metrics <- scicomptools::traj_extract(traj_mod = sub_traj, angle_type = "deg")
  
  # Finally, let's create an output object to preserve
  sub_actual <- sub_metrics %>%
    # Make a site column
    dplyr::mutate(site_code = focal_site, .before = dplyr::everything()) %>%
    # Identify nature of change
    dplyr::mutate(change_nature = paste(significance, collapse = "__"),
                  .after = site_code)
  
  # Add this to an output list
  out_list[[focal_site]] <- sub_actual
  
  # Ending message
  message("'", focal_site, "' complete") }

## -------------------------------------- ##
      # Function Export - Lifeform ----
## -------------------------------------- ##

# Wrangle the output list into a flat dataframe
out_df <- out_list %>%
  # Unlist by row binding
  list_rbind() %>%
  # And sort by change nature and site
  dplyr::arrange(change_nature, site_code) %>%
  # Make a simpler change nature column
  dplyr::mutate(change_simp = paste0("type ", as.numeric(as.factor(change_nature))),
                .before = dplyr::everything()) %>%
  # Make a reminder column about which response this is
  dplyr::mutate(response = "lifeform results", .before = dplyr::everything())

# Glimpse output
dplyr::glimpse(out_df)

# How many change types?
out_df %>%
  dplyr::group_by(change_simp, change_nature) %>%
  dplyr::summarize(site_ct = length(unique(site_code)))

# Export locally
write.csv(x = out_df, row.names = F, na = '',
          file = file.path("traj-analysis_lifeform-results.csv"))

# Clean up environment
rm(list = setdiff(ls(), c("comp")))

## -------------------------------------- ##
    # Function Analysis - Lifespan ----
## -------------------------------------- ##

# Wrangle composition object for this analysis
fxn_df <- comp %>%
  # Group by desired columns and summarize
  dplyr::group_by(site_trt, site_code, trt, n_treat_years, block_plot_subplot, lifespan) %>% 
  dplyr::summarize(max_cover = mean(max_cover, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Filter out any unknown families
  dplyr::filter(!is.na(lifespan)) %>%
  # Pivot to wide format
  tidyr::pivot_wider(names_from = lifespan, 
                     values_from = max_cover, 
                     values_fill = 0) %>%
  # Count trajectory replicates
  dplyr::group_by(site_trt, site_code, trt, n_treat_years) %>%
  dplyr::mutate(traj_reps = dplyr::n()) %>%
  dplyr::ungroup() %>%
  # Identify minimum replicates per site/treatment
  dplyr::group_by(site_code) %>%
  dplyr::mutate(min_reps = min(traj_reps, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Drop sites with insufficient replicates
  dplyr::filter(min_reps > 1) %>%
  # Drop the replicate counting columns
  dplyr::select(-dplyr::ends_with("_reps")) %>%
  # Now that we've excluded some sites, we may have columns of all zeros so we need to drop them
  ## Can do this by pivoting long, dropping zeros, then pivoting wide again
  tidyr::pivot_longer(cols = -site_trt:-block_plot_subplot) %>%
  dplyr::filter(value > 0) %>%
  # Count whether there are more than 1 levels
  dplyr::group_by(site_code) %>%
  dplyr::mutate(level_ct = length(unique(name))) %>%
  dplyr::ungroup() %>%
  # Drop sites with only one level (i.e., not multivariate)
  dplyr::filter(level_ct > 1) %>%
  # Drop that column
  dplyr::select(-level_ct)

# See what we lost
setdiff(x = unique(comp$site_code), y = unique(fxn_df$site_code))

# Glimpse this
dplyr::glimpse(fxn_df)
## Now we can do the analytical workflow!

# Make a list to store this information
out_list <- list()

# Make a vector of "bad" sites that will break the loop
bad_sites <- c()

# Iterate across sites
for(focal_site in setdiff(x = unique(fxn_df$site_code), y = bad_sites)){
  # for(focal_site in "allmendb.ch"){
  
  # Print starting message
  message("Processing begun for '", focal_site, "'")
  
  # Do necessary wrangling
  sub_data <- fxn_df %>%
    # Filter data to only this site
    dplyr::filter(site_code == focal_site) %>%
    # Pivot to wide format
    tidyr::pivot_wider(names_from = name, 
                       values_from = value, 
                       values_fill = 0)
  
  # Now make a matrix version of just the community composition
  sub_mat <- sub_data %>%
    # Drop group columns
    dplyr::select(-site_trt:-block_plot_subplot) %>%
    # Make it a matrix
    as.matrix()
  
  # Make the special dataframe required by the RRPP package
  sub_rdf <- RRPP::rrpp.data.frame("treatment" = sub_data$trt,
                                   "year" = sub_data$n_treat_years,
                                   "plot" = sub_data$block_plot_subplot,
                                   "community" = sub_mat)
  
  # Fit perMANOVA model
  sub_fit <- RRPP::lm.rrpp(community ~ treatment * year,
                           data = sub_rdf, iter = 999, RRPP = T)
  
  # Run trajectory analysis
  sub_traj <- RRPP::trajectory.analysis(fit = sub_fit,
                                        groups = sub_rdf$treatment,
                                        traj.pts = sub_rdf$year)
  
  # Extract relevant summary information
  sub_metrics <- scicomptools::traj_extract(traj_mod = sub_traj, angle_type = "deg")
  
  # Finally, let's create an output object to preserve
  sub_actual <- sub_metrics %>%
    # Make a site column
    dplyr::mutate(site_code = focal_site, .before = dplyr::everything()) %>%
    # Identify nature of change
    dplyr::mutate(change_nature = paste(significance, collapse = "__"),
                  .after = site_code)
  
  # Add this to an output list
  out_list[[focal_site]] <- sub_actual
  
  # Ending message
  message("'", focal_site, "' complete") }

## -------------------------------------- ##
      # Function Export - Lifespan ----
## -------------------------------------- ##

# Wrangle the output list into a flat dataframe
out_df <- out_list %>%
  # Unlist by row binding
  list_rbind() %>%
  # And sort by change nature and site
  dplyr::arrange(change_nature, site_code) %>%
  # Make a simpler change nature column
  dplyr::mutate(change_simp = paste0("type ", as.numeric(as.factor(change_nature))),
                .before = dplyr::everything()) %>%
  # Make a reminder column about which response this is
  dplyr::mutate(response = "lifespan results", .before = dplyr::everything())

# Glimpse output
dplyr::glimpse(out_df)

# How many change types?
out_df %>%
  dplyr::group_by(change_simp, change_nature) %>%
  dplyr::summarize(site_ct = length(unique(site_code)))

# Export locally
write.csv(x = out_df, row.names = F, na = '',
          file = file.path("traj-analysis_lifespan-results.csv"))

# Clean up environment
rm(list = setdiff(ls(), c("comp")))

## -------------------------------------- ##
 # Function Analysis - Photosynth. Path ----
## -------------------------------------- ##

# Wrangle composition object for this analysis
fxn_df <- comp %>%
  # Group by desired columns and summarize
  dplyr::group_by(site_trt, site_code, trt, n_treat_years, block_plot_subplot, ps_path) %>% 
  dplyr::summarize(max_cover = mean(max_cover, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Filter out any unknown families
  dplyr::filter(!is.na(ps_path)) %>%
  # Pivot to wide format
  tidyr::pivot_wider(names_from = ps_path, 
                     values_from = max_cover, 
                     values_fill = 0) %>%
  # Count trajectory replicates
  dplyr::group_by(site_trt, site_code, trt, n_treat_years) %>%
  dplyr::mutate(traj_reps = dplyr::n()) %>%
  dplyr::ungroup() %>%
  # Identify minimum replicates per site/treatment
  dplyr::group_by(site_code) %>%
  dplyr::mutate(min_reps = min(traj_reps, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Drop sites with insufficient replicates
  dplyr::filter(min_reps > 1) %>%
  # Drop the replicate counting columns
  dplyr::select(-dplyr::ends_with("_reps")) %>%
  # Now that we've excluded some sites, we may have columns of all zeros so we need to drop them
  ## Can do this by pivoting long, dropping zeros, then pivoting wide again
  tidyr::pivot_longer(cols = -site_trt:-block_plot_subplot) %>%
  dplyr::filter(value > 0) %>%
  # Count whether there are more than 1 levels
  dplyr::group_by(site_code) %>%
  dplyr::mutate(level_ct = length(unique(name))) %>%
  dplyr::ungroup() %>%
  # Drop sites with only one level (i.e., not multivariate)
  dplyr::filter(level_ct > 1) %>%
  # Drop that column
  dplyr::select(-level_ct)

# See what we lost
setdiff(x = unique(comp$site_code), y = unique(fxn_df$site_code))

# Glimpse this
dplyr::glimpse(fxn_df)
## Now we can do the analytical workflow!

# Make a list to store this information
out_list <- list()

# Make a vector of "bad" sites that will break the loop
bad_sites <- c(
  # Error in `RRPP::trajectory.analysis`
  ## "Error in dimnames(to) <- list(gl, gl) : length of 'dimnames' [1] not equal to array extent"
  "charleville.au"
)

# Iterate across sites
for(focal_site in setdiff(x = unique(fxn_df$site_code), y = bad_sites)){
  # for(focal_site in "jilpanger.au"){
  
  # Print starting message
  message("Processing begun for '", focal_site, "'")
  
  # Do necessary wrangling
  sub_data <- fxn_df %>%
    # Filter data to only this site
    dplyr::filter(site_code == focal_site) %>%
    # Pivot to wide format
    tidyr::pivot_wider(names_from = name, 
                       values_from = value, 
                       values_fill = 0)
  
  # Now make a matrix version of just the community composition
  sub_mat <- sub_data %>%
    # Drop group columns
    dplyr::select(-site_trt:-block_plot_subplot) %>%
    # Make it a matrix
    as.matrix()
  
  # Make the special dataframe required by the RRPP package
  sub_rdf <- RRPP::rrpp.data.frame("treatment" = sub_data$trt,
                                   "year" = sub_data$n_treat_years,
                                   "plot" = sub_data$block_plot_subplot,
                                   "community" = sub_mat)
  
  # Fit perMANOVA model
  sub_fit <- RRPP::lm.rrpp(community ~ treatment * year,
                           data = sub_rdf, iter = 999, RRPP = T)
  
  # Run trajectory analysis
  sub_traj <- RRPP::trajectory.analysis(fit = sub_fit,
                                        groups = sub_rdf$treatment,
                                        traj.pts = sub_rdf$year)
  
  # Extract relevant summary information
  sub_metrics <- scicomptools::traj_extract(traj_mod = sub_traj, angle_type = "deg")
  
  # Finally, let's create an output object to preserve
  sub_actual <- sub_metrics %>%
    # Make a site column
    dplyr::mutate(site_code = focal_site, .before = dplyr::everything()) %>%
    # Identify nature of change
    dplyr::mutate(change_nature = paste(significance, collapse = "__"),
                  .after = site_code)
  
  # Add this to an output list
  out_list[[focal_site]] <- sub_actual
  
  # Ending message
  message("'", focal_site, "' complete") }

## -------------------------------------- ##
 # Function Export - Photosynth. Path ----
## -------------------------------------- ##

# Wrangle the output list into a flat dataframe
out_df <- out_list %>%
  # Unlist by row binding
  list_rbind() %>%
  # And sort by change nature and site
  dplyr::arrange(change_nature, site_code) %>%
  # Make a simpler change nature column
  dplyr::mutate(change_simp = paste0("type ", as.numeric(as.factor(change_nature))),
                .before = dplyr::everything()) %>%
  # Make a reminder column about which response this is
  dplyr::mutate(response = "ps_path results", .before = dplyr::everything())

# Glimpse output
dplyr::glimpse(out_df)

# How many change types?
out_df %>%
  dplyr::group_by(change_simp, change_nature) %>%
  dplyr::summarize(site_ct = length(unique(site_code)))

# Export locally
write.csv(x = out_df, row.names = F, na = '',
          file = file.path("traj-analysis_ps-path-results.csv"))

# Clean up environment
rm(list = setdiff(ls(), c("comp")))

# End ----
