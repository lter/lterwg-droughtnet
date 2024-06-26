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
librarian::shelf(tidyverse, RRPP, NCEAS/scicomptools)

# Clear environment
rm(list = ls())

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
  dplyr::filter(year_ct > 1)

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

# Final wrangling to composiiton object
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
         # Trajectory Analysis ----
## -------------------------------------- ##

# Make a list to export outputs to
out_list <- list()

# Make a vector of "bad" sites that will break the loop
bad_sites <- c(
  # Error in `RRPP::trajectory.analysis`
  ## "Error: Not every trajectory point has replication (more than one observation)."
  "chacra.ar", "cobar.au", "hyide.de")

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
    dplyr::select(-site_trt:-year_ct) %>%
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
                .before = dplyr::everything()) %>%
  # Make a reminder column about which response this is
  dplyr::mutate(response = "species abundance", .before = dplyr::everything())
 
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
