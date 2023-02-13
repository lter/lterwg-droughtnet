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
librarian::shelf(tidyverse, RRPP, NCEAS/scicomptools)

# Clear environment
rm(list = ls())

# Read in trait information (and wrangle slightly)
taxa_info <- read.csv(file.path("IDE_taxa_info_2023-02-06.csv")) %>%
  # Make taxon column match how it appears in the composition data
  dplyr::mutate(Taxon = toupper(gsub(pattern = " ", replacement = "_", x = standard.Taxon))) %>%
  # Pare down to just desired columns
  dplyr::select(Taxon, site_code, Family, provenance, lifeform, lifespan, ps_path) %>%
  # Pivot longer
  tidyr::pivot_longer(cols = -Taxon:-site_code) %>%
  # Drop missing trait values
  dplyr::filter(value != "NULL" & nchar(value) != 0) %>%
  # Standardize some disagreeing entries in the CSV
  dplyr::mutate(value = dplyr::case_when(
    ## All Bryophyte's lifeforms should be BRYOPHYTE
    stringr::str_sub(Taxon, start = 1, end = 12) == "BRYOPHYTE_SP" & 
      name == "lifeform" ~ "BRYOPHYTE",
    ## One species is listed as both PERENNIAL and ANNUAL
    Taxon == "LAGENOPHORA_STIPITATA" & site_code == "jilpanger.au" & 
      name == "lifespan" ~ "INDETERMINATE",
    ## Same issue for some unknown species at other sites
    Taxon == "UNKNOWN_GRASS" & site_code == "kiny.au" &
      name == "lifespan" ~ "INDETERMINATE",
    Taxon == "UNKNOWN_POACEAE_SP.(HARD.US)" & site_code == "hard.us" &
      name == "lifespan" ~ "INDETERMINATE",
    Taxon == "UNKNOWN__SP.(LCSOUTH.CL)" & site_code == "lcsouth.cl" &
      name == "lifespan" ~ "INDETERMINATE",
    ## Trifolium provenance is inconsistently identified for some sites
    Taxon == "TRIFOLIUM_SP." & site_code == "hopl.us" & name == "provenance" ~ "NAT",
    Taxon == "TRIFOLIUM_SP." & site_code == "kiny.au" & name == "provenance" ~ "INT",
    ## Provenance is also inconsistently identified for some unknown species
    Taxon == "UNKNOWN_SP.(LYGRAINT.NO)" & site_code == "lygraint.no" &
      name == "provenance" ~ "NAT",
    Taxon == "UNKNOWN_ASTERACEAE" & site_code == "kiny.au" &
      name == "provenance" ~ "INT",
    Taxon == "UNKNOWN" & site_code == "kiny.au" &
      name == "provenance" ~ "NAT",
    ## Non-issue entries should be returned as they are
  TRUE ~ value)) %>%
  # Some fixes require altering the Taxon name
  dplyr::mutate(Taxon = dplyr::case_when(
    ## "UNKOWN" used for multiple different unknown taxa in different functional groups
    Taxon == "UNKNOWN" & name == "lifeform" & 
      site_code %in% c("baddrt.de", "kiny.au", "sevi.us") ~ paste0(Taxon, "_", value),
    ## Same issue but for a fungi vs. moss at another site
    Taxon == "UNKNOWN__SP.(LCSOUTH.CL)" & name == "lifeform" & 
      site_code == "lcsouth.cl" ~ paste0(Taxon, "_", value),
    ## Conflicting lifeform IDs for various unknowns at "oklah.us"
    stringr::str_sub(Taxon, start = 1, end = 12) %in% c("UNKNOWN_SP.1", "UNKNOWN_SP.2", 
                                                        "UNKNOWN_SP.4", "UNKNOWN_SP.6") & 
      site_code == "oklah.us" & name == "lifeform" ~ paste0(Taxon, "_", value),
    ## Two unknown grasses found at kiny.au, one native and one exotic
    Taxon == "UNKNOWN_GRASS" & site_code == "kiny.au" & 
      name == "provenance" ~ paste0(Taxon, "_", value),
    ## Return non-issue taxa names as they are
    TRUE ~ Taxon)) %>%
  # Keep only unique rows (duplicates are produced by above `case_when` fixes)
  dplyr::distinct() %>%
  # Pivot back to wide format filling missing areas with NA
  tidyr::pivot_wider(names_from = name, values_from = value, values_fill = NA)

# Glimpse it
dplyr::glimpse(taxa_info)

# Read in composition data
comp_raw <- read.csv(file.path("cover_ppt_2023-01-02.csv"))

# Do some preliminary wrangling
comp <- comp_raw %>%
  # Filter to include only the *first* calendar year before treatment AND all post-treatment years
  dplyr::filter(n_treat_years >= 0) %>%
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
  dplyr::mutate(year_ct = length(unique(year))) %>%
  dplyr::ungroup() %>%
  # Trajectory analysis can't be run / doesn't make sense for sites with only one year of data (no trajectory can exist with only one point per group)
  dplyr::filter(year_ct > 1) %>%
  # Attach the functional group information
  dplyr::left_join(y = taxa_info, by = c("Taxon", "site_code")) %>%
  # Pre-emptively remove sites that caused issues for 'nature of change' analysis
  dplyr::filter(!site_code %in% c("chacra.ar", "cobar.au", "hyide.de"))

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
            # Family Analysis ----
## -------------------------------------- ##

# Wrangle composition object for this analysis
fxn_df <- comp %>%
  # Group by desired columns and summarize
  dplyr::group_by(site_code, year, trt, block_plot_subplot, Family) %>% 
  dplyr::summarize(max_cover = mean(max_cover, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Filter out any unknown families
  dplyr::filter(!is.na(Family))

# Glimpse this
dplyr::glimpse(fxn_df)
## Now we can do the analytical workflow!

# Make a list to export outputs to
out_list <- list()

# Make a vector of "bad" sites that will break the loop
bad_sites <- c(
  # Error in `RRPP::lm.rrpp`: 
  ## Error in (function (cond)  :  error in evaluating the argument 'x' in selecting a method for function 'as.matrix': subscript out of bounds
  "eea.br"
)

# Loop across sites to get trajectory analysis results
for(focal_site in setdiff(x = unique(fxn_df$site_code), y = bad_sites)){
  # for(focal_site in "allmendb.ch"){
  
  # Print starting message
  message("Processing begun for '", focal_site, "'")
  
  # Do necessary wrangling
  sub_data <- fxn_df %>%
    # Filter data to only this site
    dplyr::filter(site_code == focal_site) %>%
    # Pivot to wide format
    tidyr::pivot_wider(names_from = Family, 
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
  
  # Extract relevant summary information
  sub_metrics <- scicomptools::traj_extract(traj_mod = sub_traj, angle_type = "deg")
  
  # Finally, let's create an output object to preserve
  sub_actual <- sub_metrics %>%
    # Make a column for site
    dplyr::mutate(site_code = focal_site, .before = dplyr::everything()) %>%
    # Identify nature of change
    dplyr::mutate(change_nature = paste(significance, collapse = "__"),
                  .after = site_code)
  
  # Add this to an output list
  out_list[[focal_site]] <- sub_actual
  
  # Ending message
  message("'", focal_site, "' complete") }

## -------------------------------------- ##
              # Family Export ----
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
  # Also make a column for what response this is
  dplyr::mutate(response = "family abundance", .before = dplyr::everything())

# Glimpse output
dplyr::glimpse(out_df)

# How many change types?
out_df %>%
  dplyr::group_by(change_simp, change_nature) %>%
  dplyr::summarize(site_ct = length(unique(site_code)))

# Name response
response <- "family"

# Export locally
write.csv(x = out_df, row.names = F, na = '',
          file = file.path(paste0("traj-analysis_", response, "-results.csv")))

# Clean up environment
rm(list = setdiff(ls(), c("comp")))



# End ----