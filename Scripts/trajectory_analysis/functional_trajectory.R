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
librarian::shelf(tidyverse, RRPP)

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
  dplyr::filter(n_treat_days >= -365 & n_treat_years >= 0) %>%
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
  dplyr::left_join(y = taxa_info, by = c("Taxon", "site_code"))

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
# Function Trajectory Analysis ----
## -------------------------------------- ##

# End ----