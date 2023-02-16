## ------------------------------------------------------- ##
              # Family Abundance Analysis ----
## ------------------------------------------------------- ##

# Purpose:
## Perform perMANOVA and trajectory analysis on family abundance

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
  # Filter to include only the *first* calendar year before treatment AND up to 4 years after
  dplyr::filter(n_treat_years >= 0 & n_treat_years <= 4) %>%
  # Also drop the "0.5" n_treat_years (sampling after treatment but before day 365)
  dplyr::filter(n_treat_years != 0.5) %>%
  # Filter to only accepted treatments
  dplyr::filter(trt %in% c("Control", "Drought")) %>%
  # Make some new columns
  dplyr::mutate(
    ## Concatenated block-plot-subplot column
    block_plot_subplot = paste(block, plot, subplot, sep = "_"),
    ## Taxon column without spaces
    Taxon = gsub(pattern = " ", replacement = "_", x = Taxon)) %>%
  # Get average cover within year / treatment / block-plot-subplot
  dplyr::group_by(site_code, n_treat_years, n_treat_days, trt, block_plot_subplot, Taxon) %>% 
  dplyr::summarize(max_cover = mean(max_cover, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Attach the functional group information
  dplyr::left_join(y = taxa_info, by = c("Taxon", "site_code"))

# Check out what that leaves us with
glimpse(comp_v2)
  
# Now do final push of wrangling pre-analysis
comp <- comp_v2 %>%
  # Drop photosynthetic pathway
  dplyr::select(-ps_path) %>%
  # Make a site + treatment column
  dplyr::mutate(site_trt = paste0(site_code, "_", trt),
                .before = dplyr::everything())

# Check out what that yields
dplyr::glimpse(comp)

# Make sure filter steps worked as desired
unique(comp$trt)
sort(unique(comp$n_treat_years))

# Any sites lost during tidying?
setdiff(x = unique(comp_raw$site_code), y = unique(comp$site_code))

# Clean up environment
rm(list = setdiff(ls(), c("comp")))

## -------------------------------------- ##
   # perMANOVA - Triple Interaction ----
## -------------------------------------- ##

# Use this to process a neat family level dataframe
fam_df <- comp %>%
  # Group by desired columns and summarize
  dplyr::group_by(site_code, trt, n_treat_years, block_plot_subplot, Family) %>% 
  dplyr::summarize(max_cover = mean(max_cover, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Filter out any unknown families
  dplyr::filter(!is.na(Family)) %>%
  # Pivot to wide format
  tidyr::pivot_wider(names_from = Family, 
                     values_from = max_cover, 
                     values_fill = 0)
  
# Glimpse this
dplyr::glimpse(fam_df)

# Now make a matrix version of just the community composition
fam_mat <- fam_df %>%
  dplyr::select(-site_code:-block_plot_subplot) %>%
  as.matrix()

# Make the special dataframe required by the RRPP package
fam_rdf <- RRPP::rrpp.data.frame("site" = fam_df$site_code,
                                 "treatment" = fam_df$trt,
                                 "treatment_years" = fam_df$n_treat_years,
                                 "plot" = fam_df$block_plot_subplot,
                                 "community" = fam_mat)

# Fit perMANOVA model
## Note: takes a few minutes
fam_fit <- RRPP::lm.rrpp(community ~ treatment * treatment_years * site,
                         data = fam_rdf, iter = 999, RRPP = T,
                         print.progress = T)

# Check out summary of this
fam_aov <- RRPP::anova.lm.rrpp(object = fam_fit, effect.type = "F", print.progress = T)

# Strip out summary stats table
fam_aov_out <- as.data.frame(fam_aov$table) %>%
  dplyr::mutate(model_term = row.names(.), .before = dplyr::everything())

# Check that out
fam_aov_out

# Export this
write.csv(x = fam_aov_out, row.names = F, na = '',
          file = file.path("family-permanova_triple-ixn-aov.csv"))

# Clean up environment
rm(list = setdiff(ls(), c("comp")))

## -------------------------------------- ##
    # perMANOVA - Incl. Family Term ----
## -------------------------------------- ##

# Choose minimum percent threshold for families to be included
cutoff_thresh <- 0.05

# Use this to process a neat family level dataframe
fam_df <- comp %>%
  # Group by desired columns and summarize
  dplyr::group_by(site_code, trt, n_treat_years, block_plot_subplot, Family) %>% 
  dplyr::summarize(max_cover = mean(max_cover, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Filter out any unknown families
  dplyr::filter(!is.na(Family)) %>%
  # Find maximum cover per site / year
  dplyr::group_by(site_code, n_treat_years) %>%
  dplyr::mutate(maximum_cover = max(max_cover, na.rm = T)) %>%
  dplyr::ungroup()
  
  
  
  # Calculate total cover per site (across treatments & blocks)
  dplyr::group_by(site_code) %>% 
  dplyr::mutate(tot_cover = mean(max_cover, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Calculate that threshold as a percent
  dplyr::mutate(perc_cutoff = tot_cover * cutoff_thresh)

# Check this out
fam_df %>%
  filter(site_code == "allmendb.ch" & n_treat_years == 0) %>%
  view()

  
  
   %>%
  # Calculate per site percentages of each family
  dplyr::group_by(site_code, n_treat_years, Family) %>%
  dplyr::mutate(fam_perc = (max_cover / tot_cover) * 100) %>%
  dplyr::ungroup() %>%
  # Keep only rows above this percentage
  dplyr::filter(fam_perc >= perc_cutoff)

# Glimpse it
dplyr::glimpse(fam_df)

# Make the special dataframe required by the RRPP package
fam_rdf <- RRPP::rrpp.data.frame("site" = fam_df$site_code,
                                 "treatment" = fam_df$trt,
                                 "treatment_years" = fam_df$n_treat_years,
                                 "family" = fam_df$Family,
                                 "plot" = fam_df$block_plot_subplot,
                                 "cover" = fam_df$max_cover)

# Fit perMANOVA model
## Note: takes a few minutes
fam_fit <- RRPP::lm.rrpp(cover ~ treatment * treatment_years + family + site,
                         data = fam_rdf, iter = 999, RRPP = T,
                         print.progress = T)

# Check out summary of this
fam_aov <- RRPP::anova.lm.rrpp(object = fam_fit, effect.type = "F", print.progress = T)

# Strip out summary stats table
fam_aov_out <- as.data.frame(fam_aov$table) %>%
  dplyr::mutate(model_term = row.names(.), .before = dplyr::everything())

# Check that out
fam_aov_out










# Get pairwise comparisons
fam_site_pairs <- RRPP::pairwise(fit = fam_fit, groups = fam_df$site_code)
fam_trt_pairs <- RRPP::pairwise(fit = fam_fit, groups = fam_df$trt)
fam_year_pairs <- RRPP::pairwise(fit = fam_fit, groups = fam_df$n_treat_years)

# Strip relevant parts of that
fam_site_pairs_out <- as.data.frame(summary(fam_site_pairs, test.type = "dist")$summary.table) %>%
  dplyr::rename(UCL_95perc = `UCL (95%)`,
                P_Value = `Pr > d`) %>%
  dplyr::mutate(pairs = row.names(.), .before = dplyr::everything())
## For treatment pairwise comparisons too
fam_trt_pairs_out <- as.data.frame(summary(fam_trt_pairs, test.type = "dist")$summary.table) %>%
  dplyr::rename(UCL_95perc = `UCL (95%)`,
                P_Value = `Pr > d`) %>%
  dplyr::mutate(pairs = row.names(.), .before = dplyr::everything())
## And year pairwise comparisons
fam_year_pairs_out <- as.data.frame(summary(fam_year_pairs, test.type = "dist")$summary.table) %>%
  dplyr::rename(UCL_95perc = `UCL (95%)`,
                P_Value = `Pr > d`) %>%
  dplyr::mutate(pairs = row.names(.), .before = dplyr::everything())

# Glimpse these
dplyr::glimpse(fam_site_pairs_out)
dplyr::glimpse(fam_trt_pairs_out)
dplyr::glimpse(fam_year_pairs_out)

# Export all outputs
write.csv(x = fam_aov_out, row.names = F, na = '',
          file = file.path("permanova_MULTI-SITE_family-results_aov-table.csv"))
write.csv(x = fam_site_pairs_out, row.names = F, na = '',
          file = file.path("permanova_MULTI-SITE_family-results_site-pairwise.csv"))
write.csv(x = fam_trt_pairs_out, row.names = F, na = '',
          file = file.path("permanova_MULTI-SITE_family-results_trt-pairwise.csv"))
write.csv(x = fam_year_pairs_out, row.names = F, na = '',
          file = file.path("permanova_MULTI-SITE_family-results_year-pairwise.csv"))

# Clean up environment
rm(list = setdiff(ls(), c("comp")))





## -------------------------------------- ##
# Family & Function perMANOVA ----
## -------------------------------------- ##


# Get average within family / functional groups of interest
# Also need fully complete family/function information
# dplyr::filter(!is.na(Family) & !is.na(provenance) & !is.na(lifeform) & !is.na(lifespan)) %>%
  # dplyr::group_by(site_code, n_treat_years, n_treat_days, trt, block_plot_subplot,
#                 Family, provenance, lifeform, lifespan) %>%
#   dplyr::summarize(max_cover = mean(max_cover, na.rm = T)) %>%
#   dplyr::ungroup() %>%

# End ----
