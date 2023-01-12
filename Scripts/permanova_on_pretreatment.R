## ------------------------------------------------------- ##
#      PERMANOVA ON PRE-TREATMENT YEAR IN COVER DATA
## ------------------------------------------------------- ##

# Clear environment
rm(list = ls())

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, RRPP)

# Name our export folder
export_folder <- paste0("export_permanova_", Sys.Date())

# Make our export folder
dir.create(path = file.path(export_folder), showWarnings = FALSE)

# Create empty list to store our statistics in later
anova_list <- list() 

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


# Make a list of "bad" sites that will break the loop
bad_sites <- c("")

# For the rest of the sites that work, we...
for (a_site in setdiff(x = unique(comp_pre_treatment$site_code), y = bad_sites)){
  message("Processing begun for site: ", a_site)
  
  comp <- comp_pre_treatment %>%
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
    mod_fit <- RRPP::lm.rrpp(community ~ treatment,
                             data = comp_rdf, # data = RRPP 'dataframe'
                             iter = 999, # 999 permutations
                             RRPP = TRUE # permute residuals
    )
    
    # Get the summary values (with F statistics)
    anova_for_site <- anova(mod_fit, effect.type = "F")
    anova_list[[a_site]] <- anova_for_site
  }
}

# Extracting the anova statistics from each site and mapping every summary table into one dataframe
anova_df <- anova_list %>%
  purrr::map(.f = 1) %>%
  purrr::map(.f = ~as.data.frame(.)) %>%
  purrr::map(.f = ~rownames_to_column(.,"variable")) %>%
  purrr::imap(.f = ~mutate(.x, site = paste0(.y), .before = everything())) %>%
  purrr::map_dfr(.f = select, everything())

# Exporting the anova summary statistics csv
write.csv(anova_df, file = file.path(export_folder, "anova_summary.csv"), row.names = FALSE)
