## ------------------------------------------ ##
   # Drought WG - Biomass Iteration Counting
## ------------------------------------------ ##
# Written by: Angel Chen & Nick J Lyon

# Purpose:
## Combine "..._treatments" .CSVs into a single data object

# Needed libraries
library(tidyverse)

# Clear environment
rm(list = ls())

# Grab working directory
myWD <- getwd()
myWD

# Also need to identify path to DropBox folder of .CSVs
## Set WD to root directory (tilde is shorthand for that)
setwd("~")
## Set WD to local sync of Dropbox
setwd(file.path("Dropbox", "Raw csv data"))
## Confirm that worked
getwd()

# Retrieve Raw Data --------------------------------

# Identify every file in the folder
raw_names <- dir()

# Manually specify "bad" .CSVs (i.e., those with non-standard formatting or functional duplicates of other files)
unwanted_names <- c(
  # wide format and duplicate with long format variant
  "uffe-australiasites-2017-update-biomass_wide.csv",
  # Summarized through taxa (also unsummarized version found in file of same name but with "taxa" instead of "total)
  "Ethabuka-ide-2017_updated_20210914_biomass_total.csv",
  # Contained only cover data, no biomass data
  "Bayreuth_DroughtNet ide data_2017_biomass.csv",
  # Looks like a shortcut that is redundant with a file of the same name that is a CSV
  "Elva-ide-data-2015-2019_biomass - Shortcut.lnk")

# Identify only files with "biomass" in their name
relevant_names <- as.list(setdiff(raw_names[stringr::str_detect(string = raw_names, pattern = "biomass")], unwanted_names))

# Create index connecting file integers to human-readable file names
data_codes <- data.frame(filename = unlist(relevant_names)) %>%
  mutate(filecode = as.character(seq_along(filename)))

# Now use that list to read in all those csvs
bio_v0 <- relevant_names %>%
  # Read in data
  # purrr::map(.f = read.csv, colClasses = "character",
             # blank.lines.skip = T, row.names = NULL) %>%
  purrr::map(.f = readr::read_csv, col_types = cols(.default = "c")) %>%
  # Bind them together preserving unique columns
  dplyr::bind_rows(.id = "filecode") %>%
  # Get file names into a column
  dplyr::left_join(data_codes, by = "filecode") %>%
  # Ditch intermediary file code & any blank columns
  dplyr::select(-filecode, -starts_with('X', ignore.case = F)) %>%
  # Relocate file name before everything
  dplyr::relocate(filename, .before = everything()) %>%
  # Remove data type suffix from file name
  dplyr::mutate(filename = gsub(paste0("_", "treatments"), "", filename))

# Re-set working directory to project directory (so we export to the right place)
setwd(myWD); getwd()

# Handle Redundant Columns ------------------------------

# Drop columns that are completely empty
bio_v1 <- bio_v0 %>%
  dplyr::select(-dplyr::starts_with("..."), -Art, -Datum,
                -`General Note: Plots K refer to controls, plots R refer to rain-out (=drought)`)
  
# Check what was lost
setdiff(names(bio_v0), names(bio_v1)) # that's fine
 
# Now combine synonymous columns
bio_v2 <- bio_v1 %>%
  ## All manually checked to ensure that they never contain competing information
  dplyr::mutate(
    filename = filename,
    year = dplyr::coalesce(year, Year),
    date = dplyr::coalesce(date, Date, DATE),
    season = Season,
    site = dplyr::coalesce(site, Site, plot...1),
    block = dplyr::coalesce(block, Block),
    block_label = block_label,
    plot = dplyr::coalesce(plot, Plot, `Plot #`),
    plot_label = dplyr::coalesce(plot_id, plot_label, plot...2, plot...8),
    subplot = dplyr::coalesce(subplot, Subplot),
    replicate = dplyr::coalesce(`repeat`, replicate),
    # Treatment information
    treatment = dplyr::coalesce(treatment, Treatment, Drought, trat,
                                treatment_type_following_DN),
    fertilization = fertilization,
    precipitation = dplyr::coalesce(precipitation, Rainfall),
    rainout_shelter = `rainout shelter (yes/no)`,
    snow_removal = `snow removal (yes/no)`,
    # Taxon information
    functional_group_temp = dplyr::coalesce(Taxa, `taxa Group`, `functional group`,
                                            functional_type, `growth form (see taxa table)`,
                                            `date or note`, taxa_meaning, taxa_code),
    mixed_taxa_temp = taxa,
    species_temp = dplyr::coalesce(species, Species, taxa...5, taxa...6),
    # Living vs. Dead info
    live_or_dead = dplyr::coalesce(status, Type, taxa_condition),
    proportion_dead_tissue = `proportion of dead tissue`,
    # Biomass information
    ## Only columns with identical unit specification are combined
    ## Manual checking revealed this to be necessary, not a hypothetical precaution
    biomass = dplyr::coalesce(biomass, mass),
    biomass_g = vorhergesagteBiomasse2018.in.g,
    biomass_m2 = `mass per m2`,
    biomass_g_m2 = dplyr::coalesce(`Biomass (g/m^2)`, `mass (g/m2 )`, `mass (g/m2)`),
    biomass_kg_ha = `Kg/ha`,
    biomass_kg_m2 = mass_kgm2,
    biomass_live = dplyr::coalesce(alive_mass, `live mass`, mass_green,
                                   mass_live, Mass_live),
    biomass_standing = `standing biomass`,
    biomass_dead = mass_dead,
    biomass_dry = Mass_dry,
    biomass_dry_g = `dry mass [g]`,
    biomass_dry_g_m2 = dplyr::coalesce(`dry mass (g per square meter)`, `dry mass [g m-2]`),
    biomass_dry_weight = `Dry mass Weight Per Sample`,
    biomass_current_season_g = `current growth season (g)`,
    biomass_prev_season_g = `previous growth season (g)`,
    biomass_dead_prev_year = `dead_mass of previous year`,
    biomass_roots_g = `Roots (g)`,
    biomass_sums = Sums,
    biomass_total_aboveground_g = `Total aboveground mass (g)`,
    biomass_total_g_m2 = `total biomass [g m-2]`,
    # Combining the notes columns is a little more finnicky
    notes_temp = dplyr::coalesce(notes, note, note_biomass, `note on taxa`),
    notes = dplyr::case_when(
      !is.na(notes_temp) & !is.na(note_treatment) ~ paste(notes_temp,
                                                          note_treatment,
                                                          sep = "; "),
      !is.na(notes_temp) & is.na(note_treatment) ~ notes_temp,
      is.na(notes_temp) & !is.na(note_treatment) ~ note_treatment,
      TRUE ~ ' ')) %>%
  # Combine taxa columns selectively to get as few NAs as possible
  dplyr::mutate(taxa_composite = dplyr::case_when(
    # If functional group has a value, use that!
    !is.na(functional_group_temp) ~ functional_group_temp,
    # If functional group is NA but 'mixed taxa' isn't, use mixed taxa
    is.na(functional_group_temp) &
      !is.na(mixed_taxa_temp) ~ mixed_taxa_temp,
    # If both functional group & mixed taxa are NA, use species
    is.na(functional_group_temp) &
      is.na(mixed_taxa_temp) &
      !is.na(species_temp) ~ species_temp)) %>%
  # Keep only columns we just created
  dplyr::select(filename, year, date, season, site, block, block_label,
                plot, plot_label, subplot, replicate,
                treatment, fertilization, precipitation, rainout_shelter, snow_removal,
                taxa_composite, functional_group_temp, mixed_taxa_temp, species_temp, 
                live_or_dead, proportion_dead_tissue,
                biomass, dplyr::starts_with("biomass_"),
                notes) %>%
  # Return a dataframe
  as.data.frame()

# Handle Differing Biomass Metrics ----------------------

# Many different (and overlapping) styles for measuring biomass
# We need to resolve that before we continue

bio_v3 <- bio_v2 %>%
  # Rotating to long format preserves all different biomass metrics while still yielding a single value for "biomass" that we can count the number of iterations of within a given grouping structures
  tidyr::pivot_longer(cols = dplyr::starts_with("biomass"),
                      names_to = "biomass_metric",
                      values_to = "biomass_measurements") %>%
  # Drop any NAs in biomass_measurements (these are created by pivoting columns that didn't have a value for a given file)
  dplyr::filter(!is.na(biomass_measurements)) %>%
  # Return a dataframe
  as.data.frame()

# Take a look!
head(bio_v3)
str(bio_v3)

# Count Biomass Samples Per Group -----------------------

# Do it
bio_v4 <- bio_v3 %>%
  # Count biomass samples within desired groups
  dplyr::group_by(filename, site, year, date, block, plot,
                  taxa_composite, biomass_metric) %>%
  dplyr::mutate(biomass_sample_ct = dplyr::n()) %>%
  dplyr::ungroup() %>%
  # Keep only rows with more than 1 per group
  dplyr::filter(biomass_sample_ct > 1) %>%
  # Retain only filename and biomass sample count
  dplyr::select(filename, site, year, date, block, plot,
                taxa_composite, biomass_metric, biomass_sample_ct) %>%
  # And keep only unique values (i.e., one row per file)
  base::unique()

# Separately Summarize Dates by Sites -------------------

# Grab the dates for each site/file name combination
bio_dates <- bio_v3 %>%
  # Count biomass samples within desired groups
  dplyr::select(filename, site, year, date, biomass_metric) %>%
  # And keep only unique values (i.e., one row per file)
  base::unique()

# Export Data -------------------------------------------

# Check that WD is specified appropriately
setwd(myWD); getwd()

# Create folder to save to
dir.create("Data", showWarnings = F)

# Write out "final" files
write.csv(x = bio_v4, row.names = F,
          file = file.path("Data", "drought_biomass-iter-count.csv"))
write.csv(x = bio_dates, row.names = F,
          file = file.path("Data", "drought_biomass-dates.csv"))

# End ----
