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
  "Bayreuth_DroughtNet ide data_2017_biomass.csv")

# Identify only files with "biomass" in their name
relevant_names <- as.list(setdiff(raw_names[stringr::str_detect(string = raw_names, pattern = "biomass")], unwanted_names))

# Create index connecting file integers to human-readable file names
data_codes <- data.frame(filename = unlist(relevant_names)) %>%
  mutate(filecode = as.character(seq_along(filename)))

# Now use that list to read in all those csvs
bio_v1 <- relevant_names %>%
  # Read in data
  purrr::map(.f = read.csv, colClasses = "character",
             blank.lines.skip = T, row.names = NULL) %>%
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

# Process Biomass Data -----------------------------

# Some wrangling must be done before we can advance to our ultimate task
bio_v2 <- bio_v1 %>%
  # Drop columns that are completely empty
  dplyr::select(-L, -Datum, -Art,
                -General.Note..Plots.K.refer.to.controls..plots.R.refer.to.rain.out...drought.) %>%
  # Coalesce some duplicate columns
  dplyr::mutate(
    filename = filename,
    site = dplyr::coalesce(site, Site),
    block = dplyr::coalesce(block, Block),
    block_label = block_label,
    plot = dplyr::coalesce(plot, Plot, Plot..),
    plot_label = dplyr::coalesce(plot_label, plot_id, plot.1),
    subplot = dplyr::coalesce(subplot, Subplot),
    functional_group = dplyr::coalesce(date.or.note, functional_type, functional.group, Taxa, growth.form..see.taxa.table., taxa_meaning),
    species = dplyr::coalesce(taxa, taxa.1, Species, species),
    taxa_code = taxa_code,
    live_or_dead = dplyr::coalesce(taxa_condition, status, Type),
    # _actual = dplyr::coalesce(),
    ) %>%
  # Keep only the columns we just created
  
  # Return a dataframe
  as.data.frame()

# HERE NOW ----
# Tasks remaining:
## 1) Mine combined "functional_group" column because it includes dead/alive info
## 2) Mine combined "species" column because it includes functional groups and dead/alive info
## 3) Coalsce remaining columns (see below)




# Remaining columns to coalesce / standardize:
# Biomass
"mass"
"Sums"
"Biomass..g.m.2."
"mass.per.m2"
"biomass"
"alive_mass"
"dead_mass.of.previous.year"
"mass..g.m2."
"Kg.ha"
"dry.mass..g."
"dry.mass..g.m.2."
"total.biomass..g.m.2."
"Total.aboveground.mass..g."
"previous.growth.season..g."
"current.growth.season..g."
"Roots..g."
"standing.biomass"
"vorhergesagteBiomasse2018.in.g"
"mass_kgm2"
"mass_green"
"mass_dead"
"mass_live"
"Mass_dry"
"Mass_live"
"mass..g.m2.."
"mass..g.m2"
"live.mass"
"proportion.of.dead.tissue"


# Date
"date"
"year"
"date.1"
"Date"
"DATE"
"Season"
"Year"

# Rain
"precipitation"
"Rainfall"

# Nutrients
"fertilization"

# Treatment
"treatment"
"Treatment"
"trat"
"Drought"
"treatment_type_following_DN"


# Notes
"note_biomass"
"note.on.taxa"
"note"
"row.names"
"note_treatment"
"notes"

# Replicate
"replicate"
"repeat." # maybe?

# Count Biomass Samples Per Group -----------------------

# Do it
bio_v3 <- bio_v2 %>%
  # Count biomass samples within desired groups
  dplyr::group_by(filename, site, plot, year, functional_group) %>%
  dplyr::mutate(biomass_sample_ct = n()) %>%
  dplyr::ungroup() %>%
  # Keep only rows with more than 1 per group
  dplyr::filter(biomass_sample_ct > 1) %>%
  # Retain only filename and biomass sample count
  dplyr::select(filename, biomass_sample_ct) %>%
  # And keep only unique values (i.e., one row per file)
  base::unique()

# Export Data -------------------------------------------

# Check that WD is specified appropriately
setwd(myWD); getwd()

# Create folder to save to
dir.create("Data", showWarnings = F)

# Write out "final" files
write.csv(x = bio_v3, row.names = F,
          file = file.path("Data", "drought_biomass-iter-count.csv"))

# End ----
