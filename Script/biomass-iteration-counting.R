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

# Handle Redundant Columns ------------------------------

# Some wrangling must be done before we can advance to our ultimate task
bio_v2 <- bio_v1 %>%
  # Drop columns that are completely empty
  dplyr::select(-Datum, -Art,
                -General.Note..Plots.K.refer.to.controls..plots.R.refer.to.rain.out...drought.) %>%
  # Coalesce some duplicate columns
  ## All manually checked to ensure that they never contain competing information
  dplyr::mutate(
    filename = filename,
    site = dplyr::coalesce(site, Site),
    site_label = row.names,
    block = dplyr::coalesce(block, Block),
    block_label = block_label,
    plot = dplyr::coalesce(plot, Plot, Plot..),
    plot_label = dplyr::coalesce(plot_label, plot_id, plot.1),
    subplot = dplyr::coalesce(subplot, Subplot),
    mixed_taxa_col = dplyr::coalesce(date.or.note, functional_type, functional.group, Taxa, growth.form..see.taxa.table., taxa_meaning, taxa.Group),
    mixed_spp_col = dplyr::coalesce(taxa, taxa.1, Species, species),
    taxa_code = taxa_code,
    replicate = dplyr::coalesce(replicate, repeat.),
    treatment = dplyr::coalesce(treatment, Treatment, trat,
                                Drought, treatment_type_following_DN),
    fertilization = fertilization,
    precipitation = dplyr::coalesce(precipitation, Rainfall),
    year = dplyr::coalesce(year, Year),
    date = dplyr::coalesce(date, Date, DATE),
    season = Season,
    # Many variants on mass column
    biomass_gm2 = dplyr::coalesce(mass, biomass, Biomass..g.m.2., mass.per.m2,
                                  mass..g.m2., mass..g.m2..,
                                  vorhergesagteBiomasse2018.in.g),
    biomass_kgha = Kg.ha,
    biomass_kgm2 = mass_kgm2,
    summed_biomass_gm2 = dplyr::coalesce(total.biomass..g.m.2., Sums,
                                         Total.aboveground.mass..g.),
    biomass_living = dplyr::coalesce(alive_mass, mass_green, mass_live,
                                     Mass_live, live.mass),
    biomass_dead = mass_dead,
    biomass_dry = Mass_dry,
    biomass_dry_g = dry.mass..g.,
    biomass_dry_gm2 = dplyr::coalesce(dry.mass..g.m.2.,
                                      dry.mass..g.per.square.meter.),
    biomass_dry_per_sample = Dry.mass.Weight.Per.Sample,
    biomass_roots_g = Roots..g.,
    biomass_standing = standing.biomass,
    biomass_dead_prev_year = dead_mass.of.previous.year,
    biomass_prev_season = previous.growth.season..g.,
    biomass_current_season = current.growth.season..g.,
    proportion_dead_tissue = proportion.of.dead.tissue,
    live_or_dead = dplyr::coalesce(taxa_condition, status, Type),
    # Combining the notes columns is a little more finnicky
    notes_temp = dplyr::coalesce(notes, note, note_biomass, note.on.taxa),
    notes = dplyr::case_when(
      !is.na(notes_temp) & !is.na(note_treatment) ~ paste(notes_temp,
                                                          note_treatment,
                                                          sep = "; "),
      !is.na(notes_temp) & is.na(note_treatment) ~ notes_temp,
      is.na(notes_temp) & !is.na(note_treatment) ~ note_treatment,
      TRUE ~ ' ') ) %>%
  # Keep only the columns we just created
  dplyr::select(filename, site, site_label, block, block_label,
    plot, plot_label, subplot, mixed_taxa_col, mixed_spp_col, taxa_code,
    replicate, treatment, fertilization, precipitation,
    year, date, season,
    # Many variants on mass column
    biomass_gm2, biomass_kgha, biomass_kgm2,
    summed_biomass_gm2, biomass_living, biomass_dead,
    biomass_dry, biomass_dry_g, biomass_dry_gm2, biomass_dry_per_sample,
    biomass_roots_g, biomass_standing, biomass_dead_prev_year,
    biomass_prev_season, biomass_current_season, proportion_dead_tissue,
    live_or_dead, notes) %>%
  # Return a dataframe
  as.data.frame()

# Wrangle the Taxa / Species Columns -----------------------

# The coalesced columns for taxa & species are useful because we have fewer columns to contend with now but have extremly mixed uses among files.
# They need to be mined to separate functional group, live vs. dead, and species


bio_v3 <- bio_v2 %>%
  dplyr::mutate(
    # Assemble true functional group column
    functional_group = dplyr::case_when(
      !mixed_taxa_col %in% c("Dead", "Dry material", "litter") ~ mixed_taxa_col,
      stringr::str_detect(string = mixed_spp_col,
                          pattern = "Unknown tree") ~ mixed_spp_col,
      stringr::str_detect(string = mixed_spp_col,
                          pattern = "unknown_forb") ~ mixed_spp_col,
      mixed_spp_col %in% c("above ground", "ANPP","dead forb", "Dead forb",
                           "Dead Graminoids", "Dead Graminoids ", "dead grass",
                           "dead herb-forb-woody", "Dead Wood", "exotic_forb",
                           "exotic_grass", "forb", "Forb", "FORB", "forb_dead",
                           "forb_live", "forbs", "Forbs",
                           "forbs and annual grasses", "Graminoid", "GRAMINOID",
                           "Graminoid: grass and Carex", "Graminoids",
                           "Graminoids ", "grass", "Grass", "Grass ",
                           "Grass and graminoid", "Grass Dead", "Grass Live",
                           "grass_dead", "grass_live", "Grasses", "Green", 
                           "Herb", "Herb Dead", "Herb Live", "herbs", "legme",
                           "legume", "Legume", "LEGUME", "legume_live", 
                           "legumes", "Legumes", "litter", "Litter", "LITTER",
                           "Litter ", "Live forb", "live grass", 
                           "Live grass", "live herb-forb", "live woody", 
                           "mix", "Mix", "Mix ", "Mixed", "Mixed plant",
                           "native_forb", "native_grass", "Non-leguminous forb",
                           "non-leguminous forbs", "Non-leguminous forbs",
                           "others", "short grasses", "shrub", "Shrub", "SHRUB",
                           "Shrub ", "Shrub Dead", "Shrub leaves", "Shrub Live",
                           "Shrubs", "shrubs and trees", "Tree", "Wood", "woody",
                           "Woody", "WOODY", "woody plants", "woody_dead") ~ mixed_spp_col),
    # Now true species column
    species = dplyr::case_when(
      !mixed_spp_col %in% c("above ground", "ANPP","dead forb", "Dead forb",
                            "Dead Graminoids", "Dead Graminoids ", "dead grass",
                            "dead herb-forb-woody", "Dead Wood", "exotic_forb",
                            "exotic_grass", "forb", "Forb", "FORB", "forb_dead",
                            "forb_live", "forbs", "Forbs",
                            "forbs and annual grasses", "Graminoid", "GRAMINOID",
                            "Graminoid: grass and Carex", "Graminoids",
                            "Graminoids ", "grass", "Grass", "Grass ",
                            "Grass and graminoid", "Grass Dead", "Grass Live",
                            "grass_dead", "grass_live", "Grasses", "Green", 
                            "Herb", "Herb Dead", "Herb Live", "herbs", "legme",
                            "legume", "Legume", "LEGUME", "legume_live", 
                            "legumes", "Legumes", "litter", "Litter", "LITTER",
                            "Litter ", "LIVE", "Live forb", "live grass", 
                            "Live grass", "live herb-forb", "live woody", 
                            "mix", "Mix", "Mix ", "Mixed", "Mixed plant",
                            "native_forb", "native_grass", "Non-leguminous forb",
                            "non-leguminous forbs", "Non-leguminous forbs",
                            "others", "short grasses", "shrub", "Shrub", "SHRUB",
                            "Shrub ", "Shrub Dead", "Shrub leaves", "Shrub Live",
                            "Shrubs", "shrubs and trees", "Tree", "Wood", "woody",
                            "Woody", "WOODY", "woody plants", "woody_dead")  ~ mixed_spp_col),
    live_or_dead = dplyr::case_when(
      is.na(live_or_dead) &
        mixed_taxa_col %in% c("Dead", "dead grass", "dead herb-forb-woody",
                              "litter", "live grass",
                              "live herb-forb", "live woody") ~ mixed_taxa_col,
      is.na(live_or_dead) & stringr::str_detect(string = mixed_spp_col,
                          pattern = "dead") ~ mixed_spp_col,
      is.na(live_or_dead) & stringr::str_detect(string = mixed_spp_col,
                          pattern = "Dead") ~ mixed_spp_col,
      is.na(live_or_dead) & stringr::str_detect(string = mixed_spp_col,
                          pattern = "(green)") ~ mixed_spp_col,
      is.na(live_or_dead) &
        mixed_spp_col %in% c("dead forb", "Dead forb", "Dead Graminoids",
                             "Dead Graminoids ", "dead grass",
                             "dead herb-forb-woody", "Dead Wood", "forb_dead",
                             "forb_live", "Grass Dead", "Grass Live",
                             "grass_dead", "grass_live", "Green", 
                             "Herb Dead", "Herb Live", "legume_live",
                             "litter", "Litter", "LITTER", "Litter ",
                             "LIVE", "Live forb", "live grass", 
                             "Live grass", "live herb-forb", "live woody", 
                             "Shrub Dead", "Shrub Live", "woody_dead") ~ mixed_spp_col,
      TRUE ~ live_or_dead)) %>%
  # And remove the 'mixed' columns now that they've been mined
  dplyr::select(-mixed_taxa_col, -mixed_spp_col)

# Check to see if that improved things
## Live vs. Dead
plyr::count(is.na(bio_v2$live_or_dead))
plyr::count(is.na(bio_v3$live_or_dead)) # fewer NAs!
## Functional group
sort(unique(bio_v3$functional_group))
## Species
sort(unique(bio_v3$species))
### Looks much cleaner!

# Handle Differing Biomass Metrics ----------------------

# Many different (and overlapping) styles for measuring biomass
# We need to resolve that before we continue

bio_v4 <- bio_v3 %>%
  # Rotating to long format preserves all different biomass metrics while still yielding a single value for "biomass" that we can count the number of iterations of within a given grouping structures
  tidyr::pivot_longer(cols = dplyr::contains("biomass"),
                      names_to = "biomass_metrics",
                      values_to = "biomass_measurements") %>%
  # Drop any NAs in biomass_measurements (these are created by pivoting columns that didn't have a value for a given file)
  dplyr::filter(!is.na(biomass_measurements)) %>%
  # Return a dataframe
  as.data.frame()

# Take a look!
head(bio_v4)
str(bio_v4)

# Count Biomass Samples Per Group -----------------------

# Do it
bio_v5 <- bio_v4 %>%
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
