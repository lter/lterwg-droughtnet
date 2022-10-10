## ------------------------------------------------------- ##
          # Identify Traits for Plant Species
## ------------------------------------------------------- ##

# Clear environment
rm(list = ls())

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, rtry, njlyon0/helpR)

## ---------------------------------------- ##
              # Load Data ----
## ---------------------------------------- ##

# Load full TRY species list
try_spp <- rtry::rtry_import(input = "TRY_AccSpecies.txt")

# Load working group species list
wg_spp <- read.csv(file = file.path("Data", "cover_ppt_map_10-08-2022.csv"))

## ---------------------------------------- ##
              # Wrangle Data ----
## ---------------------------------------- ##

# Look at data
dplyr::glimpse(wg_spp)
dplyr::glimpse(try_spp)

# Create column of correct casing of species
wg_spp$species <- stringr::str_to_sentence(wg_spp$Taxon)

# Wrangle WG data as needed
wg_spp_actual <- wg_spp %>%
  # Keep only species column
  dplyr::select(species) %>%
  # Strip to only unique values
  unique() %>%
  # Fix double spacing issues
  dplyr::mutate(species_fix = gsub(pattern = "  ",
                                   replacement = " ",
                                   x = species)) %>%
  # And missing space for genus level IDs
  dplyr::mutate(species_fix = gsub(pattern = "sp\\.\\(|sp1\\.\\(|sp2\\.\\(|s2p\\.\\(",
                                   replacement = "sp\\. \\(",
                                   x = species_fix)) %>%
  # And other missing space type
  dplyr::mutate(species_fix = gsub(pattern = "sp\\.1|sp\\.2|sp\\.7|sp\\.g",
                                   replacement = "sp\\. X",
                                   x = species_fix)) %>%
  # Break genus & epithet apart
  tidyr::separate(col = species_fix, remove = T, sep = " ",
                  into = c("genus", "epithet", "extra"),
                  extra = "merge", fill = "right") %>%
  # Filter out unknown species and genus level IDs
  dplyr::filter(genus != "Unknown" & epithet != "sp." & 
                  nchar(epithet) > 0) %>%
  # Reassemble a simpler species column
  dplyr::mutate(species_simp = paste(genus, epithet))

# Wrangle TRY species list as needed
try_spp_actual <- try_spp %>%
  # Keep only needed columns
  dplyr::select(AccSpeciesName, AccSpeciesID) %>%
  # Split genus and species
  tidyr::separate(col = AccSpeciesName, remove = F, sep = " ",
                  into = c("genus", "epithet", "additional"),
                  extra = "merge", fill = "right") %>%
  # Reassemble a "cleaner" species name
  dplyr::mutate(AccSpeciesName_simp = paste(genus, epithet))

## ---------------------------------------- ##
   # Explore TRY vs. WG Species Overlap ----
## ---------------------------------------- ##

# Filter TRY as needed
try_spp_sub <- try_spp_actual %>%
  # Filter TRY to only species in filtered WG list
  dplyr::filter(AccSpeciesName_simp %in% wg_spp_actual$species_simp)

# Check which WG spp are not in TRY list
helpR::diff_chk(old = wg_spp_actual$species_simp,
                new = try_spp_sub$AccSpeciesName_simp)

## ---------------------------------------- ##
# Create Index of WG Species + TRY Codes ----
## ---------------------------------------- ##

# Subset TRY data to relevant species
list_actual <- try_spp_actual %>%
  # Filter to keep the species in the TRY database
  dplyr::filter(AccSpeciesName_simp %in% wg_spp_actual$species_simp) %>%
  # Reorder cols
  dplyr::select(dplyr::starts_with("AccSpeciesName"), genus, epithet, additional, AccSpeciesID) %>%
  # Keep only unique columns
  unique()

# Take a look
dplyr::glimpse(list_actual)
# view(list_actual)
list_actual$AccSpeciesID

# Export it!
write.csv(list_actual, na = "", row.names = F,
          file = "TRY_species_codes.csv")


# End ----
