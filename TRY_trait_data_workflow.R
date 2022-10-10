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

## ---------------------------------------- ##
      # Load/Wrangle TRY Trait Data ----
## ---------------------------------------- ##

# Load full TRY data (from PUBLIC ONLY query)
try_data_v0 <- rtry::rtry_import(input = file.path("Data", "TRY_full_public_data.txt"))

# Glimpse it
dplyr::glimpse(try_data_v0)
dplyr::glimpse(list_actual)

# Filter to only species IDs that we have determined we want
try_data_v1 <- try_data_v0 %>%
  dplyr::filter(AccSpeciesID %in% list_actual$AccSpeciesID) %>%
  # Subset also to only requested trait
  dplyr::filter(TraitID == 22)

# Check dim of both (should lose many rows)
dim(try_data_v1); dim(try_data_v0)

# Attempt to fix weird special characters ("multibyte strings")
try_data_v2 <- try_data_v1 %>%
  dplyr::mutate(OrigValueStr = gsub(
    pattern = "\\<|\\>",
    replacement = "",
    x = OrigValueStr))

# Now do other needed wrangling
try_data_v3 <- try_data_v2 %>%
  # Pare down to needed columns
  dplyr::select(DataName, AccSpeciesName,
                OrigValueStr, Comment) %>%
  # Unique values
  unique() %>%
  # Keep only values that *do* become NA when you coerce them to numeric
  dplyr::filter(is.na(suppressWarnings(as.numeric(OrigValueStr))) | OrigValueStr == "3" | OrigValueStr == "4") %>%
  # Now filter out bad/unknown values
  dplyr::filter(!OrigValueStr %in% c("??", "unknown")) %>%
  # Consolidate entries for photosynthesis
  dplyr::mutate(photosynthetic_mode = dplyr::case_when(
    OrigValueStr == "c3" ~ "C3",
    OrigValueStr == "3" ~ "C3", # checked in data
    OrigValueStr == "c4" ~ "C4",
    TRUE ~ OrigValueStr)) %>%
  # Pare down columns further
  dplyr::select(AccSpeciesName, photosynthetic_mode) %>%
  # Keep only unique rows
  unique()

# Check that as well
dplyr::glimpse(try_data_v3)
unique(try_data_v3$photosynthetic_mode)
# view(try_data_v3)

# Let's refine species somewhat
try_data_v4 <- try_data_v3 %>%
  # Break genus & epithet apart
  tidyr::separate(col = AccSpeciesName, remove = F, sep = " ",
                  into = c("genus", "epithet", "extra"),
                  extra = "merge", fill = "right") %>%
  # Assemble simpler binomial species column
  dplyr::mutate(AccSpeciesName_simp = paste(genus, epithet)) %>%
  # Remove old species column and pare further down
  dplyr::select(AccSpeciesName_simp, photosynthetic_mode) %>%
  unique()

# Check it out
dplyr::glimpse(try_data_v4)

# Resolve multiple conflicting answers
try_data_v5 <- try_data_v4 %>%
  # Group by species
  dplyr::group_by(AccSpeciesName_simp) %>%
  # Collapse cases with multiple entries
  dplyr::summarize(merge_photo = paste(photosynthetic_mode, collapse = "; ")) %>%
  # Manually handle these cases
  dplyr::mutate(photo_actual = dplyr::case_when(
    # Handle species without several values
    merge_photo == "C3" ~ "C3",
    merge_photo == "C4" ~ "C4",
    merge_photo == "CAM" ~ "CAM",
    merge_photo == "C3/C4" ~ "C3/C4 intermediate",
    # Handle ones without disagreement but still >1 value
    merge_photo %in% c("C3; C3?", "C3?; C3") ~ "C3",
    merge_photo %in% c("C4; C4?") ~ "C4",
    # Handle intermediate photosynthesis
    merge_photo %in% c("C3; C4; C3/C4", "C4; C3; C3/C4", "C4; C3/C4", "C3; C3/C4") ~ "C3/C4 intermediate",
    # Expert knowledge answers
    merge_photo == "C3?" ~ "C3",
    merge_photo == "C3; C4?" ~ "C3",
    # Hard checked
    merge_photo == "C3; C3/CAM" ~ "C3",
    AccSpeciesName_simp == "Agropyron cristatum" ~ "C3",
    AccSpeciesName_simp == "Agrostis capillaris" ~ "C3",
    AccSpeciesName_simp == "Avena barbata" ~ "C3",
    AccSpeciesName_simp == "Axonopus fissifolius" ~ "C4",
    AccSpeciesName_simp == "Bassia scoparia" ~ "C4",
    AccSpeciesName_simp == "Bothriochloa ischaemum" ~ "C4",
    AccSpeciesName_simp == "Bromus erectus" ~ "C3",
    AccSpeciesName_simp == "Calamagrostis canadensis" ~ "C3",
    AccSpeciesName_simp == "Chenopodium glaucum" ~ "C3/C4 intermediate",
    AccSpeciesName_simp == "Dactylis glomerata" ~ "C4",
    AccSpeciesName_simp == "Euphorbia drummondii" ~ "C3/C4 intermediate",
    AccSpeciesName_simp == "Euphorbia serpyllifolia" ~ "C3/C4 intermediate",
    AccSpeciesName_simp == "Festuca rubra" ~ "C3",
    AccSpeciesName_simp == "Lolium perenne" ~ "C3",
    AccSpeciesName_simp == "Panicum maximum" ~ "C3/C4 intermediate",
    AccSpeciesName_simp == "Paspalum dilatatum" ~ "C4",
    AccSpeciesName_simp == "Polygonum aviculare" ~ "C3",
    AccSpeciesName_simp == "Potentilla anserina" ~ "C3",
    AccSpeciesName_simp == "Salsola kali" ~ "C4",
    AccSpeciesName_simp == "Scleropogon brevifolius" ~ "C4",
    AccSpeciesName_simp == "Setaria viridis" ~ "C4",
    AccSpeciesName_simp == "Suaeda glauca" ~ "C4",
    AccSpeciesName_simp == "Trachypogon spicatus" ~ "C4",
    AccSpeciesName_simp == "Tridens flavus" ~ "C4")) %>%
  # Drop unmerged entries and rename final column
  dplyr::rename(photosynthetic_mode = photo_actual,
                species_simp = AccSpeciesName_simp) %>%
  dplyr::select(species_simp, photosynthetic_mode)
  
# Check remaining 
dplyr::glimpse(try_data_v5)
unique(try_data_v5$photosynthetic_mode)

## ---------------------------------------- ##
  # Integrate TRY Data & WG Species List ----
## ---------------------------------------- ##

# Check for differences
helpR::diff_chk(old = unique(wg_spp_actual$species_simp),
                new = unique(try_data_v5$species_simp))

# Bind with photosynthetic mode as identified above
metadata <- wg_spp_actual %>%
  dplyr::left_join(y = try_data_v5, by = "species_simp") %>%
  # Pare to minimum needed cols
  dplyr::select(species, photosynthetic_mode) %>%
  # Rename species column as originally named
  dplyr::rename(Taxon = species) %>%
  # Drop missing photosynthetic mode species
  dplyr::filter(!is.na(photosynthetic_mode))

# Glimpse it
dplyr::glimpse(metadata)

# Export it
write.csv(metadata, na = "", row.names = F,
          file = file.path("Data", "photosynth_path_metadata.csv"))

# End ----
