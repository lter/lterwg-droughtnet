## ------------------------------------------------------- ##
# Trait Information Tidying
## ------------------------------------------------------- ##

# Purpose:
## The trait table per species is in a state of flux as gaps are filled and entries are edited
## We need a consistent tidy version of that information across all separate function scripts
## This script does that tidying and assumes you've read in the CSV as "taxa_raw" BEFORE `source`ing this script

## -------------------------------------- ##
          # Trait Wrangling ----
## -------------------------------------- ##
# Read in trait information (and wrangle slightly)
taxa_info <- taxa_raw %>%
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
  tidyr::pivot_wider(names_from = name, values_from = value, values_fill = NA) %>%
  # Standardize entries
  ## Provenance (Native vs. Introduced vs. Unknown)
  dplyr::mutate(provenance = dplyr::case_when(
    provenance %in% c("NAT", "n", "Native", "Naturalised", "Native?", "native") ~ "NAT",
    provenance %in% c("INT", "Introduced", "introduced") ~ "INT",
    provenance %in% c("UNK",  "unknown") ~ "UNK",
    is.na(provenance) ~ "UNK"),
    ## Lifeform (growth form)
    lifeform = dplyr::case_when(
      lifeform %in% c("Forb", "forb/herb", "Forb/herb", "FORB/HERB",
                      "NON-LEGUMINOUS FORB", "PERENNIAL FORB") ~ "FORB",
      lifeform %in% c("ANNUAL GRASS", "Grass") ~ "GRASS",
      lifeform %in% c("Graminoid") ~ "GRAMINOID",
      lifeform %in% c("herb", "Herb", "HERB ", "HERB?", "HERBS") ~ "HERB",
      lifeform %in% c("LEGUME FORB") ~ "LEGUME",
      lifeform %in% c("SEDGE") ~ "GRAMINOID",
      lifeform %in% c("SPIKE MOSS", "CLUBMOSS") ~ "MOSS",
      lifeform %in% c("Shrub", "Shrubs", "SHUB", "SHURB",
                      "CREEPING TUNDRA GROUND SHRUB") ~ "SHRUB",
      lifeform %in% c("Shrublet", "SHRUBLET", "SUB-SHRUB",
                      "DWARF SHRUB") ~ "SUBSHRUB",
      lifeform %in% c("TREE/SHRUB", "Woody", "WOOD") ~ "WOODY",
      lifeform %in% c("Vine") ~ "VINE",
      ### Also need to remove some problem characters when these become column names
      lifeform == "CORM/BULB" ~ "CORM_BULB",
      lifeform == "FORB/VINE" ~ "FORB_VINE",
      lifeform == "HERB/VINE" ~ "HERB_VINE",
      TRUE ~ lifeform),
    ## Fix problem characters when these become column names
    ps_path = dplyr::case_when(
      ps_path == "C3-C4 Intermediate" ~ "C3_C4_Intermediate",
      TRUE ~ ps_path))

# End ----

