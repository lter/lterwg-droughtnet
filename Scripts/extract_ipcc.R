## ------------------------------------------------------- ##
                    # Identify IPCC Regions
## ------------------------------------------------------- ##

# Purpose:
## Strip out IPCC regions for DroughtNet Sites
## See this paper for more information:
### https://essd.copernicus.org/articles/12/2959/2020/essd-12-2959-2020.html

## -------------------------------------- ##
            # Housekeeping ----
## -------------------------------------- ##
# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, sf)

# Clear environment
rm(list = ls())

# Read in data
drought_df <- read.csv(file = file.path("Data", "Site_Elev-Disturb.csv"))

# Glimpse it
dplyr::glimpse(drought_df)

## -------------------------------------- ##
        # Prep Coordinate Object ----
## -------------------------------------- ##

# Begin by making an ultra-simple data object with **only** coordinate info
coords <- drought_df %>%
  # Keep only coordinate columns
  dplyr::select(latitud, longitud) %>%
  # Keep only one row per coordinate
  dplyr::distinct() %>%
  # Drop invalid coordinates
  dplyr::filter(latitud <= 90 & latitud >= -90) %>%
  dplyr::filter(longitud <= 180 & longitud >= -180)

# Check structure
dplyr::glimpse(coords)

# Make it "really" spatial
coords_sf <- coords %>%
  # Make it an `sf` object (i.e., "really" spatial)
  sf::st_as_sf(x = ., coords = c("latitud", "longitud")) %>%
  # Set Coordinate Reference System (CRS) manually
  sf::st_set_crs(x = ., value = 4326)

# Check structure
dplyr::glimpse(coords_sf)

## -------------------------------------- ##
        # Extract IPCC Regions ----
## -------------------------------------- ##

# Old (AR5) regions
ar5_regions <- sf::st_read(dsn = file.path("Data", "IPCC AR5 Regions", "referenceRegions.shp"))
## Link to download here: https://www.ipcc-data.org/documents/ar5/regions/referenceRegions.zip 

# Check structure
dplyr::glimpse(ar5_regions)

# Strip out old region information at coordinates
old_extract <- sf::st_intersection(x = ar5_regions, at = coords_sf) %>%
  # Drop geometry so we get just the 'data' part
  sf::st_drop_geometry()

# Check structure of that
dplyr::glimpse(old_extract)

# Updated IPCC regions

## Link to download here: https://zenodo.org/records/3998463



# End ----
