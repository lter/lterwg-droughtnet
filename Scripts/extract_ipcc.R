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
librarian::shelf(tidyverse)

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
  dplyr::distinct()

# Check structure
dplyr::glimpse(coords)


# End ----
