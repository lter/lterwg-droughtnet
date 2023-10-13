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
  sf::st_as_sf(x = ., coords = c("longitud", "latitud")) %>%
  # Set Coordinate Reference System (CRS) manually
  sf::st_set_crs(x = ., value = 4326)

# Check structure
dplyr::glimpse(coords_sf)

## -------------------------------------- ##
        # Extract IPCC Regions ----
## -------------------------------------- ##

# IMPORTANT NOTE ABOUT BELOW DATA:
## The "IPCC-WGI-reference-regions-v4_shapefile" folder is downloaded as a ZIP file
## You'll need to 'unzip' it yourself for the next line of code to run

# Read in IPCC regions
ipcc_regions <- sf::st_read(dsn = file.path("Data", 
                                            "SantanderMetGroup-ATLAS-d75f71f",
                                            "reference-regions", 
                                            "IPCC-WGI-reference-regions-v4_shapefile",
                                            "IPCC-WGI-reference-regions-v4.shp"))
## Link to download here: https://zenodo.org/records/3998463

# Glimpse it
dplyr::glimpse(ipcc_regions)

# Make an output list
out_list <- list()

# Loop across points
for(k in 1:nrow(coords_sf)){
  
  # Message start
  message("Extracting for point ", k, " of ", nrow(coords_sf))
  
  # Identify intersection point(s)
  ixn <- sf::st_intersects(x = coords_sf[k,]$geometry, y = ipcc_regions)
  
  # If multiple intersections...
  if(length(ixn[[1]]) > 1) {
    
    # Make an empty vector
    temp_lab <- NULL
    
    # Loop across intersections
    for(multi_ixn in 1:length(ixn[[1]])){
      
      # Get label at that intersection
      multi_lab <- ipcc_regions$Acronym[as.integer(ixn[[1]][multi_ixn])]
      
      # Add to vector
      temp_lab <- c(temp_lab, multi_lab)
      
      # Collapse into a one-element vector
      ipcc_lab <- paste(temp_lab, collapse = "; ") }
    
    # If only one, get just that one
  } else { ipcc_lab <- ipcc_regions$Acronym[as.integer(ixn)] }
  
  # Assemble into a dataframe and add to output list
  out_list[[k]] <- data.frame("longitud" = coords_sf[k,]$geometry[[1]][2],
                              "latitud" = coords_sf[k,]$geometry[[1]][1],
                              "ipcc_regions" = ipcc_lab)
}

# Unlist the output list
out_df <- purrr::list_rbind(x = out_list)

# Check that out
dplyr::glimpse(out_df)





# End ----
