## ------------------------------------------------------- ##
                    # NMDS Exploration
## ------------------------------------------------------- ##

# Clear environment
rm(list = ls())

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, vegan, njlyon0/helpR)

## ---------------------------------------- ##
              # Housekeeping ----
## ---------------------------------------- ##

# Load working group species list
wg_data <- read.csv(file = file.path("Data", "cover_ppt_map_10-08-2022.csv"))

# Wrangle data
wg_simp <- wg_data %>%
  # Pare down to needed columns
  dplyr::select(site_code, block, plot, subplot, trt, n_treat_years, Taxon, max_cover) %>%
  # Generate a simpler grouping column
  dplyr::mutate(nms_group = paste(trt, n_treat_years, "_", block, plot, subplot, sep = "_"), .after = site_code) %>%
  # Remove columns that have been concatenated into one
  dplyr::select(site_code, nms_group, Taxon, max_cover) %>%
  # DELETE ME (vvv)
  # Group by everything
  dplyr::group_by(site_code, nms_group, Taxon) %>%
  # Average through "duplicated" taxa
  dplyr::summarize(max_cover = mean(max_cover, na.rm = T)) %>%
  # DELETE ME (^^^)
  # Pivot wider
  tidyr::pivot_wider(names_from = Taxon,
                     values_from = max_cover)
  

# Check it out
dplyr::glimpse(wg_simp[1:10])

# Make an exporting folder
dir.create("NMS_Exploration", showWarnings = F)

## ---------------------------------------- ##
              # NMS Creation ----
## ---------------------------------------- ##

# Create an ordination for every site code
for(place in unique(wg_simp$site_code)){
  
  # Subset the data
  
  
  # Run multidimensional scaling
  
  
  # Create (& export NMS)
  
  
  
  
}




mds <- vegan::metaMDS(wg_simp[-c(1:2)], autotransform = F,
                      expand = F, k = 2, try = 100)









# End ----

