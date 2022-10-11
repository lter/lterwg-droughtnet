## ------------------------------------------------------- ##
                    # NMDS Exploration
## ------------------------------------------------------- ##

# Clear environment
rm(list = ls())

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, vegan, njlyon0/helpR, lter/lterpalettefinder)

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
  dplyr::mutate(nms_group = paste(trt, n_treat_years, sep = "_"),
                combo_group = paste(trt, n_treat_years, "_", block, plot, subplot, sep = "_"), .after = site_code) %>%
  # Remove columns that have been concatenated into one
  dplyr::select(site_code, nms_group, combo_group, Taxon, max_cover) %>%
  # DELETE ME (vvv)
  # Group by everything
  dplyr::group_by(site_code, nms_group, combo_group, Taxon) %>%
  # Average through "duplicated" taxa
  dplyr::summarize(max_cover = mean(max_cover, na.rm = T)) %>%
  # DELETE ME (^^^)
  # Ungroup
  dplyr::ungroup() %>%
  # Pivot wider
  tidyr::pivot_wider(names_from = Taxon,
                     values_from = max_cover,
                     values_fill = 0)
  

# Check it out
dplyr::glimpse(wg_simp[1:10])

# Make an exporting folder
dir.create("NMS_Exploration", showWarnings = F)

## ---------------------------------------- ##
              # NMS Creation ----
## ---------------------------------------- ##

# Create an ordination for every site code
for(place in unique(wg_simp$site_code)){
# for(place in "hard.us"){
  
  # Subset the data
  wg_sub <- wg_simp %>%
    dplyr::filter(site_code == place) %>%
    # Drop site code column
    dplyr::select(-site_code, -combo_group)
  
  # Run multidimensional scaling
  mds_obj <- vegan::metaMDS(wg_sub[-1], autotransform = F,
                        expand = F, k = 2, try = 100)
  
  # Grab vector of colors
  lter_palt <- lterpalettefinder::palette_find(name = "mushroom tree")
  
  # Crop it to needed length
  lter_colors <- lterpalettefinder::palette_subsample(palette = lter_palt, wanted = length(unique(wg_sub$nms_group)))
  
  # Create (& export NMS)
  tiff(file = file.path("NMS_Exploration",
                        paste0(place, "_NMS.tiff")),
       width = 850, height = 850, units = "px", pointsize = 20)
  helpR::nms_ord(mod = mds_obj,
                 groupcol = wg_sub$nms_group,
                 shapes = rep(x = 21:25, times = 4),
                 lines = rep(x = 1, times = 40),
                 colors = lter_colors,
                 title = place, leg_pos = 'topright')
  dev.off()
  
  # Message completion
  message("NMS created for site '", place, "'")
  
}

# End ----

