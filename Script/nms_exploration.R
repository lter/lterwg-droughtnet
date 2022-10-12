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
  # Simplify treatments (slightly)
  dplyr::mutate(trt = dplyr::case_when(
    trt == "Control_Infrastructure" ~ "Control",
    TRUE ~ trt)) %>%
  # Filter out unwanted plot types
  dplyr::filter(trt %in% c("Control", "Drought")) %>%
  # Filter out missing 'n_treat_years' rows
  dplyr::filter(!is.na(n_treat_years)) %>%
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
  dplyr::ungroup()
  
# Check it out
dplyr::glimpse(wg_simp)
unique(wg_simp$nms_group)

# Make an exporting folder
dir.create("NMS_Exploration", showWarnings = F)

## ---------------------------------------- ##
              # NMS Creation ----
## ---------------------------------------- ##

# Create an ordination for every site code
# for(place in "hard.us"){
# for(place in unique(wg_simp$site_code)){
for(place in setdiff(unique(wg_simp$site_code),
                     # Exclude sites that error out
                     c("indiana.us", "kranz.de"))){
  
  # Subset the data
  wg_sub <- wg_simp %>%
    # Subset to place
    dplyr::filter(site_code == place) %>%
    # Pivot wider
    tidyr::pivot_wider(names_from = Taxon,
                       values_from = max_cover,
                       values_fill = 0) %>%
    # Drop site code column
    dplyr::select(-site_code, -combo_group)
  
  # Run multidimensional scaling
  mds_obj <- vegan::metaMDS(wg_sub[-1], distance = "bray",
                            autotransform = F, expand = F,
                            k = 2, try = 100)
  
  # Create color palette / line types
  col_length <- length(unique(wg_sub$nms_group)) / 2
  ide_colors <- c(rep("mediumseagreen", times = col_length),
                  rep("rosybrown4",times = col_length))
  ide_lines <- c(rep(1, times = col_length),
                 rep(2, times = col_length))
  
  # Create (& export NMS)
  tiff(file = file.path("NMS_Exploration",
                        paste0(place, "_NMS.tiff")),
       width = 850, height = 850, units = "px", pointsize = 20)
  helpR::nms_ord(mod = mds_obj,
                 groupcol = wg_sub$nms_group,
                 shapes = rep(x = 21:25, times = 4),
                 colors = ide_colors,
                 lines = ide_lines,
                 title = place, leg_pos = 'topright')
  dev.off()
  
  # Message completion
  message("NMS created for site '", place, "'") }

# End ----
