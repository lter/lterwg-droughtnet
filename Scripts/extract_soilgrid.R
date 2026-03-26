
library(geodata)
library(terra)
library(dplyr)


sites <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/Site_Elev-Disturb.csv")%>%
  mutate(id = site_code, lat = latitud, lon = longitud)%>%
  dplyr::select(id, lat, lon)

# depths in cm (SoilGrids convention)
depths <- c(5, 15, 30, 60)#, 100)

sites_v <- vect(
  sites,
  geom = c("lon", "lat"),
  crs = "EPSG:4326"
)

for (d in depths) {
  soildat <- soil_world("sand", depth = d, stat = "mean")
  sand_vals <- extract(soildat, sites_v)
  
  # name column based on depth
  colname <- paste0("sand_0_", d, "cm")
  sites[[colname]] <- sand_vals[, 2]
}


layer_info <- data.frame(
  depth = c(5, 15, 30, 60, 100),
  top = c(0, 5, 15, 30, 60),
  bottom = c(5, 15, 30, 60, 100)
)

layer_info$thickness <- layer_info$bottom - layer_info$top
layer_info


weighted_soil_mean <- function(df, var_prefix, depths, thickness) {
  cols <- paste0(var_prefix, "_0_", depths, "cm")
  
  values <- as.matrix(df[, cols])
  weights <- thickness / sum(thickness)
  
  rowSums(values * matrix(weights,
                          nrow = nrow(values),
                          ncol = length(weights),
                          byrow = TRUE),
          na.rm = TRUE)
}

sites$sand_0_60cm_weighted <- weighted_soil_mean(
  df = sites,
  var_prefix = "sand",
  depths = c(5, 15, 30, 60),
  thickness = c(5, 10, 15, 30)
)


site_soil_data <- sites%>%
                  mutate(site_code = id)







# --- extract SOC by depth ---
for (d in depths) {
  soildat <- soil_world("soc", depth = d, stat = "mean")
  soc_vals <- extract(soildat, sites_v)
  
  # name column based on depth
  colname <- paste0("soc_0_", d, "cm")
  sites[[colname]] <- soc_vals[, 2]
}

# --- layer metadata (for weighting) ---
layer_info <- data.frame(
  depth  = c(5, 15, 30, 60, 100),
  top    = c(0, 5, 15, 30, 60),
  bottom = c(5, 15, 30, 60, 100)
)

layer_info$thickness <- layer_info$bottom - layer_info$top
layer_info



weighted_soil_mean <- function(df, var_prefix, depths, thickness) {
  cols <- paste0(var_prefix, "_0_", depths, "cm")
  
  values <- as.matrix(df[, cols])
  weights <- thickness / sum(thickness)
  
  rowSums(
    values * matrix(weights,
                    nrow = nrow(values),
                    ncol = length(weights),
                    byrow = TRUE),
    na.rm = TRUE
  )
}

sites$soc_0_60cm_weighted <- weighted_soil_mean(
  df = sites,
  var_prefix = "soc",
  depths = c(5, 15, 30, 60),
  thickness = c(5, 10, 15, 30)
)


write.csv(site_soil_data, "C:/Users/ohler/Dropbox/IDE/data_processed/site_sand_soc_from_soilgrid_2026-03-26.csv")
