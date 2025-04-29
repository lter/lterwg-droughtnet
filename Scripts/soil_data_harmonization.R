library(tidyverse)
# devtools::install_github("lter/ltertools")
library(ltertools)




test_key <- ltertools::begin_key(raw_folder = "C:/Users/ohler/Dropbox/IDE_data_May 2018/Soil Reports/Tidy version", data_format = "csv", 
                                guess_tidy = TRUE)

#write.csv(test_key, "C:/Users/ohler/Dropbox/IDE_data_May 2018/Soil Reports/ltertools_key.csv")
#


key <- read.csv("C:/Users/ohler/Dropbox/IDE_data_May 2018/Soil Reports/ltertools_key.csv")

key$dup <- duplicated(dplyr::select(key, source, tidy_name) )

harmony_df <- ltertools::harmonize(key = key, raw_folder = "C:/Users/ohler/Dropbox/IDE_data_May 2018/Soil Reports/Tidy version", 
                                   data_format = "csv", quiet = TRUE)%>%
              subset(site_code != "")%>%
                      dplyr::select(-c( "treatment", "x", "x_1", "x_2","x_3", "x_mo", "sample_id", "fert", "sample_density_g_ml", "exch_acidity_cmol_l", "total_cations_cmol_l", "acid_sat", "lab", "sample", "h", "c_org_n", "conductividad_s", "x2000_50_m", "x50_20_m", "x20_2_m", "x_2_m", "Sikora")) 



write.csv(harmony_df, "C:/Users/ohler/Dropbox/IDE/data_processed/IDE_soil_2024-12-16.csv")
#data <- harmonize(key = NULL, raw_folder = "C:/Users/ohler/Dropbox/IDE_data_May 2018/Soil Reports/Tidy version", data_format = "xlsx")

?harmonize
