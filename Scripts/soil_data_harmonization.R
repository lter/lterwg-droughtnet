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
              subset(site_code != "")



write.csv(harmony_df, "C:/Users/ohler/Dropbox/IDE/data_processed/IDE_soil_2024-10-18.csv")
#data <- harmonize(key = NULL, raw_folder = "C:/Users/ohler/Dropbox/IDE_data_May 2018/Soil Reports/Tidy version", data_format = "xlsx")

?harmonize
