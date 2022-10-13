## ------------------------------------------ ##
  # Drought WG - Treatment File Combination
## ------------------------------------------ ##
# Written by: Angel Chen & Nick J Lyon

# Purpose:
## Combine "..._treatments" .CSVs into a single data object

# Needed libraries
library(tidyverse)

# Clear environment
rm(list = ls())

# Grab working directory
myWD <- getwd()
myWD

# Also need to identify path to DropBox folder of .CSVs
## Set WD to root directory (tilde is shorthand for that)
setwd("~")
## Set WD to local sync of Dropbox
setwd(file.path("Dropbox", "Raw csv data"))
## Confirm that worked
getwd()

# Retrieve Raw Data --------------------------------

# Identify every file in the folder
raw_names <- dir()

# Identify only files with "treatment" in their name
relevant_names <- as.list(raw_names[stringr::str_detect(string = raw_names, pattern = "treatments")])

# Create index connecting file integers to human-readable file names
data_codes <- data.frame(filename = unlist(relevant_names)) %>%
  mutate(filecode = as.character(seq_along(filename)))

# Now use that list to read in all those csvs
trt_v1 <- relevant_names %>%
  # Read in data
  purrr::map(.f = read.csv, colClasses = "character",
             blank.lines.skip = T) %>%
  # Bind them together preserving unique columns
  dplyr::bind_rows(.id = "filecode") %>%
  # Get file names into a column
  dplyr::left_join(data_codes, by = "filecode") %>%
  # Ditch intermediary file code & any blank columns
  dplyr::select(-filecode, -starts_with('X', ignore.case = F)) %>%
  # Relocate file name before everything
  dplyr::relocate(filename, .before = everything()) %>%
  # Remove data type suffix from file name
  dplyr::mutate(filename = gsub(paste0("_", "treatments"), "", filename))

# Re-set working directory to project directory (so we export to the right place)
setwd(myWD); getwd()

# Export Data -------------------------------------------

# Create folder to save to
dir.create("Data", showWarnings = F)

# Write out "final" files
write.csv(x = trt_v1, row.names = F,
          file = file.path("Data", "drought_treatments.csv"))

# End ----
