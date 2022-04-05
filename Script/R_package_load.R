# List all packages needed
my_packages<-c("dplyr","devtools", "tidyr","data.table","nlme","ggplot2",
               "ggpubr","ggthemes","raster","rgdal", "extrafontdb","extrafont",
               "ggmap","grid","gridExtra","mapdata","maps", "GGally",
               "tidyr","tidyverse","sf","plyr","lme4","lmerTest",
               "rcompanion","MuMIn","AICcmodavg","ppcor","emmeans",
               "mctest", "segmented","nlstools","Hmisc","ggpubr")

# Check for and install required packages
for (package in my_packages) {
  if (!require(package, character.only=T, quietly=T)) {
    try(install.packages(package))
    library(package, character.only=T)
  }
}
#font_import()
#loadfonts(device = "win")
