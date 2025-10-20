library(plyr)
library(tidyverse)

#read n_treat_years data
IDE_treatment_years<- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/IDE_treatment_years_2025-10-20.csv")

#read in cover data survey
cover_survey <- read.csv("C:/Users/ohler/Dropbox/IDE_data_May 2018/IDE Site Info/cover_survey_results.csv")

#read in cover data
full_cover <- read.csv("C:/Users/ohler/Dropbox/IDE/data_raw/full_cover_2025-10-20.csv")%>%
              subset(live == 1)
full_cover$trt <- plyr::revalue(full_cover$trt, c("Control_Infrastructure"="Control"))

site_map <- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/Site_Elev-Disturb.csv")%>%
  dplyr::select(site_code, precip, habitat.type)%>%
  dplyr::rename(map = precip)

full_cover_v2 <- full_cover %>%
  # pivot longer to check duplicate values
 pivot_longer(cols = c(local_provenance, local_lifeform, local_lifespan, functional_group, N.fixer, ps_path), 
        names_to = "traits", values_to = "trait_values") %>%
  # consolidating trait values into fewer categories
  dplyr::mutate(trait_values = case_when(
    nchar(trait_values) == 0 | is.na(trait_values) ~ "NULL",
#    trait_values %in% c("ANNUAL GRASS","Grass") ~ "GRASS",
    trait_values %in% c("Forb","NON-LEGUMINOUS FORB", "PERENNIAL FORB") ~ "FORB",
    trait_values %in% c("Graminoid", "SEDGE") ~ "GRAMINOID",
    trait_values == "HERBS" ~ "HERB",
    #trait_values == "INDETERMINATE" ~ "NULL", # INDETERMINATE is a valid category
    trait_values == "Introduced" ~ "INT",
    trait_values == "LEGUME FORB" ~ "LEGUME",
    trait_values %in% c("n","Native", "Native?") ~ "NAT",
#    trait_values %in% c("Shrubs", "SHURB") ~ "SHRUB", #should be fixed now
    trait_values == "SUB-SHRUB" ~ "SUBSHRUB", #should be fixed now
    trait_values %in% c("unknown", "UNKNOWN") ~ "UNK",
    trait_values == "C3-C4 Intermediate" ~ "C3-C4 INTERMEDIATE",
    # fixing an error that someone made when entering the data
    Taxon == "UNKNOWN SP.6(OKLAH.US)" & traits == "functional_group" ~ "GRASS",
    TRUE ~ trait_values)) %>%
  # more fine-tuning
  dplyr::mutate(trait_values = case_when(
    Taxon %in% c("CUSCUTA PLANIFLORA") & traits == "local_lifeform" ~ "FORB",
    Taxon %in% c("PSORALIDIUM TENUIFLORUM") & traits == "local_lifeform" ~ "FORB",
    Taxon  == "UNKNOWN SP.6(OKLAH.US)" & traits == "local_lifeform" ~ "FORB",
    Taxon %in% c("MIMOSA NUTTALLII", "DALEA CANDIDA") & traits == "local_lifeform" ~ "LEGUME",
    Taxon %in% c("BRYOPHYTE SP.(LYGRAINT.NO)") & traits == "local_lifeform" ~ "BRYOPHYTE",
    trait_values == "HERB" & Family == "Fabaceae" ~ "LEGUME",
    trait_values == "HERB" & Family == "Poaceae" ~ "GRASS",
    trait_values == "HERB" & Family %in% c("Juncaceae", "Cyperaceae") ~ "GRAMINOID",
    trait_values == "HERB" & !Family %in% c("Fabaceae", "Poaceae", "Juncaceae","Cyperaceae") ~ "FORB",
    TRUE ~ trait_values)) %>%
  # dropping duplicates
  unique() %>%
  group_by( site_code, block, plot, subplot, year,  first_treatment_date, cover_date, n_treat_days, trt, Family, Taxon, live, max_cover, traits) %>%
  # counting how many different trait_values there are 
  dplyr::mutate(obs_count = n()) %>%
  ungroup() %>%
  # if trait_values is UNKNOWN or NULL and if obs_count is greater than 1, we drop that row
  dplyr::mutate(drop_me = ifelse(test = trait_values %in% c("UNKNOWN", "UNK", "INT", "NULL") & obs_count > 1,
         yes = "drop me",
         no = "keep")) %>%
  # filter to all the rows we want to keep
  dplyr::filter(drop_me == "keep") %>%
  # we don't need obs_count and drop_me anymore
  dplyr::select(-obs_count, -drop_me) %>%
  
  ###
#  dplyr::select(-trait_values)%>%
##  group_by(site_name, site_code, block, plot, subplot,              
#           year, first_treatment_year, first_treatment_date, cover_date, n_treat_days,         
#           n_treat_years, trt, Family, Taxon, live, max_cover, traits)%>%
#  filter(n() > 1)
 #x <- duplicated(full_cover_v2)
  ###
  
  subset(Taxon != "ALLIUM POLYRHIZUM")%>% ###A temporary fix to a problem at urat.cn that Ingrid should solve
  # put it back to wide format
  pivot_wider(names_from = "traits", values_from = "trait_values") %>%
  # reorder columns
  dplyr::select(site_code, block, plot, subplot,              
        year, first_treatment_date, cover_date, n_treat_days,         
          trt, Family, Taxon, live,           
         local_provenance, local_lifeform, local_lifespan, functional_group,     
         N.fixer, ps_path, max_cover)

# check to see it looks ok
glimpse(full_cover_v2)

full_cover_v2 <- full_cover_v2%>%
  ddply(.(site_code, year, trt,  block, plot, subplot, first_treatment_date, Family, Taxon, live, local_provenance, local_lifeform, local_lifespan, functional_group, N.fixer, ps_path), function(x)data.frame(
    #this is where to enter the max cover date, but it would require a lubridate function first to convert character to date            cover_date = 
    n_treat_days = max(x$n_treat_days),
    max_cover = max(x$max_cover)
  ))




#####################################################################
##############GAP FILL VARIOUS TRAIT DATA
NutNet_funcgroup_C3C4 <- read.csv("C:/Users/ohler/Dropbox/IDE/data_raw/NutNet_cover_C3C4.csv")%>%
  dplyr::select(Taxon, functional_group, ps_path)%>%
  dplyr::rename(c(functional_group_nutnet = functional_group, ps_path_nutnet = ps_path))


######C3/C4
unique(full_cover_v2$ps_path)
length(subset(full_cover_v2, ps_path == "NULL")$ps_path) #36246 need ps path


comb <- full_cover_v2%>%
  left_join(NutNet_funcgroup_C3C4, by = "Taxon")

comb$ps_path <- ifelse(comb$ps_path == "NULL" , comb$ps_path_nutnet, comb$ps_path) #| comb$ps_path == "NA"
comb$ps_path <- ifelse(comb$Taxon == "PHYLLOSTACHYS EDULIS", "C3", comb$ps_path)


length(dplyr::filter(comb, is.na(ps_path))$ps_path) #27087 need ps path

#Gap fill missing PS path ad hoc

comb$ps_path <- ifelse(comb$Taxon == "ACER RUBRUM", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "ACER SACCHARUM", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "AGROSTIS sp.", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "AJUGA PYRAMIDALIS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "ALCHEMILLA ALPINA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "ALCHEMILLA SP.(BUOYA.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "ALLIONIA CHOISYI", "C4", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "ANEMONE NEMOROSA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "ANTENNARIA MEDIA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "ANTENNARIA PARVIFLORA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "APHELIA PUMILIO", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "ARCTOUS ALPINA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "ARCTOUS ALPINA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "ARGYROLOBIUM MOLLE", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "ARGYROLOBIUM STIPULACEUM", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "ARISTIDA SP.", "C4", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "ASCLEPIAS GIBBA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "ASTRAGALUS BISULCATUS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "ASTRAGALUS MOLLISSIMUS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "ASTRAGALUS SHORTIANUS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "ASTROLOMA HUMIFUSUM", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "ATRIPLEX SP.(MILPARINKA.AU)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "AUSTROSTIPA SP.(JILPANGER.AU)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "BETULA ALLEGHANIENSIS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "BETULA PAPYRIFERA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "BETULA PUBESCENS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "BLENNOSPORA DRUMMONDII", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "BOECHERA HOLBOELLII", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "BOERHAVIA GLABRATA", "C4", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "BRACHYCOME SP.(NYNGAN.AU)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "BRACHYSCOME PERPUSILLA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CALYTRIX TETRAGONA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CONVOLVULUS SP.(JILPANGER.AU)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CRASSULA SIEBERIANA", "CAM", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CRASSULA SP.(JILPANGER.AU)", "CAM", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "DROSERA ABERRANS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "DROSERA GLANDULIGERA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "DROSERA HOOKERI", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "GERANIUM SP.(BUOYA.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "GERANIUM SP.(JILPANGER.AU)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "GERANIUM SP.(NYNGAN.AU)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "GLYCINE SP. (llara.au)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "GOODENIA BLACKIANA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "HIBBERTIA SP.(JILPANGER.AU)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "HYALOSPERMA DEMISSUM", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "HYALOSPERMA DEMISSUM", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "LEPIDOSPERMA CARPHOIDES", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "LEPIDOSPERMA CONGESTUM", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "LOMANDRA COLLINA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "LOMANDRA JUNCEA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "LOMANDRA MICRANTHA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "LOMANDRA NANA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "MEDICAGO SP.(NYNGAN.AU)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "OXALIS SP.(NYNGAN.AU)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "PIMELEA HUMILIS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "PLANTAGO CUNNINGHAMII", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "PLANTAGO MAJOR", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "PODOSPERMA ANGUSTIFOLIUM", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "PTILOTUS SP.(NYNGAN.AU)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "RANUNCULUS ROBERTSONII", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "RANUNCULUS SP.(NYNGAN.AU)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "SCHOENUS APOGON", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "SCLEROLAENA BIRCHII", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "SCLEROLAENA QUINQUECUSPIS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "SIDA CUNNINGHAMII", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "SOLANUM ESURIALE", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "SOLANUM HETERODOXUM", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "SOLANUM NIGRUM", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "THYSANOTUS PATERSONII", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "THYSANOTUS TUBEROSUS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "TRIBULUS MICROCOCCUS", "C4", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "TRIFOLIUM sp.(jilpanger.au)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "VITTADINIA CERVICULARIS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "WAHLENBERGIA GRACILENTA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "WAHLENBERGIA SP. (llara.au)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "WAHLENBERGIA STRICTA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "WURMBEA DIOICA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "FAGUS GRANDIFOLIA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "FAGUS SYLVATICA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "FRAXINUS AMERICANA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "PICEA ABIES", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "PINUS STROBUS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "POPULUS GRANDIDENTATA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "PRUNUS SEROTINA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "QUERCUS ALBA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "QUERCUS RUBRA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "QUERCUS VELUTINA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "ULMUS AMERICANA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "UNKNOWN FABACEAE SP.(NYNGAN.AU)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "UNKNOWN POLEMONIACEAE SP.(OREAA.US)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "UNKNOWN POLEMONIACEAE SP.(OREAC.US)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CALADENIA CARNEA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CALADENIA TENTACULATA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CAMISSONIOPSIS PALLIDA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CAREX BIGELOWII", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CAREX DIGITATA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CAREX FLAVA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CAREX SP.(BUOYA.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CAREX SP.(HAVER.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CARPINUS CAROLINIANA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CARYA OVATA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "Centrolepis ARISTATA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "Centrolepis POLYGYNA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "Centrolepis STRIGOSA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CHAMAESARACHA SORDIDA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CHAMAESCILLA CORYMBOSA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CHEILOSORIA TENUIFOLIA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CHENOPODIUM POLYGONOIDES", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CHENOPODIUM ROBERTIANUM", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CHONDRILLA JUNCEA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CIRSIUM UNDULATUM", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CROTALARIA DURA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CRYPTANTHA BARBIGERA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CRYPTANTHA BARBIGERA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CRYPTANTHA MINIMA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "CYMOPTERUS MONTANUS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "DAUCUS PUSILLUS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "DEYEUXIA FLAVENS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "DICHOPOGON STRICTUS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "DONTOSTEMON DENTATUS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "ECHIUM PLANTAGINEUM", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "ERIGERON OCHROLEUCUS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "EUPATORIUM COMPOSITIFOLIUM", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "GEUM RIVALE", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "GNEPHOSIS DRUMMONDII", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "GRINDELIA SQUARROSA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "HELIANTHUS ANNUS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "HYDROCOTYLE FOVEOLATA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "HYDROCOTYLE SETULOSA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "HYPOLAENA FASTIGIATA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "HYPOXIS ARGENTEA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "INDIGOFERA HEDYANTHA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "IPOMOEA COSTELLATA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "IPOMOEA SIMPLEX", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "ISOLEPIS MARGINATA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "IVA AXILLARIS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "Lagenophora stipitata", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "LAPPULA REDOWSKII", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "LATHYRUS APHACA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "LEVENHOOKIA DUBIA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "MACHAERANTHERA TANACETIFOLIA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "MATTHIOLA PARVIFLORA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "MELAMPYRUM PRATENSE", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "MELAMPYRUM SP.(BUOYA.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "MICROSERIS SCAPIGERA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "MICROTIS ARENARIA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "MILLOTIA MUELLERI", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "MILLOTIA TENUIFOLIA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "MIRABILIS LINEARIS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "MIRABILIS SP.", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "OENOTHERA ALBICAULIS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "OSTRYA VIRGINIANA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "OXYTROPIS SERICEA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "PELARGONIUM RODNEYANUM", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "PENSTEMON ANGUSTIFOLIUS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "PETRORHAGIA NANTEUILII", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "PHEMERANTHUS PARVIFLORUS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "PORTULACA PILOSA", "C4", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "PHLOX MULTIFLORA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "POLYGALA AMATYMBICA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "POLYGALA POLYGAMA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "POLYGALA SP.(BUOYA.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "POTENTILLA CRANTZII", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "PTERYXIA HENDERSONII", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "PTILAGROSTIS DICHOTOMA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "PYROLA MINOR", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "PYROLA SP.(BUOYA.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "RHODANTHE MOSCHATA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "ROMULEA ROSEA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "SAXIFRAGA OPPOSITIFOLIA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "SCORZONERA LACINIATA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "SEDUM LANCEOLATUM", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "SELAGINELLA SELAGINOIDES", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "SENECIO RHOMBOIDEUS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "SILOXERUS MULTIFLORUS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "SILPHIUM COMPOSITUM", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "SINAPIS ARVENSIS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "DICRANUM SP.(BUOYA.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "DICRANUM SP.(HAVER.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "DICRANUM SP.1(HAVER.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "DICRANUM SP.2(HAVER.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "GALIUM SP.(BUOYA.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "HIERACIUM SP.(BUOYA.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "HIERACIUM SP.1(BUOYA.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "HYPERICUM SP.(BUOYA.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "LUZULA SP.(BUOYA.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "LUZULA SP.(HAVER.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "POLYTRICHUM SP.(HAVER.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "RACOMITRIUM SP.(BUOYA.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "RACOMITRIUM SP.(HAVER.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "RHINANTHUS SP.(BUOYA.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "RHINANTHUS SP.(HAVER.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "RHYTIDIADELPHUS SP.(BUOYA.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "RHYTIDIADELPHUS SP.(HAVER.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "VICIA SP.(BUOYA.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "VIOLA CANINA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "VIOLA SP.(BUOYA.NO)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "SUCCISA PRATENSIS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "SOLENOGYNE DOMINII", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "STIPA SP. (cedarsav.us)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "STYLOCHITON NATALENSIS", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "TETRAPTERON PALMERI", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "THELYMITRA SP.(JILPANGER.AU)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "THESIUM NATALENSE", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "TRICORYNE TENELLA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "VITIS ROTUNDIFOLIA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "QUINETIA URVILLEI", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "ACAENA ECHINATA", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "OPHIOGLOSSUM SP.(JILPANGER.AU)", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "EUPHORBIA CURTISII", "C4", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "EUPHORBIA DAVIDII", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "EUPHORBIA MELANADENIA", "C4", comb$ps_path)

#




#####FUNCTIONAL GROUP 
unique(comb$functional_group)
length(subset(comb, functional_group == "NULL" | functional_group == "UNK")$functional_group) #151

#this section doesn't help anything. The IDE functional group column is quite good and only misses species that are unknowns/genus level or are not in this nutnet sheet
#comb$functional_group_nutnet <- replace_na(comb$functional_group_nutnet, replace= "NULL")
#comb$functional_group_revised <- ifelse(comb$functional_group == "NULL" | comb$functional_group == "UNK", #comb$functional_group_nutnet, comb$functional_group)
#length(subset(comb, functional_group_revised == "NULL")$functional_group_revised) #cut down on 150 Nulls

comb$functional_group <- ifelse(comb$functional_group != "NULL", comb$functional_group,
                                 ifelse(comb$Family == "Poaceae" | comb$Family == "Juncaceae" | comb$Family == "Cyperaceae", "GRAMINOID",
                                ifelse(comb$Family == "Fabaceae", "LEGUME",
                               ifelse(comb$Family == "Cactaceae", "CACTUS",
                              ifelse(comb$Family == "Oxalidaceae" | comb$Family == "Compositae" | comb$Family == "Polygonaceae" | comb$Family == "Malvaceae" | comb$Family == "Lamiaceae" | comb$Family == "Apiaceae" | comb$Family == "Convolvulaceae", "FORB",
                             ifelse(comb$Family == "Cistaceae", "WOODY",
                                    "NULL"))))))


length(subset(comb, functional_group == "NULL")$functional_group) #cut down on ~75 unknowns/NULLS




######N FIXER
unique(comb$N.fixer)
length(subset(comb, N.fixer == "NULL")$N.fixer) #22618 NULLS

comb$N.fixer <- ifelse(comb$N.fixer != "NULL", comb$N.fixer,
                ifelse(comb$Family == "Fabaceae", "1",
                               "0"))

length(subset(comb, N.fixer == "NULL")$N.fixer) #0 NULLS


comb <- dplyr::select(comb, -c("functional_group_nutnet", "ps_path_nutnet"))


####Gap fill from Meghan A's list

avolio.traits <- read.csv("C:/Users/ohler/Dropbox/IDE/data_raw/missingtraitdata_filled.csv")%>%
  dplyr::rename(local_lifeform.avolio = local_lifeform, local_lifespan.avolio = local_lifespan, ps_path.avolio = ps_path, N.fixer.avolio = N.fixer)


temp <- comb%>%
        left_join(avolio.traits, by = c("site_code","Family","Taxon"))

length(subset(temp, is.na(temp$ps_path)==TRUE))
length(subset(temp, is.na(temp$ps_path)==TRUE))

temp$local_lifeform <- ifelse(temp$local_lifeform == "NULL" | temp$local_lifeform == "NA", temp$local_lifeform.avolio, temp$local_lifeform)
temp$local_lifespan <- ifelse(temp$local_lifespan == "NULL" | temp$local_lifespan == "NA", temp$local_lifespan.avolio, temp$local_lifespan)
temp$ps_path <- ifelse(temp$ps_path == "NULL" | temp$ps_path == "NA", temp$ps_path.avolio, temp$ps_path)
temp$N.fixer <- ifelse(temp$N.fixer == "NULL" | temp$N.fixer == "NA", temp$N.fixer.avolio, temp$N.fixer)

comb <- temp%>%
  dplyr::select(-c("local_lifeform.avolio", "local_lifespan.avolio", "ps_path.avolio", "N.fixer.avolio"))


#treatment_info <- read.csv("C:/Users/ohler/Downloads/full_biomass_test.csv")
#treatment_info <- treatment_info[, c("site_code", "year", "n_treat_days", "block", "plot", "subplot")]
#treatment_info <- unique(treatment_info)

#full_biomass <- read.csv("C:/Users/ohler/Dropbox/IDE MS_Single year extreme/Data/full_biomass_test.csv")
#details <- full_biomass[, c("site_code", block, plot, subplot, year, )]

#read in precip data
ppt.1 <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/data/precip/anpp_clean_trt_ppt_no-perc_365-0days_2025-10-20.csv")

#reduce column names to minimum
ppt.1$ppt.1 <- ppt.1$ppt#change precip column names in lag files to reflect lags
ppt.1 <- ddply(ppt.1, c("site_code", "year", "trt"), 
        function(x)data.frame(
         ppt.1 = x$ppt[x$biomass_date %in% max(x$biomass_date)]
        ))



#read in precip data
ppt.2 <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/data/precip/anpp_clean_trt_ppt_no-perc_730-365days_2025-10-20.csv")

#reduce column names to minimum
ppt.2$ppt.2 <- ppt.2$ppt#change precip column names in lag files to reflect lags
ppt.2 <- ddply(ppt.2, c("site_code", "year", "trt"), 
        function(x)data.frame(
         ppt.2 = x$ppt[x$biomass_date %in% max(x$biomass_date)]
        ))

#read in precip data
ppt.3 <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/data/precip/anpp_clean_trt_ppt_no-perc_1095-730days_2025-10-20.csv")

#reduce column names to minimum
ppt.3$ppt.3 <- ppt.3$ppt#change precip column names in lag files to reflect lags
ppt.3 <- ddply(ppt.3, c("site_code", "year", "trt"), 
        function(x)data.frame(
         ppt.3 = x$ppt[x$biomass_date %in% max(x$biomass_date)]
        ))

#read in precip data
ppt.4 <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/data/precip/anpp_clean_trt_ppt_no-perc_1460-1095days_2025-10-20.csv")

#reduce column names to minimum
ppt.4$ppt.4 <- ppt.4$ppt#change precip column names in lag files to reflect lags
ppt.4 <- ddply(ppt.4, c("site_code", "year", "trt"), 
        function(x)data.frame(
         ppt.4 = x$ppt[x$biomass_date %in% max(x$biomass_date)]
        ))

#merge all the precip-lag years
full_ppt <- left_join(ppt.1, ppt.2, by = c("site_code", "year", "trt"))%>%
 unique()%>%
 left_join(ppt.3, by = c("site_code", "year", "trt"))%>%
 unique()%>%
 left_join(ppt.4, by = c("site_code", "year", "trt"))%>%
 unique()


cover_ppt <- left_join(comb, full_ppt, by = c("site_code", "year", "trt"))%>%
 subset(live == 1)


#read worldclim data
#worldclim <- read.csv("worldclim_map.csv")

#cover_ppt_map <- merge(cover_ppt, worldclim, by = "site_code", all.x = TRUE)


#specify n_trt_years with n_treat_days
#cover_ppt <- cover_ppt[-c(12)]#remove old column which isn't trustworthy
cover_ppt$n_treat_days <- as.numeric(cover_ppt$n_treat_days)

#temp <- cover_ppt[,c("site_code", "n_treat_days", "year")]
#temp <- unique(temp)

#cover_ppt$n_treat_years <- ifelse(cover_ppt$n_treat_days > 50 & cover_ppt$n_treat_days < 415, "1", "NA")
#years <- unique(cover_ppt[,c("site_code", "year", "n_treat_years")])
#years$n_treat_years2 <- 

#cover_ppt <- cover_ppt%>%
#  mutate(n_treat_years = case_when(
#    n_treat_days <= 50 ~ 0,
#    n_treat_days > 50 & n_treat_days < 415 ~ 1,
#    n_treat_days >= 415 & n_treat_days < 780 ~ 2,
#    n_treat_days >= 780 & n_treat_days < 1145 ~ 3,
#    n_treat_days >= 1145 & n_treat_days < 1510 ~ 4,
#    n_treat_days >= 1510 & n_treat_days < 1875 ~ 5,
#    n_treat_days >= 1875 & n_treat_days < 2240 ~ 6
#  ))

#temp <- cover_ppt_map[, c("site_code", "n_treat_years")]
#temp <- unique(temp)

#hist(subset(temp, n_treat_years != 0)$n_treat_years)


cover_ppt_full <- left_join(cover_ppt, IDE_treatment_years, by = c("site_code", "year"))%>%
                    subset(trt == "Control"| trt == "Drought")%>%
                  left_join(cover_survey, by = "site_code")%>%
                  left_join(site_map, by = "site_code")%>%
  unite(rep_year, c("site_code", "year", "block", "plot", "subplot"), remove = FALSE)%>%
  subset(rep_year != "bayrdrt.de_2021_1_1_A")%>%#recovery data from bayreuath
  subset(rep_year != "bayrdrt.de_2021_1_6_A")%>%
  subset(rep_year != "bayrdrt.de_2021_2_2_A")%>%
  subset(rep_year != "bayrdrt.de_2021_2_7_A")%>%
  subset(rep_year != "bayrdrt.de_2021_3_3_A")%>%
  subset(rep_year != "bayrdrt.de_2021_3_8_A")%>%
  subset(rep_year != "bayrdrt.de_2021_4_4_A")%>%
  subset(rep_year != "bayrdrt.de_2021_4_9_A")%>%
  subset(rep_year != "bayrdrt.de_2021_5_10_A")%>%
  subset(rep_year != "bayrdrt.de_2021_5_5_A")%>%
  subset(rep_year != "bayrdrt.de_2022_1_1_A")%>%
  subset(rep_year != "bayrdrt.de_2022_1_6_A")%>%
  subset(rep_year != "bayrdrt.de_2022_2_2_A")%>%
  subset(rep_year != "bayrdrt.de_2022_2_7_A")%>%
  subset(rep_year != "bayrdrt.de_2022_3_3_A")%>%
  subset(rep_year != "bayrdrt.de_2022_3_8_A")%>%
  subset(rep_year != "bayrdrt.de_2022_4_4_A")%>%
  subset(rep_year != "bayrdrt.de_2022_4_9_A")%>%
  subset(rep_year != "bayrdrt.de_2022_5_10_A")%>%
  subset(rep_year != "bayrdrt.de_2022_5_5_A")%>%
  subset(select=-c(rep_year))


write.csv(cover_ppt_full, "C:/Users/ohler/Dropbox/IDE/data_processed/cover_ppt_2025-10-20.csv")


length(unique(subset(cover_ppt_full, n_treat_years == 1)$site_code))
length(unique(subset(cover_ppt_full, n_treat_years == 2)$site_code))
length(unique(subset(cover_ppt_full, n_treat_years == 3)$site_code))
length(unique(subset(cover_ppt_full, n_treat_years == 4)$site_code))

