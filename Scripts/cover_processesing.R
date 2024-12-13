library(plyr)
library(tidyverse)

#read n_treat_years data
IDE_treatment_years<- read.csv("C:/Users/ohler/Dropbox/IDE/data_processed/IDE_treatment_years_2024-07-18.csv")

#read in cover data survey
cover_survey <- read.csv("C:/Users/ohler/Dropbox/IDE_data_May 2018/IDE Site Info/cover_survey_results.csv")

#read in cover data
full_cover <- read.csv("C:/Users/ohler/Dropbox/IDE/data_raw/full_cover_2024-12-18.csv")%>%
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
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)
comb$ps_path <- ifelse(comb$Taxon == "", "C3", comb$ps_path)

# ACAENA ECHINATA
# 
# BLENNOSPORA DRUMMONDII
# BOECHERA HOLBOELLII
# BOERHAVIA GLABRATA
# BRACHYCOME SP.(NYNGAN.AU)
# BRACHYSCOME PERPUSILLA
# CALADENIA CARNEA
# CALADENIA TENTACULATA
# CALYTRIX TETRAGONA
# CAMISSONIOPSIS PALLIDA
# CAREX BIGELOWII
# CAREX DIGITATA
# CAREX FLAVA
# CAREX SP.(BUOYA.NO)
# CAREX SP.(HAVER.NO)
# CARPINUS CAROLINIANA
# CARYA OVATA
# Centrolepis ARISTATA
# Centrolepis POLYGYNA
# Centrolepis STRIGOSA
# CHAMAESARACHA SORDIDA
# CHAMAESCILLA CORYMBOSA
# CHEILOSORIA TENUIFOLIA
# CHENOPODIUM POLYGONOIDES
# CHENOPODIUM ROBERTIANUM
# CHONDRILLA JUNCEA
# CIRSIUM UNDULATUM
# CLADONIA SP.(BUOYA.NO)
# CLADONIA SP.(HAVER.NO)
# CLADONIA SP.1(HAVER.NO)
# CLADONIA SP.2(HAVER.NO)
# CLADONIA STELLARIS
# CONVOLVULUS SP.(JILPANGER.AU)
# CRASSULA SIEBERIANA
# CRASSULA SP.(JILPANGER.AU)
# CROTALARIA DURA
# CRYPTANTHA BARBIGERA
# CRYPTANTHA BARBIGERA
# CRYPTANTHA MINIMA
# CYMOPTERUS MONTANUS
# DAUCUS PUSILLUS
# DEYEUXIA FLAVENS
# DICHOPOGON STRICTUS
# DICRANUM SP.(BUOYA.NO)
# DICRANUM SP.(HAVER.NO)
# DICRANUM SP.1(HAVER.NO)
# DICRANUM SP.2(HAVER.NO)
# DONTOSTEMON DENTATUS
# DROSERA ABERRANS
# DROSERA GLANDULIGERA
# DROSERA HOOKERI
# ECHIUM PLANTAGINEUM
# ERIGERON OCHROLEUCUS
# EUPATORIUM COMPOSITIFOLIUM
# EUPHORBIA CURTISII
# EUPHORBIA DAVIDII
# EUPHORBIA MELANADENIA
# EUPHORBIA SP.(NYNGAN.AU)
# EUPHRASIA SP.(BUOYA.NO)
# EUPHRASIA SP.(HAVER.NO)
# FAGUS GRANDIFOLIA
# FAGUS SYLVATICA
# FRAXINUS AMERICANA
# GALIUM SP.(BUOYA.NO)
# GERANIUM SP.(BUOYA.NO)
# GERANIUM SP.(JILPANGER.AU)
# GERANIUM SP.(NYNGAN.AU)
# GEUM RIVALE
# GLYCINE SP. (llara.au)
# GNEPHOSIS DRUMMONDII
# GOODENIA BLACKIANA
# GRINDELIA SQUARROSA
# HELIANTHUS ANNUS
# HIBBERTIA SP.(JILPANGER.AU)
# HIERACIUM SP.(BUOYA.NO)
# HIERACIUM SP.1(BUOYA.NO)
# HYALOSPERMA DEMISSUM
# HYDROCOTYLE FOVEOLATA
# HYDROCOTYLE SETULOSA
# HYPERICUM SP.(BUOYA.NO)
# HYPOLAENA FASTIGIATA
# HYPOXIS ARGENTEA
# INDIGOFERA HEDYANTHA
# IPOMOEA COSTELLATA
# IPOMOEA SIMPLEX
# ISOLEPIS MARGINATA
# IVA AXILLARIS
# Lagenophora stipitata
# LAPPULA REDOWSKII
# LATHYRUS APHACA
# LEPIDOSPERMA CARPHOIDES
# LEPIDOSPERMA CONGESTUM
# LEVENHOOKIA DUBIA
# LOMANDRA COLLINA
# LOMANDRA JUNCEA
# LOMANDRA MICRANTHA
# LOMANDRA NANA
# LUZULA SP.(BUOYA.NO)
# LUZULA SP.(HAVER.NO)
# MACHAERANTHERA TANACETIFOLIA
# MATTHIOLA PARVIFLORA
# MEDICAGO SP.(NYNGAN.AU)
# MELAMPYRUM PRATENSE
# MELAMPYRUM SP.(BUOYA.NO)
# MICROSERIS SCAPIGERA
# MICROTIS ARENARIA
# MILLOTIA MUELLERI
# MILLOTIA TENUIFOLIA
# MIRABILIS LINEARIS
# MIRABILIS SP.
# OENOTHERA ALBICAULIS
# OPHIOGLOSSUM SP.(JILPANGER.AU)
# OSTRYA VIRGINIANA
# OXALIS SP.(NYNGAN.AU)
# OXYTROPIS SERICEA
# PELARGONIUM RODNEYANUM
# PENSTEMON ANGUSTIFOLIUS
# PETRORHAGIA NANTEUILII
# PHEMERANTHUS PARVIFLORUS
# PHLOX MULTIFLORA
# PICEA ABIES
# PIMELEA HUMILIS
# PINUS STROBUS
# PLANTAGO CUNNINGHAMII
# PLANTAGO MAJOR
# PODOSPERMA ANGUSTIFOLIUM
# POLYGALA AMATYMBICA
# POLYGALA POLYGAMA
# POLYGALA SP.(BUOYA.NO)
# POLYTRICHUM SP.(HAVER.NO)
# POPULUS GRANDIDENTATA
# PORTULACA PILOSA
# POTENTILLA CRANTZII
# PRUNUS SEROTINA
# PTERYXIA HENDERSONII
# PTILAGROSTIS DICHOTOMA
# PTILOTUS SP.(NYNGAN.AU)
# PYROLA MINOR
# PYROLA SP.(BUOYA.NO)
# QUERCUS ALBA
# QUERCUS RUBRA
# QUERCUS VELUTINA
# QUINETIA URVILLEI
# RACOMITRIUM SP.(BUOYA.NO)
# RACOMITRIUM SP.(HAVER.NO)
# RANUNCULUS ROBERTSONII
#RANUNCULUS SP.(NYNGAN.AU)
#RHINANTHUS SP.(BUOYA.NO)
#RHINANTHUS SP.(HAVER.NO)
#RHODANTHE MOSCHATA
#RHYTIDIADELPHUS SP.(BUOYA.NO)
#RHYTIDIADELPHUS SP.(HAVER.NO)
#ROMULEA ROSEA
#SAXIFRAGA OPPOSITIFOLIA
#SCHOENUS APOGON
#SCLEROLAENA BIRCHII
#SCLEROLAENA QUINQUECUSPIS
#SCORZONERA LACINIATA
#SEDUM LANCEOLATUM
#SELAGINELLA SELAGINOIDES
#SENECIO RHOMBOIDEUS
#SIDA CUNNINGHAMII
#SILOXERUS MULTIFLORUS
#SILPHIUM COMPOSITUM
#SINAPIS ARVENSIS
#SOLANUM ESURIALE
#SOLANUM HETERODOXUM
#SOLANUM NIGRUM
#SOLENOGYNE DOMINII
#STIPA SP. (cedarsav.us)
#STYLOCHITON NATALENSIS
#SUCCISA PRATENSIS
#SUCCISA PRATENSIS
#TETRAPTERON PALMERI
#THELYMITRA SP.(JILPANGER.AU)
#THESIUM NATALENSE
#THYSANOTUS PATERSONII
#THYSANOTUS TUBEROSUS
#TRIBULUS MICROCOCCUS
#TRICORYNE TENELLA
#TRIFOLIUM sp.(jilpanger.au)
#ULMUS AMERICANA
#UNKNOWN ASTERACEAE SP. (llara.au)
#UNKNOWN ASTERACEAE SP.(CREDOM.AU)
#UNKNOWN ASTERACEAE SP.(HARD.US)
#UNKNOWN ASTERACEAE SP.(OREAC.US)
#UNKNOWN ASTERACEAE SP.1(NYNGAN.AU)
#UNKNOWN ASTERACEAE SP.2(MILPARINKA.AU)
#UNKNOWN ASTERACEAE SP.8(NYNGAN.AU)
#UNKNOWN FABACEAE SP.(NYNGAN.AU)
#UNKNOWN GRASS SP. (llara.au)
#UNKNOWN POACEAE SP.(JILPANGER.AU)
#UNKNOWN POACEAE SP.(MATADOR.CA)
#UNKNOWN POACEAE SP.(MILPARINKA.AU)
#UNKNOWN POACEAE SP.(QUILPIE.AU)
#UNKNOWN POACEAE SP.1(QUILPIE.AU)
#UNKNOWN POACEAE SP.6(NYNGAN.AU)
#UNKNOWN POACEAE SP3.(qdtsouth.cl)
#UNKNOWN POLEMONIACEAE SP.(OREAA.US)
#UNKNOWN POLEMONIACEAE SP.(OREAC.US)
#VICIA SP.(BUOYA.NO)
#VIOLA CANINA
#VIOLA SP.(BUOYA.NO)
#VITIS ROTUNDIFOLIA
#VITTADINIA CERVICULARIS
#WAHLENBERGIA GRACILENTA
#WAHLENBERGIA SP. (llara.au)
#WAHLENBERGIA STRICTA
#WURMBEA DIOICA






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
ppt.1 <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/data/precip/anpp_clean_trt_ppt_no-perc_365-0days_2024-12-18.csv")

#reduce column names to minimum
ppt.1$ppt.1 <- ppt.1$ppt#change precip column names in lag files to reflect lags
ppt.1 <- ddply(ppt.1, c("site_code", "year", "trt"), 
        function(x)data.frame(
         ppt.1 = x$ppt[x$biomass_date %in% max(x$biomass_date)]
        ))



#read in precip data
ppt.2 <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/data/precip/anpp_clean_trt_ppt_no-perc_730-365days_2024-12-18.csv")

#reduce column names to minimum
ppt.2$ppt.2 <- ppt.2$ppt#change precip column names in lag files to reflect lags
ppt.2 <- ddply(ppt.2, c("site_code", "year", "trt"), 
        function(x)data.frame(
         ppt.2 = x$ppt[x$biomass_date %in% max(x$biomass_date)]
        ))

#read in precip data
ppt.3 <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/data/precip/anpp_clean_trt_ppt_no-perc_1095-730days_2024-12-18.csv")

#reduce column names to minimum
ppt.3$ppt.3 <- ppt.3$ppt#change precip column names in lag files to reflect lags
ppt.3 <- ddply(ppt.3, c("site_code", "year", "trt"), 
        function(x)data.frame(
         ppt.3 = x$ppt[x$biomass_date %in% max(x$biomass_date)]
        ))

#read in precip data
ppt.4 <- read.csv("C:/Users/ohler/Dropbox/IDE Meeting_Oct2019/data/precip/anpp_clean_trt_ppt_no-perc_1460-1095days_2024-12-18.csv")

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
                  left_join(site_map, by = "site_code")#%>%
                  #subset(habitat.type=="Grassland"|habitat.type=="Shrubland")



write.csv(cover_ppt_full, "C:/Users/ohler/Dropbox/IDE/data_processed/cover_ppt_2024-12-19.csv")


length(unique(subset(cover_ppt_full, n_treat_years == 1)$site_code))
length(unique(subset(cover_ppt_full, n_treat_years == 2)$site_code))
length(unique(subset(cover_ppt_full, n_treat_years == 3)$site_code))
length(unique(subset(cover_ppt_full, n_treat_years == 4)$site_code))

