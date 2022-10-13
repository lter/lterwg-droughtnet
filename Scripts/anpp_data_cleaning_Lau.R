############################
#### ANPP Data Cleaning ####
####### DroughtNet #########
###### Lau Gherardi ########
############################
#Source R package load
source("C:\\Users\\Kwilks86\\Dropbox\\IDE MS_Single year extreme\\Code\\R_package_load.R")

#Choose file to get working directory/file name
#file.choose()

# Read in full biomass file from SQL database
full_biomass<-read.csv("C:\\Users\\Kwilks86\\Dropbox\\IDE MS_Single year extreme\\Data\\Full_Biomass-SurveyResults_03-28-2022.csv")
str(full_biomass)

#list in which cleaned ANPP datafiles are added to
ANPP_done <- list()

# Site-specific code: this code calculates ANPP for each site-plot-year according to the instructions provided by PIs
# anpp includes WOODY ANPP, UNLESS otherwise specified

# (1) Allmend_Brachy -------------------------------------------------------
aallmend_brachy<-full_biomass%>%
  dplyr::filter(site_code =="allmendb.ch",mass_category %in% c("ANPP_NON-WOODY"))%>%
  dplyr::group_by(site_code,block,plot,subplot,year)%>%
  dplyr::summarize(mass=sum(mass)) %>%
  dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")

#add to clean list
ANPP_done[["aallmend_brachy"]] <-aallmend_brachy
# remove intermediate products
rm(aallmend_brachy)

# (2) Allmend_Ovina --------------------------------------------------
aallmend_ovina<-full_biomass%>%
  dplyr::filter(site_code=="allmendo.ch",mass_category %in% c("ANPP_NON-WOODY"))%>%
  dplyr::group_by(site_code,block,plot,subplot,year)%>%
  dplyr::summarize(mass=sum(mass)) %>%
  dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
  as_tibble()

#add to clean list
ANPP_done[["aallmend_ovina"]] <-aallmend_ovina

# remove intermediate products
rm(aallmend_ovina)

# (3) Ayora (re-estimated biomass for woody plants to make it anpp) ----------------------------------
ayora<-full_biomass%>%
  dplyr::filter(site_code=="ayora.es",mass_category=="ANPP_WOODY+NON-WOODY")%>%
  dplyr::group_by(site_code,block,plot,subplot,year)%>%
  dplyr::summarize(mass=sum(mass)) %>%
  dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
  as_tibble()

#add to clean list
ANPP_done[["ayora"]] <-ayora

# remove intermediate products
rm(ayora)

# (4) Bad_Drought ----------------------------------------------
#Also, has two sampling dates that should be added together, but only use the last date
bad_drought<- full_biomass %>%
  dplyr::filter(site_code=="baddrt.de", mass_category  %in% c("FORB", "WOODY", "GRAMINOID","LEGUME")) %>% 
  dplyr::group_by(site_code,block,plot,subplot,year) %>%
  dplyr::summarize(mass=sum(mass)) %>%
  dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY") %>%
  as_tibble()
#add to clean list
ANPP_done[["bad_drought"]] <-bad_drought

# remove intermediate products
rm(bad_drought)

# (5) Bayreuth -------------------------------------------------------
#NOTE<-"anpp includes WOODY. instructed to sum both harvest values by PI, year listed is from final harvest year"

bayreuth<-full_biomass%>%
  dplyr::filter(site_code=="bayrdrt.de",mass_category %in% c("FORB","GRAMINOID","LEGUME","WOODY","GRASS")) %>% 
  dplyr::group_by(site_code,block,plot,subplot,year)%>%
  dplyr::summarize(mass=sum(mass)) %>%
  dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY") %>%
  as_tibble()
  
#add to clean list
  ANPP_done[["bayreuth"]] <-bayreuth
  
# remove intermediate products
  rm(bayreuth)

# (6) Biddulph ------------------------------------------------------
biddulph<-full_biomass%>%
    dplyr::filter(site_code=="biddulph.ca",mass_category %in% c("ANPP_NON-WOODY","ANPP_WOODY+NON-WOODY")) %>% 
    dplyr::group_by(site_code,block,plot,subplot,year)%>%
    dplyr::summarize(mass=sum(mass)) %>%
    dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY") %>%
    as_tibble()

#add to clean list
  ANPP_done[["biddulph"]] <-biddulph

# remove intermediate products
rm(biddulph)

# (7) Brookdale ------------------------------------------------
#NOTE<- "anpp includes WOODY, sum  Forbs ,  Graminoids ,  Shrub ,  Shrub leaves ,  Dead Current"
brookdale<-full_biomass%>%
  dplyr::filter(site_code=="brookdale.ca",mass_category %in% c("FORB","GRAMINOID","WOODY","DEAD CURRENT"))%>%
  dplyr::group_by(site_code,block,plot,subplot,year)%>% 
  dplyr::summarize(mass=sum(mass))%>%
  dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
  as_tibble()

  ANPP_done[["brookdale"]] <-brookdale
  
  # remove intermediate products
  rm(brookdale)

#(8) Cedar Creek Savanna -----------------------------------------------
CedarSav<-full_biomass%>%
  dplyr::filter(site_code== "cedarsav.us",mass_category %in% c("FORB","GRAMINOID","SHRUB","WOODY","LEGUME"))%>%
  dplyr::group_by(site_code,block,plot,subplot,year)%>% 
  dplyr::summarize(mass=sum(mass))%>%
  dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
  as_tibble()

ANPP_done[["cedarsav"]] <-CedarSav

# remove intermediate products
rm(CedarSav)

## DOUBLE CHECK MASS CATEGORIES TO SUM
# (9) Cedar Creek Trait-----------------------------------------------
Cedartrait<-full_biomass%>%
  dplyr::filter(site_code== "cedartrait.us",mass_category %in% c("FORB","GRAMINOID","SHRUB","WOODY","LEGUME"))%>%
  dplyr::group_by(site_code,block,plot,subplot,year)%>% 
  dplyr::summarize(mass=sum(mass))%>%
  dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
  as_tibble()

ANPP_done[["cedartrait"]] <-Cedartrait

# remove intermediate products
rm(Cedartrait)

# (10) Chacra -------------------------------------------------------------
NOTE<-"sum all taxa, they do not report litter or standing dead or categories to exclude. Here taxa are grouped into functional type according to the SQL database"
chacra<-full_biomass%>%
  dplyr::filter(site_code== "chacra.ar")%>%
  dplyr::group_by(site_code,block,plot,subplot,year)%>% 
  dplyr::summarize(mass=sum(mass))%>%
  dplyr::mutate(mass_category="anpp",note_biomass="only grass category in full_biomass")%>%
  as_tibble()

ANPP_done[["chacra"]] <-chacra

# remove intermediate products
rm(chacra,NOTE)


# (11) Charleville -------------------------------------------------------------
NOTE<-"anpp includes WOODY"
charleville<-full_biomass%>%
  dplyr::filter(site_code== "charleville.au",mass_category %in% c("FORB","GRASS","WOODY","LEGUME"))%>%
  dplyr::group_by(site_code,block,plot,subplot,year)%>% 
  dplyr::summarize(mass=sum(mass))%>%
  dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
  as_tibble()

ANPP_done[["charleville"]] <-charleville

# remove intermediate products
rm(charleville,NOTE)

# (12) Ciempozuelos -------------------------------------------------------
ciempozuelos<-full_biomass%>%
  dplyr::filter(site_code== "ciempoz.es",mass_category %in% c("FORB","GRASS","WOODY","LEGUME"))%>%
  dplyr::group_by(site_code,block,plot,subplot,year)%>% 
  dplyr::summarize(mass=sum(mass))%>%
  dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY. Only WOODY and LITTER categories in full_biomass")%>%
  as_tibble()

ANPP_done[["ciempozuelos"]] <-ciempozuelos

# remove intermediate products
rm(ciempozuelos)

# (13) casper_mountain-----------------------------------------------
# Told by PI to remove WOODY and CACTI
casper_mountain<-full_biomass%>%
  dplyr::filter(site_code== "cmss.us",mass_category %in% c("FORB","GRAMINOID"))%>% 
  dplyr::group_by(site_code,block,plot,subplot,year)%>% 
  dplyr::summarize(mass=sum(mass))%>%
  dplyr::mutate(mass_category="anpp",note_biomass="ANPP does not include WOODY")%>%
  as_tibble()

ANPP_done[["casper_mountain"]] <-casper_mountain

# remove intermediate products
rm(casper_mountain)

# (14) Cobar -----------------------------------------------Need to figure our what mass_categories to sum for anpp: GRASS and GRAMINOID categories???####
cobar<-full_biomass%>%
  dplyr::filter(site_code== "cobar.au",mass_category %in% c("FORB","GRAMINOID","WOODY","GRASS", "LEGUME"))%>%
  dplyr::group_by(site_code,block,plot,subplot,year)%>% 
  dplyr::summarize(mass=sum(mass))%>%
  dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
  as_tibble()

ANPP_done[["cobar"]] <-cobar

# remove intermediate products
rm(cobar)

# (15) Cowichan --------------------------------------------
#Only remove Litter for this site####
cowichan<-full_biomass%>%
  dplyr::filter(site_code== "cowidrt.ca",mass_category!=c("LITTER"))%>%
  dplyr::group_by(site_code,block,plot,subplot,year)%>% 
  dplyr::summarize(mass=sum(mass))%>%
  dplyr::mutate(mass_category="anpp",note_biomass="anpp does include WOODY")%>%
  as_tibble()

ANPP_done[["cowichan"]] <-cowichan

# remove intermediate products
rm(cowichan)

# (16) EEA: Using total to estimate ANPP but not confirmed ####
eea<-full_biomass%>%
  dplyr::filter(site_code== "eea.br",mass_category == "ANPP_NON-WOODY")%>% #Total is ANPP (as confirmed by survey)
  dplyr::group_by(site_code,block,plot,subplot,year)%>% 
  dplyr::summarize(mass=sum(mass))%>%
  dplyr::mutate(mass_category="anpp",note_biomass="Total biomass is anpp")%>%
  as_tibble()

ANPP_done[["eea"]] <-eea

# remove intermediate products
rm(eea)

# (17) ESW --------------------------------------------------
esw<-full_biomass%>%
  dplyr::filter(site_code== "esw.ca",mass_category != "LITTER")%>%
  dplyr::group_by(site_code,block,plot,subplot,year)%>% 
  dplyr::summarize(mass=sum(mass))%>%
  dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY")%>%
  as_tibble()

ANPP_done[["esw"]] <-esw

# remove intermediate products
rm(esw)

# (18) Falls_Creek ---------------------------------------------------
falls_creek<-full_biomass%>%
  dplyr::filter(site_code== "falls.au",mass_category != "LITTER")%>%
  dplyr::group_by(site_code,block,plot,subplot,year)%>%
  dplyr::summarize(mass=sum(mass))%>%
  dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
  as_tibble()

  ANPP_done[["falls_creek"]] <-falls_creek
  
  # remove intermediate products
  rm(falls_creek)

# (19) Guaribas ----------------------------------------------------------------
  Guaribas<-full_biomass%>%
    dplyr::filter(site_code== "guaribas.br",mass_category %in% c("FORB","GRAMINOID","LEGUME","GRASS","WOODY"))%>% 
    dplyr::group_by(site_code,block,plot,subplot,year)%>%
    dplyr::summarize(mass=sum(mass)) %>%
    dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
    as_tibble()
  
  ANPP_done[["Guaribas"]] <-Guaribas
  
  # remove intermediate products
  rm(Guaribas)
  
# (20) Hardware_Ranch ------------------------------------------------
hardware_ranch<-full_biomass%>%
  dplyr::filter(site_code== "hard.us",mass_category %in% c("GRAMINOID","WOODY","FORB","LEGUME"))%>%
  dplyr::group_by(site_code,block,plot,subplot,year)%>%
    dplyr::summarize(mass=sum(mass)) %>%
  dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
  as_tibble()

ANPP_done[["hardware_ranch"]] <-hardware_ranch

# remove intermediate products
  rm(hardware_ranch)

# (21) Jena ----------------------------------------------------------
#mass should be summed by plot/subplot
jena<-full_biomass%>%
dplyr::filter(site_code== "jenadrt.de",mass_category != "LITTER")%>%
dplyr::group_by(site_code,block,plot,subplot,year) %>%
dplyr::summarize(mass=sum(mass)) %>%
dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY") %>%
as_tibble()

#add to clean list
ANPP_done[["jena"]] <-jena
# remove intermediate products
rm(jena)


# (22) Jilpanger ----------------------------------------------------------
jilpanger<-full_biomass%>%
  dplyr::filter(site_code== "jilpanger.au",mass_category == "NON-WOODY")%>%
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
  dplyr::summarize(mass=sum(mass)) %>%
      dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY. This category was excluded because not all plots have WOODY and the ones that do have 15 times the biomass of the rest")%>%
      as_tibble()
    
    #add to clean list
    ANPP_done[["jilpanger"]] <-jilpanger
    
    # remove intermediate products
    rm(jilpanger)
    
    
# (23) Jornada --------------------------------------------------
    jornada<-full_biomass%>%
      dplyr::filter(site_code== "jorndrt.us",mass_category %in% c("FORB","GRAMINOID","LEGUME"))%>% 
      dplyr::group_by(site_code, block, plot, subplot,year)  %>%
      dplyr::summarize(mass=(sum(mass)))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
      as_tibble()

    #add to clean list
    ANPP_done[["jornada"]] <-jornada

    # remove intermediate products
    rm(jornada)
    
# (24) Kernen_burned ------------------------------------------------------
    kernen_burned<-full_biomass%>%
      dplyr::filter(site_code== "kernb.ca",mass_category %in% c("GRAMINOID","FORB","WOODY","LEGUME"))%>%
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass)) %>%
      dplyr::mutate(mass_category="anpp",note_biomass="anpp")%>%
      as_tibble()
    
    ANPP_done[["kernen_burned"]] <-kernen_burned
    
    # remove intermediate products
    rm(kernen_burned)

# (25) Kernen_Unburned ----------------------------------------------------
    kernen_unburned<-full_biomass%>%
      dplyr::filter(site_code== "kernnu.ca", mass_category %in% c("FORB","GRAMINOID","LEGUME","WOODY"))%>%
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass)) %>%
      dplyr::mutate(mass_category="anpp",note_biomass="anpp")%>%
      as_tibble()
    
    ANPP_done[["kernen_unburned"]] <-kernen_unburned
    
    # remove intermediate products
    rm(kernen_unburned)
    
# (26) Kiskunsag ---------------------------------
    Kiskunag<-full_biomass%>%
      dplyr::filter(site_code== "kiskun.hu",mass_category == "TOTAL")%>%
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass)) %>%
      dplyr::mutate(mass_category="anpp",note_biomass="unclear if anpp includes WOODY, only TOTAL listed")%>%
      as_tibble()
    
    ANPP_done[["Kiskunag"]] <-Kiskunag
    
    # remove intermediate products
    rm(Kiskunag)
 
# (27) Las_Chilcas_2016 ----------------------------------------------------
    las_chilcas<-full_biomass%>%
      dplyr::filter(site_code== "chilcasdrt.ar",mass_category %in% c("FORB","GRAMINOID","LEGUME"))%>%
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY")%>%
      as_tibble()
    
    ANPP_done[["las_chilcas"]] <-las_chilcas
    
    # remove intermediate products
    rm(las_chilcas)


# (28) Marchiquita_2015 ---------------------------------------------------
    marchiquita<-full_biomass%>%
      dplyr::filter(site_code== "marcdrt.ar",mass_category %in% c("FORB","GRAMINOID","LEGUME"))%>%
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="aboveground peak biomass (proxy for ANPP)")%>%
      as_tibble()
      
    
    ANPP_done[["marchiquita"]] <-marchiquita
    
    #remove intermediate products
    rm(marchiquita)
    
# (29) Matador -------------------------------------------------------
    matador<-full_biomass%>%
      dplyr::filter(site_code== "matador.ca",mass_category  %in% c("FORB","GRAMINOID","LEGUME","WOODY"))%>%
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass)) %>%
      dplyr::mutate(mass_category="anpp",note_biomass="anpp")%>%
      as_tibble()
    
    ANPP_done[["matador"]] <-matador
    
    #remove intermediate products
    rm(matador)


# (30) Monte_Oriental-----------------------------------------------------
    monte_oriental<-full_biomass%>%
      dplyr::filter(site_code== "morient.ar",mass_category %in% c("FORB","GRAMINOID","LEGUME","WOODY"))%>% 
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
      as_tibble()
    
    ANPP_done[["monte_oriental"]] <-monte_oriental
    
    #remove intermediate products
    rm(monte_oriental)
    
# (31) Naposta -------------------------------------------------------
    naposta<-full_biomass%>%
      dplyr::filter(site_code== "naposta.ar",mass_category %in% c("LIVE"))%>% 
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="unclear if anpp includes WOODY, only LIVE listed")%>%
      as_tibble()
    
    ANPP_done[["naposta"]] <-naposta
    
    #remove intermediate products
    rm(naposta)

# (32) nyngan -------------------------------------------------------
    nyngan<-full_biomass%>%
      dplyr::filter(site_code== "nyngan.au",mass_category %in% c("FORB","GRAMINOID","LEGUME","GRASS","WOODY","FERN"))%>% 
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
      as_tibble()
    
    ANPP_done[["nyngan"]] <-nyngan
    
    #remove intermediate products
    rm(nyngan)
    
# (33) Oklahoma (KAEFS-OK) -------------------------------------------------------
    #PI clarified that WOODY only included leaves, no stems
    oklah<-full_biomass%>%
      dplyr::filter(site_code== "oklah.us",mass_category %in% c("ANPP_WOODY+NON-WOODY"))%>% 
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
      as_tibble()
    
    ANPP_done[["oklah"]] <-oklah
    
    #remove intermediate products
    rm(oklah)
    
# (34) ORE IDE AA -------------------------------------------------------
      oreaa<-full_biomass%>%
      dplyr::filter(site_code== "oreaa.us",mass_category %in% c("LIVE"))%>% 
    dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="ANPP does not include WOODY")%>%
      as_tibble()
    
    ANPP_done[["oreaa"]] <-oreaa
    
    #remove intermediate products
    rm(oreaa)
    
# (35) ORE IDE AC -------------------------------------------------------
    oreac<-full_biomass%>%
      dplyr::filter(site_code== "oreac.us",mass_category %in% c("LIVE"))%>% 
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="ANPP does not include WOODY")%>%
      as_tibble()
    
    ANPP_done[["oreac"]] <-oreac
    
    #remove intermediate products
    rm(oreac)
    
# (36) Pineta ---------------------------------------------------
    #42584 refers to 2016, other year is bad too, is a formatting issue in excel, bad data formatting
    # Assuming this has been fixed in SQL
    pineta<-full_biomass%>%
      dplyr::filter(site_code== "pineta.es",mass_category %in% c("FORB","GRAMINOID","LEGUME","WOODY"))%>% 
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
      as_tibble()
    
   ANPP_done[["pineta"]] <-pineta
   
   #remove intermediate products
   rm(pineta)
    
#(37) Plattville ---------------------------------------------------------
     plattville<-full_biomass%>%
      dplyr::filter(site_code=="plattev.us",mass_category %in% c("FORB","GRASS","LEGUME"))%>%
       dplyr::group_by(site_code,block,plot,subplot,year)%>%
     dplyr::summarize(mass=sum(mass))%>%
       dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY")%>%
       as_tibble()
     
     ANPP_done[["plattville"]] <-plattville
     
     #remove intermediate products
     rm(plattville)
     
# (38) Potrok_Aike --------------------------------------------------------
    paike<-full_biomass%>%
      dplyr::filter(site_code== "paike.ar",mass_category %in% c("WOODY","LIVE", "LEGUME", "FORB","GRAMINOID"))%>%
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
      as_tibble()
    
     ANPP_done[["paike"]] <-paike
    
     #remove intermediate products
     rm(paike)
   
# (39) pne burned-------------------------------------------------------
    pneburn<-full_biomass%>%
      dplyr::filter(site_code== "pneburn.br",mass_category %in% c("GRAMINOID","FORB","WOODY"))%>% 
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
      as_tibble()
    
    ANPP_done[["pneburn"]] <-pneburn
    
    #remove intermediate products
    rm(pneburn)
    
# (40) pne unburned-------------------------------------------------------
    pneunburn<-full_biomass%>%
      dplyr::filter(site_code== "pneunburn.br",mass_category %in% c("GRAMINOID","FORB","WOODY"))%>% 
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
      as_tibble()
    
    ANPP_done[["pneunburn"]] <-pneunburn
    
    #remove intermediate products
    rm(pneunburn)

# (41) Pozos --------------------------------------------------------
    pozos<-full_biomass%>%
    dplyr::filter(site_code== "pozos.ar",mass_category %in% c("WOODY","GRAMINOID"))%>%
    dplyr::group_by(site_code,block,plot,subplot,year)%>%
    dplyr::summarize(mass=sum(mass))%>%
    dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
    as_tibble()
     
    ANPP_done[["pozos"]] <-pozos
    
    #remove intermediate products
    rm(pozos)    

# (42) Quilpie --------------------------------------------------------
     quilpie<-full_biomass%>%
       dplyr::filter(site_code== "quilpie.au",mass_category %in% c("WOODY","GRASS", "LEGUME", "FORB","GRAMINOID"))%>%
       dplyr::group_by(site_code,block,plot,subplot,year)%>%
       dplyr::summarize(mass=sum(mass))%>%
       dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
       as_tibble()
     
     ANPP_done[["quilpie"]] <-quilpie
     
     #remove intermediate products
     rm(quilpie)
    
# (43) Rhijnauwen ----------------------------------------------------
    #two dates summed for one ANPP value, put last harvest year for year...
    rhijnauwen<-full_biomass%>%
      dplyr::filter(site_code== "rhijn.nl",mass_category %in% c("TOTAL"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=sum(mass))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="Total is anpp")%>% #change year to last of the two harvests
        as_tibble()
    
    ANPP_done[["rhijnauwen"]] <-rhijnauwen
    
    #remove intermediate products
    rm(rhijnauwen)
    
# (44) Rio_Mayo  ------------------------------------------------------
    rio_mayo<-full_biomass%>%
      dplyr::filter(site_code== "riomayo.ar",mass_category %in% c("WOODY","GRASS", "LEGUME","GRAMINOID"))%>%
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
      as_tibble()
     
    ANPP_done[["rio_mayo"]] <-rio_mayo
    
    #remove intermediate products
    rm(rio_mayo)
    
# (45) San_Claudio ---------------------------------------------------
    san_claudio<-full_biomass%>%
      dplyr::filter(site_code== "sclaudio.ar",mass_category %in% c("FORB","GRAMINOID"))%>%
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY")%>%
      as_tibble()
      
    ANPP_done[["san_claudio"]] <-san_claudio
    
    #remove intermediate products
    rm(san_claudio)
    
# (46) Santa Cruz High----------------------------------------------------
    Santa_Cruz_High<-full_biomass%>%
      dplyr::filter(site_code== "scruzh.us",mass_category %in% c("TOTAL"))%>%
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="unclear if anpp includes WOODY, only TOTAL listed")%>% 
      as_tibble()
    
    ANPP_done[["Santa_Cruz_High"]] <-Santa_Cruz_High
    
    #remove intermediate products
    rm(Santa_Cruz_High)
    
# (47) Santa Cruz Middle----------------------------------------------------
    Santa_Cruz_Middle<-full_biomass%>%
      dplyr::filter(site_code== "scruzm.us",mass_category %in% c("TOTAL"))%>%
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="unclear if anpp includes WOODY, only TOTAL listed")%>% 
      as_tibble()
    
    ANPP_done[["Santa_Cruz_Middle"]] <-Santa_Cruz_Middle
    
    #remove intermediate products
    rm(Santa_Cruz_Middle)

# (48) Santa Cruz Low----------------------------------------------------
    Santa_Cruz_Low<-full_biomass%>%
      dplyr::filter(site_code== "scruzl.us",mass_category %in% c("TOTAL"))%>%
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="unclear if anpp includes WOODY, only TOTAL listed")%>% #change year to last of the two harvests
      as_tibble()
    
    ANPP_done[["Santa_Cruz_Low"]] <-Santa_Cruz_Low
    
    #remove intermediate products
    rm(Santa_Cruz_Low)
 
# (49) Sedgewick ---------------------------------------------------
    sedgewick<-full_biomass%>%
      dplyr::filter(site_code== "sedgwick.us",mass_category %in% c("FORB","GRAMINOID"))%>%
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY")%>%
      as_tibble()
    
    ANPP_done[["sedgewick"]] <-sedgewick
    
    #remove intermediate products
    rm(sedgewick)
    
# (50) Sevilleta Forest [EXCLUDE: Site does not report ANPP- WOODY includes secondary growth]
    sevforest<-full_biomass%>%
      dplyr::filter(site_code== "sevforest.us",mass_category %in% c("WOODY"))%>%
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="site DOES NOT report ANPP and cannot calculate anpp")%>%
      as_tibble()
    
    ANPP_done[["sevforest"]] <-sevforest
    
    #remove intermediate products
    rm(sevforest)
 
# (51) Shortgrass Steppe ---------------------------------------------------
    sgs<-full_biomass%>%
      dplyr::filter(site_code== "sgsdrt.us",mass_category %in% c("FORB","GRAMINOID","WOODY"))%>%
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
      as_tibble()
    
    ANPP_done[["sgs"]] <-sgs
    
    #remove intermediate products
    rm(sgs)

# (52) Sonora ---------------------------------------------------
    sonora<-full_biomass%>%
      dplyr::filter(site_code== "sonora.us",mass_category %in% c("FORB","GRAMINOID"))%>%
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY")%>%
      as_tibble()
    
    ANPP_done[["sonora"]] <-sonora
    
    #remove intermediate products
    rm(sonora)
    
#(53) San_Pablo_de_Valdes #According to follow up e-mail, site collects peak biomass as proxy for ANPP: ------------------------------------------
      sanpablo<-full_biomass%>%
      dplyr::filter(site_code== "spvdrt.ar",mass_category %in% c("FORB","GRAMINOID","LEGUME"))%>%
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="site DOES NOT report ANPP and cannot calculate anpp")%>%
      as_tibble()
    
    ANPP_done[["sanpablo"]] <-sanpablo
    
    #remove intermediate products
    rm(sanpablo)

# (54) Teshio ------------------------------------------
    teshio<-full_biomass%>%
      dplyr::filter(site_code== "teshio.jp",mass_category %in% c("FORB","GRAMINOID","LEGUME","GRASS"))%>%
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=sum(mass))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY")%>%
      as_tibble()
    
    ANPP_done[["teshio"]] <-teshio
    
    #remove intermediate products
    rm(teshio)

# (55) Validate: NOT USING THIS SITE BECAUSE THEY DON'T REPORT ANPP-------------------------
      validate<-full_biomass%>%
      dplyr::filter(site_code== "validate.fr",mass_category %in% c("LIVE","TOTAL"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=max(mass))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="Standing mass - phytomass")%>%
        as_tibble()
      
      ANPP_done[["validate"]] <-validate
      
      #remove intermediate products
      rm(validate)
      
# (56) Yaramundi ----------------------------------------------------
      yaramundi<-full_biomass%>%
        dplyr::filter(site_code== "yarradrt.au",mass_category %in% c("FORB","GRAMINOID","LEGUME","GRASS","BRYOPHYTE"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY")%>%
        as_tibble()
        
      ANPP_done[["yaramundi"]] <-yaramundi
      
      #remove intermediate products
      rm(yaramundi)

      
# (57) Bamboo Drought ----------------------------------------------------
      bamboo<-full_biomass%>%
        dplyr::filter(site_code== "bamboo.cn",mass_category %in% c("ANPP_NON-WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp includes bamboo")%>%
        as_tibble()
      
      ANPP_done[["bamboo"]] <-bamboo
      
      #remove intermediate products
      rm(bamboo)

# (58) Bivens Arm ----------------------------------------------------
      bivens<-full_biomass%>%
        dplyr::filter(site_code== "bivensarm.us",mass_category %in% c("FORB","GRAMINOID","GRASS","LEGUME","WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
        as_tibble()
      
      ANPP_done[["bivens"]] <-bivens
      
      #remove intermediate products
      rm(bivens)
      
# (59) Broken Hill ----------------------------------------------------
      brokenh<-full_biomass%>%
        dplyr::filter(site_code== "brokenh.au",mass_category %in% c("FORB","GRAMINOID","GRASS","LEGUME","WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
        as_tibble()
      
      ANPP_done[["brokenh"]] <-brokenh
      
      #remove intermediate products
      rm(brokenh)
      
# (60) CAP McDowell ----------------------------------------------------
      capmcd<-full_biomass%>%
        dplyr::filter(site_code== "capmcd.us",mass_category %in% c("ANPP_NON-WOODY","GRAMINOID", "FORB","LEGUME"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY")%>%
        as_tibble()
      
      ANPP_done[["capmcd"]] <-capmcd
      
      #remove intermediate products
      rm(capmcd)
      
# (61) CAP White Mountain ----------------------------------------------------
      capwhite<-full_biomass%>%
        dplyr::filter(site_code== "capwhite.us",mass_category %in% c("ANPP_NON-WOODY","GRAMINOID", "FORB","LEGUME"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY")%>%
        as_tibble()
      
      ANPP_done[["capwhite"]] <-capwhite
      
      #remove intermediate products
      rm(capwhite)

# (62) Los Cerrillos ----------------------------------------------------
      cerrillos<-full_biomass%>%
        dplyr::filter(site_code== "cerrillos.ar",mass_category %in% c("GRAMINOID","NON-WOODY","WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
        as_tibble()
      
      ANPP_done[["cerrillos"]] <-cerrillos
      
      #remove intermediate products
      rm(cerrillos)

# (63) Freiburg ----------------------------------------------------
      freiburg<-full_biomass%>%
        dplyr::filter(site_code== "freiburg.de",mass_category %in% c("FORB","GRAMINOID","LEGUME"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY")%>%
        as_tibble()
      
      ANPP_done[["freiburg"]] <- freiburg
      
      #remove intermediate products
      rm(freiburg)

# (64) GMDRC Granite Cove ----------------------------------------------------
      granite<-full_biomass%>%
        dplyr::filter(site_code== "gmgranite.us",mass_category %in% c("ANPP_NON-WOODY","GRAMINOID", "FORB","LEGUME"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY")%>%
        as_tibble()
      
      ANPP_done[["granite"]] <- granite
      
      #remove intermediate products
      rm(granite)

# (65) GMDRC Molar Junction ----------------------------------------------------
      molar<-full_biomass%>%
        dplyr::filter(site_code== "gmmolar.us",mass_category %in% c("ANPP_NON-WOODY","GRAMINOID", "FORB","LEGUME"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY")%>%
        as_tibble()
      
      ANPP_done[["molar"]] <- molar
      
      #remove intermediate products
      rm(molar)

# (66) Kinsella ----------------------------------------------------
      kinsella<-full_biomass%>%
        dplyr::filter(site_code== "kinsella.ca",mass_category %in% c("ANPP_NON-WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY")%>%
        as_tibble()
      
      ANPP_done[["kinsella"]] <- kinsella
      
      #remove intermediate products
      rm(kinsella)

      
# (67) Konza ----------------------------------------------------
      konza<-full_biomass%>%
        dplyr::filter(site_code== "konzadrt.us",mass_category %in% c("FORB","GRAMINOID"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY")%>%
        as_tibble()
      
      ANPP_done[["konza"]] <- konza
      
      #remove intermediate products
      rm(konza)  

# (68) Mattheis ----------------------------------------------------
      mattheis<-full_biomass%>%
        dplyr::filter(site_code== "mattheis.ca",mass_category %in% c("ANPP_NON-WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY")%>%
        as_tibble()
      
      ANPP_done[["mattheis"]] <- mattheis
      
      #remove intermediate products
      rm(mattheis)
 
# (69) Milparinka ----------------------------------------------------
      milparinka<-full_biomass%>%
        dplyr::filter(site_code== "milparinka.au",mass_category %in% c("FORB","GRAMINOID","GRASS","WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
        as_tibble()
      
      ANPP_done[["milparinka"]] <- milparinka
      
      #remove intermediate products
      rm(milparinka)     
      
# (70) Passo Gavia ----------------------------------------------------
      passogavia<-full_biomass%>%
        dplyr::filter(site_code== "passogavia.it",mass_category %in% c("BRYOPHYTE","FORB","GRAMINOID","WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY & BRYOPHYTE")%>%
        as_tibble()
      
      ANPP_done[["passogavia"]] <- passogavia
      #remove intermediate products
      rm(passogavia)
      
# (71) Purdue ----------------------------------------------------
      purdue<-full_biomass%>%
        dplyr::filter(site_code== "purdue.us",mass_category %in% c("LIVE"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="unclear if anpp includes WOODY,only LIVE listed")%>%
        as_tibble()
      
      ANPP_done[["purdue"]] <- purdue      
      #remove intermediate products
      rm(purdue) 

# (72) Stubai Bahn ----------------------------------------------------
      stubai<-full_biomass%>%
        dplyr::filter(site_code== "stubai.at",mass_category %in% c("LIVE"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="unclear if anpp includes WOODY,only LIVE listed")%>%
        as_tibble()
      
      ANPP_done[["stubai"]] <- stubai      
      #remove intermediate products
      rm(stubai) 
 
# (73) Swift Current ----------------------------------------------------
      swift<-full_biomass%>%
        dplyr::filter(site_code== "swift.ca",mass_category %in% c("FORB","GRAMINOID"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY")%>%
        as_tibble()
      
      ANPP_done[["swift"]] <- swift      
      #remove intermediate products
      rm(swift)

# (74) Syferkuil/DroughtNet Limpopo ----------------------------------------------------
      syferkuil<-full_biomass%>%
        dplyr::filter(site_code== "syferkuil.za",mass_category %in% c("FORB","GRASS"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY")%>%
        as_tibble()
      
      ANPP_done[["syferkuil"]] <- syferkuil      
      #remove intermediate products
      rm(syferkuil)  
      
# (75) Torla (aka Ordesa) ----------------------------------------------------
#PI stated to remove WOODY biomass
      torla<-full_biomass%>%
        dplyr::filter(site_code== "torla.es",mass_category %in% c("FORB","GRAMINOID","LEGUME"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY")%>%
        as_tibble()
      
      ANPP_done[["torla"]] <- torla      
      #remove intermediate products
      rm(torla)  
      
# (76) Ukulinga ----------------------------------------------------
      ukulinga<-full_biomass%>%
        dplyr::filter(site_code== "ukulingadrt.za",mass_category %in% c("FORB","GRAMINOID","LEGUME"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY")%>%
        as_tibble()
      
      ANPP_done[["ukulinga"]] <- ukulinga     
      #remove intermediate products
      rm(ukulinga) 
      
# (77) Wayqecha ----------------------------------------------------
      wayqecha<-full_biomass%>%
        dplyr::filter(site_code== "wayqecha.pe",mass_category %in% c("WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp only includes WOODY")%>%
        as_tibble()
      
      ANPP_done[["wayqecha"]] <- wayqecha     
      #remove intermediate products
      rm(wayqecha)

# (78) Wytham Rain Drop ----------------------------------------------------
      wytham<-full_biomass%>%
        dplyr::filter(site_code== "wytham.uk",mass_category %in% c("BRYOPHYTE","FORB","GRAMINOID","LEGUME"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp includes BRYOPHYTE, does not include WOODY")%>%
        as_tibble()
      
      ANPP_done[["wytham"]] <- wytham     
      #remove intermediate products
      rm(wytham) 
      
# (79) HOIDE ----------------------------------------------------
    # Since Woody values in 2015 are not ANPP, we will exclude 2015 altogether
    # They calculated WOODY for subsequent years by subtracting from previous year starting in 2015, but
    ## UPDATED 03-29-2022
      hoide0<-full_biomass%>%
        dplyr::filter(site_code== "hoide.de",mass_category %in% c("GRASS","GRAMINOID","WOODY"),(!year %in% c(2015)))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
        as_tibble()
      
      #Plot 10 is an outlier in 2017 and 2019 (biomass less than 4 g/m^2 when it's over 800 in 2018 and other plots are over 100-200)
      hoide<-hoide0%>%dplyr::filter(!(year %in% c(2017,2019) & plot %in% (10)))%>%
        dplyr::as_tibble()
      
      ANPP_done[["hoide.de"]] <- hoide     
      #remove intermediate products
      rm(hoide) 
      
# (80) HYIDE ----------------------------------------------------
      # Since Woody values in 2015 are not ANPP, we will exclude 2015 altogether
      # They calculated WOODY for subsequent years by subtracting from previous year starting in 2015, but
      ## UPDATED 03-29-2022
      hyide<-full_biomass%>%
        dplyr::filter(site_code== "hyide.de",mass_category %in% c("GRASS","GRAMINOID","WOODY"),(!year %in% c(2015)))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY")%>%
        as_tibble()
      
      ANPP_done[["hyide.de"]] <- hyide     
      #remove intermediate products
      rm(hyide) 

# (81) Boulder 
      boulder<-full_biomass%>%
        dplyr::filter(site_code== "bldrdrt.us",mass_category %in% c("ANPP_NON-WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY")%>%
        as_tibble()
      
      ANPP_done[["bldrdrt.us"]] <- boulder    
      #remove intermediate products
      rm(boulder) 

# (82) Middle Ebro Valley [NOTE FROM PI IN SURVEY:it says the data is not ANPP, but we should subtract year's biomass from previous year to get ANPP]
       #However, we get negative values when we take this approach...still unclear as to what we should do
       ebro<-full_biomass%>%
        dplyr::filter(site_code== "ebro.es",mass_category %in% c("TOTAL"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="does not report ANPP")%>%
        as_tibble()
      
      ANPP_done[["ebro.es"]] <- ebro   
      #remove intermediate products
      rm(ebro) 
      
# (83) Elizabeth Woods (EXCLUDE: Did not follow DroughtNet protocols)
      elizwood<-full_biomass%>%
      dplyr::filter(site_code== "elizwood.us",mass_category %in% c("TOTAL"))%>%
      dplyr::group_by(site_code,block,plot,subplot,year)%>%
      dplyr::summarize(mass=(sum(mass)))%>%
      dplyr::mutate(mass_category="anpp",note_biomass="TOTAL=Wood production in each year")%>%
      as_tibble()
      
      ANPP_done[["elizwood.us"]] <- elizwood   
      #remove intermediate products
      rm(elizwood)

      
# (84) Ethabuka DNB (Burned)
      ethabuka_dnb<-full_biomass%>%
        dplyr::filter(site_code =="ethadb.au",mass_category %in% c("GRAMINOID","FORB","LEGUME"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=sum(mass)) %>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY")%>%
        as_tibble()
      
      ANPP_done[["ethabuka_dnb"]] <- ethabuka_dnb
      #remove intermediate products
      rm(ethabuka_dnb)    
      
# (85) Ethabuka DNU (Unburned)
      ethabuka_dnu<-full_biomass%>%
        dplyr::filter(site_code =="ethadn.au",mass_category %in% c("GRAMINOID","FORB","LEGUME"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=sum(mass)) %>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include WOODY")%>%
        as_tibble()
      
      ANPP_done[["ethabuka_dnu"]] <- ethabuka_dnu
      #remove intermediate products
      rm(ethabuka_dnu)    
      
#(86) Garraf [Only include controls because project running since 1999 and didn't follow Dnet protocols]
      garraf<-full_biomass%>%
        dplyr::filter(site_code== "garraf.es",mass_category %in% c("WOODY","LEGUME"),trt=="Control")%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
        as_tibble()
      
      ANPP_done[["garraf.es"]] <- garraf   
      #remove intermediate products
      rm(garraf)

# (87) Hubbard Brook
      hubbard<-full_biomass%>%
        dplyr::filter(site_code== "hubbard.us",mass_category %in% c("WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp includes WOODY")%>%
        as_tibble()
      
      ANPP_done[["hubbard.us"]] <- hubbard   
      #remove intermediate products
      rm(hubbard)  
      
# (88) La Campana North Facing [EXCLUDE: TOTAL biomass includes previous year's dead (litter) and PI cannot separate it]
      lcnorth<-full_biomass%>%
        dplyr::filter(site_code== "lcnorth.cl",mass_category %in% c("TOTAL"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="TOTAL include LITTER")%>%
        as_tibble()
      
      ANPP_done[["lcnorth.cl"]] <- lcnorth   
      #remove intermediate products
      rm(lcnorth)
      
# (89) La Campana South Facing [EXCLUDE: TOTAL biomass includes previous year's dead (litter) and PI cannot separate it]
      lcsouth<-full_biomass%>%
        dplyr::filter(site_code== "lcsouth.cl",mass_category %in% c("TOTAL"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="TOTAL include LITTER")%>%
        as_tibble()
      
      ANPP_done[["lcsouth.cl"]] <- lcsouth   
      #remove intermediate products
      rm(lcsouth)
      
# (90) Matta ILTER
      matta<-full_biomass%>%
        dplyr::filter(site_code== "matta.il",mass_category %in% c("FORB","LEGUME","WOODY","GRASS"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp include WOODY")%>%
        as_tibble()
      
      ANPP_done[["matta.il"]] <- matta   
      #remove intermediate products
      rm(matta)
      
# (91) North Platte
      #Remove Woody based on conversation with PI
      nplatte<-full_biomass%>%
        dplyr::filter(site_code== "nplatte.us",mass_category %in% c("GRAMINOID","FORB"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp include WOODY")%>%
        as_tibble()
      
      ANPP_done[["nplatte.us"]] <- nplatte   
      #remove intermediate products
      rm(nplatte)

# (92) Prades [Only include controls: long-term data that couldn't have followed DNet protocols since 1999]
      prades<-full_biomass%>%
        dplyr::filter(site_code== "prades.es",mass_category %in% c("WOODY","LEGUME"),trt=="Control")%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp include WOODY")%>%
        as_tibble()
      
      ANPP_done[["prades.es"]] <- prades   
      #remove intermediate products
      rm(prades) 
      
# (93) Quebrada de Talca North [EXCLUDE: TOTAL biomass includes previous year's dead (litter) and PI cannot separate it]
        qdtnorth<-full_biomass%>%
        dplyr::filter(site_code== "qdtnorth.cl",mass_category %in% c("TOTAL"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="TOTAL includes LITTER")%>%
        as_tibble()
      
      ANPP_done[["qdtnorth.cl"]] <- qdtnorth 
      #remove intermediate products
      rm(qdtnorth)
      
# (94) Quebrada de Talca South [EXCLUDE: TOTAL biomass includes previous year's dead (litter) and PI cannot separate it]
      qdtsouth<-full_biomass%>%
        dplyr::filter(site_code== "qdtsouth.cl",mass_category %in% c("TOTAL"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="TOTAL includes LITTER")%>%
        as_tibble()
      
      ANPP_done[["qdtsouth.cl"]] <- qdtsouth 
      #remove intermediate products
      rm(qdtsouth)
      
# (95) Thompson [EXCLUDE: Data is not ANPP because data includes secondary growth]
      thompson<-full_biomass%>%
        dplyr::filter(site_code== "thompson.us",mass_category %in% c("WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="TOTAL includes secondary growth")%>%
        as_tibble()
      
      ANPP_done[["thompson.us"]] <- thompson 
      #remove intermediate products
      rm(thompson)

# (96) Brandbjerg (ONLY INCLUDE CONTROL PLOTS)
      brandbjerg<-full_biomass%>%
        dplyr::filter(site_code== "brandjberg.dk",mass_category %in% c("LIVE"),trt=="Control")%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp includes this year's dead")%>%
        as_tibble()
      
      ANPP_done[["brandbjerg"]] <- brandbjerg   
      #remove intermediate products
      rm(brandbjerg)   

# (97) Xilinhot
      xilinhot<-full_biomass%>%
        dplyr::filter(site_code== "xilin.cn",mass_category %in% c("GRAMINOID","FORB","GRASS","LEGUME"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp doesn't include woody")%>%
        as_tibble()
      
      ANPP_done[["xilinhot"]] <- xilinhot  
      #remove intermediate products
      rm(xilinhot)    

# (98) Yanchi
      yanchi<-full_biomass%>%
        dplyr::filter(site_code== "yanchi.cn",mass_category %in% c("FORB","GRASS","LEGUME"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp doesn't include woody")%>%
        as_tibble()
      
      ANPP_done[["yanchi"]] <- yanchi  
      #remove intermediate products
      rm(yanchi)    
      
# (99) Urat
      urat<-full_biomass%>%
        dplyr::filter(site_code== "urat.cn",mass_category %in% c("GRAMINOID","FORB","GRASS","LEGUME"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp doesn't include woody")%>%
        as_tibble()
      
      ANPP_done[["urat"]] <- urat  
      #remove intermediate products
      rm(urat)   

# (100) Haibei
      haibei<-full_biomass%>%
        dplyr::filter(site_code== "haibei.cn",mass_category %in% c("TOTAL"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp=total biomass")%>%
        as_tibble()
      
      ANPP_done[["haibei"]] <- haibei  
      #remove intermediate products
      rm(haibei)
      
# (101) Bange
      bange<-full_biomass%>%
        dplyr::filter(site_code== "bange.cn",mass_category %in% c("GRAMINOID","FORB","GRASS","LEGUME"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp doesn't include woody")%>%
        as_tibble()
      
      ANPP_done[["bange"]] <- bange  
      #remove intermediate products
      rm(bange)  
      
# (102) Hulunber
      hulunber<-full_biomass%>%
        dplyr::filter(site_code== "hulun.cn",mass_category %in% c("GRAMINOID","FORB","GRASS","LEGUME"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp doesn't include woody")%>%
        as_tibble()
      
      ANPP_done[["hulunber"]] <- hulunber  
      #remove intermediate products
      rm(hulunber)       

# (103) Naqu
      naqu<-full_biomass%>%
        dplyr::filter(site_code== "naqu.cn",mass_category %in% c("GRAMINOID","FORB","GRASS","LEGUME"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp doesn't include woody")%>%
        as_tibble()
      
      ANPP_done[["naqu"]] <- naqu  
      #remove intermediate products
      rm(naqu)   
  
# (104) Haveroya (Everything except Woody is ANPP)
      haveroya<-full_biomass%>%
        dplyr::filter(site_code== "haver.no",mass_category %in% c("FORB","LICHEN","GRAMINOID","BRYOPHYTE","LEGUME"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp doesn't include woody")%>%
        as_tibble()
      
      ANPP_done[["haveroya"]] <- haveroya  
      #remove intermediate products
      rm(haveroya)
    
# (105) Skotsvaer (Everything except Woody is ANPP)
      skotsvaer<-full_biomass%>%
        dplyr::filter(site_code== "skotsvar.no",mass_category %in% c("FORB","LICHEN","GRAMINOID","BRYOPHYTE","LEGUME"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp doesn't include woody")%>%
        as_tibble()
      
      ANPP_done[["skotsvaer"]] <- skotsvaer 
      #remove intermediate products
      rm(skotsvaer)

# (106) Lygra_young (Everything except Woody is ANPP)
      lygra.young<-full_biomass%>%
        dplyr::filter(site_code== "lygrayng.no",mass_category %in% c("FORB","LICHEN","GRAMINOID","BRYOPHYTE","LEGUME"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp doesn't include woody")%>%
        as_tibble()
      
      ANPP_done[["lygra.young"]] <- lygra.young 
      #remove intermediate products
      rm(lygra.young) 
      
# (107) Lygra_intermediate (Everything except Woody is ANPP)
      lygra.int<-full_biomass%>%
        dplyr::filter(site_code== "lygraint.no",mass_category %in% c("FORB","LICHEN","GRAMINOID","BRYOPHYTE","LEGUME"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp doesn't include woody")%>%
        as_tibble()
      
      ANPP_done[["lygra.int"]] <- lygra.int 
      #remove intermediate products
      rm(lygra.int) 

# (108) Store Buoya (Everything except Woody is ANPP)
      storebuoya<-full_biomass%>%
        dplyr::filter(site_code== "buoya.no",mass_category %in% c("FORB","LICHEN","GRAMINOID","BRYOPHYTE","LEGUME","VASCULAR"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp doesn't include woody")%>%
        as_tibble()
      
      ANPP_done[["storebuoya"]] <- storebuoya
      #remove intermediate products
      rm(storebuoya)     

# (109) Hongyuan 
      hongyuan<-full_biomass%>%
        dplyr::filter(site_code== "hong.cn",mass_category %in% c("FORB","LEGUME","GRAMINOID","GRASS"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp doesn't include woody")%>%
        as_tibble()
      
      ANPP_done[["hong.cn"]] <- hongyuan
      #remove intermediate products
      rm(hongyuan)      

# (110) Sevilleta Blue 
      sevblue<-full_biomass%>%
        dplyr::filter(site_code== "sevblue.us",mass_category %in% c("FORB","LEGUME","GRAMINOID","WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp includes woody")%>%
        as_tibble()
      
      ANPP_done[["sevblue.us"]] <- sevblue
      #remove intermediate products
      rm(sevblue) 
      
# (111) Sevilleta Mixed 
      sevmixed<-full_biomass%>%
        dplyr::filter(site_code== "sevmixed.us",mass_category %in% c("FORB","LEGUME","GRAMINOID","WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp includes woody")%>%
        as_tibble()
      
      ANPP_done[["sevmixed.us"]] <- sevmixed
      #remove intermediate products
      rm(sevmixed)
      
# (112) Sevilleta Black
      sevblack<-full_biomass%>%
        dplyr::filter(site_code== "sevblack.us",mass_category %in% c("FORB","LEGUME","GRAMINOID","WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp includes woody")%>%
        as_tibble()
      
      ANPP_done[["sevblack.us"]] <- sevblack
      #remove intermediate products
      rm(sevblack)   
      
# (113) Kranzberg
      kranzberg<-full_biomass%>%
        dplyr::filter(site_code== "kranz.de",mass_category %in% c("WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp only includes woody")%>%
        as_tibble()
      
      ANPP_done[["kranz.de"]] <- kranzberg
      #remove intermediate products
      rm(kranzberg)
      
# (114) Credo J
     credoj<-full_biomass%>%
        dplyr::filter(site_code== "credoj.au",mass_category %in% c("GRAMINOID", "FORB","WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp includes woody")%>%
        as_tibble()
      
      ANPP_done[["credoj.au"]] <- credoj
      #remove intermediate products
      rm(credoj) 
      
# (115) Credo M
      credom<-full_biomass%>%
        dplyr::filter(site_code== "credom.au",mass_category %in% c("GRAMINOID", "FORB","WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp includes woody")%>%
        as_tibble()
      
      ANPP_done[["credom.au"]] <- credom
      #remove intermediate products
      rm(credom) 
      
# (116) BFL
      bfl<-full_biomass%>%
        dplyr::filter(site_code== "bfl.us",mass_category %in% c("ANPP_NON-WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include woody")%>%
        as_tibble()
      
      ANPP_done[["bfl.us"]] <- bfl
      #remove intermediate products
      rm(bfl)
      
# (117) SLP
      slp<-full_biomass%>%
        dplyr::filter(site_code== "slp.us",mass_category %in% c("ANPP_NON-WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include woody")%>%
        as_tibble()
      
      ANPP_done[["slp.us"]] <- slp
      #remove intermediate products
      rm(slp)

# (118) Dangxiong
      dang<-full_biomass%>%
        dplyr::filter(site_code== "dang.cn",mass_category %in% c("FORB","GRASS","GRAMINOID"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp does not include woody")%>%
        as_tibble()
      
      ANPP_done[["dang.cn"]] <- dang
      #remove intermediate products
      rm(dang)      

# (119) Horizon
      horizon<-full_biomass%>%
        dplyr::filter(site_code== "horizon.cr",mass_category %in% c("WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp only include woody")%>%
        as_tibble()
      
      ANPP_done[["horizon.cr"]] <- horizon
      #remove intermediate products
      rm(horizon) 
      
# (120) Lygra_old (Everything except Woody is ANPP)
      lygra.old<-full_biomass%>%
        dplyr::filter(site_code== "lygraold.no",mass_category %in% c("FORB","GRAMINOID","BRYOPHYTE"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp doesn't include woody")%>%
        as_tibble()
      
      ANPP_done[["lygra.old"]] <- lygra.old 
      #remove intermediate products
      rm(lygra.old) 
 
# (121) P12
      p12<-full_biomass%>%
        dplyr::filter(site_code== "p12.pa",mass_category %in% c("WOODY","PALM"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp includes woody and palm")%>%
        as_tibble()
      
      ANPP_done[["p12"]] <- p12
      #remove intermediate products
      rm(p12)   
      
# (122) P13
      p13<-full_biomass%>%
        dplyr::filter(site_code== "p13.pa",mass_category %in% c("WOODY","PALM"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp includes woody and palm")%>%
        as_tibble()
      
      ANPP_done[["p13"]] <- p13
      #remove intermediate products
      rm(p13)

# (123) Sherman Crane
      sherman<-full_biomass%>%
        dplyr::filter(site_code== "sherman.pa",mass_category %in% c("WOODY","PALM"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp only includes woody")%>%
        as_tibble()
      
      ANPP_done[["sherman"]] <- sherman
      #remove intermediate products
      rm(sherman)
      
# (124) Gigante
      gigante<-full_biomass%>%
        dplyr::filter(site_code== "gigante.pa",mass_category %in% c("WOODY","PALM"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp includes woody and palm")%>%
        as_tibble()
      
      ANPP_done[["gigante"]] <-gigante
      #remove intermediate products
      rm(gigante)
      
# (125) Neudamm #Unclear if Total includes litter or woody biomass and 
        #we have not been able to reach the Pi for the past two years
      neudamm<-full_biomass%>%
        dplyr::filter(site_code== "neudamm.na",mass_category %in% c("TOTAL"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="unclear if biomass is live")%>%
        as_tibble()
      
      ANPP_done[["neudamm"]] <-neudamm
      #remove intermediate products
      rm(neudamm)
      
# (126) Changling
      chang<-full_biomass%>%
        dplyr::filter(site_code== "chang.cn",mass_category %in% c("GRASS","FORB","LEGUME"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp")%>%
        as_tibble()
      
      ANPP_done[["chang"]] <-chang
      #remove intermediate products
      rm(chang)
      
# (127) Antelope
      antelope<-full_biomass%>%
        dplyr::filter(site_code== "antelope.us",mass_category %in% c("GRASS","GRAMINOID","FORB","LEGUME","WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp")%>%
        as_tibble()
      
      ANPP_done[["antelope"]] <-antelope
      #remove intermediate products
      rm(antelope)

# (128) JRN Chihuahuan
      chihuahuan<-full_biomass%>%
        dplyr::filter(site_code== "jrnchi.us",mass_category %in% c("ANPP_NON-WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp")%>%
        as_tibble()
      
      ANPP_done[["chihuahuan"]] <- chihuahuan
      #remove intermediate products
      rm(chihuahuan)     
      
# (129) NNSS Mojave
      mojave<-full_biomass%>%
        dplyr::filter(site_code== "nnss.us",mass_category %in% c("ANPP_NON-WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp")%>%
        as_tibble()
      
      ANPP_done[["mojave"]] <- mojave
      #remove intermediate products
      rm(mojave)
      
# (130) OCTC Great Basin
      greatbasin<-full_biomass%>%
        dplyr::filter(site_code== "octc.us",mass_category %in% c("ANPP_NON-WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp")%>%
        as_tibble()
      
      ANPP_done[["greatbasin"]] <- greatbasin
      #remove intermediate products
      rm(greatbasin)

# (131) Elva
      elva<-full_biomass%>%
        dplyr::filter(site_code== "elvadrt.ee",mass_category %in% c("FORB", "LEGUME","GRAMINOID","BRYOPHYTE"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp")%>%
        as_tibble()
      
      ANPP_done[["elva"]] <- elva
      #remove intermediate products
      rm(elva)      

# (132) Sandhills #Pine needles also listed, but excluding for now
      sandhills<-full_biomass%>%
        dplyr::filter(site_code== "sand.us",mass_category %in% c("GRAMINOID","LEGUME","WOODY","FORB","BRYOPHYTE"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp")%>%
        as_tibble()
      
      ANPP_done[["sandhills"]] <- sandhills
      #remove intermediate products
      rm(sandhills)     
      
# (133) Youyu
      youyu<-full_biomass%>%
        dplyr::filter(site_code== "youyu.cn",mass_category %in% c("GRASS","LEGUME","FORB"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp")%>%
        as_tibble()
      
      ANPP_done[["youyu"]] <- youyu
      #remove intermediate products
      rm(youyu)  
      
# (134) MPG Ranch
      mpg<-full_biomass%>%
        dplyr::filter(site_code== "mpgranch.us",mass_category %in% c("GRAMINOID","LEGUME","FORB","WOODY"))%>%
        dplyr::group_by(site_code,block,plot,subplot,year)%>%
        dplyr::summarize(mass=(sum(mass)))%>%
        dplyr::mutate(mass_category="anpp",note_biomass="anpp")%>%
        as_tibble()
      
      ANPP_done[["mpg"]] <- mpg
      #remove intermediate products
      rm(mpg)  
     
#We have 136 sites total, but these are sites who have sent cover data, but not biomass data
#2 sites: (135) "indiana.us" (136) "noor.ir"    
      
# anpp data.frame ---------------------------------------------------------
#merge all derived datasets into one data frame
# go to edit and expand all sections
      anpp_done_2<-rbindlist(ANPP_done)
      # 
      # #make into data frame
      anpp_data.frame<-as.data.frame(anpp_done_2)
      
      length(unique(anpp_data.frame$site_code))

      #rm(ANPP_done)
      #rm(anpp_done_2)
setdiff(full_biomass$site_code,anpp_data.frame$site_code)
#write.csv(anpp_data.frame,"C:/Users/lgherar1.ASURITE/Dropbox (ASU)/IDE Meeting_Oct2019/Full Biomass/anpp_clean_12-18-2019.csv")
write.csv(anpp_data.frame,"C:\\Users\\Kwilks86\\Dropbox\\IDE MS_Single year extreme\\Data\\anpp_clean_03-28-2022.csv",row.names=FALSE)
#dev.off()
