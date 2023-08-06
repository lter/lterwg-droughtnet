###Code to for how different control-treatment communities are with drought
###code by Meghan Avolio, Feb 8, 2023
###code by Maggie Anderson, May 03, 2023
###March 21, 2023. Working to include all communities for the difference analysis.

################################################################################
# PART 1: setup
################################################################################

# clear workspace
rm(list = ls()) 

# detach pesky packages
detach(package:plyr,unload=TRUE)

# Load libraries 
library(tidyverse) # data wrangling
library(codyn) # diversity indices
library(lme4) # linear models
library(lmerTest)
library(vegan) # diveristy indices
library(chisq.posthoc.test) # post-hoc test
library(plotly) # interactive plots
library(Hmisc) # confidence intervals
library(ggpubr) # arranging plots
library(emmeans) # extracting stats
library(car)

theme_set(theme_bw(16)) # set theme

################################################################################
# PART 2: load in data
################################################################################

setwd("G:/My Drive/DroughtnetTest/Data_offline")
#meghan's path
setwd('C://Users/mavolio2//Dropbox//IDE (1)//data_processed')

# cover data
dat<-read.csv("cover_ppt_2023-05-10.csv") %>% 
  mutate(replicate=paste(block, plot, subplot, sep="::"))

# load in multivariate axis data from Meghan H.
pca <- read.csv("IDE_taxon_site_trait_ordination.csv")

# load in dominance dataset from Meghan H.
# dom.old <- read.csv("IDE_Year1_DominantSpp.csv")
# # Load in PCA dataset from Meghan Hayden
dom <- read.csv("codominant_spp_list_yr0.csv")# includes dominant and co-dominant species. Previously "IDE_Year1_DominantSpp.csv"
# species are either dominant (D), codominant (CD), or subdominant (SD)


# site information
site_types<-read.csv("Prc_LifeHistory_Yr1Controls.csv")

continent<-read.csv("Site_Elev-Disturb.csv") %>% 
  dplyr::select(site_code, continent)

# sites we are using for analysis (77)
sites <- read.csv("sites_77.csv")


################################################################################
# PART 3: getting measures of difference
################################################################################

###looping through site to get at differences
datall<-dat%>% 
  mutate(rep=paste(site_code, replicate, sep=";")) %>% 
  filter(n_treat_years!=0.5&n_treat_years!=-1) %>% 
  filter(n_treat_years==3) %>%
  # update: filter out "brokenh.au" because there is literally only one species
  filter(site_code != "brokenh.au")

sc<-unique(datall$site_code) # site codes for looping

diff_metrics<-data.frame() # new data frame

for (i in 1:length(sc)){
  
  subset<-datall%>%
    filter(site_code==sc[i]) # subset for a given site
  
  nyrs<-subset %>% 
    dplyr::select(year, n_treat_years) %>% # from the selected site, get the year(s) information
    unique()
  
  scode<-unique(subset$site_code) # unique site code
  print(scode)
  
  diff_mult<-multivariate_difference(df=subset, time.var="year", species.var = "Taxon", abundance.var = "max_cover", replicate.var="replicate", treatment.var="trt", reference.treatment = "Control") %>% 
    dplyr::select(-trt, -trt2) # calculates differences in composition between pairs of treatments @ single time point
  
  diff_ranks<-RAC_difference(df=subset, time.var="year", species.var = "Taxon", abundance.var = "max_cover", replicate.var="replicate", treatment.var="trt", reference.treatment = "Control") %>% 
    mutate(site_code=scode) %>% # differences in rank abundances between control and treated plots across years
    dplyr::select(-replicate2, -trt, -trt2) %>% 
    group_by(site_code, year, replicate) %>% 
    summarize_all(mean) %>% 
    dplyr::select(-replicate) %>% 
    group_by(site_code, year) %>% 
    summarize_all(mean)
  
  diffmetrics<-diff_ranks %>% # join dataframes together
    left_join(diff_mult) %>% 
    left_join(nyrs)
  
  
  diff_metrics<-diff_metrics %>% 
    bind_rows(diffmetrics)
}


################################################################################
# PART 4: calculate drought severity
################################################################################

#getting drought severity
drt<-dat %>% 
  filter(trt=="Drought") %>% 
  dplyr::select(site_code, n_treat_years, trt, year, map, ppt.1, ppt.2, ppt.3, ppt.4) %>% 
  unique() %>% 
  mutate(drtseverity=(ppt.1-map)/map) %>% 
  dplyr::select(-ppt.1, -ppt.2, -ppt.3, -ppt.4)


#looking at data
difflong<-diff_metrics %>% 
  dplyr::select(-abs_dispersion_diff, -trt_greater_disp) %>% 
  pivot_longer(names_to = 'measure', values_to = 'value', richness_diff:composition_diff) %>% 
  left_join(drt) %>% 
  left_join(site_types)

unique(diff_metrics$site_code)

ggplot(data=difflong, aes(x=drtseverity, y=value))+
  geom_point()+
  geom_smooth(method='lm')+
  facet_grid(measure~n_treat_years, scales="free")


################################################################################
# PART 5: prep data for trait gain/loss analyses
################################################################################
drop_no_pretrt<-dat %>% 
  select(site_code, n_treat_years) %>% 
  unique() %>% 
  filter(n_treat_years==0) %>% 
  select(-n_treat_years)

dat2<-dat %>% 
  right_join(drop_no_pretrt) %>% 
  mutate(rep=paste(site_code, replicate, sep=";")) %>% 
  filter(n_treat_years!=0.5&n_treat_years!=-1&n_treat_years<4) %>% 
  filter(site_code!="cdpt_drt.us"&site_code!="eea.br")

length(unique(dat2$site_code))

datblip<-dat2%>% # prep dataframe for looping
  mutate(rep=paste(site_code, replicate, sep=";")) %>% 
  filter(max_cover!=0&!is.null(Family)) %>% 
  # standardizing lifeform and lifespan information
  mutate(local_lifeform = ifelse(local_lifeform %in% c("GRAMINOID","Grass", 'GRASS'),"Grass",
                          ifelse(local_lifeform%in% c("VINE","TREE","WOODY","SHRUB","SUBSHRUB"),"Woody",
                          ifelse(local_lifeform %in% c("BRYOPHYTE","MOSS","CLUBMOSS","LICHEN","FUNGI"),"NONVASCULAR",
                          ifelse(local_lifeform %in% c("FORB", "LEGUME"), "Forb", local_lifeform)))),
         local_lifespan = ifelse(local_lifespan %in% c("BIENNIAL","PERENNIAL"), "Perennial",
                                 ifelse(local_lifespan == "UNK","NULL",local_lifespan))) %>% # local_lifeform consolidation
  filter(local_lifeform != "FERN") %>%
  filter(local_lifeform != "SUCCULENT") %>%
  filter(local_lifeform != "CACTUS"&local_lifeform!='NONVASCULAR') %>% 
  filter(local_lifeform != "NULL",
         local_lifespan != "NULL"&local_lifespan!="INDETERMINATE",
         ps_path != "NULL"&ps_path!='C3-C4 INTERMEDIATE') %>%
  mutate(N.fixer = ifelse(N.fixer == 0, "Non-N-fixer",
                          ifelse(N.fixer == 1, "N-fixer",NA)))
length(unique(datblip$site_code)) # 101 sites total

# # find dominant species in site (across all years)
# head(dom) # dominance classification dataset from Meghan H.
# unique(dom$dominance) # species are either dominant (D), codominant (CD), or subdominant (SD)
# 
# datblip <- datblip %>% # rejoin dominance information to OG dataframe
#   left_join(dom[,c(2:3,5)],by=c("site_code","Taxon")) %>% 
#   
# datblip$dominance[is.na(datblip$dominance)] <- "SD" # change NAs in dominance column subdominant
# 
# # Make sure we have the correct sites
# head(sites) # sites we are using for this analysis
# sites <- sites %>% # rename site column for consistency                    
#   dplyr::rename(site_code=sc)
# length(unique(sites$site_code)) # 77 sites total (correct)

# rejoin site info to dataframe
# datblip <- datblip %>% 
#   inner_join(., sites, by = "site_code") %>%
#   dplyr::filter(n_treat_years!=0.5&n_treat_years!=-1&n_treat_years!=4&n_treat_years!=5) %>% # why do we need this again?
#   filter(local_lifeform != "NULL",
#          local_lifespan != "NULL",
#          ps_path != "NULL") %>%
#   mutate(N.fixer = ifelse(N.fixer == 0, "Non-N-fixer",
#                           ifelse(N.fixer == 1, "N.fixer",NA)),
#          dominance = ifelse(dominance == "SD","Subdominant",
#                             ifelse(dominance == "D","Dominant",
#                                    ifelse(dominance == "CD", "Codominant",NA))))
# 
#   # ^^ MAYBE DON"T KEEP THIS
############# begin loop

################################################################################
# PART 6: count species gains and losses
################################################################################

datblip2<-datblip %>% 
  select(site_code, trt, block, plot, subplot, Taxon, local_lifeform, local_lifespan, N.fixer, ps_path, n_treat_years, max_cover) %>% 
  pivot_wider(names_from = n_treat_years, names_prefix = "y", values_from = max_cover, values_fill = 0) %>% 
  mutate(outcome=ifelse(y0>0 & y1>0 & y2>0 & y3>0, "persist", 
                 ifelse(y0>0&y1==0&y2==0&y3==0|y0>0&y1>0&y2==0&y3==0, "loss", 
                 ifelse(y0==0&y1>0&y2>0&y3>0|y0==0&y1==0&y2>0&y3>0, "gain","blip")))) %>% 
  mutate(loss=ifelse(outcome=='loss', 1, 0),
         gain=ifelse(outcome=="gain", 1, 0),
         persist=ifelse(outcome=='persist', 1, 0))


numloss<-sum(datblip2$loss)
numgain<-sum(datblip2$gain)
numpersist<-sum(datblip2$persist)

#now trying to to the binomial logistic regression
#focusing only on Losses for the poster

# gain_lifeform <- glmer(gain ~ trt*local_lifeform + (1|site_code), family = binomial(), data = datblip2)
# summary(gain_lifeform)
# anova(gain_lifeform)

loss_lifeform <- glmer(loss ~ trt*local_lifeform + (1|site_code), family = binomial(), data = datblip2)
summary(loss_lifeform)
Anova(loss_lifeform)
#there is no interaction in prob of loss between local lifeform and drought

emmeans(loss_lifeform, pairwise ~ local_lifeform|trt, type = 'response', adjust="Tukey")

lf<- data.frame(summary(emmeans(loss_lifeform, pairwise ~ trt*local_lifeform, type = 'response')$emmeans)) %>% 
  mutate(trait = "Lifeform") %>% 
  rename(TraitCat=local_lifeform)

loss_lifespan <- glmer(loss ~ trt*local_lifespan + (1|site_code), family = binomial(), data = datblip2)
summary(loss_lifespan)
Anova(loss_lifespan)

#there is an interaction between lifespan and loss by drought

emmeans(loss_lifespan, pairwise ~ local_lifespan|trt, type = 'response')

ls<- data.frame(summary(emmeans(loss_lifespan, pairwise ~ trt*local_lifespan, type = 'response')$emmeans)) %>% 
  mutate(trait = "Lifespan")%>% 
  rename(TraitCat=local_lifespan) %>% 
  mutate(TraitCat=ifelse(TraitCat=="ANNUAL", "Annual", 'Perennial'))

loss_Nfix <- glmer(loss ~ trt*N.fixer + (1|site_code), family = binomial(), data = datblip2)
summary(loss_Nfix)
Anova(loss_Nfix)

#there is not interaction between Nfixer and drought

emmeans(loss_Nfix, pairwise ~ N.fixer|trt, type = 'response')

nfix<- data.frame(summary(emmeans(loss_Nfix, pairwise ~ trt*N.fixer, type = 'response')$emmeans)) %>% 
  mutate(trait = "N_Fixer")%>% 
  rename(TraitCat=N.fixer)

loss_ps <- glmer(loss ~ trt*ps_path + (1|site_code), family = binomial(), data = datblip2)
summary(loss_ps)
Anova(loss_ps)

#there is no interaction between PS and drought

emmeans(loss_ps, pairwise ~ ps_path|trt, type = 'response')

ps<- data.frame(summary(emmeans(loss_ps, pairwise ~ trt*ps_path, type = 'response')$emmeans)) %>% 
  mutate(trait = "Photo_Path")%>% 
  rename(TraitCat=ps_path)


pvals<-data.frame(trait=c('Lifeform', 'Lifespan', 'N_Fixer', 'Photo_Path'), pval=c(0.355, 0.0002, 0.675, 0.689)) %>% 
  mutate(padj=paste("p = ", p.adjust(pval, method="BH")))

toplot<-lf %>% 
  bind_rows(ls, nfix, ps) %>%
  left_join(pvals)

loss<-
ggplot(data=toplot, aes(x=TraitCat, y=prob, color=trt, label=padj))+
  geom_point(size=3)+
  scale_color_manual(values=c("darkgreen", "darkorange"), name="")+
  geom_errorbar(aes(ymin=prob-SE, ymax=prob+SE), width=0.2)+
  geom_text(aes(x=Inf, y=Inf), hjust=1.05, vjust=1.2, color="black")+
  facet_wrap(~trait, scales = 'free')+
  ylab("Probability of Loss")+
  xlab("Trait")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "top")

ggsave("C:\\Users\\mavolio2\\Dropbox\\IDE (1)\\Papers\\Community-comp_change\\probloss.jpg", plot=loss, units = "in", width=6.5, height=5)



########MAGGIE'S CODE
sc<-length(datblip$site_code) # site codes for looping

blip_metrics_lifeform<-data.frame() # new data frame for lifeform responses
blip_metrics_lifespan<-data.frame() # new data frame for lifespan responses
blip_metrics_dominance<-data.frame() # new data frame for dominance responses
blip_metrics_Nfixing<-data.frame()  # new data frame for Nfixing responses
species_metrics <- data.frame() # dataframe on blip metrics for all species

for (i in 1:length(sc)){ # eea.br and cdpt_drt.us should not be included
  t <- datblip %>%
    filter(site_code == sc[i]) # subset for a given site
  print(sc[i]) # OR: unique(subset$site_code)

  # t <- datblip %>% # use this to test code below if something breaks
  #   filter(site_code == "bange.cn")
  
  # create dataframe (t1) which keeps track of gains, losses, persistence, and "blips"
  t1 <- t %>% 
    mutate(rep = rep(1)) %>% #create var. of repeating 1s
    pivot_wider(names_from = n_treat_years, values_from = rep) %>%
    dplyr::select(-c(X.y,X.1,plot,year,block,n_treat_days,subplot,functional_group,first_treatment_date,ppt.1,ppt.2,ppt.3,ppt.4,max_cover,cover_survey,relative_absolute,sampling_area_m2,cover_survey_comments,replicate)) %>% #redundant --remove this
    replace(is.na(.), 0) %>%
    group_by(site_code,trt,Family,Taxon,local_provenance,local_lifeform,local_lifespan,dominance,N.fixer,ps_path,map,habitat.type) %>%
    summarise_all(sum) %>% # summarise data
    mutate(yrspresent = sum(c_across(where(is.numeric)), na.rm = T), .after = habitat.type) %>% # number of years species is present
    mutate(outcome = ifelse(`0`>0&across(last_col())[[1]]>0,"persist", # create new gained/lost/persisted/blipped column based on indiv. species' presences across years
                            ifelse(`0`>0&across(last_col())[[1]]==0,"loss",
                                   ifelse(`0`==0&across(last_col())[[1]]>0,"gain",
                                          ifelse(`0`==0&across(last_col())[[1]]==0&yrspresent>0,"transient",NA))))) %>% # reference last column --no clue why this works
    dplyr::select(site_code,trt,Family,Taxon,local_provenance,local_lifeform,local_lifespan,dominance,N.fixer,ps_path,map,habitat.type,yrspresent,outcome)
  
  # create dataframes (t2, t3, & t4) with proportion of species that are gained/lost/etc.. Separate by lifeform, lifespan, and dominance
  # t2 = lifeform
  t2 <- t1 %>%
    mutate(rep = rep(1)) %>% # create var. of repeating 1s
    pivot_wider(names_from = outcome,values_from = rep) %>%
    replace(is.na(.), 0) %>% # replace NAs in new columns with 0s
    group_by(site_code,trt,local_lifeform,map,habitat.type) %>%
    dplyr::select(-c(Family,Taxon,local_provenance,dominance,local_lifespan,N.fixer,ps_path,yrspresent)) %>% # left out grouping vars: trt,plot,Family,Taxon,local_provenance,N.fixer,ps_path,yrspresent
    summarise_all(sum) %>% # summarise data to get total number of species for each outcome
    mutate(loss = ifelse("loss" %in% names(.), loss, NA), # in case we have no species doing any of these things: this asks if a column exists and creates a column of NAs with the same name if it doesn't
           blip = ifelse("blip" %in% names(.), blip, NA),
           gain = ifelse("gain" %in% names(.), gain, NA),
           persist = ifelse("persist" %in% names(.), persist, NA)) %>%
    replace(is.na(.), 0) %>%
    dplyr::select(site_code,trt,local_lifeform,map,habitat.type,gain,loss,blip,persist) %>% # reorder vars for consistency 
    pivot_longer(cols = gain:persist, names_to = "outcome", values_to = "SpCount") %>%
    filter(SpCount>0) %>% # get rid of rows where we don't have species data
    mutate(prop = SpCount/sum(SpCount)) 
  # propResponse: positive values indicates the difference in the proportion of plants within each group which experienced each type of event.
      # ^^ Positive values: drought plots experienced more events in a group compared to control plots
      # ^^ Negative values: control plots experienced more events in a group compared to drought plots
  
  # t3 = lifespan (repeat)
  t3 <- t1 %>%
    mutate(rep = rep(1)) %>% 
    pivot_wider(names_from = outcome,values_from = rep) %>%
    replace(is.na(.), 0) %>% 
    group_by(site_code,trt,local_lifespan,map,habitat.type) %>%
    dplyr::select(-c(Family,Taxon,local_provenance,local_lifeform,dominance,N.fixer,ps_path,yrspresent)) %>% 
    summarise_all(sum) %>% 
    mutate(loss = ifelse("loss" %in% names(.), loss, NA), 
           blip = ifelse("blip" %in% names(.), blip, NA),
           gain = ifelse("gain" %in% names(.), gain, NA),
           persist = ifelse("persist" %in% names(.), persist, NA)) %>%
    replace(is.na(.), 0) %>%
    dplyr::select(site_code,trt,local_lifespan,map,habitat.type,gain,loss,blip,persist) %>% 
    pivot_longer(cols = gain:persist, names_to = "outcome", values_to = "SpCount") %>%
    filter(SpCount>0) %>% 
    mutate(prop = SpCount/sum(SpCount)) 
  
  # t4 = dominance (repeat)
  t4 <- t1 %>%
    mutate(rep = rep(1)) %>% 
    pivot_wider(names_from = outcome,values_from = rep) %>%
    replace(is.na(.), 0) %>% 
    group_by(site_code,trt,dominance,map,habitat.type) %>%
    dplyr::select(-c(Family,Taxon,local_provenance,local_lifespan,local_lifeform,N.fixer,ps_path,yrspresent)) %>% 
    summarise_all(sum) %>% 
    mutate(loss = ifelse("loss" %in% names(.), loss, NA), 
           blip = ifelse("blip" %in% names(.), blip, NA),
           gain = ifelse("gain" %in% names(.), gain, NA),
           persist = ifelse("persist" %in% names(.), persist, NA)) %>%
    replace(is.na(.), 0) %>%
    dplyr::select(site_code,trt,dominance,map,habitat.type,gain,loss,blip,persist) %>% 
    pivot_longer(cols = gain:persist, names_to = "outcome", values_to = "SpCount") %>%
    filter(SpCount>0) %>% 
    mutate(prop = SpCount/sum(SpCount)) 
  
  # t5 = N-fixing 
  t5 <- t1 %>%
    mutate(rep = rep(1)) %>% 
    pivot_wider(names_from = outcome,values_from = rep) %>%
    replace(is.na(.), 0) %>%
    group_by(site_code,trt,N.fixer,map,habitat.type) %>%
    dplyr::select(-c(Family,Taxon,local_provenance,local_lifespan,local_lifeform,dominance,ps_path,yrspresent)) %>% 
    summarise_all(sum) %>% 
    mutate(loss = ifelse("loss" %in% names(.), loss, NA), 
           blip = ifelse("blip" %in% names(.), blip, NA),
           gain = ifelse("gain" %in% names(.), gain, NA),
           persist = ifelse("persist" %in% names(.), persist, NA)) %>%
    replace(is.na(.), 0) %>%
    dplyr::select(site_code,trt,N.fixer,map,habitat.type,gain,loss,blip,persist) %>%  
    pivot_longer(cols = gain:persist, names_to = "outcome", values_to = "SpCount") %>%
    filter(SpCount>0) %>% 
    mutate(prop = SpCount/sum(SpCount)) 
  
  # species metrics
  species_metrics<-species_metrics %>% 
    bind_rows(t1)
  
  # lifeform dataframe
  blip_metrics_lifeform<-blip_metrics_lifeform %>% 
    bind_rows(t2) 
  
  # lifespan dataframe
  blip_metrics_lifespan<-blip_metrics_lifespan %>% 
    bind_rows(t3)
  
  # dominance dataframe
  blip_metrics_dominance<-blip_metrics_dominance %>% 
    bind_rows(t4)
  
  # Nfixing dataframe
  blip_metrics_Nfixing<-blip_metrics_Nfixing %>% 
    bind_rows(t5)
}


################################################################################
# PART 7: prep data for binomial logistic regression
################################################################################

# join species_metrics to pca dataset
# clean data a bit
species_metrics <- species_metrics %>%
  filter(.,!grepl("NONVASCULAR",local_lifeform)) %>%
  filter(.,!grepl("C3-C4",ps_path))

# binomial: loss only
blr.l <- species_metrics %>%
  mutate(spp_site = paste(Taxon,site_code,sep = "_")) %>%
  left_join(.,pca, by = "spp_site") %>%
  mutate(outcome = as.factor(outcome)) %>%
  mutate(loss = ifelse(outcome == "loss",1,0))
# binomial: gain only
blr.g <- species_metrics %>%
  mutate(spp_site = paste(Taxon,site_code,sep = "_")) %>%
  left_join(.,pca, by = "spp_site") %>%
  mutate(outcome = as.factor(outcome)) %>%
  mutate(gain = ifelse(outcome == "gain",1,0))
# binomial: persist only
blr.p <- species_metrics %>%
  mutate(spp_site = paste(Taxon,site_code,sep = "_")) %>%
  left_join(.,pca, by = "spp_site") %>%
  mutate(outcome = as.factor(outcome)) %>%
  mutate(persist = ifelse(outcome == "persist",1,0))


################################################################################
# PART 8: binomial logistic regression to predict outcome from traits
################################################################################
    library("foreign")
    library(sjPlot) # .html tables
# some notes on interpreting binomial logistic models
    # If a categorical predictor is significant, we can conclude that not all of the levels of the factor have the same probability
    # The "z-value" for the statistical (summary()) output is the test-statistic for the Wald-test that the parameter is 0

# binomial logistic regressions (trait categories)
    coefs <- data.frame() # coefficients for plots
    coefs2 <- data.frame() # model output coefficients for table
    # drop null categories
    # only include species with complete cases?????????????? Ask Meghan Avolio
    # loss
    # ISSUE: df output is given as "Inf", but this is maybe okay...?
        # Explanation: https://stackoverflow.com/questions/73536308/how-to-get-emmeans-to-print-degrees-of-freedom-for-glmer-class

    # lifeform
    {# gain
      gain_lifeform <- glmer(gain ~ trt*local_lifeform + (1|site_code), family = binomial(), data = blr.g)
          dfs <- data.frame(summary(emmeans(gain_lifeform, pairwise ~ trt*local_lifeform, type = 'response')$contrast)) #data.frame(summary(gain_lifeform)$coefficients)
          dfs <- dfs %>% mutate(outcome = rep("gain"),trait = rep("lifeform")) # exclueded traitCategory = rownames(.)
          df <- data.frame(summary(emmeans(gain_lifeform, pairwise ~ trt*local_lifeform, type = 'response')$emmeans))
          df <- df %>%
            mutate(outcome = rep("gain"),
                   trait = rep("lifeform")) %>%
            rename_with(.cols = 2, ~"traitCategory") %>%
            mutate(traitCategory = as.factor(traitCategory))
          coefs<-df %>%
            bind_rows(df)
          coefs2<-dfs%>%
            bind_rows(dfs)
      # loss
      loss_lifeform <- glmer(loss ~ trt*local_lifeform + (1|site_code), family = binomial(), data = blr.l)
            dfs <- data.frame(summary(emmeans(loss_lifeform, pairwise ~ trt*local_lifeform, type = 'response')$contrast)) #data.frame(summary(loss_lifeform)$coefficients)
            dfs <- dfs %>% mutate(outcome = rep("loss"),trait = rep("lifeform"))
            df <- data.frame(summary(emmeans(loss_lifeform, pairwise ~ trt*local_lifeform, type = 'response')$emmeans))
            df <- df %>%
              mutate(outcome = rep("loss"),
                     trait = rep("lifeform")) %>%
              rename_with(.cols = 2, ~"traitCategory") %>%
              mutate(traitCategory = as.factor(traitCategory))
            coefs<-coefs %>%
              bind_rows(df)
            coefs2<-coefs2%>%
              bind_rows(dfs)
    }
    
    # lifespan
    {# gain
        gain_lifespan <- glmer(gain ~ trt*local_lifespan + (1|site_code), family = binomial(), data = blr.g)
            dfs <- data.frame(summary(emmeans(gain_lifespan, pairwise ~ trt*local_lifespan, type = 'response')$contrast))
            dfs <- dfs %>% mutate(outcome = rep("gain"),trait = rep("lifespan"))
            df <- data.frame(summary(emmeans(gain_lifespan, pairwise ~ trt*local_lifespan, type = 'response')$emmeans))
            df <- df %>%
              mutate(outcome = rep("gain"),
                     trait = rep("lifespan")) %>%
              rename_with(.cols = 2, ~"traitCategory") %>%
              mutate(traitCategory = as.factor(traitCategory))
            coefs<-coefs %>%
              bind_rows(df)
            coefs2<-coefs2%>%
              bind_rows(dfs)
            
        # loss
        loss_lifespan <- glmer(loss ~ trt*local_lifespan + (1|site_code), family = binomial(), data = blr.l)
            dfs <- data.frame(summary(emmeans(loss_lifespan, pairwise ~ trt*local_lifespan, type = 'response')$contrast))
            dfs <- dfs %>% mutate(outcome = rep("loss"),trait = rep("lifespan"))
            df <- data.frame(summary(emmeans(loss_lifespan, pairwise ~ trt*local_lifespan, type = 'response')$emmeans))
            df <- df %>%
              mutate(outcome = rep("loss"),
                     trait = rep("lifespan")) %>%
              rename_with(.cols = 2, ~"traitCategory") %>%
              mutate(traitCategory = as.factor(traitCategory))
            coefs<-coefs %>% # note that this is different from previous iteration
              bind_rows(df)
            coefs2<-coefs2%>%
              bind_rows(dfs)
            }
        
    # N.fixer
    {# gain
      gain_N.fixer <- glmer(gain ~ trt*N.fixer + (1|site_code), family = binomial(), data = blr.g)
      dfs <- data.frame(summary(emmeans(gain_N.fixer, pairwise ~ trt*N.fixer, type = 'response')$contrast))
      dfs <- dfs %>% mutate(outcome = rep("gain"),trait = rep("N.fixer"))
      df <- data.frame(summary(emmeans(gain_N.fixer, pairwise ~ trt*N.fixer, type = 'response')$emmeans))
      df <- df %>%
        mutate(outcome = rep("gain"),
               trait = rep("N.fixer")) %>%
        rename_with(.cols = 2, ~"traitCategory") %>%
        mutate(traitCategory = as.factor(traitCategory))
      coefs<-coefs %>%
        bind_rows(df)
      coefs2<-coefs2%>%
        bind_rows(dfs)
      
      # loss
      loss_N.fixer <- glmer(loss ~ trt*N.fixer + (1|site_code), family = binomial(), data = blr.l)
          dfs <- data.frame(summary(emmeans(loss_N.fixer, pairwise ~ trt*N.fixer, type = 'response')$contrast))
          dfs <- dfs %>% mutate(outcome = rep("loss"),trait = rep("N.fixer"))
          df <- data.frame(summary(emmeans(loss_N.fixer, pairwise ~ trt*N.fixer, type = 'response')$emmeans))
          df <- df %>%
            mutate(outcome = rep("loss"),
                   trait = rep("N.fixer")) %>%
            rename_with(.cols = 2, ~"traitCategory") %>%
            mutate(traitCategory = as.factor(traitCategory))
          coefs<-coefs %>%
            bind_rows(df)
          coefs2<-coefs2%>%
            bind_rows(dfs)
      }
            
    # ps_path
    {# gain
      gain_ps_path <- glmer(gain ~ trt*ps_path + (1|site_code), family = binomial(), data = blr.g)
          dfs <- data.frame(summary(emmeans(gain_ps_path, pairwise ~ trt*ps_path, type = 'response')$contrast))
          dfs <- dfs %>% mutate(outcome = rep("gain"),trait = rep("ps_path"))
          df <- data.frame(summary(emmeans(gain_ps_path, pairwise ~ trt*ps_path, type = 'response')$emmeans))
          df <- df %>%
            mutate(outcome = rep("gain"),
                   trait = rep("ps_path")) %>%
            rename_with(.cols = 2, ~"traitCategory") %>%
            mutate(traitCategory = as.factor(traitCategory))
          coefs<-coefs %>%
            bind_rows(df)
          coefs2<-coefs2%>%
            bind_rows(dfs)
      
     # loss
      loss_ps_path <- glmer(loss ~ trt*ps_path + (1|site_code), family = binomial(), data = blr.l)
          dfs <- data.frame(summary(emmeans(loss_ps_path, pairwise ~ trt*ps_path, type = 'response')$contrast))
          dfs <- dfs %>% mutate(outcome = rep("loss"),trait = rep("ps_path"))
          df <- data.frame(summary(emmeans(loss_ps_path, pairwise ~ trt*ps_path, type = 'response')$emmeans))
          df <- df %>%
            mutate(outcome = rep("loss"),
                   trait = rep("ps_path")) %>%
            rename_with(.cols = 2, ~"traitCategory") %>%
            mutate(traitCategory = as.factor(traitCategory))
          coefs<-coefs %>%
            bind_rows(df)
          coefs2<-coefs2%>%
            bind_rows(dfs)
    }
        
    # dominance
    {# gain
      gain_dominance <- glmer(gain ~ trt*dominance + (1|site_code), family = binomial(), data = blr.g)
          dfs <- data.frame(summary(emmeans(gain_dominance, pairwise ~ trt*dominance, type = 'response')$contrast))
          dfs <- dfs %>% mutate(outcome = rep("gain"),trait = rep("dominance"))
          df <- data.frame(summary(emmeans(gain_dominance, pairwise ~ trt*dominance, type = 'response')$emmeans))
          df <- df %>%
            mutate(outcome = rep("gain"),
                   trait = rep("dominance")) %>%
            rename_with(.cols = 2, ~"traitCategory") %>%
            mutate(traitCategory = as.factor(traitCategory))
          coefs<-coefs %>%
            bind_rows(df)
          coefs2<-coefs2%>%
            bind_rows(dfs) 
          
     # loss
      loss_dominance <- glmer(loss ~ trt*dominance + (1|site_code), family = binomial(), data = blr.l)
          dfs <- data.frame(summary(emmeans(loss_dominance, pairwise ~ trt*dominance, type = 'response')$contrast))
          dfs <- dfs %>% mutate(outcome = rep("loss"),trait = rep("dominance"))
          df <- data.frame(summary(emmeans(loss_dominance, pairwise ~ trt*dominance, type = 'response')$emmeans))
          df <- df %>%
            mutate(outcome = rep("loss"),
                   trait = rep("dominance")) %>%
            rename_with(.cols = 2, ~"traitCategory") %>%
            mutate(traitCategory = as.factor(traitCategory))
          coefs<-coefs %>%
            bind_rows(df)
          coefs2<-coefs2%>%
            bind_rows(dfs)
      }

    # persist (helpful, but currently not in use)
    {   # lifeform
      persist_lifeform <- glmer(persist ~ trt*local_lifeform + (1|site_code), family = binomial(), data = blr.p)
      dfs <- data.frame(summary(emmeans(persist_lifeform, pairwise ~ trt*lifeform, type = 'response')$contrast))
      dfs <- dfs %>% mutate(outcome = rep("persist"),trait = rep("lifeform"))
      df <- data.frame(summary(emmeans(persist_lifeform, pairwise ~ trt*local_lifeform, type = 'response')$emmeans))
      df <- df %>%
        mutate(outcome = rep("persist"),
               trait = rep("lifeform")) %>%
        rename_with(.cols = 2, ~"traitCategory") %>%
        mutate(traitCategory = as.factor(traitCategory))
      coefs<-coefs %>%
        bind_rows(df)
      coefs2<-coefs2%>%
        bind_rows(dfs)
      # lifespan
      persist_lifespan <- glmer(persist ~ trt*local_lifespan + (1|site_code), family = binomial(), data = blr.p)
      dfs <- data.frame(summary(emmeans(persist_lifespan, pairwise ~ trt*lifespan, type = 'response')$contrast))
      dfs <- dfs %>% mutate(outcome = rep("persist"),trait = rep("lifespan"))
      df <- data.frame(summary(emmeans(persist_lifespan, pairwise ~ trt*local_lifespan, type = 'response')$emmeans))
      df <- df %>%
        mutate(outcome = rep("persist"),
               trait = rep("lifespan")) %>%
        rename_with(.cols = 2, ~"traitCategory") %>%
        mutate(traitCategory = as.factor(traitCategory))
      coefs<-coefs %>%
        bind_rows(df)
      coefs2<-coefs2%>%
        bind_rows(dfs)
      # N.fixer
      persist_N.fixer <- glmer(persist ~ trt*N.fixer + (1|site_code), family = binomial(), data = blr.p)
      dfs <- data.frame(summary(emmeans(persist_N.fixer, pairwise ~ trt*N.fixer, type = 'response')$contrast))
      dfs <- dfs %>% mutate(outcome = rep("persist"),trait = rep("N.fixer"))
      df <- data.frame(summary(emmeans(persist_N.fixer, pairwise ~ trt*N.fixer, type = 'response')$emmeans))
      df <- df %>%
        mutate(outcome = rep("persist"),
               trait = rep("N.fixer")) %>%
        rename_with(.cols = 2, ~"traitCategory") %>%
        mutate(traitCategory = as.factor(traitCategory))
      coefs<-coefs %>%
        bind_rows(df)
      coefs2<-coefs2%>%
        bind_rows(dfs)
      # ps_path
      persist_ps_path <- glmer(persist ~ trt*ps_path + (1|site_code), family = binomial(), data = blr.p)
      dfs <- data.frame(summary(emmeans(persist_ps_path, pairwise ~ trt*ps_path, type = 'response')$contrast))
      dfs <- dfs %>% mutate(outcome = rep("persist"),trait = rep("ps_path"))
      df <- data.frame(summary(emmeans(persist_ps_path, pairwise ~ trt*ps_path, type = 'response')$emmeans))
      df <- df %>%
        mutate(outcome = rep("persist"),
               trait = rep("ps_path")) %>%
        rename_with(.cols = 2, ~"traitCategory") %>%
        mutate(traitCategory = as.factor(traitCategory))
      coefs<-coefs %>%
        bind_rows(df)
      coefs2<-coefs2%>%
        bind_rows(dfs)
      # dominance
      persist_dominance <- glmer(persist ~ trt*dominance + (1|site_code), family = binomial(), data = blr.p)
      dfs <- data.frame(summary(emmeans(persist_dominance, pairwise ~ trt*dominance, type = 'response')$contrast))
      dfs <- dfs %>% mutate(outcome = rep("persist"),trait = rep("dominance"))
      df <- data.frame(summary(emmeans(persist_dominance, pairwise ~ trt*dominance, type = 'response')$emmeans))
      df <- df %>%
        mutate(outcome = rep("persist"),
               trait = rep("dominance")) %>%
        rename_with(.cols = 2, ~"traitCategory") %>%
        mutate(traitCategory = as.factor(traitCategory))
      coefs<-coefs %>%
        bind_rows(df)
      coefs2<-coefs2%>%
        bind_rows(dfs)
    }
    
# ammend coefs dataframe (cor gains and losses only)
    coefs2 <- coefs2 %>%
      mutate(sig = ifelse(p.value<0.001,"***",
                          ifelse(p.value<0.01,"**",
                                 ifelse(p.value<0.05,"*",
                                        ifelse(p.value<0.1,"."," "))))) %>%
      dplyr::select(trait,outcome,contrast,odds.ratio,SE,df,null,z.ratio,p.value,sig) %>% # reorder some columns
      dplyr::rename(Trait=trait,Outcome=outcome,`Contrast`=contrast,`Odds ratio`=odds.ratio,`Std. Error`=SE,`z-ratio`=z.ratio,`Pr(>|z|)`=p.value,` `=sig) %>%
      remove_rownames
# write coefficients output as a .csv file
    getwd()
    # main statistical output (Table 2)
    write.csv(coefs2,"glmerCoefs_2023-08-01.csv")
    # output used for generating Figure 2
    write.csv(coefs,"plotCoefs_2023-08-01.csv")
    
head(coefs2)

  
    # plots for binomial logistic regressions (trait categories)
      {  # plot - lifeform
        a<-coefs %>%
          filter(outcome != "persist") %>%
          filter(trait == "lifeform") %>%
          ggplot(., aes(x=traitCategory, y=prob,col=trt)) +
            geom_point(position = position_dodge(width=0.25))+
            theme_bw() +
            scale_color_manual(name="Treatment",
                               values=c("mediumseagreen","rosybrown4")) +
            geom_errorbar(aes(ymin=prob-SE,ymax=prob+SE,width=0),position = position_dodge(width=0.25)) +
            theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
            theme(strip.background =element_rect(fill="white"))+
            facet_grid(~outcome) +
            ylab("Probability") + xlab("") +ggtitle("Lifeform");a
          
          # plot - lifespan
        b<-coefs %>%
            filter(outcome != "persist") %>%
            filter(trait == "lifespan") %>%
            ggplot(., aes(x=traitCategory, y=prob,col=trt)) +
            geom_point(position = position_dodge(width=0.25))+
            theme_bw() +
            scale_color_manual(name="Treatment",
                               values=c("mediumseagreen","rosybrown4")) +
            geom_errorbar(aes(ymin=prob-SE,ymax=prob+SE,width=0),position = position_dodge(width=0.25)) +
            theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
            theme(strip.background =element_rect(fill="white"))+
            facet_grid(~outcome) +
            ylab("") + xlab("") +ggtitle("Lifespan");b
          
          # plot - N.fixer
        c<-coefs %>%
            filter(outcome != "persist") %>%
            filter(trait == "N.fixer") %>%
            ggplot(., aes(x=traitCategory, y=prob,col=trt)) +
            geom_point(position = position_dodge(width=0.25))+
            theme_bw() +
            scale_color_manual(name="Treatment",
                               values=c("mediumseagreen","rosybrown4")) +
            geom_errorbar(aes(ymin=prob-SE,ymax=prob+SE,width=0),position = position_dodge(width=0.25)) +
            theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
            theme(strip.background =element_rect(fill="white"))+
            facet_grid(~outcome) +
            ylab("") + xlab("") +ggtitle("N-fixer")
        
          # plot - N.fixer
        d<-coefs %>%
          filter(outcome != "persist") %>%
          filter(trait == "ps_path") %>%
          ggplot(., aes(x=traitCategory, y=prob,col=trt)) +
          geom_point(position = position_dodge(width=0.25))+
          theme_bw() +
          scale_color_manual(name="Treatment",
                             values=c("mediumseagreen","rosybrown4")) +
          geom_errorbar(aes(ymin=prob-SE,ymax=prob+SE,width=0),position = position_dodge(width=0.25)) +
          theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
          theme(strip.background =element_rect(fill="white"))+
          facet_grid(~outcome) +
          ylab("Probability") + xlab("") +ggtitle("Photosynthetic pathway")
          
          # plot - dominance
        e<-coefs %>%
            filter(outcome != "persist") %>%
            filter(trait == "dominance") %>%
            ggplot(., aes(x=traitCategory, y=prob,col=trt)) +
            geom_point(position = position_dodge(width=0.25))+
            theme_bw() +
            scale_color_manual(name="Treatment",
                               values=c("mediumseagreen","rosybrown4")) +
            geom_errorbar(aes(ymin=prob-SE,ymax=prob+SE,width=0),position = position_dodge(width=0.25)) +
            theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
            theme(strip.background =element_rect(fill="white"))+
            facet_grid(~outcome) +
            ylab("") + xlab("") +ggtitle("Dominance");e
        #legend only
        # Extract the legend. Returns a gtable
        leg <- as_ggplot(get_legend(e))
        ggarrange(a,b,c,d,e,leg,common.legend = T,legend = F)
        }
  
# binomial logistic regressions (PCA axes)
    coefs <- data.frame()
    # drop null categories
    # only include species with complete cases?????????????? Ask Meghan Avolio
    # loss
    {
      # pca axes
      lossPCA<- glmer(loss ~ trt+PC1+PC2+PC3 + (1|site_code), family = binomial(), data = blr.l)
      df <- data.frame(summary(emmeans(lossPCA, pairwise ~ trt*PC1, type = 'response')$emmeans))
      df <- df %>%
        mutate(outcome = rep("loss"),
               trait = rep("PCA")) %>%
        rename_with(.cols = 2, ~"traitCategory") %>%
        mutate(traitCategory = as.factor(traitCategory))
      coefs<-df %>%
        bind_rows(df)
      # lifeform
      loss_lifeform <- glmer(loss ~ trt*local_lifeform + (1|site_code), family = binomial(), data = blr.l)
      df <- data.frame(summary(emmeans(loss_lifeform, pairwise ~ trt*local_lifeform, type = 'response')$emmeans))
      df <- df %>%
        mutate(outcome = rep("loss"),
               trait = rep("lifeform")) %>%
        rename_with(.cols = 2, ~"traitCategory") %>%
        mutate(traitCategory = as.factor(traitCategory))
      coefs<-coefs %>%
        bind_rows(df)
      # lifespan
      loss_lifespan <- glmer(loss ~ trt*local_lifespan + (1|site_code), family = binomial(), data = blr.l)
      df <- data.frame(summary(emmeans(loss_lifespan, pairwise ~ trt*local_lifespan, type = 'response')$emmeans))
      df <- df %>%
        mutate(outcome = rep("loss"),
               trait = rep("lifespan")) %>%
        rename_with(.cols = 2, ~"traitCategory") %>%
        mutate(traitCategory = as.factor(traitCategory))
      coefs<-coefs %>%
        bind_rows(df)
      # N.fixer
      loss_N.fixer <- glmer(loss ~ trt*N.fixer + (1|site_code), family = binomial(), data = blr.l)
      df <- data.frame(summary(emmeans(loss_N.fixer, pairwise ~ trt*N.fixer, type = 'response')$emmeans))
      df <- df %>%
        mutate(outcome = rep("loss"),
               trait = rep("N.fixer")) %>%
        rename_with(.cols = 2, ~"traitCategory") %>%
        mutate(traitCategory = as.factor(traitCategory))
      coefs<-coefs %>%
        bind_rows(df)
      # ps_path
      loss_ps_path <- glmer(loss ~ trt*ps_path + (1|site_code), family = binomial(), data = blr.l)
      df <- data.frame(summary(emmeans(loss_ps_path, pairwise ~ trt*ps_path, type = 'response')$emmeans))
      df <- df %>%
        mutate(outcome = rep("loss"),
               trait = rep("ps_path")) %>%
        rename_with(.cols = 2, ~"traitCategory") %>%
        mutate(traitCategory = as.factor(traitCategory)) 
      coefs<-coefs %>%
        bind_rows(df)
      # dominance
      loss_dominance <- glmer(loss ~ trt*dominance + (1|site_code), family = binomial(), data = blr.l)
      df <- data.frame(summary(emmeans(loss_dominance, pairwise ~ trt*dominance, type = 'response')$emmeans))
      df <- df %>%
        mutate(outcome = rep("loss"),
               trait = rep("dominance")) %>%
        rename_with(.cols = 2, ~"traitCategory") %>%
        mutate(traitCategory = as.factor(traitCategory))
      coefs<-coefs %>%
        bind_rows(df)
      }

# SIMPER analysis?
    df.simp <- species_metrics %>%
      dplyr::select(site_code,trt,Taxon,local_lifeform,local_lifespan,dominance,N.fixer,ps_path) %>%
      mutate(Count = rep(1)) %>%
      filter(trt == "Drought") %>%
      group_by(site_code,trt,local_lifeform,local_lifespan,dominance,N.fixer,ps_path) %>%
      dplyr::select(-c(Taxon)) %>%
      summarise_all(sum)
    
    env <- distinct(dat[,c(2,30)])
    
    
    # lifeform
    df.simp.lifeform <- df.simp[,c(1,3,8)] 
    df.simp.lifeform <- df.simp.lifeform %>%
      group_by(site_code,local_lifeform) %>%
      summarise_all(sum) %>%
      pivot_wider(names_from = local_lifeform,values_from = Count) %>%
      replace(is.na(.), 0) %>%
      remove_rownames %>% column_to_rownames(var="site_code")
      
      sim <- with(env, simper(df.simp.lifeform, map,permutations = 99))
      
    data(dune)
    data(dune.env)
    (sim <- with(dune.env, simper(dune, Management, permutations = 99)))
    ## IGNORE_RDIFF_BEGIN
    summary(sim)
    


# Statistical analyses of difference --------------------------------------
#############doing statistical tests of this
rich<-difflong %>% 
  filter(measure=="richness_diff")
t.test(rich$value, mu=0, alternative = "two.sided")
#this is sig.
even<-difflong %>% 
  filter(measure=="evenness_diff")
t.test(even$value, mu=0, alternative = "two.sided")
#this is NOT sig.
rank<-difflong %>% 
  filter(measure=="rank_diff")
t.test(rank$value, mu=0, alternative = "two.sided")
#this is sig.
sp<-difflong %>% 
  filter(measure=="species_diff")
t.test(sp$value, mu=0, alternative = "two.sided")
#this is sig.

comp<-difflong %>% 
  filter(measure=="composition_diff")
t.test(comp$value, mu=0, alternative = "two.sided")
#this is sig.

rd<-lm(value~drtseverity+map+PctGrass+PctAnnual, data=subset(difflong, measure=="richness_diff"))
summary(rd)

ggplot(data=subset(difflong, measure=="richness_diff"), aes(x=map, y=value))+
  geom_point()+
  ylab("Richness Diff")+
  geom_hline(yintercept=0)+
  geom_smooth(method="lm", color="black")+
  xlab("MAP")+
  annotate("text", x=-Inf, y=-Inf, vjust=-1, hjust=-1 ,label="p < 0.01")

ed<-lm(value~drtseverity+map+PctGrass+PctAnnual, data=subset(difflong, measure=="evenness_diff"))
summary(ed)
#pct grass

ggplot(data=subset(difflong, measure=="evenness_diff"), aes(x=PctGrass, y=value))+
  geom_point()+
  ylab("Evenness Diff")+
  geom_hline(yintercept=0)+
  geom_smooth(method="lm", color="black")+
  xlab("% Grass")+
  annotate("text", x=-Inf, y=-Inf, vjust=-1, hjust=-1 ,label="p < 0.01")

ra<-lm(value~drtseverity+map+PctGrass+PctAnnual, data=subset(difflong, measure=="rank_diff"))
summary(ra)
#not sig

spd<-lm(value~drtseverity+map+PctGrass+PctAnnual, data=subset(difflong, measure=="species_diff"))
summary(spd)
#sig


cd<-lm(value~drtseverity+map+PctGrass+PctAnnual, data=subset(difflong, measure=="composition_diff"))
summary(cd)

ggplot(data=subset(difflong, measure=="composition_diff"), aes(x=PctAnnual, y=value))+
  geom_point()+
  geom_smooth(method = "lm", color="black")+
  ylab("Composition Diff")+
  xlab("% Annual")

# Permanova ---------------------------------------------------------------

site_vec <- unique(datall$site_code)

permanova_out_master <- {}

for(i in 1:length(site_vec)) {
  ide_temp <- datall %>% 
    filter(site_code==site_vec[i]) %>% 
    select(site_code, trt, Taxon, max_cover, replicate) %>% 
    pivot_wider(names_from = Taxon, values_from = max_cover, values_fill = 0)
  
  permanova_temp <- adonis2(ide_temp[4:ncol(ide_temp)] ~ trt, data=ide_temp, permutations=99)
  
  perm_out_temp <- data.frame(
    site_code = site_vec[i],
    perm_Pvalue =  permanova_temp$'Pr(>F)'[1]
  )
  
  permanova_out_master <- rbind(permanova_out_master, perm_out_temp)
  
}




# Getting measures of change ----------------------------------------------
#dropping datasets without pretreatment
drop_no_pretrt<-dat %>% 
  select(site_code, n_treat_years) %>% 
  unique() %>% 
  filter(n_treat_years==0) %>% 
  select(-n_treat_years)

dat2<-dat %>% 
  right_join(drop_no_pretrt) %>% 
  mutate(rep=paste(site_code, replicate, sep=";")) %>% 
  filter(n_treat_years!=0.5&n_treat_years!=-1)

###looping through site for changes with pretreatmetn as a reference year

#having problems with eea.br, dropping this. not sure why
dat3<-dat2 %>% 
  filter(site_code!="eea.br")

sc<-unique(dat3$site_code)

deltamult<-data.frame()
deltaracs<-data.frame()

for (i in 1:length(sc)){
  
  subset<-dat3%>%
    filter(site_code==sc[i])
  
  pretrt<-unique(filter(subset, n_treat_years==0)$year)
  scode<-unique(subset$site_code)
  
  treats<-subset %>% 
    select(replicate, site_code, trt) %>% 
    unique()
  
  nyear<-subset %>% 
    select(year, n_treat_years) %>% 
    unique() %>% 
    rename(year2=year)
  
  change_mult<-multivariate_change(df=subset, time.var="year", species.var = "Taxon", abundance.var = "max_cover", replicate.var="replicate", treatment.var="trt", reference.time =  as.integer(pretrt)) %>% 
    select(-year) %>% 
    left_join(nyear) %>% 
    rename(year=year2) %>% 
    mutate(site_code=scode)
  
  deltamult<-deltamult %>% 
    bind_rows(change_mult)
  
  change_ranks<-RAC_change(df=subset, time.var="year", species.var = "Taxon", abundance.var = "max_cover", replicate.var="replicate", reference.time = as.integer(pretrt)) %>% 
    left_join(treats) %>% 
    left_join(nyear) %>% 
    select(-year) %>% 
    rename(year=year2)
  
  deltaracs<-deltaracs %>% 
    bind_rows(change_ranks)
}


##looking at data a bit
years<-deltamult %>% 
  select(site_code, year, n_treat_years) %>% 
  unique()

dom<-read.csv("BP_index_change.csv") %>% 
  select(-X, -plot) %>% 
  rename(replicate=rep, value=BP_index_change) %>% 
  mutate(measure="dominance") %>% 
  left_join(years)

RRMult<-deltamult %>% 
  pivot_longer(names_to="measure", values_to = "value", composition_change:dispersion_change) %>%
  pivot_wider(names_from = "trt", values_from = "value") %>% 
  mutate(RR=Drought-Control) %>% 
  select(-Drought, -Control, -n_treat_years)

chgRaclong<-deltaracs %>% 
  pivot_longer(names_to="measure", values_to = "value", richness_change:losses) %>% 
  bind_rows(dom)

chgRacMeans<-chgRaclong%>% 
  group_by(site_code, year, trt, measure) %>% 
  summarize(mean=mean(abs(value))) %>% 
  pivot_wider(names_from="trt", values_from = "mean")

ChgCntlSD<-chgRaclong %>% 
  filter(trt=="Control") %>% 
  group_by(site_code, year, measure) %>% 
  summarize(cntSD=sd(value))

GlassD<-chgRacMeans %>% 
  left_join(ChgCntlSD) %>% 
  mutate(RR=(Drought-Control)/cntSD) %>% 
  filter(RR!=Inf&RR!=-Inf) %>% 
  select(-Drought, -Control) %>% 
  bind_rows(RRMult) %>% 
  left_join(drt) %>% 
  filter(n_treat_years<4)


ggplot(data = subset(GlassD, -20<RR&RR<20), aes(x=measure, y=RR))+
  geom_boxplot(aes(group=measure))+
  scale_x_discrete(limits=c("richness_change", "evenness_change", "dominance", 'rank_change', 'gains', 'losses', 'composition_change', 'dispersion_change'), labels=c("Richness", "Evenness", "Dominance", "Ranks", "Gains", 'Losses', "Composition", "Dispersion"))+
  geom_hline(yintercept = 0)+
  annotate("text", x=2, y=10, label="*", size=8, color="red")+
  annotate("text", x=5, y=12, label="*", size=8, color="red")+
  annotate("text", x=6, y=16, label="*", size=8,color="red")+
  annotate("text", x=7, y=8, label="*", size=8,color="red")+
  xlab("Measure of Community Change")+
  ylab("Glass's D")



# doing stats on change ---------------------------------------------------

##1) Are there differences from zero?

rich<-GlassD %>% 
  filter(measure=="richness_change")
t.test(rich$RR, mu=0, alternative = "two.sided")
#this is NOT sig.
even<-GlassD %>% 
  filter(measure=="evenness_change")
t.test(even$RR, mu=0, alternative = "two.sided")
#this is sig.
dom<-GlassD %>% 
  filter(measure=="dominance")
t.test(dom$RR, mu=0, alternative = "two.sided")
#this is not sig.
rank<-GlassD %>% 
  filter(measure=="rank_change")
t.test(rank$RR, mu=0, alternative = "two.sided")
#this is NOT sig.
gain<-GlassD %>% 
  filter(measure=="gains")
t.test(gain$RR, mu=0, alternative = "two.sided")
#this is sig.
loss<-GlassD %>% 
  filter(measure=="losses")
t.test(loss$RR, mu=0, alternative = "two.sided")
#this is sig.
comp<-GlassD %>% 
  filter(measure=="composition_change")
t.test(comp$RR, mu=0, alternative = "two.sided")
#this is sig.
disp<-GlassD %>% 
  filter(measure=="dispersion_change")
t.test(disp$RR, mu=0, alternative = "two.sided")
#this is NOT sig.

###looking at changes over time.
##there are no differences among the years at all

mrich<-lmer(RR~as.factor(n_treat_years)+(1|site_code), data=rich)
summary(mrich)
#no effect of year
meven<-lmer(RR~as.factor(n_treat_years)+(1|site_code), data=even)
summary(meven)
#no effect of year
mdom<-lmer(RR~as.factor(n_treat_years)+(1|site_code), data=dom)
summary(mdom)
#Sig effect of year
ggplot(data=dom, aes(x=n_treat_years, y=RR))+
  geom_point()+
  geom_smooth(method = "lm", color="black")+
  ylab("Dominance Glass's D")+
  xlab("Year of Drought")

mrank<-lmer(RR~as.factor(n_treat_years)+(1|site_code), data=rank)
summary(mrank)
#no effect of year
mgain<-lmer(RR~as.factor(n_treat_years)+(1|site_code), data=gain)
summary(mgain)
#no effect of year
mloss<-lmer(RR~as.factor(n_treat_years)+(1|site_code), data=loss)
summary(mloss)
#no effect of year
mcomp<-lmer(RR~as.factor(n_treat_years)+(1|site_code), data=comp)
summary(mcomp)
#no effect of year
mdisp<-lmer(RR~as.factor(n_treat_years)+(1|site_code), data=disp)
summary(mdisp)
#no effect of year



RR2<-GlassD %>% 
  left_join(site_types) %>% 
  left_join(continent) 




###looking at local and regional drivers
mrich<-lmer(RR~drtseverity+map+PctAnnual+PctGrass+(1|site_code), data=subset(RR2, measure=="richness_change"))
summary(mrich)
#effect of percent annual

ggplot(data=subset(RR2, measure=="richness_change"), aes(x=PctAnnual, y=RR))+
  geom_point()+
  ylab("Richness Glass's D")+
  geom_smooth(method="lm", color="black")


meven<-lmer(RR~drtseverity+map+PctAnnual+PctGrass+(1|site_code), data=subset(RR2, measure=="evenness_change"))
summary(meven)
#no effect of anything
mrank<-lmer(RR~drtseverity+map+PctAnnual+PctGrass+(1|site_code), data=subset(RR2, measure=="rank_change"))
summary(mrank)
#nothing
mgain<-lmer(RR~drtseverity+map+PctAnnual+PctGrass+(1|site_code), data=subset(RR2, measure=="gains"))
summary(mgain)
#what does sig intercept mean? mild relationship with MAP

ggplot(data=subset(RR2, measure=="gains"), aes(x=map, y=RR))+
  geom_point()+
  ylab("Gains Glass's D")+
  xlab("MAP")+
  geom_smooth(method = "lm", color="black")

mloss<-lmer(RR~drtseverity+map+PctAnnual+PctGrass+(1|site_code), data=subset(RR2, measure=="losses"))
summary(mloss)
#nothing is significant
mcomp<-lmer(RR~drtseverity+map+PctAnnual+PctGrass+(1|site_code), data=subset(RR2, measure=="composition_change"))
summary(mcomp)
#percent annaul mild manner
ggplot(data=subset(RR2, measure=="composition_change"), aes(x=PctAnnual, y=RR))+
  geom_point()+
  geom_smooth(method="lm", color="black")+
  ylab("Composition Response")

mdisp<-lmer(RR~drtseverity+map+PctAnnual+PctGrass+(1|site_code), data=subset(RR2, measure=="dispersion_change"))
summary(mdisp)
#big effect percent grass

ggplot(data=subset(RR2, measure=="dispersion_change"), aes(x=PctGrass, y=RR))+
  geom_point()+
  geom_smooth(method="lm", color="black")+
  ylab("Dispersion Response")

###pairs
allwide<-GlassD %>% 
  select(-cntSD) %>% 
  pivot_wider(names_from="measure", values_from = "RR") %>% 
  select(-dominance, -dispersion_change, -rank_change)



panel.cor <- function(x, y, cex.cor = 0.8, method = "pearson", ...) {
  options(warn = -1)                   # Turn of warnings (e.g. tied ranks)
  usr <- par("usr"); on.exit(par(usr)) # Saves current "usr" and resets on exit
  par(usr = c(0, 1, 0, 1))             # Set plot size to 1 x 1
  r <- cor(x, y, method = method, use = "pair")               # correlation coef
  p <- cor.test(x, y, method = method)$p.val                  # p-value
  n <- sum(complete.cases(x, y))                              # How many data pairs
  txt <- format(r, digits = 3)                                # Format r-value
  txt1 <- format(p, digits = 3)                                 # Format p-value
  txt2 <- paste0("r= ", txt, '\n', "p= ", txt1, '\n', 'n= ', n) # Make panel text
  text(0.5, 0.5, txt2, cex = cex.cor, ...)                      # Place panel text
  options(warn = 0)                                             # Reset warning
}

pairs(allwide[,7:11], lower.panel = panel.cor, cex.cor=2)

