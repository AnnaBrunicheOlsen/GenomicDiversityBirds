#------------------------------------------------------------------------------
#Load libraries

suppressMessages(library(tidyverse))
library(readxl)

#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Read in data
raw_genetics <- read_excel('data/bird_ROH_lifehistory_03042019.xlsx',
                           sheet='genomic_diviersity')

raw_covs <- read_excel('data/bird_ROH_lifehistory_03042019.xlsx',
                       sheet='lifehistory')

#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Format data
genetic_data <- raw_genetics %>%
  rename(species = Genus_Species, het = heteroAll, doc = DepthOfCoverage, 
         G20 = proportion_of_genome_with_20SNPs) %>%
  mutate(species = 
          case_when(
            species == 'Strix_occidentalis_caurina' ~ 'Strix_occidentalis',
            species == 'Saxicola_maurus_maurus' ~ 'Saxicola_maurus',
            species == 'Phoenicopterus_ruber_ruber' ~ 'Phoenicopterus_ruber',
            species == 'Patagioenas_fasciata_monilis' ~ 'Patagioenas_fasciata',
            TRUE ~ species)) %>%
  mutate(S20 = `SNPs_in_Scaffolds_with_20_or_more_SNPs`/SNPs) %>%
  select(species, het, Froh, N50, doc, G20, S20)

options(warn=-1)
cov_data <- raw_covs %>%
  rename(species = `Genus_Species`) %>%
  mutate(species = str_replace(species,' ','')) %>%
  mutate(species = case_when(
                    species == 'Apteryx_haastii' ~ 'Apteryx_haasti',
                    TRUE ~ species)) %>%
  mutate(threatened = ifelse(threatenedNonthreatened=='TR',1,0),
         thr4 = case_when(GlobalIUCNRedListCategory%in%c('CR','EN') ~ 'EN',
                          GlobalIUCNRedListCategory%in%c('DD','NA') ~ NA_character_,
                          TRUE ~ GlobalIUCNRedListCategory),
         thr4 = factor(thr4,levels=c('LC','VU','NT','EN')),
         carnivore = ifelse(`Diet_5Cat`=='VertFishScav',1,0),
         declining = case_when(
                      populationTrend == 'Decreasing' ~ 1,
                      populationTrend %in% c('Increasing','Stable') ~ 0),
         mass = as.numeric(adultBodyMassGram),
         gentime = as.numeric(generationTimeIUCNyears),
         migrates = ifelse(movementPatterns=='migrant',1,0),
         flying = as.numeric(flyingOrNot=="flight"),
         repro = as.numeric(litterOrClutchSizeNumber) * 
           as.numeric(littersOrClutchesPerYears),
         Diet_5Cat = factor(Diet_5Cat,levels=c('PlantSeed','FruiNect',
                                               'Invertebrate','Omnivore',
                                               'VertFishScav'))
         ) %>%
  select(species,threatened,carnivore,declining,mass,gentime,migrates,flying,
         repro,Diet_5Cat,thr4)
options(warn=0)

#Combine
all_data <- genetic_data %>%
  left_join(cov_data, by='species')
