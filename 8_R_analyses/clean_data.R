#------------------------------------------------------------------------------
#Load libraries

suppressMessages(library(tidyverse))
library(readxl)

#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Read in data
raw_genetics <- read_excel('data/bird_ROH_lifehistory_19032019.xlsx',
                           sheet='genomic_diviersity')

raw_covs <- read_excel('data/bird_ROH_lifehistory_19032019.xlsx',
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
  rename(species = `Genus _Species`) %>%
  mutate(species = str_replace(species,' ','')) %>%
  mutate(species = case_when(
                    species == 'Apteryx_haastii' ~ 'Apteryx_haasti',
                    TRUE ~ species)) %>%
  mutate(threatened = ifelse(threatenedNonthreatened=='TR',1,0),
         carnivore = ifelse(`Diet _5Cat`=='VertFishScav',1,0),
         declining = case_when(
                      populationTrend == 'Decreasing' ~ 1,
                      populationTrend %in% c('Increasing','Stable') ~ 0),
         mass = as.numeric(adultBodyMassGram),
         gentime = as.numeric(generationTimeIUCNyears),
         migrates = ifelse(movementPatterns=='migrant',1,0) 
         ) %>%
  select(species,threatened,carnivore,declining,mass,gentime,migrates)
options(warn=0)

#Combine
all_data <- genetic_data %>%
  left_join(cov_data, by='species')
