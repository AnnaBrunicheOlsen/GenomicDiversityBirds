library(tidyverse)
library(readxl)

traits <- read_excel('data2/bird_GD_lifehistory_71species_22022020.xlsx',
                      sheet='Table S1 lifehistory') %>%
  rename(Species=Genus_Species) %>%
  rename(IUCN = GlobalIUCNRedListCategory) %>%
  mutate(IUCN = factor(IUCN, levels=c('LC','NT','VU','EN','CR'))) %>% 
  select(Species, threatenedNonthreatened, Diet_5Cat, 
         adultBodyMassGram, flyingOrNot, litterOrClutchSizeNumber,
  movementPatterns, IUCN)

areas <- read_csv('data2/enm_areas.csv')
chng <- read_csv('data2/enm_change.csv')
nesize <- read_csv('data2/ne_size.csv')
nechng <- read_csv('data2/ne_change.csv')

gen <- read_excel('data2/bird_GD_lifehistory_71species_22022020.xlsx',
                      sheet='Table S3 GD') %>%
  select(Species, `heterozygosity_for_autosomes_>10k_bp`)

names(gen) <- c('Species', 'Het')

dat <- data.frame(traits, Het=gen$Het) %>% as_tibble()

dat <- dat %>%
  #left_join(traits, by='Species') %>%
  mutate(threatened=ifelse(threatenedNonthreatened=="NT", 0, 1)) %>%
  mutate(mass = as.numeric(adultBodyMassGram)) %>%
  mutate(diet = relevel(as.factor(Diet_5Cat), ref='VertFishScav')) %>%
  mutate(litter = as.numeric(litterOrClutchSizeNumber)) %>%
  mutate(carn = ifelse(Diet_5Cat=='VertFishScav','Carnivore','Non-carnivore')) %>%
  mutate(carn=factor(carn, levels=c('Non-carnivore','Carnivore'))) %>%
  mutate(migrates=ifelse(movementPatterns=='non-migrant',0,1))

lat <- read_csv('data2/mean_lat.csv') %>%
  rename(Species=SCINAME) %>%
  mutate(Species=gsub(" ", "_", Species))

dat_lat <- dat %>%
  left_join(lat, by='Species')

ne_long <- nesize %>% gather(key='period', value='ne', pliest:earlyholo)

dat_ne <- areas %>%
  mutate(species=dat$Species) %>%
  gather(key='period', value='area', pliest:present) %>%
  left_join(ne_long) %>%
  rename(Species=species) %>%
  left_join(dat_lat) %>%
  filter(period != 'present')

dat_stan <- dat_ne %>%
  mutate(threat = ifelse(threatenedNonthreatened=='NT',0,1),
         fruit = ifelse(diet == "FruiNect", 1, 0),
         omni = ifelse(diet == "Omnivore", 1, 0),
         invert = ifelse(diet == "Invertebrate", 1, 0),
         plant = ifelse(diet == "PlantSeed", 1, 0)) %>%
  mutate(NT = ifelse(IUCN == "NT", 1, 0),
         VU = ifelse(IUCN == "VU", 1, 0),
         EN = ifelse(IUCN == "EN", 1, 0),
         CR = ifelse(IUCN == "CR", 1, 0)) %>%
  select(Species,period,area,ne,Het,threat,fruit,omni,invert,plant,migrates,              mass, mean_lat, NT, VU, EN, CR) %>%
  drop_na()

stan_data <- list(logNe=log(dat_stan$ne), area=as.numeric(scale(dat_stan$area)),
                  threat=dat_stan$threat, fruit=dat_stan$fruit,
                  omni=dat_stan$omni, invert=dat_stan$invert,
                  NT=dat_stan$NT, VU=dat_stan$VU, EN=dat_stan$EN,
                  CR=dat_stan$CR,
                  plant=dat_stan$plant,
                  spnames = dat_stan$Species,
                  migrates=dat_stan$migrates,
                  mass=as.numeric(scale(dat_stan$mass)), 
                  lat = as.numeric(scale(dat_stan$mean_lat)),
                  species=as.numeric(as.factor(dat_stan$Species)),
                  nspecies=length(unique(dat_stan$Species)),
                  nobs=nrow(dat_stan))

saveRDS(dat_stan, 'stan_data.Rds')

params <- c('alpha', 'b_area', 'grand_mean',#'b_threat',
            'b_nt','b_vu', 'b_en', 'b_cr',
            'b_fruit','b_plant','b_invert',
            'b_omni','b_mig','b_mass','b_lat','ar_mean','ar_sd',
            'fruit_plant', 'fruit_invert', 'fruit_omni', 'plant_invert',
            'plant_omni', 'invert_omni',
            'nt_vu','nt_en','nt_cr','vu_en','vu_cr','en_cr',
             'yrep'
            )

library(rstan)
options(mc.cores = parallel::detectCores()-1)
fit <- stan('stan_model.stan', data = stan_data, pars = params, chains = 4, 
            init = 0,
            iter = 4000, warmup = 3000, thin = 2, 
            control = list(adapt_delta = 0.99))

saveRDS(fit, 'stan_mod.Rds')
                  
