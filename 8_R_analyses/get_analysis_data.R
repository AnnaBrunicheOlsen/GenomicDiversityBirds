library(tidyverse)
library(readxl)

# Past habitat areas-----------------------------------------------------------

if(!file.exists('data/enm_areas.csv')){
  library(raster)
  library(pbapply)

  allsp <- list.files("data2/maps")
  allsp <- gsub("_maps.Rds", "", allsp)
  allsp <- gsub("_pliest", "", allsp)
  allsp <- unique(allsp)

  calc_areas <- function(maps){
    lapply(maps, function(x){
      crs(x) <- "+proj=longlat"
      cell_areas <- getValues(area(x))
      keep <- getValues(x)>=0.36
      sum(cell_areas[keep], na.rm=T)
    })
  }

  extract_areas <- function(species){
    maps1 <- readRDS(paste0("data2/maps/",species,"_maps.Rds"))
    maps2 <- readRDS(paste0("data2/maps/",species,"_pliest_maps.Rds"))

    maps1 <- maps1[c("interglacial","glacialmax","earlyholo","present")]
    maps2 <- maps2["pliest"]
    maps <- c(maps2, maps1)
    unlist(calc_areas(maps))
  }

  all_areas <- pblapply(allsp, extract_areas)
  all_areas <- as.data.frame(do.call("rbind", all_areas))
  all_areas <- data.frame(species=allsp, all_areas)

  write.csv(all_areas, "data/enm_areas.csv", row.names=F)
}

areas <- read.csv("data/enm_areas.csv")

areas <- areas %>%
  mutate(species = recode(species, Anser_cygnoid="Anser_cygnoides")) %>%
  mutate(past_area_mean = c(pliest+interglacial+glacialmax+earlyholo)/4) %>%
  mutate(past_area_mean_noholo = c(pliest+interglacial+glacialmax)/3)

areas_var <- apply(areas[,c("pliest","interglacial","glacialmax","earlyholo")],
                   1,var,na.rm=T)
areas$past_area_var <- areas_var

# Mean Ne----------------------------------------------------------------------

allsp <- list.files("data/data/psmc")
allsp <- gsub(".tar.gz","", allsp, fixed=TRUE)

get_ne <- function(species){
  tmp_dir <- tempdir()
  zf <- paste0('data/data/psmc/',species,'.tar.gz')
  untar(zf, exdir=tmp_dir)

  if(species=='Apteryx_haastii') species <- 'Apteryx_haasti'
  if(species=='Dryobates_pubescens') species <- 'Picoides_pubescens'
  sp_dir <- paste0(tmp_dir,'/',species)
  template <- list.files(sp_dir, pattern="\\.0\\.txt$")
  fbase <- gsub("0.txt", "", template)

  dat <- read.table(paste0(sp_dir, "/", fbase, '0.txt'))[,1:2]
  names(dat) <- c("time", "pop")
  dat$keep <- TRUE
  dat$keep[1:4] <- FALSE
  dat$species <- species

  mean_Ne <- mean(dat$pop[dat$keep], na.rm=T)
  var_Ne <- var(dat$pop[dat$keep], na.rm=T)

  return(list(data=dat, mean_Ne=mean_Ne, var_Ne=var_Ne))
}

all_ne <- lapply(allsp, get_ne)

all_mean <- lapply(all_ne, function(x) data.frame(species=x$data$species[1], mean_Ne=x$mean_Ne, var_Ne=x$var_Ne))
all_mean <- do.call(rbind, all_mean) %>%
  rename(Species=species)

all_mean$Species[all_mean$Species == "Apteryx_haasti"] <- "Apteryx_haastii"
all_mean$Species[all_mean$Species == "Picoides_pubescens"] <- "Dryobates_pubescens"

# Life history traits----------------------------------------------------------

traits <- read_excel('data/bird_GD_lifehistory_71species_19102020.xlsx',
                      sheet='Table S1 lifehistory') %>%
  rename(Species=Genus_Species) %>%
  rename(IUCN = GlobalIUCNRedListCategory) %>%
  mutate(IUCN = factor(IUCN, levels=c('LC','NT','VU','EN','CR'))) %>%
  select(Species, threatenedNonthreatened, Diet_5Cat,
         adultBodyMassGram, flyingOrNot, litterOrClutchSizeNumber,
  movementPatterns,IUCN)

# Heterozygosity---------------------------------------------------------------
gen <- read_excel('data/bird_GD_lifehistory_71species_19102020.xlsx',
                      sheet='Table S3 GD') %>%
  select(Species, heterozygosity)

names(gen) <- c('Species', 'Het')

# Combine data-----------------------------------------------------------------

dat <- data.frame(traits, Het=gen$Het) %>% as_tibble()

dat <- dat %>%
  mutate(mass = as.numeric(adultBodyMassGram)) %>%
  mutate(diet = relevel(as.factor(Diet_5Cat), ref='VertFishScav')) %>%
  mutate(IUCN=fct_recode(IUCN, `EN+CR`="EN",`EN+CR`="CR")) %>%
  mutate(Species = recode(Species, Picoides_pubescens='Dryobates_pubescens')) %>%
  left_join(areas %>% rename(Species=species)) %>%
  filter(Species != "Apteryx_rowi") %>%
  filter(Species != "Cuculus_canorus") %>%
  filter(Species != "Corvus_cornix_AKA_corvus_corone") %>%
  left_join(all_mean) %>%
  select(Species, Het, mean_Ne, var_Ne, mass, diet, IUCN, present, past_area_mean,
         past_area_mean_noholo, past_area_var)

write.csv(dat, "data/dat_all.csv", row.names=FALSE)
