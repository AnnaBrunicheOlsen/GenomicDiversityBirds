# Read in required functions and libraries
# Load in climate data
source('ENM_setup.R')

# List of species
nsp <- length(read.table('data/all_birds_fixed.txt')$V1)
species <- as.character(read.table('data/all_birds_fixed.txt')$V1)[4:nsp]

# Run ENM models for all species
# Occurrence records and predicted maps are saved to disk as .Rds files
run_species <- function(s, bufsize=10){

  tryCatch({
    cat(paste('\nWorking on', s,'\n'))
    # Get occurrence records
    occs <- get_occ(s)
    cat(paste("Using",nrow(occs),"records\n"))
    # Plot occurrences
    plot_occ(occs, s)
    # Get covariate data for all occurences
    covs <- get_covs(occs, wc_data, bufsize)
    # Fit EN models
    mod_list <- fit_models(occs, covs, s)
    # Select best model
    bestmod <- best_model(mod_list)
    # Generate predictive maps based on best model
    pr_maps <- predict_maps(bestmod, occs, covs, map_list, s, bufsize=bufsize)

    # Do the same things but for MIS-19 data (which uses a subset of covs)
    covs_pl <- get_covs(occs, subset(wc_data, c(1,4,8:19)), bufsize)
    mod_pl <- fit_models(occs, covs_pl, s, pliest=TRUE)
    best_pl <- best_model(mod_pl)
    pliest_map <- predict_maps(best_pl, occs, covs_pl,
                               list(pliest=pliest), s, pliest=TRUE,bufsize=bufsize)
    cat('Complete\n')

  }, error= function(e) cat(paste('Species', s, 'failed\n'))
  )
}

lapply(species, run_species)

# species that needs special attention
run_species("Dendrocopus noguchii", bufsize=5)

# Check final sample sizes
library(tidyverse)
dat_all <- read.csv("data/dat_all.csv")

sp <- dat_all$Species
sp[4] <- 'Anser_cygnoid'

pnts <- rep(NA, length(sp))

for (i in 1:length(sp)){

  fname <- paste0("data2/points/",sp[i],"_points.Rds")

  if(file.exists(fname)){
  inp <- readRDS(fname)
  pnts[i] <- nrow(inp)
  }
}

# Histogram of sample sizes
hist(pnts)

# Check occurrence record accuracy
setwd("/mnt/media/bird_enm/points") # location of stored occurrence record .Rds files
files <- list.files()
sp <- gsub("_points.Rds", "", files)
spname <- sapply(strsplit(sp, "_"), function(x) x[[2]])

correct_pct <- rep(NA, length(sp))
names(correct_pct) <- sp
for (i in 1:length(files)){
  dat <- readRDS(files[i])
  if("SCINAME" %in% names(dat)){
    is_correct <- grepl(spname[i], dat$SCINAME)
  } else if("name" %in% names(dat)){
    is_correct <- grepl(spname[i], dat$name)
  }
  correct_pct[i] <- mean(is_correct, na.rm=T)
}

correct_pct
