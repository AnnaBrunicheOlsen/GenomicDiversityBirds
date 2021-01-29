library(rgbif)
library(ENMeval)
library(raster)
library(sp)
library(sf)
library(pbapply)
library(tidyverse)
library(RStoolbox)
library(ggplot2)
library(cowplot)
library(rnaturalearth)
library(rnaturalearthdata)

#BirdLife Distributions
cat('Loading BirdLife distributions\n')
bd_dist <- readRDS('data/bird_dists.Rds')

#Covariate maps
cat('Loading bioclim covariate maps\n')
map_list <- lapply(list(
                        interglacial=paste0('data/lig/bio_',1:19,'.tif'),
                        glacialmax=paste0('data/ccl/cclgmbi',1:19,'.tif'),
                        earlyholo=paste0('data/eh25/bio_',1:19,'.tif'),         
                        proj70_rcp4.5=paste0('data/cc45/cc45bi70',1:19,'.tif')),
                   function(files) stack(lapply(files, raster)))

map_list$interglacial$bio_3 <- mask(map_list$interglacial$bio_3,
                                    map_list$interglacial$bio_2)

pliest <- stack(lapply(paste0('data/pliest/bio_',c(1,4,8:19),'.tif'), raster))

cat("Loading saved WorldClim data\n")
wc_data <- readRDS('data/worldclim_2.5.Rds')

plot_occ <- function(occ, sp){

  pts <- get_sp(occ)
  wm <- ne_coastline()
  ds <- bd_dist[bd_dist$SCINAME==gsub("_"," ", sp),]
  plot(wm)
  plot(ds, add=T)
  plot(pts, add=T)
}

get_occ <- function(species, ignore_bl=FALSE){
  fname <- paste0('data/points/',species,'_points.Rds')
  if(file.exists(fname)){
    cat("Loading saved occurrence data\n")
    return(readRDS(fname))
  }

  species <- gsub('_', ' ', species)
  
  lat_lim <- '-90,90'
  lng_lim <- '-180,180'
  if(!ignore_bl){
    bd_sub <- bd_dist %>%
      filter(SCINAME == gsub("_", " ", species))

    bb <- st_bbox(bd_sub)
    lat_lim <- paste(bb[c(2,4)],collapse=',')
    lng_lim <- paste(bb[c(1,3)],collapse=',')
  }

  sp_use <- species
  if(species == 'Anser cygnoid') sp_use <- 'Anser cygnoides'

  avail_points <- occ_search(scientificName=sp_use,
                             hasCoordinate=TRUE,
                              hasGeospatialIssue=FALSE, year='1970,2019',
                             decimalLatitude=lat_lim,
                             decimalLongitude=lng_lim,
                              limit=0)$meta$count

  npoints <- min(10000, avail_points)
  
  nbatches <- floor(npoints / 300)
  left <- npoints %% 300
  if(left > 0) nbatches <- nbatches + 1
  index <- 1
  cat("Downloading occurrence data\n")
  out <- pblapply(1:nbatches, function(i){
    lim <- ifelse(left > 0 & i == nbatches, left, 300)
    out <- occ_search(scientificName=sp_use,
                      hasCoordinate=TRUE,
               hasGeospatialIssue=FALSE, year='1970,2019',
               decimalLatitude=lat_lim,
                decimalLongitude=lng_lim,
               fields='minimal',limit=lim, start=index)$data
    index <<- index + lim
    out
  })

  out <- do.call("rbind", out) %>% drop_na()
  
  cat("Cleaning occurrence data\n")
  out <- out[!duplicated(out[,3:4]),]
  out <- suppressWarnings(st_as_sf(out, coords=c("decimalLongitude", "decimalLatitude"),
                  crs=st_crs("+proj=longlat +datum=WGS84"), remove=FALSE))
 
  if(!ignore_bl){
    out <- suppressWarnings(suppressMessages(st_intersection(out, bd_sub)))
  }
  out <- out %>% st_set_geometry(NULL)

  if(nrow(out)>3000) out <- out[sample(1:nrow(out), 3000),]
  saveRDS(out, fname)
  cat(paste("Saved", nrow(out), "records\n"))

  out
}

get_sp <- function(occs){
  SpatialPoints(as.data.frame(occs[,c('decimalLongitude','decimalLatitude')]))
}

get_buffer <- function(occs){
  data_sp <- get_sp(occs) 
  bb <- bbox(data_sp)
  extent(bb[1]-10, bb[3]+10, bb[2]-10, bb[4]+10)
}

get_covs <- function(occs, wc){

  bb_buf <- get_buffer(occs)
  cat('Cropping WorldClim data\n')
  wc_clip <- crop(wc, bb_buf)

  #PCA
  cat('Doing PCA on climate data\n')
  out <- rasterPCA(wc_clip, spca=TRUE, maskCheck=FALSE, nSamples=10000)
  out$map <- subset(out$map, 1:6)
  out
}

fit_models <- function(occs, covs, sp, pliest=FALSE){
  
  fn <- paste0('data/models/',sp,'_models.Rds')
  if(pliest) fn <- paste0('data/models/',sp,'_pliest_models.Rds')
  if(file.exists(fn)){
    cat('Loading saved models\n')
    return(readRDS(fn))
  }

  occs <- occs[,c('decimalLongitude','decimalLatitude')] 
  rcovs <- covs$map 

  #Background points
  bg <- randomPoints(rcovs[[1]], n=10000)
  bg <- as.data.frame(bg)

  out <- ENMevaluate(occs, rcovs, bg, 
                #method='checkerboard2', 
                method='randomkfold',
                kfolds=4,
                RMvalues=c(1,2,5), 
                fc=c('L','LQ','LQP'#,'LQPT'
                     #,'LQPTH'
                     ), 
                algorithm='maxnet')
  saveRDS(out, fn)
  out
}

best_model <- function(mod_list, AUC=FALSE){
  AUCs <- mod_list@results$avg.test.AUC
  best <- ifelse(AUC, which(AUCs==min(AUCs,na.rm=TRUE)),
                      which(mod_list@results$delta.AICc==0))
  cat(paste0('Best model is ',mod_list@results$settings[best],'\n'))
  mod_list@models[[best]]
}

get_scores <- function(new_stack, covs){
  vals <- getValues(new_stack)
  for (i in 1:ncol(vals)){
    vals[,i] <- (vals[,i] - covs$model$center[i]) / covs$model$scale[i]
  }
  loads <- covs$model$loadings[,1:6]
  new_vals <- vals %*% loads
  values(new_stack) <- new_vals
  names(new_stack) <- names(covs$map)
  new_stack
}

predict_maps <- function(mod, occs, covs, map_list, sp, pliest=FALSE){
  
  fn <- paste0('data/maps/',sp,'_maps.Rds')
  if(pliest) fn <- paste0('data/maps/',sp,'_pliest_maps.Rds')
  if(file.exists(fn)){
    cat('Loading saved maps\n')
    return(readRDS(fn))
  }

  cat('Generating distribution maps from model\n')
  map_list <- c(present=covs$map, map_list)

  out <- pblapply(seq_along(map_list), function(i){
            if(i==1){
              st <- map_list[[i]]
            } else{
              st <- crop(map_list[[i]], get_buffer(occs))
              st <- get_scores(st, covs)
            }
            maxnet.predictRaster(mod, st, type='logistic', clamp=F)
  })
  names(out) <- names(map_list)
  saveRDS(out, fn)
  out
}
