library(raster)
library(pbapply)

allsp <- list.files("data/maps")
allsp <- gsub("_maps.Rds", "", allsp)
allsp <- gsub("_pliest", "", allsp)
allsp <- unique(allsp)

calc_areas <- function(maps){

  lapply(maps, function(x){
    crs(x) <- "+proj=longlat"
    cell_areas <- getValues(area(x))
    keep <- getValues(x)>=0.50
    sum(cell_areas[keep], na.rm=T)
  })
}

extract_areas <- function(species){
  maps1 <- readRDS(paste0("data/maps/",species,"_maps.Rds"))
  maps2 <- readRDS(paste0("data/maps/",species,"_pliest_maps.Rds"))
  
  maps1 <- maps1[c("interglacial","glacialmax","earlyholo","present")]
  maps2 <- maps2["pliest"]
  maps <- c(maps2, maps1)
  unlist(calc_areas(maps))
}

all_areas <- pblapply(allsp, extract_areas)
all_areas <- as.data.frame(do.call("rbind", all_areas))
all_areas <- data.frame(species=allsp, all_areas)

write.csv(all_areas, "data/enm_areas_50.csv", row.names=F)

#Change in habitat areas

pl_ig <- (all_areas$interglacial - all_areas$pliest)/all_areas$pliest
ig_gm <- (all_areas$glacialmax - all_areas$interglacial)/all_areas$interglacial
gm_eh <- (all_areas$earlyholo - all_areas$glacialmax)/all_areas$glacialmax

all_chng <- data.frame(species=allsp, pl_ig=pl_ig, ig_gm=ig_gm, gm_eh=gm_eh)

write.csv(all_chng, "data/enm_change_50.csv", row.names=F)

#Change in effective pop size


interp_value <- function(dat, newval){  
  b1 <- max(which(newval > dat$time))
  b2 <- min(which(newval <= dat$time))
  xvals <- c(dat$time[b1], dat$time[b2])
  yvals <- c(dat$pop[b1], dat$pop[b2])
  tryCatch(approx(xvals, yvals, xout=newval)$y,
           error=function(e) NA)
}

get_ne <- function(species){

  eholo <- mean(c(8.326e3,11.7e3))
  gmax <- 21000
  iglacial <- c(130000)
  pliest <- 787000

  times <- c(pliest=pliest, interglacial=iglacial, glacialmax=gmax, 
             earlyholo=eholo)
  
  if(species=='Anser_cygnoid') species <- 'Anser_cygnoides'
  if(species=='Corvus_corone') species <- 'Corvus_cornix'
 
  tmp_dir <- tempdir()
  zf <- paste0('data/psmc/',species,'.tar.gz')
  untar(zf, exdir=tmp_dir)

  if(species=='Apteryx_haastii') species <- 'Apteryx_haasti'
  if(species=='Dryobates_pubescens') species <- 'Picoides_pubescens'
  sp_dir <- paste0(tmp_dir,'/',species)
  template <- list.files(sp_dir, pattern="\\.0\\.txt$")
  fbase <- gsub("0.txt", "", template)

  dat <- read.table(paste0(sp_dir, "/", fbase, '0.txt'))[,1:2]
  names(dat) <- c("time", "pop")
  
  sapply(times, function(x) interp_value(dat, x))

}

all_ne <- pblapply(allsp, get_ne)
all_ne <- as.data.frame(do.call("rbind", all_ne))
all_ne <- data.frame(species=allsp, all_ne)

write.csv(all_ne, "data/ne_size.csv", row.names=F)

pl_ig <- (all_ne$interglacial - all_ne$pliest)/all_ne$pliest
ig_gm <- (all_ne$glacialmax - all_ne$interglacial)/all_ne$interglacial
gm_eh <- (all_ne$earlyholo - all_ne$glacialmax)/all_ne$glacialmax

ne_chng <- data.frame(species=allsp, pl_ig=pl_ig, ig_gm=ig_gm, gm_eh=gm_eh)

write.csv(ne_chng, "data/ne_change.csv", row.names=F)
