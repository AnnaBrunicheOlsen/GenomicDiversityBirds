library(sf)

birds <- as.character(read.table('data/all_birds_fixed.txt')$V1)
for (i in 1:length(birds)){
  birds[i] <- gsub("_", " ", birds[i])
  birds[i] <- paste0("'",birds[i],"'")
}

birds <- paste0('(',paste(birds, collapse=', '),')')

sq_test <- st_read('~/local/bird-data/bird-dist.gdb', layer='All_Species',
  query=paste("SELECT SCINAME, DATE_, PRESENCE, SEASONAL, Shape_Area, Shape
               FROM ALL_Species 
               WHERE SCINAME IN", birds))

sq_test <- sq_test %>% 
  filter(PRESENCE == 1, SEASONAL %in% c(1,2)) %>%
  rename(geometry=Shape) %>%
  st_simplify(dTolerance=0.05)

saveRDS(sq_test, file='data/bird_dists.Rds')





