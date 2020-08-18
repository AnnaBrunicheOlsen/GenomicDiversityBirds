library(sf)
library(tidyverse)

dist_maps <- readRDS('data/bird_dists.Rds')

cents <- st_centroid(dist_maps) %>%
  cbind(., st_coordinates(.)) %>%
  st_set_geometry(NULL) %>%
  group_by(SCINAME) %>%
  summarize(mean_lat = mean(abs(Y), na.rm=T))

write_csv(cents, 'data2/mean_lat.csv')
