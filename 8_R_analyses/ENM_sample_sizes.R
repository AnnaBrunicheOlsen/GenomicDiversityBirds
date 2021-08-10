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
