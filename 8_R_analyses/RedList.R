# get IUCN Red List info and life history traits for each species

####### LIFE HISTORY #######
setwd("~/Documents/Bird_ROHs/data/")

df1 <- read.csv("Amniote-Life-History-Database.csv", header = T) 

# Red list data from BirdLife
df2 <- read.csv("BirdLife_species_20190305_11590.csv", header = T)

birds <- read.table("birds_84.txt", header = T)
birdVector <- as.vector(birds$Genus_Species)

library(dplyr)
df84 <- df2 %>% filter(Scientific_name %in% birdVector)

names <- sort(df2$Scientific_name)




df3 <- read.csv("BirdFuncDat.csv", header = T)

####### RED LIST #######

install.packages("rredlist")
library(rredlist)

rredlist::rl_use_iucn()
