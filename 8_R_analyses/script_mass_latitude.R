
##############################

#Add lat/long data

roh20 <- read.csv('ROH_20x.csv',header=T)[1:63,]
latlong <- read.csv('dist_center_latlong.csv',header=T)

roh20$binomial.match <- as.character(roh20$PWD)
roh20$binomial.match[roh20$PWD=='Bison_bison_bison'] <- 'Bison_bison'
roh20$binomial.match[roh20$PWD=='Bubalus_bubalis'] <- 'Bubalus_arnee'
roh20$binomial.match[roh20$PWD=='Trichechus_manatus_latirostris'] <- 'Trichechus_manatus'
roh20$binomial.match[roh20$PWD=='Physeter_catodon'] <- 'Physeter_macrocephalus'

spname.raw <- as.character(latlong$binomial)

spname <- rep(NA,length(spname.raw))
for (i in 1:length(spname)){
  char1 <- strsplit(spname.raw[i]," ")[[1]][1]
  char2 <- strsplit(spname.raw[i]," ")[[1]][2]
  spname[i] <- paste(char1,"_",char2,sep="")
}

for (i in 1:dim(roh20)[1]){
  ind <- which(spname==roh20$binomial.match[i])

  if(length(ind)==1){
    roh20$lat[i] <- latlong$ycoord[ind]
  } else if(length(ind)>1){
    roh20$lat[i] <- mean(latlong$ycoord[ind])

  } else{roh20$lat[i] <- NA}

}


roh20$lat[roh20$binomial.match=='Camelus_bactrianus'] <- 45.75
roh20$lat[roh20$binomial.match=='Cebus_capucinus'] <- 12.77
roh20$lat[roh20$binomial.match=='Cricetulus_griseus'] <- 46.86

nolat <- roh20$binomial.match[is.na(roh20$lat)]

###############

#Add body mass data

bodymass <- read.csv('body_mass.csv',header=T)

spname.raw <- as.character(bodymass$Scientific_name)

spname <- rep(NA,length(spname.raw))
for (i in 1:length(spname)){
  char1 <- strsplit(spname.raw[i]," ")[[1]][1]
  char2 <- strsplit(spname.raw[i]," ")[[1]][2]
  spname[i] <- paste(char1,"_",char2,sep="")
}

for (i in 1:dim(roh20)[1]){
  ind <- which(spname==roh20$binomial.match[i])
  if(length(ind)==1){
    roh20$mass[i] <- bodymass$AdultBodyMass_g[ind]
  } else{roh20$mass[i]  <- NA}

}

roh20$mass[roh20$binomial.match=='Fukomys_damarensis'] <- 82
roh20$mass[roh20$binomial.match=='Bos_indicus'] <- 750*1000
roh20$mass[roh20$binomial.match=='Bos_taurus'] <- 750*1000
roh20$mass[roh20$binomial.match=='Camelus_bactrianus'] <- 500*1000
roh20$mass[roh20$binomial.match=='Cricetulus_griseus'] <- 380
roh20$mass[roh20$binomial.match=='Felis_catus'] <- 4*1000

nomass <- roh20$binomial.match[is.na(roh20$mass)]

write.csv(roh20,file='roh20X_full.csv',row.names=F)

