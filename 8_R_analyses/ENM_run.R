source('ENM_functions.R')

nsp <- length(read.table('data/all_birds_fixed.txt')$V1)
species <- as.character(read.table('data/all_birds_fixed.txt')$V1)[4:nsp]

#Not enough data
#Amazona vittata, Chlamydotis macqueeni

#Super split distributions
#Chrysolophus pictus

lapply(species, function(s){

  tryCatch({
    cat(paste('\nWorking on', s,'\n'))
    occs <- get_occ(s)
    cat(paste("Using",nrow(occs),"records\n"))
    plot_occ(occs, s)
    covs <- get_covs(occs, wc_data)
    mod_list <- fit_models(occs, covs, s)
    bestmod <- best_model(mod_list)
    pr_maps <- predict_maps(bestmod, occs, covs, map_list, s)

    #Pliestocene map
    covs_pl <- get_covs(occs, subset(wc_data, c(1,4,8:19)))
    mod_pl <- fit_models(occs, covs_pl, s, pliest=TRUE)
    best_pl <- best_model(mod_pl)
    pliest_map <- predict_maps(best_pl, occs, covs_pl,
                               list(pliest=pliest), s, pliest=TRUE)
    cat('Complete\n')

  }, error= function(e) cat(paste('Species', s, 'failed\n'))
  )


})

s <- 'Acanthisitta_chloris'
s <- 'Cyrtonyx_montezumae'
cat(paste('\nWorking on', s,'\n'))
occs <- get_occ(s, TRUE)
cat(paste("Using",nrow(occs),"records\n"))
plot_occ(occs, s)
    covs <- get_covs(occs, wc_data)
    mod_list <- fit_models(occs, covs, s)
    bestmod <- best_model(mod_list)
    pr_maps <- predict_maps(bestmod, occs, covs, map_list, s)

    #Pliestocene map
    covs_pl <- get_covs(occs, subset(wc_data, c(1,4,8:19)))
    mod_pl <- fit_models(occs, covs_pl, s, pliest=TRUE)
    best_pl <- best_model(mod_pl)
    pliest_map <- predict_maps(best_pl, occs, covs_pl,
                               list(pliest=pliest), s, pliest=TRUE)

