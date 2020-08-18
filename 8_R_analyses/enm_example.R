source('fit_functions.R')

species <- 'Accipiter_nisus'
test <- get_occ(species)
covs_test <- get_covs(test)

mod_list <- fit_models(test, covs_test)

bestmod <- best_model(mod_list)

pr_maps <- predict_maps(bestmod, test, covs_test, map_list)

#plot
all_plots <- mapply(plot_map, pr_maps, 
                    title=c("Present day", "Last glacial maximum",
                      "Last interglacial", "Mid-Holocene", "2070 RCP 4.5"),
                    SIMPLIFY=FALSE)

grobs <- ggplotGrob(plot_map(pr_maps[[1]],'test',"left"))$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

#Overall title
title <- ggdraw() + 
  draw_label(gsub("_", " ", species), fontface = 'bold.italic',
    x = 0, hjust = 0, size=18) +
  theme(
    plot.margin = margin(0,0, 0, 0)
)

content <- plot_grid(plotlist=c(all_plots[c(2:4,1,5)], list(legend)), nrow=2)



plot_grid(title, content, ncol=1, rel_heights=c(0.1,1))

ggsave(paste0(species,'.pdf'))



library(wdpar)

usa_pd <- wdpa_fetch("United States")

usa_sub <- usa_pd %>%
  filter(REP_AREA > 1000, MARINE == 0) %>%
  st_crop(st_as_sf(get_sp(test3)))

ggplot() +
  geom_sf(data=usa_sub)

pd + 
  geom_sf(data=usa_sub, fill='transparent') +
  theme(panel.grid.major=element_line(color='transparent'))
