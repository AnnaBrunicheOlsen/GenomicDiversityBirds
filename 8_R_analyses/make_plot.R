source('plot_functions.R')

cols <- rep('grey80',4)

nsp <- length(read.table('data/all_birds_fixed.txt')$V1)
allsp <- as.character(read.table('data/all_birds_fixed.txt')$V1)

all_map <- gsub("_maps.Rds", "", list.files('data2/maps'), fixed=TRUE)
all_map <- unique(gsub("_pliest", "", all_map))

all_psmc <- gsub(".tar.gz", "", list.files("data/data/psmc"))

all_map[all_map == "Anser_cygnoid"] <- "Anser_cygnoides"
all_psmc[all_psmc == "Apteryx_haasti"] <- "Apteryx_haastii"
all_psmc[all_psmc == "Picoides_pubescens"] <- "Dryobates_pubescens"

allsp <- all_psmc[all_psmc %in% all_map]

make_plot <- function(species){

  draw_file <-  get_drawing(species)

  if(species=="Apteryx_haastii"){
    psmc_species <- "Apteryx_haasti"
  } else if(species=="Dryobates_pubescens"){
    psmc_species <- "Picoides_pubescens"
  } else{
    psmc_species <- species
  }
  ppl <- plot_psmc(zipped=paste0('data/data/psmc/',psmc_species,'.tar.gz'),
                  #terrain.colors(300)[1],
                  'chocolate2',
                  #ggpubr::get_palette("npg", 1),
                  cols[1:4], FALSE)
  pp <- ppl$plot

  pred_maps <- readRDS(paste0('data2/maps/',species,'_maps.Rds'))
  pl_map <- readRDS(paste0('data2/maps/',species,'_pliest_maps.Rds'))

  present <- plot_map(pred_maps$present, title="Present day", bcolor='white')
  eh <- plot_map(pred_maps$earlyholo, title=NULL, bcolor='white')
  ig <- plot_map(pred_maps$interglacial, title=NULL, bcolor='white')
  gm <- plot_map(pred_maps$glacialmax, title=NULL, bcolor='white')
  pli <- plot_map(pl_map$pliest, title=NULL, bcolor='white')

  apl <- area_plot(pred_maps, pl_map, ppl$x_max)

  grobs <- ggplotGrob(
            plot_map(pred_maps[[1]],'test',"left"))$grobs
  legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

  bl_plot <- ggplot() + ylim(0,1) +
    theme_bw() +
    theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        axis.text.y = element_blank(), axis.ticks.y=element_blank(),
        axis.text.x = element_text(size=10)) +
    scale_x_continuous(NULL,
                   breaks=c(1,3,5,7),
                   labels=c('Early Holocene', 'Last glacial max',
                            'Last interglacial', 'MIS19'), limits=c(0,8))

  dist_grid <- plot_grid(eh, gm , ig, pli, nrow=1)
  dg2 <- bl_plot  + draw_plot(dist_grid, x=0, y=0, width=8, height=1)

  frm <- plot_grid(legend, dg2, nrow=1, rel_widths=c(0.2,1))

  #plot_grid(pp, apl, frm, nrow=3, rel_heights=c(1,0.6,0.7),
  #          rel_widths=c(2,1,1))

  top_rows <- plot_grid(pp, apl, nrow=2, align='v', rel_heights=c(1,0.6))

  out <- plot_grid(top_rows, frm, nrow=2, rel_heights=c(1,0.4))
  if(!is.null(draw_file)){
    out <- ggdraw() + draw_plot(out) +
        draw_image(draw_file, 1, 1, width=0.15, hjust=1.1, vjust=1.06, halign=1,valign=1)
  }
  out
}

make_plot(allsp[12])


for (i in 59){

  tryCatch({
    out <- make_plot(allsp[i])
    ggsave(paste0('species_figures/',allsp[i],'.tiff'), width=6, height=7,
         dpi=300, compression='lzw')

  }, error=function(e) cat(paste("Failed for species", allsp[i],"\n")))

}

out <- make_plot('Acanthisitta_chloris')
ggsave(paste0('figures/','Acanthisitta_chloris','.tiff'), width=6, height=7,
         dpi=300, compression='lzw')

