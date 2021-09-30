# Load required functions and data
source('Fig3_SI3_setup.R')

# Make appendix with all individual species plots------------------------------

# Function to make an individual species plot in SI 3
make_plot <- function(species){

  draw_file <-  get_drawing(species)

  if(species=="Apteryx_haastii"){
    psmc_species <- "Apteryx_haasti"
  } else if(species=="Dryobates_pubescens"){
    psmc_species <- "Picoides_pubescens"
  } else{
    psmc_species <- species
  }

  bcols <- terrain.colors(5)[1:4]

  ppl <- plot_psmc(zipped=paste0('data/data/psmc/',psmc_species,'.tar.gz'),
                  'chocolate2',
                  bcols, FALSE)
  pp <- ppl$plot

  pred_maps <- readRDS(paste0('data2/maps/',species,'_maps.Rds'))
  pl_map <- readRDS(paste0('data2/maps/',species,'_pliest_maps.Rds'))

  eh <- plot_map(pred_maps$earlyholo, title=NULL, bcolor=bcols[1])
  ig <- plot_map(pred_maps$interglacial, title=NULL, bcolor=bcols[3])
  gm <- plot_map(pred_maps$glacialmax, title=NULL, bcolor=bcols[2])
  pli <- plot_map(pl_map$pliest, title=NULL, bcolor=bcols[4])

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

  top_rows <- plot_grid(pp, apl, nrow=2, align='v', rel_heights=c(1,0.6))

  out <- plot_grid(top_rows, frm, nrow=2, rel_heights=c(1,0.4))
  if(!is.null(draw_file)){
    out <- ggdraw() + draw_plot(out) +
        draw_image(draw_file, 1, 1, width=0.15, hjust=1.1, vjust=1.06, halign=1,valign=1)
  }
  out
}

# Generate PDF with all plots
pdf("figures/species_figures_highres.pdf", paper="letter")
for (i in sort(allsp)){
  out <- make_plot(i)
  plot(out)
  cat(i,"\n")
}
dev.off()

# Compress PDF
file.copy("figures/species_figures_highres.pdf", "figures/species_figures_print.pdf",
          overwrite=TRUE)
tools::compactPDF("figures/species_figures_print.pdf", gs_quality="printer")


# Make figure 3 for paper------------------------------------------------------

# Modified plotting code excluding habitat area plot
make_plot2 <- function(species, yaxis=TRUE, yvjust=0){

  draw_file <-  get_drawing(species)

  if(species=="Apteryx_haastii"){
    psmc_species <- "Apteryx_haasti"
  } else if(species=="Dryobates_pubescens"){
    psmc_species <- "Picoides_pubescens"
  } else{
    psmc_species <- species
  }
  ppl <- plot_psmc(zipped=paste0('data/data/psmc/',psmc_species,'.tar.gz'),
                  'chocolate2',
                  terrain.colors(5)[1:4],
                  TRUE)
  pp <- ppl$plot + theme(plot.title=element_blank()) + ylim(0,1.1) +
    theme(axis.title.y=element_text(vjust=yvjust), plot.margin=unit(c(0.2,0.2,0.8,0),"cm"),
          axis.title.x=element_blank())

  if(!yaxis) pp <- pp + theme(axis.text.y=element_blank(),
                              axis.title.y=element_blank(),
                              axis.ticks.y=element_blank())

  pred_maps <- readRDS(paste0('data2/maps/',species,'_maps.Rds'))
  pl_map <- readRDS(paste0('data2/maps/',species,'_pliest_maps.Rds'))

  bcols <- terrain.colors(5)[1:4]
  present <- plot_map(pred_maps$present, title="Present day", bcolor="white")
  eh <- plot_map(pred_maps$earlyholo, title=NULL, bcolor=bcols[1])
  ig <- plot_map(pred_maps$interglacial, title=NULL, bcolor=bcols[3])
  gm <- plot_map(pred_maps$glacialmax, title=NULL, bcolor=bcols[2])
  pli <- plot_map(pl_map$pliest, title=NULL, bcolor=bcols[4])

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
                   labels=c('Early\nHolocene', 'Last\nglacial max',
                            'Last\ninterglacial', 'MIS19'), limits=c(0,8))

  dist_grid <- plot_grid(eh, gm , ig, pli, nrow=1)
  dg2 <- bl_plot  + draw_plot(dist_grid, x=0, y=0, width=8, height=1)

  frm <- dg2

  top_rows <- pp

  out <- plot_grid(top_rows, frm, nrow=2, rel_heights=c(1,0.6), align="v")

  out
}

# Make habitat map legend
pred_maps <- readRDS(paste0('data2/maps/','Calypte_anna','_maps.Rds'))
grobs <- ggplotGrob(plot_map(pred_maps[[1]], 'test',"left"))$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

# Make individual plots
pl1 <- make_plot2('Corvus_brachyrhynchos', yvjust=7) +
  theme(plot.margin=unit(c(0,0.1,0,1),"cm"))

pl2 <- make_plot2('Buceros_rhinoceros', yaxis=FALSE) +
  theme(plot.margin=unit(c(0,0.2,0,0),"cm"))

# Combine plots
plot_grid(pl1, pl2, rel_widths=c(1,0.85)) + draw_plot(legend, -0.44, -0.3) +
  draw_image("drawings/corvus_brachyrhynchos.png", 0.14, 0.38, width=0.15) +
  draw_image("drawings/buceros_rhinoceros.png", 0.58, 0.39, width=0.15) +
  draw_text("Time since present (years)", 0.55,0.40, size=12)

# Save figure
ggsave("figures/fig3_psmc_maps.tiff", compression='lzw', dpi=300, width=8, height=4.5)
