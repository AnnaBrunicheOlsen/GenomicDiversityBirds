# Contains functions needed to generate SI 3 and Figure 3

library(ggplot2)
library(cowplot)
library(raster)
library(sf)
library(RStoolbox)

# Load required data-----------------------------------------------------------

# Get species
sp <- read.csv("data/dat_all.csv", header=TRUE)$Species
nsp <- length(sp)

# Load predictive maps
all_map <- gsub("_maps.Rds", "", list.files('data2/maps'), fixed=TRUE)
all_map <- unique(gsub("_pliest", "", all_map))

# Load PSMC data
all_psmc <- gsub(".tar.gz", "", list.files("data/data/psmc"))

# Adjust species names
all_map[all_map == "Anser_cygnoid"] <- "Anser_cygnoides"
all_psmc[all_psmc == "Apteryx_haasti"] <- "Apteryx_haastii"
all_psmc[all_psmc == "Picoides_pubescens"] <- "Dryobates_pubescens"

# Adjust species list
allsp <- all_psmc[all_psmc %in% all_map]
allsp <- allsp[allsp %in% sp]

# Get bird drawing file if it exists-------------------------------------------
get_drawing <- function(species){
  draw_file <- paste0("data2/drawings/",tolower(species),".png")
  if(file.exists(draw_file)) return(draw_file)
  return(NULL)
}

# PSMC trajectories + bootstraps-----------------------------------------------

plot_psmc <- function(zipped, linecol, cols, xtitle=TRUE){

  sp <- gsub(".tar.gz", "", basename(zipped))
  tmp_dir <- tempdir()
  untar(zipped, exdir=tmp_dir)
  sp_dir <- paste0(tmp_dir,'/',sp)

  nboots <- length(list.files(sp_dir, pattern=".txt"))-1
  if(nboots < 30) cat("Boots less than 30\n")

  template <- list.files(sp_dir, pattern="\\.0\\.txt$")
  fbase <- gsub("0.txt", "", template)

  dat <- get_plot_data(paste0(sp_dir, "/", fbase, '0.txt'))

  pop_max <- max(dat$pop)

  bs_files <- paste0(sp_dir, "/", fbase, 1:30, '.txt')
  bs <- lapply(bs_files, get_plot_data)

  bs <- lapply(bs, function(x){
          x$pop <- x$pop/pop_max
          x
         })
  dat$pop <- dat$pop/pop_max

  eholo <- c(8.326e3,11.7e3)
  gmax <- 21000
  iglacial <- c(120000,140000)
  pliest <- 787000

  x_min <- 8.326e3
  x_max <- max(pliest, max(dat$time, na.rm=T), na.rm=T)

  nbmax <- max(sapply(bs, function(x) max(x$pop,na.rm=T)),na.rm=T)

  nbmin <- min(sapply(bs, function(x) min(x$pop,na.rm=T)),na.rm=T)
  ymax <- min(nbmax*1.2, max(dat$pop, na.rm=T)*1.2)
  ymin <- max(nbmin*0.9, min(dat$pop, na.rm=T)*0.9)

  out <- ggplot(data=dat, aes(x=time, y=pop)) +
    theme_bw() +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          axis.title=element_text(size=12),
          axis.text=element_text(size=12),
          plot.title = element_text(hjust = 0.5, size=14)) +
    geom_rect(data=dat[1,],xmin=log10(eholo[1]),xmax=log10(eholo[2]),ymin=-Inf,ymax=Inf,
              fill=cols[1], alpha=0.3) +
    geom_vline(xintercept=gmax, col=cols[2], size=2, alpha=0.3) +
    geom_rect(data=dat[1,],xmin=log10(iglacial[1]),xmax=log10(iglacial[2]),ymin=-Inf,ymax=Inf,
              fill=cols[3], alpha=0.3) +
    geom_vline(xintercept=pliest, col=cols[4], size=2, alpha=0.3) +
    scale_x_log10("Time since present (years)",
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        limits=c(x_min,x_max),
        labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    ylab(expression(Normalised~italic(Ne))) +
    ggtitle(bquote(italic(.(gsub("_", " ", sp)))))

  if(!xtitle){
    out <- out + theme(axis.title.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.text.x = element_blank())
  }

  for (i in 1:length(bs)){
    bs_sub <- bs[[i]]
    out <- out + geom_step(data=bs_sub, col=linecol, alpha=0.1)
  }

  out <- out + geom_step(size=1, col=linecol)

  return(list(plot=out, x_max=x_max))
}

get_plot_data <- function(file){
  tab_cut <- read.table(file)
  names(tab_cut) <- c("time", "pop")
  tab_cut
}


# Plots of summed total suitable area------------------------------------------

area_plot <- function(pred_maps, pl_map, x_max=787000){

  area_dat <- data.frame(
      names=letters[1:4],
      area=unlist(get_areas(c(pred_maps, pl_map["pliest"])))[c(4,3,2,6)],
      time=c(mean(c(8.326e3,11.7e3)),21000,130000,787000))

  area_adjust <- area_dat$area/max(area_dat$area)

  area_dat %>%
  ggplot(aes(x=time, y=area_adjust)) +
    theme_bw() +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          axis.title=element_text(size=12),
          axis.text=element_text(size=12),
          plot.title = element_text(hjust = 0.5, size=12)) +
    scale_x_log10("Time since present (years)",
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        limits=c(8.326e3,x_max),
        labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    ylab("Normalised area") +
    geom_line(linetype=2) +
    geom_point(aes(col=names), size=3) +
    scale_color_manual(values=terrain.colors(5)[1:4]) +
    theme(legend.position='none')
}

# Get suitable area
get_areas <- function(maps){

  lapply(maps, function(x){
    crs(x) <- "+proj=longlat"
    cell_areas <- getValues(area(x))
    keep <- getValues(x)>=0.36
    sum(cell_areas[keep], na.rm=T)
  })
}


# Plot suitability maps--------------------------------------------------------
plot_map <- function(pred, title=NULL, legend="none", use_mask=NULL,
                     bcolor='white', area=FALSE, scale_cols){

  #Calculate area
  crs(pred) <- "+proj=longlat"
  if(!is.null(use_mask)){
    pred <- mask(pred, use_mask)
  }
  cell_areas <- raster::getValues(area(pred))
  keep <- raster::getValues(pred)>=0.36 #medium and high
  tot_area <- sum(cell_areas[keep], na.rm=TRUE)

  tot_area <- formatC(tot_area, format = "e", digits = 2)
  prts <- strsplit(tot_area, "e+")
  bnum <- prts[[1]][1]
  exnum <- as.numeric(gsub("+", "", prts[[1]][2]))

  out <- ggplot() +
    geom_raster(data=pred, aes(x=x,y=y,fill=layer), interpolate=F) +
    scale_fill_gradientn(colors=rev(terrain.colors(300)),limits=c(0,1),
                       na.value='white',
                       guide=guide_legend(reverse=TRUE,
                                          title.position="left",
                                          title.vjust=0.5,
                                          title.hjust=0.5,
                                          title.theme=element_text(size=12,
                                                                   angle=90))) +
    coord_quickmap() +
    theme_void() +
    theme(legend.position=legend,
          plot.subtitle=element_text(hjust=0.5),
          panel.border = element_rect(colour = bcolor, fill=NA, size=1),
          plot.title=element_text(hjust=0.5, size=12)
          )

    if(area){
      out <- out +
      labs(title = title,
         subtitle = bquote(.(bnum)*x*10^{.(exnum)}~km^{2}),
         fill='Pr(Habitat)')
    }else{
      out <- out + labs(title = title,
         fill='P(suitable habitat)')
    }
    out
}
