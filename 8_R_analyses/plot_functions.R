library(ggplot2)
library(cowplot)
library(raster)
library(sf)
library(RStoolbox)

get_areas <- function(maps){

  lapply(maps, function(x){
    crs(x) <- "+proj=longlat"
    cell_areas <- getValues(area(x))
    keep <- getValues(x)>=0.36
    sum(cell_areas[keep], na.rm=T)
  })
}

plot_map <- function(pred, title=NULL, legend="none", use_mask=NULL,
                     bcolor='white', area=FALSE, scale_cols){
  
  #Calculate area
  crs(pred) <- "+proj=longlat"
  if(!is.null(use_mask)){
    pred <- mask(pred, use_mask)
  }
  cell_areas <- getValues(area(pred))
  keep <- getValues(pred)>=0.36 #medium and high
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
    #scale_fill_viridis(limits=c(0,1), na.value='white',option='viridis') +
    coord_quickmap() +
    theme_void() +
    theme(legend.position=legend,
          #panel.border=element_rect(fill='transparent'),
          plot.subtitle=element_text(hjust=0.5),
          plot.title=element_text(hjust=0.5, size=12)
          #plot.title=element_text(face='bold')
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

get_plot_data <- function(file){
  tab_cut <- read.table(file)  
  #w <- which.max(cummax(tab$V2 != tab$V2[1])) - 1  
  #tab_cut <- tab[w:nrow(tab),1:2]
  names(tab_cut) <- c("time", "pop")
  tab_cut
}

plot_psmc <- function(zipped, linecol, cols, xtitle=TRUE){
  
  #sp <- gsub(".zip", "", basename(zipped)) 
  sp <- gsub(".tar.gz", "", basename(zipped))
  tmp_dir <- tempdir()
  #unzip(zipped, exdir=tmp_dir)
  untar(zipped, exdir=tmp_dir)
  sp_dir <- paste0(tmp_dir,'/',sp)
  
  nboots <- length(list.files(sp_dir, pattern=".txt"))-1

  template <- list.files(sp_dir, pattern="\\.0\\.txt$")
  fbase <- gsub("0.txt", "", template)

  dat <- get_plot_data(paste0(sp_dir, "/", fbase, '0.txt'))

  bs_files <- paste0(sp_dir, "/", fbase, 1:nboots, '.txt')
  bs <- lapply(bs_files, get_plot_data)
  
  eholo <- c(8.326e3,11.7e3)
  gmax <- 21000
  iglacial <- c(120000,140000)
  pliest <- 787000

  #x_min <- min(eholo[1], min(dat$time,na.rm=T),na.rm=T)
  #x_min <- max(10^4, x_min)
  x_min <- 8.326e3
  x_max <- max(pliest, max(dat$time, na.rm=T), na.rm=T)

  nbmax <- max(sapply(bs, function(x) max(x$pop,na.rm=T)),na.rm=T)

  nbmin <- min(sapply(bs, function(x) min(x$pop,na.rm=T)),na.rm=T)
  ymax <- min(nbmax*1.2, max(dat$pop, na.rm=T)*1.2)
  ymin <- max(nbmin*0.9, min(dat$pop, na.rm=T)*0.9)

  #dat <- dat[dat$time>=eholo[1],]

  out <- ggplot(data=dat, aes(x=time, y=pop)) +
    theme_bw() +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          axis.title=element_text(size=12),
          axis.text=element_text(size=12),
          plot.title = element_text(hjust = 0.5, size=14)) +
    geom_rect(xmin=log10(eholo[1]),xmax=log10(eholo[2]),ymin=-Inf,ymax=Inf,
              fill=cols[1], alpha=0.2) +
    geom_vline(xintercept=gmax, col=cols[2], size=2) +
    geom_rect(xmin=log10(iglacial[1]),xmax=log10(iglacial[2]),ymin=-Inf,ymax=Inf,
              fill=cols[3], alpha=0.2) +
    geom_vline(xintercept=pliest, col=cols[4], size=2) +
    scale_x_log10("Time since present (years)",
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        limits=c(x_min,x_max),
        labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    #scale_y_continuous(breaks = round(seq(0, round(max(dat$pop))+2, length.out=4),1)) +
    ylab(expression(paste("Effective population size (x", 10^4, ")"))) +
    ylim(ymin,ymax) +
    ggtitle(bquote(italic(.(gsub("_", " ", sp)))))

  if(!xtitle){
    out <- out + theme(axis.title.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.text.x = element_blank())
  }

  for (i in 1:length(bs)){
    bs_sub <- bs[[i]][bs[[i]]$time>=eholo[1],]
    out <- out + geom_step(data=bs_sub, col=linecol, alpha=0.1)
  }

  out <- out + geom_step(size=1, col=linecol)
  return(list(plot=out, x_max=x_max))
}

area_plot <- function(pred_maps, pl_map, x_max=787000){

  area_dat <- data.frame(
      area=unlist(get_areas(c(pred_maps, pl_map["pliest"])))[c(4,3,2,6)],
      time=c(mean(c(8.326e3,11.7e3)),21000,130000,787000))

  div <- floor(log10(mean(area_dat$area)))

  area_dat$area_adjust = area_dat$area / 10^div

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
    ylab(bquote(Habitat~"("*x*10^{.(div)}~km^{2}*")")) +
    #scale_y_log10(expression(paste("Habitat (", km^2, ")")),
    #    breaks = scales::trans_breaks("log10", function(x) 10^x),
    #    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    geom_line(linetype=2) +
    geom_point(size=3,col=cols)
}

