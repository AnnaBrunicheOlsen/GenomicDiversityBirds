library(ggplot2)

get_plot_data <- function(file){
  tab <- read.table(file)  
  w <- which.max(cummax(tab$V2 != tab$V2[1])) - 1  
  tab_cut <- tab[w:nrow(tab),1:2]
  names(tab_cut) <- c("time", "pop")
  tab_cut
}

psmc_plot <- function(zipped){
  
  sp <- gsub(".zip", "", zipped)
  tmp_dir <- tempdir()
  unzip(zipped, exdir=tmp_dir)
  sp_dir <- paste0(tmp_dir,'/',sp)
  
  fbase <- paste0(sp_dir, "/", sp, ".psmc.")
  
  dat <- get_plot_data(paste0(fbase, '0.txt'))

  bs_files <- paste0(fbase, 1:100, '.txt')
  bs <- lapply(bs_files, get_plot_data)

  out <- ggplot(data=dat, aes(x=time, y=pop)) +
    theme_bw() +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          axis.title=element_text(size=14),
          axis.text=element_text(size=14),
          plot.title = element_text(hjust = 0.5, size=14)) +
    scale_x_log10("Time since present (years)",
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    scale_y_continuous(breaks = round(seq(0, round(max(dat$pop))+2, by = 2),1)) +
    ylab(expression(paste("Effective population size (x", 10^4, ")"))) +
    ggtitle(bquote(italic(.(gsub("_", " ", sp)))))

  for (i in 1:length(bs)){
    out <- out + geom_step(data=bs[[i]], col='red', alpha=0.1)
  }

  out <- out + geom_step(size=1, col='black')
  out
}

psmc_plot('Amazona_vittata.zip')

ggsave("Amazona_vittata.png", width=6, height=4)
