source('plot_functions.R')

files <- paste0("data/data/psmc/",list.files('data/data/psmc'))

get_traj <- function(zipped){
  sp <- gsub(".tar.gz", "", basename(zipped))
  tmp_dir <- tempdir()
  #unzip(zipped, exdir=tmp_dir)
  untar(zipped, exdir=tmp_dir)
  sp_dir <- paste0(tmp_dir,'/',sp)

  nboots <- length(list.files(sp_dir, pattern=".txt"))-1

  template <- list.files(sp_dir, pattern="\\.0\\.txt$")
  fbase <- gsub("0.txt", "", template)

  dat <- get_plot_data(paste0(sp_dir, "/", fbase, '0.txt'))

  pop_max <- max(dat$pop)
  pop_norm <- dat$pop / pop_max
  data.frame(species=sp, time=dat$time, pop=dat$pop, pop_norm=pop_norm)
}

panel1_dat <- lapply(files, get_traj)
panel1_dat <- do.call("rbind", panel1_dat)

eholo <- c(8.326e3,11.7e3)
  gmax <- 21000
  iglacial <- c(120000,140000)
  pliest <- 787000
  x_min <- 8.326e3
  x_max <- max(pliest, max(dat$time, na.rm=T), na.rm=T)

  ymax <- max(dat$pop_norm, na.rm=T)*1.2
  ymin <- min(dat$pop_norm, na.rm=T)*0.9

ggplot(data=panel1_dat, aes(x=time, y=pop_norm, group=species)) +

    #geom_rect(xmin=log10(eholo[1]),xmax=log10(eholo[2]),ymin=-Inf,ymax=Inf,
    #          fill=cols[1], alpha=0.2) +
    #geom_vline(xintercept=gmax, col=cols[2], size=2) +
    #geom_rect(xmin=log10(iglacial[1]),xmax=log10(iglacial[2]),ymin=-Inf,ymax=Inf,
    #          fill=cols[3], alpha=0.2) +
    #geom_vline(xintercept=pliest, col=cols[4], size=2) +
    scale_x_log10("Time since present (years)",
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        #limits=c(x_min,x_max),
        labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    #scale_y_continuous(breaks = round(seq(0, round(max(dat$pop))+2, length.out=4),1)) +
    #ylab(expression(paste("Effective population size (x", 10^4, ")"))) +
    ylab(expression(Normalized~italic(Ne))) +
    #ylim(0,1.1) +

    geom_step(size=1, alpha=0.2) +

    theme_classic(base_size=14)




nesize <- read_csv('data/ne_size_new.csv')
areas <- read_csv('data/enm_areas.csv')

ne_max <- panel1_dat %>%
  group_by(species) %>%
  summarize(max=max(pop))

write_csv(ne_max, "data/ne_max.csv")
