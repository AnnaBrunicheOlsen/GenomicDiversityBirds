library(ggplot2)
suppressMessages(library(ggpubr))
library(gridExtra)
library(grid)

setwd('..')
source('clean_data.R')
setwd('figures')

my_theme <- theme(  
          axis.title=element_text(size=12),
          axis.text=element_text(size=10,color="black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size=8),
          legend.key.size = unit(0.25,'cm'),
          legend.margin=margin(c(1,1,1,1)),
          strip.background =element_rect(fill="white"),
          strip.text = element_text(size=18))


plot_data <- cov_data %>%
  mutate(Diet_5Cat = fct_recode(Diet_5Cat, 'PS'='PlantSeed', 'FN'='FruiNect',
                      'I'='Invertebrate','O'='Omnivore','C'='VertFishScav'))

#Repro vs gentime
fit <- lm(gentime ~ log(repro), data=plot_data)
r2 <- summary(fit)$r.squared
r <- cor(log(plot_data$repro), plot_data$gentime, use='complete.obs')

repro_gen <- plot_data %>%
  ggplot(aes(x=log(repro),y=gentime)) +
  geom_point(size=1,color=get_palette('npg',1)) +
  geom_smooth(method='lm',col='black') +
  ylab('Generation time') +
  xlab('log(reproductive output)') +
  theme_bw() +
  my_theme +
  theme(axis.title.x = element_blank()) +
  annotate('text',3.7,30.5,label='italic(r)==-0.55',parse=T,size=3)

#Repro vs mass
fit <- lm(log(mass) ~ log(repro), data=plot_data)
r2 <- summary(fit)$r.squared
r <- cor(log(plot_data$repro), log(plot_data$mass), use='complete.obs')

repro_mass <- plot_data %>%
  ggplot(aes(x=log(repro),y=log(mass))) +
  geom_point(size=1,color=get_palette('npg',1)) +
  geom_smooth(method='lm',col='black') +
  ylab('log(mass)') +
  xlab('log(reproductive output)') +
  theme_bw() +
  my_theme +
  theme(axis.title.x = element_blank()) +
  annotate('text',3.7,11.5,label='italic(r)==-0.33',parse=T,size=3)


tiff('fig_corrs_repro.tiff',height=4,width=4,units='in',res=300)

grid.arrange(repro_gen, repro_mass, ncol=2,
             bottom=textGrob('log(reproductive output)',
                           gp=gpar(fontsize=12)))

dev.off()
