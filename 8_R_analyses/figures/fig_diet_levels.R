library(ggplot2)
library(ggpubr)

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
          legend.text = element_text(size=10),
          legend.key.size = unit(0.25,'cm'),
          legend.margin=margin(c(1,1,1,1)),
          strip.background =element_rect(fill="white"),
          strip.text = element_text(size=18))


plot_data <- all_data %>%
  mutate(Diet_5Cat = fct_recode(Diet_5Cat, 'PS'='PlantSeed', 'FN'='FruiNect',
                      'I'='Invertebrate','O'='Omnivore','C'='VertFishScav'))

pairwise.t.test(log(all_data$het),all_data$Diet_5Cat)

diffs <- data.frame(diet=factor(levels(plot_data$Diet_5Cat),levels=
                           levels(plot_data$Diet_5Cat)),
                    label=c('a','ab','a','ab','b'))


tiff('fig_diet_levels.tiff',height=4,width=4,units='in',res=300)

plot_data %>%
  ggplot(aes(x=Diet_5Cat,y=log(het), fill=Diet_5Cat)) +
  scale_fill_manual(values=get_palette('npg',5)) +
  geom_violin(draw_quantiles=0.5) +
  ylab(expression(log(italic(H)))) +
  xlab('Diet Category') +
  theme_bw() +
  my_theme +
  theme(legend.position='none') +
  annotate("text",x=diffs$diet,y=-4, label=diffs$label,size=4)

dev.off()
