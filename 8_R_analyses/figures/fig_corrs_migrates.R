library(ggplot2)
suppressMessages(library(ggpubr))
library(gridExtra)
library(grid)
library(cowplot)

setwd('..')
source('clean_data.R')
setwd('figures')

my_theme <- theme(  
          axis.title=element_text(size=14),
          axis.text=element_text(size=12,color="black"),
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

#Flying vs migrates
plot_data <- plot_data %>% group_by(migrates) %>%
  summarize(pct = mean(flying,na.rm=T),
            n = n()) %>%
  mutate(se = sqrt(pct*(1-pct))/sqrt(n),
         upr = pct+se,
         lwr = pct-se,
         migrates = ifelse(migrates==1,'migrates',"doesn't migrate"),
         migrates = factor(migrates,levels=c('migrates',"doesn't migrate")))


fig <- plot_data %>%
  ggplot(aes(x=migrates,y=pct,color=migrates)) +
  scale_color_manual(values=get_palette('npg',2)) +
  geom_point(size=2) +
  geom_errorbar(aes(x=migrates,ymin=lwr,ymax=upr,color=migrates),
                width=0.2) +
  ylab("Proportion flying species") +
  theme_bw() +
  my_theme +
  theme(legend.position='none', axis.title.x = element_blank())

inset_fig <- cov_data %>%
  mutate(flying = ifelse(flying==1,'flying','flightless')) %>%
  group_by(flying) %>%
  summarize(total = n()) %>%
  mutate(pct = total/sum(total)) %>%
  ggplot(aes(x="",y=pct, fill=flying)) +
  scale_fill_manual(values=get_palette('npg',6)[5:6]) +
  geom_col() +
  coord_polar("y") +
  geom_text(aes(label=paste0(round(pct*100),'%')),
                             position=position_stack(vjust=0.5),size=4) +
  theme_bw() +
  my_theme +
  theme(legend.position='bottom',
        legend.text = element_text(size=10),
        axis.title = element_blank(),
        axis.text = element_blank(),
        #panel.border= element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        )


tiff('fig_corrs_migrates.tiff',height=4,width=4,units='in',res=300)

ggdraw(fig) +
  draw_plot(inset_fig, 0.15, 0.1,0.4,0.4)


dev.off()


