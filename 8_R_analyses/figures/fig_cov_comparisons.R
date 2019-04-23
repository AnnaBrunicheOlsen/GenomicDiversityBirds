#Load libraries
library(ggplot2)
suppressMessages(library(ggpubr))
library(gridExtra)
library(grid)

setwd('..')
source('clean_data.R')
setwd('figures')

my_theme <- theme(  
          axis.title=element_text(size=8),
          axis.text=element_text(size=6,color="black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size=6),
          legend.key.size = unit(0.25,'cm'),
          legend.margin=margin(c(1,1,1,1)),
          strip.background =element_rect(fill="white"),
          strip.text = element_text(size=18))


plot_data <- cov_data %>%
  mutate(Diet_5Cat = fct_recode(Diet_5Cat, 'PS'='PlantSeed', 'FN'='FruiNect',
                      'I'='Invertebrate','O'='Omnivore','C'='VertFishScav'))


tiff('fig_cov_comparisons.tiff',height=6,width=4,units='in',res=300)

#thr4 vs flying
thr4_fly <- plot_data %>%
  filter(!is.na(thr4)) %>%
  group_by(thr4) %>%
  summarize(flying=mean(flying)) %>%
  mutate(`non-flying` = 1-flying) %>%
  gather(key='mobility',value='pct',2:3) %>% 
  ggplot(aes(x=thr4, fill=mobility)) +
  geom_bar(aes(y=pct),stat='identity') +
  xlab('Red List Status') + ylab('Proportion') +
  theme_bw() +
  scale_fill_manual(values=get_palette('npg',2)) +
  my_theme +
  theme(legend.text = element_text(size=6),
        legend.position=c(0.72,0.7))

#Diet vs mass
pairwise.t.test(log(plot_data$mass),plot_data$Diet_5Cat)

diffs <- data.frame(diet=factor(levels(plot_data$Diet_5Cat),levels=
                           levels(plot_data$Diet_5Cat)),
                    label=c('ab','ab','a','ab','b'))

diet_mass <- plot_data %>%
  ggplot(aes(x=Diet_5Cat,y=log(mass),fill=Diet_5Cat)) +
  scale_fill_manual(values=get_palette('npg',5)) +
  geom_violin(draw_quantiles=0.5) +
  xlab('Diet type') +
  ylim(2,14) +
  theme_bw() +
  my_theme +
  theme(legend.position='none') +
  annotate("text", x=diffs$diet, y=13.5, label=diffs$label,size=3)

#Diet vs gentime
pairwise.t.test(plot_data$gentime,plot_data$Diet_5Cat)

diffs <- data.frame(diet=factor(levels(plot_data$Diet_5Cat),levels=
                           levels(plot_data$Diet_5Cat)),
                    label=c('ab','ab','a','ab','b'))

diet_gen <- plot_data %>%
  ggplot(aes(x=Diet_5Cat,y=gentime,fill=Diet_5Cat)) +
  scale_fill_manual(values=get_palette('npg',5)) +
  geom_violin(draw_quantiles=0.5) +
  xlab('Diet type') + ylab('Generation time') +
  theme_bw() +
  my_theme +
  ylim(0,25) +
  theme(legend.position='none') +
  annotate("text", x=diffs$diet, y=24, label=diffs$label,size=3)

#Diet vs migrates
diet_mig <- plot_data %>%
  filter(!is.na(Diet_5Cat)) %>%
  group_by(Diet_5Cat) %>%
  summarize(migrates=mean(migrates)) %>%
  mutate(`doesn't migrate` = 1-migrates) %>%
  gather(key='migrates',value='pct',2:3) %>% 
  ggplot(aes(x=Diet_5Cat, fill=migrates)) +
  geom_bar(aes(y=pct),stat='identity') +
  xlab('Diet type') + ylab('Proportion') +
  theme_bw() +
  scale_fill_manual(values=get_palette('npg',2)) +
  my_theme +
  theme(legend.text = element_text(size=6),
        legend.position=c(0.72,0.3))

#Gentime vs mass
fit <- lm(gentime ~ log(mass), data=plot_data)
r2 <- summary(fit)$r.squared

gen_mass <- plot_data %>%
  ggplot(aes(x=log(mass),y=gentime)) +
  geom_point(size=1,color=get_palette('npg',1)) +
  geom_smooth(method='lm',col='black') +
  ylab('Generation time') +
  theme_bw() +
  my_theme +
  annotate('text',9,1,label='R^{2}==0.45',parse=T,size=3)

#Flying vs mass
fly_mass <- plot_data %>%
  filter(!is.na(flying)) %>%
  mutate(flying=ifelse(flying==1,'flying','non-flying')) %>%
  ggplot(aes(x=flying,y=log(mass),fill=flying)) +
  scale_fill_manual(values=get_palette('npg',2)) +
  geom_violin(draw_quantiles=0.5) +
  xlab('Flight status') +
  ylim(2,13) +
  theme_bw() +
  my_theme +
  theme(legend.position='none') +
  annotate("text", x=1.5, y=12.5, label='P < 0.001',size=3)

#Flying vs genetime
fly_gen <- plot_data %>%
  filter(!is.na(flying)) %>%
  mutate(flying=ifelse(flying==1,'flying','non-flying')) %>%
  ggplot(aes(x=flying,y=gentime,fill=flying)) +
  scale_fill_manual(values=get_palette('npg',2)) +
  geom_violin(draw_quantiles=0.5) +
  xlab('Flight status') + ylab('Generation time') +
  theme_bw() +
  my_theme +
  ylim(0,25) +
  theme(legend.position='none') +
  annotate("text", x=1.5, y=24, label='P < 0.001',size=3)


grid.arrange(thr4_fly, diet_mass, 
             diet_gen, diet_mig, 
             gen_mass, fly_mass,
             fly_gen, ncol=2)

dev.off()
