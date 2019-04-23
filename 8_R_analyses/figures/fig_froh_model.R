#Load libraries
library(ggplot2)
suppressMessages(library(ggpubr))
library(gridExtra)
library(grid)

setwd('..')
source('clean_data.R')
setwd('figures')


carn_test <- wilcox.test(Froh~carnivore, data=all_data)
mig_test <- wilcox.test(Froh~migrates, data=all_data)
repro_test <- cor.test(all_data$Froh, all_data$repro, method='spearman')

pval_raw <- c(carn_test$p.value, mig_test$p.value, repro_test$p.value)

pvals <- as.character(round(pval_raw,3))
pvals <- paste0("P = ",pvals)
pvals[which(pvals=='P = 0')] <- 'P < 0.001'

#Carnivore fig
fig_data <- all_data %>%
  mutate(treat = ifelse(carnivore==0,'Non-carnivore','Carnivore'))


nsize <- data.frame(treat=c('Non-carnivore','Carnivore'),
                    n = paste0('n = ',table(all_data$carnivore)),
                    yval=-15)

carnivore <- fig_data %>% 
  ggplot(aes(x=treat,y=log(Froh),fill=treat)) +
  scale_fill_manual(values=get_palette('npg',2)) +
  #geom_point(data=raw_dat,aes(x=treat,y=fit),col='black',alpha=0.5) +
  geom_violin(draw_quantiles=0.5) +
  ylab(expression(log(F[ROH]))) +
  xlab('Diet type') +
  theme_bw() +
  theme(  axis.title.y=element_blank(),
          axis.title=element_text(size=8),
          axis.text=element_text(size=6,color="black"),
          #panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"),
          legend.position='none',
          strip.background =element_rect(fill="white"),
          strip.text = element_text(size=18)) +
  annotate("text",1.5,0.9*max(log(fig_data$Froh)),label=pvals[1], size=2) +
  geom_text(aes(x=treat,y=yval,label=n),data=nsize,color='black',size=2)

#Migration status
fig_data <- all_data %>%
  mutate(treat = ifelse(migrates==0,'Migrates','Does not migrate'))

nsize <- data.frame(treat=c('Migrates','Does not migrate'),
                    n = paste0('n = ',table(all_data$migrates)),
                    yval=-15)

mig <- fig_data %>% 
  ggplot(aes(x=treat,y=log(Froh),fill=treat)) +
  scale_fill_manual(values=get_palette('npg',2)) +
  #geom_point(data=raw_dat,aes(x=treat,y=fit),col='black',alpha=0.5) +
  geom_violin(draw_quantiles=0.5) +
  ylab(expression(log(F[ROH]))) +
  xlab('Migration status') +
  theme_bw() +
  theme(  axis.title.y=element_blank(),
          axis.title=element_text(size=8),
          axis.text=element_text(size=6,color="black"),
          #panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"),
          legend.position='none',
          strip.background =element_rect(fill="white"),
          strip.text = element_text(size=18)) +
  annotate("text",1.5,0.9*max(log(fig_data$Froh)),label=pvals[2], size=2) +
  geom_text(aes(x=treat,y=yval,label=n),data=nsize,color='black',size=2)


#Repro

mcol <- get_palette('npg',2)[1]
repro_fig <- all_data %>% 
  ggplot(aes(x=log(repro),y=log(Froh))) +
  geom_point(size=1,col=mcol) +
  ylab(expression(log(F[ROH]))) +
  xlab('log(reproductive capacity)') +
  theme_bw() +
  theme(  axis.title.y=element_blank(),
          axis.title=element_text(size=8),
          axis.text=element_text(size=6,color="black"),
          #panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"),
          legend.position='none',
          strip.background =element_rect(fill="white"),
          strip.text = element_text(size=10)) +
  annotate("text",mean(log(range(all_data$repro,na.rm=T))),
           0,label=pvals[3], size=2)


#########
tiff('fig_froh_model.tiff',height=4,width=4,units='in',res=300)
grid.arrange(carnivore,mig,repro_fig,ncol=2,nrow=2,
             left=textGrob(expression(log(F[ROH])),rot=90,
                           gp=gpar(fontsize=14)))
dev.off()
