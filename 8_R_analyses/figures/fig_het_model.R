#Load libraries
library(ggplot2)
suppressMessages(library(ggpubr))
library(gridExtra)
library(grid)

setwd('..')
source('clean_data.R')
setwd('figures')

#Run some tests
test <- lm(log(het)~carnivore+migrates+
           scale(log(repro)), data=all_data)
pvals <- as.character(round(summary(test)$coefficients[,4],3))
pvals <- paste0('P = ',pvals)
pvals[which(pvals=='P = 0')] <- 'P < 0.001'

#Carnivore fig
pr_carn <- predict(test, se.fit=T,
                     newdata=data.frame(carnivore=c(0,1),migrates=0,
                                        repro=mean(all_data$repro,na.rm=T)))

fig_data <- data.frame(treat=c('Non-carnivore','Carnivore'),
                       fit=pr_carn$fit, 
                       upr=pr_carn$fit+1.96*pr_carn$se.fit,
                       lwr=pr_carn$fit-1.96*pr_carn$se.fit)

nsize <- data.frame(treat=c('Non-carnivore','Carnivore'),
                    n = paste0('n = ',table(all_data$carnivore)),
                    yval=1.03*min(fig_data$lwr))

raw_dat <- all_data %>%
  rename(fit=het) %>%
  mutate(fit = log(fit),
         treat = ifelse(carnivore==0,'Non-carnivore','Carnivore')) %>%
  select(fit,treat)

carnivore <- fig_data %>% 
  ggplot(aes(x=treat,y=fit,col=treat)) +
  scale_color_manual(values=get_palette('npg',2)) +
  #geom_point(data=raw_dat,aes(x=treat,y=fit),col='black',alpha=0.5) +
  geom_point(size=2) +
  geom_errorbar(aes(x=treat,ymin=lwr,ymax=upr,col=treat),size=1,width=0.2) +
  ylab(expression(log(italic(H)))) +
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
  annotate("text",1.5,0.97*max(fig_data$upr),label=pvals[2], size=3) +
  geom_text(aes(x=treat,y=yval,label=n),data=nsize,color='black',size=3)

#Migration status
pr_mig <- predict(test, se.fit=T,
                     newdata=data.frame(carnivore=0,migrates=c(1,0),
                                        repro=mean(all_data$repro,na.rm=T)))

fig_data <- data.frame(treat=factor(c('Migrates','Does not migrate'),
                                    levels=c('Migrates','Does not migrate')),
                       fit=pr_mig$fit, 
                       upr=pr_mig$fit+1.96*pr_mig$se.fit,
                       lwr=pr_mig$fit-1.96*pr_mig$se.fit)

nsize <- data.frame(treat=factor(c('Does not migrate','Migrates'),
                                    levels=c('Migrates','Does not migrate')),
                    n = paste0('n = ',table(all_data$migrates)),
                    yval=1.03*min(fig_data$lwr))

mig <- fig_data %>% 
  ggplot(aes(x=treat,y=fit,col=treat)) +
  scale_color_manual(values=get_palette('npg',2)) +
  geom_point(size=2) +
  geom_errorbar(aes(x=treat,ymin=lwr,ymax=upr,col=treat),size=1,width=0.2) +
  ylab(expression(log(italic(H)))) +
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
  annotate("text",1.5,0.97*max(fig_data$upr),label=pvals[3], size=3) +
  geom_text(aes(x=treat,y=yval,label=n),data=nsize,color='black',size=3)

#Repro
repro_seq <- seq(min(all_data$repro,na.rm=T), max(all_data$repro,na.rm=T),
                length.out=300)
pr_repro <- predict(test, se.fit=T,
                  newdata=data.frame(carnivore=0,migrates=0,
                                        repro=repro_seq))
                                                 
fig_data <- data.frame(repro=log(repro_seq),
                       fit=pr_repro$fit, 
                       upr=pr_repro$fit+1.96*pr_repro$se.fit,
                       lwr=pr_repro$fit-1.96*pr_repro$se.fit)

mcol <- get_palette('npg',2)[1]
repro_fig <- fig_data %>% 
  ggplot(aes(x=repro,y=fit),) +
  geom_line(size=1,col=mcol) +
  geom_ribbon(aes(x=repro,ymin=lwr,ymax=upr),fill=mcol,alpha=0.4) +
  ylab(expression(log(italic(H)))) +
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
           0.99*max(fig_data$upr),label=pvals[4], size=3)

#########
tiff('fig_het_model.tiff',height=4,width=4,units='in',res=300)
grid.arrange(carnivore,mig,repro_fig,ncol=2,nrow=2,
             left=textGrob(expression(log(italic(H))),rot=90,
                           gp=gpar(fontsize=14)))
dev.off()
