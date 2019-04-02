#Load libraries
library(ggplot2)
suppressMessages(library(ggpubr))
library(gridExtra)
library(grid)

source('clean_data.R')

#Run some tests
#Mass and flight are pretty correlated so I wouldn't include both
test_all <- lm(log(het)~threatened+carnivore+migrates+scale(mass)+scale(gentime)+declining, 
           data=all_data)
summary(test_all)

test <- lm(log(het)~threatened+carnivore+migrates+scale(log(mass)),
            data=all_data)

pvals <- as.character(round(summary(test)$coefficients[,4],3))
pvals <- paste0('P = ',pvals)
pvals[which(pvals=='P = 0')] <- 'P < 0.001'

#Threatened fig
pr_threat <- predict(test, se.fit=T,
                     newdata=data.frame(threatened=c(1,0),carnivore=0,migrates=0,
                                        mass=mean(all_data$mass,na.rm=T)))

fig_data <- data.frame(treat=factor(c('Threatened','Non-threatened'),
                                    levels=c('Threatened','Non-threatened')),
                       fit=pr_threat$fit, 
                       upr=pr_threat$fit+1.96*pr_threat$se.fit,
                       lwr=pr_threat$fit-1.96*pr_threat$se.fit)

nsize <- data.frame(treat=factor(c('Non-threatened','Threatened'),
                                    levels=c('Threatened','Non-threatened')),
                    n = paste0('n = ',table(all_data$threatened)),
                    yval=1.03*min(fig_data$lwr))

redlist <- fig_data %>% 
  ggplot(aes(x=treat,y=fit,col=treat)) +
  scale_color_manual(values=get_palette('npg',2)) +
  geom_point(size=3) +
  geom_errorbar(aes(x=treat,ymin=lwr,ymax=upr,col=treat),size=1.2,width=0.2) +
  ylab(expression(log(italic(H)))) +
  xlab('Red List status') +
  theme_bw() +
  theme(  axis.title.y=element_blank(),
          axis.title=element_text(size=14),
          axis.text=element_text(size=14,color="black"),
          #panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"),
          legend.position='none',
          strip.background =element_rect(fill="white"),
          strip.text = element_text(size=18)) +
  annotate("text",1.5,0.97*max(fig_data$upr),label=pvals[2], size=5) +
  geom_text(aes(x=treat,y=yval,label=n),data=nsize,color='black',size=5)


#Carnivore fig
pr_carn <- predict(test, se.fit=T,
                     newdata=data.frame(threatened=0,carnivore=c(0,1),migrates=0,
                                        mass=mean(all_data$mass,na.rm=T)))

fig_data <- data.frame(treat=c('Non-carnivore','Carnivore'),
                       fit=pr_carn$fit, 
                       upr=pr_carn$fit+1.96*pr_carn$se.fit,
                       lwr=pr_carn$fit-1.96*pr_carn$se.fit)

nsize <- data.frame(treat=c('Non-carnivore','Carnivore'),
                    n = paste0('n = ',table(all_data$carnivore)),
                    yval=1.03*min(fig_data$lwr))

carnivore <- fig_data %>% 
  ggplot(aes(x=treat,y=fit,col=treat)) +
  scale_color_manual(values=get_palette('npg',2)) +
  geom_point(size=3) +
  geom_errorbar(aes(x=treat,ymin=lwr,ymax=upr,col=treat),size=1.2,width=0.2) +
  ylab(expression(log(italic(H)))) +
  xlab('Diet type') +
  theme_bw() +
  theme(  axis.title.y=element_blank(),
          axis.title=element_text(size=14),
          axis.text=element_text(size=14,color="black"),
          #panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"),
          legend.position='none',
          strip.background =element_rect(fill="white"),
          strip.text = element_text(size=18)) +
  annotate("text",1.5,0.97*max(fig_data$upr),label=pvals[3], size=5) +
  geom_text(aes(x=treat,y=yval,label=n),data=nsize,color='black',size=5)

#Migration status
pr_mig <- predict(test, se.fit=T,
                     newdata=data.frame(threatened=0,carnivore=0,migrates=c(1,0),
                                        mass=mean(all_data$mass,na.rm=T)))

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
  geom_point(size=3) +
  geom_errorbar(aes(x=treat,ymin=lwr,ymax=upr,col=treat),size=1.2,width=0.2) +
  ylab(expression(log(italic(H)))) +
  xlab('Migration status') +
  theme_bw() +
  theme(  axis.title.y=element_blank(),
          axis.title=element_text(size=14),
          axis.text=element_text(size=14,color="black"),
          #panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"),
          legend.position='none',
          strip.background =element_rect(fill="white"),
          strip.text = element_text(size=18)) +
  annotate("text",1.5,0.97*max(fig_data$upr),label=pvals[4], size=5) +
  geom_text(aes(x=treat,y=yval,label=n),data=nsize,color='black',size=5)

#Mass
mass_seq <- seq(min(all_data$mass,na.rm=T), max(all_data$mass,na.rm=T),
                length.out=300)
pr_mass <- predict(test, se.fit=T,
                  newdata=data.frame(threatened=0,carnivore=0,migrates=0,
                                        mass=mass_seq))
                                                 
fig_data <- data.frame(mass=log(mass_seq),
                       fit=pr_mass$fit, 
                       upr=pr_mass$fit+1.96*pr_mass$se.fit,
                       lwr=pr_mass$fit-1.96*pr_mass$se.fit)

mcol <- get_palette('npg',2)[1]
mass_fig <- fig_data %>% 
  ggplot(aes(x=mass,y=fit),) +
  geom_line(size=1.2,col=mcol) +
  geom_ribbon(aes(x=mass,ymin=lwr,ymax=upr),fill=mcol,alpha=0.4) +
  ylab(expression(log(italic(H)))) +
  xlab('log(body mass)') +
  theme_bw() +
  theme(  axis.title.y=element_blank(),
          axis.title=element_text(size=14),
          axis.text=element_text(size=14,color="black"),
          #panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"),
          legend.position='none',
          strip.background =element_rect(fill="white"),
          strip.text = element_text(size=14)) +
  annotate("text",mean(log(range(all_data$mass,na.rm=T))),
           0.99*max(fig_data$upr),label=pvals[5], size=5)

#########

grid.arrange(redlist,carnivore,mig,mass_fig,ncol=2,nrow=2,
             left=textGrob(expression(log(italic(H))),rot=90,
                           gp=gpar(fontsize=18)))
