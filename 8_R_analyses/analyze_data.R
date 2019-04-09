#Load libraries
library(ggplot2)
suppressMessages(library(ggpubr))
library(gridExtra)
library(grid)
library(multcomp)

source('clean_data.R')

#Run some tests
#Mass and flight are pretty correlated so I wouldn't include both
#Same with reproduction and threatened status

cor(cov_data[,-c(1,10,11)],use='complete.obs')

psych::corr.test(cov_data[,-c(1,2,3,410,11)], adjust='none')


my_theme <- theme(  
          axis.title=element_text(size=14),
          axis.text=element_text(size=14,color="black"),
          #panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          axis.line = element_line(colour = "black"),
          strip.background =element_rect(fill="white"),
          strip.text = element_text(size=18))

cov_data %>% group_by(threatened) %>%
  summarize(`Percent Carnivore` = mean(carnivore, na.rm=T)) %>%
  ggplot(aes(x=threatened, y = `Percent Carnivore`)) +
  geom_point(size=2)

#############
cov_data_sub <- cov_data %>%
  select(thr4, Diet_5Cat, mass, gentime, migrates,flying,repro) %>%
  mutate(migrates = as.factor(migrates), flying=as.factor(flying),
         mass = log(mass))

pick_analysis <- function(ind1,ind2,return_stat){
  
  if(ind1==ind2) return(NA)

  dat1 <- cov_data_sub[,ind1][[1]]
  dat2 <- cov_data_sub[,ind2][[1]]
  type1 <- class(dat1)
  type2 <- class(dat2)

  if(type1=='numeric'&type2=='factor'){
    an_type <- 'ANOVA'
    an <- aov(dat1 ~ dat2)
    pval <- summary(an)[[1]][[1,"Pr(>F)"]]
    stat <- summary(an)[[1]][[1,"F value"]]
  }

  if(type1=='factor'&type2=='numeric'){
    an_type <- 'ANOVA'
    an <- aov(dat2 ~ dat1)
    pval <- summary(an)[[1]][[1,"Pr(>F)"]]
    stat <- summary(an)[[1]][[1,"F value"]]
  }

  if(type1=='factor'&type2=='factor'){
    an_type <- 'CHISQ'
    an <- chisq.test(dat1,dat2)
    pval <- an$p.value
    stat <- an$statistic 
  }

  if(type1=='numeric'&type2=='numeric'){
    an_type <- 'CORR'
    an <- cor.test(dat1,dat2)
    pval <- an$p.value
    stat <- an$statistic
  }

  if(return_stat=='p') return(pval)
  if(return_stat=='stat') return(stat)
  if(return_stat=='type') return(an_type)
}


p_mat <- stat_mat <- type_mat <- matrix(NA,ncol(cov_data_sub),ncol(cov_data_sub))
colnames(p_mat) <- colnames(stat_mat) <- colnames(type_mat) <- names(cov_data_sub)
rownames(p_mat) <- rownames(stat_mat) <- rownames(type_mat) <- names(cov_data_sub)
for (i in 1:ncol(cov_data_sub)){
  for (j in 1:ncol(cov_data_sub)){
    if(i<j){ 
      p_mat[i,j] <- stat_mat[i,j] <- type_mat[i,j] <- NA
    } else {
      p_mat[i,j] <- round(pick_analysis(i,j,'p'),3)
      stat_mat[i,j] <- round(pick_analysis(i,j,'stat'),3)
      type_mat[i,j] <- pick_analysis(i,j,'type')
    }
  }
}

pdf('covariate_comparisons.pdf')

#thr4 vs flying
cov_data %>%
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
  theme(legend.text = element_text(size=14),
        legend.position='bottom')

#Diet vs mass
pairwise.t.test(log(cov_data$mass),cov_data$Diet_5Cat)

diffs <- data.frame(diet=factor(levels(cov_data$Diet_5Cat),levels=
                           levels(cov_data$Diet_5Cat)),
                    label=c('ab','ab','a','a','b'))

cov_data %>%
  ggplot(aes(x=Diet_5Cat,y=log(mass),fill=Diet_5Cat)) +
  scale_fill_manual(values=get_palette('npg',5)) +
  geom_boxplot() +
  xlab('Diet type') +
  theme_bw() +
  my_theme +
  theme(legend.position='none') +
  annotate("text", x=diffs$diet, y=12.5, label=diffs$label,size=5)

#Diet vs gentime
pairwise.t.test(cov_data$gentime,cov_data$Diet_5Cat)

diffs <- data.frame(diet=factor(levels(cov_data$Diet_5Cat),levels=
                           levels(cov_data$Diet_5Cat)),
                    label=c('ab','ab','a','a','b'))

cov_data %>%
  ggplot(aes(x=Diet_5Cat,y=gentime,fill=Diet_5Cat)) +
  scale_fill_manual(values=get_palette('npg',5)) +
  geom_boxplot() +
  xlab('Diet type') + ylab('Generation time') +
  theme_bw() +
  my_theme +
  ylim(0,25) +
  theme(legend.position='none') +
  annotate("text", x=diffs$diet, y=25, label=diffs$label,size=5)

#Diet vs migrates
cov_data %>%
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
  theme(legend.text = element_text(size=14),
        legend.position='bottom')

#Gentime vs mass
fit <- lm(gentime ~ log(mass), data=cov_data)
r2 <- summary(fit)$r.squared

cov_data %>%
  ggplot(aes(x=log(mass),y=gentime)) +
  geom_point(size=2,color=get_palette('npg',1)) +
  geom_smooth(method='lm',col='black') +
  ylab('Generation time') +
  theme_bw() +
  my_theme +
  annotate('text',9,1,label='R^{2}==0.45',parse=T,size=5)

#Flying vs mass
cov_data %>%
  filter(!is.na(flying)) %>%
  mutate(flying=ifelse(flying==1,'flying','non-flying')) %>%
  ggplot(aes(x=flying,y=log(mass),fill=flying)) +
  scale_fill_manual(values=get_palette('npg',2)) +
  geom_boxplot() +
  xlab('Flight status') +
  theme_bw() +
  my_theme +
  theme(legend.position='none') +
  annotate("text", x=1.5, y=12.5, label='P < 0.001',size=5)

#Flying vs genetime
cov_data %>%
  filter(!is.na(flying)) %>%
  mutate(flying=ifelse(flying==1,'flying','non-flying')) %>%
  ggplot(aes(x=flying,y=gentime,fill=flying)) +
  scale_fill_manual(values=get_palette('npg',2)) +
  geom_boxplot() +
  xlab('Flight status') + ylab('Generation time') +
  theme_bw() +
  my_theme +
  ylim(0,25) +
  theme(legend.position='none') +
  annotate("text", x=1.5, y=25, label='P < 0.001',size=5)

dev.off()
###############################################################################
test <- lm(log(het)~thr4+Diet_5Cat+migrates+scale(log(mass)),
            data=all_data)

K <- matrix(c(0, 0, 1, 0, 0, -1, 0,0), 1)
t <- glht(test, linfct = K)
summary(t)





pdf('bird_roh_thoughts.pdf')
pr <- predict(test,newdata=data.frame(thr4='LC',
                                      Diet_5Cat=levels(all_data$Diet_5Cat),
                                      migrates=0,mass=mean(all_data$mass,na.rm=T)),
                                      se.fit=T)

fig_data <- data.frame(diet=levels(all_data$Diet_5Cat),fit=pr$fit,
                       lwr=pr$fit-1.96*pr$se.fit,upr=pr$fit+1.96*pr$se.fit)
fig_data %>%
  ggplot(aes(x=diet,y=fit)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=lwr,ymax=upr),width=0.2) +
  ylab('log(H)') + xlab('Diet')
######
pr <- predict(test,newdata=data.frame(thr4=factor(levels(all_data$thr4),
                                                  levels=levels(all_data$thr4)),
                                      Diet_5Cat='PlantSeed',
                                      migrates=0,mass=mean(all_data$mass,na.rm=T)),
                                      se.fit=T)

fig_data <- data.frame(var=factor(levels(all_data$thr4),
                                         levels=levels(all_data$thr4)),
                                  fit=pr$fit,
                       lwr=pr$fit-1.96*pr$se.fit,upr=pr$fit+1.96*pr$se.fit)
fig_data %>%
  ggplot(aes(x=var,y=fit)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=lwr,ymax=upr),width=0.2) +
  ylab('log(H)') + xlab('Redlist Status')

##
cov_data %>% 
  mutate(threatened = ifelse(threatened == 0,'Not_threatened','Threatened')) %>%
  group_by(threatened) %>%
  summarize(mn = mean(repro,na.rm=T),
            lwr = quantile(repro,0.025,na.rm=T),
            upr = quantile(repro,0.975,na.rm=T))%>%
  ggplot(aes(x=threatened,y=mn)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=lwr,ymax=upr),width=0.2) +
  ylab('Repro: clutch size * clutch number') 
dev.off()



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
  theme_bo() +
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
pdf(file='bird_roh_het.pdf')
grid.arrange(redlist,carnivore,mig,mass_fig,ncol=2,nrow=2,
             left=textGrob(expression(log(italic(H))),rot=90,
                           gp=gpar(fontsize=18)))
dev.off()
