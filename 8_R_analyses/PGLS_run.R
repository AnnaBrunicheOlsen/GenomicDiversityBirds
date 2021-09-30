# Run PGLS analyses and generate associated figures 1, 2, 4
library(multcomp)
library(tidyverse)
library(readxl)
library(ggplot2)
library(cowplot)
library(ape)
library(nlme)
library(rr2)

# Custom methods
model.matrix.gls <- function(object, ...) {
    model.matrix(terms(object), data = getData(object), ...)
}
model.frame.gls <- function(object, ...) {
    model.frame(formula(object), data = getData(object), ...)
}
terms.gls <- function(object, ...) {
    terms(model.frame(object), ...)
}

# Read in response and covariate data
dat_all <- read.csv("data/dat_all.csv")
# Phylogenetic data
bird_tree <- read.tree('data/data/species.nwk')

# Adjust species names to match tree
dat_all$species_adjust <- dat_all$Species
dat_all$species_adjust[dat_all$Species=="Dryobates_pubescens"] <-
  "Picoides_pubescens"
dat_all$species_adjust[dat_all$Species=="Antrostomus_carolinensis"] <-
  "Caprimulgus_carolinensis"
dat_all$species_adjust[dat_all$Species=="Cyanoderma_ruficeps"] <-
  "Stachyris_ruficeps"
dat_all$species_adjust[dat_all$Species=="Bubo_blakistoni"] <-
  "Bubo_bubo"
dat_all$species_adjust[dat_all$Species=="Crypturellus_cinnamomeus"] <-
  "Crypturellus_undulatus"
dat_all$species_adjust[dat_all$Species=="Scolopax_mira"] <-
  "Scolopax_minor"
dat_all$species_adjust[dat_all$Species=="Tinamus_guttatus"] <-
  "Eudromia_elegans"

# Adjust covariates
dat_all <- dat_all %>%
  mutate(IUCN = fct_recode(IUCN, ENCR="EN+CR"))

dat_all$diet <- factor(dat_all$diet, levels=c("VertFishScav","FruiNect",
                                              "Invertebrate","Omnivore",
                                              "PlantSeed"))
dat_all$IUCN <- factor(dat_all$IUCN, levels=c("LC","NT","VU","ENCR"))

dat_all$sc_log_mass <- scale(log(dat_all$mass))

ggplot(dat_all, aes(x=diet, y=log(mass))) +
  geom_boxplot()

# Run heterozygosity analysis--------------------------------------------------

dat_het <- dat_all %>%
  rename(area=present)

dat_het$sc_log_area <- scale(log(dat_het$area))

mod <- gls(log(Het) ~ IUCN + sc_log_mass + diet + sc_log_area,
           data=dat_het,
           correlation=corPagel(1,bird_tree, form=~species_adjust))

summary(mod)
R2(mod)

# Run Ne analysis--------------------------------------------------------------

dat_ne <- dat_all %>%
  rename(area=past_area_mean)

dat_ne$sc_log_area <- scale(log(dat_ne$area))

mod_ne <- gls(log(mean_Ne) ~ IUCN + sc_log_mass + diet + sc_log_area,
           data=dat_ne,
           correlation=corPagel(1,bird_tree, form=~species_adjust))

summary(mod_ne)
R2(mod_ne)

# Tables-----------------------------------------------------------------------

sjPlot::tab_model(mod, mod_ne, file='model_results_table.html')

# stats
stat_df <- data.frame(model=c("Het","Ne"),R2=c(R2(mod)[1],R2(mod_ne)[1]),
                      lambda=c(attr(mod$apVar,"Pars")[1],
                      attr(mod_ne$apVar,"Pars")[1]))

write.csv(stat_df, 'model_stats.csv')

# Mass and diet figure (Fig 4)-------------------------------------------------

# Mass

low_mass <- dat_het$Species[1]
low_mass_draw <- paste0("drawings/",tolower(low_mass),".png")

high_mass <- dat_het$Species[which(dat_het$mass==max(dat_het$mass))]
high_mass_draw <- paste0("drawings/",tolower(high_mass),".png")

pelican <- paste0("drawings/","pelecanus_crispus.png")
calypte <- paste0("drawings/","calypte_anna.png")

s <- summary(mod)$tTable
p <- s[,4]

# Heterozygosity
df_temp <- data.frame(sc_log_mass=0,
                      diet=factor('Invertebrate', levels=levels(dat_het$diet)),
                      sc_log_area=0,
                      IUCN=factor('LC',levels=c("LC","NT","VU","ENCR")))

mass_sc <- dat_het$mass#scale(dat_het$mass)
mass_seq <- seq(range(mass_sc)[1],range(mass_sc)[2], length.out=1000)

actual_seq <- mass_seq

mass_seq_sc <- (log(mass_seq) - attr(dat_het$sc_log_mass, "scaled:center"))/
  attr(dat_het$sc_log_mass, "scaled:scale")

nd <- df_temp
nd <- nd[rep(1,1000),]
nd$sc_log_mass <- mass_seq_sc
pr <- AICcmodavg::predictSE.gls(mod, newdata=nd)

mytheme <-   theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
  axis.text=element_text(size=16), axis.title=element_text(size=18))

pval <- p["sc_log_mass"]
if(pval < 0.01){
  pval <- paste0("P < 0.01")
} else {
  pval <- paste0("P = ", sprintf('%.2f', pval))
}
p_df <- data.frame(x=mean(range(log(actual_seq))), y=0.9*max(log(dat_het$Het)),text=pval)

pl_het <- data.frame(pr=pr$fit, lower=pr$fit - 1.96*pr$se.fit,
           upper=pr$fit + 1.96*pr$se.fit, mass=actual_seq) %>%
  ggplot(aes(x=log(mass), y=pr)) +
  geom_point(data=dat_het, aes(x=log(mass), y=log(Het)), col='grey') +
  geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.5, fill="chocolate2") +
  geom_line() +
  geom_text(data=p_df, aes(x=x,y=y,label=text), cex=6) +
  labs(x='log(mass)', y=expression('log('*italic("H")*")")) +
  mytheme +
  theme(axis.title.x=element_blank(),
        plot.margin=margin(0.3,0.3,1,0.3, "cm"))

# Ne
s <- summary(mod_ne)$tTable
p <- s[,4]

df_temp <- data.frame(sc_log_mass=0,
                      diet=factor('Invertebrate', levels=levels(dat_ne$diet)),
                      sc_log_area=0,
                      IUCN=factor('LC',levels=c("LC","NT","VU","ENCR")))

mass_sc <- dat_ne$mass
mass_seq <- seq(range(mass_sc)[1],range(mass_sc)[2], length.out=1000)

actual_seq <- mass_seq

mass_seq_sc <- (log(mass_seq) - attr(dat_ne$sc_log_mass, "scaled:center"))/
  attr(dat_ne$sc_log_mass, "scaled:scale")

nd <- df_temp
nd <- nd[rep(1,1000),]
nd$sc_log_mass <- mass_seq_sc
pr <- AICcmodavg::predictSE.gls(mod_ne, newdata=nd)

pval <- p["sc_log_mass"]
if(pval < 0.01){
  pval <- paste0("P < 0.01")
} else {
  pval <- paste0("P = ", sprintf('%.2f', pval))
}
p_df <- data.frame(x=mean(range(log(actual_seq))), y=1.1*max(log(dat_ne$mean_Ne)),text=pval)

pl_ne <- data.frame(pr=pr$fit, lower=pr$fit - 1.96*pr$se.fit,
           upper=pr$fit + 1.96*pr$se.fit, mass=actual_seq) %>%
  ggplot(aes(x=log(mass), y=pr)) +
  geom_point(data=dat_ne, aes(x=log(mass), y=log(mean_Ne)), col='grey') +
  geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.5, fill="chartreuse3") +
  geom_line() +
  geom_text(data=p_df, aes(x=x,y=y,label=text), cex=6) +
  labs(x='log(mass)', y=expression(log(italic(N[e])))) +
  mytheme +
  theme(axis.title.x=element_blank(),
        plot.margin=margin(0.3,0.3,1,0.3, "cm"))

fig_mass <- plot_grid(pl_het, pl_ne) +
  draw_label("log(mass)", x=0.5, y=  0, vjust=-1, angle= 0, size=18) +
  draw_image(low_mass_draw, 0.2, 0.5, width=0.06, hjust=1.1, vjust=1.06, halign=1,valign=1) +
  draw_image(high_mass_draw, 0.45, 0.92, width=0.07, hjust=1.1, vjust=1.06, halign=1,valign=1) +
  draw_line(x=c(0.15,0.118), y=c(0.45,0.59), arrow=grid::arrow(length=unit(0.25,"cm"))) +
  draw_line(x=c(0.44,0.46), y=c(0.72,0.66), arrow=grid::arrow(length=unit(0.25,"cm"))) +
  draw_image(calypte, 0.72, 0.69, width=0.09, hjust=1.1, vjust=1.06, halign=1,valign=1) +
  draw_line(x=c(0.66,0.6182), y=c(0.56,0.475), arrow=grid::arrow(length=unit(0.25,"cm"))) +
  draw_image(pelican, 0.97, 0.65, width=0.09, hjust=1.1, vjust=1.06, halign=1,valign=1) +
  draw_line(x=c(0.92,0.883), y=c(0.5,0.357), arrow=grid::arrow(length=unit(0.25,"cm")))

# Diet
df_temp <- data.frame(sc_log_mass=0,
                      diet=factor('Invertebrate', levels=levels(dat_ne$diet)),
                      sc_log_area=0,
                      IUCN=factor('LC',levels=c("LC","NT","VU","ENCR")))


# Drawings
drawings <- list.files('drawings')
sp_drawings <- gsub(".png","", drawings)

levs <- levels(dat_het$diet)

carn <- tolower(unique(dat_het$Species[dat_het$diet==levs[1]]))
draw_carn <- which(sp_drawings %in% carn)
carn_draw <- paste0('drawings/',drawings[draw_carn[3]])

fruit <- tolower(unique(dat_het$Species[dat_het$diet==levs[2]]))
draw_fruit <- which(sp_drawings %in% fruit)
fruit_draw <- paste0('drawings/',drawings[draw_fruit[2]])

invert <- tolower(unique(dat_het$Species[dat_het$diet==levs[3]]))
draw_invert <- which(sp_drawings %in% invert)
invert_draw <- paste0('drawings/',drawings[draw_invert[5]])

omni <- tolower(unique(dat_het$Species[dat_het$diet==levs[4]]))
draw_omni <- which(sp_drawings %in% omni)
omni_draw <- paste0('drawings/',drawings[draw_omni[1]])

seed <- tolower(unique(dat_het$Species[dat_het$diet==levs[5]]))
draw_seed <- which(sp_drawings %in% seed)
seed_draw <- paste0('drawings/',drawings[draw_seed[1]])

# Heterozygosity
mc <- glht(mod, linfct=c("dietFruiNect - dietInvertebrate = 0",
                          "dietFruiNect - dietOmnivore = 0",
                          "dietFruiNect - dietPlantSeed = 0",
                          "dietInvertebrate - dietOmnivore = 0",
                          "dietInvertebrate - dietPlantSeed = 0",
                          "dietOmnivore - dietPlantSeed = 0"))
summary(mc)

nd <- df_temp[rep(1,length(levels(dat_het$diet))),]
nd$diet <- factor(levels(dat_het$diet),levels=levels(dat_het$diet))
pr <- AICcmodavg::predictSE.gls(mod, newdata=nd)

dat_raw <- dat_het %>%
  mutate(diet=fct_recode(diet, `Plants/Seeds`='PlantSeed',
          `Vertebrates/\nFish/Scavenger`='VertFishScav',
          `Invertebrates`='Invertebrate', `Fruit/Nectar`='FruiNect'))

p_df <- data.frame(x=levels(dat_raw$diet), y=0.9*max(log(dat_raw$Het)),
                   text=c("B","AB","AB","A","A"))

pl_het <- data.frame(pr=pr$fit, lower=pr$fit - 1.96*pr$se.fit,
           upper=pr$fit + 1.96*pr$se.fit,
           diet=nd$diet) %>%
  mutate(diet=fct_recode(diet, `Plants/Seeds`='PlantSeed',
          `Vertebrates/\nFish/Scavenger`='VertFishScav',
          `Invertebrates`='Invertebrate', `Fruit/Nectar`='FruiNect')) %>%
  ggplot(aes(x=pr, y=diet)) +
  geom_jitter(data=dat_raw, aes(x=log(Het), y=diet), col='grey',
              height=0.1) +
  geom_errorbarh(aes(xmin=lower,xmax=upper), height=0.4, col='chocolate2',size=0.8) +
  geom_point(col='chocolate2',size=3) +
  geom_text(data=p_df, aes(x=y,y=x,label=text), cex=5) +
  labs(x=expression('log('*italic("H")*")"), y='Diet') +
  xlim((min(log(dat_raw$Het))-0.01), -4) +
  mytheme +
  theme(axis.text.y=element_text(margin=margin(r=55)),axis.ticks.y=element_blank())

# Ne

df_temp <- data.frame(sc_log_mass=0,
                      diet=factor('Invertebrate', levels=levels(dat_ne$diet)),
                      sc_log_area=0,
                      IUCN=factor('LC',levels=c("LC","NT","VU","ENCR")))

mc <- glht(mod_ne, linfct=c("dietFruiNect - dietInvertebrate = 0",
                          "dietFruiNect - dietOmnivore = 0",
                          "dietFruiNect - dietPlantSeed = 0",
                          "dietInvertebrate - dietOmnivore = 0",
                          "dietInvertebrate - dietPlantSeed = 0",
                          "dietOmnivore - dietPlantSeed = 0"))
summary(mc)


nd <- df_temp[rep(1,length(levels(dat_ne$diet))),]
nd$diet <- factor(levels(dat_ne$diet),levels=levels(dat_ne$diet))
pr <- AICcmodavg::predictSE.gls(mod_ne, newdata=nd)

p_df <- data.frame(y=levels(dat_ne$diet), x=0.97*max(log(dat_ne$mean_Ne)),
                   text=c("B","AB","AB","AB","A"))

pl_ne <- data.frame(pr=pr$fit, lower=pr$fit - 1.96*pr$se.fit,
           upper=pr$fit + 1.96*pr$se.fit,
           diet=nd$diet) %>%
  ggplot(aes(x=pr, y=diet)) +
  geom_jitter(data=dat_ne, aes(x=log(mean_Ne), y=diet), col='grey',
              height=0.1) +
  geom_errorbarh(aes(xmin=lower,xmax=upper), height=0.4, col='chartreuse3',size=0.8) +
  geom_point(col='chartreuse3',size=3) +
  geom_text(data=p_df, aes(x=x,y=y,label=text), cex=5) +
  labs(x=expression(log(italic(N[e]))), y='Diet') +
  mytheme +
  theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(),
        axis.text.y=element_blank())

fig_diet <- plot_grid(pl_het, pl_ne, rel_widths=c(1.8,1), align='h') +
  draw_image(carn_draw, 0.305, 0.41, width=0.08, hjust=1.1, vjust=1.06, halign=1,valign=1) +
  draw_image(fruit_draw, 0.30, 0.53, width=0.08, hjust=1.1, vjust=1.06, halign=1,valign=1) +
  draw_image(invert_draw, 0.30, 0.70, width=0.08, hjust=1.1, vjust=1.06, halign=1,valign=1) +
  draw_image(omni_draw, 0.30, 0.85, width=0.08, hjust=1.1, vjust=1.06, halign=1,valign=1) +
  draw_image(seed_draw, 0.30, 1.01, width=0.08, hjust=1.1, vjust=1.06, halign=1,valign=1)

plot_grid(fig_mass, fig_diet, nrow=2)

ggsave("figures/fig4_traits.tiff", compression='lzw', dpi=300, height=8, width=9)


# IUCN figure (Fig 1)----------------------------------------------------------

df_temp <- data.frame(sc_log_mass=0,
                      diet=factor('Invertebrate', levels=levels(dat_ne$diet)),
                      sc_log_area=0,
                      IUCN=factor('LC',levels=c("LC","NT","VU","ENCR")))


levs <- levels(dat_het$IUCN)

EN <- tolower(unique(dat_het$Species[dat_het$IUCN==levs[4]]))
draw_EN <- which(sp_drawings %in% EN)
EN_draw <- paste0('drawings/',drawings[draw_EN[2]])

VU <- tolower(unique(dat_het$Species[dat_het$IUCN==levs[3]]))
draw_VU <- which(sp_drawings %in% VU)
VU_draw <- paste0('drawings/',drawings[draw_VU[1]])

NT <- tolower(unique(dat_het$Species[dat_het$IUCN==levs[2]]))
draw_NT <- which(sp_drawings %in% NT)
NT_draw <- paste0('drawings/',drawings[draw_NT[2]])

LC <- tolower(unique(dat_het$Species[dat_het$IUCN==levs[1]]))
draw_LC <- which(sp_drawings %in% LC)
LC_draw <- paste0('drawings/','picoides_pubescens.png')

# Heterozygosity
mc <- glht(mod, linfct=c("IUCNNT - IUCNVU = 0","IUCNNT - IUCNENCR = 0",
                          "IUCNVU - IUCNENCR = 0"))
summary(mc)


nd <- df_temp[rep(1,length(levels(dat_het$IUCN))),]
nd$IUCN <- factor(levels(dat_het$IUCN),levels=levels(dat_het$IUCN))

pr <- AICcmodavg::predictSE.gls(mod, newdata=nd, se.fit=T)

p_df <- data.frame(y=levels(dat_het$IUCN), x=0.9*max(log(dat_het$Het)),
                   text=c("A","A","B","B")) %>%
        mutate(y=fct_recode(y, `Endangered+\nCritically\nEndangered`='ENCR',
                               Vulnerable="VU", `Near\nThreatened`='NT',
                               `Least\nConcern`='LC'))

dat_raw <- dat_het %>%
        mutate(IUCN=fct_recode(IUCN, `Endangered+\nCritically\nEndangered`='ENCR',
                               Vulnerable="VU", `Near\nThreatened`='NT',
                               `Least\nConcern`='LC'))

pl_het <- data.frame(pr=pr$fit, lower=pr$fit - 1.96*pr$se.fit,
           upper=pr$fit + 1.96*pr$se.fit,
           IUCN=nd$IUCN) %>%
          mutate(IUCN=fct_recode(IUCN, `Endangered+\nCritically\nEndangered`='ENCR',
                               Vulnerable="VU", `Near\nThreatened`='NT',
                               `Least\nConcern`='LC'))  %>%
  ggplot(aes(y=IUCN, x=pr)) +
  geom_jitter(data=dat_raw,
              aes(y=IUCN, x=log(Het)), col='grey',
              height=0.1) +
  geom_errorbarh(aes(xmin=lower,xmax=upper), height=0.4,col='chocolate2',size=0.8) +
  geom_point(size=3,col='chocolate2') +
  geom_text(data=p_df, aes(x=x,y=y,label=text), cex=5) +
  labs(x=expression('log('*italic("H")*")"), y="IUCN threat category") +
  mytheme +
  theme(axis.text=element_text(size=14),
        axis.text.y=element_text(margin=margin(r=38)),axis.ticks.y=element_blank())

# Ne

df_temp <- data.frame(sc_log_mass=0,
                      diet=factor('Invertebrate', levels=levels(dat_ne$diet)),
                      sc_log_area=0,
                      IUCN=factor('LC',levels=c("LC","NT","VU","ENCR")))

mc <- glht(mod_ne, linfct=c("IUCNNT - IUCNVU = 0","IUCNNT - IUCNENCR = 0",
                          "IUCNVU - IUCNENCR = 0"))
summary(mc)


nd <- df_temp[rep(1,length(levels(dat_ne$IUCN))),]
nd$IUCN <- factor(levels(dat_ne$IUCN),levels=levels(dat_ne$IUCN))

pr <- AICcmodavg::predictSE.gls(mod_ne, newdata=nd, se.fit=T)

p_df <- data.frame(y=levels(dat_het$IUCN), x=1.08*max(log(dat_ne$mean_Ne)),
                   text=c("A","B","AB","AB"))

pl_ne <- data.frame(pr=pr$fit, lower=pr$fit - 1.96*pr$se.fit,
           upper=pr$fit + 1.96*pr$se.fit,
           IUCN=nd$IUCN) %>%
  ggplot(aes(y=IUCN, x=pr)) +
  geom_jitter(data=dat_ne,
              aes(y=IUCN, x=log(mean_Ne)), col='grey',
              height=0.1) +
  geom_errorbarh(aes(xmin=lower,xmax=upper), height=0.4,col='chartreuse3',size=0.8) +
  geom_point(size=3,col='chartreuse3') +
  geom_text(data=p_df, aes(x=x,y=y,label=text), cex=5) +
  labs(x=expression(log(italic(N[e]))), y="IUCN status") +
  mytheme +
  xlim(min(log(dat_ne$mean_Ne))-0.01, 12.5) +
  theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(),
        #axis.text.y=element_text(size=14),
        axis.text.y=element_blank(), axis.text.x=element_text(size=14))

plot_grid(pl_het, pl_ne, rel_widths=c(0.7,0.4), align='h') +

  draw_image(EN_draw, 0.29, 0.95, width=0.06, hjust=1.1, vjust=1.06, halign=1,valign=1) +
  draw_image(VU_draw, 0.30, 0.76, width=0.07, hjust=1.1, vjust=1.06, halign=1,valign=1) +
  draw_image(NT_draw, 0.30, 0.58, width=0.07, hjust=1.1, vjust=1.06, halign=1,valign=1) +
  draw_image(LC_draw, 0.28, 0.41, width=0.04, hjust=1.1, vjust=1.06, halign=1,valign=1)


ggsave("figures/fig1_IUCN.tiff", compression='lzw', dpi=300, height=4,width=7)


# Habitat area figure (Fig 2)--------------------------------------------------

draw_min <- "drawings/geospiza_fortis.png"
draw_max <- "drawings/tyto_alba.png"

df_temp <- data.frame(sc_log_mass=0,
                      diet=factor('Invertebrate', levels=levels(dat_ne$diet)),
                      sc_log_area=0,
                      IUCN=factor('LC',levels=c("LC","NT","VU","ENCR")))


# Heterozygosity
s <- summary(mod)$tTable
p <- s[,4]

area_seq <- seq(range(dat_het$area,na.rm=T)[1],
                range(dat_het$area,na.rm=T)[2], length.out=1000)

area_seq_sc <- (log(area_seq) - attr(dat_het$sc_log_area, "scaled:center"))/
  attr(dat_het$sc_log_area, "scaled:scale")

nd <- df_temp
nd <- nd[rep(1,1000),]
nd$sc_log_area <- area_seq_sc
pr <- AICcmodavg::predictSE.gls(mod, newdata=nd, se.fit=T)

pval <- p["sc_log_area"]
if(pval < 0.01){
  pval <- paste0("P < 0.01")
} else {
  pval <- paste0("P = ", sprintf('%.2f', pval))
}
p_df <- data.frame(x=mean(range(log(area_seq))), y=0.9*max(log(dat_het$Het)),text=pval)


pl_het <- data.frame(pr=pr$fit, lower=pr$fit - 1.96*pr$se.fit,
           upper=pr$fit + 1.96*pr$se.fit) %>%
  ggplot(aes(x=log(area_seq), y=pr)) +
  geom_point(data=dat_het, aes(x=log(area), y=log(Het)), col='grey') +
  geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.5, fill='chocolate2') +
  geom_line() +
  geom_text(data=p_df, aes(x=x,y=y,label=text), cex=6) +
  labs(x='log(suitable habitat area)', y=expression('log('*italic("H")*")")) +
  mytheme +
  theme(axis.title.x=element_blank(),
        plot.margin=margin(0.3,0.3,1,0.3, "cm"))

# Ne

draw_min_ne <- "drawings/geospiza_fortis.png"
draw_max_ne <- "drawings/egretta_garzetta.png"

df_temp <- data.frame(sc_log_mass=0,
                      diet=factor('Invertebrate', levels=levels(dat_ne$diet)),
                      sc_log_area=0,
                      IUCN=factor('LC',levels=c("LC","NT","VU","ENCR")))


s <- summary(mod_ne)$tTable
p <- s[,4]

area_seq2 <- seq(range(dat_ne$area,na.rm=T)[1],range(dat_ne$area,na.rm=T)[2], length.out=1000)

area_seq_sc <- (log(area_seq2) - attr(dat_ne$sc_log_area, "scaled:center"))/
  attr(dat_ne$sc_log_area, "scaled:scale")

nd <- df_temp
nd <- nd[rep(1,1000),]
nd$sc_log_area <- area_seq_sc
pr <- AICcmodavg::predictSE.gls(mod_ne, newdata=nd, se.fit=T)

pval <- p["sc_log_area"]
if(pval < 0.01){
  pval <- paste0("P < 0.01")
} else {
  pval <- paste0("P = ", sprintf('%.2f', pval))
}
p_df <- data.frame(x=mean(range(log(area_seq2))), y=1.1*max(log(dat_ne$mean_Ne)),text=pval)


pl_ne <- data.frame(pr=pr$fit, lower=pr$fit - 1.96*pr$se.fit,
           upper=pr$fit + 1.96*pr$se.fit) %>%
  ggplot(aes(x=log(area_seq2), y=pr)) +
  geom_point(data=dat_ne, aes(x=log(area), y=log(mean_Ne)), col='grey') +
  geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.5, fill='chartreuse3') +
  geom_line() +
  geom_text(data=p_df, aes(x=x,y=y,label=text), cex=6) +
  labs(x='log(suitable habitat area)', y=expression(log(italic(N[e])))) +
  mytheme +
  theme(axis.title.x=element_blank(),
        plot.margin=margin(0.3,0.5,1,0.3, "cm"))

plot_grid(pl_het, pl_ne, rel_widths=c(1,1.08)) +
  draw_label("log(suitable habitat area)", x=0.5, y=  0, vjust=-1, angle= 0, size=18) +
  draw_image(draw_min, 0.21, 0.94, width=0.08, hjust=1.1, vjust=1.06, halign=1,valign=1) +
  draw_image(draw_max, 0.47, 0.49, width=0.12, hjust=1.1, vjust=1.06, halign=1,valign=1) +
  draw_line(x=c(0.167,0.163), y=c(0.765,0.72), arrow=grid::arrow(length=unit(0.25,"cm"))) +
  draw_line(x=c(0.43,0.445), y=c(0.38,0.56), arrow=grid::arrow(length=unit(0.25,"cm"))) +
  draw_image(draw_min_ne, 0.72, 0.64, width=0.08, hjust=1.1, vjust=1.06, halign=1,valign=1) +
  draw_image(draw_max_ne, 0.97, 0.72, width=0.12, hjust=1.1, vjust=1.06, halign=1,valign=1) +
  draw_line(x=c(0.65,0.63), y=c(0.49,0.42), arrow=grid::arrow(length=unit(0.25,"cm"))) +
  draw_line(x=c(0.92,0.942), y=c(0.58,0.48), arrow=grid::arrow(length=unit(0.25,"cm")))

ggsave("figures/fig2_area.tiff", compression='lzw', dpi=300, height=4,width=7)
