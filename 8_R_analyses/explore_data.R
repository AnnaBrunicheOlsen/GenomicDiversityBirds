#------------------------------------------------------------------------------
#Load clean dataset

source('clean_data.R')

#------------------------------------------------------------------------------

#Look at distributions of response and diagnostic variables
genetic_data %>%
  gather(key='var',value='value',het:S20) %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~var,scales='free')

#Log-transform
genetic_data %>%
  gather(key='var',value='value',het:S20) %>%
  ggplot(aes(x = log(value+0.005))) +
  geom_histogram() +
  facet_wrap(~var,scales='free')

#Correlations
genetic_data %>% select(-species) %>%
  cor() %>%
  round(.,3)

genetic_data %>% select(-species) %>%
  cor(method='spearman') %>%
  round(.,3)

cov_data %>% select(-species) %>%
  cor(use='complete.obs') %>%
  round(.,3)

# Compare roh stats between birds and mammals

roh_mam <- read_excel('~/research/archive/runs_of_homozygosity/data/Cons_Gen_edits.xlsx')


roh_mam2 <- read_csv('~/research/archive/runs_of_homozygosity/data/roh20X_full_02272018.csv')
roh_mam2 <- rbind(roh_mam2,NA)

roh_mam <- roh_mam[,c(1,5,6,10)]
roh_mam <- cbind(roh_mam, roh_mam2$ROHnumber)
names(roh_mam) <- c('species',
                    'total_SNV','SNVS_in_scaffs_length_20snv',
                    'scaffolds_min_20snv','rohNumber')


roh_mam <- roh_mam %>%
  mutate(taxa='mammal')

roh_bird <- raw_genetics[,c(1,2,3,5,11)]
names(roh_bird) <- c('species',
                    'total_SNV','SNVS_in_scaffs_length_20snv',
                    'scaffolds_min_20snv','rohNumber')
roh_bird <- roh_bird %>%
  mutate(taxa='bird')

full <- rbind(roh_mam,roh_bird)

total_SNV <- full %>% 
  ggplot(aes(x=taxa, y=log(total_SNV), fill=taxa)) +
  geom_violin(draw_quantiles=0.5) +
  theme(legend.position='none')

SNV_scaf_20 <- full %>%
  ggplot(aes(x=taxa, y=log(SNVS_in_scaffs_length_20snv), fill=taxa)) +
  geom_violin(draw_quantiles=0.5) +
  theme(legend.position='none')

scaf_min_20 <- full %>%
  ggplot(aes(x=taxa, y=log(scaffolds_min_20snv), fill=taxa)) +
  geom_violin(draw_quantiles=0.5) +
  theme(legend.position='none')

rohNum <- full %>%
  ggplot(aes(x=taxa, y=log(rohNumber), fill=taxa)) +
  geom_violin(draw_quantiles=0.5) +
  theme(legend.position='none')

library(gridExtra)

pdf('bird_v_mam.pdf')

grid.arrange(total_SNV,SNV_scaf_20, scaf_min_20, rohNum, ncol=2)

dev.off()
