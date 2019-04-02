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



