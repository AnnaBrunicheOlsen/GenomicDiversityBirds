#Load libraries
library(ggplot2)
suppressMessages(library(ggpubr))
library(gridExtra)
library(grid)

source('clean_data.R')

#Run some tests
test <- lm(log(het)~carnivore+migrates+
           scale(log(repro)), data=all_data)
summary(test)

#Froh
wilcox.test(Froh~carnivore, data=all_data)
wilcox.test(Froh~migrates, data=all_data)
cor.test(all_data$Froh, all_data$repro, method='spearman')
