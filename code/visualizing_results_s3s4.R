# Visualizing results ----
###################################################################################
#' ### Installing and loading the libraries needed
#'
#install.packages("tidyverse")
library(tidyverse)
#'
#' Load the data
#' 
equivalence_test<-read.csv("simulations/s3_s4/results/tables_SPECIES_s3_s4_validation_long.CSV")
#'
#' Filter table for 15 most common species
#' 
codes<-c(316, 318, 12, 746, 701, 543, 951, 972, 762, 833)#, 129, 375, 125, 541, 802)
#'
equivalence_common<-equivalence_test%>% filter(equivalence_test$SPCD %in% codes)
#'
#' Plot results
#install.packages("scales")#to wrap text
library(scales)
#' 
plot_species<-ggplot(equivalence_common, aes(x = Scientific_name, y = Mean_bias, 
              ymin = Mean_bias - SD_bias, ymax = Mean_bias+SD_bias, fill = Source)) + 
  geom_bar(position = position_dodge(), stat="identity", color="black")  +
  geom_errorbar(position=position_dodge(.9), width = .2, color="black") +
  scale_fill_manual(values=c("grey20","grey80"))+scale_x_discrete(labels = wrap_format(10))+
  labs( x = "Species", y ="Mean bias (basal area)")+
  theme(panel.background = element_rect(fill="white"),axis.text=element_text(size=28, color="black"),axis.title=element_text(size=32,face="bold"), legend.text=element_text(size=32), legend.title=element_text(size=32))
#'
#ggsave("simulations/s3_s4/results/plot_species.tiff", plot = plot_species, width=600, height=450, units="mm", dpi=300, compression = "lzw")

