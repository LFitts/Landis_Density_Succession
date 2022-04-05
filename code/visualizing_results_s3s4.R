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
calibrated<-c(12,701,95,105,746, 762, 802,241, 261)
#'
not_calibrated<-c(316, 318, 125, 375, 543, 833, 951, 129, 743)
#'
equivalence_calibrated<-equivalence_test%>% filter(equivalence_test$SPCD %in% calibrated)
#'
equivalence_uncalibrated<-equivalence_test%>% filter(equivalence_test$SPCD %in% not_calibrated)
#'
#' Plot results
#install.packages("scales")#to wrap text
library(scales)
#' 
plot_calibrated<-ggplot(equivalence_calibrated, aes(x = Scientific_name, y = Mean_bias, 
              ymin = Mean_bias - SD_bias, ymax = Mean_bias+SD_bias, fill = Source)) + 
  geom_bar(position = position_dodge(), stat="identity", color="black")  +
  geom_errorbar(position=position_dodge(.9), width = .2, color="black") +
  scale_fill_manual(values=c("grey20","grey80"))+scale_x_discrete(labels = wrap_format(10))+
  labs( x = " ", y ="Mean bias (basal area)")+
  theme(panel.background = element_rect(fill="white"),axis.text=element_text(size=28, color="black"),axis.title=element_text(size=32,face="bold"), legend.text=element_text(size=32), legend.title=element_text(size=32))
#'
plot_uncalibrated<-ggplot(equivalence_uncalibrated, aes(x = Scientific_name, y = Mean_bias, 
                                                    ymin = Mean_bias - SD_bias, ymax = Mean_bias+SD_bias, fill = Source)) + 
  geom_bar(position = position_dodge(), stat="identity", color="black")  +
  geom_errorbar(position=position_dodge(.9), width = .2, color="black") +
  scale_fill_manual(values=c("grey20","grey80"))+scale_x_discrete(labels = wrap_format(10))+
  labs( x = "Species", y ="Mean bias (basal area)")+
  theme(panel.background = element_rect(fill="white"),axis.text=element_text(size=28, color="black"),axis.title=element_text(size=32,face="bold"), legend.text=element_text(size=32), legend.title=element_text(size=32), legend.position = "none")
#'
#'
#install.packages("gridExtra") #similar to facet grid, to put two graphs next to each other
library(gridExtra)
s3s4<-grid.arrange(plot_calibrated, plot_uncalibrated, nrow=2)
#'
#ggsave("simulations/s3_s4/results/plot_species.tiff", plot = s3s4, width=600, height=450, units="mm", dpi=300, compression = "lzw")

