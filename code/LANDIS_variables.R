# Preparing the dataset for Wisconsin ----
###################################################################################
#' ### Installing and loading the libraries needed
#'
#Install.packages("tidyverse")
#Install.packages("knitr")
#Install.packages("ezknit")
library(tidyverse)
library(knitr)
library(ezknitr)

###################################################################################

# 1. Read in the FIA data for the state of Wisconsin ----
#' 
#' Read in the data for Wisconsin
#' 
WI_COND<-read.csv("data/main_WI_2020/WI_COND.csv", na.strings = "NA")#read the condition table
WI_PLOT<-read.csv("data/main_WI_2020/WI_PLOT.csv", na.strings = "NA")#read the plot table
WI_TREE<-read.csv("data/main_WI_2020/WI_TREE.csv", na.strings = "NA")#read the tree table
#'
#'
#' Subset for ONE county (COUNTYCD =1) and just keep records from 2000 on
#'
WI_COND<-subset(WI_COND, INVYR >= 2000)
WI_PLOT<-subset(WI_PLOT, INVYR >= 2000)
WI_TREE<-subset(WI_TREE, INVYR >= 2000)
#'
#' Keep each identifier record different before combining tables
#'
colnames(WI_PLOT)[1]<-"PLT_CN"
colnames(WI_COND)[1]<-"COND_CN"
colnames(WI_TREE)[1]<-"TREE_CN"
#'
#' Select variables of interest for each table
#' 
WI_COND1 <- select(WI_COND1, PLT_CN, INVYR, STATECD, COUNTYCD, PLOT,COND_STATUS_CD, CONDID, DSTRBCD1, DSTRBCD2, DSTRBCD3)
WI_PLOT1 <- select(WI_PLOT1, PLT_CN, INVYR, STATECD, COUNTYCD, PLOT, ELEV, ECOSUBCD, CYCLE)
WI_TREE1 <- select(WI_TREE1, TREE_CN,PLT_CN, INVYR, STATECD, COUNTYCD, PLOT, SUBP,CONDID, TREE, STATUSCD, SPCD, SPGRPCD, DIA, DIAHTCD, HT, ACTUALHT, AGENTCD, DAMAGE_AGENT_CD1, DAMAGE_AGENT_CD2, DAMAGE_AGENT_CD3, MORTYR, STANDING_DEAD_CD, TPA_UNADJ, DRYBIO_BOLE, DRYBIO_TOP, DRYBIO_STUMP, DRYBIO_SAPLING, DRYBIO_WDLD_SPP, DRYBIO_BG, DRYBIO_AG, CARBON_AG, CARBON_BG)
#'
#' Merge the tables
#'
WI_CP<-merge(WI_PLOT1, WI_COND1, by=c("STATECD","COUNTYCD","PLOT","INVYR", "PLT_CN")) #merge the plot and condition tables
#'
WI_TP<- merge(WI_TREE1, WI_PLOT1, by=c("STATECD","COUNTYCD","PLOT","INVYR", "PLT_CN")) #merge tree and plot tables
#'
WI_TC<- merge(WI_TREE1, WI_COND1, by=c("STATECD","COUNTYCD","PLOT","INVYR", "CONDID" ,"PLT_CN")) #merge the Tree and condition tables
#'
WI_TCP<- merge(WI_TC, WI_PLOT1, by=c("STATECD","COUNTYCD","PLOT","INVYR", "PLT_CN")) #merge the three tables
#'
###################################################################################
#'
# Landis disturbance variable ----
#'
#' ## 1. Create a disturbance variable per subplot
#' 
#' ### 1.1 Prepare the table
#' 
#' #### Create a unique identifier ID with invyr_plot_subplot_tree
#' 
WI_TP$ID<- paste(WI_TP$INVYR, WI_TP$PLOT, WI_TP$SUBP, WI_TP$TREE, sep="_")
#'
#' #### 1.2 Classify tree diameters into 5" classes (for pilot, just 1 county, the max DIA is 29.4"). Update values when using other counties
#' 
#' First filter out diameters less than 5" (those correspond to the microplot)
#'
WI_TP <- WI_TP %>% filter( DIA >= 5) #filter out trees that correspond to microplot
#'
#'
#' Create a for loop that will classify diameters into diameter classes every 5". Range of diameters in Wisconsin is 5-90"
#' 
sequenceDIA<- seq(from=5, to=90, by=5) #create a sequence from 5 to 90 that will be used in the loop
#'
WI_TP$DIA_CLASS<- 0 #create an empty column for diameter class
#'
WI_TP$MIN_DIA<- 0 #create an empty column for the lower end of the diameter class
WI_TP$MAX_DIA<- 0 #create an empty column for the upper end of the diameter class
#'
#'
for(i in 1:length(sequenceDIA)){
  temp=sequenceDIA[i]
  for(j in 1:nrow(WI_TP)){
    if(WI_TP$DIA[j] >= temp & WI_TP$DIA[j]<(temp+5)
    ){WI_TP$MIN_DIA[j]=temp
    WI_TP$MAX_DIA[j]=temp+5
    WI_TP$DIA_CLASS[j] = paste("[",temp,"-",(temp+5),">")
    }}}
#'
#' Alternative code to do it with an ifelse statement instead of a for loop
#' 
#WI_TP$DIA_CLASS<- ifelse(ifelse(WI_TP$DIA>=5 & WI_TP$DIA <10, "[5-10>",
#                                ifelse(WI_TP$DIA>=10 & WI_TP$DIA <15, "[10-15>",
#                                       ifelse(WI_TP$DIA>=15 & WI_TP$DIA <20, "[15-20>",
#                                              ifelse(WI_TP$DIA>=20 & WI_TP$DIA <25, "[20-25>",
#                                       "[25-30>")))))
#'
#' #### 1.3 Use the group_by and summarise functions to create a table containing disturbances per diameter class in each subplot
#' 
DIST_SUBP<- WI_TP %>% group_by(INVYR, PLOT, SUBP, DIA_CLASS, AGENTCD)%>% #Agent of mortality will be our disturbance variable
  summarise( N_TREES_DIA= n())
#'
#' Add a new column indicating just the number of trees disturbed in each diameter class
#' 
DIST_SUBP$N_DIST_TREES <- ifelse (DIST_SUBP$AGENTCD != "NA", DIST_SUBP$N_TREES_DIA, "NA")
#'
#' Rename AGENTCD variable to DIST_TYPE
#' 
colnames(DIST_SUBP)[5] <- "DIST_TYPE"
#'
#' Fill the NAs for disturbance types and number of disturbed trees with zeros
#'
DIST_SUBP$N_DIST_TREES[is.na(DIST_SUBP$N_DIST_TREES)] <- 0 # Filling the missing values with zeros
DIST_SUBP$DIST_TYPE[is.na(DIST_SUBP$DIST_TYPE)] <- 0 # Filling the missing values with zeros
#'
#' Look at our output
#' 
head(DIST_SUBP, 10)
#'
#' This table is the one to be used for LANDIS purposes