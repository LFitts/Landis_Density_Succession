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

#' # 1. Read in the FIA data for the state of Wisconsin
#' 
#' Read in the data for Wisconsin
#' 
WI_COND<-read.csv("data/main_WI_2020/WI_COND.csv")#read the condition table
WI_PLOT<-read.csv("data/main_WI_2020/WI_PLOT.csv")#read the plot table
WI_TREE<-read.csv("data/main_WI_2020/WI_TREE.csv")#read the tree table
#'
#'
#' Subset for ONE county (COUNTYCD =1) and just keep records from 2000 on
#'
WI_COND1<-subset(WI_COND, COUNTYCD==1) %>% subset(INVYR >= 2000)
WI_PLOT1<-subset(WI_PLOT, COUNTYCD==1) %>% subset(INVYR >= 2000)
WI_TREE1<-subset(WI_TREE, COUNTYCD==1) %>% subset(INVYR >= 2000)
#'
#' Keep each identifier record different before combining tables
#'
colnames(WI_PLOT1)[1]<-"PLT_CN"
colnames(WI_COND1)[1]<-"COND_CN"
colnames(WI_TREE1)[1]<-"TREE_CN"
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
#'
#' ## Create a disturbance variable per subplot
#' 
#' ### 1. Prepare the table
#' 
#' #### 1.1 Create a unique identifier ID with invyr_plot_subplot_tree
#' 
WI_TP$ID<- paste(WI_TP$INVYR, WI_TP$PLOT, WI_TP$SUBP, WI_TP$TREE, sep="_")
#'
#' #' #### 1.2 Classify tree diameters into 5" classes (for pilot, just 1 county, the max DIA is 29.4"). Update values when using other counties
#' 
WI_TP$DIA_CLASS<- ifelse(WI_TP$DIA < 5, "[0-5>",
                         ifelse(WI_TP$DIA>=5 & WI_TP$DIA <10, "[5-10>",
                                ifelse(WI_TP$DIA>=10 & WI_TP$DIA <15, "[10-15>",
                                       ifelse(WI_TP$DIA>=15 & WI_TP$DIA <20, "[15-20>",
                                              ifelse(WI_TP$DIA>=20 & WI_TP$DIA <25, "[20-25>",
                                       "[25-30>")))))
#'
#' #' #### 1.3 Use the group_by and summarise functions to create a table containing disturbances per diameter class in each subplot
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
#' Look at our output
#' 
head(DIST_SUBP, 10)
#'
###################################################################################
#' # DISTURBANCE VARIABLE COMPARISON
#'
#' ## Create a disturbance variable per subplot per condid
#' 
#' ### 1. Prepare the table
#' 
#' #### 1.1 Create a unique identifier ID with invyr_plot_subplot_tree
#' 
WI_TCP$ID<- paste(WI_TCP$INVYR, WI_TCP$PLOT, WI_TCP$SUBP, WI_TCP$CONDID,WI_TCP$TREE, sep="_")
#'
#' #' #### 1.2 Classify tree diameters into 5" classes (for pilot, just 1 county, the max DIA is 29.4"). Update values when using other counties
#' 
WI_TCP$DIA_CLASS<- ifelse(WI_TCP$DIA < 5, "[0-5>",
                         ifelse(WI_TCP$DIA>=5 & WI_TCP$DIA <10, "[5-10>",
                                ifelse(WI_TCP$DIA>=10 & WI_TCP$DIA <15, "[10-15>",
                                       ifelse(WI_TCP$DIA>=15 & WI_TCP$DIA <20, "[15-20>",
                                              ifelse(WI_TCP$DIA>=20 & WI_TCP$DIA <25, "[20-25>",
                                                     "[25-30>")))))
#'
#' #' #### 1.3 Use the group_by and summarise functions to create a table containing disturbances per diameter class in each subplot
#' 
DIST_SUBP_CONDID<- WI_TCP %>% group_by(INVYR, PLOT, SUBP, CONDID,DIA_CLASS, AGENTCD)%>% #Agent of mortality will be our disturbance variable
  summarise( N_TREES_DIA= n())
#'
#' Add a new column indicating just the number of trees disturbed in each diameter class
#' 
DIST_SUBP_CONDID$N_DIST_TREES <- ifelse (DIST_SUBP_CONDID$AGENTCD != "NA", DIST_SUBP_CONDID$N_TREES_DIA, "NA")
#'
#' Rename AGENTCD variable to DIST_TYPE
#' 
colnames(DIST_SUBP_CONDID)[6] <- "DIST_TYPE"
#'
#' Look at our output
#' 
head(DIST_SUBP_CONDID, 10)
#'
#' Now create a column that calculates the proportion of trees that were afected by a disturbance per condition in each plot
#' 
#' For the purposes of counting the disturbances and trees affected by it, replace NA's with CEROS
#' 
DIST_SUBP_CONDID$DIST_TYPE[is.na(DIST_SUBP_CONDID$DIST_TYPE)] <- 0 #fill NA's with ceros
#'
DIST_SUBP_CONDID$N_DIST_TREES[is.na(DIST_SUBP_CONDID$N_DIST_TREES)] <- 0 #fill NA's with ceros
#'
#' Group by inventory year, plot, condition id and disturbance type to calculate the number of disturbed trees   
#' 
dist_condid<-DIST_SUBP_CONDID %>% group_by(INVYR,PLOT,CONDID, DIST_TYPE)%>%
  summarise(trees_dist= sum(N_DIST_TREES))
#'
#' Group by inventory year, plot, and condition id to calculate the number of trees
#'  
condid<-DIST_SUBP_CONDID %>% group_by(INVYR,PLOT,CONDID)%>%
  summarise(trees_tot= sum(N_TREES_DIA))
#' 
#' Now merge back the two tables and calculate the proportion of disturbed trees per condition id
#' 
disturbances_tree<- merge(dist_condid,condid, by=c("INVYR", "PLOT", "CONDID")) %>%
  mutate(proportion=(trees_dist/trees_tot)*100)
#'
head(disturbances_tree,15)
#'
#'
###################################################################################
#'
#'
#' ## Now, let's compare is this new variable works better at capturing disturbances 
#' For doing this, we will compare the original 'condition-level disturbance variable' (COND_DIST) with the 'tree-level disturbance variable' (TREE_DIST) through a X^2 test.
#' There will be three categories in each variable: not disturbed (ND), simple disturbance (SD), and compound disturbance (CD)
#' 
#' ### 1. Prepare the condition table
#' 
#' If it is a single disturbance = SD, compound disturbances = CD, no disturbance = ND
#'
WI_CP$DIST <- ifelse(WI_CP$DSTRBCD1 != 0 & WI_CP$DSTRBCD2 == 0, "SD",
                               ifelse(WI_CP$DSTRBCD1 != 0 & WI_CP$DSTRBCD2 != 0 ,"CD","ND"))
#'
#' Now let's estimate the number of plots disturbed and not disturbed. To do this, create a contingency table
#' 
cont_table_COND<- WI_CP %>% group_by(DIST) %>%
  summarise(n_plots=n())
#'
head(cont_table_COND,15)
#'
#' This disturbance variable identifies disturbances that affect 25% or more of trees in the condition
#'
#' ### 2. Prepare the tree table ###ADD COUNTY CODE!, double check issues wit repetitions in subplot level
#' 
#' Scale up to calculate proportion of disturbed plots at a plot level
#' 
#WI_DIST<- DIST_SUBP #create a duplicate of the dataset
#WI_DIST$DIST<- ifelse(WI_DIST$DIST_TYPE!= 0, 1,0) #create a variable for presence or absence of disturbance
  
#TREE_DIST<- WI_DIST %>% group_by(INVYR,PLOT) %>%
#  summarise(DIST= sum(DIST, na.rm=T)) #add up the disturbance types per plot.
#'
#head(TREE_DIST,15)
#'
#TREE_DIST$DIST_SC<-ifelse(TREE_DIST$DIST=1,"SD",)         
#'                  
#'         
#'
#' Spun using:
#' 
#'   ezspin("code/Disturbance_variables.R", out_dir = "output", keep_md=FALSE) 
