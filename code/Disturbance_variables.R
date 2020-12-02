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
WI_COND<-read.csv("data/main_WI_2020/WI_COND.csv", na.strings = "NA")#read the condition table
WI_PLOT<-read.csv("data/main_WI_2020/WI_PLOT.csv", na.strings = "NA")#read the plot table
WI_TREE<-read.csv("data/main_WI_2020/WI_TREE.csv", na.strings = "NA")#read the tree table
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
#' First filter out diameters less than 5" (those correspond to the microplot)
#'
WI_TP <- WI_TP %>% filter( DIA >= 5) #filter out trees that correspond to microplot
#'
#'##########################
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
#' 
###################################################################################
#' # DISTURBANCE VARIABLE COMPARISON
#' 
#' Tables created here are just for comparing the two disturbances variables. To differentiate the variables, we will call "diffused disturbance" to the disturbance variable created from the tree table. The original disturbance variable from the condition table will be called "condition disturbance". This variable has a 25% threshold, meaning that if the condition in the subplot has >=25% of the trees affected by a disturbance and more than 1 acre in size, it will be recorded as such, otherwise, it won't show as a disturbance
#'
#' ## Create a disturbance variable per subplot per condid
#' 
#' ### 1. Prepare the table
#'
#' First filter out diameters less than 5" (those correspond to the microplot)
#'
WI_TCP <- WI_TCP %>% filter( DIA >= 5) #filter out trees that correspond to microplot
#'
#' #### 1.1 Create a unique identifier ID with invyr_plot_subplot_tree
#' 
WI_TCP$ID<- paste(WI_TCP$INVYR, WI_TCP$PLOT, WI_TCP$SUBP, WI_TCP$CONDID,WI_TCP$TREE, sep="_")
#'
#' #' #### 1.2 Classify tree diameters into 5" classes. Update sequenceDIA values when using other States
#' 
sequenceDIA<- seq(from=5, to=90, by=5) #create a sequence from 5 to 90 that will be used in the loop
#'
WI_TCP$DIA_CLASS<- 0 #create an empty column for diameter class
#'
WI_TCP$MIN_DIA<- 0 #create an empty column for the lower end of the diameter class
WI_TCP$MAX_DIA<- 0 #create an empty column for the upper end of the diameter class
#'
for(i in 1:length(sequenceDIA)){
  temp=sequenceDIA[i]
  for(j in 1:nrow(WI_TCP)){
    if(WI_TCP$DIA[j] >= temp & WI_TCP$DIA[j]<(temp+5)
    ){WI_TCP$MIN_DIA[j]=temp
    WI_TCP$MAX_DIA[j]=temp+5
    WI_TCP$DIA_CLASS[j] = paste("[",temp,"-",(temp+5),">")
    }}}
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
#' Now create a column that calculates the proportion of trees that were affected by a disturbance per condition in each plot
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
#' Now we have created the proportion of trees affected by each disturbance. For comparison purposes, we will filter out everything under 25% (to make it comparable with the disturbance condition variable)
#'
disturbances_tree<- disturbances_tree %>% filter(proportion>=25)
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
#' ### 2. Prepare the tree table ###ADD COUNTY CODE!, double check issues with repetitions in subplot level
#'
#' If it is a single disturbance = SD, compound disturbances = CD, no disturbance = ND
#'
#'##################WORKING ON THIS NOW
#disturbances_tree$DIST <- ifelse(disturbances_tree #
                                 
#                                 $DSTRBCD1 != 0 & WI_CP$DSTRBCD2 == 0, "SD",
#                     ifelse(WI_CP$DSTRBCD1 != 0 & WI_CP$DSTRBCD2 != 0 ,"CD","ND"))
#'
#' Now let's estimate the number of plots disturbed and not disturbed. To do this, create a contingency table
#' 
#cont_table_COND<- WI_CP %>% group_by(DIST) %>%
#  summarise(n_plots=n())
#'
#head(cont_table_COND,15)
#' 
#' ####################
#'
#'
#' Scale up to calculate proportion of disturbed plots at a plot level #ERRASE
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
