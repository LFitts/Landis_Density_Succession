# Preparing the dataset for Wisconsin ----
###################################################################################
#' ### Installing and loading the libraries needed
#'
#install.packages("tidyverse")
#install.packages("knitr")
#install.packages("ezknit")
#install.packages("rFIA")
library(tidyverse)
library(knitr)
#library(ezknitr)
library(rFIA)

###################################################################################

#' # 1.Download the FIA data for the 48 conterminous states ----
#' 
#' Read a CSV file containing the list of US states
#' 
#states<- read.csv("data/states_list.CSV")
#'
#states<-states[-c(2,3,10,13,14,38,43,52),] #Keep just the 48 lower states
#'
#states<-states[,2] #keep just the abbreviations column
#states<-as.list(states) #convert it to a vector format
#' 
#' Get the FIA data from rFIA
#'
#' Check https://rfia.netlify.app/tutorial/bigdata/ for methods larger-than-RAM
#'
#getFIA(states = states, dir = 'H:/FIA_Wisconsin/Landis_Density_Succession/data/all_FIA', load = FALSE, tables = c("PLOT", "COND", "TREE"), nCores=3)
#'
#FIA<-readFIA(dir = "H:/FIA_Wisconsin/Landis_Density_Succession/data/all_FIA", tables = c("PLOT", "COND", "TREE"), states = NULL, inMemory = F, nCores = 3) #inMemory=FALSE helps conserve RAM and allows the user to produce estimates using very large datasets
#'
#'
###################################################################################
for(i in 1:2){
  FIA_TREE<-read.csv(states[3*i])
  FIA_PLOT<-read.csv(states[(3*i)-1])
  FIA_COND<-read.csv(states[(3*i)-2])
  #'
  #' Subset to just keep records from 2000 on
  #'
  FIA_COND<-subset(FIA_COND, INVYR >= 2000)
  FIA_PLOT<-subset(FIA_PLOT, INVYR >= 2000)
  FIA_TREE<-subset(FIA_TREE, INVYR >= 2000)
  #'
  #' Keep each identifier record different before combining tables
  #'
  colnames(FIA_PLOT)[1]<-"PLT_CN"
  colnames(FIA_COND)[1]<-"COND_CN"
  colnames(FIA_TREE)[1]<-"TREE_CN"
  #'
  #' Select variables of interest for each table
  #' 
  FIA_COND <- select(FIA_COND, PLT_CN, INVYR, STATECD, COUNTYCD, PLOT,COND_STATUS_CD, CONDID, DSTRBCD1, DSTRBCD2, DSTRBCD3)
  FIA_PLOT <- select(FIA_PLOT, PLT_CN, INVYR, STATECD, COUNTYCD, PLOT, ELEV, ECOSUBCD, CYCLE)
  FIA_TREE <- select(FIA_TREE, TREE_CN,PLT_CN, INVYR, STATECD, COUNTYCD, PLOT, SUBP,CONDID, TREE, STATUSCD, SPCD, SPGRPCD, DIA, DIAHTCD, HT, ACTUALHT, AGENTCD, DAMAGE_AGENT_CD1, DAMAGE_AGENT_CD2, DAMAGE_AGENT_CD3, MORTYR, STANDING_DEAD_CD, TPA_UNADJ, DRYBIO_BOLE, DRYBIO_TOP, DRYBIO_STUMP, DRYBIO_SAPLING, DRYBIO_WDLD_SPP, DRYBIO_BG, DRYBIO_AG, CARBON_AG, CARBON_BG)
  #'
  #' Merge the tables
  #'
  FIA_CP<-merge(FIA_PLOT, FIA_COND, by=c("STATECD","COUNTYCD","PLOT","INVYR", "PLT_CN")) #merge the plot and condition tables
  #'
  FIA_TP<- merge(FIA_TREE, FIA_PLOT, by=c("STATECD","COUNTYCD","PLOT","INVYR", "PLT_CN")) #merge tree and plot tables
  #'
  FIA_TC<- merge(FIA_TREE, FIA_COND, by=c("STATECD","COUNTYCD","PLOT","INVYR", "CONDID" ,"PLT_CN")) #merge the Tree and condition tables
  #'
  FIA_TCP<- merge(FIA_TC, FIA_PLOT, by=c("STATECD","COUNTYCD","PLOT","INVYR", "PLT_CN")) #merge the three tables
  #'
  #' 
  ###################################################################################
  #' # DISTURBANCE VARIABLE COMPARISON
  #' 
  #' Tables created here are just for comparing the two disturbances variables. To differentiate the variables, we will call "diffused disturbance" to the disturbance variable created from the tree table. The original disturbance variable from the condition table will be called "condition disturbance". This variable has a 25% threshold, meaning that if the condition in the subplot has >=25% of the trees affected by a disturbance and more than 1 acre in size, it will be recorded as such, otherwise, it won't show as a disturbance
  #'
  #' Starting dataset: FIA_TCP 
  #' 
  FIA_TCP <- FIA_TCP%>% filter( DIA >= 5) #filter out trees that correspond to microplot
  #'
  FIA_TCP[is.na(FIA_TCP)] <- 0 #fill NA's with zeros
  #'
  #' Create an ID
  #' 
  FIA_TCP$ID<- paste(FIA_TCP$STATECD, FIA_TCP$COUNTYCD, FIA_TCP$PLOT, FIA_TCP$INVYR, sep="_")
  #'
  ###################################################################################
  # Condition disturbance table ----
  #'
  #' ## Now, let's compare is this new variable works better at capturing disturbances 
  #' For doing this, we will compare the original 'condition-level disturbance variable' (COND_DIST) with the 'tree-level disturbance variable' (TREE_DIST) through a X^2 test.
  #' There will be three categories in each variable: not disturbed (ND), simple disturbance (SD), and compound disturbance (CD)
  #' 
  FIA_TCP2 <-FIA_TCP
  #' 
  #' ### 2. Prepare the condition table
  #'  
  #'  First let's scale up to a plot level
  #' 
  #' Convert to long format
  #' 
  plot_FIA_CP <- FIA_TCP2 %>% pivot_longer(DSTRBCD1:DSTRBCD3,names_to="DSTRBCD" , values_to="DIST")
  #'
  #' Get unique observations for the dataset
  #' 
  plot_FIA_CP <- plot_FIA_CP %>% select(STATECD, COUNTYCD, PLOT, INVYR, CONDID, ID, DSTRBCD, DIST) #select variables of interest
  #'
  plot_FIA_CP<- unique(plot_FIA_CP) #unique observations for the dataset   
  #'
  #' Create a new column with numbers 1 for presence of a disturbance and 0 for absence of a disturbance
  #' 
  plot_FIA_CP$dist_count <- ifelse(plot_FIA_CP$DIST != 0, 1,0) 
  #' 
  #' Scaling to a plot level  
  plot_FIA_CP<- plot_FIA_CP %>% group_by(STATECD, COUNTYCD, PLOT, INVYR, DIST ) %>% 
    summarise (disturbance = sum(dist_count, na.rm=T)) #it is adding up the presence of the same disturbances from all the conditions in a plot. We might have a disturbance spreading on more than 1 condition
  #'
  #' Keep single disturbance records per plot
  #' 
  plot_FIA_CP$disturbance<- ifelse(plot_FIA_CP$disturbance != 0,1,0)#because we just want the disturbances in each plot and are not interested in repeated disturbances in a plot (if condition 1 and 2 had fire for example, we want to record it just once for the plot)
  #'                  
  #' Now we can scale up to a plot level:
  #' 
  plot_FIA_CP<- plot_FIA_CP %>% group_by(STATECD, COUNTYCD, PLOT, INVYR) %>% 
    summarise (disturbance_sum = sum(disturbance, na.rm=T)) 
  #'
  #' The logic here is that plots that were affected by more than one disturbance type (CD) will be represented with value >1 for the sum of disturbances (We already took care of repeated disturbance types per plot in the previous step). A value of 1 represents SD and a value of 0 represents ND
  #' 
  #' Now standardize the names ND=0, SD=1, CD >=2
  #' 
  plot_FIA_CP$disturbance <- ifelse(plot_FIA_CP$disturbance_sum == 0, "ND",
                                   ifelse(plot_FIA_CP$disturbance_sum == 1 , "SD", "CD"))  
  #' 
  #' 
  cont_table_COND<- plot_FIA_CP %>% group_by(disturbance) %>%
    summarise(n_plots=n()) #get the number of plots per grouped category/disturbance type (SD, CD, ND)
  #'
  print(cont_table_COND) #SAVE CSV!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #'
  #' This disturbance variable identifies disturbances that affect 25% or more of trees in the condition
  #' 
  #' ########################################################################################
  # Diffused disturbance table (tree) ----
  #'
  #' ## 3. Create diffuse disturbance variable
  #' 
  #' ### Create a disturbance variable per subplot per condid
  #' 
  #' ### 3.1. Prepare the table
  #'
  FIA_TCP3 <-FIA_TCP
  #'
  #' #### 3.2 Use the group_by and summarise functions to create a table containing disturbances per diameter class in each condition
  #' 
  DIST_SUBP_CONDID<- FIA_TCP3 %>% group_by(STATECD, COUNTYCD, PLOT, INVYR, CONDID, ID,AGENTCD)%>% #Agent of mortality will be our disturbance variable
    summarise( N_TREES_DIA= n())
  #'
  #' Add a new column indicating just the number of trees disturbed in each diameter class
  #' 
  DIST_SUBP_CONDID$N_DIST_TREES <- ifelse (DIST_SUBP_CONDID$AGENTCD == 0, 0, DIST_SUBP_CONDID$N_TREES_DIA)
  #'
  #' Rename AGENTCD variable to DIST_TYPE
  #' 
  colnames(DIST_SUBP_CONDID)[7] <- "DIST_TYPE"
  #'
  #' Now create a column that calculates the proportion of trees that were affected by a disturbance per condition in each plot
  #' 
  #' Group by inventory year, plot, condition id and disturbance type to calculate the number of disturbed trees.   
  #'
  dist_condid<-DIST_SUBP_CONDID %>% group_by(STATECD, COUNTYCD, PLOT, INVYR,CONDID, ID ,DIST_TYPE)%>%
    summarise(trees_dist= sum(N_DIST_TREES))
  #'
  #' Group by inventory year, plot, and condition id to calculate the total number of trees per condid
  #'  
  condid<-DIST_SUBP_CONDID %>% group_by(STATECD, COUNTYCD, PLOT, INVYR, CONDID, ID)%>%
    summarise(trees_tot= sum(N_TREES_DIA))
  #' 
  #' Now merge back the two tables and calculate the proportion of disturbed trees per condition id
  #' 
  disturbances_tree<- merge(dist_condid,condid, by=c("STATECD", "COUNTYCD", "PLOT", "INVYR", "CONDID", "ID")) %>%
    mutate(proportion=(trees_dist/trees_tot)*100)
  #'
  #'
  #' ####################################################################################
  # Diffused variable no proportion threshold ----
  #' ####################################################################################
  #' ## 4. All diffused disturbance
  #' 
  #' #### 4.1 Create a loop to number the disturbances that show in each condition (to keep multiple disturbances)
  #' 
  #' Create an ID for each condition per plot
  #'
  disturbances_tree$IDC <- paste(disturbances_tree$STATECD, disturbances_tree$COUNTYCD, disturbances_tree$PLOT, disturbances_tree$INVYR, disturbances_tree$CONDID, sep="_")
  #'   
  vec<-unique(disturbances_tree$IDC) #vector to use in the loop
  #'
  disturbances_tree$Dcode<-0 #create a column to store the numbers for disturbances occurrences 
  #'
  #' Order the disturbance codes so the disturbances will be coded first
  #' 
  disturbances_tree<-disturbances_tree[order(disturbances_tree$DIST_TYPE, decreasing=T),]
  #'
  for(i in 1:length(vec)){
    tempkey=vec[i]
    counter=1
    for(j in 1:nrow(disturbances_tree)){
      if(disturbances_tree$IDC[j]==tempkey){
        disturbances_tree$Dcode[j]=counter
        counter=counter+1
      }
    }
  }
  #'
  #' Now convert the table to a wide format to identify the presence of simple vs compound disturbances
  #' 
  #' 
  disturbancesTreeTable <- disturbances_tree %>%
    pivot_wider(names_from = Dcode, values_from = DIST_TYPE, id_cols=c(STATECD, COUNTYCD, PLOT, INVYR, CONDID, IDC)) #convert to a wide format to identify compound disturbances
  #'
  #' Rename columns names (1,2... to dist1, dist2...)
  #' 
  disturbancesTreeTable<-disturbancesTreeTable %>% 
    rename( "dist1"="1",
            "dist2"="2",
            "dist3"="3",
            "dist4"="4")
  #'
  #' Now create a column that identifies if a plot had a single (SD), compound (CD) or no disturbance (ND)
  #'
  disturbancesTreeTable[is.na(disturbancesTreeTable)] <- 0 #fill NA's with zeros
  #'
  head(disturbancesTreeTable)
  #'
  #' This table resembles the condition table with how disturbances are recorded per condid
  #' 
  #'  #### 4.2 Now let's scale up to a plot level (do same steps as in section 2 condition table creation)
  #' 
  #' Convert to long format
  #' 
  disturbancesTreeTable <- disturbancesTreeTable %>% pivot_longer(dist1:dist4,names_to="DSTRBCD" , values_to="DIST")
  #'
  #' Get unique observations for the dataset
  #' 
  disturbancesTreeTable <- unique(disturbancesTreeTable) #make sure we have unique observations for the dataset   
  #'
  #' Create a new column with numbers 1 for presence of a disturbance and 0 for absence of a specific disturbance
  #' 
  disturbancesTreeTable$dist_count <- ifelse(disturbancesTreeTable$DIST != 0, 1,0) 
  #'   
  disturbancesTreeTable<- disturbancesTreeTable %>% group_by(STATECD, COUNTYCD, PLOT, INVYR, DIST ) %>% 
    summarise (disturbance = sum(dist_count, na.rm=T)) #it is adding up the presence of the same disturbances from each condition in a plot
  #'
  disturbancesTreeTable$disturbance<- ifelse(disturbancesTreeTable$disturbance != 0,1,0)#because we just want the disturbances in each plot and are not interested in repeated disturbances in a plot (if condition 1 and 2 had fire for example, we want to record it just once for the plot)
  #'                  
  #' Now we can scale up to a plot level:
  #' 
  disturbancesTreeTable<- disturbancesTreeTable %>% group_by(STATECD, COUNTYCD, PLOT, INVYR) %>% 
    summarise (disturbance_sum = sum(disturbance, na.rm=T)) 
  #'
  #' The logic here is that plots that were affected by more than one disturbance (CD) will be represented with value >1 for the sum of disturbances. A value of 1 represents SD and a value of 0 represents ND
  #' 
  #' Now standardize the names ND=0, SD=1, CD >=2
  #' 
  disturbancesTreeTable$disturbance <- ifelse(disturbancesTreeTable$disturbance_sum == 0, "ND",
                                              ifelse(disturbancesTreeTable$disturbance_sum == 1 , "SD", "CD"))  
  #' 
  #' 
  cont_table_TREE<- disturbancesTreeTable %>% group_by(disturbance) %>%
    summarise(n_plots=n())
  #'
  #cont_table_TREE # SAVE CSV3LFJKSAKJFSAKJFDJKFKJDF!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #'
  #' ####################################################################################
  # Diffused variable 25% threshold ----
  #' ####################################################################################
  #' 
  #' ## 5. Diffused disturbance proportion 25% or higher
  #' 
  #' Now that we have the proportion of trees affected by each disturbance, we will make this table comparable with the condition disturbance variable. For that, reclassify every disturbance >=25% to disturbed and the ones <25% as not disturbed.
  #' 
  disturbances_tree25<- merge(dist_condid,condid, by=c("STATECD", "COUNTYCD", "PLOT", "INVYR", "CONDID", "ID")) %>%
    mutate(proportion=(trees_dist/trees_tot)*100) #create the proportion variable
  #'
  disturbances_tree25$DIST_TYPE<- ifelse(disturbances_tree25$proportion >= 25, disturbances_tree25$DIST_TYPE, 0 )
  #'
  #' Create an ID for each condition per plot
  #'
  disturbances_tree25$IDC <- paste(disturbances_tree25$STATECD, disturbances_tree25$COUNTYCD, disturbances_tree25$PLOT, disturbances_tree25$INVYR, disturbances_tree25$CONDID, sep="_")
  #'   
  vec<-unique(disturbances_tree25$IDC) #vector to use in the loop
  #'
  disturbances_tree25$Dcode<-0 #create a column to store the numbers for disturbances occurrences 
  #'
  #' Order the disturbance codes so the disturbances will be coded first
  #' 
  disturbances_tree25<-disturbances_tree25[order(disturbances_tree25$DIST_TYPE, decreasing=T),]
  #'
  for(i in 1:length(vec)){
    tempkey=vec[i]
    counter=1
    for(j in 1:nrow(disturbances_tree25)){
      if(disturbances_tree25$IDC[j]==tempkey){
        disturbances_tree25$Dcode[j]=counter
        counter=counter+1
      }
    }
  }
  #'
  #' Now convert the table to a wide format to identify the presence of simple vs compound disturbances
  #' 
  #' 
  disturbancesTreeTable25 <- disturbances_tree25 %>%
    pivot_wider(names_from = Dcode, values_from = DIST_TYPE, id_cols=c(STATECD, COUNTYCD, PLOT, INVYR, CONDID, IDC)) #convert to a wide format to identify compound disturbances
  #'
  #' Rename columns names (1,2... to dist1, dist2...)
  #' 
  disturbancesTreeTable25<-disturbancesTreeTable25 %>% 
    rename( "dist1"="1",
            "dist2"="2",
            "dist3"="3",
            "dist4"="4")
  #'
  #' Now create a column that identifies if a plot had a single (SD), compound (CD) or no disturbance (ND)
  #'
  disturbancesTreeTable25[is.na(disturbancesTreeTable25)] <- 0 #fill NA's with zeros
  #'
  #' This table resembles the condition table with how disturbances are recorded per condid
  #' 
  #'  Now let's scale up to a plot level
  #' 
  #' Convert to long format
  #' 
  disturbancesTreeTable25 <- disturbancesTreeTable25 %>% pivot_longer(dist1:dist4,names_to="DSTRBCD" , values_to="DIST")
  #'
  #' Get unique observations for the dataset
  #' 
  disturbancesTreeTable25 <- unique(disturbancesTreeTable25) #make sure we have unique observations for the dataset   
  #'
  #' Create a new column with numbers 1 for presence of a disturbance and 0 for absence of a specific disturbance
  #' 
  disturbancesTreeTable25$dist_count <- ifelse(disturbancesTreeTable25$DIST != 0, 1,0) 
  #'   
  disturbancesTreeTable25<- disturbancesTreeTable25 %>% group_by(STATECD, COUNTYCD, PLOT, INVYR, DIST ) %>% 
    summarise (disturbance = sum(dist_count, na.rm=T)) #it is adding up the presence of the same disturbances from each condition in a plot
  #'
  disturbancesTreeTable25$disturbance<- ifelse(disturbancesTreeTable25$disturbance != 0,1,0)#because we just want the disturbances in each plot and are not interested in repeated disturbances in a plot (if condition 1 and 2 had fire for example, we want to record it just once for the plot)
  #'                  
  #' Now we can scale up to a plot level:
  #' 
  disturbancesTreeTable25<- disturbancesTreeTable25 %>% group_by(STATECD, COUNTYCD, PLOT, INVYR) %>% 
    summarise (disturbance_sum = sum(disturbance, na.rm=T)) 
  #'
  #' The logic here is that plots that were affected by more than one disturbance (CD) will be represented with value >1 for the sum of disturbances. A value of 1 represents SD and a value of 0 represents ND
  #' 
  #' Now standardize the names ND=0, SD=1, CD >=2
  #' 
  disturbancesTreeTable25$disturbance <- ifelse(disturbancesTreeTable25$disturbance_sum == 0, "ND",
                                                ifelse(disturbancesTreeTable25$disturbance_sum == 1 , "SD", "CD"))  
  #' 
  #' 
  cont_table_TREE25<- disturbancesTreeTable25 %>% group_by(disturbance) %>%
    summarise(n_plots=n())
  #'
 # cont_table_TREE25  #save CSV!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#'  
}













###################################################################################
#' # 2.Creation of generic functions for disturbance variable comparison ----
#'
f1=function(c,t,p){
  colnames(p)[1]<-"PLT_CN" #' Keep each identifier record different before combining tables
  colnames(c)[1]<-"COND_CN"
  colnames(t)[1]<-"TREE_CN"  
  c<- subset(c, INVYR >= 2000) #' Subset to just keep records from 2000 on
  t<- subset(t, INVYR >= 2000) 
  p<- subset(p, INVYR >= 2000)
}
#'
f2=function(c,t,p){
  #' Merge the tables
  FIA_CP<-merge(p,c, by=c("STATECD","COUNTYCD","PLOT","INVYR", "PLT_CN")) #merge the plot and condition tables
  FIA_TP<- merge(t, p, by=c("STATECD","COUNTYCD","PLOT","INVYR", "PLT_CN")) #merge tree and plot tables
  FIA_TC<- merge(t, c, by=c("STATECD","COUNTYCD","PLOT","INVYR", "CONDID" ,"PLT_CN")) #merge the Tree and condition tables
  FIA_TCP<- merge(FIA_TC, p, by=c("STATECD","COUNTYCD","PLOT","INVYR", "PLT_CN")) #merge the three tables
  #' 
  FIA_TCP <- FIA_TCP%>% filter( DIA >= 5) #filter out trees that correspond to microplot
  FIA_TCP[is.na(FIA_TCP)] <- 0 #fill NA's with zeros
  FIA_TCP$ID<- paste(FIA_TCP$STATECD, FIA_TCP$COUNTYCD, FIA_TCP$PLOT, FIA_TCP$INVYR, sep="_")#' Create an ID
  print(FIA_TCP)
} #next function, x should be FIA_TCP
#'
#' Now, let's compare is this new variable works better at capturing disturbances 
#' For doing this, we will compare the original 'condition-level disturbance variable' (COND_DIST) with the 'tree-level disturbance variable' (TREE_DIST) through a X^2 test.
#' There will be three categories in each variable: not disturbed (ND), simple disturbance (SD), and compound disturbance (CD)
#' 
f3=function(n){
  # Condition disturbance table
  FIA_TCP2 <-n
  #'  First let's scale up to a plot level
  plot_FIA_CP <- FIA_TCP2 %>% pivot_longer(DSTRBCD1:DSTRBCD3,names_to="DSTRBCD" , values_to="DIST") #Convert to long format
  #'
  plot_FIA_CP <- plot_FIA_CP %>% select(STATECD, COUNTYCD, PLOT, INVYR, CONDID, ID, DSTRBCD, DIST) #select variables of interest
  #'
  plot_FIA_CP<- unique(plot_FIA_CP) #unique observations for the dataset   
  #'
  plot_FIA_CP$dist_count <- ifelse(plot_FIA_CP$DIST != 0, 1,0) # Create a new column with numbers 1 for presence of a disturbance and 0 for absence of a disturbance
  #' Scaling to a plot level  
  plot_FIA_CP<- plot_FIA_CP %>% group_by(STATECD, COUNTYCD, PLOT, INVYR, DIST ) %>% 
    summarise (disturbance = sum(dist_count, na.rm=T)) #it is adding up the presence of the same disturbances from all the conditions in a plot. We might have a disturbance spreading on more than 1 condition
  #'
  #' Keep single disturbance records per plot
  plot_FIA_CP$disturbance<- ifelse(plot_FIA_CP$disturbance != 0,1,0)#because we just want the disturbances in each plot and are not interested in repeated disturbances in a plot (if condition 1 and 2 had fire for example, we want to record it just once for the plot)
  #' Now we can scale up to a plot level:
  #' 
  plot_FIA_CP<- plot_FIA_CP %>% group_by(STATECD, COUNTYCD, PLOT, INVYR) %>% 
    summarise (disturbance_sum = sum(disturbance, na.rm=T)) # The logic here is that plots that were affected by more than one disturbance type (CD) will be represented with value >1 for the sum of disturbances (We already took care of repeated disturbance types per plot in the previous step). A value of 1 represents SD and a value of 0 represents ND
  #' 
  #' Now standardize the names ND=0, SD=1, CD >=2
  plot_FIA_CP$disturbance <- ifelse(plot_FIA_CP$disturbance_sum == 0, "ND",
                                    ifelse(plot_FIA_CP$disturbance_sum == 1 , "SD", "CD"))  
  #' 
  cont_table_COND<- plot_FIA_CP %>% group_by(disturbance) %>%
    summarise(n_plots=n()) #get the number of plots per grouped category/disturbance type (SD, CD, ND)
  #'
  print(cont_table_COND) #' This disturbance variable identifies disturbances that affect 25% or more of trees in the condition
  # HOW DO I SAVE IT AS A CSV??????????????????????
}                        
#'
#' 
f4=function(n){
  # Diffused disturbance data preparation
  # Create a disturbance variable per subplot per condid  
  FIA_TCP3 <-n
  # Use the group_by and summarise functions to create a table containing disturbances per diameter class in each condition
  DIST_SUBP_CONDID<- FIA_TCP3 %>% group_by(STATECD, COUNTYCD, PLOT, INVYR, CONDID, ID,AGENTCD)%>% #Agent of mortality will be our disturbance variable
    summarise( N_TREES_DIA= n())
  #' Add a new column indicating just the number of trees disturbed in each diameter class
  DIST_SUBP_CONDID$N_DIST_TREES <- ifelse (DIST_SUBP_CONDID$AGENTCD == 0, 0, DIST_SUBP_CONDID$N_TREES_DIA)
  colnames(DIST_SUBP_CONDID)[7] <- "DIST_TYPE" #Rename AGENTCD variable to DIST_TYPE
  # Now create a column that calculates the proportion of trees that were affected by a disturbance per condition in each plot
  # Group by inventory year, plot, condition id and disturbance type to calculate the number of disturbed trees.   
  #'
  dist_condid<-DIST_SUBP_CONDID %>% group_by(STATECD, COUNTYCD, PLOT, INVYR,CONDID, ID ,DIST_TYPE)%>%
    summarise(trees_dist= sum(N_DIST_TREES))
  # Group by inventory year, plot, and condition id to calculate the total number of trees per condid
  #'  
  condid<-DIST_SUBP_CONDID %>% group_by(STATECD, COUNTYCD, PLOT, INVYR, CONDID, ID)%>%
    summarise(trees_tot= sum(N_TREES_DIA))
  # Now merge back the two tables and calculate the proportion of disturbed trees per condition id
  #' 
  disturbances_tree<- merge(dist_condid,condid, by=c("STATECD", "COUNTYCD", "PLOT", "INVYR", "CONDID", "ID")) %>%
    mutate(proportion=(trees_dist/trees_tot)*100)
  #'
  print(disturbances_tree)
}
#' 
#' 
f5=function(n){
  # Diffused variable no proportion threshold
  # Create a loop to number the disturbances that show in each condition (to keep multiple disturbances)
  #' 
  n$IDC <- paste(n$STATECD, n$COUNTYCD, n$PLOT, n$INVYR, n$CONDID, sep="_") #Create an ID for each condition per plot
  vec<-unique(n$IDC) #vector to use in the loop
  n$Dcode<-0 #create a column to store the numbers for disturbances occurrences 
  #' 
  disturbances_tree<-n[order(n$DIST_TYPE, decreasing=T),]#Order the disturbance codes so the disturbances will be coded first
  #'
  for(i in 1:length(vec)){
    tempkey=vec[i]
    counter=1
    for(j in 1:nrow(disturbances_tree)){
      if(disturbances_tree$IDC[j]==tempkey){
        disturbances_tree$Dcode[j]=counter
        counter=counter+1
      }
    }
  }
  #' Now convert the table to a wide format to identify the presence of simple vs compound disturbances
  disturbancesTreeTable <- disturbances_tree %>%
    pivot_wider(names_from = Dcode, values_from = DIST_TYPE, id_cols=c(STATECD, COUNTYCD, PLOT, INVYR, CONDID, IDC)) #convert to a wide format to identify compound disturbances
  #'
  disturbancesTreeTable<-disturbancesTreeTable %>% # Rename columns names (1,2... to dist1, dist2...)
    rename( "dist1"="1",
            "dist2"="2",
            "dist3"="3",
            "dist4"="4")
  #'
  #' Now create a column that identifies if a plot had a single (SD), compound (CD) or no disturbance (ND)
  #'
  disturbancesTreeTable[is.na(disturbancesTreeTable)] <- 0 #fill NA's with zeros. This table resembles the condition table with how disturbances are recorded per condid
  #'
  # Now let's scale up to a plot level (do same steps as in section 2 condition table creation)
  #' 
  disturbancesTreeTable <- disturbancesTreeTable %>% pivot_longer(dist1:dist4,names_to="DSTRBCD" , values_to="DIST") #Convert to long format
  #' Get unique observations for the dataset
  #' 
  disturbancesTreeTable <- unique(disturbancesTreeTable) #make sure we have unique observations for the dataset   
  #'
  #' Create a new column with numbers 1 for presence of a disturbance and 0 for absence of a specific disturbance
  #' 
  disturbancesTreeTable$dist_count <- ifelse(disturbancesTreeTable$DIST != 0, 1,0) 
  #'   
  disturbancesTreeTable<- disturbancesTreeTable %>% group_by(STATECD, COUNTYCD, PLOT, INVYR, DIST ) %>% 
    summarise (disturbance = sum(dist_count, na.rm=T)) #it is adding up the presence of the same disturbances from each condition in a plot
  #'
  disturbancesTreeTable$disturbance<- ifelse(disturbancesTreeTable$disturbance != 0,1,0)#because we just want the disturbances in each plot and are not interested in repeated disturbances in a plot (if condition 1 and 2 had fire for example, we want to record it just once for the plot)
  #'                  
  # Now we can scale up to a plot level:
  disturbancesTreeTable<- disturbancesTreeTable %>% group_by(STATECD, COUNTYCD, PLOT, INVYR) %>% 
    summarise (disturbance_sum = sum(disturbance, na.rm=T)) # The logic here is that plots that were affected by more than one disturbance (CD) will be represented with value >1 for the sum of disturbances. A value of 1 represents SD and a value of 0 represents ND
  #' 
  disturbancesTreeTable$disturbance <- ifelse(disturbancesTreeTable$disturbance_sum == 0, "ND",
                                              ifelse(disturbancesTreeTable$disturbance_sum == 1 , "SD", "CD"))  # Standardize the names ND=0, SD=1, CD >=2
  #' 
  cont_table_TREE<- disturbancesTreeTable %>% group_by(disturbance) %>%
    summarise(n_plots=n())
  #'
  print(cont_table_TREE) #SAVE AS A CSV!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
}
#'
#' 
f6=function(x,y){
  #Diffused disturbance proportion 25% or higher
  #' Now that we have the proportion of trees affected by each disturbance, we will make this table comparable with the condition disturbance variable. For that, reclassify every disturbance >=25% to disturbed and the ones <25% as not disturbed.
  disturbances_tree25<- merge(x,y, by=c("STATECD", "COUNTYCD", "PLOT", "INVYR", "CONDID", "ID")) %>%
    mutate(proportion=(trees_dist/trees_tot)*100) #create the proportion variable
  disturbances_tree25$DIST_TYPE<- ifelse(disturbances_tree25$proportion >= 25, disturbances_tree25$DIST_TYPE, 0 )
  #' Create an ID for each condition per plot
  disturbances_tree25$IDC <- paste(disturbances_tree25$STATECD, disturbances_tree25$COUNTYCD, disturbances_tree25$PLOT, disturbances_tree25$INVYR, disturbances_tree25$CONDID, sep="_")
  #'   
  vec<-unique(disturbances_tree25$IDC) #vector to use in the loop
  disturbances_tree25$Dcode<-0 #create a column to store the numbers for disturbances occurrences 
  #'
  disturbances_tree25<-disturbances_tree25[order(disturbances_tree25$DIST_TYPE, decreasing=T),] # Order the disturbance codes so the disturbances will be coded first
  #'
  for(i in 1:length(vec)){
    tempkey=vec[i]
    counter=1
    for(j in 1:nrow(disturbances_tree25)){
      if(disturbances_tree25$IDC[j]==tempkey){
        disturbances_tree25$Dcode[j]=counter
        counter=counter+1
      }
    }
  }
  #'
  #' Now convert the table to a wide format to identify the presence of simple vs compound disturbances
  disturbancesTreeTable25 <- disturbances_tree25 %>%
    pivot_wider(names_from = Dcode, values_from = DIST_TYPE, id_cols=c(STATECD, COUNTYCD, PLOT, INVYR, CONDID, IDC)) #convert to a wide format to identify compound disturbances
  #'
  disturbancesTreeTable25<-disturbancesTreeTable25 %>% # Rename columns names (1,2... to dist1, dist2...)
    rename( "dist1"="1",
            "dist2"="2",
            "dist3"="3",
            "dist4"="4")
  #'
  #' Now create a column that identifies if a plot had a single (SD), compound (CD) or no disturbance (ND)
  #'
  disturbancesTreeTable25[is.na(disturbancesTreeTable25)] <- 0 #fill NA's with zeros. This table resembles the condition table with how disturbances are recorded per condid
  #'
  #'  Now let's scale up to a plot level
  disturbancesTreeTable25 <- disturbancesTreeTable25 %>% pivot_longer(dist1:dist4,names_to="DSTRBCD" , values_to="DIST") # Convert to long format
  #'
  disturbancesTreeTable25 <- unique(disturbancesTreeTable25) #make sure we have unique observations for the dataset   
  #'
  disturbancesTreeTable25$dist_count <- ifelse(disturbancesTreeTable25$DIST != 0, 1,0) # Create a new column with numbers 1 for presence of a disturbance and 0 for absence of a specific disturbance
  #'   
  disturbancesTreeTable25<- disturbancesTreeTable25 %>% group_by(STATECD, COUNTYCD, PLOT, INVYR, DIST ) %>% 
    summarise (disturbance = sum(dist_count, na.rm=T)) #it is adding up the presence of the same disturbances from each condition in a plot
  #'
  disturbancesTreeTable25$disturbance<- ifelse(disturbancesTreeTable25$disturbance != 0,1,0)#because we just want the disturbances in each plot and are not interested in repeated disturbances in a plot (if condition 1 and 2 had fire for example, we want to record it just once for the plot)
  #'                  
  #' Now we can scale up to a plot level:
  #' 
  disturbancesTreeTable25<- disturbancesTreeTable25 %>% group_by(STATECD, COUNTYCD, PLOT, INVYR) %>% 
    summarise (disturbance_sum = sum(disturbance, na.rm=T)) 
  #'
  #' The logic here is that plots that were affected by more than one disturbance (CD) will be represented with value >1 for the sum of disturbances. A value of 1 represents SD and a value of 0 represents ND
  #' 
  #' Now standardize the names ND=0, SD=1, CD >=2
  #' 
  disturbancesTreeTable25$disturbance <- ifelse(disturbancesTreeTable25$disturbance_sum == 0, "ND",
                                                ifelse(disturbancesTreeTable25$disturbance_sum == 1 , "SD", "CD"))  
  #' 
  #' 
  cont_table_TREE25<- disturbancesTreeTable25 %>% group_by(disturbance) %>%
    summarise(n_plots=n())
  #'
  print(cont_table_TREE25) #save as CSV!!!!!!!qwrew#rwrer
}
#'
###################################################################################
#' Set working directory where all the state files are located
#'
setwd("H:/FIA_Wisconsin/Landis_Density_Succession/data/all_FIA") #set working directory to the subfolder where the CSV files are located
#'
#' Create a vector for the states
#'   
states<-list.files("data/all_FIA/")
#'
###################################################################################

#' # 3.Creation of the loop to run the functions for all the states ----
#'
#' Now create a loop with all the functions within       
#'                  
#'            
for(i in 1:2){
  FIA_TREE<-read.csv(states[3*i])
  FIA_PLOT<-read.csv(states[(3*i)-1])
  FIA_COND<-read.csv(states[(3*i)-2])

  f1(c=FIA_COND,t=FIA_TREE,p=FIA_PLOT)
  f2(c=c,t=t,p=p)
  f3(n=FIA_TCP)
  f4(n=FIA_TCP)
  f5(n=disturbances_tree)
  f6(x=dist_condid, y=condid)
  f7(x=disturbances_tree)
}  
#'
#' ###################################################################
#' 
#'  #' Test it for wisconsin
#' 
FIA_COND<-read.csv(states[136])
FIA_PLOT<-read.csv(states[137])
FIA_TREE<-read.csv(states[138])

f1(c=FIA_COND,t=FIA_TREE,p=FIA_PLOT)
f2(c=c,t=t,p=p)
f3(n=FIA_TCP)
f4(n=FIA_TCP)
f5(n=disturbances_tree)
f6(x=dist_condid, y=condid)
f7(x=disturbances_tree) 
#' 
for(i in 1:2){
  FIA_TREE<-read.csv(states[3*i])
  FIA_PLOT<-read.csv(states[(3*i)-1])
  FIA_COND<-read.csv(states[(3*i)-2])
  
  f1(c=FIA_COND,t=FIA_TREE,p=FIA_PLOT)
  f2(c=c,t=t,p=p)
  f3(n=FIA_TCP)
  f4(n=FIA_TCP)
  f5(n=disturbances_tree)
  f6(x=dist_condid, y=condid)
  f7(x=disturbances_tree)
} 
#' 
#' ################################################################## FOR LATER
#' 
f7=function(x){
# Visualization
x$agent_mortality<- ifelse(x$DIST_TYPE==10,"Insect", # First rename the disturbance types
                                           ifelse(x$DIST_TYPE==20,"Disease",
                                           ifelse(x$DIST_TYPE==30,"Fire",
                                                  ifelse(x$DIST_TYPE==40,"Animal",
                                                         ifelse(x$DIST_TYPE==50,"Weather",
                                                                ifelse(x$DIST_TYPE==60,"Vegetation",
                                                                       ifelse(x$DIST_TYPE==70,"Other",
                                                                              ifelse(x$DIST_TYPE==80,"Silvicultural", 
                                                                                     ifelse(x$DIST_TYPE==0,"Not disturbed", NA)))))))))
#' 
x<- x %>% filter(agent_mortality != "Not disturbed") # Remove the non disturbed category
#'
#' Boxplots
#'
p.box <- ggplot(x, aes(x= agent_mortality, y=proportion)) +
  geom_boxplot(color="black", fill="gray",)+xlab("Disturbance type")+ylab("Proportion of each plot disturbed in %")+theme_classic()
#'
p.box
#'
#' Save the boxplot to a tiff format
#
#ggsave(p.box, file="figures/disturbances/proportion_disturbances.jpg", width=7, height=4)
#'
#ggsave(file="figures/disturbances/proportion_disturbances.tiff", plot = p.box, width=600, height=450, units="mm", dpi=300, compression = "lzw") #save the file!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
}
#'
#'
f8(x=cont_table_COND, y=cont_table_TREE) #Condition disturbance vs diffused disturbance 0% threshold
f8(x=cont_table_COND, y=cont_table_TREE25) #Condition disturbance vs diffused disturbance 25% threshold

f8=function(x,y){
# Chi square tests of independence
cont_table1<- merge(x,y, by="disturbance") #Merge both tables
#'
names(cont_table1)<-c("Disturbance", "Condition_table", "Diffused") # Rename the columns
#'
#Now perform the Chi square independent tests with an alpha of 0.05.
#'
#' Ho: There is no association between variables (they are independent)./ The way we build the disturbance variable is independent from the result
#' Ha: There is an association between variables
#' 
print(x_cond_diff<-cont_table1[,c(2,3)] %>% chisq.test())}#pick columns 2,3 to make it a contingency table
#'
#' As p-value <0.05, we reject Ho, therefore we conclude there is an association between the variables. In other words, the way we calculate the disturbance variable makes a difference 
#' 
#' ####################
#'         
#'
#' Spun using:
#' 
#'   ezspin("code/Disturbance_variables_rFIA.R", out_dir = "output", keep_md=FALSE) 


