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
#' Set working directory where all the state files are located
#'
setwd("H:/FIA_Wisconsin/Landis_Density_Succession/data/all_FIA") #set working directory to the subfolder where the CSV files are located
#'
#' Create a vector for the states
#'   
#states<-list.files("data/all_FIA/") #use this or the next line according to where your working directory is set
states<-list.files()
#'
###################################################################################
#' Create empty data frames for storing data on number of plots disturbed
#' 
cond_d_dataframe<-data.frame()
dist_100_dataframe<-data.frame()
dist_25_dataframe<-data.frame()
dist_vis_dataframe<-data.frame()


#' Now create the loop
for(i in 1:48){
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
  cont_table_COND<- plot_FIA_CP %>% group_by(STATECD, disturbance) %>%
    summarise(n_plots=n()) #get the number of plots per grouped category/disturbance type (SD, CD, ND)
  #'
  #'
  cond_d_dataframe<-rbind(cond_d_dataframe, cont_table_COND)
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
  #'
  #' Save the disturbance tree data frame to a cumulative data frame
  #' 
  dist_vis_dataframe<-rbind(dist_vis_dataframe, disturbances_tree)
  #'
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
  cont_table_TREE<- disturbancesTreeTable %>% group_by(STATECD, disturbance) %>%
    summarise(n_plots=n())
  #'
  dist_100_dataframe<-rbind(dist_100_dataframe, cont_table_TREE)
  #'
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
  cont_table_TREE25<- disturbancesTreeTable25 %>% group_by(STATECD, disturbance) %>%
    summarise(n_plots=n())
  #'
 # 
  dist_25_dataframe<-rbind(dist_25_dataframe, cont_table_TREE25)
#'  
}

################################################################################################################
#' #### Save the tables as CSV files ----
#'
setwd("H:/FIA_Wisconsin/Landis_Density_Succession") #set working directory to the subfolder where the CSV files are located
#'
#'   
#write.csv(cond_d_dataframe,'data//condition_disturbance_database.CSV')
#write.csv(dist_100_dataframe,'data//diffused_disturbance100_database.CSV')
#write.csv(dist_25_dataframe,'data//diffused_disturbance25_database.CSV')
#write.csv(dist_vis_dataframe,'data//diffused_disturbance_visualization_database.CSV')
#'
#'disturbances_vis
#'
################################################################################################################
#' ####  Read in the files (when we open a new session)----
#'
cond_d_dataframe<-read.csv('data/condition_disturbance_database.CSV')
dist_100_dataframe<-read.csv('data/diffused_disturbance100_database.CSV')
dist_25_dataframe<-read.csv('data/diffused_disturbance25_database.CSV')
dist_vis_dataframe<- read.csv('data/diffused_disturbance_visualization_database.CSV')
#'
cond_d_dataframe<-cond_d_dataframe[,-1] #delete the automatic X column generated when reading in the file
dist_100_dataframe<-dist_100_dataframe[,-1]
dist_25_dataframe<-dist_25_dataframe[,-1]
dist_vis_dataframe<-dist_vis_dataframe[,-1]
#'
#' ####################################################################################
# Visualization ----
#' ####################################################################################
#'
#' ## Represent proportion of conditions disturbed per disturbance type
#'
#' First rename the disturbance types
#' 
dist_vis_dataframe$agent_mortality<- ifelse(dist_vis_dataframe$DIST_TYPE>=10 & dist_vis_dataframe$DIST_TYPE<20 ,"Insect",
                                           ifelse(dist_vis_dataframe$DIST_TYPE>=20 & dist_vis_dataframe$DIST_TYPE<30,"Disease",
                                                  ifelse(dist_vis_dataframe$DIST_TYPE>=30 & dist_vis_dataframe$DIST_TYPE<40,"Fire",
                                                         ifelse(dist_vis_dataframe$DIST_TYPE>=40 & dist_vis_dataframe$DIST_TYPE<50,"Animal",
                                                                ifelse(dist_vis_dataframe$DIST_TYPE>=50 & dist_vis_dataframe$DIST_TYPE<60,"Weather",
                                                                       ifelse(dist_vis_dataframe$DIST_TYPE>=60 & dist_vis_dataframe$DIST_TYPE<70,"Vegetation",
                                                                              ifelse(dist_vis_dataframe$DIST_TYPE>=70 & dist_vis_dataframe$DIST_TYPE<80,"Other",
                                                                                     ifelse(dist_vis_dataframe$DIST_TYPE>=80 & dist_vis_dataframe$DIST_TYPE<90,"Silviculture", 
                                                                                            ifelse(dist_vis_dataframe$DIST_TYPE==0,"Not disturbed", NA)))))))))
#'
#'
#' Remove the non disturbed category
#' 
dist_vis_dataframe<- dist_vis_dataframe %>% filter(agent_mortality != "Not disturbed")
#'
#' Merge the state names with the table
#' 
statecd<-read.csv('data/states_codes.csv')
dist_vis_dataframe<-merge(statecd, dist_vis_dataframe, by='STATECD')
#'
#'
#' ### Boxplots
#'
#' For all the states combined
#'
p.box <- ggplot(dist_vis_dataframe, aes(x= agent_mortality, y=proportion)) +
  geom_boxplot(color="black", fill="gray",)+xlab("Disturbance type")+ylab("Proportion of each plot disturbed in %")+theme_classic()+
  #theme(axis.text=element_text(size=20, color="black"),
  #      axis.title=element_text(size=20,face="bold"),
  #      strip.text.x = element_text(size = 20))+
        geom_hline(yintercept=25, linetype="dashed", color = "red", size=2)
#'
#'
#' Save the boxplot to a tiff format
#' 
ggsave(p.box, file="figures/disturbances/proportion_disturbances.jpg", width=7, height=4)
#'
ggsave(file="figures/disturbances/proportion_disturbances.tiff", plot = p.box, width=600, height=450, units="mm", dpi=300, compression = "lzw")
#'
#'
#' For all the states combined
#'
p.box_48 <- ggplot(dist_vis_dataframe, aes(x= agent_mortality, y=proportion)) +
  geom_boxplot(color="black", fill="gray",)+xlab("Disturbance type")+ylab("Proportion of each plot disturbed in %")+theme_classic()+
  #theme(axis.text=element_text(size=32, color="black"),
       # axis.title=element_text(size=34,face="bold"),
        #legend.key.size = unit(5,"line"),
        #strip.text.x = element_text(size = 32))+
  geom_hline(yintercept=25, linetype="dashed", color = "red", size=2)+
  facet_wrap(~State)
#'
#'
#' Save the boxplot to a tiff format
#' 
ggsave(p.box_48, file="figures/disturbances/proportion_disturbances_48US.jpg", width=7, height=4)
#'
ggsave(file="figures/disturbances/proportion_disturbances_48US.tiff", plot = p.box_48, width=600, height=450, units="mm", dpi=300, compression = "lzw")
#'
#'
#'
#' ####################################################################################
# Chi square tests ----
#' ####################################################################################
#' 
#' ## Perform tests of independence
#' 
#' Create the tables for 3 types of comparisons:
#' A) Condition disturbance vs diffused disturbance 0% threshold
#' B) Condition disturbance vs diffused disturbance 25% threshold
#' C) Condition disturbance, diffused disturbance 0% threshold, and diffused disturbance 25% threshold
#' 
#' ### Merge the tables
#'
#' #### A) Condition disturbance vs diffused disturbance 0% threshold
cont_tableA<- merge(cond_d_dataframe,dist_100_dataframe, by=c("STATECD", "disturbance"))
#'
#' Rename the columns
names(cont_tableA)<-c("State", "Disturbance", "Condition_table", "Diffused_complete")
#'
#' Test for all states added up condition
#'
#' Add up the plot counts from all the 48 states
#'
cont_tableA_48US<-cont_tableA %>% group_by(Disturbance) %>% 
  summarize(Condition=sum(Condition_table),
            Diffused100=sum(Diffused_complete))
#'
#' #### Now perform the Chi square independent tests with an alpha of 0.05.
#'
#' Ho: There is no association between variables (they are independent)./ The way we build the disturbance variable is independent from the result
#' 
#' Ha: There is an association between variables
#'   
print(x_cond_diffA<-cont_tableA_48US[,c(2,3)] %>% chisq.test()) #pick columns 2,3 to make it a contingency table
#'
#' As p-value <0.05, we reject Ho, therefore we conclude there is an association between the variables. In other words, the way we calculate the disturbance variable makes a difference 
#'   
#'   
#' #### B) Condition disturbance vs diffused disturbance 25% threshold  
cont_tableB<- merge(cond_d_dataframe,dist_25_dataframe, by=c("STATECD", "disturbance"))
#'
#' Rename the columns
names(cont_tableB)<-c("State", "Disturbance", "Condition_table", "Diffused_25")
#'
#' Test for all states added up condition
#'
#' Add up the plot counts from all the 48 states
#'
cont_tableB_48US<-cont_tableB %>% group_by(Disturbance) %>% 
  summarize(Condition=sum(Condition_table),
            Diffused25=sum(Diffused_25))
#'
#' #### Now perform the Chi square independent tests with an alpha of 0.05.
#'
#' Ho: There is no association between variables (they are independent)./ The way we build the disturbance variable is independent from the result
#' 
#' Ha: There is an association between variables
#'   
print(x_cond_diffB<-cont_tableB_48US[,c(2,3)] %>% chisq.test()) #pick columns 2,3 to make it a contingency table
#'
#' As p-value <0.05, we reject Ho, therefore we conclude there is an association between the variables. In other words, the way we calculate the disturbance variable makes a difference 
#'    
#' #### C) Condition disturbance, diffused disturbance 0% threshold, and diffused disturbance 25% threshold
cont_tableC<- merge(cond_d_dataframe,dist_100_dataframe, by=c("STATECD", "disturbance"))
cont_tableC<- merge(cont_tableC,dist_25_dataframe, by=c("STATECD", "disturbance"))
#'
#' Rename the columns
names(cont_tableC)<-c("State", "Disturbance", "Condition_table", "Diffused_complete", "Diffused_25")
#'
#' Test for all states added up condition
#'
#' Add up the plot counts from all the 48 states
#'
cont_tableC_48US<-cont_tableC %>% group_by(Disturbance) %>% 
  summarize(Condition=sum(Condition_table),
            Diffused100=sum(Diffused_complete),
            Diffused25=sum(Diffused_25))
#'
#' #### Now perform the Chi square independent tests with an alpha of 0.05.
#'
#' Ho: There is no association between variables (they are independent)./ The way we build the disturbance variable is independent from the result
#' 
#' Ha: There is an association between variables
#'   
print(x_cond_diffC<-cont_tableC_48US[,c(2,3,4)] %>% chisq.test()) #pick columns 2,3 to make it a contingency table
#'
#' As p-value <0.05, we reject Ho, therefore we conclude there is an association between the variables. In other words, the way we calculate the disturbance variable makes a difference 
#' 
#' #### Test for each state individually condition disturbance vs diffused disturbance complete
#'
#' A) Condition disturbance vs diffused disturbance 0% threshold
#'
test_M_A<-cont_tableA%>%
  pivot_longer(Condition_table:Diffused_complete,names_to="Variable") #Convert the table to a long format

test_M_A <- test_M_A %>%
  group_by(State) %>% #group by state as we want a chisq test for every state
  nest() %>%
  mutate(M = map(data, function(dat){ #convert the dataset to a nested matrix
    dat2 <- dat %>% spread(Disturbance, value)
    M <- as.matrix(dat2[, -1])
    row.names(M) <- dat2$Variable
    return(M)
  }))

#'
test_M_A$M[[1]] #test the matrix with the first level
#'
testA <- test_M_A %>% #run the chi square for every level of the matrix
  mutate(pvalue = map_dbl(M, ~chisq.test(.x)$p.value)) %>%
  select(-data, -M) %>%
  ungroup()
#'
#write.csv(testA,'data//_disturbances_chi_test_A.CSV')
#'
#' B) Condition disturbance vs diffused disturbance 25% threshold
#'
test_M_B<-cont_tableB%>%
  pivot_longer(Condition_table:Diffused_25,names_to="Variable")

test_M_B <- test_M_B %>%
  group_by(State) %>%
  nest() %>%
  mutate(M = map(data, function(dat){
    dat2 <- dat %>% spread(Disturbance, value)
    M <- as.matrix(dat2[, -1])
    row.names(M) <- dat2$Variable
    return(M)
  }))

#'
test_M_B$M[[1]]
#'
testB <- test_M_B %>%
  mutate(pvalue = map_dbl(M, ~chisq.test(.x)$p.value)) %>%
  select(-data, -M) %>%
  ungroup()
#'
#write.csv(testB,'data//_disturbances_chi_test_B.CSV')
#'
#' C) Condition disturbance, diffused disturbance 0% threshold, and diffused disturbance 25% threshold
#'
test_M_C<-cont_tableC%>%
  pivot_longer(Condition_table:Diffused_25,names_to="Variable")

test_M_C <- test_M_C %>%
  group_by(State) %>%
  nest() %>%
  mutate(M = map(data, function(dat){
    dat2 <- dat %>% spread(Disturbance, value)
    M <- as.matrix(dat2[, -1])
    row.names(M) <- dat2$Variable
    return(M)
  }))

#'
test_M_C$M[[1]]
#'
testC <- test_M_C %>%
  mutate(pvalue = map_dbl(M, ~chisq.test(.x)$p.value)) %>%
  select(-data, -M) %>%
  ungroup()
#'
#write.csv(testC,'data//_disturbances_chi_test_C.CSV')
#'  
#'    
#' ####################################################################################
# Maps ----
#' ####################################################################################
#'
#'
#'
#' ####################
#'         
#'
#' Spun using:
#' 
#'   ezspin("code/Disturbance_variables_test.R", out_dir = "output", keep_md=FALSE) 

