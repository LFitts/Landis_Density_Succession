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

# Read in the FIA data for the state of Wisconsin ----
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
# Landis disturbance variable ----
#'
#' ## 1. LANDIS Create a disturbance variable per subplot
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
#' 
###################################################################################
#' # DISTURBANCE VARIABLE COMPARISON
#' 
#' Tables created here are just for comparing the two disturbances variables. To differentiate the variables, we will call "diffused disturbance" to the disturbance variable created from the tree table. The original disturbance variable from the condition table will be called "condition disturbance". This variable has a 25% threshold, meaning that if the condition in the subplot has >=25% of the trees affected by a disturbance and more than 1 acre in size, it will be recorded as such, otherwise, it won't show as a disturbance
#'
#' Starting dataset: WI_TCP 
#' 
WI_TCP <- WI_TCP%>% filter( DIA >= 5) #filter out trees that correspond to microplot
#'
WI_TCP[is.na(WI_TCP)] <- 0 #fill NA's with zeros
#'
#' Create an ID
#' 
WI_TCP$ID<- paste(WI_TCP$STATECD, WI_TCP$COUNTYCD, WI_TCP$PLOT, WI_TCP$INVYR, sep="_")
#'
nrow(unique(WI_TCP))
#'
###################################################################################
# Condition disturbance table ----
#'
#' ## Now, let's compare is this new variable works better at capturing disturbances 
#' For doing this, we will compare the original 'condition-level disturbance variable' (COND_DIST) with the 'tree-level disturbance variable' (TREE_DIST) through a X^2 test.
#' There will be three categories in each variable: not disturbed (ND), simple disturbance (SD), and compound disturbance (CD)
#' 
WI_TCP2 <-WI_TCP
#' 
#' ### 2. Prepare the condition table
#'  
#'  First let's scale up to a plot level
#' 
#' Convert to long format
#' 
plot_WI_CP <- WI_TCP2 %>% pivot_longer(DSTRBCD1:DSTRBCD3,names_to="DSTRBCD" , values_to="DIST")
#'
#' Get unique observations for the dataset
#' 
plot_WI_CP <- plot_WI_CP %>% select(STATECD, COUNTYCD, PLOT, INVYR, CONDID, ID, DSTRBCD, DIST) #select variables of interest
#'
plot_WI_CP<- unique(plot_WI_CP) #unique observations for the dataset   
#'
#' Create a new column with numbers 1 for presence of a disturbance and 0 for absence of a disturbance
#' 
plot_WI_CP$dist_count <- ifelse(plot_WI_CP$DIST != 0, 1,0) 
#' 
#' Scaling to a plot level  
plot_WI_CP<- plot_WI_CP %>% group_by(STATECD, COUNTYCD, PLOT, INVYR, DIST ) %>% 
  summarise (disturbance = sum(dist_count, na.rm=T)) #it is adding up the presence of the same disturbances from all the conditions in a plot. We might have a disturbance spreading on more than 1 condition
#'
#' Keep single disturbance records per plot
#' 
plot_WI_CP$disturbance<- ifelse(plot_WI_CP$disturbance != 0,1,0)#because we just want the disturbances in each plot and are not interested in repeated disturbances in a plot (if condition 1 and 2 had fire for example, we want to record it just once for the plot)
#'                  
#' Now we can scale up to a plot level:
#' 
plot_WI_CP<- plot_WI_CP %>% group_by(STATECD, COUNTYCD, PLOT, INVYR) %>% 
  summarise (disturbance_sum = sum(disturbance, na.rm=T)) 
#'
#' The logic here is that plots that were affected by more than one disturbance type (CD) will be represented with value >1 for the sum of disturbances (We already took care of repeated disturbance types per plot in the previous step). A value of 1 represents SD and a value of 0 represents ND
#' 
#' Now standardize the names ND=0, SD=1, CD >=2
#' 
plot_WI_CP$disturbance <- ifelse(plot_WI_CP$disturbance_sum == 0, "ND",
                                 ifelse(plot_WI_CP$disturbance_sum == 1 , "SD", "CD"))  
#' 
#' 
cont_table_COND<- plot_WI_CP %>% group_by(disturbance) %>%
  summarise(n_plots=n()) #get the number of plots per grouped category/disturbance type (SD, CD, ND)
#'
cont_table_COND
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
WI_TCP3 <-WI_TCP
#'
#' #### 3.2 Use the group_by and summarise functions to create a table containing disturbances per diameter class in each condition
#' 
DIST_SUBP_CONDID<- WI_TCP3 %>% group_by(STATECD, COUNTYCD, PLOT, INVYR, CONDID, ID,AGENTCD)%>% #Agent of mortality will be our disturbance variable
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
#' Look at our output
#' 
head(DIST_SUBP_CONDID, 10)
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
head(disturbances_tree,15)
#'
length(unique(disturbances_tree$ID)) #total number of plots 320, matches the plot_WI_CP
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
cont_table_TREE
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
cont_table_TREE25
#'
#' ####################################################################################
# Visualization ----
#' ####################################################################################
#' 
#' ## 6. Represent proportion of conditions disturbed per disturbance type
#'
#'
#' ### Violin plots
#' 
p.vio <- ggplot(disturbances_tree, aes(x= as.factor(DIST_TYPE), y=proportion)) +
  geom_violin()
#' 
p.vio 
#'  
#' ### Boxplots
#'
p.box <- ggplot(disturbances_tree, aes(x= as.factor(DIST_TYPE), y=proportion)) +
  geom_boxplot()
#'
p.box
#'
#' #MAKE THE GRAPHS PUBLICATION READY
#'
#' ####################################################################################
# Chi square tests ----
#' ####################################################################################
#' 
#' ## 7. Perform tests of independence
#' 
#' ### 7.1 Condition disturbance vs diffused disturbance 0% threshold
#' 
#' Database from condition disturbance: cont_table_COND
#' 
#' Database from complete diffused disturbance (0% threshold/considering all proportions): cont_table_TREE 
#' 
#' #### 7.1.1 Merge both tables
#' 
cont_table1<- merge(cont_table_COND,cont_table_TREE, by="disturbance")
#'
#' Rename the columns
names(cont_table1)<-c("Disturbance", "Condition_table", "Diffused_complete")
#'
cont_table1
#'
#' #### 7.1.2 Now perform the Chi square independent tests with an alpha of 0.05.
#'
#' Ho: There is no association between variables (they are independent)./ The way we build the disturbance variable is independent from the result
#' 
#' Ha: There is an association between variables
#'   
print(x_cond_diff<-cont_table1[,c(2,3)] %>% chisq.test()) #pick columns 2,3 to make it a contingency table
x_cond_diff$expected #display the expected values
#'
#' As p-value <0.05, we reject Ho, therefore we conclude there is an association between the variables. In other words, the way we calculate the disturbance variable makes a difference 
#' 
#' ### 7.2 Condition disturbance vs diffused disturbance 25% threshold
#' 
#' Database from condition disturbance: cont_table_COND
#' 
#' Database from diffused disturbance with 25% threshold (considering proportions >=25): cont_table_TREE25 
#' 
#' #### 7.2.1 Merge both tables
#' 
cont_table2<- merge(cont_table_COND,cont_table_TREE25, by="disturbance")
#'
#' Rename the columns
names(cont_table2)<-c("Disturbance", "Condition_table", "Diffused_25")
#'
cont_table2
#'
#' #### 7.2.2 Now perform the Chi square independent tests with an alpha of 0.05.
#'
#' Ho: There is no association between variables (they are independent)./ The way we build the disturbance variable is independent from the result
#' 
#' Ha: There is an association between variables
#'   
print(x_cond_diff25<-cont_table2[,c(2,3)] %>% chisq.test()) #pick columns 2,3 to make it a contingency table
x_cond_diff25$expected #display the expected values
#'
#' As p-value >0.05, we fail to reject Ho, therefore we conclude that when considering a 25% threshold, there is no association between the variables. In other words, the way we calculate the disturbance variable does not make a difference 
#' 
#'
#' ####################
#'         
#'
#' Spun using:
#' 
#'   ezspin("code/Disturbance_variables_test.R", out_dir = "output", keep_md=FALSE) 


