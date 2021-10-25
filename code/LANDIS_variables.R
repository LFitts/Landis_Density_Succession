# Preparing the dataset for Wisconsin ----
###################################################################################
#' ### Installing and loading the libraries needed
#'
#install.packages("tidyverse")
#install.packages("knitr")
#install.packages("ezknit")
library(tidyverse)
library(knitr)
library(ezknitr)

###################################################################################

# Read in the FIA data for the state of Wisconsin ----
#' 
#' Read in the data for Wisconsin
#' 
WI_COND<-fread("data/main_WI_2020/WI_COND.csv", na.strings = "NA")#read the condition table
WI_PLOT<-fread("data/main_WI_2020/WI_PLOT.csv", na.strings = "NA")#read the plot table
WI_TREE<-fread("data/main_WI_2020/WI_TREE.csv", na.strings = "NA")#read the tree table
WI_SUBPLOT<-fread("data/main_WI_2020/WI_SUBPLOT.csv", na.strings = "NA")
WI_SUBP_COND<-fread("data/main_WI_2020/WI_SUBP_COND.csv", na.strings = "NA")
WI_SITETREE<-fread("data/main_WI_2020/WI_SITETREE.csv", na.strings = "NA")
#'
WI_COND<-fread("H:/FIA_Wisconsin/Landis_Density_Succession/data/main_WI_2020/WI_COND.csv", na.strings = "NA")#read the condition table
WI_PLOT<-fread("H:/FIA_Wisconsin/Landis_Density_Succession/data/main_WI_2020/WI_PLOT.csv", na.strings = "NA")#read the plot table
WI_TREE<-fread("H:/FIA_Wisconsin/Landis_Density_Succession/data/main_WI_2020/WI_TREE.csv", na.strings = "NA")#read the tree table
#'
#'
#' Just keep records from 2000 on. This is when the annual inventory for WI started
#'
WI_COND<-subset(WI_COND, INVYR >= 2000)
WI_PLOT<-subset(WI_PLOT, INVYR >= 2000)
WI_TREE<-subset(WI_TREE, INVYR >= 2000)
WI_SUBPLOT<-subset(WI_SUBPLOT, INVYR >= 2000)
WI_SUBP_COND<-subset(WI_SUBP_COND, INVYR >= 2000)
WI_SITETREE<-subset(WI_SITETREE, INVYR >= 2000)
#'
#'
#' Keep each identifier record different before combining tables
#'
colnames(WI_PLOT)[1]<-"PLT_CN"
colnames(WI_COND)[1]<-"COND_CN"
colnames(WI_TREE)[1]<-"TREE_CN"
colnames(WI_SUBPLOT)[1]<-"SUBPLOT_CN"
colnames(WI_SUBP_COND)[1]<-"SUBP_COND_CN"
colnames(WI_SITETREE)[1]<-"SITETREE_CN"
#'
#' Select variables of interest for each table
#' 
WI_COND <- select(WI_COND, PLT_CN, INVYR, STATECD, COUNTYCD, PLOT,COND_STATUS_CD, CONDID, DSTRBCD1, DSTRBCD2, DSTRBCD3)
WI_PLOT <- select(WI_PLOT, PLT_CN, INVYR, STATECD, COUNTYCD, PLOT, ELEV, ECOSUBCD, CYCLE)
WI_TREE <- select(WI_TREE, TREE_CN,PLT_CN, INVYR, STATECD, COUNTYCD, PLOT, SUBP,CONDID, TREE, STATUSCD, SPCD, SPGRPCD, DIA, DIAHTCD, HT, ACTUALHT, AGENTCD, DAMAGE_AGENT_CD1, DAMAGE_AGENT_CD2, DAMAGE_AGENT_CD3, MORTYR, STANDING_DEAD_CD) #, TRTCD1, TRTCD2, TRTCD3) #,TPA_UNADJ, DRYBIO_BOLE, DRYBIO_TOP, DRYBIO_STUMP, DRYBIO_SAPLING, DRYBIO_WDLD_SPP, DRYBIO_BG, DRYBIO_AG, CARBON_AG, CARBON_BG)
WI_SUBPLOT<-select(WI_SUBPLOT, SUBPLOT_CN, PLT_CN, STATECD, COUNTYCD, PLOT, SUBP, INVYR, SUBPCOND, CONDLIST)
WI_SITETREE<-select(WI_SITETREE, SITETREE_CN, PLT_CN, INVYR, SUBP, STATECD, COUNTYCD, PLOT, CONDID, TREE, SPCD, DIA, HT, AGEDIA, SITREE, SIBASE)
#' 
#' ##################################################################################
# 1. Homogenize time periods: ----
#'###################################################################################
#' 
#' Here we will put the data set in a wide format so that the remeasurements are 
#' 
#' 1) PLOT LEVEL
#' 
#' Work with the plot table first
#' 
#'Let's select the identifier columns of our database to work just with those
#'
#WI_LOOP_LD<- select(WI_PLOT, STATECD, COUNTYCD, PLOT, INVYR) #select the identifier columns that we will use for the loop
#WI_LOOP_LD<- unique(WI_LOOP_LD) #get unique values for those identifiers
#'
#' Now let's work the loop:
#' 
#'  Make remeasurements uniform by standardizing the years into time 0, 1, 2 and so on
#'
#' The idea is to create a loop that will give a number for period 1,2,3 each time it finds a plot occurring (time 0 and all the remeasurements)
#'
#'  First sort database by year, so we're sure earlier measurements of the same plot don't occur after more recent ones 
#'
#WI_LOOP_LD<-WI_LOOP_LD[order(WI_LOOP_LD$INVYR),]
#'Now create a new column KEY holding the contents of the first three columns (STATECD, COUNTYCD and PLOT) so that the loop can look for every combination of state, county and plot and assign an increasing number to each time it finds that combination
#WI_LOOP_LD$KEY<-paste(WI_LOOP_LD$STATECD, WI_LOOP_LD$COUNTYCD, WI_LOOP_LD$PLOT, sep="_")
#'As the new column KEY has repeated occurrences, let's create a vector: vec that will hold the unique items of KEY (this will be used inside the loop to find unique occurences)
#vec<-unique(WI_LOOP_LD$KEY)
#'
#'Create the loop
#'
#' Create a new column PLOT_NUM: it will hold the RANK of the measurement taken for each (state,county,plot) combination (aka each key)
#' (i.e., a key that has three measurements will have three occurrences/rows in the database, with column PLOT_NUM holding values 0, 1, and 2 successively)
#'
#' Remove comment symbol when running from scratch
#WI_LOOP_LD$TIME_PER<-0
#for(i in 1:length(vec)){
#  tempkey=vec[i]
#  counter=0
#  for(j in 1:nrow(WI_LOOP_LD)){
#    if(WI_LOOP_LD$KEY[j]==tempkey){
#      WI_LOOP_LD$TIME_PER[j]=counter
#      counter=counter+1}}}
#'
#'
#write.csv(WI_LOOP_LD,'LANDIS_work/data/R_created/WI_LOOP_TIMEP.CSV') #next time we run this code we won't create the loop again, just read the csv created
#'
#' Read in the loop just created for uniformed time periods
#'
WI_LOOP_LD<-read.csv("LANDIS_work/data/R_created/WI_LOOP_TIMEP.CSV")
#WI_LOOP_LD<-WI_LOOP_LD[,-1] #Remove the auto-created extra column (x)
#'
#' Set time periods in a wide format
#' 
WI_LOOP_LD$TIME_PER=ifelse(WI_LOOP_LD$TIME_PER == 0, "t0",
                           ifelse(WI_LOOP_LD$TIME_PER == 1, "t1",
                                  ifelse(WI_LOOP_LD$TIME_PER == 2, "t2","t3")))
#'    
#' 
time_wide <- WI_LOOP_LD %>%
  pivot_wider(id_cols=c(STATECD, COUNTYCD, PLOT, KEY), names_from = TIME_PER, values_from = c("INVYR"))
#' 
#'  Fill NA with zeros
#'  
time_wide$t0[is.na(time_wide$t0)] <- 0
time_wide$t1[is.na(time_wide$t1)] <- 0
time_wide$t2[is.na(time_wide$t2)] <- 0
time_wide$t3[is.na(time_wide$t3)] <- 0
#'  
#'  
time_wide$N_REM<-ifelse(time_wide$t0 != 0 & time_wide$t1 == 0 & time_wide$t2 == 0 & time_wide$t3 == 0, 1,
                        ifelse(time_wide$t0 != 0 & time_wide$t1 != 0 & time_wide$t2 == 0 & time_wide$t3 == 0,2,
                               ifelse( time_wide$t0 != 0 & time_wide$t1 != 0 & time_wide$t2 != 0 & time_wide$t3 == 0,3,
                                       ifelse(time_wide$t0 != 0 & time_wide$t1 != 0 & time_wide$t2 != 0 & time_wide$t3 != 0, 4, NA)))) 
#'
#'
#' Read in the plot list that will be used in LANDIS (F and M)
#' 
WI_PLOT_LIST<-read.csv("LANDIS_work/data/R_created/WI_PLOT_LIST.CSV")
LIST_KEY<-as.data.frame(WI_PLOT_LIST[,c(6,7,9)]) #JUST KEEP THE KEY COLUMN
#'
#' Merge with the time_wide data set
#'
time_wide<-merge(time_wide, LIST_KEY)
#'
n_rem<-time_wide%>% group_by(N_REM)%>%
  summarize(n=n())
#'
#' Final list of plots (excluding the ones that only have 1 or 2 measurements)
#'
WI_PLOT_LIST<-time_wide%>% filter(N_REM>=3)
#write.csv(WI_PLOT_LIST,'LANDIS_work/data/R_created/WI_PLOT_LIST_updated.CSV')
#'
#' ##################################################################################
# 2. Looking into the number of conditions in a plot/subplot (just for general information) ----
#'###################################################################################
#'
#' Merge the time-period table (WI_LOOP_TIMEPER) with the subplot table
#' 
WI_SUBPLOT <- merge(WI_LOOP_LD, WI_SUBPLOT, by=c("STATECD", "COUNTYCD", "PLOT", "INVYR"))
#'
#' Recategorize plots into F,NF,M,W,NS
#' 
#' Create a new data set recategorizing the plots with conditions of forest (F), non-forest (NF), mixed (M), water (W), and non-sampled (NS)
#' 
NEW_COND<-WI_COND %>% select (STATECD, COUNTYCD,PLOT, INVYR, COND_STATUS_CD) #select variables of interest
NEW_COND<-unique(NEW_COND) #create a unique data set for each combination of state, county, plot, inventory year and condition status
#'
#' Put conditions in a wide format
NEW_COND<-NEW_COND%>% pivot_wider(names_from=COND_STATUS_CD, values_from=COND_STATUS_CD)
#'
#' Rename columns
#' 
names(NEW_COND) <- c("STATECD","COUNTYCD","PLOT","INVYR", "CONDITION1", "CONDITION2", "CONDITION5", "CONDITION4", "CONDITION3")
#'
#' Now let's recategorize the conditions
#'
NEW_COND$CONDITION1[is.na(NEW_COND$CONDITION1)] <- 0 # First filling the missing values with zeros
NEW_COND$CONDITION2[is.na(NEW_COND$CONDITION2)] <- 0 # First filling the missing values with zeros
NEW_COND$CONDITION3[is.na(NEW_COND$CONDITION3)] <- 0 # First filling the missing values with zeros
NEW_COND$CONDITION4[is.na(NEW_COND$CONDITION4)] <- 0 # First filling the missing values with zeros
NEW_COND$CONDITION5[is.na(NEW_COND$CONDITION5)] <- 0 # First filling the missing values with zeros
#'
#' Creating an ifelse statement for recategorizing conditions (F: forest, NF: Non-forest, M:Mixed, NS:Not sampled with posibility of forest land)
NEW_COND$CONDITION=ifelse(NEW_COND$CONDITION1 != 0 & NEW_COND$CONDITION2 == 0 & NEW_COND$CONDITION3 == 0 & NEW_COND$CONDITION4 == 0 & NEW_COND$CONDITION5 == 0, "F",
                          ifelse(NEW_COND$CONDITION2 != 0 & NEW_COND$CONDITION1 == 0 & NEW_COND$CONDITION3 == 0 & NEW_COND$CONDITION4 == 0 & NEW_COND$CONDITION5 == 0 ,"NF",
                                 ifelse(NEW_COND$CONDITION3 != 0 | NEW_COND$CONDITION4 != 0 & NEW_COND$CONDITION1 == 0 & NEW_COND$CONDITION2 == 0 & NEW_COND$CONDITION5 == 0 ,"W",
                                        ifelse(NEW_COND$CONDITION5 != 0 & NEW_COND$CONDITION1 == 0 & NEW_COND$CONDITION2 == 0 & NEW_COND$CONDITION3 == 0 & NEW_COND$CONDITION4 == 0, "NS" ,"M"))))
#'
#' Keep only the condition F/M/NF/W/NS column
#' 
CONDITION_PLOT<-NEW_COND%>% select(STATECD, COUNTYCD, PLOT, INVYR, CONDITION)
#'
#' Merge with the WI_LOOP_LD data set to obtain the homogenized time periods
#'
CONDITION_PLOT <- merge(CONDITION_PLOT, WI_LOOP_LD, by=c("STATECD", "COUNTYCD", "PLOT", "INVYR"))
#'
#' Keep subplots for the first remeasurement
#' 
WI_PLOT_TIME0<-CONDITION_PLOT%>% filter(TIME_PER=="t0") #this is going to be the initial pool of plots for time 0
#'
#' Let's see how many are forests vs non-forest at time zero (number of plots and percentage)
#'
WI_COND_0<-WI_PLOT_TIME0%>% group_by(CONDITION)%>%
  summarize(number_plots=n(),
            percentage=(n()*100)/length(WI_PLOT_TIME0$CONDITION))
#'
#'######################
#'#For the subplot level, not so useful given the scale at which the cond status is given at
#' 
#' 
#' Merge with subplot table
#' 
#WI_SUBPLOT<- merge(WI_SUBPLOT, CONDITION_PLOT, by=c("STATECD", "COUNTYCD", "PLOT", "INVYR"))
#'
#' Keep subplots for the first remeasurement
#' 
#WI_SUBP_TIME0<-WI_SUBPLOT%>% filter(TIME_PER=="t0")
#'
#' Let's see how many are forests vs non-forest at time zero (number of plots and percentage)
#'
#WI_COND_0<-WI_SUBP_TIME0%>% group_by(CONDITION)%>%
#  summarize(number_plots=n(),
#            percentage=(n()*100)/78391)
#'  
#' ########################################################
#' Separating the list of conditions in each subplot
#'
#WI_SUBP_TIME0$CONDLIST <-as.character(WI_SUBP_TIME0$CONDLIST)# convert the numeric values to text values
#WI_SUBP_TIME0$COND1<-substr(WI_SUBP_TIME0$CONDLIST,1,1) #create a column that will just keep the 1st character in the string. Will do the same for characters 2,3, and 4, each in one independent column
#WI_SUBP_TIME0$COND2<-substr(WI_SUBP_TIME0$CONDLIST,2,2)
#WI_SUBP_TIME0$COND3<-substr(WI_SUBP_TIME0$CONDLIST,3,3)
#WI_SUBP_TIME0$COND4<-substr(WI_SUBP_TIME0$CONDLIST,4,4)
#'
#WI_SUBP_TIME0$CONDITION_TYPE<-ifelse(WI_SUBP_TIME0$COND1 != 0 & WI_SUBP_TIME0$COND2 == 0 & WI_SUBP_TIME0$COND3 ==0 & WI_SUBP_TIME0$COND4 == 0, "ONE",
#                                  ifelse(WI_SUBP_TIME0$COND2 != 0 & WI_SUBP_TIME0$COND3 ==0 & WI_SUBP_TIME0$COND4 == 0, "TWO",
#                                         ifelse(WI_SUBP_TIME0$COND3 !=0 & WI_SUBP_TIME0$COND4 == 0, "THREE",
#                                                ifelse(WI_SUBP_TIME0$COND4 != 0, "FOUR", "NA"))))
#'
#' 
#' Now estimate the proportion of plots with a one, two, three, and four conditions
#' 
#COND_SUMMARY<- WI_SUBP_TIME0%>% group_by (CONDITION_TYPE) %>%
#  summarize(n=n(),
#            percentage=(n()*100)/length(WI_SUBP_TIME0$SUBP))
#'
#'
#' Now check the dominance of a single condition on a plot
#' 
#DOM<-WI_SUBP_COND %>% group_by(STATECD, COUNTYCD, PLOT, SUBP, INVYR) %>% 
#  summarize(n=n())
#'
# This confirms that the subplot condition table contains all the conditions present in the subplot
#'
#' Now let's reclassify the proportions into intervals (0-0.25; 0.25-0.50; 0.50-0.75; 0.75-1)
#'
#WI_SUBP_COND$PROP_CLASS<- ifelse(WI_SUBP_COND$SUBPCOND_PROP>=0 & WI_SUBP_COND$SUBPCOND_PROP <0.25, "[0-0.25>",
#                                 ifelse(WI_SUBP_COND$SUBPCOND_PROP>=0.25 & WI_SUBP_COND$SUBPCOND_PROP <0.5, "[0.25-0.5>",
#                                        ifelse(WI_SUBP_COND$SUBPCOND_PROP>=0.5 & WI_SUBP_COND$SUBPCOND_PROP <0.75, "[0.5-0.75>",
#                                               ifelse(WI_SUBP_COND$SUBPCOND_PROP>=0.75 & WI_SUBP_COND$SUBPCOND_PROP <= 1, "[0.75-1>",
#                                                     "NA"))))
#'
#'Now let's see which condition proportion is greater in each 
#' 
#test<- WI_SUBPLOT%>% group_by (STATECD, COUNTYCD, PLOT, INVYR, CONDITION_TYPE) %>%  #MAYBE DELETE????
#'
#' ##################################################################################
# 3. Selecting the sample of subplots for the initial landscape ----
#'###################################################################################
#'
#' Keep only forested and mixed plots at time 0
#' 
WI_PLOT_FM_TIME0<-WI_PLOT_TIME0%>% filter(CONDITION=="F" | CONDITION =="M")
#' 
#' Create a column with a random sequence of subplots per plot so we can select our sample for model run, calibration, and validation
#'
#'Create the loop
#'
#' Remove comment symbol when running from scratch
#WI_PLOT_FM_TIME0$subplot_list<-0
#for(i in 1:nrow(WI_PLOT_FM_TIME0)){
#  WI_PLOT_FM_TIME0$subplot_list[i]=paste(sample(1:4,4, replace=F), collapse="")}
#' 
#' Since this is a probabilistic sample, each time the loop is run it will output a different set of plots
#'
#write.csv(WI_PLOT_FM_TIME0,'LANDIS_work/data/R_created/WI_PLOT_LIST.CSV') #next time we run this code we won't create the loop again, just read the csv created
#'
#' Read in the loop just created for uniformed time periods
#'
WI_PLOT_LIST<-read.csv("LANDIS_work/data/R_created/WI_PLOT_LIST.CSV")
WI_PLOT_LIST<-WI_PLOT_LIST[,-1] #Remove the auto-created extra column (x)
#'
#' Separate the list of plots into different columns so we have different subplots list for different uses
WI_PLOT_LIST$subplot_list <-as.character(WI_PLOT_LIST$subplot_list)# convert the numeric values to text values
WI_PLOT_LIST$s1<-substr(WI_PLOT_LIST$subplot_list,1,1) #create a column that will just keep the 1st character in the string. Will do the same for characters 2,3, and 4, each in one independent column
WI_PLOT_LIST$s2<-substr(WI_PLOT_LIST$subplot_list,2,2)
WI_PLOT_LIST$s3<-substr(WI_PLOT_LIST$subplot_list,3,3)
WI_PLOT_LIST$s4<-substr(WI_PLOT_LIST$subplot_list,4,4)
#'
#' Now create a key for each and a new dataframe containing that information
#' 
WI_PLOT_LIST$s1_key<-paste(WI_PLOT_LIST$STATECD, WI_PLOT_LIST$COUNTYCD, WI_PLOT_LIST$PLOT, WI_PLOT_LIST$s1, sep="_")
#'
WI_PLOT_LIST$s2_key<-paste(WI_PLOT_LIST$STATECD, WI_PLOT_LIST$COUNTYCD, WI_PLOT_LIST$PLOT, WI_PLOT_LIST$s2, sep="_")
#'
WI_PLOT_LIST$s3_key<-paste(WI_PLOT_LIST$STATECD, WI_PLOT_LIST$COUNTYCD, WI_PLOT_LIST$PLOT, WI_PLOT_LIST$s3, sep="_")
#'
WI_PLOT_LIST$s4_key<-paste(WI_PLOT_LIST$STATECD, WI_PLOT_LIST$COUNTYCD, WI_PLOT_LIST$PLOT, WI_PLOT_LIST$s4, sep="_")
#'
#' Get independent data frames that will be merged with the tree table to have the data base for each purpose
s1<-as.data.frame(WI_PLOT_LIST$s1_key)
names(s1)= "KEY"
#'
s2<-as.data.frame(WI_PLOT_LIST$s2_key)
names(s2)= "KEY"
#'
s3<-as.data.frame(WI_PLOT_LIST$s3_key)
names(s3)= "KEY"
#'
s4<-as.data.frame(WI_PLOT_LIST$s4_key)
names(s4)= "KEY"
#'
#' Prepare the key column in the tree table
#' 
WI_TREE2<-WI_TREE
WI_TREE2$KEY<- paste(WI_TREE2$STATECD, WI_TREE2$COUNTYCD, WI_TREE2$PLOT, WI_TREE2$SUBP, sep="_")
#'
#' Now merge with the tree table
#'
s1_run<-merge(s1, WI_TREE2, by="KEY")
#'
s2_run<-merge(s2, WI_TREE2, by="KEY")
#'
s3_run<-merge(s3, WI_TREE2, by="KEY")
#'
s4_run<-merge(s4, WI_TREE2, by="KEY")
#'
#' Save the CSV files
#' 
#write.csv(s1_run,'LANDIS_work/data/R_created/s1_run.CSV') #next time we run this code we won't create the loop again, just read the csv created
#write.csv(s2_run,'LANDIS_work/data/R_created/s2_run.CSV') #next time we run this code we won't create the loop again, just read the csv created
#write.csv(s3_run,'LANDIS_work/data/R_created/s3_run.CSV') #next time we run this code we won't create the loop again, just read the csv created
#write.csv(s4_run,'LANDIS_work/data/R_created/s4_run.CSV') #next time we run this code we won't create the loop again, just read the csv created
#'
#' Read in the files just created
#'
s1_run<-read.csv("LANDIS_work/data/R_created/s1_run.CSV")
s1_run<-s1_run[,-1] #Remove the auto-created extra column (x) 
#'
s2_run<-read.csv("LANDIS_work/data/R_created/s2_run.CSV")
s2_run<-s2_run[,-1] #Remove the auto-created extra column (x)
#'
s3_run<-read.csv("LANDIS_work/data/R_created/s3_run.CSV")
s3_run<-s3_run[,-1] #Remove the auto-created extra column (x)
#'
s4_run<-read.csv("LANDIS_work/data/R_created/s4_run.CSV")
s4_run<-s4_run[,-1] #Remove the auto-created extra column (x)
#'
#' ##################################################################################
# 4. Create the Scenario file (ready)----
#'###################################################################################
#' 
#' Example code:
#'
#' To write a table-like file
#write.table(WI_TREE1, file = "WI_TREE1.txt", sep = "\t",
#            row.names = T)
#'
#' To write lines on a text file
#fileConn <- file("example2.txt")
#writeLines(c(paste("TutorialsPoint", "E-learning"),"2006", "Video Courses", "Tutorials", "Books"), fileConn)
#close(fileConn)
#file.show("example.txt")
#'
# Write the scenario file:
fileConn <- file("LANDIS_work/all_txt/scenario.txt")
writeLines(c(paste("LandisData", "Scenario", sep="\t"), 
             "\n",
             paste("Duration", "20", sep="\t"), 
             paste("Species", "species.txt", sep="\t"),
             "\n",
             paste("Ecoregions", "ecoregions.txt", sep="\t"), 
             paste("EcoregionsMap", "ecoregion_test.img", sep="\t"),
             "\n",
             paste("CellLength", "12.97", sep="\t"),
             "\n",
             "\n",
             paste(">> Succession Extension", "Initialization File", sep="\t"),
             paste(">> --------------------", "-------------------", sep="\t"),
             paste("\"Density-Succession\"", "densitysuccession.txt", sep="\t"),
             "\n",
             "\n",
             paste(">> Disturbance Extensions", "Initialization File", sep="\t"),
             paste(">> --------------------", "-------------------", sep="\t"),
             paste(">>   \"Land Use Change\"",		"\"land-use.txt\"", sep="\t"),
             "\n",
             paste(">> Output Extensions", "Initialization File", sep="\t"),
             paste(">> --------------------", "-------------------", sep="\t"),
             paste(" \"Density Output\"", "output_Density.txt", sep="\t"),
             "\n",
             "RandomNumberSeed  4357"),fileConn)
close(fileConn)   
# file.show("scenario_test.txt")
#'
#'
#' ##################################################################################
# 5. Create table: Species information (ready)----
#'################################################################################### 
#'
#' Read in the species attributes table
#'
species_attributes<-read.csv('data/species_attributes.CSV')
#'
#' Create a "name" column with the format Landis uses (4 letters of the genus+4letters of the species)
species_attributes<-tidyr::separate(species_attributes, Scientific_name, c("genus", "species"), "\\s(?:.+\\s)?") #separate the string for scientific name into character vectors divided by the space
#'
#' convert the genus to lower letter
#' 
species_attributes$genus<-tolower(species_attributes$genus)
#'
#' Create temporary columns with parts
species_attributes<-species_attributes %>% 
  mutate(name1=substr(genus, start=1, stop=4))%>%
  mutate(name2=substr(species, start=1, stop=4))
#'
#' Now paste the two pieces together
#' 
species_attributes$Name<-paste(species_attributes$name1,species_attributes$name2, sep="" )
#'
#' Create a subset of this table just showing the species name and codes (this will be merged with the rest of the tables)
#' 
species_codes<-species_attributes%>% select(SPCD, Name)
#'
#' Leave the necessary columns
#' 
species_attributes<-species_attributes %>% select(Name, Longevity, Sexual_maturity, Shade_tolerance, Fire_tolerance, SeedDispersal_Effective, SeedDispersal_Maximum, Vegetative_reprod_prob, SproutAge_Min, SproutAge_Max, Post.fire_regen)
#'
#' Now for this project, we won't be allowing species to reproduce or establish. Therefore, we need to change those life history attributes to zero
#'
species_attributes<-species_attributes %>% 
  mutate(SeedDispersal_Effective=0)%>%
  mutate(SeedDispersal_Maximum=0)%>%
  mutate(Vegetative_reprod_prob=0)%>%
  mutate(SproutAge_Min=0)%>%
  mutate(SproutAge_Max=0)%>%
  mutate(Post.fire_regen="none")
#'
#' Write a text file
#' 
#' Modify the Name of the first column to include the >> symbol for LANDIS
#' 
names(species_attributes)[1]<-">> Name"
#'
#' Write the species file
writeLines(c(paste("LandisData", "Species", sep="\t"),"\n"), con = "all_txt/species.txt") #creates the independent lines of text
write.table(species_attributes, "all_txt/species.txt", row.names=F, append=TRUE, quote = FALSE) #rownames=F to prevent the indexing column to be created. quote=F prevents quotes surrounding character strings
#' 
#' ##################################################################################
# 6. Create table: ecoregion (ready) ---- 
#'###################################################################################
#' 
#' Obtain the ecological provinces from the plot table
#' 
#' Create a table listing all the ecological sections in WI and their description:
#' 
province<-read.csv('LANDIS_work/data/excel_created/ecological_provinces.CSV') #read in the list of ecological provinces from the FIA list and their descriptions from https://www.fs.fed.us/research/publications/misc/73326-wo-gtr-76d-cleland2007.pdf
#'
#' Now create a table for ecological provinces in the subplot 
#' ECO_PROVINCES:
#'
WI_PLOT$ECO_PROVINCE<-substr(WI_PLOT$ECOSUBCD, start=2, stop=5) #Leave only the strings that correspond to the ecological province (from 2 to 4). Note that there is a blanc space at the beginning of the ECOSUBCD column from the FIA database
#' 
WI_ECO <- WI_PLOT  %>% group_by(ECO_PROVINCE) %>% #summarize by ecological province
  summarize(n = n())
#'
#' Now transform the WI_ECO data frame into a format that can go into the ecoregion txt file
#'
WI_ECO<-WI_ECO %>% 
  mutate(Active= "yes")%>%
  mutate(Map_code=seq(1:nrow(WI_ECO)))
#'
WI_ECO<-as.data.frame(WI_ECO)
#'
#' Now merge the province file containing the descriptions with the WI_ECO table 
#'
WI_ECO<-merge(WI_ECO,province, by="ECO_PROVINCE", all.x=T)
#'
#' Rename the ECO_PROVINCE column to match LANDIS txt contents
#' 
names(WI_ECO)[1]<-"Name"
names(WI_ECO)[5]<-"Description"
#'
#' Now leave only the columns desired:
#'
WI_ECO<-WI_ECO%>% select(Active, Map_code, Name, Description)
#'
#' Modify the Name of the first column to include the >> symbol for LANDIS
#' 
names(WI_ECO)[1]<-">> Active"
#'
#' Now write the ecoregion text file:
#' 
writeLines(c(paste("LandisData", "Ecoregions", sep="\t"),"\n"), con = "LANDIS_work/all_txt/ecoregions.txt") #creates the independent lines of text
write.table(WI_ECO, "LANDIS_work/all_txt/ecoregions.txt", row.names=F, append=TRUE, quote = FALSE) #rownames=F to prevent the indexing column to be created. quote=F prevents quotes surrounding character strings
#' 
#'##################################
#' Take a look at the number of ecoregions available in WI (won't be used for the table creation itself but for general information)
#' 
#' Select only the variables of use
#' 
#WI_PLOT2 <- WI_PLOT %>% select(STATECD, COUNTYCD, PLOT, INVYR, ELEV, ECOSUBCD, LAT, LON)
#WI_SUBPLOT2 <- WI_SUBPLOT %>% select (STATECD, COUNTYCD, PLOT,  INVYR, SUBP, SUBP_STATUS_CD)
#' 
#'  Merge with subplot variable and with the previous loop (WI_LOOP_LD)
#'
#' Merge with WI_LOOP_LD
#' 
#WI_SUBP_ECO<- merge(WI_PLOT2, WI_LOOP_LD, by=c("STATECD", "COUNTYCD", "PLOT", "INVYR"))
#' 
#' Merge with Subplot
#'
#WI_SUBP_ECO<- merge(WI_SUBPLOT2 ,WI_SUBP_ECO, by=c("STATECD", "COUNTYCD", "PLOT", "INVYR"), all=T)
#'
#' Modify the KEY column to reflect subplot level keys
#'  
#WI_SUBP_ECO <- WI_SUBP_ECO %>% mutate (KEY = paste (KEY, SUBP, sep='_'))
#'  
#' Leave data for only time period 0 (as these are the initial conditions) #do we want to calibrate ecoregions too? if yes, don't remove other time period's data#############
#'
#WI_SUBP_ECO<- WI_SUBP_ECO %>% filter(TIME_PER == 0) 
#'
#' See how many ecoregions we are working with
#'
#ECO <- WI_SUBP_ECO %>% group_by(ECOSUBCD) %>% 
#  summarize(n = n())
#'
#' With this classification, we are dealing with 40 ecoregions.
#' 
#' How about using a broader category:
#'
#' ECO_PROVINCES:
#'
#WI_SUBP_ECO$ECO_PROVINCE<-substr(WI_SUBP_ECO$ECOSUBCD, start=1, stop=5) #Leave only the strings that correspond to the ecological province (from 1 to 4)
#' 
#ECO_P <- WI_SUBP_ECO %>% group_by(ECO_PROVINCE) %>% 
#  summarize(n = n())
#' 
#' With this classification, we are dealing with 12 ecoregions.
#' 
#' ECOREGION:
#' 
#WI_SUBP_ECO$ECOREGION<-substr(WI_SUBP_ECO$ECOSUBCD, start=1, stop=4) #Leave only the strings that correspond to the ecological province (from 1 to 3)
#'
#' 
#ECOR <- WI_SUBP_ECO%>% group_by(ECOREGION) %>% 
#  summarize(n = n())
#' 
#' 
#' Check how many subplots are in forested condition, non-forested, or non-sampled
#' 
#SUBP_STATUS<-WI_SUBP_ECO%>% group_by(SUBP_STATUS_CD) %>%
#  summarize(n=n(),
#            percentage=(n()*100)/length(WI_SUBP_ECO$SUBP))
#' 
#' At time zero, we have 32 886 subplots (41.95%) on condition 1 (forested), 
#' 43 041 subplots (54.91%) on condition 2 (non-forested), and 2 464 (3.14%) on condition 3 (non-sampled)
#' 
#' Filter SUBP_STATUS_CD 1 ???
#' 
#WI_SUBP_ECO <- WI_SUBP_ECO %>% filter(SUBP_STATUS_CD==1)
#' 
#' 
#write.csv(WI_SUBP_ECO,'LANDIS_work/data/R_created/WI_SUBP_ECO.CSV') #Table showing all the forested subplots with coordinates and ecoregion at time 0 (first remeasurement)
#'
#'##############################################################################
#'
#' ##################################################################################
# 7. Create table: establishment probability (ready) ----
#'################################################################################### 
#'
#' This portion of the code will create a txt for a combination of all the years vs ecoregions
#' 
#' For this table, we need to grab information from the previous tables and use the function union to show all the combinations of them
#' 
#' Let's start with the years 1:20 and the ecoregions
#' 
#' Year
#' 
#year<- seq(1,20,1)
#' 
#' Ecoregion
#' 
#ecoregion<- WI_ECO$Name
#' 
#' Species
#' 
#species<-species_attributes$`>> Name`
#'
#' 
#' Use the expand.grid function to create a dataframe with all the different combinations of the 3 columns:
#' 
#establishment_probability=expand.grid(x = year, y = ecoregion, z=species)
#'
#' Now add the establishment probability column
#establishment_probability$ProbEst<-0
#' 
#' Modify the names of the df
#' 
#names(establishment_probability)<-c(">> Year", "Landtype", "Species", "ProbEst")
#'
#' Now write the establishment_probability text file:
#' 
#writeLines(c(paste("LandisData", "Dynamic Input Data", sep="\t"),"\n"), con = "LANDIS_work/all_txt/establishment_probability.txt") #creates the independent lines of text
#write.table(establishment_probability, "LANDIS_work/all_txt/establishment_probability.txt", row.names=F, append=TRUE, quote = FALSE) #rownames=F to prevent the indexing column to be created. quote=F prevents quotes surrounding character strings
#'  
#'  #######################################################
#' This section of the code will only create establishment probability for year 1 since LANDIS will take those values for the rest of the years
#' 
#' For this table, we need to grab information from the previous tables and use the function union to show all the combinations of them
#' 
#' Let's start with the years 1:20 and the ecoregions
#' 
#' Year
#' 
year<- 1
#' 
#' Ecoregion
#' 
ecoregion<- WI_ECO$Name
#' 
#' Species
#' 
species<-species_attributes$`>> Name`
#'
#' 
#' Use the expand.grid function to create a dataframe with all the different combinations of the 3 columns:
#' 
establishment_probability=expand.grid(x = year, y = ecoregion, z=species)
#'
#' Now add the establishment probability column
establishment_probability$ProbEst<-0
#' 
#' Modify the names of the df
#' 
names(establishment_probability)<-c(">> Year", "Landtype", "Species", "ProbEst")
#'
#' Now write the establishment_probability text file:
#' 
writeLines(c(paste("LandisData", "Dynamic Input Data", sep="\t"),"\n"), con = "LANDIS_work/all_txt/establishment_probability.txt") #creates the independent lines of text
write.table(establishment_probability, "LANDIS_work/all_txt/establishment_probability.txt", row.names=F, append=TRUE, quote = FALSE) #rownames=F to prevent the indexing column to be created. quote=F prevents quotes surrounding character strings
#' 
#' 
#' ##################################################################################
# 8. Create table: Density succession (ready) ----
#'###################################################################################
#' 
# Write the scenario file:
fileConn <- file("LANDIS_work/all_txt/densitysuccession.txt")
writeLines(c(paste("LandisData", "Density-Succession", sep="\t"),
             "\n",
             paste("Density-Succession", "Value", sep="\t"),
             "\n",
             paste("Timestep", "1", sep="\t"),
             paste("StartYear", "2000", sep="\t"),
             paste(">> SeedingAlgorithm", "WardSeedDispersal", sep="\t"),
             paste("SeedingAlgorithm", "DensitySeeding", sep="\t"),
             "\n",
             paste("InitialCommunities", "initial-communities.txt", sep="\t"), 
             paste("InitialCommunitiesMap", "initialcommunity_test.img", sep="\t"),
             "\n",
             paste("DensitySpeciesParameters", "density_speciesparameters.txt", sep="\t"),
             "\n",
             paste("EcoregionParameters", "EcoregionParameters_density.txt", sep="\t"),
             "\n",
             paste("DynamicEcoregionFile", "DynamicEcoregionInput.txt", sep="\t"),
             "\n", 
             paste("DynamicInputFile", "establishment_probability.txt", sep="\t"),
             "\n",
             paste("DynamicInputFile", "Ecoregion_diameter_table.txt", sep="\t"),
             "\n",
             paste(">> DisturbanceReductions", "disturbance_reductions.txt", sep="\t"),
             "\n",
             paste("BiomassVariableFile",		"BioMassCoef.txt", sep="\t")),fileConn)
close(fileConn)   
#' 
#' 
#' 
#' ##################################################################################
# 9. Create table: Initial communities ----
#'###################################################################################
#' 
#' # 1. Read in the FIA data for the state of Wisconsin
#' 
#' Read in the data for Wisconsin
#' 
#WI_COND<-fread("data/main_WI_2020/WI_COND.csv")#read the condition table
WI_PLOT<-read_csv("data/main_WI_2020/WI_PLOT.csv")#read the plot table
#fiaDir <- 'D:/fia/rFIA'
#getFIA(states = "WI", dir = fiaDir, load = FALSE, nCores=3) #download the FIA tables for Wisconsin
#'
#wiTB <- readFIA(fiaDir, states = c('WI'), tables=c("TREE"), inMemory = T, nCores = 3)
#WI_TREE <- wiTB$TREE %>% filter(INVYR >= 2000)
WI_TREE<-read_csv("data/main_WI_2020/WI_TREE.csv")#read the tree table


#WI_COND <- WI_COND %>% mutate(PLT_CN = as.character(PLT_CN))
WI_TREE <- WI_TREE %>% mutate(CN = as.character(CN), PLT_CN = as.character(PLT_CN), PREV_TRE_CN = as.character(PREV_TRE_CN))
WI_PLOT <- WI_PLOT %>% mutate(CN = as.character(CN), PREV_PLT_CN = as.character(PREV_PLT_CN))


#' 
#' Recode species
#' Vectors containing generic categories
vec99991<-c(531,462)
vec99992<-c(202,57)
vec99993<-c(901,373,552)
vec99994<-c(68)
vec99995<-c(763,319,682,921,923,920,681,501,357)
vec99996<-c(500,356,761,660,922,766,502,760,934)
#' 
#' Rename the species codes
WI_TREE$SPCD<-ifelse(WI_TREE$SPCD ==391,701,
                     ifelse(WI_TREE$SPCD ==975|WI_TREE$SPCD ==317|WI_TREE$SPCD ==977|WI_TREE$SPCD ==974,972,
                            ifelse(WI_TREE$SPCD ==602|WI_TREE$SPCD ==601|WI_TREE$SPCD ==600,402,
                                   ifelse(WI_TREE$SPCD ==741|WI_TREE$SPCD ==742|WI_TREE$SPCD ==752,746,
                                          ifelse(WI_TREE$SPCD ==130|WI_TREE$SPCD ==136,125,
                                                 ifelse(WI_TREE$SPCD ==804,802,
                                                        ifelse(WI_TREE$SPCD ==91|WI_TREE$SPCD ==96,94,
                                                               ifelse(WI_TREE$SPCD ==314|WI_TREE$SPCD ==320,318,
                                                                      ifelse(WI_TREE$SPCD ==826,823,
                                                                             ifelse(WI_TREE$SPCD ==70,71,
                                                                                    ifelse(WI_TREE$SPCD ==546,544,
                                                                                           ifelse(WI_TREE$SPCD ==10|WI_TREE$SPCD ==15,12,
                                                                                                  ifelse(WI_TREE$SPCD == 830,809,
                                                                                                         ifelse(WI_TREE$SPCD %in% vec99991,99991,
                                                                                                                ifelse(WI_TREE$SPCD %in% vec99992,99992,
                                                                                                                       ifelse(WI_TREE$SPCD %in% vec99993,99993,
                                                                                                                              ifelse(WI_TREE$SPCD %in% vec99994,99994,
                                                                                                                                     ifelse(WI_TREE$SPCD %in% vec99995,99995,
                                                                                                                                            ifelse(WI_TREE$SPCD %in% vec99996,99996,WI_TREE$SPCD)))))))))))))))))))

#' 
#' 
#
plt_list <- read_csv('data/WI_PLOT_LIST_updated.CSV')
plt_list <- plt_list %>% mutate(SUBKEY = str_c(KEY, t0, str_sub(subplot_list, 1, 1), sep='_'))

#WI_COND <- WI_COND %>% mutate(KEY = str_c(STATECD, COUNTYCD, PLOT, sep='_')) %>%
#  mutate(tKEY = str_c(STATECD, COUNTYCD, PLOT, INVYR, sep='_')) %>%
#  filter(KEY %in% unique(plt_list$KEY))
#filter(tKEY %in% unique(plt_list$tKEY))

WI_TREE <- WI_TREE %>% mutate(SUBKEY = str_c(STATECD, COUNTYCD, PLOT, INVYR, SUBP, sep='_')) %>% 
  filter(SUBKEY %in% unique(plt_list$SUBKEY))


#FIA species name reference table
#Available https://apps.fs.usda.gov/fia/datamart/CSV/REF_SPECIES.csv 
#ref <- read_csv('data/REF_SPECIES.csv')
#ref <- ref %>% mutate(LANDSPEC = paste0(tolower(str_sub(GENUS,1,4)), str_sub(SPECIES,1,4))) %>%
#  select(SPCD, LANDSPEC)
ref<-species_codes
#Area of site (raster cell size squared in meters)
siteSize = 169
#'
#'
#' Subset for ONE county (COUNTYCD =1) and just keep records from cycle 8
#' and select variables of interest
#'
#WI_COND<- WI_COND %>% subset(CYCLE == 8) %>% #WHY CYCLE 8????
#  select(PLT_CN, INVYR, STATECD, COUNTYCD, PLOT, COND_STATUS_CD, CONDID, DSTRBCD1, DSTRBCD2, DSTRBCD3) %>%
#  mutate(MAPCODE = 1:nrow(.))
WI_PLOT <- WI_PLOT %>%  dplyr::select(CN, INVYR, STATECD, COUNTYCD, PLOT, ELEV, ECOSUBCD, CYCLE) %>%
  mutate(ECO_PROVINCE = str_replace(str_sub(ECOSUBCD, 1, -2), ' ', ''), 
         KEY = str_c(STATECD, COUNTYCD, PLOT, sep='_'))


WI_TREE<- WI_TREE %>% 
  select(CN,PLT_CN, INVYR, STATECD, COUNTYCD, PLOT, SUBP, TREE, STATUSCD, SPCD,
         SPGRPCD, DIA, DIAHTCD, HT, ACTUALHT, AGENTCD, DAMAGE_AGENT_CD1, 
         DAMAGE_AGENT_CD2, DAMAGE_AGENT_CD3, MORTYR, STANDING_DEAD_CD, 
         TPA_UNADJ, DRYBIO_BOLE, DRYBIO_TOP, DRYBIO_STUMP, DRYBIO_SAPLING, 
         DRYBIO_WDLD_SPP, DRYBIO_BG, DRYBIO_AG, CARBON_AG, CARBON_BG, SUBKEY) %>%
  left_join(., ref, by = 'SPCD')
#'
#' Function to convert species and diameter to age cohort
#' 
ageClass <- function(ecoregion, spcd, diameter)
{
  if (is.na(diameter))
  {
    return(NA)
  }
  if (!((spcd %in% landGrow$SPECIES) & (ecoregion %in% landGrow$ECOREGION)))
  {
    return(NA)
  }
  if (!exists('landGrow'))
  {
    landGrow <- read.table('LANDIS_work/all_txt/Ecoregion_diameter_table.txt', skip=4, col.names=c('ECOREGION','SPECIES','AGE','DIAMETER'))
  }
  if (diameter <= min(landGrow[(landGrow$SPECIES == spcd) & (landGrow$ECOREGION == ecoregion), 'DIAMETER']))
  {return(min(landGrow[(landGrow$SPECIES == spcd) & (landGrow$ECOREGION == ecoregion), 'AGE']))}
  if (diameter >= max(landGrow[(landGrow$SPECIES == spcd) & (landGrow$ECOREGION == ecoregion), 'DIAMETER']))
  {return(max(landGrow[(landGrow$SPECIES == spcd) & (landGrow$ECOREGION == ecoregion), 'AGE']))}
  minAgeClass <- abs(diameter - landGrow[min(which(landGrow[(landGrow$SPECIES == spcd) & (landGrow$ECOREGION == ecoregion), 'DIAMETER'] >= diameter)), 'DIAMETER']) 
  maxAgeClass <- abs(diameter - landGrow[max(which(landGrow[(landGrow$SPECIES == spcd) & (landGrow$ECOREGION == ecoregion), 'DIAMETER'] <= diameter)), 'DIAMETER'])
  if (minAgeClass <= maxAgeClass) 
  {return(landGrow[min(which(landGrow[(landGrow$SPECIES == spcd) & (landGrow$ECOREGION == ecoregion), 'DIAMETER'] >= diameter)), 'AGE'])}
  else 
  {return(landGrow[max(which(landGrow[(landGrow$SPECIES == spcd) & (landGrow$ECOREGION == ecoregion), 'DIAMETER'] <= diameter)), 'AGE'])}
}

#'
#' Loop through conditions and subplots
#' Assign each tree in a subplot to an age cohort
#' Write age cohort and number of trees out to LANDIS initial community file
#' 
plt_exist <- plt_list %>% filter(SUBKEY %in% unique(WI_TREE$SUBKEY))
landGrow <- read.table('all_txt/Ecoregion_diameter_table.txt', skip=4, col.names=c('ECOREGION','SPECIES','AGE','DIAMETER'))

MV_KEY <- data.frame()
PLOTMAPVALUE <- 1
outFile = file('all_txt/Initial_Community.txt', 'w')
cat('LandisData "Initial Communities"\n', file=outFile, sep='\n')
for (i in 1:nrow(plt_exist))
{
  #COND_SUB <- WI_COND[i,]
  #if (COND_SUB$COND_STATUS_CD != 1){next}
  #PLOT_SUB <- WI_PLOT %>% filter(CN == COND_SUB$PLT_CN)
  #if (nrow(WI_TREE %>% filter(PLT_CN == COND_SUB$PLT_CN & STATUSCD == 1)) == 0){next}
  #TREE_SUB <- WI_TREE %>% filter(PLT_CN == COND_SUB$PLT_CN & STATUSCD == 1 & Name %in% unique(landGrow$SPECIES) & !(is.na(DIA)) & !(is.na(TPA_UNADJ))) %>% 
  #  select(PLT_CN, Name, SUBP, TPA_UNADJ, DIA)
  TREE_SUB <- WI_TREE %>% filter(SUBKEY == plt_exist[i, 'SUBKEY', drop=T])
  if (nrow(TREE_SUB) == 0){next}
  TREE_SUB <- TREE_SUB %>% filter(STATUSCD == 1 & !(is.na(DIA)) & !(is.na(TPA_UNADJ))) %>% 
    select(PLT_CN, Name, SUBP, TPA_UNADJ, DIA)
  if (nrow(TREE_SUB) == 0){next}
  
  PLOT_SUB <- WI_PLOT %>% filter(CN == unique(TREE_SUB$PLT_CN))
  ecoProv <- str_replace(PLOT_SUB$ECO_PROVINCE, ' ', '')
  TREE_SUB <- TREE_SUB %>% rowwise() %>%
    mutate(AGECLASS = ageClass(ecoProv, Name, (DIA * 2.54)),
           TREENUM = round((TPA_UNADJ * 4 / 4046.86) * siteSize))
  
  TREE_SUB <- TREE_SUB %>% filter(AGECLASS < 1000)
  
  
  TREE_GRP <- TREE_SUB %>% group_by(SUBP, Name, AGECLASS) %>%
    summarise(TREESUM = sum(TREENUM, na.rm=T), .groups='drop') %>%
    mutate(ICSTRING = paste0(' ', AGECLASS, ' (', TREESUM, ')'))
  

  TREE_OUT <- TREE_GRP %>% group_by(Name) %>%
    summarise(ICSTRING = paste(unique(Name), paste0(unique(ICSTRING) , collapse = ''), collapse = ''), .groups='drop')
  
  cat(paste('MapCode', PLOTMAPVALUE), file=outFile, sep='\n')
  cat(TREE_OUT$ICSTRING, file=outFile, sep='\n')
  cat('\n', file=outFile)
  
  out_row <- data.frame(MAPVALUE = PLOTMAPVALUE, PLT_KEY = plt_exist[i, 'SUBKEY', drop=T], ECO_PROVINCE = ecoProv)
  MV_KEY <- rbind(MV_KEY, out_row)
  PLOTMAPVALUE <- PLOTMAPVALUE + 1
  
  
}
close(outFile)
write_csv(MV_KEY, 'output/MAPVALUE_KEY.csv')
#' 
#' Now write the Initial communities text file:
#' 
#writeLines(c(paste("LandisData", "\"Initial Communities\"", sep="\t"),"\n"), con = "LANDIS_work/all_txt/initial-communities.txt") #creates the independent lines of text
#write.table(establishment_probability, "LANDIS_work/all_txt/establishment_probability.txt", row.names=F, append=TRUE, quote = FALSE) #rownames=F to prevent the indexing column to be created. quote=F prevents quotes surrounding character strings
#' 
#' 
#' ##################################################################################
# 10. Read-in the Climate data (ready)----
#'###################################################################################
#' 
#' The climate txt will be a placeholder for this work. LANDIS won't interact with it, so no changes
#' needed from the sample file
#' 
climate<-read.table('LANDIS_work/all_txt/Climate.txt', header=T)
#'
#'
#' ##################################################################################
# 11. Create table: Biomass coefficients (ready)----
#'###################################################################################
#' 
#' Values from Jenkins et al 2003
#'
# Write the scenario file:
fileConn <- file("LANDIS_work/all_txt/BioMassCoef.txt")
writeLines(c(paste("LandisData", "BiomassCoefficients", sep="\t"),
             "\n",
             ">> species classification used to calculate Biomass",
             "\n",
             paste("Number_of_species_class", "19", sep="\t"),
             paste("minimum_DBH_for_calculating_biomass", "2.54", sep="\t"),
             paste(">> V0", "V1", "Type of Species", sep="\t"),
             paste("-2.2094", "2.3867", ">> 01-Aspen", sep="\t"),
             paste("-2.2094", "2.3867", ">> 02-alder", sep="\t"),
             paste("-2.2094", "2.3867", ">> 03-cottonwood", sep="\t"),
             paste("-2.2094", "2.3867", ">> 04-willow", sep="\t"),
             paste("-1.9123", "2.3651", ">> 05-Soft maple", sep="\t"),
             paste("-1.9123", "2.3651", ">> 06-birch", sep="\t"),
             paste("-2.4800", "2.4835", ">> 07-Mixed hardwood", sep="\t"),
             paste("-2.0127", "2.4342", ">> 08-Hard maple", sep="\t"),
             paste("-2.0127", "2.4342", ">> 09-hickory", sep="\t"),
             paste("-2.0127", "2.4342", ">> 10-beech", sep="\t"),
             paste("-2.0127", "2.4342", ">> 11-oak", sep="\t"),
             paste("-2.0336", "2.2592", ">> 12-Cedar", sep="\t"),
             paste("-2.0336", "2.2592", ">> 13-larch", sep="\t"),
             paste("-2.2304", "2.4435", ">> 14-Douglas-fir", sep="\t"),
             paste("-2.5384", "2.4814", ">> 15-True fir", sep="\t"),
             paste("-2.5384", "2.4814", ">> 16-hemlock", sep="\t"),
             paste("-2.5356", "2.4349", ">> 17-Pine", sep="\t"),
             paste("-2.0773", "2.3323", ">> 18-Spruce", sep="\t"),
             paste("-1.0000", "1.0000", ">> 19-Grass", sep="\t")),fileConn)
close(fileConn)   
#' 
#' ##################################################################################
# 12. Create table: Density species parameters (ready)----
#'###################################################################################
#'
#' Read in the REF_SPECIES table
#' 
REF_SPECIES<- read.csv("LANDIS_work/data/main/REF_SPECIES.CSV")%>%
  select(SPCD, JENKINS_TOTAL_B1, JENKINS_TOTAL_B2) #select variables of interest
#'
#' Read our species codes
#' 
biomass_coef<-read.csv("LANDIS_WORK/data/excel_created/biomass_coefficients.CSV")
#'
#' Merge both tables
#' 
biomass_coef<-merge(biomass_coef, REF_SPECIES, by="SPCD", all.x=T)
#'
#' Create a "name" column with the format Landis uses (4 letters of the genus+4letters of the species)
biomass_coef<-tidyr::separate(biomass_coef, Scientific_name, c("genus", "species"), "\\s(?:.+\\s)?") #separate the string for scientific name into character vectors divided by the space
#'
#' convert the genus to lower letter
#' 
biomass_coef$genus<-tolower(biomass_coef$genus)
#'
#' Now add a biomass class
#' 
biomass_coef$BioCoef<-ifelse(biomass_coef$JENKINS_TOTAL_B1==-2.2094 & biomass_coef$JENKINS_TOTAL_B2==2.3867 & grepl("aspen",biomass_coef$Common_name),1,
                             ifelse(biomass_coef$JENKINS_TOTAL_B1==-2.2094 & biomass_coef$JENKINS_TOTAL_B2==2.3867 & grepl("alder",biomass_coef$Common_name),2,
                                    ifelse(biomass_coef$JENKINS_TOTAL_B1==-2.2094 & biomass_coef$JENKINS_TOTAL_B2==2.3867 & grepl("cottonwood",biomass_coef$Common_name),3,
                                           ifelse(biomass_coef$JENKINS_TOTAL_B1==-2.2094 & biomass_coef$JENKINS_TOTAL_B2==2.3867 & grepl("willow",biomass_coef$Common_name),4,
                                                  ifelse(biomass_coef$JENKINS_TOTAL_B1==-1.9123 & biomass_coef$JENKINS_TOTAL_B2==2.3651 & grepl("maple",biomass_coef$Common_name),5,
                                                         ifelse(biomass_coef$JENKINS_TOTAL_B1==-1.9123 & biomass_coef$JENKINS_TOTAL_B2==2.3651 & grepl("boxelder",biomass_coef$Common_name),5,
                                                                ifelse(biomass_coef$JENKINS_TOTAL_B1==-1.9123 & biomass_coef$JENKINS_TOTAL_B2==2.3651 & grepl("birch",biomass_coef$Common_name),6,
                                                                       ifelse(biomass_coef$JENKINS_TOTAL_B1==-2.4800 & biomass_coef$JENKINS_TOTAL_B2==2.4835,7,
                                                                              ifelse(biomass_coef$JENKINS_TOTAL_B1==-2.0127 & biomass_coef$JENKINS_TOTAL_B2==2.4342 & grepl("maple",biomass_coef$Common_name),8,
                                                                                     ifelse(biomass_coef$JENKINS_TOTAL_B1==-2.0127 & biomass_coef$JENKINS_TOTAL_B2==2.4342 & grepl("hickory",biomass_coef$Common_name),9,
                                                                                            ifelse(biomass_coef$JENKINS_TOTAL_B1==-2.0127 & biomass_coef$JENKINS_TOTAL_B2==2.4342 & grepl("beech",biomass_coef$Common_name),10,
                                                                                                   ifelse(biomass_coef$JENKINS_TOTAL_B1==-2.0127 & biomass_coef$JENKINS_TOTAL_B2==2.4342 & grepl("oak",biomass_coef$Common_name),11,
                                                                                                          ifelse(biomass_coef$JENKINS_TOTAL_B1==-2.0336 & biomass_coef$JENKINS_TOTAL_B2==2.2592 & grepl("cedar",biomass_coef$Common_name),12,
                                                                                                                 ifelse(biomass_coef$JENKINS_TOTAL_B1==-2.0336 & biomass_coef$JENKINS_TOTAL_B2==2.2592 & grepl("larch",biomass_coef$Common_name),13,
                                                                                                                        ifelse(biomass_coef$JENKINS_TOTAL_B1==-2.0336 & biomass_coef$JENKINS_TOTAL_B2==2.2592 & grepl("tamarack",biomass_coef$Common_name),13,
                                                                                                                               ifelse(biomass_coef$JENKINS_TOTAL_B1==-2.2304 & biomass_coef$JENKINS_TOTAL_B2==2.4435,14,
                                                                                                                                      ifelse(biomass_coef$JENKINS_TOTAL_B1==-2.5384 & biomass_coef$JENKINS_TOTAL_B2==2.4814 & grepl("fir",biomass_coef$Common_name),15,
                                                                                                                                             ifelse(biomass_coef$JENKINS_TOTAL_B1==-2.5384 & biomass_coef$JENKINS_TOTAL_B2==2.4814 & grepl("hemlock",biomass_coef$Common_name),16,
                                                                                                                                                    ifelse(biomass_coef$JENKINS_TOTAL_B1==-2.5356 & biomass_coef$JENKINS_TOTAL_B2==2.4349,17,
                                                                                                                                                           ifelse(biomass_coef$JENKINS_TOTAL_B1==-2.0773 & biomass_coef$JENKINS_TOTAL_B2==2.3323,18,
                                                                                                                                                                  ifelse(biomass_coef$JENKINS_TOTAL_B1==-1.0000 & biomass_coef$JENKINS_TOTAL_B2==1.0000,19,0)))))))))))))))))))))
#' For the generic categories, replace the biomass coefficient class to the correct one
#' 
biomass_coef$BioCoef<-ifelse(biomass_coef$Name == "L_Tohard",8,
                             ifelse(biomass_coef$Name=="L_Toconi",15,
                                    ifelse(biomass_coef$Name=="L_Inhard",1,
                                           ifelse(biomass_coef$Name=="L_Inconi",17,
                                                  ifelse(biomass_coef$Name=="S_Tohard"|biomass_coef$Name=="S_Inhard",7,biomass_coef$BioCoef)))))
#' 
#' Create column max diameter (for this research, the maximum diameter won't be taken into account in LANDIS since we are giving it the age-diameter curves. The maxdia parameter is only used if the model needs to use its default growth curves)
#' Therefore, we will assign a high value of 9999 only as a placeholder
#'   
biomass_coef$MaxDia <-9999
#'
#' Create column total seed (for this research, since we won't have species seed in, the value is zero)
biomass_coef$TotalSeed <-0
#'   
#' Now prepare the text file 
#' Select variables of interest
#' 
biomass_coef<-biomass_coef %>% select (Name, Sp_type, BioCoef, MaxDia, MaxSDI, TotalSeed )
#'
names(species_attributes)[1]<-">> Name" #add the comment symbol for LANDIS
#'
#' Write the species file
writeLines(c(paste("LandisData", "DensitySpeciesParameters", sep="\t"),"\n"), con = "LANDIS_work/all_txt/density_speciesparameters.txt") #creates the independent lines of text
write.table(biomass_coef, "LANDIS_work/all_txt/density_speciesparameters.txt", row.names=F, append=TRUE, quote = FALSE) #rownames=F to prevent the indexing column to be created. quote=F prevents quotes surrounding character strings
#' 
#'
#' ##################################################################################
# 13. Create table: Dynamic ecoregion input ----
#'###################################################################################
#'
#' Calculate basal area growth by ecoregion over time
#install.packages("quantreg")
library(quantreg)
#'
#' Single example
#cohort_log <- read_csv('all_txt/Density_cohort_log_MGS100.csv')
#
#ba_summ1 <- cohort_log %>% group_by(Time, EcoName, SiteIndex) %>% 
#  summarise(BASALAREA = sum((Diameter / 2.54)^2 * 0.005454154 / 0.0415)) %>% 
#  group_by(EcoName, Time) %>% summarise(MEANBA = mean(BASALAREA))
#
#plot_ba<-ggplot(ba_summ, aes(x=Time, y=MEANBA, group=EcoName, color=EcoName)) +
#  geom_line() +
#  geom_point()

#' Loop to compare different MaxGSO 
#' These files will need to be saved elsewhere or renamed for the different maximum-GSO alternatives
#' since they will be overwritten
#' I would recommend putting the MaxGSO value at the end of the file name
#' Example = Density_cohort_log_MGS10.csv for the current MaxGSO of 1.0
#' 
fllst <- list.files('all_txt/', 'Density_cohort_log_') #Read in the files uploaded from LANDIS runs
#
gso_comp <- tibble()
#
for (i in fllst){
  cohort_log <- read_csv(paste0('all_txt/', i))
  # mgs <- sprintf("%0.1f", as.integer(str_split(str_replace(i, '[.]csv', ''), '_', simplify = T)[,4]) / 10) #something is producing an NA value here
  mgs<-str_split(str_replace(i, '[.]csv', ''), '_', simplify = T)[,4] #obtain the 4th column of the string in the file's name (MGS100)
  mgs<-(as.integer(str_extract(mgs, "[0-9]+")))/100 #extract numbers from string and divide them by 100 to represent the units of the maximum growing space
  mgs <- sprintf("%0.2f", mgs) #leave 2 digits
  ba_summ <- cohort_log %>% group_by(Time, EcoName, SiteIndex) %>% 
    summarise(BASALAREA = sum((Diameter / 2.54)^2 * 0.005454154 / 0.0415)) %>% 
    group_by(EcoName, Time) %>% 
    summarise(MEANBA = mean(BASALAREA)) %>% 
    mutate(MGS = as.numeric(mgs))
  #'  
  gso_comp <- bind_rows(gso_comp, ba_summ)
  #"  
  plot_ba<-ggplot(ba_summ, aes(x=Time, y=MEANBA, group=EcoName, color=EcoName)) +
    geom_line() +
    geom_point()
  #  ggsave(file=paste("figures/BA_MGS_LANDIS",mgs,".tiff", sep=""), plot = ba_mgs_plot, width=600, height=450, units="mm", dpi=300, compression = "lzw")
}

#save(gso_comp, file = 'code/gso_comp.RData')
#' Create a plot visualizing the results for the different maximum growing spaces run
par(mfrow=c(3,3))
ba_mgs_plot<-ggplot(gso_comp, aes(x=Time, y=MEANBA, group=EcoName, color=EcoName)) +
  geom_line() +
  geom_point()+
  facet_wrap(~MGS)
#facet_grid(. ~ MGS)
#'
ggsave(file="figures/BA_MGS_LANDIS.tiff", plot = ba_mgs_plot, width=600, height=450, units="mm", dpi=300, compression = "lzw")
#'
#' LANDIS basal area model
landisBA = gso_comp %>% group_by(MGS, EcoName) %>% summarise(maxBA = max(MEANBA)) # get the max mean basal area per maximum growing space
save(landisBA, file = 'code/landisBA.RData')
#'
baModel = lm(MGS ~ maxBA, data=landisBA)
save(baModel, file = 'code/baModel.RData')
#'
#' Get basal area observations by plot, stocking level, and ecoregion
#' 
wiBA <- tpa(wiTB, grpBy = c('ECO_PROVINCE', 'ALSTK'), byPlot = T)
wiBA <- wiBA %>% filter(PLOT_STATUS_CD == 1)
#'
#' Get a graph for one ecoregion
#ggplot(wiBA %>% filter(ECO_PROVINCE == '222M'), aes(x=ALSTK, y=BAA)) +
#  geom_point()
#'
#' Get the graph for all ecoregions
#' 
FIA_mgs_plot<-ggplot(wiBA, aes(x=ALSTK, y=BAA)) +
  geom_point()+
  facet_wrap(~ECO_PROVINCE)
#'
ggsave(file="figures/BA_MGS_FIA.tiff", plot = FIA_mgs_plot, width=600, height=450, units="mm", dpi=300, compression = "lzw")
#'
#
#' Quantile regression stocking and basal area
qs = c(0.95)
#'
subsec.quant = wiBA %>% #not sure what is happening from here onward???????
  split(.$ECO_PROVINCE) %>%
  map(~ rq(BAA ~ poly(ALSTK, 2), data=.x, tau=qs))
#'
subsec.quant.pred = subsec.quant %>% 
  imap(~ predict(.x, data.frame(ALSTK=100), tau=qs))
#'
#growingSpace = function(df)
#{
#  classNum = length(unique(df$BIOCLASS))
#  ecoSection = str_sub(df$SUB, 1, -2)[1]
#  predDF = data.frame(maxBA = subsec.quant.pred[[ecoSection]][(5-classNum):4])
#  predGS = predict(baModel, data.frame(maxBA = predDF))
#  df %>% mutate(MAXGS = predGS[df$BIOCLASS])
#}

current.MGSO =  test %>% filter(CLIMATE == 'CURRENT') %>% group_by(SUB, CLIMATE, .add=T) %>% group_split() %>% 
  map_dfr(growingSpace)

#' ##################################################################################
# 14. Create table: Land use ----
#'###################################################################################
#'
#'
#' # 1. Read in the FIA data for the state of Wisconsin
#' 
#' Read in the data for Wisconsin (run from the first section of the code)
#' 
#WI_COND<-fread("data/main_WI_2020/WI_COND.csv")#read the condition table
#WI_PLOT<-read_csv("data/main_WI_2020/WI_PLOT.csv")#read the plot table
#fiaDir <- 'D:/fia/rFIA'
#wiTB <- readFIA(fiaDir, states = c('WI'), tables=c("TREE"), inMemory = T, nCores = 3)
#WI_TREE <- wiTB$TREE %>% filter(INVYR >= 2000)
#WI_TREE<-read_csv("data/main_WI_2020/WI_TREE.csv")#read the tree table
#
#WI_COND <- WI_COND %>% mutate(PLT_CN = as.character(PLT_CN))
WI_TREE <- WI_TREE %>% mutate(TREE_CN = as.character(TREE_CN), PLT_CN = as.character(PLT_CN), PREV_TRE_CN = as.character(PREV_TRE_CN))
WI_PLOT <- WI_PLOT %>% mutate(PLT_CN = as.character(PLT_CN), PREV_PLT_CN = as.character(PREV_PLT_CN))
#' 
#' Recode species
#' Vectors containing generic categories
vec99991<-c(531,462)
vec99992<-c(202,57)
vec99993<-c(901,373,552)
vec99994<-c(68)
vec99995<-c(763,319,682,921,923,920,681,501,357)
vec99996<-c(500,356,761,660,922,766,502,760,934)
#' 
#' Rename the species codes
WI_TREE$SPCD<-ifelse(WI_TREE$SPCD ==391,701,
                     ifelse(WI_TREE$SPCD ==975|WI_TREE$SPCD ==317|WI_TREE$SPCD ==977|WI_TREE$SPCD ==974,972,
                            ifelse(WI_TREE$SPCD ==602|WI_TREE$SPCD ==601|WI_TREE$SPCD ==600,402,
                                   ifelse(WI_TREE$SPCD ==741|WI_TREE$SPCD ==742|WI_TREE$SPCD ==752,746,
                                          ifelse(WI_TREE$SPCD ==130|WI_TREE$SPCD ==136,125,
                                                 ifelse(WI_TREE$SPCD ==804,802,
                                                        ifelse(WI_TREE$SPCD ==91|WI_TREE$SPCD ==96,94,
                                                               ifelse(WI_TREE$SPCD ==314|WI_TREE$SPCD ==320,318,
                                                                      ifelse(WI_TREE$SPCD ==826,823,
                                                                             ifelse(WI_TREE$SPCD ==70,71,
                                                                                    ifelse(WI_TREE$SPCD ==546,544,
                                                                                           ifelse(WI_TREE$SPCD ==10|WI_TREE$SPCD ==15,12,
                                                                                                  ifelse(WI_TREE$SPCD == 830,809,
                                                                                                         ifelse(WI_TREE$SPCD %in% vec99991,99991,
                                                                                                                ifelse(WI_TREE$SPCD %in% vec99992,99992,
                                                                                                                       ifelse(WI_TREE$SPCD %in% vec99993,99993,
                                                                                                                              ifelse(WI_TREE$SPCD %in% vec99994,99994,
                                                                                                                                     ifelse(WI_TREE$SPCD %in% vec99995,99995,
                                                                                                                                            ifelse(WI_TREE$SPCD %in% vec99996,99996,WI_TREE$SPCD)))))))))))))))))))

#' 
#'
plt_list <- read_csv('data/WI_PLOT_LIST_updated.CSV')
plt_list <- plt_list %>% mutate(SUBKEY = str_c(KEY, str_sub(subplot_list, 1, 1), sep='_'))
#'
#'
WI_TREE <- WI_TREE %>% mutate(SUBKEY = str_c(STATECD, COUNTYCD, PLOT, SUBP, sep='_')) %>% 
  filter(SUBKEY %in% unique(plt_list$SUBKEY))
#'
#"
#FIA species name reference table
#Available https://apps.fs.usda.gov/fia/datamart/CSV/REF_SPECIES.csv 
#ref <- read_csv('data/REF_SPECIES.csv')
#ref <- ref %>% mutate(LANDSPEC = paste0(tolower(str_sub(GENUS,1,4)), str_sub(SPECIES,1,4))) %>%
#  select(SPCD, LANDSPEC)
ref<-species_codes
#Area of site (raster cell size squared in meters)
siteSize = 169
#'
#' ###########################
#'
#' Creating a change database
#' 
#' Put the tree table into a wide format
plt_codes<-plt_list %>% select(SUBKEY,t0,t1,t2,t3)%>%
  pivot_longer(t0:t3, names_to="Time", values_to="INVYR")%>%
  filter(INVYR!=0)
#' 
tree_long<-merge(WI_TREE, plt_codes, by=c("SUBKEY","INVYR"), all.x=T)%>%
  select(SUBKEY, STATECD, COUNTYCD, PLOT, SUBP, TREE, INVYR, Time, SPCD, DIA, AGENTCD, STATUSCD, TPA_UNADJ)
#'
#' Create a column for if the tree was killed or not
tree_long$AGENTCD[is.na(tree_long$AGENTCD)] <- 0 #fill NA's with zeros
#'  
#' 
tree_wide <- tree_long %>%
  pivot_wider(id_cols=c(SUBKEY, STATECD, COUNTYCD, PLOT, SUBP, TREE, SPCD, TPA_UNADJ), names_from = Time, values_from = c("INVYR", "STATUSCD", "DIA", "AGENTCD"))
#' 
#' Fill in missing values
tree_wide[is.na(tree_wide)] <- 0 #fill NA's with zeros
#'
#' Now create an event/land use column
#' 
land_use<-tree_wide
land_use$event<-ifelse(land_use$DIA_t0==0 & land_use$DIA_t1!=0,"plant_t1",
                       ifelse(land_use$DIA_t1==0 & land_use$DIA_t2!=0,"plant_t2",
                              ifelse(land_use$DIA_t2==0 & land_use$DIA_t3!=0,"plant_t3",
                                     ifelse(land_use$AGENTCD_t1!=0 ,"kill_t1",
                                            ifelse(land_use$AGENTCD_t2!=0 ,"kill_t2",
                                                   ifelse(land_use$AGENTCD_t3!=0 ,"kill_t3", "no_change"))))))
#'
#' Filter out trees that started dead at t0
#' 
land_use<-land_use%>% filter(STATUSCD_t0!=2) #HOW ABOUT STATUSCD 3 (CUT/REMOVED BY HUMAN)
#' 
#' Filter out trees with no change
land_use_change<-land_use %>% filter(event!="no_change")
#' 
#' Now create a column for action, action year, and diameter at the action time to reflect plant/kill, the year when to do so, and the diameter of the tree to plant or remove
land_use_change$ACTION_YEAR<-ifelse(land_use_change$event=="plant_t1",land_use_change$INVYR_t1,
                                    ifelse(land_use_change$event=="plant_t2",land_use_change$INVYR_t2,
                                           ifelse(land_use_change$event=="plant_t3",land_use_change$INVYR_t3,
                                                  ifelse(land_use_change$event=="kill_t1",land_use_change$INVYR_t1,
                                                         ifelse(land_use_change$event=="kill_t2",land_use_change$INVYR_t2,
                                                                ifelse(land_use_change$event=="kill_t3",land_use_change$INVYR_t3,NA))))))
#' 
land_use_change$ACTION<-ifelse(land_use_change$event=="plant_t1"|land_use_change$event=="plant_t2"|land_use_change$event=="plant_t3", "plant",
                               ifelse(land_use_change$event=="kill_t1"|land_use_change$event=="kill_t2"|land_use_change$event=="kill_t3", "remove", NA))
#' 
land_use_change$DIA_ACTION<-ifelse(land_use_change$event=="plant_t1",land_use_change$DIA_t1,
                                   ifelse(land_use_change$event=="plant_t2",land_use_change$DIA_t2,
                                          ifelse(land_use_change$event=="plant_t3",land_use_change$DIA_t3,
                                                 ifelse(land_use_change$event=="kill_t1",land_use_change$DIA_t1,
                                                        ifelse(land_use_change$event=="kill_t2",land_use_change$DIA_t2,
                                                               ifelse(land_use_change$event=="kill_t3",land_use_change$DIA_t3,NA))))))
#' 
#' Transform diameter to cm and action year to time step
#' 
land_use_change<-land_use_change%>% mutate(TIME_STEP=ACTION_YEAR-2000,
                                           DIA_ACTION= DIA_ACTION*2.54)
#' 
#' 
#' Add the species names
#' 
land_use_change<-merge(land_use_change,species_codes, by="SPCD", all.x=T)
#' Select variables of interest
#' 
land_use_change<-land_use_change%>% select(SUBKEY, STATECD, COUNTYCD,PLOT,SUBP,TREE,TPA_UNADJ, SPCD, Name, ACTION, TIME_STEP, DIA_ACTION)
#' 
#'
#' 
#' ########################### THIS IS FROM THE INITIAL COMMUNITIES CODE, HAVEN'T CHANGED IT YET
#' Select variables of interest
#'
WI_PLOT <- WI_PLOT %>%  dplyr::select(PLT_CN, INVYR, STATECD, COUNTYCD, PLOT, ECOSUBCD) %>%
  mutate(ECO_PROVINCE = str_replace(str_sub(ECOSUBCD, 1, -2), ' ', ''), 
         KEY = str_c(STATECD, COUNTYCD, PLOT, sep='_'))


WI_TREE<- WI_TREE %>% 
  select(TREE_CN,PLT_CN, INVYR, STATECD, COUNTYCD, PLOT, SUBP, TREE, STATUSCD, SPCD,
         DIA, AGENTCD, MORTYR, STANDING_DEAD_CD, 
         TPA_UNADJ, SUBKEY) %>%
  left_join(., ref, by = 'SPCD')
#'
#' Function to convert species and diameter to age cohort
#' 
ageClass <- function(ecoregion, spcd, diameter)
{
  if (is.na(diameter))
  {
    return(NA)
  }
  if (!((spcd %in% landGrow$SPECIES) & (ecoregion %in% landGrow$ECOREGION)))
  {
    return(NA)
  }
  if (!exists('landGrow'))
  {
    landGrow <- read.table('LANDIS_work/all_txt/Ecoregion_diameter_table.txt', skip=4, col.names=c('ECOREGION','SPECIES','AGE','DIAMETER'))
  }
  if (diameter <= min(landGrow[(landGrow$SPECIES == spcd) & (landGrow$ECOREGION == ecoregion), 'DIAMETER']))
  {return(min(landGrow[(landGrow$SPECIES == spcd) & (landGrow$ECOREGION == ecoregion), 'AGE']))}
  if (diameter >= max(landGrow[(landGrow$SPECIES == spcd) & (landGrow$ECOREGION == ecoregion), 'DIAMETER']))
  {return(max(landGrow[(landGrow$SPECIES == spcd) & (landGrow$ECOREGION == ecoregion), 'AGE']))}
  minAgeClass <- abs(diameter - landGrow[min(which(landGrow[(landGrow$SPECIES == spcd) & (landGrow$ECOREGION == ecoregion), 'DIAMETER'] >= diameter)), 'DIAMETER']) 
  maxAgeClass <- abs(diameter - landGrow[max(which(landGrow[(landGrow$SPECIES == spcd) & (landGrow$ECOREGION == ecoregion), 'DIAMETER'] <= diameter)), 'DIAMETER'])
  if (minAgeClass <= maxAgeClass) 
  {return(landGrow[min(which(landGrow[(landGrow$SPECIES == spcd) & (landGrow$ECOREGION == ecoregion), 'DIAMETER'] >= diameter)), 'AGE'])}
  else 
  {return(landGrow[max(which(landGrow[(landGrow$SPECIES == spcd) & (landGrow$ECOREGION == ecoregion), 'DIAMETER'] <= diameter)), 'AGE'])}
}

#'
#' Loop through conditions and subplots
#' Assign each tree in a subplot to an age cohort
#' Write age cohort and number of trees out to LANDIS initial community file
#' 
plt_exist <- plt_list %>% filter(SUBKEY %in% unique(WI_TREE$SUBKEY))
landGrow <- read.table('all_txt/Ecoregion_diameter_table.txt', skip=4, col.names=c('ECOREGION','SPECIES','AGE','DIAMETER'))

MV_KEY <- data.frame()
PLOTMAPVALUE <- 1
outFile = file('all_txt/Initial_Community.txt', 'w')
cat('LandisData "Initial Communities"\n', file=outFile, sep='\n')
for (i in 1:nrow(plt_exist))
{
  #COND_SUB <- WI_COND[i,]
  #if (COND_SUB$COND_STATUS_CD != 1){next}
  #PLOT_SUB <- WI_PLOT %>% filter(CN == COND_SUB$PLT_CN)
  #if (nrow(WI_TREE %>% filter(PLT_CN == COND_SUB$PLT_CN & STATUSCD == 1)) == 0){next}
  #TREE_SUB <- WI_TREE %>% filter(PLT_CN == COND_SUB$PLT_CN & STATUSCD == 1 & Name %in% unique(landGrow$SPECIES) & !(is.na(DIA)) & !(is.na(TPA_UNADJ))) %>% 
  #  select(PLT_CN, Name, SUBP, TPA_UNADJ, DIA)
  TREE_SUB <- WI_TREE %>% filter(SUBKEY == plt_exist[i, 'SUBKEY', drop=T])
  if (nrow(TREE_SUB) == 0){next}
  TREE_SUB <- TREE_SUB %>% filter(STATUSCD == 1 & !(is.na(DIA)) & !(is.na(TPA_UNADJ))) %>% 
    select(PLT_CN, Name, SUBP, TPA_UNADJ, DIA)
  if (nrow(TREE_SUB) == 0){next}
  
  PLOT_SUB <- WI_PLOT %>% filter(CN == unique(TREE_SUB$PLT_CN))
  ecoProv <- str_replace(PLOT_SUB$ECO_PROVINCE, ' ', '')
  TREE_SUB <- TREE_SUB %>% rowwise() %>%
    mutate(AGECLASS = ageClass(ecoProv, Name, (DIA * 2.54)),
           TREENUM = round((TPA_UNADJ * 4 / 4046.86) * siteSize))
  
  TREE_SUB <- TREE_SUB %>% filter(AGECLASS < 1000)
  
  
  TREE_GRP <- TREE_SUB %>% group_by(SUBP, Name, AGECLASS) %>%
    summarise(TREESUM = sum(TREENUM, na.rm=T), .groups='drop') %>%
    mutate(ICSTRING = paste0(' ', AGECLASS, ' (', TREESUM, ')'))
  
  
  TREE_OUT <- TREE_GRP %>% group_by(Name) %>%
    summarise(ICSTRING = paste(unique(Name), paste0(unique(ICSTRING) , collapse = ''), collapse = ''), .groups='drop')
  
  cat(paste('MapCode', PLOTMAPVALUE), file=outFile, sep='\n')
  cat(TREE_OUT$ICSTRING, file=outFile, sep='\n')
  cat('\n', file=outFile)
  
  out_row <- data.frame(MAPVALUE = PLOTMAPVALUE, PLT_KEY = plt_exist[i, 'SUBKEY', drop=T], ECO_PROVINCE = ecoProv)
  MV_KEY <- rbind(MV_KEY, out_row)
  PLOTMAPVALUE <- PLOTMAPVALUE + 1
  
  
}
close(outFile)
write_csv(MV_KEY, 'output/MAPVALUE_KEY.csv')
#' 
#'
#'
#'
#' ##################################################################################
# 15. Create table: Ecoregion parameter density (ready)----
#'###################################################################################
#'
#' Create a data frame with the ecoregion names, the climate text file and a latitude number (for this exercise, the value for latitude and the climate txt won't be used so it does not matter)
EcoregionParameters<-WI_ECO$Name
#'
ClimateFileName<-"climate.txt"
#'
Latitude<- 45
#'  
#' Combine in one data frame
#' 
ecoreg_parameters<-data.frame(EcoregionParameters, ClimateFileName, Latitude)
#'   
#'       
#' Now write the ecoregion parameters density text file:    
writeLines(c(paste("LandisData", "EcoregionParameters", sep="\t"),"\n"), con = "LANDIS_work/all_txt/EcoregionParameters_density.txt") #creates the independent lines of text
write.table(ecoreg_parameters, "LANDIS_work/all_txt/EcoregionParameters_density.txt", row.names=F, append=TRUE, quote = FALSE) #rownames=F to prevent the indexing column to be created. quote=F prevents quotes surrounding character strings
#'
#' ##################################################################################
# 16. Create table: Ecoregion diameter table ----
#'###################################################################################
#'
#' From FIA diameter growth estimates
#' 
#install.packages("rFIA")
library(rFIA)
library(tidyverse)
#'###############################################################
#Point to directory containing FIA tables
fiaDir <- 'D:/fia/rFIA/'
#getFIA(states = "WI", dir = fiaDir, load = FALSE, nCores=3) #download the FIA tables for Wisconsin
#'
wiTB <- readFIA(fiaDir, states = c('WI'), tables=c("COND", "COND_DWM_CALC", "INVASIVE_SUBPLOT_SPP", "P2VEG_SUBP_STRUCTURE", "PLOT", "POP_ESTN_UNIT","POP_EVAL", "POP_EVAL_GRP", "POP_EVAL_TYP", "POP_PLOT_STRATUM_ASSGN", "POP_STRATUM", "SEEDLING", "SUBP_COND", "SUBP_COND_CHNG_MTRX", "SUBPLOT", "SURVEY", "TREE", "TREE_GRM_BEGIN", "TREE_GRM_COMPONENT", "TREE_GRM_MIDPT"), inMemory = T, nCores = 3)%>% clipFIA() #These are the minimum FIA tables that we need for this exercise
#wiTB <- clipFIA(wiTB) #keeps only the most recent inventory
wiTB2<-wiTB

#'
#' Species codes for the 30 most abundant WI species (same as the ones used inn the species attributes table)
spcds <- c(746, 316, 318, 12, 125, 241, 375, 543, 833, 951, 129, 743, 972, 105, 95, 762, 809, 71, 802, 544, 701, 371, 837, 541, 261, 94, 823, 313, 407, 402)
#'
#----Estimate diameter growth for trees smaller than 5" DBH----
#' 
#' With the tree table:
wi.st <- wiTB$TREE %>% filter(DIA < 5 & STATUSCD == 1 & SPCD %in% spcds) %>% select(CN, PLT_CN, PREV_TRE_CN, INVYR,CYCLE, PLOT, SUBP, TREE, SPCD, DIA)
#' With the plot table (to get the ecoregions)
wi.sp <- wiTB$PLOT %>% select(PLT_CN, ECOSUBCD)
wi.sp$ECO_PROVINCE<- substr(wi.sp$ECOSUBCD, start=2, stop=5) #Leave only the strings that correspond to the ecological province (from 2 to 4). Note that there is a blanc space at the beginning of the ECOSUBCD column from the FIA database
#' 
#' Now keep only the PLT_CN and the ecological province
#' 
wi.sp<-wi.sp %>% select(PLT_CN, ECO_PROVINCE)
#'
#' Merge with the tree table
#'
wi.st<-merge(wi.st, wi.sp, by='PLT_CN')
#'
#' Now calculate diameter growth between cycles 9 and 8
#' 
wi.tg <- wi.st %>% filter(CYCLE == 9) %>% 
  left_join(wi.st %>% filter(CYCLE == 8) %>% select(CN, PREV_TRE_CN, DIA), by = c('PREV_TRE_CN' = 'CN')) 
#'
wi.tg$DIA_GROW <- wi.tg$DIA.x - wi.tg$DIA.y #remember FIA is in inches
wi.tg$sizeClass <- makeClasses(wi.tg$DIA.x, interval = 1, numLabs = T) #creates the diameter classes
dg.summ <- wi.tg %>% group_by(ECO_PROVINCE, SPCD, sizeClass) %>% summarise(DIA_GROW = mean(DIA_GROW, na.rm=T))%>% na.omit() #get the mean diameter growth and omit the na values (rows with no data for diameter growth)
#'
#'State-wide growth rates
stVR <- vitalRates(wiTB, bySpecies = T, bySizeClass = T, treeType = 'live')

#----Estimates of diameter growth for large trees----
wiTB$PLOT$ECO_PROVINCE<-substr(wiTB$PLOT$ECOSUBCD, start=2, stop=5) #Create the ecological province column in the plot table
#'
wiVR <- vitalRates(wiTB, bySpecies = T, bySizeClass = T, treeType = 'live', grpBy=(ECO_PROVINCE))
wiVR <- wiVR %>% filter(SPCD %in% spcds)
wiVR$KEY<-paste(wiVR$ECO_PROVINCE,wiVR$SPCD, sep="_") #create an identifier (KEY) column for later referencing in the loop (each species-ecoregion)
#'
#'
#'
# ----Create the loop for all the species----
#'
#' Get the species list
dg.summ$KEY<-paste(dg.summ$ECO_PROVINCE,dg.summ$SPCD, sep="_")
sp_eco_listWI<-unique(dg.summ$KEY)
#' Create a table for maximum diameters per species per ecoregion
temp<-wiTB$TREE %>% merge(wi.sp, by='PLT_CN')%>%
  filter(STATUSCD == 1) #merge the tree table with the ecoregion and only keep live trees
#
maxtable<-temp%>% group_by(ECO_PROVINCE,SPCD)%>% #create the max diameter table by species-ecoregion
  summarise(max_dia=max(DIA, na.rm=T))%>%
  mutate(KEY=paste(ECO_PROVINCE,SPCD, sep="_"))#%>% na.omit()#%>%#create the identifier here as well
#'
#' Create the empty data frames needed for the loop
#' 
#' 
dia_list <- list()

#'
#' 
library(data.table)
#' Create the for loop
#' 
for(i in 1:length(sp_eco_listWI)){
  #"
  age_dia<-data.frame()
  tempkey=sp_eco_listWI[i]
  #'
  smallWorkingTB <- dg.summ %>% filter(KEY==tempkey) %>% ungroup()
  fillTB <- tibble(sizeClass = as.double(1:4))
  smallWorkingTB <- smallWorkingTB %>% full_join(fillTB, by='sizeClass')
  smallWorkingTB$DIA_GROW<-ifelse(smallWorkingTB$DIA_GROW<=0,NA,smallWorkingTB$DIA_GROW) #replace the zeros and negative values with a very very low number
  
  smallWorkingTB <- smallWorkingTB %>% fill(everything(), .direction = 'downup') %>% 
    arrange(sizeClass)
  
  smallWorkingTB$DIA_GROW<-ifelse(is.na(smallWorkingTB$DIA_GROW),0.04,smallWorkingTB$DIA_GROW) #replace the zeros and negative values with a very very low number
  
  #'
  smallGrowthMD <-tibble(KEY=tempkey,AGE = 1, DIA = 1.0)
 
  
  while(max(smallGrowthMD$DIA) < 5){
    age <- max(smallGrowthMD$AGE) + 1
    subTB <- smallWorkingTB %>% filter(sizeClass <= max(smallGrowthMD$DIA)) %>% ungroup() %>% select(DIA_GROW) 
    diaGR <- subTB %>% slice(nrow(subTB))
    smallGrowthMD <- bind_rows(smallGrowthMD, tibble(KEY=tempkey,AGE = age, DIA = max(smallGrowthMD$DIA) + diaGR[[1,1]]))

  }
  #'
  #' Now for the trees >=5"
  #' 
  workingTB <- wiVR %>% filter(KEY==tempkey & !(is.na(DIA_GROW)))
  #'
  growthMD <- smallGrowthMD
  age <- max(growthMD$AGE) + 1
  #'
  #' Fill in zeros
  workingTB$DIA_GROW<-ifelse(workingTB$DIA_GROW<=0,NA,workingTB$DIA_GROW) #replace the zeros and negative values with a very very low number
  workingTB <- workingTB %>% fill(DIA_GROW, .direction = 'downup')
  workingTB$DIA_GROW<-ifelse(is.na(workingTB$DIA_GROW),0.04,workingTB$DIA_GROW) #replace the zeros and negative values with a very very low number
  
  #'
  while(max(growthMD$DIA) < maxtable$max_dia[which(maxtable$KEY==tempkey)]){
    if (max(growthMD$DIA) < min(workingTB$sizeClass, na.rm = T))
    {
      diaGR <- subTB %>% slice(1)
    } else {
      subTB <- workingTB %>% filter(sizeClass <= max(growthMD$DIA)) %>% select(DIA_GROW)
      diaGR <- subTB %>% slice(nrow(subTB))
    }
    
    growthMD <- bind_rows(growthMD, tibble(KEY=tempkey,AGE = age, DIA = max(growthMD$DIA) + diaGR[[1,1]]))
    age <- age + 1
  }
  age_dia<-rbind(age_dia, growthMD)
  
  age_dia <- age_dia %>% 
    mutate(ECO_PROVINCE = str_split(KEY, '_', simplify=T)[,1],
           SPCD = as.integer(str_split(KEY, '_', simplify=T)[,2]),
           DIA = round((DIA * 2.54), digits = 3)) %>% 
    left_join(species_codes, by='SPCD')
  if(sum(is.na(age_dia$Name)) > 0){print(tempkey)}
  
  dia_list[[tempkey]] <- age_dia %>% select(ECO_PROVINCE, Name, AGE, DIA)
  #print(tempkey)
  
}

print('End part 1')
ecoSp <- expand.grid(unique(wiTB$PLOT$ECO_PROVINCE), spcds, stringsAsFactors = F) %>% 
  mutate(SPECOKEY = str_c(Var1, Var2, sep = '_')) %>% select(SPECOKEY)

msngEcoSP <- ecoSp %>% filter(!(SPECOKEY %in% names(dia_list)))

maxtable.state <- wiTB$TREE %>% filter(STATUSCD == 1) %>%  group_by(SPCD) %>% #create the max diameter table by species-ecoregion
  summarise(max_dia=max(DIA, na.rm=T))

dg.summ.state <- wi.tg %>% group_by(SPCD, sizeClass) %>% summarise(DIA_GROW = mean(DIA_GROW, na.rm=T))%>% na.omit() #get the mean diameter growth and omit the na values (rows with no data for diameter growth)

# Copy dia_list in case something goes wrong
dia_list_cp <- dia_list

#' Do it again for missing species - ecoregion combinations
#' 

for(i in 1:nrow(msngEcoSP)){
  #"
  age_dia <- data.frame()
  tempkey <- msngEcoSP[i, 1]
  spcd <- as.integer(str_split(tempkey, pattern = '_', simplify = T)[,2])
  #'
  smallWorkingTB <- dg.summ.state %>% filter(SPCD == spcd) %>% ungroup()
  fillTB <- tibble(sizeClass = as.double(1:4))
  smallWorkingTB <- smallWorkingTB %>% full_join(fillTB, by='sizeClass')
  smallWorkingTB$DIA_GROW<-ifelse(smallWorkingTB$DIA_GROW<=0,NA,smallWorkingTB$DIA_GROW) #replace the zeros and negative values with a very very low number
  
  smallWorkingTB <- smallWorkingTB %>% fill(everything(), .direction = 'downup') %>% 
    arrange(sizeClass)
  
  smallWorkingTB$DIA_GROW<-ifelse(is.na(smallWorkingTB$DIA_GROW),0.04,smallWorkingTB$DIA_GROW) #replace the zeros and negative values with a very very low number
  
  #'
  smallGrowthMD <-tibble(KEY=tempkey,AGE = 1, DIA = 1.0)
  
  while(max(smallGrowthMD$DIA) < 5){
    age <- max(smallGrowthMD$AGE) + 1
    subTB <- smallWorkingTB %>% filter(sizeClass <= max(smallGrowthMD$DIA)) %>% ungroup() %>% select(DIA_GROW) 
    diaGR <- subTB %>% slice(nrow(subTB))
    smallGrowthMD <- bind_rows(smallGrowthMD, tibble(KEY=msngEcoSP[i, 1],AGE = age, DIA = max(smallGrowthMD$DIA) + diaGR[[1,1]]))
    
  }
  #'
  #' Now for the trees >=5"
  #' 
  workingTB <- stVR %>% filter(SPCD == spcd & !(is.na(DIA_GROW)))
  #'
  growthMD <- smallGrowthMD
  age <- max(growthMD$AGE) + 1
  #'
  #' Fill in zeros
  workingTB$DIA_GROW<-ifelse(workingTB$DIA_GROW<=0,NA,workingTB$DIA_GROW) #replace the zeros and negative values with a very very low number
  workingTB <- workingTB %>% fill(DIA_GROW, .direction = 'downup')
  workingTB$DIA_GROW<-ifelse(is.na(workingTB$DIA_GROW),0.04,workingTB$DIA_GROW) #replace the zeros and negative values with a very very low number
  
  #'
  while(max(growthMD$DIA) < maxtable.state$max_dia[which(maxtable.state$SPCD==spcd)]){
    if (max(growthMD$DIA) < min(workingTB$sizeClass, na.rm = T))
    {
      diaGR <- subTB %>% slice(1)
    } else {
      subTB <- workingTB %>% filter(sizeClass <= max(growthMD$DIA)) %>% select(DIA_GROW)
      diaGR <- subTB %>% slice(nrow(subTB))
    }
    
    growthMD <- bind_rows(growthMD, tibble(KEY=tempkey,AGE = age, DIA = max(growthMD$DIA) + diaGR[[1,1]]))
    age <- age + 1
  }
  age_dia<-rbind(age_dia, growthMD)
  
  age_dia <- age_dia %>% 
    mutate(ECO_PROVINCE = str_split(KEY, '_', simplify=T)[,1],
           SPCD = as.integer(str_split(KEY, '_', simplify=T)[,2]),
           DIA = round((DIA * 2.54), digits = 3)) %>% 
    left_join(species_codes, by='SPCD')
  if(sum(is.na(age_dia$Name)) > 0){print(tempkey)}

  dia_list[[tempkey]] <- age_dia %>% select(ECO_PROVINCE, Name, AGE, DIA)
  #print(tempkey)
  
}
print('End part 2')

#' add to dia the values for the generic categories?
genericSpLst <- list(l_tohard = 318,
                     l_toconi = 12,
                     l_inhard = 746,
                     l_inconi = 125,
                     s_tohard = 701,
                     s_inhard = 500)

for (i in 1:length(genericSpLst))
{
  age_dia <- data.frame()
  spcd <- genericSpLst[[i]]
  #'
  smallWorkingTB <- dg.summ.state %>% filter(SPCD == spcd) %>% ungroup()
  fillTB <- tibble(sizeClass = as.double(1:4))
  smallWorkingTB <- smallWorkingTB %>% full_join(fillTB, by='sizeClass')
  smallWorkingTB$DIA_GROW<-ifelse(smallWorkingTB$DIA_GROW<=0,NA,smallWorkingTB$DIA_GROW) #replace the zeros and negative values with a very very low number
  
  smallWorkingTB <- smallWorkingTB %>% fill(everything(), .direction = 'downup') %>% 
    arrange(sizeClass)
  
  smallWorkingTB$DIA_GROW<-ifelse(is.na(smallWorkingTB$DIA_GROW),0.04,smallWorkingTB$DIA_GROW) #replace the zeros and negative values with a very very low number
  
  #'
  smallGrowthMD <-tibble(KEY=tempkey,AGE = 1, DIA = 1.0)
  
  while(max(smallGrowthMD$DIA) < 5){
    age <- max(smallGrowthMD$AGE) + 1
    subTB <- smallWorkingTB %>% filter(sizeClass <= max(smallGrowthMD$DIA)) %>% ungroup() %>% select(DIA_GROW) 
    diaGR <- subTB %>% slice(nrow(subTB))
    smallGrowthMD <- bind_rows(smallGrowthMD, tibble(KEY=tempkey,AGE = age, DIA = max(smallGrowthMD$DIA) + diaGR[[1,1]]))

  }
  #'
  #' Now for the trees >=5"
  #' 
  workingTB <- stVR %>% filter(SPCD == spcd & !(is.na(DIA_GROW)))
  #'
  growthMD <- smallGrowthMD
  age <- max(growthMD$AGE) + 1
  #'
  #' Fill in zeros
  workingTB$DIA_GROW<-ifelse(workingTB$DIA_GROW<=0,NA,workingTB$DIA_GROW) #replace the zeros and negative values with a very very low number
  workingTB <- workingTB %>% fill(DIA_GROW, .direction = 'downup')
  workingTB$DIA_GROW<-ifelse(is.na(workingTB$DIA_GROW),0.04,workingTB$DIA_GROW) #replace the zeros and negative values with a very very low number
  
  #'
  while(max(growthMD$DIA) < maxtable.state$max_dia[which(maxtable.state$SPCD==spcd)]){
    if (max(growthMD$DIA) < min(workingTB$sizeClass, na.rm = T))
    {
      diaGR <- subTB %>% slice(1)
    } else {
      subTB <- workingTB %>% filter(sizeClass <= max(growthMD$DIA)) %>% select(DIA_GROW)
      diaGR <- subTB %>% slice(nrow(subTB))
    }
    
    growthMD <- bind_rows(growthMD, tibble(KEY=tempkey,AGE = age, DIA = max(growthMD$DIA) + diaGR[[1,1]]))
    age <- age + 1
  }
  age_dia<-rbind(age_dia, growthMD) %>%       
    mutate(Name = names(genericSpLst)[i], 
           DIA = round((DIA * 2.54), digits = 3))
  

  
  for (j in unique(wiTB$PLOT$ECO_PROVINCE))
  {
    tempkey <- str_c(j, names(genericSpLst)[i])
    
    age_dia <- age_dia %>% 
      mutate(ECO_PROVINCE = j)
    if(sum(is.na(age_dia$Name)) > 0){print(tempkey)}
    
    dia_list[[tempkey]] <- age_dia %>% select(ECO_PROVINCE, Name, AGE, DIA)
    #print(tempkey)
  }
  
}

# Check out the maximum ages
check <- do.call(rbind.data.frame, dia_list)
maxAge <- do.call(rbind.data.frame, dia_list) %>% group_by(Name) %>% summarise(MAXAGE = max(AGE))

#'
#'
#' Now write the ecoregion parameters density text file:    
writeLines(c(paste("LandisData", "EcoregionDiameterTable", sep="\t"),"\n",paste(">>Ecoregion", "Species", "Age", "Diameter", sep="\t")), con = "all_txt/Ecoregion_diameter_table.txt") #creates the independent lines of text
lapply(1:length(dia_list), function(i) write.table(dia_list[[i]],
                                                   file = "all_txt/Ecoregion_diameter_table.txt",
                                                   row.names = F,
                                                   append = T,
                                                   quote = F,
                                                   col.names = F))

#'
#'
#' ##################################################################################
# 17. Create table: Tree information (work in progress) ----
#'################################################################################### 
#'
#' Pull up the subplot list from the previous filtered table
#'
#' Use the sub_temp data frame created above
#' 
SUB_temp<- WI_SUPB_ECO %>% select(STATECD, COUNTYCD, PLOT, SUBP, KEY)
#'
#' Now prepare the tree table with just the necessary variables
#'
WI_TREE_temp2<- WI_TREE %>% select( STATECD, COUNTYCD, PLOT, SUBP, INVYR ,TREE_CN, TREE, STATUSCD, SPCD, SPGRPCD, DIA, DRYBIO_AG, AGENTCD)
#'
#' Merge the tree table with the SUB_temp to leave only the subplots of interest
#' 
tree<-merge(SUB_temp, WI_TREE_temp2, by=c("STATECD", "COUNTYCD", "PLOT", "SUBP"))
#'
#' Now merge the tree tables with the species codes (keep all the values from the tree table and just the values from the codes tables that match)
#'
tree<-merge(tree, species_codes,by="SPCD", all.x=T)
#'
#' Create a KEY for the tree table
tree$KEY<-paste(tree$KEY, tree$TREE, sep="_")
#'
#' The generated NA values correspond to the generic species groups (all sp, all tolerant, and all intolerant). This was generated for Landis use as we should not give the model too many species. The reasoning for selecting species is as follows: leave the 30 most abundant species (which together account for almost 96% of the cumulative abundance) and all the rest group into 3 groups: tolerant species, intolerant species, and all the rest. These groups will be randomly assigned (with equal weights) and the species will lose its identity and instead it will be assigned to on of these generic groups 
#' 
#' Now we should replace the NAs with the randomly selected groups 1 to 3
#' 
tree$name[is.na(tree$name)] <- 0 #replace NAs with zeros
#' 
set.seed(12345)
tree$name<-ifelse(tree$name == 0 , sample(1:3, replace=T), tree$name) #replace zeros with randomly selected group number
#' 
tree$name<-ifelse(tree$name == 1 , "Allsp",
                  ifelse(tree$name== 2, "Alltole", 
                         ifelse(tree$name==3, "Allinto", tree$name))) #Replace the group numbers with the names (All sp, all tolerant, all intolerant)
#'
#' Convert the biomass units from --- CHECK #$%#$%#%#%#$%#$%#$%#$%#
#' 
#' *(2.47105/2204.64)),#Biomass aboveground per hectacre (1 hectare = 2.47105 acres; 1 Mg = 2204.64 pounds) --> Units: Mg/ha
#' 
#' Diameter from inches to cm --- CHECK  #$$%#$%#$%#$%#$%#$%#$%#$
#' 
#' 1 INCH=2.54cm
#'
#' Now add the time periods from the WI_LOOP_LD table
#'
WI_LOOP_LD<-WI_LOOP_LD[,-5] #Removing the "key" column as that contains codes up to a plot level. The tree table contains it at a subplot level, so we will leave that one
#'
#'Now merge the tree and WI_LOOP_LD tables
#'
tree<-merge(WI_LOOP_LD, tree, by=c("STATECD", "COUNTYCD", "PLOT", "INVYR"), all.y = T)
#'
#' Rename the values in the time period column
#' 
tree$TIME_PER=ifelse(tree$TIME_PER == 0, "t0",
                     ifelse(tree$TIME_PER == 1, "t1",
                            ifelse(tree$TIME_PER == 2, "t2","t3")))
#'
#' Now put the table into a wide format
#' 
tree_wide <- tree %>%
  pivot_wider(id_cols=c(STATECD, COUNTYCD, PLOT, SUBP, TREE, KEY, name), names_from = TIME_PER, values_from = c("INVYR", "STATUSCD", "DIA", "DRYBIO_AG", "AGENTCD"))
#'  
#'
#write.csv(tree_wide,'LANDIS_work/data/R_created/tree_wide.CSV')
#'
#'
#' ##################################################################################
# 18. Maps: Subplots with ecoregions ----
#'################################################################################### 
#'
#' Merge the plot list database with the ecological province variable:
#'
WI_PLOT$ECO_PROVINCE<-substr(WI_PLOT$ECOSUBCD, start=2, stop=5) #Leave only the strings that correspond to the ecological province (from 2 to 4). Note that there is a blank space at the beginning of the ECOSUBCD column from the FIA database
#' 
WI_PLOT_COORD<-merge(WI_PLOT, WI_PLOT_LIST, by=c("STATECD", "COUNTYCD", "PLOT", "INVYR"))
#' 
#' Select variables of interest
WI_PLOT_COORD<-WI_PLOT_COORD%>% select(STATECD, COUNTYCD, PLOT, INVYR, PLT_CN, ECO_PROVINCE)
#' 
#' Get a sequence of "map codes" for distinct ecoregions
#' 
eco_codes<-as.data.frame(unique(WI_PLOT$ECO_PROVINCE)) 
names(eco_codes)<-"ECO_PROVINCE"
eco_codes <- eco_codes %>% arrange(ECO_PROVINCE) %>% mutate(Map_code_ecoregion = 1:nrow(eco_codes))

#' 
WI_PLOT_COORD<-merge(WI_PLOT_COORD,eco_codes, by="ECO_PROVINCE")
WI_PLOT_COORD$SUBP_ID<-seq(1,nrow(WI_PLOT_COORD),by=1)
#'
#' Now create the grid of plots
#' First add the X and Y coordinates
#' Since 6400 is an 80 by 80 square, we have 80 cells on the x axis with a length of 12.97 each (80*12.97=1024.63). Same for the y axis
#x<- seq(0,1024.63,by=12.97) 
#y<- seq(0,1024.63,by=12.97)
#
#A=expand.grid(x = x, y = y) #the expand.grid function allows us to do all the combination of the numbers. This will create coordinate combinations for every cell in our imaginary landscape
#'
#' Now bind A with the WI_PLOT_COORD data frame
#' 
#WI_PLOT_COORD<-cbind(WI_PLOT_COORD, A)
#'
#'
#ecoregion_map<-ggplot() +
#  geom_raster(data = WI_PLOT_COORD , aes(x = x, y = y, fill = ECO_PROVINCE)) + 
#  scale_fill_manual(values = terrain.colors (12), name="Ecological \nsections") +
#  theme(axis.title = element_blank()) + 
#  coord_quickmap()
# https://datacarpentry.org/r-raster-vector-geospatial/02-raster-plot/

#install.packages("raster")
#library(raster)
#writeRaster(ecoregion_map, "ecoregion_test.tif", NAflag=-9999) #can't save the ggplot into a spatial image
#ggsave(file="ecoregion_test.tiff")


#WI_PLOT_COORD<-WI_PLOT_COORD[order(WI_PLOT_COORD$SUBP_ID, decreasing=F),]# reorder the dataframe for reproducibility later on when we create new rasters and want to maintain the same subplot distribution
#'
initial_communities_map<-subplot_key<-ecoregion_map<-raster(ncol=80, nrow=80, xmn=0,xmx=1037.6,ymn=0,ymx=1037.6,vals=0) #create empty ecoregion, initial communities and a subplot id raster to fill up laterLU1<-LU2<-LU3<-LU4<-LU5<-LU6<-LU7<-LU8<-LU9<-LU10<-LU11<-LU12<-LU13<-LU14<-LU15<-LU16<-LU17<-LU18<-LU19<-LU20<-raster(ncol=80, nrow=80, xmn=0,xmx=1037.6,ymn=0,ymx=1037.6) #Create empty land use rasters to be filled later
res(ecoregion_map) #check resolution ...IS THIS IN METERS?
ncell(ecoregion_map) #check number of cells
#values(ecoregion_map)<-WI_PLOT_COORD$Map_code_ecoregion #add values to the rasters
#values(initial_communities_map)<-WI_PLOT_COORD$SUBP_ID
#values(subplot_key)<-as.factor(WI_PLOT_COORD$model_run_key)

MV_KEY <- MV_KEY %>% 
left_join(WI_PLOT %>% dplyr::select(KEY, ECO_PROVINCE), by = c('KEY')) %>% 
left_join(eco_codes, by='ECO_PROVINCE')

initial_communities_map[1:max(MV_KEY$MAPVALUE)] <- MV_KEY$MAPVALUE

ecoregion_map[1:nrow(MV_KEY)] <- MV_KEY$Map_code_ecoregion

#'
#' Now stack the different raster layers into one
#' 
#s1 <- stack(ecoregion_map, subplot_key) #stack raster
#'
#b1 <- brick(r1, r2, r3) #brick raster
#'
#' Save the files
writeRaster(ecoregion_map, "all_txt/ecoregion.img", NAflag=-9999, overwrite=T, datatype='INT2S')

plot(ecoregion_map)
#'
writeRaster(initial_communities_map, "all_txt/initialcommunity.img", NAflag=-9999, overwrite=T, datatype='INT2S')

#'
#' 
#' ##################################################################################
# 6. Create table: Subplot_ disturbances ?? ----
#'################################################################################### 
#'
# WI_SUPB_ECO<- read.csv('LANDIS_work/data/R_created/WI_SUBP_ECO.CSV')
# WI_SUPB_ECO<- WI_SUPB_ECO[,-1]
#'
#' Pull up the subplot list from the previous filtered table
#'
SUB_temp<- WI_SUPB_ECO %>% select(STATECD, COUNTYCD, PLOT, SUBP)
#'
#' Now prepare the tree table with just the necessary variables
#'
WI_TREE_temp<- WI_TREE %>% select( STATECD, COUNTYCD, PLOT, SUBP, INVYR ,TREE_CN, TREE, STATUSCD, SPCD, SPGRPCD, DIA, AGENTCD)
#'
#' Now merge the two tables
#'
disturbances <- merge(SUB_temp, WI_TREE_temp, by=c("STATECD", "COUNTYCD", "PLOT", "SUBP"))
#'
#' Convert NAs to 9999 for the loop to run
#' 
disturbances$DIA[is.na(disturbances$DIA)] <- 9999
#'
#' Create a for loop that will classify diameters into diameter classes every 5". Range of diameters in Wisconsin is 5-90"
#' 
sequenceDIA<- seq(from=0, to=60, by=5) #create a sequence from 0 to 60 that will be used in the loop. The max DIA recorded for this dataset is 55.6
#'
disturbances$DIA_CLASS<- 0 #create an empty column for diameter class
#'
#'
#for(i in 1:length(sequenceDIA)){
#  temp=sequenceDIA[i]
#  for(j in 1:nrow(disturbances)){
#    if(disturbances$DIA[j] >= temp & disturbances$DIA[j]<(temp+5)
#    ){ disturbances$DIA_CLASS[j] = paste("[",temp,"-",(temp+5),">")
#    } else{NULL}}}
#'
#'
#' Alternative code to do it with an ifelse statement instead of a for loop
#' 
#WI_TP$DIA_CLASS<- ifelse(ifelse(disturbances$DIA>=5 & disturbances$DIA <10, "[5-10>",
#                                ifelse(disturbances$DIA>=10 & disturbances$DIA <15, "[10-15>",
#                                       ifelse(disturbances$DIA>=15 & disturbances$DIA <20, "[15-20>",
#                                              ifelse(disturbances$DIA>=20 & disturbances$DIA <25, "[20-25>",
#                                       "[25-30>")))))
#'
#write.csv(disturbances, 'LANDIS_work/data/R_created/disturbances.CSV')
#'
#' Read the file created by the loop (this is for saving time when we want to keep working on the code later on and we don't want to run the loop again as this takes a lot of time)
#'
#disturbances<-read.csv('LANDIS_work/data/R_created/disturbances.CSV')
#disturbances<-disturbances[,-1]
#' 
#' I STOPPED HERE ---- CHECK SPECIES CODES
#' #### Use the group_by and summarise functions to create a table containing disturbances per diameter class in each subplot
#' 
DIST_SUBP<- disturbances %>% group_by(STATECD, COUNTYCD, INVYR, PLOT, ECOSUBCD,SUBP, SPGRPCD, DIA_CLASS, AGENTCD)%>% #Agent of mortality will be our disturbance variable
  summarise( N_TREES_DIA= n())
#'
#' Add a new column indicating just the number of trees disturbed in each diameter class
#' 
DIST_SUBP$N_DIST_TREES <- ifelse (DIST_SUBP$AGENTCD != "NA", DIST_SUBP$N_TREES_DIA, "NA")
#'
#' Rename AGENTCD variable to DIST_TYPE
#' 
colnames(DIST_SUBP)[8] <- "DIST_TYPE"
#'
#' Fill the NAs for disturbance types and number of disturbed trees with zeros
#'
DIST_SUBP$N_DIST_TREES[is.na(DIST_SUBP$N_DIST_TREES)] <- 0 # Filling the missing values with zeros
DIST_SUBP$DIST_TYPE[is.na(DIST_SUBP$DIST_TYPE)] <- 0 # Filling the missing values with zeros
#'
#' Look at our output
#' 
head(DIST_SUBP, 10) #This table will be the Disturbance variable ########################
#'
# write.csv(DIST_SUBP,'data//WI_CSV//DIST_SUBP_LANDIS.CSV')
#'







WI_TREE <- select(WI_TREE, TREE_CN,PLT_CN, INVYR, STATECD, COUNTYCD, PLOT, SUBP,CONDID, TREE, STATUSCD, SPCD, SPGRPCD, DIA, DIAHTCD, HT, ACTUALHT, AGENTCD, DAMAGE_AGENT_CD1, DAMAGE_AGENT_CD2, DAMAGE_AGENT_CD3, MORTYR, STANDING_DEAD_CD, TPA_UNADJ, DRYBIO_BOLE, DRYBIO_TOP, DRYBIO_STUMP, DRYBIO_SAPLING, DRYBIO_WDLD_SPP, DRYBIO_BG, DRYBIO_AG, CARBON_AG, CARBON_BG)
#' 





#' 
#' 
#' ################################################################################################
#' Merge the tables
#'
WI_CP<-merge(WI_PLOT, WI_COND, by=c("STATECD","COUNTYCD","PLOT","INVYR", "PLT_CN")) #merge the plot and condition tables
#'
WI_TP<- merge(WI_TREE, WI_PLOT, by=c("STATECD","COUNTYCD","PLOT","INVYR", "PLT_CN")) #merge tree and plot tables
#'
WI_TC<- merge(WI_TREE, WI_COND, by=c("STATECD","COUNTYCD","PLOT","INVYR", "CONDID" ,"PLT_CN")) #merge the Tree and condition tables
#'
WI_TCP<- merge(WI_TC, WI_PLOT, by=c("STATECD","COUNTYCD","PLOT","INVYR", "PLT_CN")) #merge the three tables
#'
###################################################################################
#'
# Disturbance variable ----
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
#WI_TP <- WI_TP %>% filter( DIA >= 5) #filter out trees that correspond to microplot. Not doing this for the Landis analysis
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
# write.csv(WI_TP,'data//WI_CSV//WI_diameter_class.CSV')
#'
#'
#' #### 1.3 Use the group_by and summarise functions to create a table containing disturbances per diameter class in each subplot
#' 
DIST_SUBP<- WI_TP %>% group_by(STATECD, COUNTYCD, INVYR, PLOT, ECOSUBCD,SUBP, SPGRPCD, DIA_CLASS, AGENTCD)%>% #Agent of mortality will be our disturbance variable
  summarise( N_TREES_DIA= n())
#'
#' Add a new column indicating just the number of trees disturbed in each diameter class
#' 
DIST_SUBP$N_DIST_TREES <- ifelse (DIST_SUBP$AGENTCD != "NA", DIST_SUBP$N_TREES_DIA, "NA")
#'
#' Rename AGENTCD variable to DIST_TYPE
#' 
colnames(DIST_SUBP)[8] <- "DIST_TYPE"
#'
#' Fill the NAs for disturbance types and number of disturbed trees with zeros
#'
DIST_SUBP$N_DIST_TREES[is.na(DIST_SUBP$N_DIST_TREES)] <- 0 # Filling the missing values with zeros
DIST_SUBP$DIST_TYPE[is.na(DIST_SUBP$DIST_TYPE)] <- 0 # Filling the missing values with zeros
#'
#' Look at our output
#' 
head(DIST_SUBP, 10) #This table will be the Disturbance variable ########################
#'
# write.csv(DIST_SUBP,'data//WI_CSV//DIST_SUBP_LANDIS.CSV')
#'
#' ######################################################################################
# Species Attribute ----
#' ######################################################################################
#'
#' There are 21 species group codes in WI from the SPGRPCD variable in FIA tree table
#'
#' Read the FVS file containing the species codes
#' 
FVS_species<-read.csv("data/WI_CSV/species_FVS.CSV") #species not totally match
#' 
#' Read the rFIA tree table
#' 
sp_list<-read.csv("data/WI_CSV/WI_SP_LIST_FIA.CSV")
#' 
#' Read the list of plots to be used in this study an merge with tree data
#' 
TREE_PLOT<-read.csv("LANDIS_work/data/R_created/WI_PLOT_LIST.CSV")%>%
  select(STATECD, COUNTYCD, PLOT, KEY)%>%
  merge(WI_TREE, all.x=T, by=c("STATECD", "COUNTYCD", "PLOT"))
#' 
#' Get the species cumulative abundance for WI
#' 
TREE_AB<-TREE_PLOT %>% group_by(SPGRPCD,SPCD)%>% # All species in the plots used for this study
  summarise(abundance=n())
#' 
#TREE_AB<-WI_TREE %>% group_by(SPGRPCD,SPCD)%>% #Code for all species in WI
#  summarise(abundance=n())
#'
#' Merge the tree abundance and the species list tables
#' 
TREE_AB<-merge(TREE_AB, sp_list, by="SPCD")
#'   
#' Order the observations so more abundant species show up first
#' 
TREE_AB<-TREE_AB[order(TREE_AB$abundance, decreasing=T),]
#' 
TREE_AB$CUM_PER<-(100*cumsum(TREE_AB$abundance))/sum(TREE_AB$abundance)
#'
#write.csv(TREE_AB,'data//WI_CSV//SPECIES_CUM_LIST_STUDY.CSV')
#'   
#
#'
#'#'
#' Creating uniform time periods for remeasurements
#' 
#'For this we will create a loop which will assign a number for period 1 (on the first occurence of a subplot) and so on for every remeasurement period for every subplot (after we filtered by status code, so just information for live and dead trees will be included)
#'
#'Let's select the identifier columns of our database to work just with those
#'
WI_LOOP<- select(WI_TP, STATECD, COUNTYCD, PLOT, SUBP, INVYR) #select the identifier columns that we will use for the loop
WI_LOOP<- unique(WI_LOOP) #get unique values for those identifiers
#'
#' Now let's work the loop:
#' 
#'  Make remeasurements uniform by standarizing the years into time 0, 1, 2 and so on
#'
#' The idea is to create a loop that will give a number for period 1,2,3 each time it finds a SUBplot occuring (time 0 and all the remeasurements)
#'
#'  First sort database by year, so we're sure earlier measurements of the same plot don't occur after more recent ones 
#'
WI_LOOP<-WI_LOOP[order(WI_LOOP$INVYR),]
#Now create a new column KEY holding the contents of the first three columns (STATECD, COUNTYCD, PLOT, SUBP) so that the loop can look for every combination of state, county and plot and assign an increasing number to each time it finds that combination
#' 
WI_LOOP$KEY<-paste(WI_LOOP$STATECD, WI_LOOP$COUNTYCD, WI_LOOP$PLOT, WI_LOOP$SUBP, sep='_')
#'
#'As the new column KEY has repeated occurences, let's create a vector: vec that will hold the unique items of KEY (this will be used inside the loop to find unique occurences)
vec<-unique(WI_LOOP$KEY)
#'
#'Create the loop
#'
#' Create a new column PLOT_NUM: it will hold the RANK of the measurement taken for each (state,county,plot) combination (aka each key)
#' (i.e., a key that has three measurements will have three occureneces/rows in the database, with column PLOT_NUM holding values 0, 1, and 2 successively)
#'
WI_LOOP$TIME_PER<-0
for(i in 1:length(vec)){
  tempkey=vec[i]
  counter=0
  for(j in 1:nrow(WI_LOOP)){
    if(WI_LOOP$KEY[j]==tempkey){
      WI_LOOP$TIME_PER[j]=counter
      counter=counter+1}}}
#'
test2<-WI_LOOP
#'
test2<-test2%>% group_by(STATECD, COUNTYCD, PLOT, SUBP, INVYR, TIME_PER)%>%
  summarize(n=n())
#'
#'
#write.csv(WI_remper,'data/WI_CSV/remeasurements.CSV') #next time we run this code we won't create the loop again, just read the csv created
#'
