# Preparing the dataset for Wisconsin ----
###################################################################################
#' ### Installing and loading the libraries needed
#'
#install.packages("tidyverse")
library(tidyverse)
library(data.table)
library(rFIA)
library(raster)
###################################################################################

# Read in the FIA data for the state of Wisconsin ----
#' 
#' Read in the data for Wisconsin
#' 
#WI_COND<-fread("main_WI_2020/WI_COND.csv", na.strings = "NA")#read the condition table
#WI_PLOT<-fread("main_WI_2020/WI_PLOT.csv", na.strings = "NA")#read the plot table
#WI_TREE<-fread("main_WI_2020/WI_TREE.csv", na.strings = "NA")#read the tree table
WI_COND<-read.csv("data/main_WI_2020/WI_COND.csv")#read the condition table
WI_PLOT<-read.csv("data/main_WI_2020/WI_PLOT.csv")#read the plot table
WI_TREE<-read.csv("data/main_WI_2020/WI_TREE.csv")#read the tree table
#'
#' Just keep records from 2000 on. This is when the annual inventory for WI started
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
# Create subsets for the different model runs ----
#' 
#' Read in the plot list that meet the F/M condition & at least 3 remeasurements
plt_list <- read_csv('code/WI_PLOT_FILTERED.csv')
plt_list<-plt_list[,-13]
#'
#' Prepare the key column in the tree table
#' 
WI_TREE2<-WI_TREE #get an extra copy of the tree table to avoid re-reading it from scratch
WI_TREE$KEY<- paste(WI_TREE$STATECD, WI_TREE$COUNTYCD, WI_TREE$PLOT, WI_TREE$SUBP, sep="_")
#'
#'
# Species codes ----
#' 
#' Read in the species attributes table previously created
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
species_codes<-species_attributes%>% dplyr::select(SPCD, Name)
#'
#' ##################################################################################
# 1. Create table: Initial communities ----
#'###################################################################################
#' 
#' # 1. For model run #1
#' 
WI_TREE <- WI_TREE %>% mutate(TREE_CN = as.character(TREE_CN), PLT_CN = as.character(PLT_CN), PREV_TRE_CN = as.character(PREV_TRE_CN))
WI_PLOT <- WI_PLOT %>% mutate(PLT_CN = as.character(PLT_CN), PREV_PLT_CN = as.character(PREV_PLT_CN))
#' 
#' Recode species
#' Vectors containing generic categories
vec99991<-c(531,462)#L_int_hard
vec99992<-c(202,57) #L_tol_con
vec99993<-c(901,373,552) #L_int_hard
vec99994<-c(68) #L_int_con
vec99995<-c(763,319,682,921,923,920,681,501,357,765,997,935,927,358,937,491) #S_tol_hard
vec99996<-c(500,356,761,660,922,766,502,760,934) #S_int_hard
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
#' Create a subkey that includes time zero for the plt_list
#'
#plt_list <- plt_list %>% mutate(SUBKEY = str_c(KEY, t0, str_sub(subplot_list, 2, 2), sep='_'))

#plt_list_s1 <- plt_list %>% mutate(SUBKEY = str_c(KEY, t0, str_sub(subplot_list, 1, 1), sep='_'))
#plt_list_s2 <- plt_list %>% mutate(SUBKEY = str_c(KEY, t0, str_sub(subplot_list, 2, 2), sep='_'))
plt_list_s3 <- plt_list %>% mutate(SUBKEY = str_c(KEY, t0, str_sub(subplot_list, 3, 3), sep='_')) #########change depending on subplot to use
plt_list_s4 <- plt_list %>% mutate(SUBKEY = str_c(KEY, t0, str_sub(subplot_list, 4, 4), sep='_'))
plt_list<-rbind(plt_list_s3, plt_list_s4)
#'
#'
#'
#' Create the same subkey for the tree table
#WI_TREE<-WI_TREE2
WI_TREE <- WI_TREE %>% mutate(SUBKEY = str_c(STATECD, COUNTYCD, PLOT, INVYR, SUBP, sep='_')) %>% 
  dplyr::filter(SUBKEY %in% unique(plt_list$SUBKEY))
#'
#' Species codes
ref<-species_codes
#Area of site (raster cell size squared in meters)
siteSize = 169
#'
#'
WI_PLOT <- WI_PLOT %>%  dplyr::select(PLT_CN, INVYR, STATECD, COUNTYCD, PLOT, ELEV, ECOSUBCD, CYCLE) %>%
  mutate(ECO_PROVINCE = str_replace(str_sub(ECOSUBCD, 1, -2), ' ', ''), 
         KEY = str_c(STATECD, COUNTYCD, PLOT, sep='_'))


WI_TREE<- WI_TREE %>% 
  dplyr::select(TREE_CN,PLT_CN, INVYR, STATECD, COUNTYCD, PLOT, SUBP, TREE, STATUSCD, SPCD,
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
    landGrow <- read.table('simulations/s3_s4/Ecoregion_diameter_table_adjusted.txt', skip=4, col.names=c('ECOREGION','SPECIES','AGE','DIAMETER')) ##########update subplots when needed
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
plt_exist <- plt_list %>% dplyr::filter(SUBKEY %in% unique(WI_TREE$SUBKEY))
landGrow <- read.table('simulations/s3_s4/Ecoregion_diameter_table_adjusted.txt', skip=4, col.names=c('ECOREGION','SPECIES','AGE','DIAMETER')) ###########update subplot location when needed
#'
MV_KEY <- data.frame()
PLOTMAPVALUE <- 1
outFile = file('simulations/s3_s4/Initial_Community.txt', 'w') ###### update output location when needed
cat('LandisData "Initial Communities"\n', file=outFile, sep='\n')
for (i in 1:nrow(plt_exist))
{
  TREE_SUB <- WI_TREE %>% dplyr::filter(SUBKEY == plt_exist[i, 'SUBKEY', drop=T])
  if (nrow(TREE_SUB) == 0){next}
  TREE_SUB <- TREE_SUB %>% dplyr::filter(STATUSCD == 1 & !(is.na(DIA)) & !(is.na(TPA_UNADJ))) %>% 
    dplyr::select(PLT_CN, Name, SUBP, TPA_UNADJ, DIA)
  if (nrow(TREE_SUB) == 0){next}
#'  
  PLOT_SUB <- WI_PLOT %>% dplyr::filter(PLT_CN == unique(TREE_SUB$PLT_CN))
  ecoProv <- str_replace(PLOT_SUB$ECO_PROVINCE, ' ', '')
  TREE_SUB <- TREE_SUB %>% rowwise() %>%
    mutate(AGECLASS = ageClass(ecoProv, Name, (DIA * 2.54)),
           TREENUM = round((TPA_UNADJ * 4 / 4046.86) * siteSize))
#'  
  TREE_SUB <- TREE_SUB %>% dplyr::filter(AGECLASS < 1000)
#'  
#'  
  TREE_GRP <- TREE_SUB %>% group_by(SUBP, Name, AGECLASS) %>%
    summarise(TREESUM = sum(TREENUM, na.rm=T), .groups='drop') %>%
    mutate(ICSTRING = paste0(' ', AGECLASS, ' (', TREESUM, ')'))
#'  
#'  
  TREE_OUT <- TREE_GRP %>% group_by(Name) %>%
    summarise(ICSTRING = paste(unique(Name), paste0(unique(ICSTRING) , collapse = ''), collapse = ''), .groups='drop')
#'  
  cat(paste('MapCode', PLOTMAPVALUE), file=outFile, sep='\n')
  cat(TREE_OUT$ICSTRING, file=outFile, sep='\n')
  cat('\n', file=outFile)
#'  
  out_row <- data.frame(MAPVALUE = PLOTMAPVALUE, PLT_KEY = plt_exist[i, 'SUBKEY', drop=T], ECO_PROVINCE = ecoProv)
  MV_KEY <- rbind(MV_KEY, out_row)
  PLOTMAPVALUE <- PLOTMAPVALUE + 1
#' 
#'  
}
close(outFile)
write_csv(MV_KEY, 'simulations/s3_s4/output/MAPVALUE_KEY.csv') ###########update output when needed
#' 
#' ##################################################################################
# 2. Maps: Subplots with ecoregions ----
#'################################################################################### 
#'
MV_KEY<-read_csv('simulations/s3_s4/output/MAPVALUE_KEY.csv') ################update subplot location
#' Merge the plot list database with the ecological province variable:
#'
WI_PLOT$ECO_PROVINCE<-substr(WI_PLOT$ECOSUBCD, start=1, stop=4) #Leave only the strings that correspond to the ecological province (from 2 to 4). Note that there is a blank space at the beginning of the ECOSUBCD column from the FIA database
#' 
WI_PLOT_COORD<-merge(WI_PLOT, plt_list, by.x=c("STATECD", "COUNTYCD", "PLOT", "INVYR"), by.y=c("STATECD", "COUNTYCD", "PLOT", "t0"))
#' 
#' Select variables of interest
WI_PLOT_COORD<-WI_PLOT_COORD%>% dplyr::select(STATECD, COUNTYCD, PLOT, INVYR, PLT_CN, ECO_PROVINCE)
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
#install.packages("raster")
#library(raster)
#'
#initial_communities_map<-subplot_key<-ecoregion_map<-raster(ncol=80, nrow=80, xmn=0,xmx=1037.6,ymn=0,ymx=1037.6,vals=0) #create empty ecoregion, initial communities and a subplot id raster to fill up later
initial_communities_map<-subplot_key<-ecoregion_map<-raster(ncol=100, nrow=100, xmn=0,xmx=1297,ymn=0,ymx=1297,vals=0) #s1_s2..create empty ecoregion, initial communities and a subplot id raster to fill up later ###### update according to number of subplots pooled
#initial_communities_map<-subplot_key<-ecoregion_map<-raster(ncol=117, nrow=117, xmn=0,xmx=1517.49,ymn=0,ymx=1517.49,vals=0) #s1_s2_s3..create empty ecoregion, initial communities and a subplot id raster to fill up later
res(ecoregion_map) #check resolution
ncell(ecoregion_map) #check number of cells
#'
MV_KEY <- MV_KEY %>% 
  left_join(eco_codes, by='ECO_PROVINCE')

#
initial_communities_map[1:max(MV_KEY$MAPVALUE)] <- MV_KEY$MAPVALUE
#
ecoregion_map[1:nrow(MV_KEY)] <- MV_KEY$Map_code_ecoregion
#
#' Save the files
writeRaster(ecoregion_map, "simulations/s3_s4/ecoregion.img", NAflag=-9999, overwrite=T, datatype='INT2S') ############update output location
#'
#plot(ecoregion_map)
#'
writeRaster(initial_communities_map, "simulations/s3_s4/initialcommunity.img", NAflag=-9999, overwrite=T, datatype='INT2S') ############## update output location

#'
#' ##################################################################################
# 3. Create table: Land use ----
#'###################################################################################
#' Read in initial communities key
MV_KEY<-read.csv('simulations/s3_s4/output/MAPVALUE_KEY.csv') ############# update subplot location
MV_KEY <- MV_KEY %>% rowwise() %>% mutate(KEY = paste(str_split(PLT_KEY, '_', simplify=T)[,1:3], collapse='_'))
MV_KEY <- MV_KEY %>% mutate(SUBKEY = str_c(KEY, '_', str_split(PLT_KEY, '_', simplify=T)[,5]))
#'
plt_list <- read.csv('code/WI_PLOT_FILTERED.csv')
#'
#' Choose the appropriate subplot group (s1,s2,s3, or s4) ############# update subplot selection 
#'
#plt_list_s1 <- plt_list %>% 
#  mutate(SUBKEY = str_c(KEY, str_sub(subplot_list, 1, 1), sep='_')) %>% #Change the subplot list subplot for every run
#  dplyr::filter(SUBKEY %in% unique(MV_KEY$SUBKEY))
#'
#plt_list_s2 <- plt_list %>% 
#  mutate(SUBKEY = str_c(KEY, str_sub(subplot_list, 2, 2), sep='_')) %>% #Change the subplot list subplot for every run
#  dplyr::filter(SUBKEY %in% unique(MV_KEY$SUBKEY))
#'
plt_list_s3 <- plt_list %>% 
  mutate(SUBKEY = str_c(KEY, str_sub(subplot_list, 3, 3), sep='_')) %>% #Change the subplot list subplot for every run
  dplyr::filter(SUBKEY %in% unique(MV_KEY$SUBKEY))
#'
plt_list_s4<- plt_list %>% 
    mutate(SUBKEY = str_c(KEY, str_sub(subplot_list, 4, 4), sep='_')) %>% #Change the subplot list subplot for every run
  dplyr::filter(SUBKEY %in% unique(MV_KEY$SUBKEY))
#'
plt_list<-rbind(plt_list_s3,plt_list_s4) ####### update here which subplots to bind
#'
WI_TREE<-WI_TREE2 #reset the WI_TREE table to the original

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


WI_TREE <- WI_TREE %>% mutate(SUBKEY = str_c(STATECD, COUNTYCD, PLOT, SUBP, sep='_')) %>% 
  dplyr::filter(SUBKEY %in% unique(plt_list$SUBKEY))
#"
ref<-species_codes
#Area of site (raster cell size squared in meters)
siteSize = 169
#'
#' ###########################
#'
#' Creating a change database
#' 
#' Put the tree table into a wide format
plt_codes<-plt_list %>% dplyr::select(SUBKEY,t0,t1,t2,t3)%>%
  pivot_longer(t0:t3, names_to="Time", values_to="INVYR")%>%
  dplyr::filter(INVYR!=0)
#' 
tree_long<-merge(WI_TREE, plt_codes, by=c("SUBKEY","INVYR"), all.x=T)%>%
  dplyr::select(SUBKEY, STATECD, COUNTYCD, PLOT, SUBP, TREE, INVYR, Time, SPCD, DIA, AGENTCD, STATUSCD, TPA_UNADJ)
#'
#' Create a column for if the tree was killed or not
tree_long$AGENTCD[is.na(tree_long$AGENTCD)] <- 0 #fill NA's with zeros
tree_long$AGENTCD<-ifelse(tree_long$AGENTCD==60 | tree_long$AGENTCD==70,0,tree_long$AGENTCD) #replace the agent code vegetation and other for not disturbed. The logic here is that LANDIS should remove those trees with self thinning and the background mortality incorporated in the model
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
land_use<-land_use%>% dplyr::filter(STATUSCD_t0!=2) #HOW ABOUT STATUSCD 3 (CUT/REMOVED BY HUMAN)
#' 
#' Filter out trees with no change
land_use_change<-land_use %>% dplyr::filter(event!="no_change")
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

land_use_change <- land_use_change %>% mutate(KEY = str_c(STATECD, COUNTYCD, PLOT, sep='_')) %>% 
  left_join(plt_list %>% dplyr::select(KEY, t0), by = c('KEY'))
#' 
#' Transform diameter to cm and action year to time step
#' 
land_use_change<-land_use_change%>% mutate(TIME_STEP=ACTION_YEAR-t0,
                                           DIA_ACTION= DIA_ACTION*2.54)
#' 
#' 
#' Add the species names
#' 
land_use_change<-merge(land_use_change,species_codes, by="SPCD", all.x=T)

land_use_change <- land_use_change %>% dplyr::filter(!(ACTION == 'plant' & DIA_ACTION >= 12.7))
land_use_change <- land_use_change %>% dplyr::filter(DIA_ACTION > 0) %>% dplyr::filter(SPCD != 299)
#
#' ###########################
#' Select variables of interest
#'
WI_PLOT <- WI_PLOT %>%  dplyr::select(PLT_CN, INVYR, STATECD, COUNTYCD, PLOT, ECOSUBCD) %>%
  mutate(ECO_PROVINCE = str_replace(str_sub(ECOSUBCD, 1, -2), ' ', ''), 
         KEY = str_c(STATECD, COUNTYCD, PLOT, sep='_'))
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
    landGrow <- read.table('simulatios/s3_s4/Ecoregion_diameter_table_adjusted.txt.txt', skip=4, col.names=c('ECOREGION','SPECIES','AGE','DIAMETER'))  ############ update subplot location
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
outFile <-  file('simulations/s3_s4/land-use.txt', 'w') ############### update output
#'
#'
cat('LandisData   "Land Use"',
    '',
    'Timestep    1',
    'InputMaps	landuse-{timestep}.img',
    'SiteLog		output/land-use/site-log.csv',
    '',
    '>>------------------------------------',
    '',
    'LandUse          forest',
    '>>------------------------------------',
    'MapCode          0', 
    'AllowHarvest?    yes', 
    'LandCoverChange  NoChange',
    '',
    file = outFile, sep='\n')

luKEY <- tibble()
luMapCode <- 1
for (ts in unique(land_use_change$TIME_STEP)[order(unique(land_use_change$TIME_STEP))])
{
  land_use_sub <- land_use_change %>% dplyr::filter(TIME_STEP == ts)     
  sbkey <- unique(land_use_sub$SUBKEY)
  
  for (subplt in sbkey)
  {
    lnd_use <- land_use_sub %>% dplyr::filter(SUBKEY == subplt)
    ecoProv <- WI_PLOT %>% dplyr::filter(KEY == unique(lnd_use$KEY)) %>% 
      dplyr::select(ECO_PROVINCE) %>% unique()
    ecoProv <- ecoProv[[1]]
    lnd_use <- lnd_use %>% mutate(ecoProv = ecoProv)
    
    lnd_use <- lnd_use %>% rowwise() %>%
      mutate(AGECLASS = ageClass(ecoProv, Name, DIA_ACTION),
             TREENUM = round((TPA_UNADJ * 4 / 4046.86) * siteSize))
    
    lnd_remove <- lnd_use %>% dplyr::filter(ACTION == 'remove') %>% 
      group_by(Name, AGECLASS) %>% summarise(TREESUM = sum(TREENUM, na.rm=T), .groups='drop') %>%
      mutate(ICSTRING = paste0(' ', AGECLASS, ' (', TREESUM, ')'))
    
    lnd_plant <- lnd_use %>% dplyr::filter(ACTION == 'plant') %>%
      mutate(AGECLASS = 1) %>% 
      group_by(Name, AGECLASS) %>% summarise(TREESUM = sum(TREENUM, na.rm=T), .groups='drop') %>%
      mutate(ICSTRING = paste0(' ', Name, ' (', TREESUM, ')'))
    
    cat(paste0('LandUse  "LandUse', luMapCode, '"'), file = outFile, sep='\n')
    cat(paste0('MapCode  ', luMapCode), file = outFile, sep='\n')
    cat('AllowHarvest?    yes', file = outFile, sep='\n')
    cat('LandCoverChange RemoveDensity', file = outFile, sep='\n')
    if (nrow(lnd_remove > 0))
    {
      remove_OUT <- lnd_remove %>% group_by(Name) %>%
        summarise(ICSTRING = paste(unique(Name), paste0(unique(ICSTRING) , collapse = ''), collapse = ''), .groups='drop')
      cat(remove_OUT$ICSTRING, file = outFile, sep='\n')
      
    } else{
      cat('acersacc  10 (0)', file = outFile, sep='\n')
    }
    
    if (nrow(lnd_plant > 0))
    {
      cat('Plant', lnd_plant$ICSTRING, file=outFile, sep='\t')
      cat('', file=outFile, sep='\n')
    }
    
    cat('', '>>------------------------------------', file = outFile, sep='\n')
    
    luKEY <- bind_rows(luKEY, tibble(SUBKEY = subplt, LUMAPCODE = luMapCode, TIMESTEP = ts))
    luMapCode <- luMapCode + 1
  }
}

close(outFile)
write_csv(luKEY, 'simulations/s3_s4/output/landuse_key.csv') ################# update output
#'
#' Create land-use maps
#' 
MV_KEY <- read_csv('simulations/s3_s4/output/MAPVALUE_KEY.csv') ###############update subplot
MV_KEY <- MV_KEY %>% rowwise() %>% mutate(KEY = paste(str_split(PLT_KEY, '_', simplify=T)[,1:3], collapse='_'))
MV_KEY <- MV_KEY %>% mutate(SUBKEY = str_c(KEY, '_', str_split(PLT_KEY, '_', simplify=T)[,5]))
#'
mapKey <- luKEY %>% left_join(MV_KEY, by = c('SUBKEY'))
#"
# 4. Create Land Use maps ----
#' 
library(raster)
#' 
#' Loop through individual time-steps to create maps
#' 
#' Update with raster objects sizes according to how many subplots are being used
#' 
#lu_map <- raster(ncol=80, nrow=80, xmn=0,xmx=1037.6,ymn=0,ymx=1037.6,vals=0) #create empty map
lu_map <- raster(ncol=100, nrow=100, xmn=0,xmx=1297,ymn=0,ymx=1297,vals=0) #create empty map for the pooled s1_s2 or s3_S4
#lu_map <- raster(ncol=117, nrow=117, xmn=0,xmx=1517.49,ymn=0,ymx=1517.49,vals=0) #create empty map for the pooled s1_s2_s3
#"
for (ts in 0:20)
{
  writeRaster(lu_map, paste0("simulations/s3_s4/landuse-", ts, ".img"), NAflag=-9999, overwrite=T, datatype='INT2S') ################## update output
}

for (ts in unique(mapKey$TIMESTEP)[order(unique(mapKey$TIMESTEP))])
{
  sub_lu <- mapKey %>% dplyr::filter(TIMESTEP == ts)
  sub_map <- lu_map 
  sub_map[sub_lu$MAPVALUE] <- sub_lu$LUMAPCODE
  
  writeRaster(sub_map, paste0("simulations/s3_s4/landuse-", ts, ".img"), NAflag=-9999, overwrite=T, datatype='INT2S') ############ update output location
}

#
