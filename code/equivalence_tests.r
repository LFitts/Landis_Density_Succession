#install.packages("equivalence")
library("equivalence")
library(tidyverse)

#Example 1. Observed and predicted tree heights (obsht and predht)
obsht<-c(11,12,13,14,15,16,17,18,19,20)
predht<-c(12,19,17,14,16,19,17,23,24,12)
 
tree=data.frame(obsht=obsht,predht=predht)

equivalence.xyplot(tree$obsht ~ tree$predht,
  alpha=0.05, b0.ii=0.05, b1.ii=0.2,
  xlab="Predicted height (m)",
  ylab="Measured height (m)")

#two-one-sided test (Results indicate obsht and predht are not equivalent, b/c dissimilarity is not rejected) 
m1<-tost(tree$predht, tree$obsht, epsilon = 3)
m1$result


#Example 2. Observed and predicted tree heights from a different model (obsht and predht2)
predht2<-c(11,12,11,14,18,16,17,18,18,20)
tree2=data.frame(obsht=obsht,predht2=predht2)

equivalence.xyplot(tree2$obsht ~ tree2$predht2,
  alpha=0.05, b0.ii=0.05, b1.ii=0.2,
  xlab="Predicted height (m)",
  ylab="Measured height (m)")

#two-one-sided test (Results indicate obsht and predht2 are  equivalent, b/c dissimilarity is rejected)
m2<-tost(tree2$predht2, tree2$obsht, epsilon = 3)
m2$result
#'
#' #################################################
# Prepare the FIA table for validation ####
#'
WI_TREE<-read.csv("main_WI_2020/WI_TREE.csv")#read the tree table
#WI_PLOT<-read.csv("data/main_WI_2020/WI_PLOT.csv")#read the plot table
WI_TREE <- WI_TREE %>% filter(INVYR >= 2000)
#
WI_TREE <- WI_TREE %>% mutate(TREE_CN = as.character(CN), PLT_CN = as.character(PLT_CN), PREV_TRE_CN = as.character(PREV_TRE_CN))
WI_PLOT <- WI_PLOT %>% mutate(PLT_CN = as.character(CN), PREV_PLT_CN = as.character(PREV_PLT_CN))
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
#' Read in the list of subplots run for the initial communities
plt_list <- read_csv('data/WI_PLOT_LIST_updated.CSV')
plt_list <- plt_list %>% mutate(SUBKEY = str_c(KEY, str_sub(subplot_list, 1, 1), sep='_'))
#'
#' Create a column for difference in time (tf-to)/time step in LANDIS-II
#' 
plt_list$Time<-ifelse(plt_list$t3!=0,plt_list$t3-plt_list$t0,
                      ifelse(plt_list$t2!=0,plt_list$t2-plt_list$t0, plt_list$t1-plt_list$t0))
#'
#' Now create the column tf (final remeasurement time)
#' 
plt_list$tf<-ifelse(plt_list$t3!=0,plt_list$t3,
                      ifelse(plt_list$t2!=0,plt_list$t2, plt_list$t1))
#'
#' Select variables of interest
#' 
plt_list<-plt_list %>% select(SUBKEY, Time, tf)
names(plt_list)[3]<-"INVYR" #rename tf
#'
#' (create SUBKEY in tree table)
WI_TREE <- WI_TREE %>% mutate(SUBKEY = str_c(STATECD, COUNTYCD, PLOT, SUBP, sep='_')) %>% 
  filter(SUBKEY %in% unique(plt_list$SUBKEY))
#"
#' Get the species list names
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
species_attributes$Species<-paste(species_attributes$name1,species_attributes$name2, sep="" )
#'
#' Create a subset of this table just showing the species name and codes (this will be merged with the rest of the tables)
#' 
species_codes<-species_attributes%>% select(SPCD, Species)
#'
ref<-species_codes
#'
#' Select variables of interest tree table
#' 
WI_TREE1<-WI_TREE %>% select(SUBKEY, INVYR, SPCD, DIA, TPA_UNADJ, DRYBIO_AG)%>%
  mutate(DIA_cm = DIA*2.54, #transform to cm
         TreeNumber=round((TPA_UNADJ * 4 / 4046.86) * 169),#create a column with number of trees. 169 is the rounded subplot area in square meters. Expressed in a per hectare unit
         DRYBIO_AG=DRYBIO_AG*(2.47105/2204.64)) #Biomass aboveground per hectacre (1 hectare = 2.47105 acres; 1 Mg = 2204.64 pounds) --> Units: Mg/ha)
#'
#' Merge with plot list and species codes
#' 
WI_TREE1<-merge(WI_TREE1,species_codes, by="SPCD")
#'
WI_TREE1<-merge(WI_TREE1,plt_list, by=c("SUBKEY", "INVYR"))
#' 
#' Remove trees with dry biomass of zero (which also don't have a diameter measured)
#'
WI_TREE1<-WI_TREE1 %>% filter(DRYBIO_AG!=0)
#'
#' Select variables of interest
#' 
WI_TREE1<-WI_TREE1 %>% select(SUBKEY, Time, Species, DIA_cm, TreeNumber)
#'
#' Create a column indicating that these observations are from FIA
#' 
WI_TREE1$Source<-"FIA"
#' #################################################
# Prepare the LANDIS data for validation ####
#'
#Read in LANDIS-II density log
#'
density<-read.csv("all_txt/Density_cohort_log_20yrsND.CSV")
#'
#' Read the initial communities map codes (will be the SUBKEY)
#' 
map_codes<-read.csv("output/MAPVALUE_KEY.CSV")
#' Get a SUBKEY removing INVYR
p<-str_split(map_codes$PLT_KEY, '_', simplify=T)
map_codes <- map_codes %>% mutate(SUBKEY = str_c(p[,1],'_',p[,2],'_', p[,3],'_',p[,5]))
#'
#' Merge the density file with the map codes
#' 
density<-merge(density, map_codes, by.y=("MAPVALUE"), by.x=("SiteIndex"))
#'
density<-density %>% select (SUBKEY, Time, Species, Diameter, TreeNumber)
#'
names(density) <-c("SUBKEY", "Time", "Species", "DIA_cm", "TreeNumber") #rename DB to match WI_TREE
#'
#' Now filter only tf in FIA
#' 
tf<-unique(WI_TREE1%>% select(SUBKEY,Time))
#'
densityL<-merge(density, tf, by=c("SUBKEY","Time"))
#'
#' Create the source column
#' 
densityL$Source <-"LANDIS"
#' 
