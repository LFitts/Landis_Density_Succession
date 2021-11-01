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
WI_PLOT<-read.csv("main_WI_2020/WI_PLOT.csv")#read the plot table
WI_TREE <- WI_TREE %>% filter(INVYR >= 2000 & STATUSCD==1)
#
WI_TREE <- WI_TREE %>% mutate(TREE_CN = as.character(CN), PLT_CN = as.character(PLT_CN), PREV_TRE_CN = as.character(PREV_TRE_CN))
WI_PLOT <- WI_PLOT %>% mutate(PLT_CN = as.character(CN), PREV_PLT_CN = as.character(PREV_PLT_CN))
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
#' Read in the list of subplots run for the initial communities
#' Read in the plot list that meet the F/M condition & at least 3 remeasurements
plt_list <- read.csv('code/WI_PLOT_FILTERED.csv')
plt_list <- plt_list %>% mutate(SUBKEY = str_c(KEY, str_sub(subplot_list, 2, 2), sep='_'))
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
#' Select variables of interest (this will be used to merge with the tree table and filter the correct observations to compare)
#' 
plt_list<-plt_list %>% select(SUBKEY, Time, tf)
names(plt_list)[3]<-"INVYR" #rename tf
#'
#' (create SUBKEY in tree table)
WI_TREE1 <- WI_TREE %>% mutate(SUBKEY = str_c(STATECD, COUNTYCD, PLOT, SUBP, sep='_')) %>% 
  mutate(KEY=str_c(STATECD, COUNTYCD, PLOT, sep='_'))%>%
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
WI_TREE1<-WI_TREE1 %>% select(KEY,SUBKEY, INVYR, SPCD, DIA, TPA_UNADJ)%>%
  mutate(DIA_cm = DIA*2.54, #transform inches to cm
         TreeNumber=round((TPA_UNADJ * 4 / 4046.86) * 169),#create a column with number of trees. 169 is the rounded subplot area in square meters. Convert the TPA_UNADJ to fit the cell size
         BA_m2=(pi*((DIA/2)*0.0254)^2*TreeNumber))#, #unit: sq meters. The tree number is already reflecting the scale (cell size)
# DRYBIO_AG=DRYBIO_AG*(2.47105/2204.64)) #Biomass aboveground per hectacre (1 hectare = 2.47105 acres; 1 Mg = 2204.64 pounds) --> Units: Mg/ha)
#'mutate(BA_PA=0.005454154*(DIA^2)*TPA_UNADJ) 
#'BA.PH= (sum(BA_PA, na.rm=T)*(2.47105/10.7639))) #Basal area per hectare m^2/ha (1 square meter = 10.7639 square feet; 1 hectare = 2.47105 acres)
#'
#' Merge with plot list and species codes
#' 
WI_TREE1<-merge(WI_TREE1,species_codes, by="SPCD")
#'
WI_TREE1<-merge(WI_TREE1,plt_list, by=c("SUBKEY", "INVYR"))
#' 
#'
#' Create a column indicating that these observations are from FIA
#' 
WI_TREE1$Source<-"FIA"
#'
#' Now add the ecoregion to each observation
#' 
WI_PLOT <- WI_PLOT %>%  dplyr::select(PLT_CN, INVYR, STATECD, COUNTYCD, PLOT, ELEV, ECOSUBCD, CYCLE) %>%
  mutate(ECO_PROVINCE = str_replace(str_sub(ECOSUBCD, 1, -2), ' ', ''), 
         KEY = str_c(STATECD, COUNTYCD, PLOT, sep='_'))%>%
          select(KEY, ECO_PROVINCE)
#'
WI_PLOT<-unique(WI_PLOT)#unique values for plot and ecoregion
#'
WI_TREE1<-merge(WI_TREE1,WI_PLOT, all.x=T, by=("KEY"))
#'
#' Select variables of interest
#' 
FIA_DB<-WI_TREE1 %>% select(SUBKEY, Time, Species, ECO_PROVINCE, TreeNumber, DIA_cm, BA_m2, Source)
#'
#' Remove any white spaces
FIA_DB<-FIA_DB %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))
#'
#'
#' #################################################
# Prepare the LANDIS data for validation ####
#'
#Read in LANDIS-II density log
#'
density<-read.csv("simulations/s2/Density_cohort_log_s2.CSV")
#'
#' Read the initial communities map codes (will be the SUBKEY)
#' 
map_codes<-read.csv("simulations/s2/output/MAPVALUE_KEY.CSV")
#' Get a SUBKEY removing INVYR
p<-str_split(map_codes$PLT_KEY, '_', simplify=T)
map_codes <- map_codes %>% mutate(SUBKEY = str_c(p[,1],'_',p[,2],'_', p[,3],'_',p[,5]))
#'
#' Merge the density file with the map codes
#' 
density<-merge(density, map_codes, by.y=("MAPVALUE"), by.x=("SiteIndex"))
#'
density<-density %>% select (SUBKEY, Time, Species, ECO_PROVINCE, TreeNumber, Diameter)
#'
names(density) <-c("SUBKEY", "Time", "Species", "ECO_PROVINCE", "TreeNumber", "DIA_cm") #rename DB to match WI_TREE
#'
#' Now create the basal area variable
#' 
density<-density %>% mutate(BA_m2=(pi*((DIA_cm/2)*0.01)^2*TreeNumber)) #basal area in sq meters
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
#' Remove extra space from species column
#install.packages("stringr")
#library(stringr)
densityL<-densityL %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))
#'
#'  #################################################
# Merge the LANDIS and FIA datasets ####
#'
#' 
s2<-rbind(FIA_DB, densityL)
#' 
#' Obtain the dataset for the first set of equivalence tests (ecoregion)
s2_ecoreg<-s2%>% group_by(Source,ECO_PROVINCE, SUBKEY)%>%
  summarize(meanDIA=sum(DIA_cm*TreeNumber, na.rm=T)/sum(TreeNumber, na.rm=T),
            totalBA=sum(BA_m2, na.rm=T),
            density=sum(TreeNumber, na.rm=T))
#'
#' Obtain the dataset for the second set of equivalence tests (species)
s2_species_SUB<-s2%>% group_by(Source, Species, SUBKEY)%>%
  summarize(meanDIA=sum(DIA_cm*TreeNumber, na.rm=T)/sum(TreeNumber, na.rm = T),
            totalBA=sum(BA_m2, na.rm=T),
            density=sum(TreeNumber, na.rm=T))
#'
s2_species_ECO<-s2%>% group_by(Source, ECO_PROVINCE, Species)%>%
  summarize(meanDIA=sum(DIA_cm*TreeNumber, na.rm=T)/sum(TreeNumber, na.rm = T),
            totalBA=sum(BA_m2, na.rm=T),
            density=sum(TreeNumber, na.rm=T))
#'
#' Convert the dataset into a wide format
#' 
s2_ecoreg_wide<-s2_ecoreg%>% pivot_wider(id_cols=c(ECO_PROVINCE, SUBKEY), names_from=Source, values_from=c(meanDIA, totalBA, density))
#'
s2_species_wide_eco<-s2_species_ECO%>% pivot_wider(id_cols=c(ECO_PROVINCE, Species), names_from=Source, values_from=c(meanDIA, totalBA, density))
#'
s2_species_wide_SUB<-s2_species_SUB%>% pivot_wider(id_cols=c(Species, SUBKEY), names_from=Source, values_from=c(meanDIA, totalBA, density))
#'
#' Remove rows with missing values
#'
s2_ecoreg_wide<-s2_ecoreg_wide[complete.cases(s2_ecoreg_wide),]
#'
s2_species_wide_eco<-s2_species_wide_eco[complete.cases(s2_species_wide_eco),]
#'
s2_species_wide_SUB<-s2_species_wide_SUB[complete.cases(s2_species_wide_SUB),]
#'
# Equivalence test ecoregion mean diameter ####
#install.packages("lattice")
library(lattice)
#'
ecoreg_vec<-unique(s2_ecoreg_wide$ECO_PROVINCE)
tost_eco<-list()
result<-data.frame()
tost_eco_result<-data.frame()
#'
for(i in 1:length(ecoreg_vec)){
tempkey=ecoreg_vec[i]
ecoregion<-s2_ecoreg_wide %>% filter(ECO_PROVINCE==tempkey)
#

my.plot<-equivalence.xyplot(ecoregion$meanDIA_FIA ~ ecoregion$meanDIA_LANDIS,
                                      alpha=0.05, b0.ii=0.05, b1.ii=0.2,
                                      xlab="Projected mean diameter (cm)",
                                      ylab="Observed mean diameter (cm)")
#
 trellis.device(device="tiff", filename=paste("xyplot",tempkey,"meanDIA",".tiff", sep=""))
 print(my.plot)
 dev.off() #save the plot files
#
ecoregion_tost<-tost(ecoregion$meanDIA_FIA, ecoregion$meanDIA_LANDIS, epsilon = 3)
result<-as.data.frame(ecoregion_tost$result)
result$ECO_PROVINCE<-tempkey
#
tost_eco<-rbind(ecoregion_tost,tost_eco)
tost_eco_result<-rbind(result,tost_eco_result)
}
#'
#' For all together
#equivalence.xyplot(s2_ecoreg_wide$meanDIA_FIA ~ s2_ecoreg_wide$meanDIA_LANDIS,
#                   alpha=0.05, b0.ii=0.05, b1.ii=0.2,
#                   xlab="Projected mean diameter (cm)",
#                   ylab="Observed mean diameter (cm)")

#two-one-sided test (Results indicate obsht and predht2 are  equivalent, b/c dissimilarity is rejected)
#s2_ecoreg_meanDIA<-tost(s2_ecoreg_wide$meanDIA_FIA, s2_ecoreg_wide$meanDIA_LANDIS, epsilon = 3)
#s2_ecoreg_meanDIA$result #not equivalent because dissimilarity is not rejected
#'
# Equivalence test ecoregion density ####
library(lattice)
#'
ecoreg_vec<-unique(s2_ecoreg_wide$ECO_PROVINCE)
tost_eco_D<-list()
result_D<-data.frame()
tost_eco_result_D<-data.frame()
#'
for(i in 1:length(ecoreg_vec)){
  tempkey=ecoreg_vec[i]
  ecoregion<-s2_ecoreg_wide %>% filter(ECO_PROVINCE==tempkey)
  #
  
  my.plot<-equivalence.xyplot(ecoregion$density_FIA ~ ecoregion$density_LANDIS,
                              alpha=0.05, b0.ii=0.05, b1.ii=0.2,
                              xlab="Projected number of trees",
                              ylab="Observed number of trees")
  #
  trellis.device(device="tiff", filename=paste("xyplot",tempkey,"density",".tiff", sep=""))
  print(my.plot)
  dev.off() #save the plot files
  #
  ecoregion_tost_D<-tost(ecoregion$density_FIA, ecoregion$density_LANDIS, epsilon = 3)
  result_D<-as.data.frame(ecoregion_tost_D$result)
  result_D$ECO_PROVINCE<-tempkey
  #
  tost_eco_D<-rbind(ecoregion_tost_D,tost_eco_D)
  tost_eco_result_D<-rbind(result_D,tost_eco_result_D)
}
#'
#'
# Equivalence test ecoregion total basal area ####
library(lattice)
#'
ecoreg_vec<-unique(s2_ecoreg_wide$ECO_PROVINCE)
tost_eco_BA<-list()
result_BA<-data.frame()
tost_eco_result_BA<-data.frame()
#'
for(i in 1:length(ecoreg_vec)){
  tempkey=ecoreg_vec[i]
  ecoregion<-s2_ecoreg_wide %>% filter(ECO_PROVINCE==tempkey)
  #
  
  my.plot<-equivalence.xyplot(ecoregion$totalBA_FIA ~ ecoregion$totalBA_LANDIS,
                              alpha=0.05, b0.ii=0.05, b1.ii=0.2,
                              xlab="Projected Basal area (m^2)",
                              ylab="Observed Basal area (m^2)")
  #
  trellis.device(device="tiff", filename=paste("xyplot",tempkey,"totalBA",".tiff", sep=""))
  print(my.plot)
  dev.off() #save the plot files
  #
  ecoregion_tost_BA<-tost(ecoregion$totalBA_FIA, ecoregion$totalBA_LANDIS, epsilon = 3)
  result_BA<-as.data.frame(ecoregion_tost$result)
  result_BA$ECO_PROVINCE<-tempkey
  #
  tost_eco_BA<-rbind(ecoregion_tost_BA,tost_eco_BA)
  tost_eco_result_BA<-rbind(result_BA,tost_eco_result_BA)
}
#'
#'
#' ##########################################################
#' SPECIES
#' 
# Equivalence test SPECIES mean diameter ####
#install.packages("lattice")
library(lattice)
#'
species_vec<-unique(s2_species_wide_SUB$Species)
tost_sp<-list()
result_sp<-data.frame()
tost_sp_result<-data.frame()
#'
for(i in 1:length(species_vec)){
  tempkey=species_vec[i]
  species<-s2_species_wide_SUB %>% filter(Species==tempkey)
  #
  
  my.plot<-equivalence.xyplot(species$meanDIA_FIA ~ species$meanDIA_LANDIS,
                              alpha=0.05, b0.ii=0.05, b1.ii=0.2,
                              xlab="Projected mean diameter (cm)",
                              ylab="Observed mean diameter (cm)")
  #
  trellis.device(device="tiff", filename=paste("xyplot",tempkey,"meanDIA",".tiff", sep=""))
  print(my.plot)
  dev.off() #save the plot files
  #
  species_tost<-tost(species$meanDIA_FIA, species$meanDIA_LANDIS, epsilon = 3)
  result_sp<-as.data.frame(species_tost$result)
  result_sp$Species<-tempkey
  #
  tost_sp<-rbind(species_tost,tost_sp)
  tost_sp_result<-rbind(result_sp,tost_sp_result)
}
#'
#write.csv(tost_sp_result,'E:/Landis_Density_Succession/simulations/s2/results/tost_sp_dia_result.CSV')
#'
# Equivalence test SPECIES density ####
library(lattice)
#'
species_vec<-unique(s2_species_wide_SUB$Species)
tost_sp_d<-list()
result_sp_d<-data.frame()
tost_sp_result_d<-data.frame()
#'
for(i in 1:length(species_vec)){
  tempkey=species_vec[i]
  species<-s2_species_wide_SUB %>% filter(Species==tempkey)
  #
  
  my.plot<-equivalence.xyplot(species$density_FIA ~ species$density_LANDIS,
                              alpha=0.05, b0.ii=0.05, b1.ii=0.2,
                              xlab="Projected number of trees (cm)",
                              ylab="Observed number of trees (cm)")
  #
  trellis.device(device="tiff", filename=paste("xyplot",tempkey,"density",".tiff", sep=""))
  print(my.plot)
  dev.off() #save the plot files
  #
  species_tost_d<-tost(species$density_FIA, species$density_LANDIS, epsilon = 3)
  result_sp_d<-as.data.frame(species_tost_d$result)
  result_sp_d$Species<-tempkey
  #
  tost_sp_d<-rbind(species_tost_d,tost_sp_d)
  tost_sp_result_d<-rbind(result_sp_d,tost_sp_result_d)
}
#write.csv(tost_sp_result_d,'E:/Landis_Density_Succession/simulations/s2/results/tost_sp_density_result.CSV')
#'
#'
# Equivalence test SPECIES total basal area ####
library(lattice)
#'
species_vec<-unique(s2_species_wide_SUB$Species)
tost_sp_ba<-list()
result_sp_ba<-data.frame()
tost_sp_result_ba<-data.frame()
#'
for(i in 1:length(species_vec)){
  tempkey=species_vec[i]
  species<-s2_species_wide_SUB %>% filter(Species==tempkey)
  #
  
  my.plot<-equivalence.xyplot(species$totalBA_FIA ~ species$totalBA_LANDIS,
                              alpha=0.05, b0.ii=0.05, b1.ii=0.2,
                              xlab="Projected total basal area (m^2)",
                              ylab="Observed total basal area (m^2)")
  #
  trellis.device(device="tiff", filename=paste("xyplot",tempkey,"totalBA",".tiff", sep=""))
  print(my.plot)
  dev.off() #save the plot files
  #
  species_tost_ba<-tost(species$totalBA_FIA, species$totalBA_LANDIS, epsilon = 3)
  result_sp_ba<-as.data.frame(species_tost_ba$result)
  result_sp_ba$Species<-tempkey
  #
  tost_sp_ba<-rbind(species_tost_ba,tost_sp_ba)
  tost_sp_result_ba<-rbind(result_sp_ba,tost_sp_result_ba)
}
#'
#write.csv(tost_sp_result_ba,'E:/Landis_Density_Succession/simulations/s2/results/tost_sp_ba_result.CSV')
#'
