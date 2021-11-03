#install.packages("equivalence")
#library("equivalence")
library(tidyverse)
#install.packages("TOSTER")
library(TOSTER)

#' #################################################
# Prepare the FIA table for validation ####
#'
WI_TREE<-read_csv("data/main_WI_2020/WI_TREE.csv")#read the tree table
WI_PLOT<-read_csv("data/main_WI_2020/WI_PLOT.csv")#read the plot table
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
#plt_list <- plt_list %>% mutate(SUBKEY = str_c(KEY, str_sub(subplot_list, 2, 2), sep='_'))
#'
plt_list1 <- plt_list %>% mutate(SUBKEY = str_c(KEY, str_sub(subplot_list, 1, 1), sep='_'))
plt_list2 <- plt_list %>% mutate(SUBKEY = str_c(KEY, str_sub(subplot_list, 2, 2), sep='_'))
plt_list <- rbind(plt_list1, plt_list2)
#'
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
plt_list<-plt_list %>% dplyr::select(SUBKEY, Time, tf)
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
species_codes<-species_attributes%>% dplyr::select(SPCD, Species)
#'
ref<-species_codes
#'
#' Select variables of interest tree table
#' 
WI_TREE1<-WI_TREE1 %>% dplyr::select(KEY,SUBKEY, INVYR, SPCD, DIA, TPA_UNADJ)%>%
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
        dplyr::select(KEY, ECO_PROVINCE)
#'
WI_PLOT<-unique(WI_PLOT)#unique values for plot and ecoregion
#'
WI_TREE1<-merge(WI_TREE1,WI_PLOT, all.x=T, by=("KEY"))
#'
#' Select variables of interest
#' 
FIA_DB<-WI_TREE1 %>% dplyr::select(SUBKEY, Time, Species, ECO_PROVINCE, TreeNumber, DIA_cm, BA_m2, Source)
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
density<-density %>% dplyr::select(SUBKEY, Time, Species, ECO_PROVINCE, TreeNumber, Diameter)
#'
names(density) <-c("SUBKEY", "Time", "Species", "ECO_PROVINCE", "TreeNumber", "DIA_cm") #rename DB to match WI_TREE
#'
#' Now create the basal area variable
#' 
density<-density %>% mutate(BA_m2=(pi*((DIA_cm/2)*0.01)^2*TreeNumber)) #basal area in sq meters
#'
#' Now filter only tf in FIA
#' 
tf<-unique(WI_TREE1%>% dplyr::select(SUBKEY,Time))
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
#'
ecoreg_vec<-unique(s2_ecoreg_wide$ECO_PROVINCE)

eco_result_DIA <- tibble()
tost_eco_result_DIA <- list()
#'
for(i in 1:length(ecoreg_vec)){
  tempkey=ecoreg_vec[i]
  ecoregion<-s2_ecoreg_wide %>% filter(ECO_PROVINCE==tempkey)
  #
  #
  tostTest <- dataTOSTpaired(ecoregion, pairs = list(c(i1='meanDIA_LANDIS', i2='meanDIA_FIA')),
                             low_eqbound = -0.5, high_eqbound = 0.5, desc=T, plots=T)
  
  result <- tibble(ECO_PROVINCE = tempkey,
                    EPSILON = 0.5,
                    p0 = tostTest$tost$asDF$`p[0]`,
                    p1 = tostTest$tost$asDF$`p[1]`,
                    p2 = tostTest$tost$asDF$`p[2]`,
                   LANDIS_MEAN = tostTest$desc$asDF$`m[1]`,
                   FIA_MEAN = tostTest$desc$asDF$`m[2]`,
                   MEAN_BIAS = mean(ecoregion$meanDIA_FIA-ecoregion$meanDIA_LANDIS),
                   SD_BIAS = sd(ecoregion$meanDIA_FIA-ecoregion$meanDIA_LANDIS),
                   N= length(ecoregion$meanDIA_FIA)) %>% 
                  mutate(NULLHYP = if_else((p1 < 0.05) & (p2 < 0.05), 'REJECTED', 'NOT REJECTED'))
  
  eco_result_DIA <- eco_result_DIA %>% bind_rows(result)

  tost_eco_result_DIA[[tempkey]] <- tostTest
}
#write.csv(eco_result_DIA,'C:/Users/fitts010/Desktop/Landis_Density_Succession/simulations/s2/results/tost_eco_dia_result0.5.CSV')
#'
# Equivalence test ecoregion density ####
#'
ecoreg_vec<-unique(s2_ecoreg_wide$ECO_PROVINCE)

eco_result_DENS <- tibble()
tost_eco_result_DENS <- list()
#'
for(i in 1:length(ecoreg_vec)){
  tempkey=ecoreg_vec[i]
  ecoregion<-s2_ecoreg_wide %>% filter(ECO_PROVINCE==tempkey)
  #

  tostTest <- dataTOSTpaired(ecoregion, pairs = list(c(i1='density_LANDIS', i2='density_FIA')),
                             low_eqbound = -0.5, high_eqbound = 0.5, desc=T, plots=T)
  
  result <- tibble(ECO_PROVINCE = tempkey,
                   EPSILON = 0.5,
                   p0 = tostTest$tost$asDF$`p[0]`,
                   p1 = tostTest$tost$asDF$`p[1]`,
                   p2 = tostTest$tost$asDF$`p[2]`,
                   LANDIS_MEAN = tostTest$desc$asDF$`m[1]`,
                   FIA_MEAN = tostTest$desc$asDF$`m[2]`, 
                   MEAN_BIAS = mean(ecoregion$density_FIA-ecoregion$density_LANDIS),
                   SD_BIAS = sd(ecoregion$density_FIA-ecoregion$density_LANDIS),
                   N= length(ecoregion$density_FIA)) %>% 
                   mutate(NULLHYP = if_else((p1 < 0.05) & (p2 < 0.05), 'REJECTED', 'NOT REJECTED'))

  eco_result_DENS <- eco_result_DENS %>% bind_rows(result)
  
  tost_eco_result_DENS[[tempkey]] <- tostTest
}
#'
#write.csv(eco_result_DENS,'C:/Users/fitts010/Desktop/Landis_Density_Succession/simulations/s2/results/tost_eco_den_result0.5.CSV')
#'
#'
# Equivalence test ecoregion total basal area ####
#'
ecoreg_vec<-unique(s2_ecoreg_wide$ECO_PROVINCE)

eco_result_BA <- tibble()
tost_eco_result_BA <- list()
#'
for(i in 1:length(ecoreg_vec)){
  tempkey=ecoreg_vec[i]
  ecoregion<-s2_ecoreg_wide %>% filter(ECO_PROVINCE==tempkey)
  #
  #
  tostTest <- dataTOSTpaired(ecoregion, pairs = list(c(i1='totalBA_LANDIS', i2='totalBA_FIA')),
                             low_eqbound = -0.5, high_eqbound = 0.5, desc=T, plots=T)
  
  result <- tibble(ECO_PROVINCE = tempkey,
                   EPSILON = 0.5,
                   p0 = tostTest$tost$asDF$`p[0]`,
                   p1 = tostTest$tost$asDF$`p[1]`,
                   p2 = tostTest$tost$asDF$`p[2]`,
                   LANDIS_MEAN = tostTest$desc$asDF$`m[1]`,
                   FIA_MEAN = tostTest$desc$asDF$`m[2]`, 
                   MEAN_BIAS = mean(ecoregion$totalBA_FIA-ecoregion$totalBA_LANDIS),
                   SD_BIAS = sd(ecoregion$totalBA_FIA-ecoregion$totalBA_LANDIS),
                   N= length(ecoregion$totalBA_FIA)) %>% 
                   mutate(NULLHYP = if_else((p1 < 0.05) & (p2 < 0.05), 'REJECTED', 'NOT REJECTED'))

  eco_result_BA <- eco_result_BA %>% bind_rows(result)
  
  tost_eco_result_BA[[tempkey]] <- tostTest
}
#'
#write.csv(eco_result_BA,'C:/Users/fitts010/Desktop/Landis_Density_Succession/simulations/s2/results/tost_eco_BA_result0.5.CSV')
#'
#'
#' ##########################################################
#' SPECIES
#' 
# Equivalence test SPECIES mean diameter ####
#'
species_vec<-unique(s2_species_wide_SUB$Species)

sp_result_DIA <- tibble()
tost_sp_result_DIA <- list()
#'
for(i in 1:length(species_vec)){
  tempkey=species_vec[i]
  species<-s2_species_wide_SUB %>% filter(Species==tempkey)
  #
  tostTest <- dataTOSTpaired(species, pairs = list(c(i1='meanDIA_LANDIS', i2='meanDIA_FIA')),
                             low_eqbound = -0.5, high_eqbound = 0.5, desc=T, plots=T)
  
  result <- tibble(ECO_PROVINCE = tempkey,
                   EPSILON = 0.5,
                   p0 = tostTest$tost$asDF$`p[0]`,
                   p1 = tostTest$tost$asDF$`p[1]`,
                   p2 = tostTest$tost$asDF$`p[2]`,
                   LANDIS_MEAN = tostTest$desc$asDF$`m[1]`,
                   FIA_MEAN = tostTest$desc$asDF$`m[2]`, 
                   MEAN_BIAS = mean(species$meanDIA_FIA-species$meanDIA_LANDIS),
                   SD_BIAS = sd(species$meanDIA_FIA-species$meanDIA_LANDIS),
                   N= length(species$meanDIA_FIA)) %>% 
    mutate(NULLHYP = if_else((p1 < 0.05) & (p2 < 0.05), 'REJECTED', 'NOT REJECTED'))
  
  sp_result_DIA <-  sp_result_DIA %>% bind_rows(result)
  
  tost_eco_result_BA[[tempkey]] <- tostTest
}

#'
#write.csv( sp_result_DIA,'C:/Users/fitts010/Desktop/Landis_Density_Succession/simulations/s2/results/tost_sp_dia_result0.5.CSV')
#'
# Equivalence test SPECIES density ####
#'
species_vec<-unique(s2_species_wide_SUB$Species)
sp_result_D <- tibble()
tost_sp_result_D <- list()
#'
for(i in 1:length(species_vec)){
  tempkey=species_vec[i]
  species<-s2_species_wide_SUB %>% filter(Species==tempkey)
  #
  
  tostTest <- dataTOSTpaired(species, pairs = list(c(i1='density_LANDIS', i2='density_FIA')),
                             low_eqbound = -0.5, high_eqbound = 0.5, desc=T, plots=T)
  
  result <- tibble(ECO_PROVINCE = tempkey,
                   EPSILON = 0.5,
                   p0 = tostTest$tost$asDF$`p[0]`,
                   p1 = tostTest$tost$asDF$`p[1]`,
                   p2 = tostTest$tost$asDF$`p[2]`,
                   LANDIS_MEAN = tostTest$desc$asDF$`m[1]`,
                   FIA_MEAN = tostTest$desc$asDF$`m[2]`, 
                   MEAN_BIAS = mean(species$density_FIA-species$density_LANDIS),
                   SD_BIAS = sd(species$density_FIA-species$density_LANDIS),
                   N= length(species$density_FIA)) %>% 
    mutate(NULLHYP = if_else((p1 < 0.05) & (p2 < 0.05), 'REJECTED', 'NOT REJECTED'))
  
  sp_result_D <- sp_result_D %>% bind_rows(result)
  
  tost_sp_result_D[[tempkey]] <- tostTest
}
#write.csv(sp_result_D,'C:/Users/fitts010/Desktop/Landis_Density_Succession/simulations/s2/results/tost_sp_density_result0.5.CSV')
#'
#'
# Equivalence test SPECIES total basal area ####
#'
species_vec<-unique(s2_species_wide_SUB$Species)
sp_result_BA <- tibble()
tost_sp_result_BA <- list()
#'
for(i in 1:length(species_vec)){
  tempkey=species_vec[i]
  species<-s2_species_wide_SUB %>% filter(Species==tempkey)
  #
  tostTest <- dataTOSTpaired(species, pairs = list(c(i1='totalBA_LANDIS', i2='totalBA_FIA')),
                             low_eqbound = -0.5, high_eqbound = 0.5, desc=T, plots=T)
  
  result <- tibble(ECO_PROVINCE = tempkey,
                   EPSILON = 0.5,
                   p0 = tostTest$tost$asDF$`p[0]`,
                   p1 = tostTest$tost$asDF$`p[1]`,
                   p2 = tostTest$tost$asDF$`p[2]`,
                   LANDIS_MEAN = tostTest$desc$asDF$`m[1]`,
                   FIA_MEAN = tostTest$desc$asDF$`m[2]`, 
                   MEAN_BIAS = mean(species$totalBA_FIA-species$totalBA_LANDIS),
                   SD_BIAS = sd(species$totalBA_FIA-species$totalBA_LANDIS),
                   N= length(species$totalBA_FIA)) %>% 
    mutate(NULLHYP = if_else((p1 < 0.05) & (p2 < 0.05), 'REJECTED', 'NOT REJECTED'))
  
  sp_result_BA <- sp_result_BA %>% bind_rows(result)
  
  tost_sp_result_BA[[tempkey]] <- tostTest
  }
#'
#write.csv( sp_result_BA,'C:/Users/fitts010/Desktop/Landis_Density_Succession/simulations/s2/results/tost_sp_ba_result0.5.CSV')
#'
