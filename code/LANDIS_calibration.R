#'
#' ##################################################################################
# 1. Check species response by ecological section ----
#'###################################################################################
#'
#' From FIA diameter growth estimates
#'
library(tidyverse)
s2_species_wide_SUB <- read_csv('./output/s2_species_wide_sub.csv')

speciesSet = c('abiebals', 'poputrem', 'pinuresi', 'thujocci', 'pinubank', 'prunsero',
               'picemari', 'queralba', 'ostrvirg', 'tsugcana')

speciesSumm <- s2_species_wide_SUB %>% group_by(Species) %>% 
  summarise(maxBA = max(totalBA_LANDIS, totalBA_FIA),
            maxDIA = max(meanDIA_LANDIS, meanDIA_FIA),
            maxDENS = max(density_LANDIS, density_FIA))

for (sp in speciesSet){
  spMaxBA <- speciesSumm %>% filter(Species == sp) %>% select(maxBA) %>% pull()
  ggplot(s2_species_wide_SUB %>% filter(Species == sp), aes(totalBA_FIA, totalBA_LANDIS)) +
    geom_point() +
    scale_x_continuous(limits = c(0, spMaxBA)) +
    scale_y_continuous(limits = c(0, spMaxBA)) +
    coord_fixed() +
    geom_abline(slope = 1, intercept = 0) +
    facet_wrap(vars(ECO_PROVINCE)) +
    theme_classic()
  ggsave(paste0('./output/CalibrationTest/', sp, '_BA.tiff'))
  
  spMaxDIA <- speciesSumm %>% filter(Species == sp) %>% select(maxDIA) %>% pull()
  ggplot(s2_species_wide_SUB %>% filter(Species == sp), aes(meanDIA_FIA, meanDIA_LANDIS)) +
    geom_point() +
    scale_x_continuous(limits = c(0, spMaxDIA)) +
    scale_y_continuous(limits = c(0, spMaxDIA)) +
    coord_fixed() +
    geom_abline(slope = 1, intercept = 0) +
    geom_vline(aes(xintercept = 12.7, colour = 'red')) +
    geom_hline(aes(yintercept = 12.7, colour = 'red')) +
    facet_wrap(vars(ECO_PROVINCE)) +
    theme_classic()
  ggsave(paste0('./output/CalibrationTest/', sp, '_DIA.tiff'))
  
  spMaxDENS <- speciesSumm %>% filter(Species == sp) %>% select(maxDENS) %>% pull()
  ggplot(s2_species_wide_SUB %>% filter(Species == sp), aes(density_FIA, density_LANDIS)) +
    geom_point() +
    scale_x_continuous(limits = c(0, spMaxDENS)) +
    scale_y_continuous(limits = c(0, spMaxDENS)) +
    coord_fixed() +
    geom_abline(slope = 1, intercept = 0) +
    facet_wrap(vars(ECO_PROVINCE)) +
    theme_classic()
  ggsave(paste0('./output/CalibrationTest/', sp, '_DENS.tiff'))
}

#'
#' ##################################################################################
# 2. Create table: Calibrate Species Ecoregion diameter table ----
#'###################################################################################
#'
#' From FIA diameter growth estimates
#' 
#install.packages("rFIA")
library(rFIA)
library(tidyverse)
#'###############################################################
#' Species code object
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
#' Now back to the ecoregion-diameter table
#' 
#' Point to directory containing FIA tables
#'
fiaDir <- 'C:/Users/fitts010/Desktop/ch3_paper/Landis_Density_Succession/data/main_WI_2020'
#fiaDir <- 'D:/fia/rFIA'
#getFIA(states = "WI", dir = fiaDir, load = FALSE, nCores=3) #download the FIA tables for Wisconsin
#'
wiTB <- readFIA(fiaDir, states = c('WI'), tables=c("COND", "COND_DWM_CALC", "INVASIVE_SUBPLOT_SPP", "P2VEG_SUBP_STRUCTURE", "PLOT", "POP_ESTN_UNIT","POP_EVAL", "POP_EVAL_GRP", "POP_EVAL_TYP", "POP_PLOT_STRATUM_ASSGN", "POP_STRATUM", "SEEDLING", "SUBP_COND", "SUBP_COND_CHNG_MTRX", "SUBPLOT", "SURVEY", "TREE", "TREE_GRM_BEGIN", "TREE_GRM_COMPONENT", "TREE_GRM_MIDPT"), inMemory = T, nCores = 3)%>% clipFIA() #These are the minimum FIA tables that we need for this exercise
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

colnames(species_codes) = c('SPCD', 'Name')
#'
#' Create the empty data frames needed for the loop
#' 
#' 
#' Species with over-prediction of small diameter
smallDiaSlow <- c(12, 95,701, 746, 762) #added the spruce

#' Species with under-prediction of small diameter
smallDiaBoost <- c(241, 802) #added the oak

#' Species with over-prediction of large diameter
largeDiaSlow <- c(95, 105, 746)

#' Species with under-prediction of large diameter
largeDiaBoost <- c(802, 241, 261)

#' Set growth modifier
growMod <- 0.2 #this is what needs to be modified every time we calibrate with a different value
#' 
library(data.table)
#' Create the for loop
#' 
dia_list <- list()
for(i in 1:length(sp_eco_listWI)){
  #"
  
  age_dia<-data.frame()
  tempkey=sp_eco_listWI[i]
  ecoSec <- tempkey %>% str_split('_') %>% map_chr(., 1)
  spCD <- as.integer(tempkey %>% str_split('_') %>% map_chr(., 2))
  if (!(spCD %in% c(smallDiaSlow, smallDiaBoost, largeDiaSlow, largeDiaBoost))){
    next
  }
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
    growth <- diaGR[[1,1]]
    if (spCD %in% smallDiaSlow)
    {
      growth <- growth - (growth * growMod)  
    }
    else if (spCD %in% smallDiaBoost)
    {
      growth <- growth + (growth * growMod)  
    }
    
    smallGrowthMD <- bind_rows(smallGrowthMD, tibble(KEY=tempkey,AGE = age, DIA = max(smallGrowthMD$DIA) + growth))
    
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
    
    growth <- diaGR[[1,1]]
    if (spCD %in% largeDiaSlow)
    {
      growth <- growth - (growth * growMod)  
    }
    else if (spCD %in% largeDiaBoost)
    {
      growth <- growth + (growth * growMod)  
    }
    
    growthMD <- bind_rows(growthMD, tibble(KEY=tempkey,AGE = age, DIA = max(growthMD$DIA) + growth))
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
  print(tempkey)
  
}

print('End part 1')
ecoSp <- expand.grid(unique(wiTB$PLOT$ECO_PROVINCE), unique(c(smallDiaSlow, smallDiaBoost, largeDiaSlow, largeDiaBoost)), stringsAsFactors = F) %>% 
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
  ecoSec <- tempkey %>% str_split('_') %>% map_chr(., 1)
  
  spcd <- as.integer(str_split(tempkey, pattern = '_', simplify = T)[,2])
  if (!(spcd %in% c(smallDiaSlow, smallDiaBoost, largeDiaSlow, largeDiaBoost))){
    next
  }
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
    
    growth <- diaGR[[1,1]]
    if (spCD %in% smallDiaSlow)
    {
      growth <- growth - (growth * growMod)  
    }
    else if (spCD %in% smallDiaBoost)
    {
      growth <- growth + (growth * growMod)  
    }
    
    smallGrowthMD <- bind_rows(smallGrowthMD, tibble(KEY=msngEcoSP[i, 1],AGE = age, DIA = max(smallGrowthMD$DIA) + growth))
    
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
    
    growth <- diaGR[[1,1]]
    if (spCD %in% largeDiaSlow)
    {
      growth <- growth - (growth * growMod)  
    }
    else if (spCD %in% largeDiaBoost)
    {
      growth <- growth + (growth * growMod)  
    }
    
    growthMD <- bind_rows(growthMD, tibble(KEY=tempkey,AGE = age, DIA = max(growthMD$DIA) + growth))
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
  print(tempkey)
  
}

print('End part 2')

spcNames <- species_codes %>% 
  filter(SPCD %in% c(smallDiaSlow, smallDiaBoost, largeDiaSlow, largeDiaBoost)) %>% 
  select(Name) %>% pull()

#diameterTemp <- read.table('all_txt/Ecoregion_diameter_table.txt', skip = 4, col.names = c('Ecoregion', 'Species', 'Age', 'Diameter'), sep=" ")
diameterTemp <- read.table('simulations/s1_s2/Ecoregion_diameter_table.txt', skip = 4, col.names = c('Ecoregion', 'Species', 'Age', 'Diameter'), sep=" ")
diameterTemp <- diameterTemp %>% filter(!(Species %in% spcNames))

#'
#'
#' Now write the ecoregion parameters density text file:    
#outFile <- paste0("all_txt/Ecoregion_diameter_table_", growMod, ".txt")
outFile <- paste0("simulations/s1_s2/Ecoregion_diameter_table_", growMod,".txt")
writeLines(c(paste("LandisData", "EcoregionDiameterTable", sep="\t"),"\n",paste(">>Ecoregion", "Species", "Age", "Diameter", sep="\t")), con = outFile) #creates the independent lines of text
write.table(diameterTemp,
            file = outFile,
            row.names = F,
            append = T,
            quote = F,
            col.names = F)
lapply(1:length(dia_list), function(i) write.table(dia_list[[i]],
                                                   file = outFile,
                                                   row.names = F,
                                                   append = T,
                                                   quote = F,
                                                   col.names = F))

#'###################################################
# TEST OAK ----
#'
#' From FIA diameter growth estimates
#' 
#install.packages("rFIA")
library(rFIA)
library(tidyverse)
#'###############################################################
#' Species code object
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
#' Now back to the ecoregion-diameter table
#' 
#' Point to directory containing FIA tables
#'
fiaDir <- 'C:/Users/fitts010/Desktop/ch3_paper/Landis_Density_Succession/data/main_WI_2020'
#fiaDir <- 'D:/fia/rFIA'
#getFIA(states = "WI", dir = fiaDir, load = FALSE, nCores=3) #download the FIA tables for Wisconsin
#'
wiTB <- readFIA(fiaDir, states = c('WI'), tables=c("COND", "COND_DWM_CALC", "INVASIVE_SUBPLOT_SPP", "P2VEG_SUBP_STRUCTURE", "PLOT", "POP_ESTN_UNIT","POP_EVAL", "POP_EVAL_GRP", "POP_EVAL_TYP", "POP_PLOT_STRATUM_ASSGN", "POP_STRATUM", "SEEDLING", "SUBP_COND", "SUBP_COND_CHNG_MTRX", "SUBPLOT", "SURVEY", "TREE", "TREE_GRM_BEGIN", "TREE_GRM_COMPONENT", "TREE_GRM_MIDPT"), inMemory = T, nCores = 3)%>% clipFIA() #These are the minimum FIA tables that we need for this exercise
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

colnames(species_codes) = c('SPCD', 'Name')
#'
#' Create the empty data frames needed for the loop
#' 
#' 
#' Species with over-prediction of small diameter
#smallDiaSlow <- c(12, 95,701, 746, 762) #added the spruce

#' Species with under-prediction of small diameter
#smallDiaBoost <- c(241, 802) #added the oak
smallDiaBoost <- c(802)
#' Species with over-prediction of large diameter
#largeDiaSlow <- c(95, 105, 746)

#' Species with under-prediction of large diameter
#largeDiaBoost <- c(802, 241, 261)
largeDiaBoost <- c(802)
#' Set growth modifier
growMod <- 2.45 #this is what needs to be modified every time we calibrate with a different value
#' 
library(data.table)
#' Create the for loop
#' 
dia_list <- list()
for(i in 1:length(sp_eco_listWI)){
  #"
  
  age_dia<-data.frame()
  tempkey=sp_eco_listWI[i]
  ecoSec <- tempkey %>% str_split('_') %>% map_chr(., 1)
  spCD <- as.integer(tempkey %>% str_split('_') %>% map_chr(., 2))
 # if (!(spCD %in% c(smallDiaSlow, smallDiaBoost, largeDiaSlow, largeDiaBoost))){
  if (!(spCD %in% c(smallDiaBoost, largeDiaBoost))){
     next
  }
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
    growth <- diaGR[[1,1]]
  if (spCD %in% smallDiaBoost)
    {
      growth <- growth + (growth * growMod)  
    }
     #   if (spCD %in% smallDiaSlow)
 #   {
 #     growth <- growth - (growth * growMod)  
 #   }
 #   else if (spCD %in% smallDiaBoost)
#    {
#      growth <- growth + (growth * growMod)  
#    }
    
    smallGrowthMD <- bind_rows(smallGrowthMD, tibble(KEY=tempkey,AGE = age, DIA = max(smallGrowthMD$DIA) + growth))
    
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
    
    growth <- diaGR[[1,1]]
    if (spCD %in% largeDiaBoost)
    {
      growth <- growth + (growth * growMod)  
    }
    
#   if (spCD %in% largeDiaSlow)
#    {
#     growth <- growth - (growth * growMod)  
#   }
#  else if (spCD %in% largeDiaBoost)
#    {
#      growth <- growth + (growth * growMod)  
#    }
    
    growthMD <- bind_rows(growthMD, tibble(KEY=tempkey,AGE = age, DIA = max(growthMD$DIA) + growth))
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
  print(tempkey)
  
}

print('End part 1')
#ecoSp <- expand.grid(unique(wiTB$PLOT$ECO_PROVINCE), unique(c(smallDiaSlow, smallDiaBoost, largeDiaSlow, largeDiaBoost)), stringsAsFactors = F) %>% 
ecoSp <- expand.grid(unique(wiTB$PLOT$ECO_PROVINCE), unique(c(smallDiaBoost,largeDiaBoost)), stringsAsFactors = F) %>% 
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
  ecoSec <- tempkey %>% str_split('_') %>% map_chr(., 1)
  
  spcd <- as.integer(str_split(tempkey, pattern = '_', simplify = T)[,2])
 # if (!(spcd %in% c(smallDiaSlow, smallDiaBoost, largeDiaSlow, largeDiaBoost))){
  if (!(spcd %in% c(smallDiaBoost, largeDiaBoost))){
     next
  }
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
    
    growth <- diaGR[[1,1]]
#    if (spCD %in% smallDiaSlow)
#    {
#      growth <- growth - (growth * growMod)  
#    }
  #  else
      if (spCD %in% smallDiaBoost)
    {
      growth <- growth + (growth * growMod)  
    }
    
    smallGrowthMD <- bind_rows(smallGrowthMD, tibble(KEY=msngEcoSP[i, 1],AGE = age, DIA = max(smallGrowthMD$DIA) + growth))
    
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
    
    growth <- diaGR[[1,1]]
   # if (spCD %in% largeDiaSlow)
   # {
   #   growth <- growth - (growth * growMod)  
   # }
   # else if (spCD %in% largeDiaBoost)
   # {
    if (spCD %in% largeDiaBoost)
      {
        growth <- growth + (growth * growMod)  
    }
    
    growthMD <- bind_rows(growthMD, tibble(KEY=tempkey,AGE = age, DIA = max(growthMD$DIA) + growth))
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
  print(tempkey)
  
}

print('End part 2')

spcNames <- species_codes %>% 
#  filter(SPCD %in% c(smallDiaSlow, smallDiaBoost, largeDiaSlow, largeDiaBoost)) %>% 
  filter(SPCD %in% c(smallDiaBoost, largeDiaBoost)) %>%
    select(Name) %>% pull()

#diameterTemp <- read.table('all_txt/Ecoregion_diameter_table.txt', skip = 4, col.names = c('Ecoregion', 'Species', 'Age', 'Diameter'), sep=" ")
diameterTemp <- read.table('simulations/s1_s2/Ecoregion_diameter_table.txt', skip = 4, col.names = c('Ecoregion', 'Species', 'Age', 'Diameter'), sep=" ")
diameterTemp <- diameterTemp %>% filter(!(Species %in% spcNames))

#'
#'
#' Now write the ecoregion parameters density text file:    
#outFile <- paste0("all_txt/Ecoregion_diameter_table_", growMod, ".txt")
outFile <- paste0("simulations/s1_s2/Ecoregion_diameter_table_", growMod,"_oak",".txt")
writeLines(c(paste("LandisData", "EcoregionDiameterTable", sep="\t"),"\n",paste(">>Ecoregion", "Species", "Age", "Diameter", sep="\t")), con = outFile) #creates the independent lines of text
write.table(diameterTemp,
            file = outFile,
            row.names = F,
            append = T,
            quote = F,
            col.names = F)
lapply(1:length(dia_list), function(i) write.table(dia_list[[i]],
                                                   file = outFile,
                                                   row.names = F,
                                                   append = T,
                                                   quote = F,
                                                   col.names = F))
