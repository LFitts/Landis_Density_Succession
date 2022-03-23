#' ##################################################################################
# 1. Create table: Create the updated Species Ecoregion diameter table ----
#'###################################################################################
#' 
#' This phase will create an updated ecoregion diameter table with the selected values for each species taken from the calibration phase
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
#smallDiaSlow <- c(12, 95,701, 746, 762) ###################CAMBIAR
abiebals_smallDiaSlow<-12
picemari_smallDiaSlow<-95
ostrvirg_smallDiaSlow<-701
poputrem_smallDiaSlow<-746
prunsero_smallDiaSlow<-762

#' Species with under-prediction of small diameter
#smallDiaBoost <- c(241, 802) ######################### CAMBIAR
thujocci_smallDiaBoost<-241
queralba_smallDiaBoost<-802

#' Species with over-prediction of large diameter
#largeDiaSlow <- c(95, 105, 746) ##########################CAMBIAR
picemari_largeDiaSlow<-95
pinubank_largeDiaSlow<-105
poputrem_largeDiaSlow<-746
#' Species with under-prediction of large diameter
#largeDiaBoost <- c(802, 241, 261) #################################CAMBIAR
queralba_largeDiaBoost<-802
thujocci_largeDiaBoost<-241
tsugcana_largeDiaBoost<-261

#' Set growth modifier
growMod_0.05 <- 0.05 #different growth values for each species
growMod_0.10<-0.10
growMod_0.35<-0.35
growMod_0.50<-0.50
growMod_0.60<-0.60
growMod_0.65<-0.65
growMod_0.70<-0.70
growMod_2.55<-2.55
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
  if (!(spCD %in% c(abiebals_smallDiaSlow, picemari_smallDiaSlow, ostrvirg_smallDiaSlow, poputrem_smallDiaSlow, prunsero_smallDiaSlow, thujocci_smallDiaBoost, queralba_smallDiaBoost, picemari_largeDiaSlow, pinubank_largeDiaSlow, poputrem_largeDiaSlow, queralba_largeDiaBoost, thujocci_largeDiaBoost, tsugcana_largeDiaBoost))){ #################CAMBIAR
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
  #  if (spCD %in% smallDiaSlow) #################CAMBIAR
  #  {
  #    growth <- growth - (growth * growMod)  
  #  }
  #  else if (spCD %in% smallDiaBoost) ################CAMBIAR
  #  {
  #    growth <- growth + (growth * growMod)  
  #  }
  #  
   smallGrowthMD <- bind_rows(smallGrowthMD, tibble(KEY=tempkey,AGE = age, DIA = max(smallGrowthMD$DIA) + growth))
    if (spCD %in% abiebals_smallDiaSlow)
    {
      growth <- growth - (growth * growMod_0.60)  
    }
    else if (spCD %in% picemari_smallDiaSlow)
    {
      growth <- growth - (growth * growMod_0.50)  
    } 
    else if (spCD %in% ostrvirg_smallDiaSlow)
    {
      growth <- growth - (growth * growMod_0.65)  
    } 
    else if (spCD %in% poputrem_smallDiaSlow)
    {
      growth <- growth - (growth * growMod_0.50)  
    } 
    else if (spCD %in%prunsero_smallDiaSlow)
    {
      growth <- growth - (growth * growMod_0.35)  
    } 
    else if (spCD %in% thujocci_smallDiaBoost)
    {
      growth <- growth + (growth * growMod_0.65)  
    } 
    else if (spCD %in% queralba_smallDiaBoost)
    {
      growth <- growth + (growth * growMod_2.55)  
    } 
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
   # if (spCD %in% largeDiaSlow) ################CAMBIAR
   # {
   #   growth <- growth - (growth * growMod)  
   # }
   # else if (spCD %in% largeDiaBoost) #########CAMBIAR
   # {
   #   growth <- growth + (growth * growMod)  
   # }
    if (spCD %in% picemari_largeDiaSlow)
       {
         growth <- growth - (growth * growMod_0.50)  
       }
    else if (spCD %in% pinubank_largeDiaSlow)
    {
      growth <- growth - (growth * growMod_0.10)  
    }
    else if (spCD %in% poputrem_largeDiaSlow)
    {
      growth <- growth - (growth * growMod_0.50)  
    }   
    else if (spCD %in% queralba_largeDiaBoost)
       {
         growth <- growth + (growth * growMod_2.55)  
    }
    else if (spCD %in% thujocci_largeDiaBoost)
    {
      growth <- growth + (growth * growMod_0.65)  
    }
    else if (spCD %in% tsugcana_largeDiaBoost)
    {
      growth <- growth + (growth * growMod_0.70)  
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
#ecoSp <- expand.grid(unique(wiTB$PLOT$ECO_PROVINCE), unique(c(smallDiaSlow, smallDiaBoost, largeDiaSlow, largeDiaBoost)), stringsAsFactors = F) %>% ###############CAMBIAR
ecoSp <- expand.grid(unique(wiTB$PLOT$ECO_PROVINCE), unique(c(abiebals_smallDiaSlow, picemari_smallDiaSlow, ostrvirg_smallDiaSlow, poputrem_smallDiaSlow, prunsero_smallDiaSlow, thujocci_smallDiaBoost, queralba_smallDiaBoost, picemari_largeDiaSlow, pinubank_largeDiaSlow, poputrem_largeDiaSlow, queralba_largeDiaBoost, thujocci_largeDiaBoost, tsugcana_largeDiaBoost)), stringsAsFactors = F) %>%
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
#  if (!(spcd %in% c(smallDiaSlow, smallDiaBoost, largeDiaSlow, largeDiaBoost))){ ############CAMBIAR
  if (!(spcd %in% c(abiebals_smallDiaSlow, picemari_smallDiaSlow, ostrvirg_smallDiaSlow, poputrem_smallDiaSlow, prunsero_smallDiaSlow, thujocci_smallDiaBoost, queralba_smallDiaBoost, picemari_largeDiaSlow, pinubank_largeDiaSlow, poputrem_largeDiaSlow, queralba_largeDiaBoost, thujocci_largeDiaBoost, tsugcana_largeDiaBoost))){
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
#    if (spCD %in% smallDiaSlow) #############CAMBIAR
#    {
#      growth <- growth - (growth * growMod)  
#    }
#    else if (spCD %in% smallDiaBoost) ################CAMBIAR
#    {
#      growth <- growth + (growth * growMod)  
#    }

    if (spCD %in% abiebals_smallDiaSlow)
    {
      growth <- growth - (growth * growMod_0.60)  
    }
    else if (spCD %in% picemari_smallDiaSlow)
    {
      growth <- growth - (growth * growMod_0.50)  
    } 
    else if (spCD %in% ostrvirg_smallDiaSlow)
    {
      growth <- growth - (growth * growMod_0.65)  
    } 
    else if (spCD %in% poputrem_smallDiaSlow)
    {
      growth <- growth - (growth * growMod_0.50)  
    } 
    else if (spCD %in%prunsero_smallDiaSlow)
    {
      growth <- growth - (growth * growMod_0.35)  
    } 
    else if (spCD %in% thujocci_smallDiaBoost)
    {
      growth <- growth + (growth * growMod_0.65)  
    } 
    else if (spCD %in% queralba_smallDiaBoost)
    {
      growth <- growth + (growth * growMod_2.55)  
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
 #   if (spCD %in% largeDiaSlow) ################CAMBIAR
 #   {
 #     growth <- growth - (growth * growMod)  
 #   }
 #   else if (spCD %in% largeDiaBoost) ######################CAMBIAR
 #   {
 #     growth <- growth + (growth * growMod)  
 #   }
    if (spCD %in% picemari_largeDiaSlow)
    {
      growth <- growth - (growth * growMod_0.50)  
    }
    else if (spCD %in% pinubank_largeDiaSlow)
    {
      growth <- growth - (growth * growMod_0.10)  
    }
    else if (spCD %in% poputrem_largeDiaSlow)
    {
      growth <- growth - (growth * growMod_0.50)  
    }   
    else if (spCD %in% queralba_largeDiaBoost)
    {
      growth <- growth + (growth * growMod_2.55)  
    }
    else if (spCD %in% thujocci_largeDiaBoost)
    {
      growth <- growth + (growth * growMod_0.65)  
    }
    else if (spCD %in% tsugcana_largeDiaBoost)
    {
      growth <- growth + (growth * growMod_0.70)  
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
#  filter(SPCD %in% c(smallDiaSlow, smallDiaBoost, largeDiaSlow, largeDiaBoost)) %>% #############CAMBIAR
  filter(SPCD %in% c(abiebals_smallDiaSlow, picemari_smallDiaSlow, ostrvirg_smallDiaSlow, poputrem_smallDiaSlow, prunsero_smallDiaSlow, thujocci_smallDiaBoost, queralba_smallDiaBoost, picemari_largeDiaSlow, pinubank_largeDiaSlow, poputrem_largeDiaSlow, queralba_largeDiaBoost, thujocci_largeDiaBoost, tsugcana_largeDiaBoost)
) %>% 
    select(Name) %>% pull()

#diameterTemp <- read.table('all_txt/Ecoregion_diameter_table.txt', skip = 4, col.names = c('Ecoregion', 'Species', 'Age', 'Diameter'), sep=" ")
diameterTemp <- read.table('simulations/s3/Ecoregion_diameter_table.txt', skip = 4, col.names = c('Ecoregion', 'Species', 'Age', 'Diameter'), sep=" ")
diameterTemp <- diameterTemp %>% filter(!(Species %in% spcNames))

#'
#'
#' Now write the ecoregion parameters density text file:    
#outFile <- paste0("all_txt/Ecoregion_diameter_table_", growMod, ".txt")
outFile <- paste0("simulations/s3/Ecoregion_diameter_table_", "phase4",".txt")
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
#'##########################################################################
# TEST OF EQUIVALENCE S3 ----
#'
library(tidyverse)
#install.packages("TOSTER")
library(TOSTER)

#' #################################################
# Prepare the FIA table for validation ####
#'
WI_TREE<-read.csv("data/main_WI_2020/WI_TREE.csv")#read the tree table
WI_PLOT<-read.csv("data/main_WI_2020/WI_PLOT.csv")#read the plot table
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
plt_list <- plt_list %>% mutate(SUBKEY = str_c(KEY, str_sub(subplot_list, 3, 3), sep='_'))
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
#density<-read.csv("simulations/s2/Density_cohort_log_s2.CSV")
density<-read.csv("simulations/s3/Density_cohort_log_s3_validation_low.CSV") ### make sure to change these when testing different subplots
#'
#' Read the initial communities map codes (will be the SUBKEY)
#' 
map_codes<-read.csv("simulations/s3/output/MAPVALUE_KEY.CSV")
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
#'
densityL<-densityL %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))
#'
#'  #################################################
# Merge the LANDIS and FIA datasets ####
#'
#' 
s3<-rbind(FIA_DB, densityL)
#' 
#' Obtain the dataset for the second set of equivalence tests (species)
s3_species_SUB<-s3%>% group_by(Source, Species, SUBKEY)%>%
  summarize(meanDIA=sum(DIA_cm*TreeNumber, na.rm=T)/sum(TreeNumber, na.rm = T),
            totalBA=sum(BA_m2, na.rm=T),
            density=sum(TreeNumber, na.rm=T))
#'
s3_species_ECO<-s3%>% group_by(Source, ECO_PROVINCE, Species)%>%
  summarize(meanDIA=sum(DIA_cm*TreeNumber, na.rm=T)/sum(TreeNumber, na.rm = T),
            totalBA=sum(BA_m2, na.rm=T),
            density=sum(TreeNumber, na.rm=T))
#'
#' Convert the dataset into a wide format
#' 
s3_species_wide_eco<-s3_species_ECO%>% pivot_wider(id_cols=c(ECO_PROVINCE, Species), names_from=Source, values_from=c(meanDIA, totalBA, density))
#'
s3_species_wide_SUB<-s3_species_SUB%>% pivot_wider(id_cols=c(Species, SUBKEY), names_from=Source, values_from=c(meanDIA, totalBA, density))
#'
#' Remove rows with missing values
#'
s3_species_wide_eco<-s3_species_wide_eco[complete.cases(s3_species_wide_eco),]
#'
s3_species_wide_SUB<-s3_species_wide_SUB[complete.cases(s3_species_wide_SUB),]
#'
#' ##########################################################
#' SPECIES
#' 
# Equivalence test SPECIES total basal area ####
#'
species_vec<-unique(s3_species_wide_SUB$Species)
sp_result_BA <- tibble()
tost_sp_result_BA <- list()
#'
for(i in 1:length(species_vec)){
  tempkey=species_vec[i]
  species<-s3_species_wide_SUB %>% filter(Species==tempkey)
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
#write.csv( sp_result_BA,'C:/Users/fitts010/Desktop/ch3_paper/Landis_Density_Succession/simulations/s3/results/tost_sp_ba_result_phase4_low.CSV')
#'
#####################################################
# VALIDATE TO LOWEST MEAN BIAS ----
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
#smallDiaSlow <- c(12, 95,701, 746, 762) ###################CAMBIAR
abiebals_smallDiaSlow<-12
picemari_smallDiaSlow<-95
ostrvirg_smallDiaSlow<-701
poputrem_smallDiaSlow<-746
prunsero_smallDiaSlow<-762

#' Species with under-prediction of small diameter
#smallDiaBoost <- c(241, 802) ######################### CAMBIAR
thujocci_smallDiaBoost<-241
queralba_smallDiaBoost<-802

#' Species with over-prediction of large diameter
#largeDiaSlow <- c(95, 105, 746) ##########################CAMBIAR
picemari_largeDiaSlow<-95
pinubank_largeDiaSlow<-105
poputrem_largeDiaSlow<-746
#' Species with under-prediction of large diameter
#largeDiaBoost <- c(802, 241, 261) #################################CAMBIAR
queralba_largeDiaBoost<-802
thujocci_largeDiaBoost<-241
tsugcana_largeDiaBoost<-261

#' Set growth modifier
growMod_0.20 <- 0.20 #different growth values for each species
growMod_0.55<-0.55
growMod_0.65<-0.65
growMod_0.70<-0.70
growMod_0.75<-0.75
growMod_0.80<-0.80
growMod_0.90<-0.90
growMod_3.0<-3.0
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
  if (!(spCD %in% c(abiebals_smallDiaSlow, picemari_smallDiaSlow, ostrvirg_smallDiaSlow, poputrem_smallDiaSlow, prunsero_smallDiaSlow, thujocci_smallDiaBoost, queralba_smallDiaBoost, picemari_largeDiaSlow, pinubank_largeDiaSlow, poputrem_largeDiaSlow, queralba_largeDiaBoost, thujocci_largeDiaBoost, tsugcana_largeDiaBoost))){ #################CAMBIAR
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
    #  if (spCD %in% smallDiaSlow) #################CAMBIAR
    #  {
    #    growth <- growth - (growth * growMod)  
    #  }
    #  else if (spCD %in% smallDiaBoost) ################CAMBIAR
    #  {
    #    growth <- growth + (growth * growMod)  
    #  }
    #  
    smallGrowthMD <- bind_rows(smallGrowthMD, tibble(KEY=tempkey,AGE = age, DIA = max(smallGrowthMD$DIA) + growth))
    if (spCD %in% abiebals_smallDiaSlow)
    {
      growth <- growth - (growth * growMod_0.65)  
    }
    else if (spCD %in% picemari_smallDiaSlow)
    {
      growth <- growth - (growth * growMod_0.55)  
    } 
    else if (spCD %in% ostrvirg_smallDiaSlow)
    {
      growth <- growth - (growth * growMod_0.75)  
    } 
    else if (spCD %in% poputrem_smallDiaSlow)
    {
      growth <- growth - (growth * growMod_0.70)  
    } 
    else if (spCD %in%prunsero_smallDiaSlow)
    {
      growth <- growth - (growth * growMod_0.55)  
    } 
    else if (spCD %in% thujocci_smallDiaBoost)
    {
      growth <- growth + (growth * growMod_0.80)  
    } 
    else if (spCD %in% queralba_smallDiaBoost)
    {
      growth <- growth + (growth * growMod_3.0)  
    }
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
    # if (spCD %in% largeDiaSlow) ################CAMBIAR
    # {
    #   growth <- growth - (growth * growMod)  
    # }
    # else if (spCD %in% largeDiaBoost) #########CAMBIAR
    # {
    #   growth <- growth + (growth * growMod)  
    # }
    if (spCD %in% picemari_largeDiaSlow)
    {
      growth <- growth - (growth * growMod_0.55)  
    }
    else if (spCD %in% pinubank_largeDiaSlow)
    {
      growth <- growth - (growth * growMod_0.90)  
    }
    else if (spCD %in% poputrem_largeDiaSlow)
    {
      growth <- growth - (growth * growMod_0.70)  
    }   
    else if (spCD %in% queralba_largeDiaBoost)
    {
      growth <- growth + (growth * growMod_3.0)  
    }
    else if (spCD %in% thujocci_largeDiaBoost)
    {
      growth <- growth + (growth * growMod_0.80)  
    }
    else if (spCD %in% tsugcana_largeDiaBoost)
    {
      growth <- growth + (growth * growMod_0.90)  
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
#ecoSp <- expand.grid(unique(wiTB$PLOT$ECO_PROVINCE), unique(c(smallDiaSlow, smallDiaBoost, largeDiaSlow, largeDiaBoost)), stringsAsFactors = F) %>% ###############CAMBIAR
ecoSp <- expand.grid(unique(wiTB$PLOT$ECO_PROVINCE), unique(c(abiebals_smallDiaSlow, picemari_smallDiaSlow, ostrvirg_smallDiaSlow, poputrem_smallDiaSlow, prunsero_smallDiaSlow, thujocci_smallDiaBoost, queralba_smallDiaBoost, picemari_largeDiaSlow, pinubank_largeDiaSlow, poputrem_largeDiaSlow, queralba_largeDiaBoost, thujocci_largeDiaBoost, tsugcana_largeDiaBoost)), stringsAsFactors = F) %>%
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
  #  if (!(spcd %in% c(smallDiaSlow, smallDiaBoost, largeDiaSlow, largeDiaBoost))){ ############CAMBIAR
  if (!(spcd %in% c(abiebals_smallDiaSlow, picemari_smallDiaSlow, ostrvirg_smallDiaSlow, poputrem_smallDiaSlow, prunsero_smallDiaSlow, thujocci_smallDiaBoost, queralba_smallDiaBoost, picemari_largeDiaSlow, pinubank_largeDiaSlow, poputrem_largeDiaSlow, queralba_largeDiaBoost, thujocci_largeDiaBoost, tsugcana_largeDiaBoost))){
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
    #    if (spCD %in% smallDiaSlow) #############CAMBIAR
    #    {
    #      growth <- growth - (growth * growMod)  
    #    }
    #    else if (spCD %in% smallDiaBoost) ################CAMBIAR
    #    {
    #      growth <- growth + (growth * growMod)  
    #    }
    
    if (spCD %in% abiebals_smallDiaSlow)
    {
      growth <- growth - (growth * growMod_0.65)  
    }
    else if (spCD %in% picemari_smallDiaSlow)
    {
      growth <- growth - (growth * growMod_0.55)  
    } 
    else if (spCD %in% ostrvirg_smallDiaSlow)
    {
      growth <- growth - (growth * growMod_0.75)  
    } 
    else if (spCD %in% poputrem_smallDiaSlow)
    {
      growth <- growth - (growth * growMod_0.70)  
    } 
    else if (spCD %in%prunsero_smallDiaSlow)
    {
      growth <- growth - (growth * growMod_0.55)  
    } 
    else if (spCD %in% thujocci_smallDiaBoost)
    {
      growth <- growth + (growth * growMod_0.80)  
    } 
    else if (spCD %in% queralba_smallDiaBoost)
    {
      growth <- growth + (growth * growMod_3.0)  
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
    #   if (spCD %in% largeDiaSlow) ################CAMBIAR
    #   {
    #     growth <- growth - (growth * growMod)  
    #   }
    #   else if (spCD %in% largeDiaBoost) ######################CAMBIAR
    #   {
    #     growth <- growth + (growth * growMod)  
    #   }
    if (spCD %in% picemari_largeDiaSlow)
    {
      growth <- growth - (growth * growMod_0.55)  
    }
    else if (spCD %in% pinubank_largeDiaSlow)
    {
      growth <- growth - (growth * growMod_0.90)  
    }
    else if (spCD %in% poputrem_largeDiaSlow)
    {
      growth <- growth - (growth * growMod_0.70)  
    }   
    else if (spCD %in% queralba_largeDiaBoost)
    {
      growth <- growth + (growth * growMod_3.0)  
    }
    else if (spCD %in% thujocci_largeDiaBoost)
    {
      growth <- growth + (growth * growMod_0.80)  
    }
    else if (spCD %in% tsugcana_largeDiaBoost)
    {
      growth <- growth + (growth * growMod_0.90)  
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
  #  filter(SPCD %in% c(smallDiaSlow, smallDiaBoost, largeDiaSlow, largeDiaBoost)) %>% #############CAMBIAR
  filter(SPCD %in% c(abiebals_smallDiaSlow, picemari_smallDiaSlow, ostrvirg_smallDiaSlow, poputrem_smallDiaSlow, prunsero_smallDiaSlow, thujocci_smallDiaBoost, queralba_smallDiaBoost, picemari_largeDiaSlow, pinubank_largeDiaSlow, poputrem_largeDiaSlow, queralba_largeDiaBoost, thujocci_largeDiaBoost, tsugcana_largeDiaBoost)
  ) %>% 
  select(Name) %>% pull()

#diameterTemp <- read.table('all_txt/Ecoregion_diameter_table.txt', skip = 4, col.names = c('Ecoregion', 'Species', 'Age', 'Diameter'), sep=" ")
diameterTemp <- read.table('simulations/s3/Ecoregion_diameter_table.txt', skip = 4, col.names = c('Ecoregion', 'Species', 'Age', 'Diameter'), sep=" ")
diameterTemp <- diameterTemp %>% filter(!(Species %in% spcNames))

#'
#'
#' Now write the ecoregion parameters density text file:    
#outFile <- paste0("all_txt/Ecoregion_diameter_table_", growMod, ".txt")
outFile <- paste0("simulations/s3/Ecoregion_diameter_table_", "phase4_LOW",".txt")
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
#'##########################################################################