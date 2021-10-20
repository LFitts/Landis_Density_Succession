library(tidyverse)
###################################################################################


#' # 1. Read in the FIA data for the state of Wisconsin
#' 
#' Read in the data for Wisconsin
#' 
#WI_COND<-fread("data/main_WI_2020/WI_COND.csv")#read the condition table
#WI_PLOT<-fread("data/main_WI_2020/WI_PLOT.csv")#read the plot table
#WI_TREE<-fread("data/main_WI_2020/WI_TREE.csv")#read the tree table

WI_COND <- wiTB$COND
WI_PLOT <- wiTB$PLOT
WI_TREE <- wiTB$TREE


plt_list <- read_csv('data/WI_PLOT_LIST.CSV')
plt_list <- plt_list %>% mutate(SUBKEY = str_c(KEY, str_sub(subplot_list, 1, 1), sep='_'))

WI_COND <- WI_COND %>% mutate(KEY = str_c(STATECD, COUNTYCD, PLOT, sep='_')) %>% 
  filter(KEY %in% unique(plt_list$KEY))

WI_TREE <- WI_TREE %>% mutate(SUBKEY = str_c(STATECD, COUNTYCD, PLOT, SUBP, sep='_')) %>% 
  filter(SUBKEY %in% unique(plt_list$SUBKEY))
#FIA species name reference table
#Available https://apps.fs.usda.gov/fia/datamart/CSV/REF_SPECIES.csv 
ref <- read_csv('data/REF_SPECIES.csv')
ref <- ref %>% mutate(LANDSPEC = paste0(tolower(str_sub(GENUS,1,4)), str_sub(SPECIES,1,4))) %>%
  select(SPCD, LANDSPEC)
  
#Area of site (raster cell size squared in meters)
siteSize = 169
#'
#'
#' Subset for ONE county (COUNTYCD =1) and just keep records from cycle 8
#' and select variables of interest
#'
WI_COND<- WI_COND %>% subset(CYCLE == 8) %>%
  select(PLT_CN, INVYR, STATECD, COUNTYCD, PLOT, COND_STATUS_CD, CONDID, DSTRBCD1, DSTRBCD2, DSTRBCD3) %>%
  mutate(MAPCODE = 1:nrow(.))
WI_PLOT<-subset(WI_PLOT, CN %in% unique(WI_COND$PLT_CN)) %>%
  select(CN, INVYR, STATECD, COUNTYCD, PLOT, ELEV, ECOSUBCD, CYCLE) %>%
  mutate(ECOSECCD = str_replace(str_sub(ECOSUBCD, 1, -2), ' ', ''))

WI_TREE<-subset(WI_TREE, PLT_CN %in% unique(WI_COND$PLT_CN)) %>% 
  select(CN,PLT_CN, INVYR, STATECD, COUNTYCD, PLOT, SUBP, TREE, STATUSCD, SPCD,
         SPGRPCD, DIA, DIAHTCD, HT, ACTUALHT, AGENTCD, DAMAGE_AGENT_CD1, 
         DAMAGE_AGENT_CD2, DAMAGE_AGENT_CD3, MORTYR, STANDING_DEAD_CD, 
         TPA_UNADJ, DRYBIO_BOLE, DRYBIO_TOP, DRYBIO_STUMP, DRYBIO_SAPLING, 
         DRYBIO_WDLD_SPP, DRYBIO_BG, DRYBIO_AG, CARBON_AG, CARBON_BG) %>%
  left_join(., ref, by = 'SPCD')

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
    landGrow <- read.table('all_txt/Ecoregion_diameter_table.txt', skip=4, col.names=c('ECOREGION','SPECIES','AGE','DIAMETER'))
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
MV_KEY <- data.frame()
PLOTMAPVALUE <- 1
outFile = file('output/Initial_Community.txt', 'w')
cat('LandisData "Initial Communities"\n', file=outFile, sep='\n')
for (i in 1:nrow(WI_COND))
{
  COND_SUB <- WI_COND[i,]
  if (COND_SUB$COND_STATUS_CD != 1){next}
  PLOT_SUB <- WI_PLOT %>% filter(CN == COND_SUB$PLT_CN)
  if (nrow(WI_TREE %>% filter(PLT_CN == COND_SUB$PLT_CN & STATUSCD == 1)) == 0){next}
  TREE_SUB <- WI_TREE %>% filter(PLT_CN == COND_SUB$PLT_CN & STATUSCD == 1 & LANDSPEC %in% unique(landGrow$SPECIES) & !(is.na(DIA)) & !(is.na(TPA_UNADJ))) %>% 
    select(PLT_CN, LANDSPEC, SUBP, TPA_UNADJ, DIA) 
  if (nrow(TREE_SUB) == 0){next}
  
  TREE_SUB <- TREE_SUB %>% rowwise() %>%
    mutate(AGECLASS = ageClass(str_replace(PLOT_SUB$ECOSECCD, ' ', ''), LANDSPEC, (DIA * 2.54)),
           TREENUM = round((TPA_UNADJ * 4 / 4046.86) * siteSize))
  
  TREE_SUB <- TREE_SUB %>% filter(AGECLASS < 1000)

  
  TREE_GRP <- TREE_SUB %>% group_by(SUBP, LANDSPEC, AGECLASS) %>%
    summarise(TREESUM = sum(TREENUM, na.rm=T), .groups='drop') %>%
    mutate(ICSTRING = paste0(' ', AGECLASS, ' (', TREESUM, ')'))
  
  for (j in unique(TREE_GRP$SUBP))
  {
    TREE_OUT <- TREE_GRP %>% filter(SUBP == j) %>% group_by(LANDSPEC) %>%
      summarise(ICSTRING = paste(unique(LANDSPEC), paste0(unique(ICSTRING) , collapse = ''), collapse = ''), .groups='drop')
    
    cat(paste('MapCode', PLOTMAPVALUE), file=outFile, sep='\n')
    cat(TREE_OUT$ICSTRING, file=outFile, sep='\n')
    cat('\n', file=outFile)
    
    out_row <- data.frame(MAPVALUE = PLOTMAPVALUE, PLT_CN = as.character(COND_SUB$PLT_CN), SUBP = j)
    MV_KEY <- rbind(MV_KEY, out_row)
    PLOTMAPVALUE <- PLOTMAPVALUE + 1
  }

}
close(outFile)
write_csv(MV_KEY, 'output/MAPVALUE_KEY.csv')
