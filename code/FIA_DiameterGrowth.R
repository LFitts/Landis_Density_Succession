library(rFIA)
library(tidyverse)

#Point to directory containing FIA tables
fiaDir <- 'D:/FIA/rFIA/'

wiTB <- readFIA(fiaDir, states = c('WI'))
wiTB <- clipFIA(wiTB)

#----Estimate diameter growth for trees smaller than 5" DBH----
wi.st <- wiTB$TREE %>% filter(DIA < 5.0 & STATUSCD == 1) %>% select(CN, PLT_CN, PREV_TRE_CN, INVYR,
                                                    CYCLE, PLOT, SUBP, TREE, SPCD, DIA)

wi.tg <- wi.st %>% filter(CYCLE == 9) %>% 
  left_join(wi.st %>% filter(CYCLE == 8) %>% select(CN, PREV_TRE_CN, DIA), by = c('PREV_TRE_CN' = 'CN')) 

wi.tg$DIA_GROW <- wi.tg$DIA.x - wi.tg$DIA.y
wi.tg$sizeClass <- makeClasses(wi.tg$DIA.x, interval = 1, numLabs = T)
dg.summ <- wi.tg %>% group_by(SPCD, sizeClass) %>% summarise(DIA_GROW = mean(DIA_GROW, na.rm=T))

#Example for red maple
smallWorkingTB <- dg.summ %>% filter(SPCD == 316)

smallGrowthMD <- tibble(AGE = 1, DIA = 1.0)
age <- 1

while(max(smallGrowthMD$DIA) < 5){
  subTB <- smallWorkingTB %>% filter(sizeClass <= max(smallGrowthMD$DIA)) %>% ungroup() %>% select(DIA_GROW) 
  diaGR <- subTB %>% slice(nrow(subTB))
  smallGrowthMD <- bind_rows(smallGrowthMD, tibble(AGE = age, DIA = max(smallGrowthMD$DIA) + diaGR[[1,1]]))
  age <- age + 1
}

#----Estimates of diameter growth for large trees----
wiVR <- vitalRates(wiTB, bySpecies = T, bySizeClass = T, treeType = 'live')

#Example for red maple
workingTB <- wiVR %>% filter(SPCD == 316)

growthMD <- smallGrowthMD
age <- max(growthMD$AGE) + 1

while(max(growthMD$DIA) < 29){
  subTB <- workingTB %>% filter(sizeClass <= max(growthMD$DIA)) %>% select(DIA_GROW)
  diaGR <- subTB %>% slice(nrow(subTB))
  growthMD <- bind_rows(growthMD, tibble(AGE = age, DIA = max(growthMD$DIA) + diaGR[[1,1]]))
  age <- age + 1
}

ggplot(data=growthMD, aes(x=AGE, y=DIA)) +
  geom_line() +
  #geom_point() +
  theme_classic()
