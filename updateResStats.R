knitr::opts_chunk$set(echo = TRUE)
library("spdep")


library(conflicted)
library(sna)
#library(rgdal)
library(terra)
library(SpaDES)
library(data.table)
library(ggplot2) 
library(readr)
library(reshape2)
library(scales)
library(tidyr)
library(RColorBrewer)
library(dplyr)
library(forcats)
library(hrbrthemes)
library(viridis)
library(naniar)
library(raster)
library(rasterVis)
library(ggh4x)
library(egg)
library(cowplot)
library(dplyr)
library(tidyr)
library(ggspatial)
library(paletteer)
library(tidyterra)
library(gt)
library(tidyverse)
library(COINr)

options(digits=3) #limit number of digits displayed

birdList <- sort(c("ALFL", "BBWA", "BCCH", "BOCH", "BRCR", "CMWA", "COYE", 
                   "DEJU", "GCKI", "GRAJ",  "LEFL", "MOWA", "OVEN", "PAWA",
                   "RBNU", "RCKI", "REVI", "SWTH", "TEWA", "YRWA"))


# make a list of directory paths
inputsDir <- checkPath("../../inputs", create = TRUE)
outputsDir <- checkPath("../../outputs", create = TRUE)
downloadFolderArea <- checkPath(file.path(inputsDir, "studyArea/studyArea_AB_BCR6"), create = TRUE)
downloadFolderForestClass <- checkPath(file.path(inputsDir, "forestClassRasters"), create = TRUE)
downloadFolderBird <- checkPath(file.path(inputsDir, "birdRasterFiles"), create = TRUE)
outputFolderBirdPreds <- checkPath(file.path(outputsDir, "5x40yrAgeClasses/outputBirdPreds"), create = TRUE)
outputFolderBirdPredsRasters <- checkPath(file.path(outputsDir, "5x40yrAgeClasses/outputBirdPredsRasters"), create = TRUE)
setPaths(modulePath = file.path("../../modules"),
         cachePath = file.path("../../cache"),
         scratchPath = file.path("../../scratch"),
         inputPath = inputsDir,
         outputPath = outputsDir)

simPaths <- getPaths()

#INPUT Residual rasters


for1DAndLc1DRes <- lapply(birdList, FUN= function(bird){
  
  # browser()
  #bird <- "ALFL"
  print(bird)  
  
  resRas_1D <- terra::rast(paste(outputsDir, "/5x40yrAgeClasses/outputBirdPredsRasters/", bird, "-for1DAndLc1DRes", sep = ""))
  
  return(resRas_1D)
  
})

names(for1DAndLc1DRes) <- birdList


for2DAndLc1DRes <- lapply(birdList, FUN= function(bird){
  
  print(bird)  
  
  resRas_2D <- terra::rast(paste(outputsDir, "/5x40yrAgeClasses/outputBirdPredsRasters/", bird, "-for2DAndLc1DRes", sep = ""))
  
  return(resRas_2D)
  
})

names(for2DAndLc1DRes) <- birdList


birdRasters <- lapply(birdList, FUN= function(bird){
  
  print(bird)  
  
  nmRas <- terra::rast(paste(inputsDir, "/birdRasterFiles/", bird,"-meanBoot_BCR-60_studyArea_AB_BCR6", sep = "")) 
  
  return(nmRas)
  
})

names(birdRasters) <- birdList



#get tables of residuals
print("get resTabs")
resTabs <- lapply(X = birdList, FUN = function(bird){
  print(bird)
  ras1D <- eval(parse(text=paste("for1DAndLc1DRes$", bird, sep = "")))
  ras2D <- eval(parse(text=paste("for2DAndLc1DRes$", bird, sep = "")))
  
  resVals1D <- as.data.table(terra::values(ras1D, dataframe = FALSE)) 
  resVals1D <- setnames( resVals1D,  "resVals")
  resVals1D <- na.omit(resVals1D)
  res1DLab <- rep("res1D", nrow(resVals1D))
  resVals1D <- cbind(resVals1D, binningType = res1DLab)
  
  resVals2D <- as.data.table(terra::values(ras2D, dataframe = FALSE))
  resVals2D <- setnames(resVals2D,  "resVals")
  resVals2D <- na.omit(resVals2D)
  res2DLab <- rep("res2D", nrow(resVals2D))
  resVals2D <- cbind(resVals2D, binningType = res2DLab)
  
  resVals <- rbind(resVals1D, resVals2D)
  birdSp <- rep(paste(bird), nrow(resVals))
  resVals <- cbind(resVals, birdSp = birdSp)
  resVals[, absResVals := abs(resVals)]
  print(resVals)
  
  #save table
  fileName <- paste(bird, "_fullResDataset.csv")
  write.csv(resVals, file =  file.path(outputFolderBirdPreds, fileName))
  
  return(resVals)
})
names(resTabs) <- birdList



#get residual stats
print("get residual stats")
residualStats <- lapply(X = birdList, FUN = function(bird){
 
  print(bird)

  nmRas <- eval(parse(text=paste("birdRasters$", bird, sep = "")))
  res1D <- eval(parse(text=paste("for1DAndLc1DRes$", bird, sep = "")))
  res2D <- eval(parse(text=paste("for2DAndLc1DRes$", bird, sep = "")))
 
  resTab <- eval(parse(text=paste("resTabs$", bird, sep = "")))
  absResStats <- resTab[,list(medResAbs = median(absResVals),
                              maxResAbs = max(absResVals)),
    by = binningType]
  
 
  m1D <- median(terra::values(res1D, dataframe = FALSE), na.rm = TRUE)
  m2D <- median(terra::values(res2D, dataframe = FALSE), na.rm = TRUE)
  sa1D  <- terra::autocor(res1D, method = "moran")
  sa2D  <- terra::autocor(res2D, method = "moran")
  # mt1D <- spdep::moran.test(res1D, alternative="greater")
  # mt2D <- spdep::moran.test(res2D, alternative="greater")
  # mtp1D <- 
  # mtp2D <- 
  m1DAbs <- absResStats[binningType == "res1D"]$medResAbs
  m2DAbs <- absResStats[binningType == "res2D"]$medResAbs
  max1DAbs <- absResStats[binningType == "res1D"]$maxResAbs
  max2DAbs <- absResStats[binningType == "res2D"]$maxResAbs
  mNM <- median(terra::values(nmRas, dataframe = FALSE), na.rm = TRUE)
  medAbsProp1D <- m1DAbs/mNM
  medAbsProp2D <- m2DAbs/mNM

    
  residualStats <- matrix(c( m1D, m2D,  sa1D, sa2D, m1DAbs, m2DAbs, max1DAbs, max2DAbs, mNM, medAbsProp1D, medAbsProp2D), ncol= 11, byrow=TRUE)
  colnames(residualStats) <- c( 'median1DRes', 'median2DRes', "autocor1DRes", "autocor2DRes", 'median1DResAbs', 'median2DResAbs', 'max1DResAbs', 'max2DResAbs', "nmMedian", "propOfNM1DMed", "propOfNM2DMed")
  row.names(residualStats) <- bird
  print(paste(bird, " calculation complete"))
  return(residualStats)
  
})

residualStats <- do.call(rbind, residualStats)

fileName <- "resStats.csv"
write.csv(residualStats, file =  file.path(outputFolderBirdPreds, fileName))

head(residualStats)
