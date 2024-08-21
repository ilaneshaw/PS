library("conflicted")
library("SpaDES.core")
# library("data.table")
# library("raster") 
# library("rgdal")
# library("sf")
# library("sp")
# library("terra")
# library("LandR")
# library("plotrix")
# library("ggpubr")
# library("diptest")
# library("nortest")
# library("dplyr")
# library("tidyverse")
# library("reshape2")
# library("ggplot2")
# library("quickPlot")
#library("googledrive")

options(spades.useRequire = FALSE)

## make a list of directory paths
inputsDir <- checkPath("../../inputs", create = TRUE)
outputsDir <- checkPath("../../outputs", create = TRUE)
downloadFolderArea <- checkPath(file.path(inputsDir, "studyArea/studyArea_AB_BCR6"), create = TRUE)
downloadFolderForestClass <- checkPath(file.path(inputsDir, "forestClassRasters"), create = TRUE)
downloadFolderBird <- checkPath(file.path(inputsDir, "birdRasterFiles"), create = TRUE)
outputFolderBirdPreds <- checkPath(file.path(outputsDir, "outputBirdPreds"), create = TRUE)
outputFolderBirdPredsRasters <- checkPath(file.path(outputsDir, "outputBirdPredsRasters"), create = TRUE)
setPaths(modulePath = file.path("../../modules"),
         cachePath = file.path("../../cache"),
         scratchPath = file.path("../../scratch"),
         inputPath = inputsDir,
         outputPath = outputsDir)

simPaths <- getPaths()

# #parameters from Drive
# birdList <- sort(c("BARS", "CAWA", "OVEN"))
# folderUrlBirdRaster <- "https://drive.google.com/drive/folders/11HdfTqNhHmzJ8Qk0Pk0NfiLKwyRkrltS" 
# rasterToMatchLocation <- as_id("https://drive.google.com/file/d/1dprb9sQZAUP6ty4BOQMbFf4gSiHKnvqp/view?usp=share_link")
# rasterToMatchName <- "LCC2005_V1_4a.tif"
# nameBCR <- "60"
# 
# ### STUDY AREA AB
# studyAreaLocation <- file.path("https://drive.google.com/file/d/1iYXdXFcZex304o5voX6igFm5EC0ck1Bp/view?usp=sharing")
# #specify file name
# .studyAreaName <- "studyAreaAB.shp" #specify file name
#  #specify folder url
# archiveStudyArea <- "studyAreaAB.zip" #give archive name
# #specify forest class raster details
# nameForClassRaster <- "vegTypesRas_ABNew_0722.tif"
# folderUrlForClass <- "https://drive.google.com/file/d/1GExTyIvk-B_nys6pwfcDhnHuaN-PvQu1/view?usp=sharing"
# #give archive name
# archiveForClass <- "AB_rasters_kNN_2022.07.zip"
# #specify non forest raster details
# #specify file name
# nameLandClassRaster <- "LandClassRas_ABNew_0722.tif"
# folderUrlLandClass  <-  "https://drive.google.com/file/d/1GExTyIvk-B_nys6pwfcDhnHuaN-PvQu1/view?usp=sharing"
# #give archive name
# archiveLandClass  <- "AB_rasters_kNN_2022.07.zip"
# #specify age raster details
# #specify file name
# nameAgeRaster <- "ageRas_ABNew_0722.tif"
# folderUrlAge  <- "https://drive.google.com/file/d/1GExTyIvk-B_nys6pwfcDhnHuaN-PvQu1/view?usp=sharing"
# #give archive name
# archiveAge  <- "AB_rasters_kNN_2022.07.zip"


#parameters from local
birdList <- sort(c("CAWA", "OVEN"))
#AB birdList 0.01 threshold (62 species)
#birdList <- sort(c("ALFL", "AMCR", "AMGO", "AMRO", "BANS", "BAOR", "BARS", "BAWW", "BBMA", "BBWA", "BBWO", "BCCH", "BHCO", "BHVI", "BLBW", "BLJA", "BLPW", "BOBO", "BOCH", "BOWA", "BRBL", "BRCR", "BTNW", "CAWA", "CCSP", "CEDW", "CHSP", "CLSW", "CMWA", "COGR", "CONW", "CORA", "COYE", "CSWA", "DEJU", "EAKI", "EUST", "FOSP", "GCKI", "GRAJ", "GRCA", "HAFL", "HAWO", "HETH", "HOLA", "HOSP", "HOWR", "KILL", "LCSP", "LEFL", "MOWA", "OVEN", "PAWA", "PHVI", "RBNU", "RCKI", "REVI", "SWTH", "TEWA", "WETA", "WIWR", "YRWA"))
#BC birdList 0.01 threshold (68 species)
#birdList <- sort(c("ALFL", "AMCR", "AMGO", "AMRO", "BANS", "BAOR", "BARS", "BAWW", "BBMA", "BBWA", "BCCH", "BEKI", "BHCO", "BHVI", "BLBW", "BLJA", "BLPW", "BOBO", "BOCH", "BOWA", "BRBL", "BRCR", "BTNW", "CAWA", "CCSP", "CEDW", "CHSP", "CLSW", "CMWA", "COGR", "CONW", "CORA", "COYE", "CSWA", "DEJU", "DOWO", "DUFL", "EAKI", "EAPH", "EUST", "FOSP", "GCKI", "GRAJ", "GRCA", "GRYE", "HAFL", "HAWO", "HETH", "HOLA", "HOSP", "HOWR", "KILL", "LCSP", "LEFL", "MOWA", "OSFL", "OVEN", "PAWA", "PHVI", "RBNU", "RCKI", "REVI", "SWTH", "TEWA", "WCSP", "WETA", "WIWR", "YRWA"))
#BC birdList Mosaics
 # birdList <- sort(c("ALFL", "AMCR", "AMGO", "AMRE", "AMRO", "ATSP", "ATTW", "BANS", "BAOR", "BARS",
 #                   "BAWW", "BBMA", "BBWA", "BBWO", "BCCH", "BHCO", "BHVI", "BLBW", "BLJA", "BLPW",
 #                   "BOBO", "BOCH", "BOWA", "BRBL", "BRCR", "BTNW", "CAWA", "CCSP", "CEDW", "CHSP",
 #                   "CLSW", "CMWA", "COGR", "CONW", "CORA", "COYE", "CSWA", "DEJU", "DOWO", "DUFL",
 #                   "EAKI", "EAPH", "EUST", "EVGR", "FOSP", "GCKI", "GCSP", "GCTH", "GRAJ", "GRYE",
 #                   "HAFL", "HAWO", "HETH", "HOLA", "HOSP", "HOWR", "KILL", "LCSP", "LEFL", "LEYE",
 #                   "LISP", "MAWA", "MAWR", "MOBL", "MODO", "MOWA", "NAWA", "NOFL", "NOWA", "OCWA",
 #                   "OSFL", "OVEN", "PAWA", "PHVI", "PIGR", "PISI", "PUFI", "RBGR", "RBNU", "RCKI",
 #                   "RECR", "REVI", "ROPI", "RTHU", "RUBL", "RUGR", "RWBL", "SAVS", "SEWR", "SOSA",
 #                   "SOSP", "SPSA", "SWSP", "SWTH", "TEWA", "TOSO", "TOWA", "TRES", "VATH", "VESP",
 #                   "WAVI", "WCSP", "WETA", "WEWP", "WISN", "WIWA", "WIWR", "WTSP", "WWCR", "YBFL",
 #                   "YBSA", "YEWA" ,"YHBL", "YRWA"))

#new AB study Area bird list (25% prob of occurrence in 1% of the area)
# birdList <- sort(c("ALFL", "AMGO", "AMRO", "BARS", "BBWA", "BCCH", "BHCO", "BOCH", 
#                    "BRBL", "BRCR", "CCSP", "CEDW", "CHSP", "CLSW", "CMWA", "COYE",
#                    "DEJU", "GCKI", "GRAJ", "HETH", "HOWR", "LEFL", "MOWA", "OVEN",
#                    "PAWA", "RBNU", "RCKI", "REVI", "SWTH", "TEWA", "YRWA"))

rasterToMatchLocation <- inputsDir
rasterToMatchName <- "LCC2005_V1_4a.tif"
studyAreaLocation <- downloadFolderArea
nameBCR <- "60"
#.studyAreaName <- "studyAreaAB.shp"
.studyAreaName <- "studyArea_AB_BCR6.shp"
#nameForClassRaster <-  "vegTypesRas_ABNew_0722.tif"
nameForClassRaster <-  "vegTypesRas_AB_BCR6_2011"
folderUrlForClass = downloadFolderForestClass
#nameLandClassRaster = "landClassRaster_AB_202305.tif"
nameLandClassRaster = "landCoverRas_AB_BCR6_2010"
folderUrlLandClass = downloadFolderForestClass
#nameAgeRaster = "ageRas_ABNew_0722.tif"
nameAgeRaster = "ageRas_AB_BCR6_2011"
folderUrlAge = downloadFolderForestClass
folderUrlBirdRaster <- downloadFolderBird


simModules <- list("PS")

## Set simulation and module parameters
simTimes <- list(start = 1, end = 1, timeunit = "year")
simParams <- list(
  PS = list( doPredsInitialTime = 1,
             .plotInitialTime = 1,
             .saveInitialTime = 1,
             fromDrive = FALSE,
             classOnly = FALSE,
             nTrees = 100, #5000
             ageGrouping = 20,
             maxAgeClass = 10,
             birdList = birdList,
             #folderUrlBirdRaster = folderUrlBirdRaster,
             .studyAreaName = .studyAreaName,
             #archiveStudyArea = archiveStudyArea,
             rasterToMatchLocation = rasterToMatchLocation,
             rasterToMatchName = rasterToMatchName,
             studyAreaLocation = studyAreaLocation,
             nameBCR = nameBCR,
             nameForClassRaster = nameForClassRaster,
             folderUrlForClass = folderUrlForClass,
             #archiveForClass = archiveForClass,
             nameLandClassRaster = nameLandClassRaster,
             folderUrlLandClass = folderUrlLandClass,
             #archiveLandClass = archiveLandClass,
             nameAgeRaster = nameAgeRaster,
             folderUrlAge = folderUrlAge
             #archiveAge = archiveAge
  )
)



## Simulation setup
mySim <- simInit(times = simTimes, params = simParams, 
                 modules = simModules, paths = simPaths)

test <- spades(mySim)

