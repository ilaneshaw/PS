

library("conflicted")
library("SpaDES")
library("googledrive")
library("terra")
library("LandR")
library("sf")


# #get rasterToMatch
# print("get rasterTomatch from local drive")
# rasterToMatch <- terra::rast(file.path("C:/Users/RALAS6/Documents/Repositories/Data/LCCProducts/LCC05/LCC2005_V1_4a.tif"))
# 
# 
# #get StudyArea shapefile
# print("get studyArea shapefile from local drive")
# studyArea <- terra::vect(file.path("C:/Users/RALAS6/Documents/Repositories/Data/studyAreaAB/studyAreaAB.shp"))
# 
# #postProcess studyArea
# studyArea <- reproducible::postProcess(studyArea,
#                                        destinationPath = getwd(),
#                                        filename2 = "studyArea", 
#                                        useTerra = TRUE,
#                                        fun = "terra::vect", #use the function vect
#                                        targetCRS = crs(rasterToMatch), #make crs same as rasterToMatch
#                                        overwrite = TRUE,
#                                        verbose = TRUE)
# 
# rasterToMatch
# 
# #crop and mask rasterToMatch
# rasterToMatch <- terra::mask(terra::crop(rasterToMatch, studyArea), studyArea) 
# names(rasterToMatch) <- "rasterToMatch"
# # clearPlot()
# # Plot(sim$rasterToMatch)
# plot(rasterToMatch, colNA = "grey")
# 
# LCC20 <- terra::rast(file.path("C:/Users/RALAS6/Documents/Repositories/Data/LCCProducts/LCC20/landcover-2020-classification.tif"))
# LCC20
# #LCC20 <- as.factor(LCC20)
# #terra::unique(LCC20)
# plot(LCC20)
# 
# reclassTab <- read.csv("C:/Users/RALAS6/Documents/Repositories/SpaDES/inputs/LCC20_reclass.csv", sep = ";", header = TRUE)
# 
# landCoverClassRaster <- terra::classify(LCC20, reclassTab)
# landCoverClassRaster
# is.factor(landCoverClassRaster)
# 
# LCCRas <- postProcessTerra(landCoverClassRaster, studyArea = studyArea)
# LCCRas
# plot(LCCRas)
# as.factor(LCCRas)
# LCCRas <- postProcessTerra(LCCRas, rasterToMatch = rasterToMatch)
# LCCRas
# is.factor(LCCRas)
# terra::unique(LCCRas)



#############################################################################################
outputsDir <- checkPath("../../outputs", create = TRUE)
inputsDir <- checkPath("../../inputs", create = TRUE)

setPaths(modulePath = file.path("../../modules"),
         cachePath = file.path("../../cache"),
         scratchPath = file.path("../../scratch"),
         inputPath = inputsDir,
         outputPath = outputsDir)

rasterToMatchLocation <- file.path(inputsDir, "LCC2005_V1_4a.tif")
rasterToMatch <- terra::rast(rasterToMatchLocation)
#rasterToMatch <- prepInputsLCC(year = 2005)

#get StudyArea shapefile
print("get studyArea shapefile from local drive")

studyArea <- terra::vect("../../inputs/studyArea/studyArea_AB_BCR6/studyArea_AB_BCR6.shp")

#postProcess studyArea
studyArea <- reproducible::postProcess(studyArea,
                                       destinationPath = getwd(),
                                       filename2 = "studyArea", 
                                       #useTerra = FALSE,
                                       #fun = "sf", #use the function vect
                                       targetCRS = crs(rasterToMatch), #make crs same as rasterToMatch
                                       overwrite = TRUE,
                                       verbose = TRUE)

rasterToMatch <- crop(rasterToMatch, studyArea)
rasterToMatch <- mask(rasterToMatch, studyArea) 
names(rasterToMatch) <- "rasterToMatch"
plot(rasterToMatch)

#LCC10 <- prepInputsLCC(year = 2010, rasterToMatch = rasterToMatch)
LCC10Location <- file.path(inputsDir, "CAN_LC_2010_CAL.tif")
LCC10 <- terra::rast(LCC10Location)
LCC10 <- postProcessTerra(from = LCC10,
                          to = rasterToMatch,
                          overwrite = FALSE,
                          verbose = TRUE)
LCC10 
plot(LCC10)

valsLCC10 <- values(LCC10)
uniqueValsLCC10 <- sort(unique(valsLCC10))


reclassTab <- read.csv("C:/Users/RALAS6/Documents/Repositories/SpaDES/inputs/LCC10_reclass.csv", sep = ";", header = TRUE)
reclassTab$landCoverClass <- as.factor(reclassTab$landCoverClass)
landCoverClassRaster <- terra::classify(LCC10, reclassTab)
landCoverClassRaster
plot(landCoverClassRaster)
is.factor(landCoverClassRaster)


terra::writeRaster(x = landCoverClassRaster,
                   filename = file.path(outputsDir, "landCoverRas_AB_BCR6_2010"),
                   filetype= "GTiff",
                   gdal="COMPRESS=NONE",
                   overwrite = TRUE)
