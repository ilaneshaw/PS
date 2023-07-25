

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

rasterToMatch <- prepInputsLCC(year = 2005)

#get StudyArea shapefile
print("get studyArea shapefile from local drive")
studyArea <- read_sf(file.path("C:/Users/RALAS6/Documents/Repositories/Data/BCR6-BC/BCR6_BC.shp"))

#postProcess studyArea
studyArea <- reproducible::postProcess(studyArea,
                                       destinationPath = getwd(),
                                       filename2 = "studyArea", 
                                       useTerra = FALSE,
                                       #fun = "sf", #use the function vect
                                       targetCRS = crs(rasterToMatch), #make crs same as rasterToMatch
                                       overwrite = TRUE,
                                       verbose = TRUE)

rasterToMatch <- crop(rasterToMatch, studyArea)
rasterToMatch <- mask(rasterToMatch, studyArea) 
names(rasterToMatch) <- "rasterToMatch"
plot(rasterToMatch)

LCC10 <- prepInputsLCC(year = 2010, rasterToMatch = rasterToMatch)
LCC10 
plot(LCC10)

valsLCC10 <- getValues(LCC10)
uniqueValsLCC10 <- unique(valsLCC10)


reclassTab <- read.csv("C:/Users/RALAS6/Documents/Repositories/SpaDES/inputs/LCC10_reclass.csv", sep = ";", header = TRUE)

landCoverClassRaster <- reclassify(LCC10, reclassTab)
landCoverClassRaster
plot(landCoverClassRaster)
is.factor(landCoverClassRaster)
writeRaster(landCoverClassRaster, "C:/Users/RALAS6/Documents/Repositories/Data/BCR6-BC/landClassRaster_BC_202305.tif")
#writeRaster(landCoverClassRaster, "C:/Users/RALAS6/Documents/Repositories/Data/studyAreaAB/landClassRaster_AB_202305.tif")

#BUT CAN I DO THIS USING TERRA? AND CAN I INSERT IT INTO ANA'S MODULE? 
