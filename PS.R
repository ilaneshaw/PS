## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "PS",
  description = "",
  keywords = "",
  authors = structure(list(list(given = c("Isolde"), family = "Lane Shaw", role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(PS = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "PS.Rmd"), ## same file
  reqdPkgs = list("PredictiveEcology/SpaDES.core@development (>= 1.1.1)", "ggplot2", "raster", "rgdal", "sf", "data.table", "terra",
                  "LandR", "googledrive", "plotrix", "ggpubr", "diptest", "nortest", "dplyr", "tidyverse", "terra", "reshape2", "RColorBrewer", "rasterVis"),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter("doPredsInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter("doPredsInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between getPreds events."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", 1, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                    "area obtained using `reproducible::studyAreaName()`"),
    defineParameter("fromDrive", "logical", TRUE, NA, NA,
                    "Should the rasterToMatch, studyArea and bird Rasters be found on Google Drive or a similar online source? If false, they should already be on your local computer."),
    defineParameter("classOnly", "logical", TRUE, NA, NA,
                    "do smoothing by cover class only (1D)? if FALSE smoothing will be done by forest type and age class where possible"),
    defineParameter("nTrees", "numeric", 5000, NA, NA,
                    "number of trees for gbm to build"),
    defineParameter("maxAgeClass", "numeric", 15, NA, NA,
                    "what the oldest age class will be (everything older will be included in this class)"),
    defineParameter("ageGrouping", "numeric", 10, NA, NA,
                    "how many years included per age class"),
    defineParameter("birdList", "character", NA, NA, NA,
                    "a list of bird species in the format of 4-letter bird codes"),
    defineParameter("rasterToMatchLocation", "character", NA, NA, NA,
                    "the file location of the rasterToMatch"),
    defineParameter("rasterToMatchName", "character", NA, NA, NA,
                    "the name of the rasterToMatch file"),
    defineParameter("studyAreaLocation", "character", NA, NA, NA,
                    "the file location of the studyArea"),
    defineParameter("nameBCR", "character", NA, NA, NA,
                    "the BAM regional model BCR region that the studyArea is located in"),
    defineParameter("nameForClassRaster", "character", NA, NA, NA,
                    "the file name of the forest class raster"),
    defineParameter("folderUrlForClass", "character", NA, NA, NA,
                    "the location of the forest class raster"),
    defineParameter("archiveForClass", "character", NA, NA, NA,
                    "the zip file the forest class raster is located in"),
    defineParameter("nameNonForRaster", "character", NA, NA, NA,
                    "the file name of the non forest raster"),
    defineParameter("folderUrlNonFor", "character", NA, NA, NA,
                    "the location of the non forest raster"),
    defineParameter("archiveNonFor", "character", NA, NA, NA,
                    "the zip file the non forest raster is located in"),
    defineParameter("nameAgeRaster", "character", NA, NA, NA,
                    "the file name of the age raster"),
    defineParameter("folderUrlAge", "character", NA, NA, NA,
                    "the location of the age raster"),
    defineParameter("archiveAge", "character", NA, NA, NA,
                    "the zip file the age raster is located in"),
    defineParameter("folderUrlBirdRaster", "character", NA, NA, NA,
                    "the location of the bird density rasters"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter("min2DStatsSample", "numeric", 50, NA, NA,
                    "exclude any classes from 2D stats table that have a sample size smaller than minStatsSample")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA)
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )
))

## event types
#   - type `init` is required for initialization

doEvent.PS = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)
      # schedule future event(s)
      
      if (P(sim)$classOnly == TRUE) {
        sim <- scheduleEvent(sim, eventTime = P(sim)$doPredsInitialTime, 
                             moduleName = "PS", eventType = "do1DPreds")
        sim <- scheduleEvent(sim, eventTime = P(sim)$doPredsInitialTime, 
                             moduleName = "PS", eventType = "assess1D")
        sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "PS", "plot")
        sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "PS", "save")
      } else {
        sim <- scheduleEvent(sim, eventTime = P(sim)$doPredsInitialTime, 
                             moduleName = "PS", eventType = "do1DPreds")
        sim <- scheduleEvent(sim, eventTime = P(sim)$doPredsInitialTime, 
                             moduleName = "PS", eventType = "assess1D")
        sim <- scheduleEvent(sim, eventTime = P(sim)$doPredsInitialTime, 
                             moduleName = "PS", eventType = "do2DPreds")
        sim <- scheduleEvent(sim, eventTime = P(sim)$doPredsInitialTime, 
                             moduleName = "PS", eventType = "assess2D")
        sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "PS", "plot")
        sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "PS", "save")
      }
      
      
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      plotFun(sim) # example of a plotting function
      # schedule future event(s)

      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "PS", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "PS", "save")

      # ! ----- STOP EDITING ----- ! #
    },
    do1DPreds = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      sim <- do1DPreds(sim)
      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      sim <- scheduleEvent(sim, eventTime = time(sim) + P(sim)$doPredsInterval, 
                           moduleName = "PS", eventType = "do1DPreds") 

      # ! ----- STOP EDITING ----- ! #
    },
    assess1D = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      sim <- assess1D(sim)
      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function
      
      # schedule future event(s)
      
      # e.g.,
      sim <- scheduleEvent(sim, eventTime = time(sim) + P(sim)$doPredsInterval, 
                           moduleName = "PS", eventType = "assess1D") 
      
      # ! ----- STOP EDITING ----- ! #
    },
    do2DPreds = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      sim <- do2DPreds(sim)
      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function
      
      # schedule future event(s)
      
      # e.g.,
      sim <- scheduleEvent(sim, eventTime = time(sim) + P(sim)$doPredsInterval, 
                           moduleName = "PS", eventType = "do2DPreds") 
      
      # ! ----- STOP EDITING ----- ! #
    },
    assess2D = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      sim <- assess2D(sim)
      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function
      
      # schedule future event(s)
      
      # e.g.,
      sim <- scheduleEvent(sim, eventTime = time(sim) + P(sim)$doPredsInterval, 
                           moduleName = "PS", eventType = "assess2D") 
      
      # ! ----- STOP EDITING ----- ! #
    },

    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  
  
  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)
  
  #save the birdDatasets tables
  print(
    "saving bird datasets"
  )
  save(sim$birdDatasets,
       file =  file.path(outputFolderBirdPreds, "birdDatasets.Rdata"))
  #load(file.path(outputFolderBirdPreds, "birdDatasets.Rdata"))
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  
  sampleData <- data.frame("TheSample" = sample(1:10, replace = TRUE))
  Plots(sampleData, fn = ggplotFn)
  
  # #rasterToMatch
  # 
  # Plot(sim$rasterToMatch, colNA="grey70")
  # 
  # #forClassRaster
  # Plot(sim$forClassRaster, colNA="grey70")
  # # customizing the legend
  # # Plot(sim$forClassRaster, legend = FALSE, col = rev(terrain.colors(7)), colNA="grey70")
  # # legend("bottomright", legend = c("Black Spruce", "Black Spruce Wet", "Conifer Mix", "Deciduous", "Mixed", "Pine", "White Spruce"), fill = rev(terrain.colors(7)))
  # # 
  # #nonForRaster
  # Plot(sim$nonForRaster,colNA="grey70")
  # # customizing the legend
  # #Plot(sim$nonForRaster, legend = FALSE, col = rev(terrain.colors(7)), colNA="grey70")
  # #legend("bottomright", legend = c("Forested", "Water/Ice", "Wetland", "Anthro/Exposed Land", "Grass/Cropland", "Shrub", "Bryoid"), fill = rev(terrain.colors(7)))
  # 
  # #ageRaster
  # Plot(sim$ageRaster, main = 'ageRaster', colNA="grey70" )
  # 
  # #landscapeRaster
  # Plot(sim$landscapeRaster, colNA="grey70") 
  # 
  # #FNFRaster
  # Plot(sim$FNFRaster, colNA="grey70") 
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
do1DPreds <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test

  #get bird datasets
  sim$birdDatasets <- lapply(X = P(sim)$birdList, FUN = function(bird){
    
    birdLayer <- eval(parse(text=paste("sim$birdRasters$", bird, sep = "")))
    landBirdRasters <- c(birdLayer, 
                         sim$landscapeRaster, 
                         sim$ageRaster, 
                         sim$FNFRaster)
    
    
    ## take the values from the rasters and input 
    ## them to a data table called cellValues
    cellValues <- terra::values(landBirdRasters, dataframe = TRUE)
    cellValues <- setnames(cellValues, c( "birdDensity", 
                                          "landForClass", 
                                          "age", 
                                          "forestedStatus"))
    cellValues <- unite(cellValues, 
                        uniqueClasses, 
                        c(forestedStatus, 
                          landForClass), 
                        remove=FALSE)
    
    ## make sure landForClass and forestedStatus 
    ## are categorical rather than numerical
    cellValues$landForClass <- as.factor(cellValues$landForClass) 
    cellValues$forestedStatus <- as.factor(cellValues$forestedStatus)
    cellValues$uniqueClasses <- as.factor(cellValues$uniqueClasses)
    
    #get rid of any rows with NA values for bird density or landForClass
    cellValues <- cellValues[!is.na(cellValues$birdDensity), ]
    cellValues <- cellValues[!is.na(cellValues$landForClass), ]
    
    nrowCV <- nrow(cellValues)
    cellValues$birdSp <- rep(bird, nrowCV)
    
    print(paste(bird," dataset complete"))
    
    return(cellValues)
  })
  
  # # names(birdDatasets) <- names(birdRasters)
  # # for (i in names(birdDatasets)) {
  # #   attr(birdDatasets[[i]],"Species") <- i
  #   
  # }
 
  names(sim$birdDatasets) <- P(sim)$birdList
  
  sim$birdPreds1D <- lapply(X = P(sim)$birdList, FUN = function(bird){ 
   
    print(bird)
  #get birdPreds1D
  singleBirdDataset <-  eval(parse(text=paste("sim$birdDatasets$", bird, sep = "")))
  singleBirdDataset <- as.data.table(singleBirdDataset)
  birdStats <- singleBirdDataset[order(forestedStatus, 
                                       landForClass) 
                                 # order the rows by the land cover class
  ][,list(classCount = .N, 
          # get the number of cells 
          # each cover class
          #meanBirdDensity = mean(birdDensity), 
          # get the mean bird density 
          #for each cover class
          meanBirdDensity = mean(birdDensity),
          #get median bird density for each class
          medianBirdDensity = median(birdDensity),
          varBirdDensity = var(birdDensity), 
          # get the variance for bird density
          # for each cover class
          seBirdDensity = std.error(birdDensity), 
          # get the standard error
          #for bird density 
          # for each cover class
          normality_stat = tryCatch(ad.test(birdDensity)$statistic, error = function(cond) { return(NaN) }),
          normality_p = tryCatch(ad.test(birdDensity)$p.value, error = function(cond) { return(NaN) }),
          #ifelse(mean(birdDensity) > 0,                                    
          #tryCatch(ad.test(birdDensity)$p.value,
          #error = function(cond){return(NA)}), NA),
          unimodality_stat =   tryCatch(dip.test(birdDensity)$statistic, error = function(cond) { return(NaN) }),
          unimodality_p =   tryCatch(dip.test(birdDensity)$p.value, error = function(cond) { return(NaN) }),
          birdSp = bird),
    by = list(forestedStatus, 
              landForClass)]
  
  birdStats <- unite(birdStats, 
                     uniqueClasses, 
                     c(forestedStatus, 
                       landForClass), 
                     remove=FALSE)
  
  fileName <- paste(bird, "_birdPreds1D.csv")
  write.csv(birdStats, file =  file.path(outputFolderBirdPreds, fileName)) 
  
  return(birdStats)
})
 
names(sim$birdPreds1D) <- P(sim)$birdList
 
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

assess1D <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test
  # get summary of assumptions/stats for 1D 
  
  print("get assumptions summaries")
  #make single dataframe 
  birdPreds1DSingleFrame <- rbindlist(sim$birdPreds1D)
  
  #Get table of binary normality and unimodality 
  #If p value is less than or equal to 0.05 it fails the test and is not considered normal/unimodal
  #fail = 0, pass = 1
  assumpTab1D <- birdPreds1DSingleFrame[,c(1,2,10, 12, 13)]
  assumpTab1D$normal <- NA
  assumpTab1D$normal[assumpTab1D$normality_p < 0.05] <- 0
  assumpTab1D$normal[assumpTab1D$normality_p == 0.05] <- 0
  assumpTab1D$normal[assumpTab1D$normality_p > 0.05] <- 1
  
  
  
  assumpTab1D$unimodal <- NA
  assumpTab1D$unimodal[assumpTab1D$unimodality_p < 0.05] <- 0
  assumpTab1D$unimodal[assumpTab1D$unimodality_p == 0.05] <- 0
  assumpTab1D$unimodal[assumpTab1D$unimodality_p > 0.05] <- 1
  assumpTab1D <- assumpTab1D[,c(1,2,5,6,7)]
  
  #we make assumption that if there is an NA, it is not normal/unimodal
  assumpTab1D$normal[is.na(assumpTab1D$normal) == TRUE] <- 0
  assumpTab1D$unimodal[is.na(assumpTab1D$unimodal) == TRUE] <- 0
  assumpTab1D
  
  #get table of prop of birds with p values under 0.05 per class
  print("get assumptions by class 1D") 
  sim$assumptionsByClass1D <- assumpTab1D[order(uniqueClasses)][,list(noBirds = .N,
                                                                      propBirdsNormal = mean(normal),
                                                                      propBirdsUnimodal = mean(unimodal),
                                                                      binningType = "1DBins"),
                                                                by = uniqueClasses]
  write.csv(sim$assumptionsByClass1D, file =  file.path(outputFolderBirdPreds, "assumptionsByClass1D.csv")) 
  
  #get table of birds giving prop of classes with p values under 0.05
  print("get assumptions by bird 1D")
  sim$assumptionsByBird1D <- assumpTab1D[order(birdSp)][,list(noClasses = .N,
                                                              propClassesNormal = mean(normal),
                                                              propClassesUnimodal = mean(unimodal),
                                                              binningType = "1DBins"),
                                                        by = birdSp]
  write.csv(sim$assumptionsByBird1D, file =  file.path(outputFolderBirdPreds, "assumptionsByBird1D.csv")) 
  
  
  ### DO KERNEL DENSITY PLOT ###
  
  
  
  
  ### MAP OUT 1D PREDICTIONS
  
  #get non-Forest 1D data together
  print("get non-for 1D data")
  nf1DPreds <- lapply(X = P(sim)$birdList, FUN = function(bird) {
   
    #separate out data table rows that are forested, get rid of unnecessary forestedStatus column
    nonforestedDF <- as.data.table(eval(parse(text=paste("sim$birdPreds1D$", bird, sep = ""))))  
    nonforestedDF <- nonforestedDF[nonforestedDF$forestedStatus == "0"]
    nonforestedDF <- nonforestedDF[ , c(3,5)]
    nonforestedDF <- droplevels(nonforestedDF)
    
    return(nonforestedDF)
  })
  
  names(nf1DPreds) <- P(sim)$birdList
  
  
  #reclassify non forest raster to get map of 1D bird preds in non forested areas
  print("make nf1DMaps")
  sim$nf1DMaps <- lapply(X = P(sim)$birdList, FUN = function(bird){
    print(bird)
    
    nfBirdPreds <- eval(parse(text=paste("nf1DPreds$", bird, sep = "")))
    
    #make numeric
    nfBirdPreds <- nfBirdPreds[, landForClass:=as.numeric(landForClass)]
    nfBirdPreds <- nfBirdPreds[, meanBirdDensity:=as.numeric(meanBirdDensity)]
    #str(nfBirdPreds) #check
    
    #raster1DBins <- nonForRaster 
    raster1DBins <- terra::classify(sim$nonForRaster, nfBirdPreds)
    
    names(raster1DBins) <- paste(bird)
    #plot(raster1DBins)
    
    print(paste(bird,"nf 1D map raster complete"))
    return(raster1DBins)
  })
  
  names(sim$nf1DMaps) <- P(sim)$birdList
  
  #as Rdata file
  #save(sim$nf1DMaps,
  #     file =  file.path(outputFolderBirdPredsRasters, "nf1DMaps.Rdata"))
  # #load(file.path(outputFolderBirdPredsRasters, "nf1DMaps.Rdata"))
  
  #get Forest 1D data together
  print("get for1DPreds")
  for1DPreds <- lapply(X = P(sim)$birdList, FUN = function(bird) {
    
    #separate out data table rows that are forested, get rid of unnecessary forestedStatus column
    forestedDF <- as.data.table(eval(parse(text=paste("sim$birdPreds1D$", bird, sep = ""))))  
    forestedDF <- forestedDF[forestedStatus == "1"]
    forestedDF  <- forestedDF [ , c(3,5)]
    forestedDF <- droplevels(forestedDF)
    
    return(forestedDF)
  })
  
  names(for1DPreds) <- P(sim)$birdList
  
  
  #reclassify forest class raster to give 1D bird prediction values for each bird sp
  print("get for1DMaps")
  sim$for1DMaps <- lapply(X = P(sim)$birdList, FUN = function(bird){
    print(bird)
    
    nfBirdPreds <- eval(parse(text=paste("for1DPreds$", bird, sep = "")))
    
    #make numeric
    nfBirdPreds <- nfBirdPreds[, landForClass:=as.numeric(landForClass)]
    nfBirdPreds <- nfBirdPreds[, meanBirdDensity:=as.numeric(meanBirdDensity)]
    str(nfBirdPreds) #check
    
    #raster1DBins <- nonForRaster 
    raster1DBinsForest <- terra::classify(sim$forClassRaster, nfBirdPreds)
    
    names(raster1DBinsForest) <- paste(bird)
    #plot(raster1DBinsForest)
    
    
    print(paste(bird,"for 1D map raster complete"))
    return(raster1DBinsForest)
  })
  
  names(sim$for1DMaps) <- P(sim)$birdList
  
  #as Rdata file
  # save(sim$for1DMaps,
  #      file =  file.path(outputFolderBirdPredsRasters, "for1DMaps.Rdata"))
  #load(file.path(outputFolderBirdPredsRasters, "for1DMaps.Rdata"))
  
  
  # Get full 1D Map
  print("get for1DAndNf1DMaps")
  sim$for1DAndNf1DMaps <- lapply(X = P(sim)$birdList, FUN = function(bird){
    
    print(bird)
    raster1DBinsNF <- eval(parse(text=paste("sim$nf1DMaps$", bird, sep = "")))
    raster2DBinsFor <- eval(parse(text=paste("sim$for1DMaps$", bird, sep = "")))
    
    birdPredsRaster1D <- terra::cover(x = raster2DBinsFor,
                               y = raster1DBinsNF)
    
    names(birdPredsRaster1D) <- paste(bird)
    
    #birdPredsRaster1D #visually check Raster
    # clearPlot()
    # Plot(birdPredsRaster1D, na.color = "grey", zero.color = "black")
    
    print(birdPredsRaster1D)
    
    print(paste(bird,"for 1D and nf 1D map complete"))
    return(birdPredsRaster1D)
  })
  
  names(sim$for1DAndNf1DMaps) <- P(sim)$birdList
  
  print("save full 1D maps")
  
  #as Rdata file
  # save(sim$for1DAndNf1DMaps,
  #      file =  file.path(outputFolderBirdPredsRasters, "for1DAndNf1DMaps.Rdata"))
  #load(file.path(outputFolderBirdPredsRasters, "for1DAndNf1DMaps.Rdata"))
  
  #as tif files
  
  
  lapply(X = P(sim)$birdList, FUN = function(bird){
    raster <- eval(parse(text=paste("sim$for1DAndNf1DMaps$", bird, sep = "")))
    names(raster) <- paste(bird)
    terra::writeRaster(x = raster, 
                       filename = file.path(outputFolderBirdPredsRasters, paste(bird, "-for1DAndNf1DMap", sep = "")),
                       filetype= "GTiff",
                       gdal="COMPRESS=NONE",
                       overwrite = TRUE)
      })
  
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

do2DPreds <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test
  
 
  
  
  #get 2D bird predictions (by forest and age class)
  birdGBM <- lapply(X = sim$birdDatasets, FUN = function(birdDF) {
    
    #keep track of how long each bird takes
    #print(Sys.time()) 
    
    bird <- unique(birdDF$birdSp)
    print(bird)
    birdDF <- as.data.table(birdDF)
    
    #separate out data table rows that are forested
    forestedDF <- birdDF[forestedStatus == "1"]
    forestedDF <- forestedDF[, c(1,3,4)]
    forestedDF$age <- as.integer(forestedDF$age)
    forestedDF <- droplevels(forestedDF)
    
    #get rid of any rows with NA for age
    forestedDF <- na.omit(forestedDF, cols = "age")
   
    #fit gbm
    print("fit gbm")
    gbmFitted <- gbm::gbm(formula = birdDensity ~ ., 
                          distribution = "gaussian",
                          data = forestedDF,
                          interaction.depth = 2,
                          n.trees = P(sim)$nTrees,
                          #verbose = TRUE,
                          shrinkage = 0.3,
                          n.minobsinnode = 5,
                          bag.fraction = .80, 
                          train.fraction = 1) #same number of trees as used in predict.gbm
    
    #print summary of relative influence by the factors
    #par(mar = c(5, 8, 1, 1))
    relInfGBM <- summary(gbmFitted)
    #relInfGBM
    # print(relInfGBM)
    # 
    #get Freidman's h-stat
    FriedmansHStat <- gbm::interact.gbm(gbmFitted,
                                        data = forestedDF,
                                        i.var = c(1,2),
                                        n.trees = P(sim)$nTrees)
    # print(FriedmansHStat)
    # 
    # #check gbm plot
    # # plotGBM <- gbm::plot.gbm(gbmFitted, i.var = c(1,2))
    # # plot(plotGBM)
    # 
    #make into single summary object
    # statsGBM <- list(relInfGBM, FriedmansHStat) #, plotGBM)
    # names(statsGBM) <- c("relInfGBM", "FriedmansHStat") #, "plotGBM")
    relInfForClass <- relInfGBM[relInfGBM$var == 'landForClass',]
    relInfForClass <- relInfForClass$rel.inf
    relInfAge <- relInfGBM[relInfGBM$var == 'age',]
    relInfAge <- relInfAge$rel.inf
    birdSp <- bird
    statsGBM <- cbind(birdSp, FriedmansHStat, relInfForClass, relInfAge)
    
    #generate prediction df using expand
    sim$maxAge <- max(forestedDF$age) #get age of oldest cell listed in forestedDF
    sim$maxAgeClassAge <- P(sim)$maxAgeClass*P(sim)$ageGrouping 
   ifelse(sim$maxAge > sim$maxAgeClassAge, sim$allAges <- c(0:sim$maxAge), sim$allAges <- c(0:sim$maxAgeClassAge)) #make a vector that counts from 0 to the age of the oldest cell, or the max age class age, whichever is bigger
    birdPredictDF <- forestedDF %>% expand(landForClass, sim$allAges) #make a data frame with two columns, landForClass and sim$allAges.The rows cumulatively provide each combination of age and forest class. 
    names(birdPredictDF) <- c("landForClass", "age") #rename the two columns in birdPredictDF
    
    #do prediction 
    #(object, newdata, n.trees, type = "link", single.tree = FALSE,...)
    #This action produces a vector of predictions for the variables given by each row in birdPredictDF
    print("do gbmPred")
    gbmPred <- gbm::predict.gbm(object = gbmFitted,
                                newdata = birdPredictDF,
                                n.trees = P(sim)$nTrees,
                                type = "link", 
                                single.tree = FALSE)  
    
    noAgeClasses <- sim$maxAge/P(sim)$ageGrouping #get the number of age classes if the max age was simply divided by the age Grouping
    ageClasses <- rep(1:noAgeClasses, each = P(sim)$ageGrouping) 
    ageClasses <- ifelse(ageClasses < P(sim)$maxAgeClass, ageClasses, P(sim)$maxAgeClass)
    ageClasses <- c(1, ageClasses)
    gbmPredDF <- cbind(birdPredictDF, gbmPred, ageClasses)
    gbmPredDT <- as.data.table(gbmPredDF)
    gbmPredDT$landForClass <- as.factor(gbmPredDT$landForClass)
    gbmPredDT$ageClasses <- as.factor(gbmPredDT$ageClasses)
    sim$ageClasses <- ageClasses
    
    # gbmPredDF <- aggregate( gbmPred ~ ageClasses * landForClass, gbmPredDF, mean )
    gbmPredDT <- gbmPredDT[order(list(landForClass, ageClasses))  
    ][,list(gbmPred = mean(gbmPred)), 
      by = list(landForClass, ageClasses)]
    
    
    #form matrix with landForClass as y axis and age as x axis
    print("form birdMatrix")
    birdMatrix <- reshape2::acast(gbmPredDT, 
                                  landForClass~ageClasses, 
                                  value.var= "gbmPred")
    #save 2D birdPreds
    matrixName <- paste(bird, "_birdPreds2D.csv")
    write.csv(birdMatrix, file =  file.path(outputFolderBirdPreds, matrixName)) 
    
    #return(birdMatrix)
    matrixAndSummary <- list(birdMatrix, statsGBM)
    names(matrixAndSummary) <- c("birdMatricies", "statsGBM")
     
    return(matrixAndSummary)
    
  })
 
  names(birdGBM) <- P(sim)$birdList
  
  #separate matricies from summaries
  print("separate out matricies")
  
  sim$birdMatricies <- lapply(X = birdList, FUN = function(bird) {

    print(bird)
    birdMatrix <-  eval(parse(text=paste("birdGBM$", bird, "$birdMatricies", sep = "")))

    matrixName <- paste(bird, "_matrix.csv")
    write.csv(birdMatrix, file =  file.path(outputFolderBirdPreds, matrixName))

    return(birdMatrix)
  })
  
  names(sim$birdMatricies) <- P(sim)$birdList
  
  #make table of all GBM Stats
  print("make table of GBM stats")
  statsList <- lapply(X = birdList, FUN = function(bird) {
    
    print(bird)
    GBMStats <-  as.data.table(eval(parse(text=paste("birdGBM$", bird, "$statsGBM", sep = ""))))
    return(GBMStats)
  })
  
  sim$statsGBM <- rbindlist(statsList)
  write.csv(sim$statsGBM, file =  file.path(outputFolderBirdPreds, "statsGBM"))
  
  
  
  # create table defining age classes
  
  #diffMaxAge <- sim$maxAge- sim$maxAgeClassAge
   # if (!diffMaxAge < 1){
  #   ageClasses <- c(1, rep(1:P(sim)$maxAgeClass, each = P(sim)$ageGrouping),   rep(P(sim)$maxAgeClass, times = diffMaxAge)) #make vector of age classes to correspond with the vector allAges
  #   ageClassDefs <- cbind(sim$allAges, ageClasses)
  # } else {
  #   allAges <- c(allAges, (sim$maxAge+1):sim$maxAgeClassAge)
  #   ageClasses <- c(1, rep(1:maxAgeClass, each = P(sim)$ageGrouping)) #make vector of age classes to correspond with the vector allAges
  #   ageClassDefs <- cbind(sim$allAges,ageClasses)
  # }
  
  ageClassDefs <- as.data.table(cbind(sim$allAges, sim$ageClasses)) #makes data table that in one column has every age from 0 to max age (or max age class age if bigger), and the corresponding age class in the second column 
  names(ageClassDefs) <- c("allAges", "ageClasses")
  #ageClassDefs <- as.data.table(ageClassDefs) 
  sim$ageClassDefs <- ageClassDefs[, ageClasses:=as.character(ageClasses)] 
  write.csv(sim$ageClassDefs, file =  file.path(outputFolderBirdPreds, "ageClassDefs"))
 
  
  #make list object of all outputs needed for MB Module
  print("make birdPreds object")
  sim$birdPreds <- list(sim$birdMatricies, sim$birdPreds1D, sim$ageClassDefs) 
  names(sim$birdPreds) <- c("birdMatricies", "birdPreds1D", "ageClassDefs")
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

assess2D <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test

 
  
  #create birdStats2D (MODULE OUTPUT), a list of lists giving a birdStats2D data table and vector classesNotPresent for each bird species on the birdList.
  print("get birdStats2D")
  
  sim$birdStats2D <- lapply(X = P(sim)$birdList, FUN = function(bird) {
    
    print(bird)
    forestedDT <-  as.data.table(eval(parse(text=paste("sim$birdDatasets$", bird, sep = "")))) 
    #separate out data table rows that are forested from the raw birdDataset 
    forestedDT <- forestedDT[forestedStatus == "1"]
    forestedDT <- droplevels(forestedDT)
    
    #get rid of any rows with NA for age
    forestedDT <- na.omit(forestedDT, cols = "age")
    
    #get age classes for each row using the ageClassDefs table
    ageClass <- forestedDT[, age]
    ageClass <- as.data.table(ageClass)
    ageClass[] <- lapply(ageClass, function(x) sim$birdPreds$ageClassDefs$ageClasses[match(x, sim$birdPreds$ageClassDefs$allAges)])
    #add the ageClass to the forestedDT 
    birdDataNew <- cbind(forestedDT, ageClass)
    
    #create new column, landAgeClass, giving the uniqueLandClass and ageClass combined
    birdDataNew <- as.data.table(unite(birdDataNew, landAgeClass, c(uniqueClasses, ageClass), sep= ".", remove=FALSE))
    
    #produce data table of statistics on the bird Data based on the 2D bins
    singleBirdStats2D <- birdDataNew[order(landAgeClass) # order the rows by the land cover class
    ][,list(                             classCount = .N, # get the number of cells each cover class
                                         meanBirdDensity = mean(birdDensity), #get mean bird density
                                         medianBirdDensity = median(birdDensity),
                                         varBirdDensity = var(birdDensity), # get the variance for bird density for each class
                                         seBirdDensity = std.error(birdDensity), # get the standard error for bird density for each  class
                                         normality.p = tryCatch(ad.test(birdDensity)$p.value,error = function(cond) { return(NaN) }), #ifelse(mean(birdDensity) > 0, tryCatch(ad.test(birdDensity)$p.value,error = function(cond){return(NA)}), NA),
                                         unimodality.p =   dip.test(birdDensity)$p.value,
                                         birdSp = bird),
      by = list(landAgeClass)]
    
    #exclude any classes from table that have a sample size smaller than minStatsSample, a parameter
    singleBirdStats2D <- subset(singleBirdStats2D, classCount > P(sim)$min2DStatsSample)
    
    # #### get list of missing classes
    # landAgeClassesPresent <- unique(singleBirdStats2D$landAgeClass) #get classes that are represented in birdStats2D
    # 
    # #get all classes possible
    # landClasses <- rep(unique(birdDataNew$uniqueClasses), times = maxAgeClass)
    # landClasses <- as.data.table(landClasses)
    # ageClassReps <- rep(1:maxAgeClass, times = length(unique(birdDataNew$uniqueClasses)))
    # ageClassReps <- as.data.table(ageClassReps)
    # allPossibleClasses <- cbind(landClasses, ageClassReps)
    # allPossibleClasses <-  unite(allPossibleClasses, allPossibleClasses, c(landClasses, ageClassReps), sep= ".", remove=FALSE)
    # 
    # #get the classes not present in birdStats2D
    # classesNotPresent <- setdiff(allPossibleClasses$allPossibleClasses, landAgeClassesPresent)
    # 
    # ##make list object of all stats outputs
    # birdStatsList2D <- list(singleBirdStats2D, classesNotPresent)
    # names(birdStatsList2D) <- c("birdStats2D", "classesNotPresent")
    
    # return(birdStatsList2D)
    
    return(singleBirdStats2D)
    
  })
  
  names(sim$birdStats2D) <- P(sim)$birdList
  
  #get 2D stats Summary
  print("get stats summary 2D")
  #make single dataframe of 2D stats
  birdStats2DSingleFrame <- rbindlist(sim$birdStats2D)
  
  #Get table of binary normality and unimodality 
  assumpTab2D <- birdStats2DSingleFrame[,c(1,7,8,9)]
  assumpTab2D$normal <- NA
  assumpTab2D$normal[assumpTab2D$normality < 0.05] <- 0
  assumpTab2D$normal[assumpTab2D$normality == 0.05] <- 0
  assumpTab2D$normal[assumpTab2D$normality > 0.05] <- 1
  
  
  assumpTab2D$unimodal <- NA
  assumpTab2D$unimodal[assumpTab2D$unimodality < 0.05] <- 0
  assumpTab2D$unimodal[assumpTab2D$unimodality == 0.05] <- 0
  assumpTab2D$unimodal[assumpTab2D$unimodality > 0.05] <- 1
  assumpTab2D <- assumpTab2D[,c(1,4,5,6)]
  assumpTab2D
  
  #get table of prop of birds with p values under 0.05 per class
  sim$assumptionsByClass2D <- assumpTab2D[order(landAgeClass)][,list(noBirds = .N, 
                                                                     propBirdsNormal = mean(normal), 
                                                                     propBirdsUnimodal = mean(unimodal), 
                                                                     binningType = "2DBins"),
                                                               by = landAgeClass]
  write.csv(sim$assumptionsByClass2D, file =  file.path(outputFolderBirdPreds, "assumptionsByClass2D.csv"))
  
  #get table of birds giving prop of classes with p values under 0.05
  sim$assumptionsByBird2D <- assumpTab2D[order(birdSp)][,list(noClasses = .N, 
                                                              propClassesNormal = mean(normal),
                                                              propClassesUnimodal = mean(unimodal),
                                                              binningType = "2DBins"),
                                                        by = birdSp]
  write.csv(sim$assumptionsByBird2D, file =  file.path(outputFolderBirdPreds, "assumptionsByBird2D.csv"))
  
  
  ### GET 2D MAPS OF MATRIX PREDICTIONS
  print("GET 2D MAPS")
  
  print("make age class raster")
  #reclassify forAgeRaster into a raster of forest age classes
  ageReClassTab <- sim$birdPreds$ageClassDefs
  ageReClassTab <- ageReClassTab[ , ageClasses:=as.numeric(ageClasses)] #change data type of ageClassDefs
  str(ageReClassTab) #check
  ageClassRaster <- sim$ageRaster #make copy of forAgeRaster to be reclassified
  ageClassRaster <- terra::classify(ageClassRaster, ageReClassTab) #do the reclassification based on ageClassDefs
  names(ageClassRaster) <- "ageClassRaster" 
  sim$ageClassRaster <- ageClassRaster#check over the raster that has been reclassified
  print(sim$ageClassRaster)
  
 
    #get for2Dmaps
  print("get for2DMaps")
  sim$for2DMaps <- lapply(X = P(sim)$birdList, FUN = function(bird){
    
    print(bird)
    # check that spatial extent is the same for ageClassraster and forClassraster
    print("extent of forClassRaster same as ageClassRaster?")
    terra::ext(sim$forClassRaster) == terra::ext(sim$ageClassRaster)
    
    #reform matrix
    matrix <- eval(parse(text=paste("sim$birdPreds$birdMatricies$", bird, sep = "")))
    reclassTab2D <- reshape2::melt(matrix)
    colnames(reclassTab2D) <- c( "forClass","ageClass", "birdDensityPred")
    
    #reclassify Raster according to reclassTab2D, ageClassRaster and forClassRaster
    raster2DBins <- terra::rast(sim$forClassRaster); raster2DBins[] = NA #make an empty NA raster the same as forClassRaster
    
    #make dataframe of all the data in forClassRaster and ageClassRaster and give each cell/row a new definition column, birdDensityPred, from reclassTab2d
    f = data.frame(forClass=sim$forClassRaster[], ageClass=sim$ageClassRaster[])
    vec = c(1:nrow(f))
    f[,3] = vec
    m = merge(f, reclassTab2D, all.x=TRUE)
    colnames(m)[3] = "ord"
    m = m[order(m$ord),]
    #populate raster2DBins with the birdDensityPred row of the table m
    raster2DBins[] = m$birdDensityPred
    
    names(raster2DBins) <- paste(bird)
    
    #check the new raster
    raster2DBins
    
    print(paste(bird,"for 2D map raster complete"))
    
    return(raster2DBins)
  })
  
  names(sim$for2DMaps) <- P(sim)$birdList
  
  #as Rdata file
  
  # save(for2DMaps,
  #      file =  file.path(outputFolderBirdPredsRasters, "for2DMaps.Rdata"))
  # # #load(file.path(outputFolderBirdPredsRasters, "for2DMaps.Rdata"))
  # sim$for2DMaps <- for2DMaps
  
  # get 2D map with NF areas filled in with 1D predictions
  print("get for2DAndNf1DMaps")
  
  sim$for2DAndNf1DMaps <- lapply(X = P(sim)$birdList, FUN = function(bird){
    
    print(bird)
    raster1DBins <- eval(parse(text=paste("sim$nf1DMaps$", bird, sep = "")))
    raster2DBins <- eval(parse(text=paste("sim$for2DMaps$", bird, sep = "")))
    
    birdPredsRaster <- terra::cover(x = raster2DBins,
                             y = raster1DBins) 
    
    names(birdPredsRaster) <- paste(bird)
    
    #birdPredsRaster #visually check Raster
    #plot(birdPredsRaster)
    
    # writeRaster(x = birdPredsRaster, filename = file.path(outputFolderBirdPredsRasters, paste(bird, "-birdPredsRaster", sep = "")), format = "GTiff", overwrite = TRUE)
    
    print(paste(bird,"for 2D and nf 1D map raster complete"))
    return(birdPredsRaster)
  })
  
  names(sim$for2DAndNf1DMaps) <- birdList
  
  print("save for2DAndNf1DMaps")
  
  #as Rdata file
  
  # save(for2DAndNf1DMaps,
  #      file =  file.path(outputFolderBirdPredsRasters, "for2DAndNf1DMaps.Rdata"))
  # #load(file.path(outputFolderBirdPredsRasters, "for2DAndNf1DMaps.Rdata"))
  # 
  #as tif files
  
  
  lapply(X = P(sim)$birdList, FUN = function(bird){
    
    raster <- eval(parse(text=paste("sim$for2DAndNf1DMaps$", bird, sep = "")))
    names(raster) <- paste(bird)
    writeRaster(x = raster, filename = file.path(outputFolderBirdPredsRasters,
                                                 paste(bird, "-for2DAndNf1DMap", sep = "")),
                format = "GTiff",
                overwrite = TRUE)
    
  })
  
  
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #
  if (P(sim)$fromDrive == TRUE) {
    
    ### GET FILES FROM GOOGLE DRIVE ###
    #get RasterToMatch
    print("download rasterToMatch")
    rasterToMatch <-  googledrive::drive_download(
      file = P(sim)$rasterToMatchLocation,
      overwrite = TRUE,
      verbose = TRUE,
      path = file.path(inputsDir, P(sim)$rasterToMatchName)) #download rasterTomatch from drive
    sim$rasterToMatch <- terra::rast(file.path(inputsDir, P(sim)$rasterToMatchName)) #load raster into R
    
    # get studyArea shapefile
    print("get studyArea shapefile from drive")
    sim$studyArea <- prepInputs(targetFile = P(sim)$.studyAreaName,
                                url = P(sim)$studyAreaLocation,
                                archive = P(sim)$archiveStudyArea,
                                alsoExtract = "similar", #Extract other files with similar names
                                destinationPath = downloadFolderArea, #folder to download to
                                #fun = "terra::vect", #use the function shapefile
                                useTerra = TRUE,
                                targetCRS = crs(sim$rasterToMatch), #make crs same as rasterToMatch
                                overwrite = TRUE,
                                verbose = TRUE)
   
    #crop and mask rasterTomatch to studyArea
    sim$rasterToMatch <- terra::mask(terra::crop(sim$rasterToMatch, sim$studyArea), sim$studyArea)
    names(sim$rasterToMatch) <- "rasterToMatch"
    # dev()
    # clearPlot()
    # Plot(sim$rasterToMatch, na.color= "grey")
    
    # get forest class raster
    print("get forClassRaster from Drive")
    sim$forClassRaster <- prepInputs(targetFile = P(sim)$nameForClassRaster,
                                 url = P(sim)$folderUrlForClass,
                                 archive = P(sim)$archiveForClass,
                                 #Extract other files with similar names
                                 alsoExtract = "similar",
                                 #save the file to a folder in the
                                 #working directory called forestClassRasters
                                 destinationPath = downloadFolderForestClass,
                                 #use the function raster
                                 targetCRS = crs(sim$rasterToMatch),
                                 fun = "terra::rast",
                                 useTerra = TRUE,
                                 #use the specified rasterToMatch to reproject to
                                 rasterToMatch = sim$rasterToMatch,
                                 #studyArea = sim$studyArea,
                                 useCache = getOption("reproducible.useCache", TRUE),
                                 overwrite = TRUE,
                                 verbose = TRUE)
    
    names(sim$forClassRaster) <- c("forClassRaster")
    
    sim$forClassRaster[sim$forClassRaster == 0] <- NA
    
    # clearPlot()
    # Plot(sim$forClassRaster, na.color= "grey")
  
    #get non forest raster
    print("get nonForRaster from Drive")
    nonForRaster <- prepInputs(targetFile = P(sim)$nameNonForRaster,
                               url = P(sim)$folderUrlNonFor,
                               archive = P(sim)$archiveNonFor,
                               #Extract other files with similar names
                               alsoExtract = "similar",
                               #save the file to a folder in the
                               #working directory called forestClassRasters
                               destinationPath = downloadFolderForestClass,
                               #use the function raster
                               fun = "terra::rast",
                               targetCRS = crs(sim$rasterToMatch),
                               #use the specified rasterToMatch to reproject to
                               rasterToMatch = sim$rasterToMatch,
                               useTerra = TRUE,
                               #studyArea = sim$studyArea,
                               useCache = getOption("reproducible.useCache", TRUE),
                               overwrite = TRUE,
                               verbose = TRUE)
    
    #nonForRaster[nonForRaster == 0] <- NA
    
    names(nonForRaster) <- c("nonForRaster")
    sim$nonForRaster <- terra::mask(nonForRaster, sim$forClassRaster, inverse = TRUE)
    # sim$nonForRaster <- overlay(x = nonForRaster,
    #                         y = sim$forClassRaster,
    #                         fun = function(x, y) {
    #                           x[!is.na(y[])] <- NA
    #                           return(x)
    #                         })
    # clearPlot()
    # Plot(sim$nonForRaster, na.color= "grey")
    
    #get age raster
    print("get ageRaster from Drive")
    sim$ageRaster <- prepInputs(targetFile = P(sim)$nameAgeRaster,
                            url = P(sim)$folderUrlAge,
                            archive = P(sim)$archiveAge,
                            #Extract other files with similar names
                            alsoExtract = "similar",
                            #save the file to a folder in the working directory
                            #called forestClassRasters
                            destinationPath = downloadFolderForestClass,
                            #use the function raster
                            useTerra = TRUE,
                            fun = "terra::rast",
                            #targetCRS = crs(sim$rasterToMatch),
                            #use the specified rasterToMatch to reproject to
                            rasterToMatch = sim$rasterToMatch,
                            #studyArea = sim$studyArea,
                            useCache = getOption("reproducible.useCache", TRUE),
                            overwrite = TRUE,
                            verbose = TRUE)
    
    names(sim$ageRaster) <- c("ageRaster")
    
    #get bird density rasters
    patternNameBirdRaster <- "mosaic-" #choose naming pattern to look for

    ## drive_ls function is used to list all the files it finds using the folder url with the given pattern
        filesToDownload <-
          googledrive::drive_ls(path = as_id(P(sim)$folderUrlBirdRaster),
                                pattern = patternNameBirdRaster)
     
          print(filesToDownload$name)

         # grepl function searches for all items in the filesToDownload that are on birdList & stores their names in rastersforBirdList
             sim$rastersForBirdList <-
               filesToDownload$name[grepl(pattern = paste(P(sim)$birdList, collapse = "|"),
                                          x = filesToDownload$name)]

                 ## for each item in turn from rastersForBirdlist the following function is applied:
             sim$birdRasters <-
               lapply(
                 X = sim$rastersForBirdList,
                 FUN = function(rasterFile) {
                   
                   nameBird <- substr(rasterFile, 8, 11) #works for strings of the form "mosaic-XXXX-run3.tif"
                   nameBird <- paste(nameBird, ".tif", sep = "")
               
                   ## if the item in rastersForBirdList is not already present at rastersPath, googledrive package downloads it
                   if (!file.exists(file.path(downloadFolderBird, rasterFile))) {
                     googledrive::drive_download(
                       file = as_id(filesToDownload[filesToDownload$name %in% rasterFile,]$id),
                        overwrite = TRUE,
                       path = file.path(downloadFolderBird, nameBird)

                     )
                   }
                  
                   ## otherwise, if it is already present and downloaded, just get the name of the item
                   return(terra::rast(file.path(downloadFolderBird, nameBird)))
                 }
               )

             #get the species codes as names for the downloadedRasters object, rather than using the whole filepath
             X <- lapply(sim$rastersForBirdList, substr, 8, 11) #works for strings of the form "mosaic-XXXX-run3.tif"
             names(sim$birdRasters) <- X
            
           sim$birdRasters <- lapply(X = sim$birdRasters, FUN = function(RasterLayer) {
             ## the function postProcesses the layer, cropping and masking it to a given study area and rasterToMatch, and saving it to a given destination path

             proRaster <- reproducible::postProcess(RasterLayer,
                                      #studyArea = sim$studyArea,
                                      rasterToMatch = sim$rasterToMatch,
                                      useTerra = TRUE,
                                      fun = "terra::rast",
                                      destinationPath = downloadFolderBird,
                                      filename2 = paste(downloadFolderBird, "/", names(RasterLayer), ".tif", sep = ""),
                                      overwrite = TRUE,
                                      verbose = TRUE)
             # clearPlot()
             # Plot(proRaster, na.color= "grey")
             return(proRaster)
           })

 
  } else {
    
    ### GET FILES FROM LOCAL LOCATION ###
    browser()
    #get rasterToMatch
    print("get rasterTomatch from local drive")
    sim$rasterToMatch <- terra::rast(file.path(P(sim)$rasterToMatchLocation))
   
    
    #get StudyArea shapefile
    print("get studyArea shapefile from local drive")
    sim$studyArea <- terra::vect(file.path(P(sim)$studyAreaLocation))
    
    #postProcess studyArea
    sim$studyArea <- reproducible::postProcess(sim$studyArea,
                                               destinationPath = downloadFolderArea,
                                               filename2 = "studyArea", 
                                               useTerra = TRUE,
                                               fun = "terra::vect", #use the function vect
                                               targetCRS = crs(sim$rasterToMatch), #make crs same as rasterToMatch
                                               overwrite = TRUE,
                                               verbose = TRUE)
    
    #crop and mask rasterToMatch
    sim$rasterToMatch <- terra::mask(terra::crop(sim$rasterToMatch, sim$studyArea), sim$studyArea) 
    names(sim$rasterToMatch) <- "rasterToMatch"
  }
  
  #make landscape raster
  print("make landscapeRaster")
  sim$landscapeRaster <- terra::cover(x = sim$forClassRaster, y = sim$nonForRaster)
  
  names(sim$landscapeRaster) <- c("landscapeRaster")
  
  # clearPlot()
  # Plot(sim$landscapeRaster, na.color= "grey")
 
  #create a raster that gives if an area is forest or not 
  #(0 for non-forest, 1 for forest)
  #This will allow me to have a value in the birdDataset 
  #that says if a cell was forested or not
  print("make FNFRaster")
  #make forest 1
  uniqueValsFR <- terra::unique(sim$forClassRaster,incomparables=FALSE)
  newValsFR <-  as.factor(rep("1", length(uniqueValsFR$forClassRaster))) #make vector that repeats 1 as amy times as there are unique values
  newValsFR <- cbind(uniqueValsFR$forClassRaster, newValsFR)
  newValsFR <- na.omit(newValsFR)
  reclassMatrixFR <- matrix(newValsFR,
                            ncol=2, byrow = FALSE)
  rasterFR <- terra::classify(sim$forClassRaster,
                         reclassMatrixFR)
  #make nonFor 0
  #valsNF <- unique(sim$rasterToMatch) #get vector of all the unique values in the forClassRaster
  uniqueValsNF <- terra::unique(sim$rasterToMatch, incomparables=FALSE)
  newValsNF <-  rep(0, length(uniqueValsNF$rasterToMatch)) #make vector that repeats 0 as amy times as there are unique values
  newValsNF <- cbind(uniqueValsNF$rasterToMatch, newValsNF)
  newValsNF <- na.omit(newValsNF)
  reclassMatrixNF <- matrix(newValsNF,
                            ncol=2, byrow = FALSE)
  rasterNF <- terra::classify(sim$rasterToMatch,
                         reclassMatrixNF)
  
  sim$FNFRaster <- terra::cover(x = rasterFR, y = rasterNF)
  
  names(sim$FNFRaster) <- c("FNFRaster")

  # clearPlot()
  # Plot(sim$FNFRaster, na.color= "grey")
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot(data, aes(TheSample)) +
    geom_histogram(...)
}

### add additional events as needed by copy/pasting from above
