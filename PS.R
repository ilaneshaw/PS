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
  authors = structure(list(list(given = c(""), family = "", role = c("aut", "cre"), email = "", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(PS = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "PS.Rmd"), ## same file
  reqdPkgs = list(
    "PredictiveEcology/SpaDES.core@development", "ggplot2", "sf", "data.table", "terra",
    "LandR", "plotrix", "ggplot2", "ggpubr", "diptest", "nortest", "dplyr", "tidyr", "reshape2"
  ),
  parameters = bindrows(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(
      ".plots", "character", "screen", NA, NA,
      "Used by Plots function, which can be optionally used here"
    ),
    defineParameter(
      ".plotInitialTime", "numeric", start(sim), NA, NA,
      "Describes the simulation time at which the first plot event should occur."
    ),
    defineParameter(
      "doPredsInitialTime", "numeric", start(sim), NA, NA,
      "Describes the simulation time at which the first plot event should occur."
    ),
    defineParameter(
      "doPredsInterval", "numeric", NA, NA, NA,
      "Describes the simulation time interval between getPreds events."
    ),
    defineParameter(
      ".plotInterval", "numeric", NA, NA, NA,
      "Describes the simulation time interval between plot events."
    ),
    defineParameter(
      ".saveInitialTime", "numeric", NA, NA, NA,
      "Describes the simulation time at which the first save event should occur."
    ),
    defineParameter(
      ".saveInterval", "numeric", 1, NA, NA,
      "This describes the simulation time interval between save events."
    ),
    defineParameter(
      ".studyAreaName", "character", NA, NA, NA,
      "Human-readable name for the study area used - e.g., a hash of the study",
      "area obtained using `reproducible::studyAreaName()`"
    ),
    defineParameter(
      "only1DPS", "logical", FALSE, NA, NA,
      "do smoothing by cover class only (1D)? if FALSE smoothing will be done by forest type and age class where possible"
    ),
    defineParameter(
      "nTrees", "numeric", 5000, NA, NA,
      "number of trees for gbm to build"
    ),
    defineParameter(
      "maxAgeClass", "numeric", 17, NA, NA,
      "what the oldest age class will be (everything older will be included in this class)"
    ),
    defineParameter(
      "ageGrouping", "numeric", 10, NA, NA,
      "how many years included per age class"
    ),
    defineParameter(
      "spList", "character", NA, NA, NA,
      "a list of sp species in the format of 4-letter sp codes"
    ),
    defineParameter(
      "rasterToMatchLocation", "character", NA, NA, NA,
      "the file location of the rasterToMatch"
    ),
    defineParameter(
      "rasterToMatchName", "character", NA, NA, NA,
      "the name of the rasterToMatch file"
    ),
    defineParameter(
      "studyAreaLocation", "character", NA, NA, NA,
      "the file location of the studyArea"
    ),
    defineParameter(
      "nameBCR", "character", NA, NA, NA,
      "the BAM regional model BCR region that the studyArea is located in"
    ),
    defineParameter(
      "nameForClassRas", "character", NA, NA, NA,
      "the file name of the forest class raster"
    ),
    defineParameter(
      "locationForClass", "character", NA, NA, NA,
      "the location of the forest class raster"
    ),
    defineParameter(
      "archiveForClass", "character", NA, NA, NA,
      "the zip file the forest class raster is located in"
    ),
    defineParameter(
      "nameLandClassRas", "character", NA, NA, NA,
      "the file name of the land cover raster"
    ),
    defineParameter(
      "locationLandClass", "character", NA, NA, NA,
      "the location of the land cover raster"
    ),
    defineParameter(
      "archiveLandClass", "character", NA, NA, NA,
      "the zip file the land cover raster is located in"
    ),
    defineParameter(
      "nameAgeRas", "character", NA, NA, NA,
      "the file name of the age raster"
    ),
    defineParameter(
      "locationAge", "character", NA, NA, NA,
      "the location of the age raster"
    ),
    defineParameter(
      "archiveAge", "character", NA, NA, NA,
      "the zip file the age raster is located in"
    ),
    defineParameter(
      "locationSpRas", "character", NA, NA, NA,
      "the location of the sp density rasters"
    ),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(
      ".seed", "list", list(), NA, NA,
      "Named list of seeds to use for each event (names)."
    ),
    defineParameter(
      ".useCache", "logical", FALSE, NA, NA,
      "Should caching of events or module be used?"
    )
  ),
  inputObjects = bindrows(
    # expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput("rasterToMatch", "SpatRaster", desc = "A raster used to determine projection of other spatial objects. Must cover all of the region covered by the studyArea"),
    expectsInput("studyArea", "SpatVector", desc = "Polygon to use as the study area."),
    expectsInput(objectName = "forClassRas", objectClass = "SpatRaster", desc = NA, sourceURL = NA),
    expectsInput(objectName = "landClassRas", objectClass = "SpatRaster", desc = NA, sourceURL = NA),
    expectsInput(objectName = "ageRas", objectClass = "SpatRaster", desc = NA, sourceURL = NA),
    expectsInput(objectName = "spRasters", objectClass = NA, desc = NA, sourceURL = NA)
  ),
  outputObjects = bindrows(
    # createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = NA, objectClass = NA, desc = NA),
    createsOutput(objectName = "spDatasets", objectClass = NA, desc = NA, sourceURL = NA),
    createsOutput(objectName = "spPreds", objectClass = NA, desc = NA, sourceURL = NA),
    createsOutput(objectName = "statsGBM", objectClass = NA, desc = NA, sourceURL = NA)
  )
))

## event types
#   - type `init` is required for initialization

doEvent.PS <- function(sim, eventTime, eventType) {
  switch(eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)
      # checkObject(sim, name = Par$stackName, layer = "habitatQuality")

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim,
        eventTime = P(sim)$doPredsInitialTime,
        moduleName = "PS", eventType = "do1DPreds"
      )
      sim <- scheduleEvent(sim,
        eventTime = P(sim)$doPredsInitialTime,
        moduleName = "PS", eventType = "map1D"
      )

      if (P(sim)$only1DPS == FALSE) {
        sim <- scheduleEvent(sim,
          eventTime = P(sim)$doPredsInitialTime,
          moduleName = "PS", eventType = "do2DPreds"
        )
        sim <- scheduleEvent(sim,
          eventTime = P(sim)$doPredsInitialTime,
          moduleName = "PS", eventType = "map2D"
        )
      }

      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "PS", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "PS", "save")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # plotFun(sim) # example of a plotting function
      # # schedule future event(s)
      #
      # # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "PS", "plot")

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
      sim <- scheduleEvent(sim,
        eventTime = time(sim) + P(sim)$doPredsInterval,
        moduleName = "PS", eventType = "do1DPreds"
      )

      # ! ----- STOP EDITING ----- ! #
    },
    map1D = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      sim <- map1D(sim)
      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      sim <- scheduleEvent(sim,
        eventTime = time(sim) + P(sim)$doPredsInterval,
        moduleName = "PS", eventType = "map1D"
      )

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
      sim <- scheduleEvent(sim,
        eventTime = time(sim) + P(sim)$doPredsInterval,
        moduleName = "PS", eventType = "do2DPreds"
      )

      # ! ----- STOP EDITING ----- ! #
    },
    map2D = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      sim <- map2D(sim)
      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      sim <- scheduleEvent(sim,
        eventTime = time(sim) + P(sim)$doPredsInterval,
        moduleName = "PS", eventType = "map2D"
      )

      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
      "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'",
      sep = ""
    ))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # save the spDatasets tables
  print(
    "saving sp datasets"
  )
  save(sim$spDatasets,
    file = file.path(sim$outputPredsLocation, "spDatasets.Rdata")
  )
  # load(file.path(sim$outputPredsLocation, "spDatasets.Rdata"))

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #

  clearPlot()

  # Plot input landscape rasters ####

  ## Plot rasterToMatch ####
  quickPlot::Plot(sim$rasterToMatch, na.color = "blue", title = "rasterToMatch", new = TRUE, legend = TRUE)
  clearPlot()

  ## Plot landClassRas ####
  quickPlot::Plot(sim$landClassRas, na.color = "blue", title = "Land Class Raster", new = TRUE, legend = TRUE)
  clearPlot()

  ## Plot ageRas
  if (P(sim)$only1DPS == FALSE) {
    quickPlot::Plot(sim$ageRas, na.color = "blue", title = "Age Raster", new = TRUE, legend = TRUE)
    clearPlot()
  }

  ## Plot forClassRas ####
  quickPlot::Plot(sim$forClassRas, title = "Forest Class Raster", na.color = "blue", new = TRUE, legend = TRUE)
  clearPlot()

  # Plot 1DPS kernel density plots ####
  lapply(sim$spList, FUN = function(sp) {
    spDataset <- sim$spDatasets[[sp]]
    spDataset$landForClass <- as.factor(spDataset$landForClass)
    spDataset$species <- as.factor(spDataset$species)
    spDataset$age <- as.integer(spDataset$age)

    if (requireNamespace("ggplot2")) {
      kernelDensityPlot <- spDataset |> ggplot2::ggplot(ggplot2::aes(x = spDensity, fill = factor(landForClass))) +
        geom_density() +
        facet_wrap(~landForClass,
          scales = "free_y"
        ) +
        theme_classic() +
        ggtitle(paste("Kernel density plots for ", sp, " by forest or land cover class", sep = "")) +
        xlab("Predicted sp density") +
        ylab("Occurence of density value")
      clearPlot()
      Plot(kernelDensityPlot, title = NULL)
    }
  })


  # Plot 1DPS predictions
  lapply(sim$spList, FUN = function(sp) {
    spPred <- sim$spPreds1D[[sp]]
    spPred$landForClass <- as.factor(spPred$landForClass)
    spPred$FoLRaster <- as.factor(spPred$FoLRaster)
    spPred$species <- as.factor(spPred$species)

    if (requireNamespace("ggplot2")) {
      spPred1DPlot <- spPred |> ggplot2::ggplot(ggplot2::aes(
        x = landForClass,
        y = meanSpDensity,
        fill = landForClass
      )) +
        geom_bar(stat = "identity") +
        geom_errorbar(
          aes(
            ymin = meanSpDensity - seSpDensity,
            ymax = meanSpDensity + seSpDensity
          ),
          width = .5
        ) +
        theme_classic() +
        ggtitle(paste("Mean density predictions for ", sp, sep = "")) +
        xlab("Land or Forest Class") +
        ylab("Mean Predicted Density") +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank()
        )

      clearPlot()
      Plot(spPred1DPlot, title = NULL)
    }
  })

  # Plot 2DPS predictions ####
  if (P(sim)$only1DPS == FALSE) {
    lapply(sim$spList, FUN = function(sp) {
      spMatrix <- sim$spPreds$spMatricies[[sp]]

      tab <- reshape2::melt(spMatrix)
      colnames(tab) <- c("forClass", "ageClass", "spDensityPred")
      tab$ageClass <- as.factor(tab$ageClass)
      tab$forClass <- as.factor(tab$forClass)
      tab$spDensityPred <- as.numeric(tab$spDensityPred)

      if (requireNamespace("ggplot2")) {
        plot2DPredictions <- tab |> ggplot2::ggplot(ggplot2::aes(
          fill = forClass,
          y = spDensityPred,
          x = ageClass
        )) +
          geom_bar(position = "dodge", stat = "identity") +
          facet_grid(~forClass,
            scales = "free"
          ) +
          theme_classic() +
          ggtitle(paste("Predicted density by forest and age class for ", sp, sep = "")) +
          xlab("Forest Age Class") +
          ylab("Predicted sp density") +
          theme(
            strip.background = element_rect(
              color = "white", fill = "white", linewidth = 1.1
            ),
            axis.text.x = element_text(angle = 45, hjust = 1)
          )

        clearPlot()
        Plot(plot2DPredictions, title = NULL)
      }
    })
  }

  print("end PS plotting")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
do1DPreds <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #

  # Make sp datasets ####
  sim$spDatasets <- lapply(X = sim$spList, FUN = function(sp) {
    spLayer <- sim$spRasters[[sp]]
    if (P(sim)$only1DPS == FALSE) {
      ## 2DPS datasets ####

      ### gather rasters
      landSpRasters <- c(
        spLayer,
        sim$forClassRas,
        sim$landClassRas,
        sim$ageRas
      )
      print(landSpRasters)

      ### input raster values to data table
      spDataset <- terra::values(landSpRasters, dataframe = TRUE)
      spDataset <- setnames(spDataset, c(
        "spDensity",
        "forClass",
        "landClass",
        "age"
      ))
    } else {
      ## 1DPS datasets ####

      ### gather rasters
      landSpRasters <- c(
        spLayer,
        sim$forClassRas,
        sim$landClassRas
      )
      print(landSpRasters)
      ### input raster values to data table
      spDataset <- terra::values(landSpRasters, dataframe = TRUE)
      spDataset <- setnames(spDataset, c(
        "spDensity",
        "forClass",
        "landClass"
      ))
    }

    ## Tidy dataset ####
    spDataset <- as.data.table(spDataset)
    spDataset <- spDataset[!is.na(spDensity)] # get rid of cells with no spDensity data

    ### create column saying if value is from forest or land class raster
    spDataset <- spDataset[, FoLRaster := ifelse(!is.na(landClass) & !is.na(forClass), "forClass",
      ifelse(!is.na(landClass), "landClass",
        ifelse(!is.na(forClass), "forClass", NA)
      )
    )]
    spDataset <- spDataset[!is.na(FoLRaster)] # get rid of cells with no forest or land class data.

    ### set data classes
    spDataset$landClass <- as.factor(spDataset$landClass)
    spDataset$forClass <- as.factor(spDataset$forClass)
    spDataset$FoLRaster <- as.factor(spDataset$FoLRaster)
    spDataset$age <- as.integer(spDataset$age)
    spDataset$spDensity <- as.numeric(spDataset$spDensity)


    ### make column giving the land or forest class for each row.
    spDataset <- spDataset[, landForClass := dplyr::coalesce(forClass, landClass)]
    spDataset$landForClass <- as.factor(spDataset$landForClass)

    ### create column combining landForClass and FoLRaster columns to ensure class uniqueness
    spDataset <- tidyr::unite(spDataset, landForClass, c(FoLRaster, landForClass),
      remove = FALSE
    )
    print(sort(unique(spDataset$landForClass)))
    spDataset <- as.data.table(spDataset)

    ### add species column
    nrowCV <- nrow(spDataset)
    spDataset$species <- rep(sp, nrowCV)
    spDataset$studyArea <- rep(P(sim)$.studyAreaName, nrowCV)

    ### save dataset
    fileName <- paste(sp, "_fullDataset.csv", sep = "")
    write.csv(spDataset, file = file.path(sim$outputPredsLocation, fileName))

    print(paste(sp, " dataset complete"))

    return(spDataset)
  })

  names(sim$spDatasets) <- sim$spList

  # Calculate 1DPS preds ####
  sim$spPreds1D <- lapply(X = sim$spList, FUN = function(sp) {
    print(sp)

    singleSpDataset <- sim$spDatasets[[sp]]
    singleSpDataset <- as.data.table(singleSpDataset)

    spStats <- singleSpDataset[
      order(
        FoLRaster,
        landForClass
      ) # order the rows by the cover class
    ][, list(
      classCount = .N, # get the number of cells
      meanSpDensity = mean(spDensity), # get mean sp density
      medianSpDensity = median(spDensity), # get median species density
      varSpDensity = var(spDensity) * (.N - 1) / .N, # get the population variance for sp density
      seSpDensity = plotrix::std.error(spDensity), # get the standard error for sp density
      normality_stat = tryCatch(nortest::ad.test(spDensity)$statistic,
        error = function(cond) {
          return(NaN)
        }
      ), # normality test stat
      normality_p = tryCatch(nortest::ad.test(spDensity)$p.value,
        error = function(cond) {
          return(NaN)
        }
      ), # normality test p-value
      unimodality_stat = tryCatch(diptest::dip.test(spDensity)$statistic,
        error = function(cond) {
          return(NaN)
        }
      ), # unimodality test stat
      unimodality_p = tryCatch(diptest::dip.test(spDensity)$p.value,
        error = function(cond) {
          return(NaN)
        }
      ), # unimodality test p-value
      species = sp, # include column giving species
      studyArea = P(sim)$.studyAreaName
    ), # include column giving study area name
    by = list(
      FoLRaster,
      landForClass
    ) # get for each class
    ]

    print(spStats)

    ### save
    fileName <- paste(sp, "_spPreds1D.csv", sep = "")
    write.csv(spStats, file = file.path(sim$outputPredsLocation, fileName))

    return(spStats)
  })

  names(sim$spPreds1D) <- sim$spList

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

map1D <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #

  # Map out 1DPS predictions ####
  print("get 1DMaps")

  ### Separate land-cover class data ####
  print("get non-for 1D data")
  lc1DPreds <- lapply(X = sim$spList, FUN = function(sp) {
    # separate out data table rows that are forested, get rid of unnecessary forestedStatus column
    landCoverDT <- as.data.table(sim$spPreds1D[[sp]])
    landCoverDT <- landCoverDT[landCoverDT$FoLRaster == "landClass"]
    landCoverDT <- landCoverDT[, c("landForClass", "meanSpDensity")]
    landCoverDT <- droplevels(landCoverDT)
    landCoverDT$landForClass <- gsub("[^0-9]", "", landCoverDT$landForClass)

    return(landCoverDT)
  })

  names(lc1DPreds) <- sim$spList


  ## Reclassify land cover raster with 1DPS predictions ####
  print("make lc1DMaps")
  sim$lc1DMaps <- lapply(X = sim$spList, FUN = function(sp) {
    print(sp)
    lcSpPreds <- lc1DPreds[[sp]]

    lcSpPreds <- lcSpPreds[, landForClass := as.numeric(landForClass)]
    lcSpPreds <- lcSpPreds[, meanSpDensity := as.numeric(meanSpDensity)]

    raster1DBins <- terra::classify(sim$landClassRas, lcSpPreds)

    names(raster1DBins) <- paste(sp)

    print(paste(sp, "lc 1D map raster complete"))
    return(raster1DBins)
  })

  names(sim$lc1DMaps) <- sim$spList


  ## Separate 1DPS forest class predictions ####
  print("get for1DPreds")
  for1DPreds <- lapply(X = sim$spList, FUN = function(sp) {
    forestedDT <- as.data.table(sim$spPreds1D[[sp]])
    forestedDT <- forestedDT[FoLRaster == "forClass"]
    forestedDT <- forestedDT[, c("landForClass", "meanSpDensity")]
    forestedDT <- droplevels(forestedDT)
    forestedDT$landForClass <- gsub("[^0-9]", "", forestedDT$landForClass)

    return(forestedDT)
  })

  names(for1DPreds) <- sim$spList


  ## Reclassify forest class raster with 1DPS predictions ####
  print("get for1DMaps")
  sim$for1DMaps <- lapply(X = sim$spList, FUN = function(sp) {
    print(sp)

    forSpPreds <- for1DPreds[[sp]]
    forSpPreds <- forSpPreds[, landForClass := as.numeric(landForClass)]
    forSpPreds <- forSpPreds[, meanSpDensity := as.numeric(meanSpDensity)]

    # raster1DBins <- landClassRas
    raster1DBinsForest <- terra::classify(sim$forClassRas, forSpPreds)

    names(raster1DBinsForest) <- paste(sp)
    # plot(raster1DBinsForest)


    print(paste(sp, "for 1D map raster complete"))
    return(raster1DBinsForest)
  })

  names(sim$for1DMaps) <- sim$spList

  ## Combine forest and land cover Maps ####
  print("get for1DAndLc1DMaps")
  sim$for1DAndLc1DMaps <- lapply(X = sim$spList, FUN = function(sp) {
    print(sp)
    raster1DBinsLc <- sim$lc1DMaps[[sp]]
    raster1DBinsFor <- sim$for1DMaps[[sp]]

    spPredsRaster1D <- terra::cover(
      x = raster1DBinsFor,
      y = raster1DBinsLc
    )

    names(spPredsRaster1D) <- paste(sp)

    print(spPredsRaster1D)
    print(paste(sp, "for 1D and lc 1D map complete"))

    return(spPredsRaster1D)
  })

  names(sim$for1DAndLc1DMaps) <- sim$spList

  ### save
  print("save full 1D maps")
  lapply(X = sim$spList, FUN = function(sp) {
    raster <- sim$for1DAndLc1DMaps[[sp]]
    names(raster) <- paste(sp)
    terra::writeRaster(
      x = raster,
      filename = file.path(sim$outputRasLocation, paste(sp, "_for1DAndLc1DMap", sep = "")),
      filetype = "GTiff",
      gdal = "COMPRESS=NONE",
      overwrite = TRUE
    )
  })


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

do2DPreds <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #

  # Make 2DPS predictions ####
  spGBM <- lapply(X = sim$spDatasets, FUN = function(spDF) {
    ### keep track of how long each sp takes
    print(Sys.time())

    sp <- unique(spDF$species)
    print(sp)
    spDT <- as.data.table(spDF)

    ## Separate rows with forest data ####
    forestedDT <- spDT[FoLRaster == "forClass"]
    forestedDT <- forestedDT[, c("spDensity", "forClass", "age")]
    forestedDT$forClass <- as.factor(forestedDT$forClass)
    forestedDT$age <- as.integer(forestedDT$age)
    forestedDT <- droplevels(forestedDT)

    ### Remove age NAs
    forestedDT <- na.omit(forestedDT, cols = "age")

    ## Fit gbm ####
    print("fit gbm")
    gbmFitted <- gbm::gbm(
      formula = spDensity ~ .,
      distribution = "gaussian",
      data = forestedDT,
      interaction.depth = 2,
      n.trees = P(sim)$nTrees,
      # verbose = TRUE,
      shrinkage = 0.3,
      n.minobsinnode = 5,
      bag.fraction = .80,
      train.fraction = 1
    ) # same number of trees as used in predict.gbm

    ## Get gbm stats ####

    ### Get relInf
    relInfGBM <- summary(gbmFitted)
    relInfGBM

    ### Get H-stat
    FriedmansHStat <- gbm::interact.gbm(gbmFitted,
      data = forestedDT,
      i.var = c("forClass", "age"),
      n.trees = P(sim)$nTrees
    )
   
    ### make into single stats object
    relInfForClass <- relInfGBM[relInfGBM$var == "forClass", ]
    relInfForClass <- relInfForClass$rel.inf
    relInfAge <- relInfGBM[relInfGBM$var == "age", ]
    relInfAge <- relInfAge$rel.inf
    species <- sp
    statsGBM <- cbind(species, FriedmansHStat, relInfForClass, relInfAge)
    print(statsGBM)

    ## Make prediction df ####
    sim$maxAge <- max(forestedDT$age) # get age of oldest cell listed in forestedDF
    sim$maxAgeClassAge <- P(sim)$maxAgeClass * P(sim)$ageGrouping
    ifelse(sim$maxAge > sim$maxAgeClassAge,
      sim$allAges <- c(0:sim$maxAge),
      sim$allAges <- c(0:sim$maxAgeClassAge)
    ) # make a vector that counts from 0 to the age of the oldest cell, or the max age class age, whichever is bigger
    spPredictDT <- forestedDT %>% expand(
      forClass,
      sim$allAges
    ) # make a data frame with two columns, landForClass and sim$allAges.The rows cumulatively provide each combination of age and forest class.
    names(spPredictDT) <- c("forClass", "age") # rename the columns

    ## gbm pred ####
    ### This produces a vector of predictions for the variables given by each row in spPredictDF
    print("do gbmPred")
    gbmPred <- gbm::predict.gbm(
      object = gbmFitted,
      newdata = spPredictDT,
      n.trees = P(sim)$nTrees,
      type = "link",
      single.tree = FALSE
    )

    ## Aggregate predictions by age class ####
    noAgeClasses <- sim$maxAge / P(sim)$ageGrouping # get the number of age classes if the max age was simply divided by the age Grouping
    ageClasses <- ifelse(noAgeClasses < P(sim)$maxAgeClass, ageClasses <- P(sim)$maxAgeClass, ageClasses <- noAgeClasses)
    ageClasses <- rep(1:ageClasses, each = P(sim)$ageGrouping)
    ageClasses <- c(1, ageClasses)
    sim$ageClasses <- ageClasses
    if (noAgeClasses > P(sim)$maxAgeClass) {
      ageClassDiff <- noAgeClasses - P(sim)$maxAgeClass
      extraClasses <- P(sim)$maxAgeClass + (1:ageClassDiff)
      for (x in extraClasses) {
        print(x)
        sim$ageClasses <- replace(sim$ageClasses, sim$ageClasses == x, P(sim)$maxAgeClass)
      }
    }
    ageClasses <- sim$ageClasses
    gbmPredDT <- cbind(spPredictDT, gbmPred, ageClasses)
    gbmPredDT <- as.data.table(gbmPredDT)
    gbmPredDT$landForClass <- as.factor(gbmPredDT$forClass)
    gbmPredDT$ageClasses <- as.factor(gbmPredDT$ageClasses)
    gbmPredDT <- gbmPredDT[, list(gbmPred = mean(gbmPred)),
      by = list(forClass, ageClasses)
    ]


    ## Form matrix with forest class as y and age as x ####
    print("form spMatrix")
    spMatrix <- reshape2::acast(gbmPredDT,
      forClass ~ ageClasses,
      value.var = "gbmPred"
    )

    ### If any matrix prediction values are negative, make them be 0
    spMatrix[spMatrix < 0] <- 0
    print(spMatrix)

    ### Save
    matrixName <- paste(sp, "_spPreds2D.csv", sep = "")
    write.csv(spMatrix, file = file.path(sim$outputPredsLocation, matrixName))

    matrixAndSummary <- list(spMatrix, statsGBM)
    names(matrixAndSummary) <- c("spMatricies", "statsGBM")

    return(matrixAndSummary)
  })

  names(spGBM) <- sim$spList

  ### Separate matricies from stats ####
  #### Species matricies
  print("separate out matricies")
  sim$spMatricies <- lapply(X = sim$spList, FUN = function(sp) {
    print(sp)

    spMatrix <- eval(parse(text = paste("spGBM$", sp, "$spMatricies", sep = "")))
    matrixName <- paste(sp, "_matrix.csv", sep = "")

    ### Save
    write.csv(spMatrix, file = file.path(sim$outputPredsLocation, matrixName))

    return(spMatrix)
  })

  names(sim$spMatricies) <- sim$spList

  ####  GBM Stats
  print("make table of GBM stats")
  statsList <- lapply(X = sim$spList, FUN = function(sp) {
    print(sp)
    GBMStats <- as.data.table(eval(parse(text = paste("spGBM$", sp, "$statsGBM", sep = ""))))
    return(GBMStats)
  })

  sim$statsGBM <- rbindlist(statsList)

  # save
  write.csv(sim$statsGBM, file = file.path(sim$outputPredsLocation, "statsGBM.csv"))

  ## Make table defining age classes ####
  ageClassDefs <- as.data.table(cbind(sim$allAges, sim$ageClasses)) # makes data table that in one column has every age from 0 to max age (or max age class age if bigger), and the corresponding age class in the second column
  names(ageClassDefs) <- c("allAges", "ageClasses")
  # ageClassDefs <- as.data.table(ageClassDefs)
  sim$ageClassDefs <- ageClassDefs[, ageClasses := as.character(ageClasses)]
  ### save
  write.csv(sim$ageClassDefs, file = file.path(sim$outputPredsLocation, "ageClassDefs"))


  ### Make list of objects for examinePS module
  print("make spPreds object")
  sim$spPreds <- list(sim$spMatricies, sim$spPreds1D, sim$ageClassDefs)
  names(sim$spPreds) <- c("spMatricies", "spPreds1D", "ageClassDefs")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

map2D <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #

  # Map 2DPS predictions ####
  print("MAKE 2D MAPS")

  print("Make age class raster")
  ## Make age class raster ####
  ### Reclassify ageRas into age classes
  ageReClassTab <- sim$spPreds$ageClassDefs
  ageReClassTab <- ageReClassTab[, ageClasses := as.numeric(ageClasses)] # change data type of ageClassDefs
  sim$ageClassRas <- sim$ageRas # make copy of forAgeRaster to be reclassified
  sim$ageClassRas <- terra::classify(sim$ageClassRas, ageReClassTab) # do the reclassification based on ageClassDefs
  names(sim$ageClassRas) <- "ageClassRas"
  sim$ageClassRas <- terra::mask(terra::crop(sim$ageClassRas, sim$forClassRas), sim$forClassRas)
  print(sim$ageClassRas) # check

  ## Map 2DPS predictions to forest areas ####
  print("Make for2DMaps")
  sim$for2DMaps <- lapply(X = sim$spList, FUN = function(sp) {
    print(sp)

    ### check spatial extent is the same for ageClassraster and forClassraster
    print("extent of forClassRas same as ageClassRas?")
    print(terra::ext(sim$forClassRas) == terra::ext(sim$ageClassRas))
    print("extent of forClassRas same as the for1DMap Raster?")
    print(terra::ext(sim$for1DMaps[[sp]]) == terra::ext(sim$forClassRas))
    print("same number of cells forClassRas  as the for1DMap Raster?")
    print(length(terra::values(sim$for1DMaps[[sp]])) == length(terra::values(sim$forClassRas)))


    ## Make reclassification tab ####
    matrix <- sim$spPreds$spMatricies[[sp]]

    reclassTab2D <- reproducible::Cache(reshape2::melt, matrix)
    colnames(reclassTab2D) <- c("forClass", "ageClass", "spDensityPred")
    reclassTab2D <- as.data.table(reclassTab2D)
    reclassTab2D$forClass <- as.factor(reclassTab2D$forClass)
    reclassTab2D$ageClass <- as.factor(reclassTab2D$ageClass)


    ## Convert forClassRas and ageClassRas to data frame ####
    rasDF <- as.data.frame(c(sim$forClassRas, sim$ageClassRas), xy = TRUE)
    names(rasDF) <- c("x_coord", "y_coord", "forClass", "ageClass")

    ### Add densities from reclassTab
    rasDF <- merge(rasDF, reclassTab2D, by = c("forClass", "ageClass"), all.x = TRUE)

    ## Rebuild raster from coordinates, and density predictions ####
    raster2DPS <- terra::rast(rasDF[, c("x_coord", "y_coord", "spDensityPred")],
      type = "xyz",
      crs = terra::crs(sim$forClassRas)
    )

    ### check
    terra::compareGeom(raster2DPS, sim$forClassRas, messages = TRUE)
    print(raster2DPS)

    return(raster2DPS)
  })

  names(sim$for2DMaps) <- sim$spList

  ## make 2D map with Land cover areas filled in with 1D predictions ####
  print("get for2DAndLc1DMaps")

  sim$for2DAndLc1DMaps <- lapply(X = sim$spList, FUN = function(sp) {
    print(sp)
    raster1DBins <- sim$lc1DMaps[[sp]]
    raster2DBins <- sim$for2DMaps[[sp]]

    spPredsRaster <- terra::cover(
      x = raster2DBins,
      y = raster1DBins
    )

    names(spPredsRaster) <- paste(sp)

    print(paste(sp, "for 2D and lc 1D map raster complete"))
    return(spPredsRaster)
  })

  names(sim$for2DAndLc1DMaps) <- sim$spList

  print("save for2DAndLc1DMaps")

  ### save
  lapply(X = sim$spList, FUN = function(sp) {
    raster <- sim$for2DAndLc1DMaps[[sp]]
    names(raster) <- paste(sp)
    terra::writeRaster(
      x = raster,
      filename = file.path(sim$outputRasLocation, paste(sp, "-for2DAndLc1DMap", sep = "")),
      filetype = "GTiff",
      gdal = "COMPRESS=NONE",
      overwrite = TRUE
    )
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

  # cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #


  # Get rasterToMatch ####
  if (!suppliedElsewhere("rasterToMatch", sim)) {
    print("get rasterToMatch from local drive")
    sim$rasterToMatch <- terra::rast(file.path(P(sim)$rasterToMatchLocation, P(sim)$rasterToMatchName))
    names(sim$rasterToMatch) <- "rasterToMatch"
  }

  # Get studyArea shapefile ####
  if (!suppliedElsewhere("studyArea", sim)) {
    print("get studyArea shapefile from local drive")
    studyArea <- terra::vect(file.path(P(sim)$studyAreaLocation, P(sim)$.studyAreaName))

    # postProcess studyArea
    sim$studyArea <- reproducible::Cache(reproducible::postProcessTo,
      from = studyArea,
      to = sim$rasterToMatch,
      overwrite = FALSE,
      verbose = TRUE
    )
  }

  # crop and mask rasterToMatch to studyArea
  sim$rasterToMatch <- reproducible::Cache(terra::crop, sim$rasterToMatch, sim$studyArea)
  sim$rasterToMatch <- reproducible::Cache(terra::mask, sim$rasterToMatch, sim$studyArea)

  # Get forest class raster ####
  if (!suppliedElsewhere("forClassRas", sim)) {
    print("get forClassRas")
    sim$forClassRas <- terra::rast(file.path(P(sim)$locationForClass, P(sim)$nameForClassRas))
    names(sim$forClassRas) <- c("forClassRas")

    # postprocess raster - cropping, masking to the study area and reprojecting to the rasterToMatch
    sim$forClassRas <- reproducible::Cache(reproducible::postProcessTo,
      from = sim$forClassRas,
      to = sim$rasterToMatch,
      cropTo = sim$studyArea,
      maskTo = sim$studyArea,
      overwrite = FALSE,
      verbose = TRUE
    )

    print(sim$forClassRas)
  }

  # Get land cover raster ####
  if (!suppliedElsewhere("landClassRas", sim)) {
    print("get landClassRas")
    sim$landClassRas <- terra::rast(file.path(P(sim)$locationLandClass, P(sim)$nameLandClassRas))
    names(sim$landClassRas) <- c("landClassRas")

    # postprocess raster - cropping, masking to the study area and reprojecting to the rasterToMatch
    sim$landClassRas <- reproducible::Cache(reproducible::postProcessTo,
      from = sim$landClassRas,
      to = sim$rasterToMatch,
      cropTo = sim$studyArea,
      maskTo = sim$studyArea,
      overwrite = FALSE,
      verbose = TRUE
    )

    # mask out areas covered by forest class raster
    sim$landClassRas <- terra::mask(sim$landClassRas, sim$forClassRas, inverse = TRUE)

    print(sim$landClassRas)
  }


  # Get ageRas ####
  if (P(sim)$only1DPS == FALSE) {
    if (!suppliedElsewhere("ageRas", sim)) {
      print("get ageRas")
      sim$ageRas <- terra::rast(file.path(P(sim)$locationAge, P(sim)$nameAgeRas))
      names(sim$ageRas) <- c("ageRas")

      # postprocess raster - cropping, masking to the study area and reprojecting to the rasterToMatch
      sim$ageRas <- reproducible::Cache(reproducible::postProcessTo,
        from = sim$ageRas,
        to = sim$rasterToMatch,
        cropTo = sim$studyArea,
        maskTo = sim$studyArea,
        overwrite = FALSE,
        verbose = TRUE
      )

      print(sim$ageRas)
    }
  }


  # Get sp density rasters ####
  if (!suppliedElsewhere("spRasters", sim)) {
    sim$spList <- sort(sim$spList)

    # list all species rasters in locationSpRas
    namesAllSpRast <- list.files(path = P(sim)$locationSpRas, full.names = FALSE)

    ## input the raster for each species in the spList one by one
    sim$spRasters <- lapply(X = P(sim)$spList, FUN = function(sp) {
      print(sp)
      tryCatch(
        {
          # sp <- paste(sp, ".tif", sep = "")

          spRas <- namesAllSpRast[grep(sp, basename(namesAllSpRast))]

          if (length(spRas) == 0) {
            print(paste("No file found in namesAllSpRast for ", sp, sep = ""))
          } else if (length(spRas) > 1) {
            print(paste("Multiple files matched ", sp,
              ":", spRas,
              sep = ""
            ))
          }

          spRas <- terra::rast(file.path(P(sim)$locationSpRas, spRas))
          names(spRas) <- sp

          # postprocess raster - cropping, masking to the study area and reprojecting to the rasterToMatch
          spRas <- reproducible::postProcessTo(
            from = spRas,
            to = sim$rasterToMatch,
            cropTo = sim$studyArea,
            maskTo = sim$studyArea,
            overwrite = FALSE,
            verbose = TRUE
          )

          return(spRas)
        },
        error = function(e) {
          return(NA)
        }
      )
    })

    names(sim$spRasters) <- P(sim)$spList
  }


  # Remove any NAs in the spRasters List ####
  naSp <- is.na(sim$spRasters)
  if (any(naSp)) {
    warning(paste(
      "no rasters for the following species:",
      paste(names(sim$spRasters)[naSp], collapse = ", ")
    ))
  }
  sim$spRasters <- sim$spRasters[!naSp]

  # change the spList to reflect only present species rasters (updated from the spList parameter)
  sim$spList <- names(sim$spRasters)

  # Set separate folders for output components ####
  sim$outputPredsLocation <- checkPath(file.path(Paths$outputPath, "PSPreds/"), create = TRUE)
  sim$outputRasLocation <- checkPath(file.path(Paths$outputPath, "PSRas/"), create = TRUE)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot(data, aes(TheSample)) +
    geom_histogram(...)
}
