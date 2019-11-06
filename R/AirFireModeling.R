#' @docType package
#' @name AirFireModeling
#' @title Utilities for working with USFS AirFire BlueSky model output
#' @description A suite of utility functions providing functionality commonly
#' needed for working with BlueSky model output in R.
#'

# ----- Internal Package State -------------------------------------------------

AirFireModelingEnv <- new.env(parent = emptyenv())
AirFireModelingEnv$dataDir <- NULL

# ----- Data Directory Configuration -------------------------------------------

#' @docType data
#' @keywords environment
#' @name ModelDataDir
#' @title Directory for Modeling Data
#' @format Absolute path string.
#' @description This package maintains an internal directory location which
#' users can set using \code{setModelDataDir()}. All package functions use this
#' directory whenever datasets are created or loaded.
#'
#' The default setting when the package is loaded is \code{getwd()}.
#' @seealso \link{getModelDataDir}
#' @seealso \link{setModelDataDir}
NULL

#' @export
#' @title Get Package Data Directory
#' @description Returns the package data directory where model data is located.
#' @return Absolute path string.
#' @seealso \link{ModelDataDir}
#' @seealso \link{setModelDataDir}

getModelDataDir <- function() {
  if ( is.null(AirFireModelingEnv$dataDir) ) {
    stop(paste0(
      "No data directory found. ",
      "Please set a data directory with setModelDataDir('~/Data/Bluesky')"
      ), call.=FALSE)
  } else {
    return(AirFireModelingEnv$dataDir)
  }
}

#' @export
#' @title Set Package Data Directory
#' @param dataDir directory where model datasets are located
#' @description Sets the package data directory where model data is located.
#' If the directory does not exist, it will be created.
#' @return Silently returns previous value of data directory.
#' @seealso \link{ModelDataDir}
#' @seealso \link{getModelDataDir}

setModelDataDir <- function(dataDir) {
  old <- AirFireModelingEnv$dataDir
  dataDir <- path.expand(dataDir)
  tryCatch({
    if (!file.exists(dataDir)) dir.create(dataDir)
    AirFireModelingEnv$dataDir <- dataDir
  }, warning = function(warn) {
    warning("Invalid path name.")
  }, error   = function(err) {
    stop(paste0("Error in setModelDataDir(",dataDir,")."))
  })
  return(invisible(old))
}

#' @keywords internal
#' @export
#' @title Remove package data directory
#' @description Resets the package data directory to NULL. Used for internal
#' testing.
#' @return Silently returns previous value of data directory.
#' @seealso ModelDataDir
#' @seealso getModelDataDir
#' @seealso setModelDataDir
removeModelDataDir <- function() {
  old <- AirFireModelingEnv$dataDir
  AirFireModelingEnv$dataDir <- NULL
}

#' @title Find bs_grid model bouding box
#'
#' @param bs_grid a bs_grid object
#'
#' @description Quickly find the bluesky models bounded region.
#' @return a bounding box matrix
modelBoundingBox <- function(bs_grid) {

  model_bb <- rbind(
    cbind(min = min(bs_grid$longitude),
          max = max(bs_grid$longitude)),
    cbind(min = min(bs_grid$latitude),
          max = max(bs_grid$latitude))
  )

  rownames(model_bb) <- c('x', 'y')

  return(model_bb)

}

#' @title Internal function for model domain info
#' @description Used as base model info
#' @seealso bluesky_availiableModels
modelDomains <- function() {
  domain <- data.frame(check.names = FALSE,
                       'NAM84-0.15deg' = c(
                         CENTER_LATITUDE   =  38.5,
                         CENTER_LONGITUDE  = -95.0,
                         WIDTH_LONGITUDE   =  70.0,
                         HEIGHT_LATITUDE   =  36.0,
                         SPACING_LONGITUDE =  0.15,
                         SPACING_LATITUDE  =  0.15 ),

                       'GFS-0.15deg-CanadaUSA-p25deg-68N' = c(
                         CENTER_LATITUDE   =  47,
                         CENTER_LONGITUDE  = -110,
                         WIDTH_LONGITUDE   =  120.0,
                         HEIGHT_LATITUDE   =  50.0,
                         SPACING_LONGITUDE =  0.25,
                         SPACING_LATITUDE  =  0.25 ),

                       'AK-12km' = c(
                         CENTER_LATITUDE   =  62.5,
                         CENTER_LONGITUDE  = -155.0,
                         WIDTH_LONGITUDE   =  39.0,
                         HEIGHT_LATITUDE   =  25.0,
                         SPACING_LONGITUDE =  0.08,
                         SPACING_LATITUDE  =  0.08 ),

                       'NAM-3km' = c(
                         CENTER_LATITUDE   =  37.5,
                         CENTER_LONGITUDE  = -95.0,
                         WIDTH_LONGITUDE   =  70.0,
                         HEIGHT_LATITUDE   =  30.0,
                         SPACING_LONGITUDE =  0.04,
                         SPACING_LATITUDE  =  0.04 ),

                       'CANSAC-4km' = c(
                         CENTER_LATITUDE   =  38.8,
                         CENTER_LONGITUDE  = -119.0,
                         WIDTH_LONGITUDE   =  19.0,
                         HEIGHT_LATITUDE   =  16.0,
                         SPACING_LONGITUDE =  0.05,
                         SPACING_LATITUDE  =  0.05 ),

                       'CANSAC-1.33km' = c(
                         CENTER_LATITUDE   =  37.25,
                         CENTER_LONGITUDE  = -119.00,
                         WIDTH_LONGITUDE   =  10.00,
                         HEIGHT_LATITUDE   =  9.5,
                         SPACING_LONGITUDE =  0.02,
                         SPACING_LATITUDE  =  0.02 ),

                       'PNW-4km' = c(
                         CENTER_LATITUDE   =  45.00,
                         CENTER_LONGITUDE  = -118.30,
                         WIDTH_LONGITUDE   =  20.5,
                         HEIGHT_LATITUDE   =  10.5,
                         SPACING_LONGITUDE =  0.04,
                         SPACING_LATITUDE  =  0.04 ),

                       'PNW-1.33km' = c(
                         CENTER_LATITUDE   =  45.55,
                         CENTER_LONGITUDE  = -120.25,
                         WIDTH_LONGITUDE   =  12.15,
                         HEIGHT_LATITUDE   =  8.35,
                         SPACING_LONGITUDE =  0.01,
                         SPACING_LATITUDE  =  0.01 )
  )

  domain['MAX_LATITUDE',] <- domain[1,] + domain[4,]/2 # Max latitude
  domain['MIN_LATITUDE',] <- domain[1,] - domain[4,]/2 # min latitude
  domain['MAX_LONGITUDE',] <- domain[2,] + domain[3,]/2 # max longitude
  domain['MIN_LONGITUDE',] <- domain[2,] - domain[3,]/2 # min longitude

  return(domain)
}

#' @export
#' @title Find coordinates appropriate model
#' @description Determine what model to use based on the target coordinates
#' provided.
#'
#' @param longitude the target longitude
#' @param latitude the target latitude
#'
#' @return vectors of model(s)
bluesky_availiableModels <- function(longitude, latitude) {

  domain <- modelDomains()

  inside <- which(
    (latitude <= domain['MAX_LATITUDE',] & latitude >= domain['MIN_LATITUDE',]) &
      (longitude <= domain['MAX_LONGITUDE',] & longitude >= domain['MIN_LONGITUDE',])
  )

  models_inside <- names(domain[inside])

  return(models_inside)

}

#' @title Model information
#'
#' @param model the model to view info of.
#' @description Used to find out model information, such as region and sub-
#' directory structure.
#' @export

modelInfo <- function(model) {
  domain <- modelDomains()
  # NOTE: For most models, the subDir depends on the hour it was run.
  # NOTE: For instance: models run on the 00 hour tend to have 'carryover',
  #       'combined' and 'forecast' directories. Whereas a model on 12 tends to only
  #       contain 'forecast' directory.
  dirInfo <- data.frame( check.names = FALSE,
                         'NAM84-0.15deg' = c('carryover, combined, forecast'),
                         'GFS-0.15deg-CanadaUSA-p25deg-68N' = c('forecast'),
                         'AK-12km' = c('forecast'),
                         'NAM-3km' = c('forecast'),
                         'CANSAC-4km' = c('forecast'),
                         'CANSAC-1.33km' =c('forecast'),
                         'PNW-4km' = c('forecast'),
                         'PNW-1.33km' = c('forecast') )

  modelInfo <- rbind(domain, 'SUBDIR' = dirInfo)

  return(modelInfo)
}
