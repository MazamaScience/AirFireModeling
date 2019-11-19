#' @docType package
#' @name AirFireModeling
#' @title Utilities for working with USFS AirFire BlueSky model output
#' @description A suite of utility functions providing functionality commonly
#' needed for working with BlueSky model output in R.
#'

# ----- Internal Package State -------------------------------------------------

AirFireModelingEnv <- new.env(parent = emptyenv())
AirFireModelingEnv$dataDir <- NULL

# ----- Internal Data ----------------------------------------------------------

#' bluesky_modelInfo
#'
#' @export
#' @docType data
#' @name bluesky_modelInfo
#' @title BlueSky Model Information
#' @format A tibble
#' @description Various information regarding BlueSky models with one model per
#' row.
bluesky_modelInfo <- dplyr::bind_rows(
  list(

    "NAM84-0.15deg" = list(
      model = "NAM84-0.15deg",
      CENTER_LATITUDE   =  38.5,
      CENTER_LONGITUDE  = -95.0,
      WIDTH_LONGITUDE   =  70.0,
      HEIGHT_LATITUDE   =  36.0,
      SPACING_LONGITUDE =  0.15,
      SPACING_LATITUDE  =  0.15,
      MAX_LATITUDE      =  56.5,
      MIN_LATITUDE      =  20.5,
      MAX_LONGITUDE     = -60.0,
      MIN_LONGITUDE     = -130.0
      # TODO:  Add more information to all entries as needed
    ),

    "GFS-0.15deg-CanadaUSA-p25deg-68N" = list(
      model = "GFS-0.15deg-CanadaUSA-p25deg-68N",
      CENTER_LATITUDE   =  47,
      CENTER_LONGITUDE  = -110,
      WIDTH_LONGITUDE   =  120.0,
      HEIGHT_LATITUDE   =  50.0,
      SPACING_LONGITUDE =  0.25,
      SPACING_LATITUDE  =  0.25,
      MAX_LATITUDE      =  72.0,
      MIN_LATITUDE      =  22.0,
      MAX_LONGITUDE     = -50.0,
      MIN_LONGITUDE     = -170.0
    ),

    "AK-12km" = list(
      model = "AK-12km",
      CENTER_LATITUDE   =  62.5,
      CENTER_LONGITUDE  = -155.0,
      WIDTH_LONGITUDE   =  39.0,
      HEIGHT_LATITUDE   =  25.0,
      SPACING_LONGITUDE =  0.08,
      SPACING_LATITUDE  =  0.08,
      MAX_LATITUDE      =  75.0,
      MIN_LATITUDE      =  50.0,
      MAX_LONGITUDE     = -135.50,
      MIN_LONGITUDE     = -174.50
    ),

    "NAM-3km" = list(
      model = "NAM-3km",
      CENTER_LATITUDE   =  37.5,
      CENTER_LONGITUDE  = -95.0,
      WIDTH_LONGITUDE   =  70.0,
      HEIGHT_LATITUDE   =  30.0,
      SPACING_LONGITUDE =  0.04,
      SPACING_LATITUDE  =  0.04,
      MAX_LATITUDE      =  52.50,
      MIN_LATITUDE      =  22.50,
      MAX_LONGITUDE     = -60.0,
      MIN_LONGITUDE     = -130.0
    ),

    "CANSAC-4km" = list(
      model = "CANSAC-4km",
      CENTER_LATITUDE   =  38.8,
      CENTER_LONGITUDE  = -119.0,
      WIDTH_LONGITUDE   =  19.0,
      HEIGHT_LATITUDE   =  16.0,
      SPACING_LONGITUDE =  0.05,
      SPACING_LATITUDE  =  0.05,
      MAX_LATITUDE      =  46.80,
      MIN_LATITUDE      =  30.80,
      MAX_LONGITUDE     = -109.50,
      MIN_LONGITUDE     = -128.50
    ),

    "CANSAC-1.33km" = list(
      model = "CANSAC-1.33km",
      CENTER_LATITUDE   =  37.25,
      CENTER_LONGITUDE  = -119.00,
      WIDTH_LONGITUDE   =  10.00,
      HEIGHT_LATITUDE   =  9.5,
      SPACING_LONGITUDE =  0.02,
      SPACING_LATITUDE  =  0.02,
      MAX_LATITUDE      =  42.0,
      MIN_LATITUDE      =  32.5,
      MAX_LONGITUDE     = -114.0,
      MIN_LONGITUDE     = -124.0
    ),

    "PNW-4km" = list(
      model = "PNW-4km",
      CENTER_LATITUDE   =  45.00,
      CENTER_LONGITUDE  = -118.30,
      WIDTH_LONGITUDE   =  20.5,
      HEIGHT_LATITUDE   =  10.5,
      SPACING_LONGITUDE =  0.04,
      SPACING_LATITUDE  =  0.04,
      MAX_LATITUDE      =  50.25,
      MIN_LATITUDE      =  39.75,
      MAX_LONGITUDE     = -108.05,
      MIN_LONGITUDE     = -128.55
    ),

    "PNW-1.33km" = list(
      model = "PNW-1.33km",
      CENTER_LATITUDE   =  45.55,
      CENTER_LONGITUDE  = -120.25,
      WIDTH_LONGITUDE   =  12.15,
      HEIGHT_LATITUDE   =  8.35,
      SPACING_LONGITUDE =  0.01,
      SPACING_LATITUDE  =  0.01,
      MAX_LATITUDE      =  49.375,
      MIN_LATITUDE      =  41.375,
      MAX_LONGITUDE     = -114.175,
      MIN_LONGITUDE     = -126.325
    )

  ) # END of list()
) # EMD of dplyr::bind_rows()

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

#' @export
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @title Find coordinates appropriate model
#' @description Determine what model to use based on the target coordinates
#' provided.
#'
#' @param longitude the target longitude
#' @param latitude the target latitude
#'
#' @return vectors of model(s)
bluesky_availiableModels <- function(longitude, latitude) {

  # Use the information found in bluesky_modelInfo
  models <-
    bluesky_modelInfo %>%
    dplyr::filter(
      longitude >= .data$MIN_LONGITUDE &
        longitude <= .data$MAX_LONGITUDE &
        latitude >= .data$MIN_LATITUDE &
        latitude <= .data$MAX_LATITUDE
    ) %>%
    dplyr::pull(.data$model)

  return(models)

}
