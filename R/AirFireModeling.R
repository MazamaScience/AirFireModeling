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
#' @export
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
