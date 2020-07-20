#' @export
#' @title Load data for a single CMAQ model
#'
#' @description This function encapsulates the process of downloading, and formatting
#' necessary for CMAQ model outputs. Additionally, it checks already existing
#' models and loads those if available.
#'
#' @description Loads CMAQ model output as a raster brick. The directory
#' previously set with \code{setModelDataDir()} is searched for "v2" formatted
#' model output. If data are not found locally, data are downloaded from
#' \code{baseUrl} and converted.
#'
#' The returned \code{dataBrick} object is of class \code{raster::RasterBrick} and
#' can be manipulated with appropriate functions from the \pkg{raster} package
#' or any of the \code{raster_~()} functions provided by \pkg{AirFireModeling}.
#'
#' @note The regurned \emph{RasterBrick} object will contain all grid cells
#' within the area defined by \code{xlim} and \code{ylim}.
#'
#' @note If \code{localPath} is used, \code{clean = FALSE} is automatically set.
#' The package will never remove original model output that it has not downloaded.
#'
#' @param localPath Absolute path to a NetCDF file not found in `modelDataDir`.
#' @param level Elevation level
#' @param xlim A vector of coordinate longitude bounds.
#' @param ylim A vector of coordinate latitude bounds.
#' @param clean Logical specifying removal of original model data after conversion
#' to "v2" format.
#' @param verbose Logical to display messages.
#'
#' @return A \pkg{raster} package \emph{RasterBrick} object.

cmaq_load <- function(
  localPath = NULL,
  level = 1,
  xlim = NULL,
  ylim = NULL,
  clean = TRUE,
  verbose = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  if ( !file.exists(localPath) )
    stop(sprintf("File not found at localPath = '%s'", localPath))

  # Defaults
  if ( !is.logical(clean) ) clean <- TRUE
  if ( !is.logical(verbose) ) verbose <- TRUE

  # ----- Download and convert -------------------------------------------------

  # Assume localPath represents a raw path.
  v2FilePath <- stringr::str_replace(localPath, '.nc', '_v2.nc')
  if ( !file.exists(v2FilePath) ) {
    v2FilePath <- cmaq_toCommonFormat(localPath, clean = clean)
  }

  # ----- Read in rasterBrick --------------------------------------------------

  # NOTE: raster bricks are not loaded into memory, only their reference.

  rasterBrick <- raster::brick(v2FilePath, level = level)

  if ( is.null(xlim) && is.null(ylim) ) {

    # Full grid
    return(rasterBrick)

  } else {

    xDomain <- c(raster::xmin(rasterBrick), raster::xmax(rasterBrick))
    yDomain <- c(raster::ymin(rasterBrick), raster::ymax(rasterBrick))

    # NOTE:  Don't complain if xlim, ylim are bigger than the model domain.
    # NOTE:  Just use the intersection.

    # Calculate xlim
    if ( is.null(xlim) ) {
      xlim <- xDomain
    } else {
      xlim <- c(
        max(xlim[1], xDomain[1]),
        min(xlim[2], xDomain[2])
      )
    }

    # Calculate ylim
    if ( is.null(ylim) ) {
      ylim <- yDomain
    } else {
      ylim <- c(
        max(ylim[1], yDomain[1]),
        min(ylim[2], yDomain[2])
      )
    }

    cells <- raster::cellFromXY(rasterBrick, cbind(xlim, ylim))
    ext <- raster::extentFromCells(rasterBrick, cells)

    # Crop to xlim, ylim
    return(raster::crop(rasterBrick, ext))

  }

}

# ===== DEBUG ==================================================================

if ( FALSE ) {

  library(AirFireModeling)
  source("local_jon/cmaq_toCommonFormat.R")
  setModelDataDir('~/Data/BlueSky')

  localPath <- "~/Data/BlueSky/CMAQ_PM25.2020167.DRId02.regrid.nc"
  level <- 1
  clean <- FALSE

  raster <- cmaq_load(
    localPath = localPath,
    level = level,
    clean = clean
  )

  raster_ggmap(raster, index = 3)

}
