#' @export
#' @title Load data for a single BlueSky model
#'
#' @description This function encapsulates the process of downloading, and formatting
#' necessary for BlueSky model outputs. Additionally, it checks already existing
#' models and loads those if available.
#'
#' @description Loads BlueSky model output as a raster brick. The directory
#' previously set with \code{setModelDataDir()} is searched for "v2" formatted
#' model output. If data are not found locally, data are downloaded from
#' \code{baseUrl} and converted.
#'
#' The returned \code{dataBrick} object is of class \code{raster::RasterBrick} and
#' can be manipulated with appropriate functions from the \pkg{raster} package
#' or any of the \code{raster_~()} functions provided by \pkg{AirFireModeling}.
#'
#' On 2019-10-11, available model identifiers include the following:
#' \itemize{
#'   \item{AK-12km}
#'   \item{CANSAC-1.33km}
#'   \item{CANSAC-4km}
#'   \item{DRI1.33km-CMAQ}
#'   \item{DRI4km-CMAQ}
#'   \item{GFS-0.15deg-CanadaUSA-p25deg-68N}
#'   \item{GFS-0.15deg}
#'   \item{NAM-3km}
#'   \item{NAM84-0.15deg}
#'   \item{PNW-1.33km}
#'   \item{PNW-4km}
#'   \item{PNW1.33km-CMAQ}
#'   \item{PNW4km-CMAQ}
#' }
#'
#' @note The regurned \emph{RasterBrick} object will contain all grid cells
#' within the area defined by \code{xlim} and \code{ylim}.
#'
#' @note If \code{localPath} is used, \code{clean = FALSE} is automatically set.
#' The package will never remove original model output that it has not downloaded.
#'
#' @param modelName Model identifier.
#' @param modelRun Model initialization datestamp as "YYYYMMDDHH".
#' @param modelMode Subdirectory path containing BlueSky output, i.e. 'forcast'.
#' @param baseUrl Base URL for BlueSky output.
#' @param localPath Absolute path to a NetCDF file not found in `modelDataDir`.
#' @param level Elevation level.
#' @param xlim A vector of coordinate longitude bounds.
#' @param ylim A vector of coordinate latitude bounds.
#' @param clean Logical specifying removal of original model data after conversion
#' to "v2" format.
#' @param verbose Logical to display messages.
#'
#' @return A \pkg{raster} package \emph{RasterBrick} object.
#'
#' @examples
#' \donttest{
#' library(AirFireModeling)
#' setModelDataDir('~/Data/BlueSky')
#'
#' # Load model data
#' raster <- bluesky_load(
#'   modelName = "PNW-4km",
#'   modelRun = 2019100900,
#'   xlim = c(-125, -115),
#'   ylim = c(42, 50)
#' )
#'
#' raster_ggmap(raster, index = 3)
#' }

bluesky_load <- function(
  modelName = NULL,
  modelRun = NULL,
  modelMode = 'forecast',
  baseUrl = 'https://haze.airfire.org/bluesky-daily/output/standard',
  localPath = NULL,
  level = 1,
  xlim = NULL,
  ylim = NULL,
  clean = TRUE,
  verbose = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(baseUrl)

  if ( is.null(localPath) ) {

    MazamaCoreUtils::stopIfNull(modelName)
    MazamaCoreUtils::stopIfNull(modelRun)
    MazamaCoreUtils::stopIfNull(modelMode)

  } else {

    clean <- FALSE
    if ( !file.exists(localPath) )
      stop(sprintf("File not found at localPath = '%s'", localPath))

  }

  # Defaults
  if ( !is.logical(clean) ) clean <- TRUE
  if ( !is.logical(verbose) ) verbose <- TRUE

  # ----- Download and convert -------------------------------------------------

  if ( is.null(localPath) ) { # No localPath

    v2FileName <- paste0(modelName, "_", modelRun, "_v2.nc")
    v2FilePath <- file.path(getModelDataDir(), v2FileName)

    if ( !file.exists(v2FilePath) ) {
      rawFilePath <- bluesky_download(
        modelName = modelName,
        modelRun = modelRun,
        modelMode = modelMode,
        baseUrl = baseUrl,
        verbose = verbose
      )
      v2FilePath <- bluesky_toCommonFormat(rawFilePath, clean = clean)
    }

  } else { # User specified localPath

    # Does localPath represent a v2 path?
    if ( grepl(x = localPath, pattern = '.+_v2.nc$') ) {

      v2FilePath <- localPath

    } else {

      # Assume localPath represents a raw path.
      v2FilePath <- stringr::str_replace(localPath, '.nc', '_v2.nc')
      if ( !file.exists(v2FilePath) ) {
        v2FilePath <- bluesky_toCommonFormat(localPath, clean = clean)
      }

    }

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
  setModelDataDir("~/Data/BlueSky")

  modelName = "CANSAC-4km"
  modelRun = 2020062200
  modelMode = 'forecast'
  baseUrl = 'https://haze.airfire.org/bluesky-daily/output/standard'
  localPath = NULL
  level = 1
  xlim = NULL
  ylim = NULL
  clean = TRUE
  verbose = TRUE


  modelBrick <- bluesky_load(
    modelName = modelName,
    modelRun = modelRun,
    modelMode = modelMode,
    baseUrl = baseUrl,
    localPath = localPath,
    level = level,
    xlim = xlim,
    ylim = ylim,
    clean = clean,
    verbose = verbose
  )

}
