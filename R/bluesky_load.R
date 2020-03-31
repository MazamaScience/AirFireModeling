
#' @title Load data for a single BlueSky model
#'
#' @description This function encapsulates the process of downloading, and formatting
#' necessary for BlueSky model outputs. Additionally, it checks already existing
#' models and loads those if avaliable.
#'
#' @description Loads BlueSky model output as a raster brick. The directory
#' previously set with \code{setModelDataDir()} is searched for "v2" formatted
#' model output. If data are not found locally, data are downloaded from
#' \code{baseUrl} and converted.
#'
#' The reurned \code{dataBrick} object is of class \code{raster::RasterBrick} and
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
#' @param model Model identifier.
#' @param modelRun Model initialization datestamp as "YYYYMMDDHH".
#' @param modelType Subdirectory path containing BlueSky output, i.e. 'forcast'.
#' @param baseUrl Base URL for BlueSky output.
#' @param localPath Absolute path to a downloaded NetCDF file that is not found
#' in `modelDataDir`.
#' @param xlim A vector of coordinate longitude bounds.
#' @param ylim A vector of coordinate latitude bounds.
#' @param clean Logical specifying removal of original model data after conversion
#' to "v2" format.
#' @param verbose Logical to display messages.
#'
#' @return A \pkg{raster} package \emph{RasterBrick} object.
#' @export
#' @examples
#' \dontrun{
#' library(AirFireModeling)
#' setModelDataDir('~/Data/BlueSky')
#'
#' raster <- bluesky_load(
#'   model = "PNW-4km",
#'   modelRun = 2019100900,
#'   xlim = c(-125, -115),
#'   ylim = c(42, 50)
#' )
#'
#' raster_map(raster, index = 3)
#' }
#'
bluesky_load <- function(
  model = 'PNW-4km',
  modelRun = NULL,
  modelType = 'forecast',
  baseUrl = 'https://haze.airfire.org/bluesky-daily/output/standard',
  localPath = NULL,
  xlim = NULL,
  ylim = NULL,
  clean = TRUE,
  verbose = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(model)
  MazamaCoreUtils::stopIfNull(modelRun)
  MazamaCoreUtils::stopIfNull(modelType)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # Defaults
  if ( !is.logical(clean) ) clean <- TRUE
  if ( !is.logical(verbose) ) verbose <- TRUE

  if ( !is.null(localPath) && !file.exists(localPath) )
    stop(sprintf("File not found at localPath = '%s'", localPath))

  # ----- Download and convert -------------------------------------------------

  v2FileName <- paste0(model, "_", modelRun, "_v2.nc")
  v2FilePath <- file.path(getModelDataDir(), v2FileName)

  if ( is.null(localPath) ) { # No localPath

    if ( !file.exists(v2FilePath) ) {
      rawFilePath <- bluesky_download(
        model = model,
        modelRun = modelRun,
        modelType = modelType,
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

  rasterBrick <- raster::brick(v2FilePath)

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
