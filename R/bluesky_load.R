
#' @title Single model load function
#'
#' @description This function encapsulates the process of downloading, and formatting
#' necessary for BlueSky model outputs. Additionally, it checks already existing
#' models and loads those if avaliable.
#'
#' @param model A model. i.e 'PNW-1.33km' or 'CANSAC-4km'.
#' @param modelRun The model run date. YYYYmmddHH format required.
#' @param baseUrl the database URL.
#' @param modelType A type of model, formely subDir. i.e. 'forecast'
#' @param localPath A path to a downloaded NetCDF model.
#' @param xlim A vector of coordinate longitude bounds.
#' @param ylim A vector of coordinate latitude bounds.
#' @param clean Option to clean, or delete, the non-formatted model.
#' @param verbose Logical to display messages.
#'
#' @return A RasterBrick.
#' @export
#' @examples
#' \dontrun{
#' library(AirFireModeling)
#'
#' setModelDataDir('~/Data/BlueSky')
#' rasterBrick <- bluesky_load(
#'   model = "PNW-4km",
#'   modelRun = 2019100900,
#'   xlim = c(-125, -115),
#'   ylim = c(42, 50)
#' )
#'
#' raster_map(rasterBrick, index = 3)
#' }
#'
bluesky_load <- function(
  model = 'PNW-4km',
  modelRun = NULL,
  baseUrl = 'https://haze.airfire.org/bluesky-daily/output/standard',
  modelType = 'forecast',
  localPath = NULL,
  xlim = NULL,
  ylim = NULL,
  clean = TRUE,
  verbose = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(model)
  MazamaCoreUtils::stopIfNull(modelRun)
  MazamaCoreUtils::stopIfNull(baseUrl)
  MazamaCoreUtils::stopIfNull(modelType)

  # Defaults
  if ( !is.logical(clean) ) clean <- TRUE
  if ( !is.logical(verbose) ) verbose <- TRUE

  if ( !is.null(localPath) && !file.exists(localPath) )
    stop(sprintf("File not found at localPath = '%s'", localPath))

  # ----- Download and convert -------------------------------------------------

  v2FileName <- paste0(model, "_", modelRun, "_v2.nc")
  v2FilePath <- file.path(getModelDataDir(), v2FileName)

  if ( is.null(localPath) ) {

    if ( !file.exists(v2FilePath) ) {
      rawFilePath <- bluesky_download(
        model = model,
        modelRun = modelRun,
        baseUrl = baseUrl,
        modelType = modelType,
        verbose = verbose
      )
      v2FilePath <- bluesky_toCommonFormat(rawFilePath, clean = clean)
    }

  } else {

    # Check for v2 filePath
    if ( grepl(x = localPath, pattern = '.+_v2.nc$') ) {

      v2FilePath <- localPath

    } else {

      v2FilePath <- stringr::str_replace(localPath, '.nc', '_v2.nc')
      if ( !file.exists(v2FilePath) ) {
        v2FilePath <- bluesky_toCommonFormat(localPath, clean = FALSE)
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

    if ( is.null(ylim) )
      ylim <- c(raster::ymin(rasterBrick), raster::ymax(rasterBrick))

    if ( is.null(xlim) )
      xlim <- c(raster::xmin(rasterBrick), raster::xmax(rasterBrick))

    if ( !any(findInterval(xlim, c(raster::xmin(rasterBrick), raster::xmax(rasterBrick)))) )
      stop('xlim out of model coordinate domain.')

    if ( !any(findInterval(ylim, c(raster::ymin(rasterBrick), raster::ymax(rasterBrick)))) )
      stop('ylim out of model coordinate domain.')

    cells <- raster::cellFromXY(rasterBrick, cbind(xlim, ylim))
    ext <- raster::extentFromCells(rasterBrick, cells)

    # Crop to xlim, ylim
    return(raster::crop(rasterBrick, ext))

  }

}
