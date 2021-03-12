#' @title Load BlueSky model data
#'
#' @param modelName Model identifier.
#' @param modelRun Model initialization datestamp as "YYYYMMDDHH".
#' @param modelMode Subdirectory path containing BlueSky output, i.e. 'forcast'.
#' @param baseUrl Base URL for BlueSky output.
#' @param localPath Vector of absolute paths to NetCDF files not found in `modelDataDir`.
#' @param xlim Vector of coordinate longitude bounds.
#' @param ylim Vector of coordinate latitude bounds.
#' @param clean Logical specifying removal of original model data after conversion
#' to "v2" format.
#' @param verbose Logical to display messages.
#'
#' @description Load or download models and automatically convert NetCDF (.nc)
#' format to a spatio-temporal Raster\* object.
#'
#' @details \code{model} and \code{modelRun} are vectorised parameters. Multiple
#' datasets can be loaded at once.
#'
#' @return A list of one or more raster objects.
#' @export
#'
#' @examples
#' \donttest{
#' library(AirFireModeling)
#' setModelDataDir('~/Data/BlueSky')
#'
#' # Load model data
#' rasterList <- raster_load(
#'   modelName = "PNW-4km",
#'   modelRun = c(2020091300, 2020091400, 2020091500, 2020091600),
#'   xlim = c(-125, -115),
#'   ylim = c(42, 50)
#' )
#'
#' raster_ggmap(rasterList, index = 3)
#' }
raster_load <- function(
  modelName = NULL,
  modelRun = NULL,
  modelMode = 'forecast',
  baseUrl = 'https://haze.airfire.org/bluesky-daily/output/standard',
  localPath = NULL,
  xlim = NULL,
  ylim = NULL,
  clean = TRUE,
  verbose = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(localPath) ) {

    MazamaCoreUtils::stopIfNull(modelName)
    MazamaCoreUtils::stopIfNull(modelMode)
    MazamaCoreUtils::stopIfNull(baseUrl)

    if ( is.null(modelRun) ) {
      # Use todays date if modelRun is null
      now <- lubridate::now(tzone = 'UTC')
      modelRun <- paste0(strftime(now, '%Y%m%d', tz = 'UTC'), '00')
    } else {
      # Make sure we end up with YYYYmmddHH
      modelRun <- as.character(modelRun)
      length <- stringr::str_length(modelRun)[1]
      if ( length == 8 ) { # if the length is 8 append 00 (model runs work YYYYmmddHH)
        modelRun <- paste0(modelRun, '00')
      }
    }

    # Verify YYYYmmddHH
    for ( singleModelRun in modelRun ) {
      if ( !stringr::str_detect(singleModelRun, '[0-9]{10}') )
        stop(sprintf("'modelRun' parameter '%s' must have 10 digits"))
    }

    if ( length(modelMode) > 1 )
      stop("Only a single 'modelMode' can be used in each call to raster_load().")

    # Defaults
    if ( !is.logical(clean) ) clean <- FALSE
    if ( !is.logical(verbose) ) verbose <- TRUE

  }

  # ----- Load data ------------------------------------------------------------

  # Empty list
  dataList <- list()

  if ( !is.null(localPath) ) {

    for ( i in seq_along(localPath) ) {

      # Get the filePath and create a name
      filePath <- localPath[i]
      name <-
        basename(filePath) %>%
        stringr::str_replace(".nc", "") %>%
        stringr::str_replace("_v2", "")

      if ( verbose )
        message(sprintf("Loading %s ...", name))

      # Try to load model data
      result <- try({
        dataList[[name]] <- bluesky_load(
          modelName = NULL,
          modelRun = NULL,
          modelMode = 'forecast',
          baseUrl = 'https://haze.airfire.org/bluesky-daily/output/standard',
          localPath = filePath,
          xlim = xlim,
          ylim = ylim,
          clean = FALSE,
          verbose = verbose
        )
      }, silent = !verbose)

    }

  } else {

    # NOTE:  We need to create all combinations of modelName and modelRun for
    # NOTE:  downloading. The expand.grid() function does just that.

    # Create combinations
    allModelsDF <- expand.grid(modelName = modelName, modelRun = modelRun)

    for ( i in seq_len(nrow(allModelsDF)) ) {

      singleModelName <- allModelsDF$modelName[i]
      singleModelRun <- allModelsDF$modelRun[i]
      name <- sprintf("%s_%s", singleModelName, singleModelRun)

      if ( verbose )
        message(sprintf("Loading %s ...", name))

      # Try to load model data
      result <- try({
        dataList[[name]] <- bluesky_load(
          modelName = singleModelName,
          modelRun = singleModelRun,
          modelMode = modelMode,
          baseUrl = baseUrl,
          localPath = NULL,
          xlim = xlim,
          ylim = ylim,
          clean = clean,
          verbose = verbose
        )
      }, silent = !verbose)

    }

  } # END is.null(localPath)


  return(dataList)

}
