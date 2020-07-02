#' @export
#' @title Aggregate multiple BlueSky model runs into a single RasterBrick object
#'
#' @param model Model identifier(s).
#' @param firstModelRun Initialization datestamp of first model run as "YYYYmmddHH".
#' @param lastModelRun Initialization datestamp of last model run as "YYYYmmddHH".
#' @param modelType Subdirectory path containing BlueSky output, i.e. 'forcast'.
#' @param baseUrl Base URL for BlueSky output.
#' @param xlim A vector of coordinate longitude bounds.
#' @param ylim A vector of coordinate latitude bounds.
#' @param clean Logical specifying removal of original model data after conversion
#' to "v2" format.
#' @param verbose Logical to display messages.
#' @param spacing Time difference (hrs) between each model run -- overrides
#' default values.
#' @param chunk The portion of the time (of width = 'spacing') to be chosen from
#' each model run. If \code{spacing = 12}, then \code{chunk = 1} corresponds to
#' the first 12 hours of the each model run, \code{chunk = 2} corresponds to the
#' next 12 hours of each model run, and so on. See the note below for further
#' explanation.
#'
#' @description Combines multiple Bluesky model runs to produce a single
#' RasterBrick object. Users can set the first run, the last run, the hourly
#' spacing between model runs, and the chunk index for the section of each
#' model's time axis to be used.
#'
#' @details
#' Setting \code{chunk = 1} results in grabbing the initial portion of each model
#' run. Setting chunk to higher numbers utilizes times further out in the the
#' forecast of each model run implying more uncertainty.
#'
#' @section Chunks:
#' Suppose a model is run every 12 hours. For this model \code{spacing = 12}.
#' In this case, \code{chunk = 1} means we select the first 12 hours from each
#' model run -- i.e. we work with the model data from as close to initialization
#' as possible. Setting \code{chunk = 2} means we select the next 12 hours
#' (i.e. hours 13-24) from each model run. Setting \code{chunk = 3} means we
#' select hours 25-48 from each model run and so on.
#'
#' Chunking can be used to account for any inaccuracies in the data that may
#' be due to poor initial conditions. For example, older models only predicted
#' impacts from existing sources, and did not take into account any smoke that
#' may have already been present at the time of initialization. In this case,
#' setting \code{chunk = 2} may produce better results.
#'
#' @return A RasterBrick object.
#'
#' @examples
#' \donttest{
#' library(AirFireModeling)
#' setModelDataDir('~/Data/BlueSky')
#'
#' # Kincade fire
#' rasterBrick <- raster_aggregate(
#'   model = "CANSAC-4km",
#'   firstModelRun = 2019102700,
#'   lastModelRun = 2019103100,
#'   xlim = c(-124, -121.5),
#'   ylim = c(37.5, 39)
#' )
#'
#' raster_facet(
#'   rasterBrick,
#'   title = "Kincade Fire -- 2019102700 through 2019103100",
#'   ncol = 12,
#'   palette = 'Spectral',
#'   col_county = 'gray95',
#'   direction = -1,
#'   breaks = c(-Inf, 0, 12, 35, 55, 150, 250, 350, Inf)
#' )
#' }
#'
raster_aggregate <- function(
  model = NULL,
  firstModelRun = NULL,
  lastModelRun = NULL,
  modelType = 'forecast',
  baseUrl = 'https://haze.airfire.org/bluesky-daily/output/standard',
  xlim = NULL,
  ylim = NULL,
  clean = TRUE,
  verbose = TRUE,
  spacing = NULL,
  chunk = 1
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(model)
  MazamaCoreUtils::stopIfNull(firstModelRun)
  MazamaCoreUtils::stopIfNull(lastModelRun)
  MazamaCoreUtils::stopIfNull(modelType)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # Verify YYYYmmddHH
  firstModelRun <- as.character(firstModelRun)
  if ( !stringr::str_detect(firstModelRun, '[0-9]{10}') )
    stop(sprintf("Parameter 'firstModelRun' must have 10 digits"))

  lastModelRun <- as.character(lastModelRun)
  if ( !stringr::str_detect(lastModelRun, '[0-9]{10}') )
    stop(sprintf("Parameter 'lastModelRun' must have 10 digits"))

  if ( length(modelType) > 1 )
    stop("Only a single 'modelType' can be used in each call to raster_aggregate().")

  # Defaults
  if ( !is.logical(clean) ) clean <- FALSE
  if ( !is.logical(verbose) ) verbose <- TRUE
  if ( !is.numeric(spacing) ) spacing <- NULL # use default
  if ( !is.numeric(chunk) ) chunk <- 1

  # ----- Get models to load ---------------------------------------------------

  # Available modelRuns
  #  * view all model subdirectories
  #  * extract 10-digit modelRuns
  url <- paste(baseUrl, model, sep = "/")
  availableModelRuns <-
    readLines(url) %>%
    stringr::str_extract_all('[0-9]{10}(?=/)') %>%
    unlist() %>%
    sort() %>%
    unique()

  if ( !firstModelRun %in% availableModelRuns )
    stop(sprintf("firstModelRun = %s is not available at %s", firstModelRun, url))

  if ( !lastModelRun %in% availableModelRuns )
    stop(sprintf("lastModelRun = %s is not available at %s", lastModelRun, url))

  # Find modelRuns between first and last
  mask <-
    ( availableModelRuns >= firstModelRun ) &
    ( availableModelRuns <= lastModelRun )

  modelRuns <- availableModelRuns[mask]

  # Convert modelRun strings into POSIXct
  modelTimes <-
    MazamaCoreUtils::parseDatetime(
      modelRuns,
      timezone = "UTC",
      expectAll = TRUE
    )

  # Calculate median time difference in hours
  medianSpacing <-
    diff(modelTimes, unit = "hour") %>%
    as.numeric() %>%
    stats::median()

  # Make sure spaing is defined
  if ( is.null(spacing) )
    spacing <- medianSpacing

  # Warn user if the selected time difference is less than it should be
  if ( verbose && (spacing < medianSpacing) ) {
    message(
      paste0(
        "The median offset between model runs is ",
        medianSpacing,
        " hours. Consider a larger 'spacing' to avoid aggregated model gaps."
      )
    )
  }

  if ( spacing > medianSpacing ) {
    stop(sprintf("Setting parameter 'spacing' > default [%d] is not yet supported.", medianSpacing))
  }

  # ----- Load data ------------------------------------------------------------

  rasterList <- raster_load(
    model = model,
    modelRun = modelRuns,
    modelType = modelType,
    baseUrl = baseUrl,
    xlim = xlim,
    ylim = ylim,
    clean = clean,
    verbose = verbose
  )

  # ----- Subset and combine ---------------------------------------------------

  timeSteps <- 1:spacing + (spacing * (chunk-1))

  # Convert rasterList to rasterBrick
  rasterBrick <- raster::brick(
    lapply(
      X = rasterList,
      FUN = function(r) {
        r[[timeSteps]]
      }
    )
  )

  return(rasterBrick)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  model = "PNW-4km"
  firstModelRun = 2019100800
  lastModelRun = 2019101100
  modelType = 'forecast'
  baseUrl = 'https://haze.airfire.org/bluesky-daily/output/standard'
  xlim = NULL
  ylim = NULL
  clean = TRUE
  verbose = TRUE
  spacing = NULL
  chunk = 1

}
