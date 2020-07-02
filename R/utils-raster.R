

#' @export
#' @keywords hidden
#'
#' @title Test for Raster\* class
#'
#' @param raster Object to test.
#'
#' @return \code{TRUE} if \code{raster} is a Raster\* object, \code{FALSE} otherwise.
#'
#' @examples
#' \donttest{
#' library(AirFireModeling)
#' setModelDataDir('~/Data/BlueSky')
#'
#' # Load model data
#' rasterList <- raster_load(
#'   modelName = c("PNW-4km"),
#'   modelRun = c(2019100900),
#'   xlim = c(-125, -115),
#'   ylim = c(42, 50)
#' )
#'
#' raster_isRaster(rasterList[[1]])
#' }

raster_isRaster <- function(raster) {

  return( stringr::str_detect(class(raster), 'Raster*') )

}

# ===== Layer Times ============================================================

#' @export
#' @keywords internal
#'
#' @title Create raster layer name times
#'
#' @param layerName Vector of raster layer names.
#'
#' @return \code{POSIXct} time.
#'
#' @examples
#' \donttest{
#' library(AirFireModeling)
#' setModelDataDir('~/Data/BlueSky')
#'
#' # Load model data
#' rasterList <- raster_load(
#'   modelName = c("PNW-4km"),
#'   modelRun = c(2019100900),
#'   xlim = c(-125, -115),
#'   ylim = c(42, 50)
#' )
#'
#' layerNames <- names(rasterList[[1]])
#' raster_createLayerNameTimes(layerNames)
#' }

raster_createLayerNameTimes <- function(
  layerName
) {

  # NOTE:  Saw this once for modelRun 2020062200:
  # note:
  # NOTE:  …, 1592992800, 1592996400, 1593000000, 1593003600, 1593007200, …
  # NOTE:  Get's converted into layer names
  # NOTE:  …, 1592992800, 1592996400, 1.593e.9, 1593003600, 1593007200, …

  # Fix "1.593e.9" style problems
  epochSecsStrings <-
    stringr::str_remove(layerName, 'X') %>%
    stringr::str_replace("e\\.", "e+")

  # Convert to seconds
  epochSecs <- as.numeric(epochSecsStrings)
  # Convert to POSIXct
  layerTime <- as.POSIXct(epochSecs, tz = "UTC", origin = lubridate::origin)

  return(layerTime)

}

#' @export
#' @keywords internal
#'
#' @title Create raster layer times
#'
#' @param raster A RasterBrick.
#'
#' @return \code{POSIXct} time.
#'
#' @examples
#' \donttest{
#' library(AirFireModeling)
#' setModelDataDir('~/Data/BlueSky')
#'
#' # Load model data
#' rasterList <- raster_load(
#'   modelName = c("PNW-4km"),
#'   modelRun = c(2019100900),
#'   xlim = c(-125, -115),
#'   ylim = c(42, 50)
#' )
#'
#' raster_createTimes(rasterList[[1]])
#' }

raster_createTimes <- function(
  raster = NULL
) {

  layerTime <- raster_createLayerNameTimes(names(raster))

  return(layerTime)

}

#' @export
#' @title Create raster layer time string
#'
#' @param raster A RasterBrick.
#' @param format Format passed on to \code{strftime()}.
#' @param timezone Olson timezone in which times will be displayed.
#' @param prefix String prepended to the time.
#'
#' @return Time string formatted for \code{timezone}.
#'
#' @examples
#' \donttest{
#' library(AirFireModeling)
#' setModelDataDir('~/Data/BlueSky')
#'
#' # Load model data
#' rasterList <- raster_load(
#'   modelName = c("PNW-4km"),
#'   modelRun = c(2019100900),
#'   xlim = c(-125, -115),
#'   ylim = c(42, 50)
#' )
#'
#' raster_createTimeStrings(
#'   raster = rasterList[[1]],
#'   timezone = "America/Los_Angeles",
#'   prefix = "time: ")
#' }

raster_createTimeStrings <- function(
  raster = NULL,
  format = "%Y-%m-%d %H:00 %Z",
  timezone = "UTC",
  prefix = ""
) {

  layerTime <- raster_createLayerNameTimes(names(raster))
  timeString <- paste0(prefix, strftime(layerTime, format = format, tz = timezone))

  return(timeString)

}
