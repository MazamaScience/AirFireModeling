

#' @export
#' @keywords hidden
#'
#' @title Test for Raster\* class
#'
#' @param raster Object to test.
#'
#' @return \code{TRUE} if \code{raster} is a Raster\* object, \code{FALSE} otherwise.
#'

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

raster_createLayerNameTimes <- function(
  layerName
) {

  epochSecs <- as.numeric(stringr::str_remove(layerName, 'X'))
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
