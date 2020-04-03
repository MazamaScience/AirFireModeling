

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
#' @title Create raster layer times
#'
#' @param raster A Raster\* object.
#'
#' @return \code{POSIXct} time.

raster_createTimes <- function(
  raster = NULL
) {

  layerName <- names(raster)
  epochSecs <- as.numeric(stringr::str_remove(layerName, 'X'))
  layerTime <- as.POSIXct(epochSecs, tz = "UTC", origin = lubridate::origin)

  return(layerTime)

}

#' @title Create raster layer time string
#'
#' @param raster A Raster\* object.
#' @param timezone Olson timezone in which times will be displayed.
#' @param prefix String prepended to the time
#'
#' @return Time string formatted for \code{timezone}.

raster_createTimeStrings <- function(
  raster = NULL,
  timezone = "UTC",
  prefix = ""
) {

  layerName <- names(raster)
  epochSecs <- as.numeric(stringr::str_remove(layerName, 'X'))
  layerTime <- as.POSIXct(epochSecs, tz = "UTC", origin = lubridate::origin)
  timeString <- paste0(prefix, strftime(layerTime, format = "%Y-%m-%d %H:00 %Z", tz = timezone))

  return(timeString)

}

#' @export
#' @keywords internal
#'
#' @title Create raster layer timestamp
#'
#' @param raster A Raster\* object.
#' @param timezone Olson timezone in which times will be displayed.
#' @param prefix String prepended to the time
#'
#' @return UTC timestamp as YYYYmmddHH.

raster_createTimeStamps <- function(
  raster = NULL
) {

  layerName <- names(raster)
  epochSecs <- as.numeric(stringr::str_remove(layerName, 'X'))
  layerTime <- as.POSIXct(epochSecs, tz = "UTC", origin = lubridate::origin)
  timeString <- strftime(layerTime, format = "%Y%m%d%H", tz = "UTC")

  return(timeString)

}

