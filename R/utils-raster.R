#' @export
#' @keywords internal
#'
#' @title Create raster layer times
#'
#' @param layerName Name of the \code{rasterLayer} object
#'
#' @return \code{POSIXct} time.

createLayerTime <- function(
  layerName = ""
) {

  epochSecs <- as.numeric(stringr::str_remove(layerName, 'X'))
  layerTime <- as.POSIXct(epochSecs, tz = "UTC", origin = lubridate::origin)

  return(layerTime)

}

#' @title Create raster layer time string
#'
#' @param layerName Name of the \code{rasterLayer} object
#' @param timezone Olson timezone in which times will be displayed.
#' @param prefix String prepended to the time
#'
#' @return Time string formatted for \code{timezone}.

createLayerTimeString <- function(
  layerName = "",
  timezone = "UTC",
  prefix = ""
) {

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
#' @param layerName Name of the \code{rasterLayer} object
#' @param timezone Olson timezone in which times will be displayed.
#' @param prefix String prepended to the time
#'
#' @return UTC timestamp as YYYYmmddHH.

createLayerTimeStamp <- function(
  layerName = ""
) {

  epochSecs <- as.numeric(stringr::str_remove(layerName, 'X'))
  layerTime <- as.POSIXct(epochSecs, tz = "UTC", origin = lubridate::origin)
  timeString <- strftime(layerTime, format = "%Y%m%d%H", tz = "UTC")

  return(timeString)

}

