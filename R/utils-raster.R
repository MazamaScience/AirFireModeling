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
#' raster_generateTime(rasterList[[1]])
#' }
raster_generateTime <- function(
  raster = NULL
) {

  nc <- ncdf4::nc_open(raster@file@name)
  timeVar <- ncdf4::ncvar_get(nc, "time")
  ncdf4::nc_close(nc)
  times <- as.POSIXct(timeVar, tz = "UTC", origin = lubridate::origin)
  return(times)

}

#' @export
#' @keywords internal
#' @title Inherit File path across Raster objects
#'
#' @param from The raster to copy path from.
#' @param to the raster to copy path to.
#'
#' @return raster
raster_copyfn <- function(from, to, ...) {

  to@file@name <- from@file@name
  return(to)

}

#' @export
#' @title Get the time-stamp(s) of a Raster object
#'
#' @param raster A Raster* object.
#'
#' @return POSIXct vector
raster_getTime <- function(raster) {

  time <- as.numeric(gsub("X", "", names(raster)))
  return(as.POSIXct(time, tz = "UTC", origin = lubridate::origin))

}
