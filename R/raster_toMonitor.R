#' @export
#' @title Create a ws_monitor object from raster object
#'
#' @param raster A raster object
#' @param longitude Target longitude
#' @param latitude Target latitude
#' @param buffer A radial buffer about the buffer, in meters.
#' @param monitorID An optional monitor identification name.
#' @param FUN A function to collapse cells if buffer > 0.
#'
#' @description Time series associated with multiple grid cells are merged into a single
#' time series by using \code{FUN} to collapse a given grid cell count, or
#' or a grid cell radi, to a single coordinate. For instance, if the
#' \code{FUN = mean} then the grid cells within the paramters are averaged to a
#' single central coordinate.
#'
#' @return A \emph{ws_monitor} object representing a single monitor.
raster_toMonitor <- function(
  raster,
  longitude = NULL,
  latitude = NULL,
  buffer = 1000,
  monitorID = NULL,
  FUN = mean
) {

  # ----- Validate parameters --------------------------------------------------
  MazamaCoreUtils::stopIfNull(raster)
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)

  # Checks
  if ( !grepl('[rR]aster.+', class(raster)) ) {
    stop(print('A valid Raster object is required.'))
  }
  if ( longitude < raster::xmin(raster) | longitude > raster::xmax(raster) |
       latitude < raster::ymin(raster) | latitude > raster::ymax(raster) ) {
    stop('Check Coordinates: Out of range.')
  }
  if ( is.null(monitorID) ) {
    monitorID <- c('GEN_ID')
    warning('No Monitor ID provided: using generated ID')
  }

  # TODO: buffer only accepts radial distance in meters. Look into adding cell
  #       count for determining monitor collapse.

  # Create target Spatial Point
  target_sp <- sp::SpatialPoints( coords = cbind(longitude, latitude),
                                  proj4string = raster::crs(raster) )

  # Extract values from Raster Object at the target spatial point(s)
  target_data <- c(t(raster::extract( x = raster,
                                      y = target_sp,
                                      buffer = buffer,
                                      fun = FUN )))

  # Assume names of raster layers are POSIX dates
  # Remove 'X' from string convert to numeric
  datetime <- as.numeric(stringr::str_remove( string = names(raster),
                                              pattern = 'X' ))
  # TIMEZONES!
  tzone <- c('UTC')

  # Assimilate datetime class
  class(datetime) <- c('POSIXct', 'POSIXt')
  # Set TIMEZONE to UTC
  attr(datetime, 'tzone') <- tzone

  # ----- Create ws_monitor object and populate --------------------------------
  # Fill Meta
  meta <- PWFSLSmoke::createEmptyMetaDataframe(1)
  meta$monitorID <- as.character(monitorID)
  meta$longitude <- as.numeric(longitude)
  meta$latitude <- as.numeric(latitude)
  meta$timezone <- as.character(tzone)
  rownames(meta) <- as.character(monitorID)

  # Fill Data
  data <- data.frame(datetime, target_data)
  colnames(data) <- c('datetime', monitorID)

  # Combine into ws_monitor list object
  monitor <- list('meta' = meta, 'data' = data)
  class(monitor) <- c('ws_monitor', class(monitor))

  return(monitor)

}
