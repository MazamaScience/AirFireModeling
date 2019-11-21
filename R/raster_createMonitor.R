#' @export
#' @title Create a ws_monitor object from raster object
#'
#' @param raster
#' @param longitude
#' @param latitude
#' @param buffer
#' @param monitorID
#' @param FUN
#'
raster_createMonitor <- function(
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

  if ( is.null(monitorID) ) {
    monitorID <- c('GEN_ID')
  }

  # TODO: buffer only accepts radial distance in meters. Look into adding cell
  #       count for determining monitor collapse.

  # Create target Spatial Point
  target_sp <- sp::SpatialPoints( coords = cbind(longitude, latitude),
                                  proj4string = crs(raster) )

  # Extract values from Raster Object at the target spatial point(s)
  target_data <- c(t(raster::extract( x = raster,
                                      y = target_sp,
                                      buffer = buffer,
                                      fun = FUN )))

  # Assume names of raster layers are POSIX dates
  datetime <- stringr::str_remove( string = names(raster),
                                   pattern = 'X' )

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

  # Fill Data
  data <- data.frame(datetime, target_data)
  colnames(data) <- c('datetime', monitorID)

  # Combine into ws_monitor list object

  monitor <- list('meta' = meta, 'data' = data)
  class(monitor) <- c('ws_monitor', class(monitor))
  return(monitor)


  if ( FALSE ) {
    raster <- X
    longitude <- sc_lon
    latitude <- sc_lat
    buffer <- 100
    FUN = mean
  }

}
