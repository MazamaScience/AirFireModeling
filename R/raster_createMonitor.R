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

  # TODO: buffer only accepts radial distance in meters. Look into adding cell
  #       count for determining monitor collapse.

  # Create target Spatial Point
  target_sp <- sp::SpatialPoints(cbind(longitude, latitude))

  # Extract values from Raster Object at the target spatial point(s)
  data <- raster::extract(x = raster, y = target_sp, buffer = buffer, fun = FUN)
}
